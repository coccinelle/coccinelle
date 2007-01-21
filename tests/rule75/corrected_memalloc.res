/*
 *  Copyright (c) by Jaroslav Kysela <perex@suse.cz>
 *                   Takashi Iwai <tiwai@suse.de>
 * 
 *  Generic memory allocators
 *
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#include <linux/config.h>
#include <linux/module.h>
#include <linux/proc_fs.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/mm.h>
#include <asm/semaphore.h>
#include <sound/memalloc.h>


MODULE_AUTHOR("Takashi Iwai <tiwai@suse.de>, Jaroslav Kysela <perex@suse.cz>");
MODULE_DESCRIPTION("Memory allocator for ALSA system.");
MODULE_LICENSE("GPL");


#ifndef SNDRV_CARDS
#define SNDRV_CARDS	8
#endif
static int enable[SNDRV_CARDS] = {[0 ... (SNDRV_CARDS-1)] = 1};
MODULE_PARM(enable, "1-" __MODULE_STRING(SNDRV_CARDS) "i");
MODULE_PARM_DESC(enable, "Enable cards to allocate buffers.");


/*
 */

static DECLARE_MUTEX(list_mutex);
static LIST_HEAD(mem_list_head);

/* buffer preservation list */
struct snd_mem_list {
	struct snd_dma_device dev;
	struct snd_dma_buffer buffer;
	int used;
	struct list_head list;
};

/* id for pre-allocated buffers */
#define SNDRV_DMA_DEVICE_UNUSED (unsigned int)-1

#ifdef CONFIG_SND_DEBUG
#define __ASTRING__(x) #x
#define snd_assert(expr, args...) do {\
	if (!(expr)) {\
		printk(KERN_ERR "snd-malloc: BUG? (%s) (called from %p)\n", __ASTRING__(expr), __builtin_return_address(0));\
		args;\
	}\
} while (0)
#else
#define snd_assert(expr, args...) /**/
#endif

#ifdef CONFIG_PCI
#if defined(__i386__) || defined(__ppc__) || defined(__x86_64__)
#define HACK_PCI_ALLOC_CONSISTENT

/*
 * A hack to allocate large buffers via pci_alloc_consistent()
 *
 * since pci_alloc_consistent always tries GFP_DMA when the requested
 * pci memory region is below 32bit, it happens quite often that even
 * 2 order of pages cannot be allocated.
 *
 * so in the following, we allocate at first without dma_mask, so that
 * allocation will be done without GFP_DMA.  if the area doesn't match
 * with the requested region, then realloate with the original dma_mask
 * again.
 */

static void *snd_pci_hack_alloc_consistent(struct pci_dev *hwdev, size_t size,
				    dma_addr_t *dma_handle)
{
	void *ret;
	u64 dma_mask;
	unsigned long mask;

	if (hwdev == NULL)
		return pci_alloc_consistent(hwdev, size, dma_handle);
	dma_mask = hwdev->consistent_dma_mask;
	mask = (unsigned long)dma_mask;
	hwdev->consistent_dma_mask = 0xffffffff; /* do without masking */
	ret = pci_alloc_consistent(hwdev, size, dma_handle);
	hwdev->consistent_dma_mask = dma_mask; /* restore */
	if (ret) {
		/* obtained address is out of range? */
		if (((unsigned long)*dma_handle + size - 1) & ~mask) {
			/* reallocate with the proper mask */
			pci_free_consistent(hwdev, size, ret, *dma_handle);
			ret = pci_alloc_consistent(hwdev, size, dma_handle);
		}
	} else {
		/* wish to success now with the proper mask... */
		if (mask != 0xffffffffUL)
			ret = pci_alloc_consistent(hwdev, size, dma_handle);
	}
	return ret;
}

/* redefine pci_alloc_consistent for some architectures */
#undef pci_alloc_consistent
#define pci_alloc_consistent snd_pci_hack_alloc_consistent

#endif /* arch */
#endif /* CONFIG_PCI */


/*
 * compare the two devices
 * returns non-zero if matched.
 */
static int compare_device(const struct snd_dma_device *a, const struct snd_dma_device *b, int allow_unused)
{
	if (a->type != b->type)
		return 0;
	if (a->id != b->id) {
		if (! allow_unused || (a->id != SNDRV_DMA_DEVICE_UNUSED && b->id != SNDRV_DMA_DEVICE_UNUSED))
			return 0;
	}
	switch (a->type) {
	case SNDRV_DMA_TYPE_CONTINUOUS:
#ifdef CONFIG_ISA
	case SNDRV_DMA_TYPE_ISA:
#endif
		return a->dev.flags == b->dev.flags;
#ifdef CONFIG_PCI
	case SNDRV_DMA_TYPE_PCI:
	case SNDRV_DMA_TYPE_PCI_SG:
		return a->dev.pci == b->dev.pci;
#endif
#ifdef CONFIG_SBUS
	case SNDRV_DMA_TYPE_SBUS:
		return a->dev.sbus == b->dev.sbus;
#endif
	}
	return 0;
}

/**
 * snd_dma_alloc_pages - allocate the buffer area according to the given type
 * @dev: the buffer device info
 * @size: the buffer size to allocate
 * @dmab: buffer allocation record to store the allocated data
 *
 * Calls the memory-allocator function for the corresponding
 * buffer type.
 * 
 * Returns zero if the buffer with the given size is allocated successfuly,
 * other a negative value at error.
 */
int snd_dma_alloc_pages(const struct snd_dma_device *dev, size_t size,
			struct snd_dma_buffer *dmab)
{
	snd_assert(dev != NULL, return -ENXIO);
	snd_assert(size > 0, return -ENXIO);
	snd_assert(dmab != NULL, return -ENXIO);

	dmab->bytes = 0;
	switch (dev->type) {
	case SNDRV_DMA_TYPE_CONTINUOUS:
		dmab->area = snd_malloc_pages(size, dev->dev.flags);
		dmab->addr = 0;
		break;
#ifdef CONFIG_ISA
	case SNDRV_DMA_TYPE_ISA:
		dmab->area = snd_malloc_isa_pages(size, &dmab->addr);
		break;
#endif
#ifdef CONFIG_PCI
	case SNDRV_DMA_TYPE_PCI:
		dmab->area = snd_malloc_pci_pages(dev->dev.pci, size, &dmab->addr);
		break;
	case SNDRV_DMA_TYPE_PCI_SG:
		snd_malloc_sgbuf_pages(dev->dev.pci, size, dmab);
		break;
#endif
#ifdef CONFIG_SBUS
	case SNDRV_DMA_TYPE_SBUS:
		dmab->area = snd_malloc_sbus_pages(dev->dev.sbus, size, &dmab->addr);
		break;
#endif
	default:
		printk(KERN_ERR "snd-malloc: invalid device type %d\n", dev->type);
		dmab->area = NULL;
		dmab->addr = 0;
		return -ENXIO;
	}
	if (! dmab->area)
		return -ENOMEM;
	dmab->bytes = size;
	return 0;
}


/**
 * snd_dma_free_pages - release the allocated buffer
 * @dev: the buffer device info
 * @dmbab: the buffer allocation record to release
 *
 * Releases the allocated buffer via snd_dma_alloc_pages().
 */
void snd_dma_free_pages(const struct snd_dma_device *dev, struct snd_dma_buffer *dmab)
{
	switch (dev->type) {
	case SNDRV_DMA_TYPE_CONTINUOUS:
		snd_free_pages(dmab->area, dmab->bytes);
		break;
#ifdef CONFIG_ISA
	case SNDRV_DMA_TYPE_ISA:
		snd_free_isa_pages(dmab->bytes, dmab->area, dmab->addr);
		break;
#endif
#ifdef CONFIG_PCI
	case SNDRV_DMA_TYPE_PCI:
		snd_free_pci_pages(dev->dev.pci, dmab->bytes, dmab->area, dmab->addr);
		break;
	case SNDRV_DMA_TYPE_PCI_SG:
		snd_free_sgbuf_pages(dmab);
		break;
#endif
#ifdef CONFIG_SBUS
	case SNDRV_DMA_TYPE_SBUS:
		snd_free_sbus_pages(dev->dev.sbus, dmab->bytes, dmab->area, dmab->addr);
		break;
#endif
	default:
		printk(KERN_ERR "snd-malloc: invalid device type %d\n", dev->type);
	}
}


/*
 * search for the device
 */
static struct snd_mem_list *mem_list_find(const struct snd_dma_device *dev, int search_empty)
{
	struct list_head *p;
	struct snd_mem_list *mem;

	list_for_each(p, &mem_list_head) {
		mem = list_entry(p, struct snd_mem_list, list);
		if (mem->used && search_empty)
			continue;
		if (compare_device(&mem->dev, dev, search_empty))
			return mem;
	}
	return NULL;
}

/**
 * snd_dma_get_reserved - get the reserved buffer for the given device
 * @dev: the buffer device info
 * @dmab: the buffer allocation record to store
 *
 * Looks for the reserved-buffer list and re-uses if the same buffer
 * is found in the list.  When the buffer is found, it's marked as used.
 * For unmarking the buffer, call snd_dma_free_reserved().
 *
 * Returns the size of buffer if the buffer is found, or zero if not found.
 */
size_t snd_dma_get_reserved(const struct snd_dma_device *dev, struct snd_dma_buffer *dmab)
{
	struct snd_mem_list *mem;

	snd_assert(dev && dmab, return 0);

	down(&list_mutex);
	mem = mem_list_find(dev, 1);
	if (mem) {
		mem->used = 1;
		mem->dev = *dev;
		*dmab = mem->buffer;
		up(&list_mutex);
		return dmab->bytes;
	}
	up(&list_mutex);
	return 0;
}

/**
 * snd_dma_free_reserved - unmark the reserved buffer
 * @dev: the buffer device info
 *
 * Looks for the matching reserved buffer and erases the mark on it
 * if found.
 *
 * Returns zero.
 */
int snd_dma_free_reserved(const struct snd_dma_device *dev)
{
	struct snd_mem_list *mem;

	snd_assert(dev, return -EINVAL);
	down(&list_mutex);
	mem = mem_list_find(dev, 0);
	if (mem)
		mem->used = 0;
	up(&list_mutex);
	return 0;
}

/**
 * snd_dma_set_reserved - reserve the buffer
 * @dev: the buffer device info
 * @dmab: the buffer to reserve
 *
 * Reserves the given buffer as a reserved buffer.
 * When an old reserved buffer already exists, the old one is released
 * and replaced with the new one.
 *
 * When NULL buffer pointer or zero buffer size is given, the existing
 * buffer is released and the entry is removed.
 * 
 * Returns zero if successful, or a negative code at error.
 */
int snd_dma_set_reserved(const struct snd_dma_device *dev, struct snd_dma_buffer *dmab)
{
	struct snd_mem_list *mem;

	snd_assert(dev, return -EINVAL);
	down(&list_mutex);
	mem = mem_list_find(dev, 0);
	if (mem) {
		if (mem->used)
			printk(KERN_WARNING "snd-page-alloc: releasing the used block (type=%d, id=0x%x\n", mem->dev.type, mem->dev.id);
		snd_dma_free_pages(dev, &mem->buffer);
		if (! dmab || ! dmab->bytes) {
			/* remove the entry */
			list_del(&mem->list);
			kfree(mem);
			up(&list_mutex);
			return 0;
		}
	} else {
		if (! dmab || ! dmab->bytes) {
			up(&list_mutex);
			return 0;
		}
		mem = kmalloc(sizeof(*mem), GFP_KERNEL);
		if (! mem) {
			up(&list_mutex);
			return -ENOMEM;
		}
		mem->dev = *dev;
		list_add_tail(&mem->list, &mem_list_head);
	}
	/* store the entry */
	mem->used = 1;
	mem->buffer = *dmab;
	up(&list_mutex);
	return 0;
}

/*
 * purge all reserved buffers
 */
static void free_all_reserved_pages(void)
{
	struct list_head *p;
	struct snd_mem_list *mem;

	down(&list_mutex);
	while (! list_empty(&mem_list_head)) {
		p = mem_list_head.next;
		mem = list_entry(p, struct snd_mem_list, list);
		list_del(p);
		snd_dma_free_pages(&mem->dev, &mem->buffer);
		kfree(mem);
	}
	up(&list_mutex);
}


/*
 *
 *  Generic memory allocators
 *
 */

static long snd_allocated_pages; /* holding the number of allocated pages */

static void mark_pages(void *res, int order)
{
	struct page *page = virt_to_page(res);
	struct page *last_page = page + (1 << order);
	while (page < last_page)
		SetPageReserved(page++);
	snd_allocated_pages += 1 << order;
}

static void unmark_pages(void *res, int order)
{
	struct page *page = virt_to_page(res);
	struct page *last_page = page + (1 << order);
	while (page < last_page)
		ClearPageReserved(page++);
	snd_allocated_pages -= 1 << order;
}

/**
 * snd_malloc_pages - allocate pages with the given size
 * @size: the size to allocate in bytes
 * @gfp_flags: the allocation conditions, GFP_XXX
 *
 * Allocates the physically contiguous pages with the given size.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_pages(size_t size, unsigned int gfp_flags)
{
	int pg;
	void *res;

	snd_assert(size > 0, return NULL);
	snd_assert(gfp_flags != 0, return NULL);
	for (pg = 0; PAGE_SIZE * (1 << pg) < size; pg++);
	if ((res = (void *) __get_free_pages(gfp_flags, pg)) != NULL) {
		mark_pages(res, pg);
	}
	return res;
}

/**
 * snd_malloc_pages_fallback - allocate pages with the given size with fallback
 * @size: the requested size to allocate in bytes
 * @gfp_flags: the allocation conditions, GFP_XXX
 * @res_size: the pointer to store the size of buffer actually allocated
 *
 * Allocates the physically contiguous pages with the given request
 * size.  When no space is left, this function reduces the size and
 * tries to allocate again.  The size actually allocated is stored in
 * res_size argument.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_pages_fallback(size_t size, unsigned int gfp_flags, size_t *res_size)
{
	void *res;

	snd_assert(size > 0, return NULL);
	snd_assert(res_size != NULL, return NULL);
	do {
		if ((res = snd_malloc_pages(size, gfp_flags)) != NULL) {
			*res_size = size;
			return res;
		}
		size >>= 1;
	} while (size >= PAGE_SIZE);
	return NULL;
}

/**
 * snd_free_pages - release the pages
 * @ptr: the buffer pointer to release
 * @size: the allocated buffer size
 *
 * Releases the buffer allocated via snd_malloc_pages().
 */
void snd_free_pages(void *ptr, size_t size)
{
	int pg;

	if (ptr == NULL)
		return;
	for (pg = 0; PAGE_SIZE * (1 << pg) < size; pg++);
	unmark_pages(ptr, pg);
	free_pages((unsigned long) ptr, pg);
}

#if defined(CONFIG_ISA) && ! defined(CONFIG_PCI)

/**
 * snd_malloc_isa_pages - allocate pages for ISA bus with the given size
 * @size: the size to allocate in bytes
 * @dma_addr: the pointer to store the physical address of the buffer
 *
 * Allocates the physically contiguous pages with the given size for
 * ISA bus.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_isa_pages(size_t size, dma_addr_t *dma_addr)
{
	void *dma_area;
	dma_area = snd_malloc_pages(size, GFP_ATOMIC|GFP_DMA);
	*dma_addr = dma_area ? isa_virt_to_bus(dma_area) : 0UL;
	return dma_area;
}

/**
 * snd_malloc_isa_pages_fallback - allocate pages with the given size with fallback for ISA bus
 * @size: the requested size to allocate in bytes
 * @dma_addr: the pointer to store the physical address of the buffer
 * @res_size: the pointer to store the size of buffer actually allocated
 *
 * Allocates the physically contiguous pages with the given request
 * size for PCI bus.  When no space is left, this function reduces the size and
 * tries to allocate again.  The size actually allocated is stored in
 * res_size argument.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_isa_pages_fallback(size_t size,
				    dma_addr_t *dma_addr,
				    size_t *res_size)
{
	void *dma_area;
	dma_area = snd_malloc_pages_fallback(size, GFP_ATOMIC|GFP_DMA, res_size);
	*dma_addr = dma_area ? isa_virt_to_bus(dma_area) : 0UL;
	return dma_area;
}

#endif /* CONFIG_ISA && !CONFIG_PCI */

#ifdef CONFIG_PCI

/**
 * snd_malloc_pci_pages - allocate pages for PCI bus with the given size
 * @pci: the pci device pointer
 * @size: the size to allocate in bytes
 * @dma_addr: the pointer to store the physical address of the buffer
 *
 * Allocates the physically contiguous pages with the given size for
 * PCI bus.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_pci_pages(struct pci_dev *pci,
			   size_t size,
			   dma_addr_t *dma_addr)
{
	int pg;
	void *res;

	snd_assert(size > 0, return NULL);
	snd_assert(dma_addr != NULL, return NULL);
	for (pg = 0; PAGE_SIZE * (1 << pg) < size; pg++);
	res = pci_alloc_consistent(pci, PAGE_SIZE * (1 << pg), dma_addr);
	if (res != NULL) {
		mark_pages(res, pg);
	}
	return res;
}

/**
 * snd_malloc_pci_pages_fallback - allocate pages with the given size with fallback for PCI bus
 * @pci: pci device pointer
 * @size: the requested size to allocate in bytes
 * @dma_addr: the pointer to store the physical address of the buffer
 * @res_size: the pointer to store the size of buffer actually allocated
 *
 * Allocates the physically contiguous pages with the given request
 * size for PCI bus.  When no space is left, this function reduces the size and
 * tries to allocate again.  The size actually allocated is stored in
 * res_size argument.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_pci_pages_fallback(struct pci_dev *pci,
				    size_t size,
				    dma_addr_t *dma_addr,
				    size_t *res_size)
{
	void *res;

	snd_assert(res_size != NULL, return NULL);
	do {
		if ((res = snd_malloc_pci_pages(pci, size, dma_addr)) != NULL) {
			*res_size = size;
			return res;
		}
		size >>= 1;
	} while (size >= PAGE_SIZE);
	return NULL;
}

/**
 * snd_free_pci_pages - release the pages
 * @pci: pci device pointer
 * @size: the allocated buffer size
 * @ptr: the buffer pointer to release
 * @dma_addr: the physical address of the buffer
 *
 * Releases the buffer allocated via snd_malloc_pci_pages().
 */
void snd_free_pci_pages(struct pci_dev *pci,
			size_t size,
			void *ptr,
			dma_addr_t dma_addr)
{
	int pg;

	if (ptr == NULL)
		return;
	for (pg = 0; PAGE_SIZE * (1 << pg) < size; pg++);
	unmark_pages(ptr, pg);
	pci_free_consistent(pci, PAGE_SIZE * (1 << pg), ptr, dma_addr);
}


#if defined(__i386__)
/*
 * on ix86, we allocate a page with GFP_KERNEL to assure the
 * allocation.  the code is almost same with kernel/i386/pci-dma.c but
 * it allocates only a single page and checks the validity of the
 * page address with the given pci dma mask.
 */

/**
 * snd_malloc_pci_page - allocate a page in the valid pci dma mask
 * @pci: pci device pointer
 * @addrp: the pointer to store the physical address of the buffer
 *
 * Allocates a single page for the given PCI device and returns
 * the virtual address and stores the physical address on addrp.
 * 
 * This function cannot be called from interrupt handlers or
 * within spinlocks.
 */
void *snd_malloc_pci_page(struct pci_dev *pci, dma_addr_t *addrp)
{
	void *ptr;
	dma_addr_t addr;
	unsigned long mask;

	mask = pci ? (unsigned long)pci->consistent_dma_mask : 0x00ffffffUL;
	ptr = (void *)__get_free_page(GFP_KERNEL);
	if (ptr) {
		addr = virt_to_phys(ptr);
		if (((unsigned long)addr + PAGE_SIZE - 1) & ~mask) {
			/* try to reallocate with the GFP_DMA */
			free_page((unsigned long)ptr);
			/* use GFP_ATOMIC for the DMA zone to avoid stall */
			ptr = (void *)__get_free_page(GFP_ATOMIC | GFP_DMA);
			if (ptr) /* ok, the address must be within lower 16MB... */
				addr = virt_to_phys(ptr);
			else
				addr = 0;
		}
	} else
		addr = 0;
	if (ptr) {
		memset(ptr, 0, PAGE_SIZE);
		mark_pages(ptr, 0);
	}
	*addrp = addr;
	return ptr;
}
#else

/* on other architectures, call snd_malloc_pci_pages() helper function
 * which uses pci_alloc_consistent().
 */
void *snd_malloc_pci_page(struct pci_dev *pci, dma_addr_t *addrp)
{
	return snd_malloc_pci_pages(pci, PAGE_SIZE, addrp);
}

#endif

#if 0 /* for kernel-doc */
/**
 * snd_free_pci_page - release a page
 * @pci: pci device pointer
 * @ptr: the buffer pointer to release
 * @dma_addr: the physical address of the buffer
 *
 * Releases the buffer allocated via snd_malloc_pci_page().
 */
void snd_free_pci_page(struct pci_dev *pci, void *ptr, dma_addr_t dma_addr);
#endif /* for kernel-doc */

#endif /* CONFIG_PCI */

#ifdef CONFIG_SBUS

/**
 * snd_malloc_sbus_pages - allocate pages for SBUS with the given size
 * @sdev: sbus device pointer
 * @size: the size to allocate in bytes
 * @dma_addr: the pointer to store the physical address of the buffer
 *
 * Allocates the physically contiguous pages with the given size for
 * SBUS.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_sbus_pages(struct sbus_dev *sdev,
			    size_t size,
			    dma_addr_t *dma_addr)
{
	int pg;
	void *res;

	snd_assert(size > 0, return NULL);
	snd_assert(dma_addr != NULL, return NULL);
	for (pg = 0; PAGE_SIZE * (1 << pg) < size; pg++);
	res = sbus_alloc_consistent(sdev, PAGE_SIZE * (1 << pg), dma_addr);
	if (res != NULL) {
		mark_pages(res, pg);
	}
	return res;
}

/**
 * snd_malloc_pci_pages_fallback - allocate pages with the given size with fallback for SBUS
 * @sdev: sbus device pointer
 * @size: the requested size to allocate in bytes
 * @dma_addr: the pointer to store the physical address of the buffer
 * @res_size: the pointer to store the size of buffer actually allocated
 *
 * Allocates the physically contiguous pages with the given request
 * size for SBUS.  When no space is left, this function reduces the size and
 * tries to allocate again.  The size actually allocated is stored in
 * res_size argument.
 *
 * Returns the pointer of the buffer, or NULL if no enoguh memory.
 */
void *snd_malloc_sbus_pages_fallback(struct sbus_dev *sdev,
				     size_t size,
				     dma_addr_t *dma_addr,
				     size_t *res_size)
{
	void *res;

	snd_assert(res_size != NULL, return NULL);
	do {
		if ((res = snd_malloc_sbus_pages(sdev, size, dma_addr)) != NULL) {
			*res_size = size;
			return res;
		}
		size >>= 1;
	} while (size >= PAGE_SIZE);
	return NULL;
}

/**
 * snd_free_sbus_pages - release the pages
 * @sdev: sbus device pointer
 * @size: the allocated buffer size
 * @ptr: the buffer pointer to release
 * @dma_addr: the physical address of the buffer
 *
 * Releases the buffer allocated via snd_malloc_pci_pages().
 */
void snd_free_sbus_pages(struct sbus_dev *sdev,
			 size_t size,
			 void *ptr,
			 dma_addr_t dma_addr)
{
	int pg;

	if (ptr == NULL)
		return;
	for (pg = 0; PAGE_SIZE * (1 << pg) < size; pg++);
	unmark_pages(ptr, pg);
	sbus_free_consistent(sdev, PAGE_SIZE * (1 << pg), ptr, dma_addr);
}

#endif /* CONFIG_SBUS */


/*
 * allocation of buffers for pre-defined devices
 */

#ifdef CONFIG_PCI
/* FIXME: for pci only - other bus? */
struct prealloc_dev {
	unsigned short vendor;
	unsigned short device;
	unsigned long dma_mask;
	unsigned int size;
	unsigned int buffers;
};

#define HAMMERFALL_BUFFER_SIZE    (16*1024*4*(26+1))

static struct prealloc_dev prealloc_devices[] __initdata = {
	{
		/* hammerfall */
		.vendor = 0x10ee,
		.device = 0x3fc4,
		.dma_mask = 0xffffffff,
		.size = HAMMERFALL_BUFFER_SIZE,
		.buffers = 2
	},
	{
		/* HDSP */
		.vendor = 0x10ee,
		.device = 0x3fc5,
		.dma_mask = 0xffffffff,
		.size = HAMMERFALL_BUFFER_SIZE,
		.buffers = 2
	},
	{ }, /* terminator */
};

static void __init preallocate_cards(void)
{
	struct pci_dev *pci = NULL;
	int card;

	card = 0;

	while ((pci = pci_find_device(PCI_ANY_ID, PCI_ANY_ID, pci)) != NULL) {
		struct prealloc_dev *dev;
		unsigned int i;
		if (card >= SNDRV_CARDS)
			break;
		for (dev = prealloc_devices; dev->vendor; dev++) {
			if (dev->vendor == pci->vendor && dev->device == pci->device)
				break;
		}
		if (! dev->vendor)
			continue;
		if (! enable[card++]) {
			printk(KERN_DEBUG "snd-page-alloc: skipping card %d, device %04x:%04x\n", card, pci->vendor, pci->device);
			continue;
		}
			
		if (pci_set_dma_mask(pci, dev->dma_mask) < 0 ||
		    pci_set_consistent_dma_mask(pci, dev->dma_mask) < 0) {
			printk(KERN_ERR "snd-page-alloc: cannot set DMA mask %lx for pci %04x:%04x\n", dev->dma_mask, dev->vendor, dev->device);
			continue;
		}
		for (i = 0; i < dev->buffers; i++) {
			struct snd_mem_list *mem;
			mem = kmalloc(sizeof(*mem), GFP_KERNEL);
			if (! mem) {
				printk(KERN_WARNING "snd-page-alloc: can't malloc memlist\n");
				break;
			}
			memset(mem, 0, sizeof(*mem));
			snd_dma_device_pci(&mem->dev, pci, SNDRV_DMA_DEVICE_UNUSED);
			if (snd_dma_alloc_pages(&mem->dev, dev->size, &mem->buffer) < 0) {
				printk(KERN_WARNING "snd-page-alloc: cannot allocate buffer pages (size = %d)\n", dev->size);
				kfree(mem);
			} else {
				down(&list_mutex);
				list_add_tail(&mem->list, &mem_list_head);
				up(&list_mutex);
			}
		}
	}
}
#else
#define preallocate_cards()	/* NOP */
#endif


#ifdef CONFIG_PROC_FS
/*
 * proc file interface
 */
static int snd_mem_proc_read(char *page, char **start, off_t off,
			     int count, int *eof, void *data)
{
	int len = 0;
	long pages = snd_allocated_pages >> (PAGE_SHIFT-12);
	struct list_head *p;
	struct snd_mem_list *mem;
	int devno;

	down(&list_mutex);
	len += sprintf(page + len, "pages  : %li bytes (%li pages per %likB)\n",
		       pages * PAGE_SIZE, pages, PAGE_SIZE / 1024);
	devno = 0;
	list_for_each(p, &mem_list_head) {
		mem = list_entry(p, struct snd_mem_list, list);
		devno++;
		len += sprintf(page + len, "buffer %d : ", devno);
		if (mem->dev.id == SNDRV_DMA_DEVICE_UNUSED)
			len += sprintf(page + len, "UNUSED");
		else
			len += sprintf(page + len, "ID %08x", mem->dev.id);
		len += sprintf(page + len, " : type ");
		switch (mem->dev.type) {
		case SNDRV_DMA_TYPE_CONTINUOUS:
			len += sprintf(page + len, "CONT [%x]", mem->dev.dev.flags);
			break;
#ifdef CONFIG_PCI
		case SNDRV_DMA_TYPE_PCI:
		case SNDRV_DMA_TYPE_PCI_SG:
			if (mem->dev.dev.pci) {
				len += sprintf(page + len, "%s [%04x:%04x]",
					       mem->dev.type == SNDRV_DMA_TYPE_PCI ? "PCI" : "PCI-SG",
					       mem->dev.dev.pci->vendor,
					       mem->dev.dev.pci->device);
			}
			break;
#endif
#ifdef CONFIG_ISA
		case SNDRV_DMA_TYPE_ISA:
			len += sprintf(page + len, "ISA [%x]", mem->dev.dev.flags);
			break;
#endif
#ifdef CONFIG_SBUS
		case SNDRV_DMA_TYPE_SBUS:
			len += sprintf(page + len, "SBUS [%x]", mem->dev.dev.sbus->slot);
			break;
#endif
		default:
			len += sprintf(page + len, "UNKNOWN");
			break;
		}
		len += sprintf(page + len, "\n  addr = 0x%lx, size = %d bytes, used = %s\n",
			       (unsigned long)mem->buffer.addr, (int)mem->buffer.bytes,
			       mem->used ? "yes" : "no");
	}
	up(&list_mutex);
	return len;
}
#endif /* CONFIG_PROC_FS */

/*
 * module entry
 */

static int __init snd_mem_init(void)
{
#ifdef CONFIG_PROC_FS
	create_proc_read_entry("driver/snd-page-alloc", 0, 0, snd_mem_proc_read, NULL);
#endif
	preallocate_cards();
	return 0;
}

static void __exit snd_mem_exit(void)
{
	remove_proc_entry("driver/snd-page-alloc", NULL);
	free_all_reserved_pages();
	if (snd_allocated_pages > 0)
		printk(KERN_ERR "snd-malloc: Memory leak?  pages not freed = %li\n", snd_allocated_pages);
}


module_init(snd_mem_init)
module_exit(snd_mem_exit)


#ifndef MODULE

/* format is: snd-page-alloc=enable */

static int __init snd_mem_setup(char *str)
{
	static unsigned __initdata nr_dev = 0;

	if (nr_dev >= SNDRV_CARDS)
		return 0;
	(void)(get_option(&str,&enable[nr_dev]) == 2);
	nr_dev++;
	return 1;
}

__setup("snd-page-alloc=", snd_mem_setup);

#endif

/*
 * exports
 */
EXPORT_SYMBOL(snd_dma_alloc_pages);
EXPORT_SYMBOL(snd_dma_free_pages);
EXPORT_SYMBOL(snd_dma_get_reserved);
EXPORT_SYMBOL(snd_dma_free_reserved);
EXPORT_SYMBOL(snd_dma_set_reserved);

EXPORT_SYMBOL(snd_malloc_pages);
EXPORT_SYMBOL(snd_malloc_pages_fallback);
EXPORT_SYMBOL(snd_free_pages);
#if defined(CONFIG_ISA) && ! defined(CONFIG_PCI)
EXPORT_SYMBOL(snd_malloc_isa_pages);
EXPORT_SYMBOL(snd_malloc_isa_pages_fallback);
#endif
#ifdef CONFIG_PCI
EXPORT_SYMBOL(snd_malloc_pci_pages);
EXPORT_SYMBOL(snd_malloc_pci_pages_fallback);
EXPORT_SYMBOL(snd_malloc_pci_page);
EXPORT_SYMBOL(snd_free_pci_pages);
EXPORT_SYMBOL(snd_malloc_sgbuf_pages);
EXPORT_SYMBOL(snd_free_sgbuf_pages);
#endif
#ifdef CONFIG_SBUS
EXPORT_SYMBOL(snd_malloc_sbus_pages);
EXPORT_SYMBOL(snd_malloc_sbus_pages_fallback);
EXPORT_SYMBOL(snd_free_sbus_pages);
#endif
