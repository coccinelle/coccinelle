/*
 * OHCI HCD (Host Controller Driver) for USB.
 * 
 * (C) Copyright 1999 Roman Weissgaerber <weissg@vienna.at>
 * (C) Copyright 2000-2002 David Brownell <dbrownell@users.sourceforge.net>
 * 
 * This file is licenced under the GPL.
 */

/*-------------------------------------------------------------------------*/

/*
 * There's basically three types of memory:
 *	- data used only by the HCD ... kmalloc is fine
 *	- async and periodic schedules, shared by HC and HCD ... these
 *	  need to use pci_pool or pci_alloc_consistent
 *	- driver buffers, read/written by HC ... single shot DMA mapped 
 *
 * There's also PCI "register" data, which is memory mapped.
 * No memory seen by this driver is pagable.
 */

/*-------------------------------------------------------------------------*/

static struct usb_hcd *ohci_hcd_alloc (void)
{
	struct ohci_hcd *ohci;

	ohci = (struct ohci_hcd *) kmalloc (sizeof *ohci, GFP_KERNEL);
	if (ohci != 0) {
		memset (ohci, 0, sizeof (struct ohci_hcd));
		return &ohci->hcd;
	}
	return 0;
}

static void ohci_hcd_free (struct usb_hcd *hcd)
{
	kfree (hcd_to_ohci (hcd));
}

/*-------------------------------------------------------------------------*/

/* Recover a TD/ED using its collision chain */
static inline void *
dma_to_ed_td (struct hash_list_t * entry, dma_addr_t dma)
{
	struct hash_t * scan = entry->head;
	while (scan && scan->dma != dma)
		scan = scan->next;
	return scan ? scan->virt : 0;
}

static inline struct td *
dma_to_td (struct ohci_hcd *hc, dma_addr_t td_dma)
{
	td_dma &= TD_MASK;
	return (struct td *) dma_to_ed_td(&(hc->td_hash [TD_HASH_FUNC(td_dma)]),
				      td_dma);
}

// FIXME:  when updating the hashtables this way, mem_flags is unusable...

/* Add a hash entry for a TD/ED; return true on success */
static inline int
hash_add_ed_td (
	struct hash_list_t *entry,
	void *virt,
	dma_addr_t dma,
	int mem_flags
)
{
	struct hash_t * scan;
	
	scan = (struct hash_t *) kmalloc (sizeof *scan, mem_flags);
	if (!scan)
		return 0;
	
	if (!entry->tail) {
		entry->head = entry->tail = scan;
	} else {
		entry->tail->next = scan;
		entry->tail = scan;
	}
	
	scan->virt = virt;
	scan->dma = dma;
	scan->next = NULL;
	return 1;
}

static inline int
hash_add_td (struct ohci_hcd *hc, struct td *td, int mem_flags)
{
	return hash_add_ed_td (&(hc->td_hash [TD_HASH_FUNC (td->td_dma)]),
			td, td->td_dma, mem_flags);
}


static inline void
hash_free_ed_td (struct hash_list_t *entry, void *virt)
{
	struct hash_t *scan, *prev;
	scan = prev = entry->head;

	// Find and unlink hash entry
	while (scan && scan->virt != virt) {
		prev = scan;
		scan = scan->next;
	}
	if (scan) {
		if (scan == entry->head) {
			if (entry->head == entry->tail)
				entry->head = entry->tail = NULL;
			else
				entry->head = scan->next;
		} else if (scan == entry->tail) {
			entry->tail = prev;
			prev->next = NULL;
		} else
			prev->next = scan->next;
		kfree(scan);
	}
}

static inline void
hash_free_td (struct ohci_hcd *hc, struct td * td)
{
	hash_free_ed_td (&(hc->td_hash[TD_HASH_FUNC(td->td_dma)]), td);
}


static int ohci_mem_init (struct ohci_hcd *ohci)
{
	ohci->td_cache = pci_pool_create ("ohci_td", ohci->hcd.pdev,
		sizeof (struct td),
		32 /* byte alignment */,
		0 /* no page-crossing issues */);
	if (!ohci->td_cache)
		return -ENOMEM;
	ohci->ed_cache = pci_pool_create ("ohci_ed", ohci->hcd.pdev,
		sizeof (struct ed),
		16 /* byte alignment */,
		0 /* no page-crossing issues */);
	if (!ohci->ed_cache) {
		pci_pool_destroy (ohci->td_cache);
		return -ENOMEM;
	}
	return 0;
}

static void ohci_mem_cleanup (struct ohci_hcd *ohci)
{
	if (ohci->td_cache) {
		pci_pool_destroy (ohci->td_cache);
		ohci->td_cache = 0;
	}
	if (ohci->ed_cache) {
		pci_pool_destroy (ohci->ed_cache);
		ohci->ed_cache = 0;
	}
}

/* TDs ... */
static struct td *
td_alloc (struct ohci_hcd *hc, int mem_flags)
{
	dma_addr_t	dma;
	struct td	*td;

	td = pci_pool_alloc (hc->td_cache, mem_flags, &dma);
	if (td) {
		td->td_dma = dma;
		/* hash it for later reverse mapping */
		if (!hash_add_td (hc, td, mem_flags)) {
			pci_pool_free (hc->td_cache, td, dma);
			return NULL;
		}
	}
	return td;
}

static void
td_free (struct ohci_hcd *hc, struct td *td)
{
	hash_free_td (hc, td);
	pci_pool_free (hc->td_cache, td, td->td_dma);
}


/* EDs ... */
static struct ed *
ed_alloc (struct ohci_hcd *hc, int mem_flags)
{
	dma_addr_t	dma;
	struct ed	*ed;

	ed = pci_pool_alloc (hc->ed_cache, mem_flags, &dma);
	if (ed) {
		memset (ed, 0, sizeof (*ed));
		INIT_LIST_HEAD (&ed->td_list);
		ed->dma = dma;
	}
	return ed;
}

static void
ed_free (struct ohci_hcd *hc, struct ed *ed)
{
	pci_pool_free (hc->ed_cache, ed, ed->dma);
}

