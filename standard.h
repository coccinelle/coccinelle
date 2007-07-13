/* this file contains:
 *   - macros found in <.h>
 *   - macros found in ".h"
 *   - macros found in .c but where we cant detect
 *     that it will be a "bad macro"
 *   - macros found in .c but that cannot be parsed and so
 *     where in the futur should be autodetected
 *     (which is not as easy for macros in .h cos require
 *     to access .h file)
 */
// #define FOO(a, OP, b) a OP b
#define FOO(a,b) fn(a,b)


// #define CONST 
// #define _attribute__const __attribute__((const))
#define __init_refok




// include/asm-arm/mach/arch.h
// #define MACHINE_START(x) struct foo { x }
#define MACHINE_START(_type,_name)			\
static const struct machine_desc __mach_desc_##_type	\
/* __used*/							\
 __attribute__((__section__(".arch.info.init"))) = {	\
	.nr		= MACH_TYPE_##_type,		\
	.name		= _name,

#define MACHINE_END				\
};

// include/asm-powerpc/machdep.h
#define define_machine(name)                                    \
         extern struct machdep_calls mach_##name;                \
         EXPORT_SYMBOL(mach_##name);                             \
         struct machdep_calls mach_##name /*__machine_desc*/ =





// ?
#define CS_CHECK(fn, ret) \
  do { last_fn = (fn); if ((last_ret = (ret)) != 0) goto cs_failed; } while (0)




// sound/oss/cs46xx_wrapper-24.h
#define CS_OWNER .owner =
#define CS_THIS_MODULE THIS_MODULE,



// drivers/media/video/sn9c102/sn9c102_sensor.h
//#define sn9c102_write_const_regs(sn9c102_device, data...)                     \
//	({ const static u8 _valreg[][2] = {data};                             \
//	sn9c102_write_regs(sn9c102_device, _valreg, ARRAY_SIZE(_valreg)); })





// drivers/s390/cio/qdio.h
#define SYNC_MEMORY if (unlikely(q->siga_sync)) qdio_siga_sync_q(q)
#define SYNC_MEMORY_ALL if (unlikely(q->siga_sync)) \
	qdio_siga_sync(q,~0U,~0U)
#define SYNC_MEMORY_ALL_OUTB if (unlikely(q->siga_sync)) \
	qdio_siga_sync(q,~0U,0)

// drivers/scsi/g_NCR5380.c
#define ANDP ,



// drivers/video/nvidia/nv_type.h
// use:	SetBitField(h_blank_e, 5: 5, 7:7)
//#define BITMASK(t,b) (((unsigned)(1U << (((t)-(b)+1)))-1)  << (b))
//#define MASKEXPAND(mask) BITMASK(1?mask,0?mask)
//#define SetBF(mask,value) ((value) << (0?mask))
//#define GetBF(var,mask) (((unsigned)((var) & MASKEXPAND(mask))) >> (0?mask) )
//#define SetBitField(value,from,to) SetBF(to, GetBF(value,from))
//#define SetBit(n) (1<<(n))
//#define Set8Bits(value) ((value)&0xff)


// drivers/video/sis/init.c
// use: GETBITSTR((SiS_Pr->CVTotal     -2), 10:10, 0:0)
//#define BITMASK(h,l)    	(((unsigned)(1U << ((h)-(l)+1))-1)<<(l))
//#define GENMASK(mask)   	BITMASK(1?mask,0?mask)
//#define GETBITS(var,mask)   	(((var) & GENMASK(mask)) >> (0?mask))
//#define GETBITSTR(val,from,to)  ((GETBITS(val,from)) << (0?to))


// fs/afs/internal.h
#define ASSERTCMP(X, OP, Y)						\
do {									\
	if (unlikely(!((X) OP (Y)))) {					\
		printk(KERN_ERR "\n");					\
		printk(KERN_ERR "AFS: Assertion failed\n");		\
		printk(KERN_ERR "%lu " /*#OP*/ " %lu is false\n",		\
		       (unsigned long)(X), (unsigned long)(Y));		\
		printk(KERN_ERR "0x%lx " /*#OP*/ " 0x%lx is false\n",	\
		       (unsigned long)(X), (unsigned long)(Y));		\
		BUG();							\
	}								\
} while(0)

#define ASSERTIFCMP(C, X, OP, Y)					\
do {									\
	if (unlikely((C) && !((X) OP (Y)))) {				\
		printk(KERN_ERR "\n");					\
		printk(KERN_ERR "AFS: Assertion failed\n");		\
		printk(KERN_ERR "%lu " /*#OP*/ " %lu is false\n",		\
		       (unsigned long)(X), (unsigned long)(Y));		\
		printk(KERN_ERR "0x%lx " /*#OP*/ " 0x%lx is false\n",	\
		       (unsigned long)(X), (unsigned long)(Y));		\
		BUG();							\
	}								\
} while(0)


#define ASSERTRANGE(L, OP1, N, OP2, H)					\
do {									\
	if (unlikely(!((L) OP1 (N)) || !((N) OP2 (H)))) {		\
		printk(KERN_ERR "\n");					\
		printk(KERN_ERR "AFS: Assertion failed\n");		\
		printk(KERN_ERR "%lu "/*#OP1*/" %lu "/*#OP2*/" %lu is false\n",	\
		       (unsigned long)(L), (unsigned long)(N),		\
		       (unsigned long)(H));				\
		printk(KERN_ERR "0x%lx "/*#OP1*/" 0x%lx "/*#OP2*/" 0x%lx is false\n", \
		       (unsigned long)(L), (unsigned long)(N),		\
		       (unsigned long)(H));				\
		BUG();							\
	}								\
} while(0)





// include/linux/sched.h
#define while_each_thread(g, t) \
        while ((t = next_thread(t)) != g)





// net/decnet/dn_fib.c
#define for_fib_info() { struct dn_fib_info *fi;\
	for(fi = dn_fib_info_list; fi; fi = fi->fib_next)
#define endfor_fib_info() }

#define for_nexthops(fi) { int nhsel; const struct dn_fib_nh *nh;\
	for(nhsel = 0, nh = (fi)->fib_nh; nhsel < (fi)->fib_nhs; nh++, nhsel++)

#define change_nexthops(fi) { int nhsel; struct dn_fib_nh *nh;\
	for(nhsel = 0, nh = (struct dn_fib_nh *)((fi)->fib_nh); nhsel < (fi)->fib_nhs; nh++, nhsel++)

#define endfor_nexthops(fi) }


// include/asm-i386/percpu.h
//#define DECLARE_PER_CPU(type, name) extern __typeof__(type) per_cpu__##name
#define DEFINE_PER_CPU(type, name) \
    __attribute__((__section__(".data.percpu"))) /*__typeof__(*/type/*)*/ per_cpu__##name




// net/ipv4/netfilter/ip_conntrack_helper_h323_asn1.c
// also used in other.c that don't do any include :( 
// but locally redefined in drivers/net/bnx2.c :( with a 
// #define FNAME	0x8
#define FNAME(name) name,


// drivers/net/tulip/de4x5.c
#define DESC_ALIGN


// net/sched/em_meta.c
#define META_COLLECTOR(FUNC) static void meta_##FUNC(struct sk_buff *skb, \
	struct tcf_pkt_info *info, struct meta_value *v, \
	struct meta_obj *dst, int *err)




// include/linux/lockd/debug.h
// include/linux/nfs_fs.h
// include/linux/nfsd/debug.h
// include/linux/sunrpc/debug.h
//#define ifdebug(flag)          if (unlikely(nlm_debug & NLMDBG_##flag))
#define ifdebug(flag)          if (0)



