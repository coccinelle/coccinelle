typedef int u32;
struct acpi_bus_ops {
        u32 acpi_op_add:1;
        u32 acpi_op_remove:1;
        u32 acpi_op_lock:1;
        u32 acpi_op_start:1;
        u32 acpi_op_stop:1;
        u32 acpi_op_suspend:1;
        u32 acpi_op_resume:1;
        u32 acpi_op_scan:1;
        u32 acpi_op_bind:1;
        u32 acpi_op_unbind:1;
        u32 acpi_op_match:1;
        u32 reserved:21;
};


struct mod_arch_specific
{
};


typedef unsigned int __u32;
typedef unsigned int __u16;


static  __u16 __swab16p(const __u16 *x)
{
        return ({ 
          __u16 __tmp = (*(x)) ; 
          ({ 
            __u16 __x = (__tmp); 
               (
//                (__u16)
                ( 
                 //                 (((__u16)(__x) & 0x00ffU) << 8)
                 ((((__x)) & 0x00ffU) << 8)
                 // 2
//                 | 
//                 (((__u16)(__x) & 0xff00U) >> 8) 
                 )
                ); 
          }); 
        });
}


static    __u16 __fswab16(__u16 x)
{
  
  int i = 0x00ffU;
  return ({ 
    __u16 __tmp = (x) ; 
    ({ 
      __u16 __x = (__tmp); 
      ((__u16)( (((__u16)(__x) & 
                  /*(__u16)*/0x00ffU
                  ) << 8) 
                | (((__u16)(__x) & 
                    /*(__u16)*/0xff00U
                    ) >> 8) 
                )); 
    }); 
  });

}






typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);

typedef int __u64;


static initcall_t __initcall_acpi_ibm_init     = acpi_ibm_init;;





# 1027 "drivers/cdrom/gscd.c"

# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stdarg.h" 1 3 4
# 43 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stdarg.h" 3 4

static  unsigned long generic_hweight64(__u64 w)
{

        return generic_hweight32((unsigned int)(w >> 32)) +
                                generic_hweight32((unsigned int)w);
# 130 "include/linux/bitops.h"
}


static  short *__check_gscd(void) { return(&(gscd_port)); }; 
static char __param_str_gscd[] = "gscd"; 
static struct kernel_param const __param_gscd     = { __param_str_gscd, 0, param_set_short, param_get_short, &gscd_port }; 
static const char __mod_gscdtype80[]     = "parmtype" "=" "gscd" ":" "short";


static  void console_silent(void)
{
        (console_printk[0]) = 0;
}

typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);

# 1027 "drivers/cdrom/gscd.c"
static const char __mod_author1027[]     = "author" "=" "Oliver Raupach <raupach@nwfs1.rz.fh-hannover.de>";
static const char __mod_license1028[]     = "license" "=" "GPL";
static  exitcall_t __exittest() { return gscd_exit; } 
void cleanup_module(void)  ;
static  initcall_t __inittest() { return gscd_init; } 
int init_module(void)  ;
static const char __mod_alias1031[]     = "alias" "=" "block-major-" "16" "-*";


# 1027 "drivers/cdrom/gscd.c"



register unsigned long current_stack_pointer asm("esp")  ;

static  size_t strnlen(const char * s, size_t count)
{
int d0;
register int __res;
__asm__ (
        "movl %2,%0\n\t"
        "jmp 2f\n"
        "1:\tcmpb $0,(%0)\n\t"
        "je 3f\n\t"
        "incl %0\n"
        "2:\tdecl %1\n\t"
        "cmpl $-1,%1\n\t"
        "jne 1b\n"
        "3:\tsubl %2,%0"
        :"=a" (__res), "=&d" (d0)
        :"c" (s),"1" (count)
        :"memory");
return __res;
}


static __inline__  __u32 ___arch__swab32(__u32 x) 
{
}

static    __u16 __fswab16(__u16 x)
{
        return ({ __u16 __tmp = (x) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); });
}

static __inline__ __attribute__((__const__)) __u32 ___arch__swab32(__u32 x)
{



        __asm__("xchgb %b0,%h0\n\t"
                "rorl $16,%0\n\t"
                "xchgb %b0,%h0"
                :"=q" (x)
                : "0" (x));

        return x;
}


static __inline__ __attribute__((__const__)) __u32 ___arch__swab32(__u32 x)
{



        __asm__("xchgb %b0,%h0\n\t"
                "rorl $16,%0\n\t"
                "xchgb %b0,%h0"
                :"=q" (x)
                : "0" (x));

        return x;
}

# 1027 "drivers/cdrom/gscd.c"

static inline int find_first_zero_bit(const unsigned long *addr, unsigned size)
{
        int d0, d1, d2;
        int res;

        if (!size)
                return 0;

        __asm__ __volatile__(
                "movl $-1,%%eax\n\t"
                "xorl %%edx,%%edx\n\t"
                "repe; scasl\n\t"
                "je 1f\n\t"
                "xorl -4(%%edi),%%eax\n\t"
                "subl $4,%%edi\n\t"
                "bsfl %%eax,%%edx\n"
                "1:\tsubl %%ebx,%%edi\n\t"
                "shll $3,%%edi\n\t"
                "addl %%edi,%%edx"
                :"=d" (res), "=&c" (d0), "=&D" (d1), "=&a" (d2)
                :"1" ((size + 31) >> 5), "2" (addr), "b" (addr) : "memory");
        return res;
}


typedef __builtin_va_list __gnuc_va_list;
typedef struct { unsigned long pgprot; } pgprot_t;
typedef unsigned char u8;
typedef struct { } spinlock_t;


static  __u16 __swab16p(const __u16 *x)
{
        return ({ 
          __u16 __tmp = (*(x)) ; 
          ({ 
            __u16 __x = (__tmp); 
            ((__u16)( (((__u16)(__x) & /*(__u16)*/0x00ffU) << 8) | (((__u16)(__x) & /*(__u16)*/0xff00U) >> 8) )); 
          }); 
        });
}


static    __u16 __fswab16(__u16 x)
{
  
  int i = 0x00ffU;
  return ({ 
    __u16 __tmp = (x) ; 
    ({ 
      __u16 __x = (__tmp); 
      ((__u16)( (((__u16)(__x) & 
                  /*(__u16)*/0x00ffU
                  ) << 8) 
                | (((__u16)(__x) & 
                    /*(__u16)*/0xff00U
                    ) >> 8) 
                )); 
    }); 
  });

}


int zorro_register_driver(int *drv)
{
  x.open = zorro_register_driver;
  x.open = &zorro_register_driver;
  x.open = (int) &zorro_register_driver;
  x->open = (int) &zorro_register_driver;
  x->open = ({ int v;   (int)&zorro_register_driver; });
}

int __kstrtab_zorro_register_driver;


struct kernel_symbol;
static const struct kernel_symbol __ksymtab_zorro_register_driver     = 
{ (unsigned long)&zorro_register_driver, __kstrtab_zorro_register_driver };


static const struct kernel_symbol var     = { zorro_register_driver };



typedef int (*acpi_op_add) (struct acpi_device *device);

typedef int u32;
struct acpi_bus_ops {
        u32 acpi_op_add:1;
        u32 acpi_op_remove:1;
        u32 acpi_op_lock:1;
        u32 acpi_op_start:1;
        u32 acpi_op_stop:1;
        u32 acpi_op_suspend:1;
        u32 acpi_op_resume:1;
        u32 acpi_op_scan:1;
        u32 acpi_op_bind:1;
        u32 acpi_op_unbind:1;
        u32 acpi_op_match:1;
        u32 reserved:21;
};



typedef char small;
small y;

struct x { 
  small small;
};


int small;


typedef char small;
int *small;


typedef int small;

typedef int small;

struct x { 
  small *small;
};

typedef int small;

struct x { 
  small small;
};






int fonction(int small);



struct small { int x; };

