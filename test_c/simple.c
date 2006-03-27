module_param(debug, bool, 0);
module_param(autoclose, bool, 0);

int register_cdrom(struct cdrom_device_info *cdi)
{
	static char banner_printed;
        struct cdrom_device_ops *cdo = cdi->ops;
        int *change_capability = (int *)&cdo->capability; /* hack */

	cdinfo(CD_OPEN, "entering register_cdrom\n"); 

	if (cdo->open == NULL || cdo->release == NULL)
		return -2;
	if (!banner_printed) {
          printk(KERN_INFO "Uniform CD-ROM driver " REVISION "\n"); /* PAD: KERN_INFO is a string in fact */
		banner_printed = 1;
#ifdef CONFIG_SYSCTL
		cdrom_sysctl_register();
#endif /* CONFIG_SYSCTL */ 
	}

	ENSURE(drive_status, CDC_DRIVE_STATUS );
	ENSURE(media_changed, CDC_MEDIA_CHANGED);
	ENSURE(tray_move, CDC_CLOSE_TRAY | CDC_OPEN_TRAY);
	ENSURE(lock_door, CDC_LOCK);
	ENSURE(select_speed, CDC_SELECT_SPEED);
	ENSURE(get_last_session, CDC_MULTI_SESSION);
	ENSURE(get_mcn, CDC_MCN);
	ENSURE(reset, CDC_RESET);
	ENSURE(audio_ioctl, CDC_PLAY_AUDIO);
	ENSURE(dev_ioctl, CDC_IOCTLS);
	ENSURE(generic_packet, CDC_GENERIC_PACKET);
	cdi->mc_flags = 0;
	cdo->n_minors = 0;
        cdi->options = CDO_USE_FFLAGS;
	
	if (autoclose==1 && CDROM_CAN(CDC_CLOSE_TRAY))
		cdi->options |= (int) CDO_AUTO_CLOSE;
	if (autoeject==1 && CDROM_CAN(CDC_OPEN_TRAY))
		cdi->options |= (int) CDO_AUTO_EJECT;
	if (lockdoor==1)
		cdi->options |= (int) CDO_LOCK;
	if (check_media_type==1)
		cdi->options |= (int) CDO_CHECK_TYPE;

	if (CDROM_CAN(CDC_MRW_W))
		cdi->exit = cdrom_mrw_exit;

	if (cdi->disk)
		cdi->cdda_method = CDDA_BPC_FULL;
	else
		cdi->cdda_method = CDDA_OLD;

	if (!cdo->generic_packet)
		cdo->generic_packet = cdrom_dummy_generic_packet;

	cdinfo(CD_REG_UNREG, "drive \"/dev/%s\" registered\n", cdi->name);
	spin_lock(&cdrom_lock);
	cdi->next = topCdromPtr; 	
	topCdromPtr = cdi;
	spin_unlock(&cdrom_lock);
	return 0;
}


static inline void sony_sleep(void)
{
	if (cdu31a_irq <= 0) {
		yield();
	} else {		/* Interrupt driven */
               DEFINE_WAIT(w); /* PAD: there is a declaration here */
		int first = 1;

		while (1) {
			prepare_to_wait(&cdu31a_irq_wait, &w,
					TASK_INTERRUPTIBLE);
			if (first) {
				enable_interrupts();
				first = 0;
			}

			if (irq_flag != 0)
				break;
			if (!signal_pending(current)) {
				schedule();
				continue;
			} else
				disable_interrupts();
			break;
		}
		finish_wait(&cdu31a_irq_wait, &w);
		irq_flag = 0;
	}
}



static inline void outb_local(unsigned char value, int port) { __asm__ __volatile__("out" "b" " %" "b" "0, %w1" : : "a"(value), "Nd"(port)); } static inline unsigned char inb_local(int port) { unsigned char value; __asm__ __volatile__("in" "b" " %w1, %" "b" "0" : "=a"(value) : "Nd"(port)); return value; } static inline void outb_local_p(unsigned char value, int port) { outb_local(value, port); slow_down_io(); } static inline unsigned char inb_local_p(int port) { unsigned char value = inb_local(port); slow_down_io(); return value; } static inline void outb(unsigned char value, int port) { outb_local(value, port); } static inline unsigned char inb(int port) { return inb_local(port); } static inline void outb_p(unsigned char value, int port) { outb(value, port); slow_down_io(); } static inline unsigned char inb_p(int port) { unsigned char value = inb(port); slow_down_io(); return value; } static inline void outsb(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "b" : "+S"(addr), "+c"(count) : "d"(port)); } static inline void insb(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "b" : "+D"(addr), "+c"(count) : "d"(port)); }

static inline void outw_local(unsigned short value, int port) { __asm__ __volatile__("out" "w" " %" "w" "0, %w1" : : "a"(value), "Nd"(port)); } static inline unsigned short inw_local(int port) { unsigned short value; __asm__ __volatile__("in" "w" " %w1, %" "w" "0" : "=a"(value) : "Nd"(port)); return value; } static inline void outw_local_p(unsigned short value, int port) { outw_local(value, port); slow_down_io(); } static inline unsigned short inw_local_p(int port) { unsigned short value = inw_local(port); slow_down_io(); return value; } static inline void outw(unsigned short value, int port) { outw_local(value, port); } static inline unsigned short inw(int port) { return inw_local(port); } static inline void outw_p(unsigned short value, int port) { outw(value, port); slow_down_io(); } static inline unsigned short inw_p(int port) { unsigned short value = inw(port); slow_down_io(); return value; } static inline void outsw(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "w" : "+S"(addr), "+c"(count) : "d"(port)); } static inline void insw(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "w" : "+D"(addr), "+c"(count) : "d"(port)); }
static inline void outl_local(unsigned int value, int port) { __asm__ __volatile__("out" "l" " %" "" "0, %w1" : : "a"(value), "Nd"(port)); } static inline unsigned int inl_local(int port) { unsigned int value; __asm__ __volatile__("in" "l" " %w1, %" "" "0" : "=a"(value) : "Nd"(port)); return value; } static inline void outl_local_p(unsigned int value, int port) { outl_local(value, port); slow_down_io(); } static inline unsigned int inl_local_p(int port) { unsigned int value = inl_local(port); slow_down_io(); return value; } static inline void outl(unsigned int value, int port) { outl_local(value, port); } static inline unsigned int inl(int port) { return inl_local(port); } static inline void outl_p(unsigned int value, int port) { outl(value, port); slow_down_io(); } static inline unsigned int inl_p(int port) { unsigned int value = inl(port); slow_down_io(); return value; } static inline void outsl(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "l" : "+S"(addr), "+c"(count) : "d"(port)); } static inline void insl(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "l" : "+D"(addr), "+c"(count) : "d"(port)); }


static inline void slow_down_io(void) {
        __asm__ __volatile__(
                "outb %%al,$0x80;"



                : : );
}

static inline void blkdev_dequeue_request(struct request *req)
{
        do { if (unlikely((list_empty(&req->queuelist))!=0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (538), "i" ("/usr/include/linux/blkdev.h")); } while(0);

        list_del_init(&req->queuelist);

        if (req->q)
                elv_remove_request(req->q, req);
}


static inline struct device *
list_to_dev(struct list_head *node)
{
        return list_entry(node, struct device, node);
}

static inline int verify_area(int type, const void * addr, unsigned long size)
{
        return (likely(({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (addr),"g" ((int)(size)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) ? 0 : -14;
}


static inline unsigned long
__copy_from_user(void *to, const void *from, unsigned long n)
{
        if (__builtin_constant_p(n)) {
                unsigned long ret;

                switch (n) {
                case 1:
                        do { ret = 0; switch (1) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; default: (*(u8 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                case 2:
                        do { ret = 0; switch (2) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; default: (*(u16 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                case 4:
                        do { ret = 0; switch (4) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; default: (*(u32 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                }
        }
        return __copy_from_user_ll(to, from, n);
}

static inline unsigned long
copy_to_user(void *to, const void *from, unsigned long n)
{
        might_sleep();
        if ((likely(({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (to),"g" ((int)(n)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)))
                n = __copy_to_user(to, from, n);
        return n;
}
# 495 "/usr/include/asm/uaccess.h" 3 4
static inline unsigned long
copy_from_user(void *to, const void *from, unsigned long n)
{
        might_sleep();
        if ((likely(({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (from),"g" ((int)(n)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)))
                n = __copy_from_user(to, from, n);
        else
                memset(to, 0, n);
        return n;
}

extern __typeof__(long) per_cpu__nr_pagecache_local;


static inline int fault_in_pages_writeable(char *uaddr, int size)
{
        int ret;





        ret = ({ long __pu_err; do { __pu_err = 0; switch ((sizeof(*(uaddr)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(uaddr)))(0))), "m"((*(struct __large_struct *)(((uaddr))))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(uaddr)))(0))), "m"((*(struct __large_struct *)(((uaddr))))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(uaddr)))(0))), "m"((*(struct __large_struct *)(((uaddr))))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*((uaddr))))(((__typeof__(*(uaddr)))(0)))), "r" (((uaddr))), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; });
        if (ret == 0) {
                char *end = uaddr + size - 1;





                if (((unsigned long)uaddr & (~((1UL << 12)-1))) !=
                                ((unsigned long)end & (~((1UL << 12)-1))))
                        ret = ({ long __pu_err; do { __pu_err = 0; switch ((sizeof(*(end)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(end)))(0))), "m"((*(struct __large_struct *)(((end))))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(end)))(0))), "m"((*(struct __large_struct *)(((end))))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(end)))(0))), "m"((*(struct __large_struct *)(((end))))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*((end))))(((__typeof__(*(end)))(0)))), "r" (((end))), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; });
        }
        return ret;
}

static inline void fault_in_pages_readable(const char *uaddr, int size)
{
        volatile char c;
        int ret;

        ret = ({ long __gu_err, __gu_val; do { __gu_err = 0; switch ((sizeof(*((char *)uaddr)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=q" (__gu_val) : "m"((*(struct __large_struct *)((((char *)uaddr))))), "i"(-14), "0"(__gu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)((((char *)uaddr))))), "i"(-14), "0"(__gu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)((((char *)uaddr))))), "i"(-14), "0"(__gu_err));break; default: (__gu_val) = __get_user_bad(); } } while (0); ((c)) = (__typeof__(*(((char *)uaddr))))__gu_val; __gu_err; });
        if (ret == 0) {
                const char *end = uaddr + size - 1;

                if (((unsigned long)uaddr & (~((1UL << 12)-1))) !=
                                ((unsigned long)end & (~((1UL << 12)-1))))
                        ({ long __gu_err, __gu_val; do { __gu_err = 0; switch ((sizeof(*((char *)end)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=q" (__gu_val) : "m"((*(struct __large_struct *)((((char *)end))))), "i"(-14), "0"(__gu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)((((char *)end))))), "i"(-14), "0"(__gu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)((((char *)end))))), "i"(-14), "0"(__gu_err));break; default: (__gu_val) = __get_user_bad(); } } while (0); ((c)) = (__typeof__(*(((char *)end))))__gu_val; __gu_err; });
        }
}


static inline struct proc_inode *PROC_I(const struct inode *inode)
{
        return container_of(inode, struct proc_inode, vfs_inode);
}

static __inline__ __u16 __fswab16(__u16 x)
{
        return ({ __u16 __tmp = (x) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); });
}


enum module_state
{
        MODULE_STATE_LIVE,
        MODULE_STATE_COMING,
        MODULE_STATE_GOING,
};


struct mod_arch_specific
{
};


extern int request_module(const char * name, ...) ;



static inline void wrmsrl (unsigned long msr, unsigned long long val)
{
        unsigned long lo, hi;
        lo = (unsigned long) val;
        hi = val >> 32;

        if (!(x & 0xffff0000u)) { /* pad: 0xffff... big integers to handle */
                x <<= 16;
                r -= 16;
        }

}

typedef int cycles_t;
static inline cycles_t get_cycles (void) /* (void) to handle */
{

        unsigned long long ret;
        __asm__ __volatile__("rdtsc" : "=A" (ret));
        return ret;
}


static __inline__ void set_bit(int nr, volatile unsigned long * addr)
{
}

static inline unsigned long generic_hweight64(__u64 w) /* __u64 is a typedef of gcc */
{

        return generic_hweight32((unsigned int)(w >> 32)) +
                                generic_hweight32((unsigned int)w);
# 125 "/usr/include/linux/bitops.h" 3 4
}

static inline void __set_64bit_constant (unsigned long long *ptr,
                                                 unsigned long long value)
{
  __set_64bit(ptr,(unsigned int)(value), (unsigned int)((value)>>32ULL));  /* again long long extension */
}


struct mpc_config_bus
{
        unsigned char mpc_type;
        unsigned char mpc_busid;
        unsigned char mpc_bustype[6] __attribute((packed));
};


static void apic_write_atomic(unsigned long reg, unsigned long v)
{
        ((__typeof__(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg))))__xchg((unsigned long)(v),((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)),sizeof(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)))));
}



extern __typeof__(unsigned long) per_cpu__process_counts;

/*
        __asm__ __volatile__( "lock  "
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));

        __asm__ __volatile__("wrmsr" : : "c" (msr), "a" (lo), "d" (hi));

 */

        asm volatile(
                ".byte 0x0f,0x01,0xc8;"
                : :"a" (eax), "c" (ecx), "d"(edx));

        asm volatile ("661:\n\t" ".byte 0x8d,0x74,0x26,0x00\n" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchw (%1)" "\n664:\n" ".previous" :: "i" ((1*32+31)), "r" (x));

