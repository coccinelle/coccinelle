

union cm_id {
	int cm_id_reg;
	struct {
		int always_one:1,	// 0
		 mfg_id:11,	// 11:1
                     
		:28,		// 63:36

		 part_num:16,	// 27:12
		 bitstream_rev:8;	// 35:28
	} ;
};



int f() {
  int i;
  return (i ? 0 : 2);

}

static  int sock_rcvlowat(const struct sock *sk, int waitall, int len)
{
        return (waitall ? len : ({ int __x = (sk->sk_rcvlowat); int __y = (len); __x < __y ? __x: __y; })) ? : 1;
}


typedef int wait_queue_head_t; 
typedef int spinlock_t;

static wait_queue_head_t azt_waitq = { 
  .xxx = 1,
  .lock = (spinlock_t) { }, 
  .task_list = { &(azt_waitq).task_list, &(azt_waitq).task_list } 
};



static spinlock_t gscd_lock = (spinlock_t) { };

void f() {
  msg(1,"requested frame %d, CD size %d ???\n", 1);
}



typedef unsigned int __u32;
typedef unsigned int __u16;

int fonction(__u16 x, __u16 y);

static    __u16 __vivivivivi(__u16 x)
{
  
  int i = 0x00ffU;
  return  (__u16)(x);
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
  return ({ __u16 __tmp = (x) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & /*(__u16)*/0x00ffU) << 8) | (((__u16)(__x) & /*(__u16)*/0xff00U) >> 8) )); }); });
}
static  __u16 __swab16p(const __u16 *x)
{
  return ({ __u16 __tmp = (*(x)) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & /*(__u16)*/0x00ffU) << 8) | (((__u16)(__x) & /*(__u16)*/0xff00U) >> 8) )); }); });
}




