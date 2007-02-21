/* gcc -E init.c */

//#define MODULE // 
#include <linux/init.h>

static void __init initme(int x, int y)
{
    int z; z = x * y;
}

static void __exit exitme(int x, int y)
{
    int z = x * y;
}


module_init(initme);
module_exit(initme);

MODULE_PARM (mtu, "1-" __MODULE_STRING (MAX_UNITS) "i");


//HANDLING: /tmp/linux-2.6.13/drivers/char/hvc_vio.c
//EXECUTING: cpp -nostdinc -isystem /usr/lib/gcc-lib/i486-slackware-linux/3.3.4/include -D__GNUC__=3 -D__KERNEL__ -I/tmp/linux-2.6.13/include -I/tmp/linux-2.6.13/include/asm-i386/mach-default -I/tmp/linux-2.6.13/include/asm-ppc/    -I/tmp/linux-2.6.13/drivers/char -DMODULE -DKBUILD_BASENAME=gscd -DKBUILD_MODNAME=gscd  /tmp/linux-2.6.13/drivers/char/hvc_vio.c> /tmp/main_ml-1.c
//parse error=File "/tmp/main_ml-2.c", line 12461, characters 31-32:
//    around = ;, whole content = console_initcall(hvc_find_vtys);
// charpos = 353096
//PASSING THROUGH:console_initcall(hvc_find_vtys);

console_initcall(hvc_find_vtys);

