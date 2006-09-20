/********************************************************************************
 *                  QLOGIC LINUX SOFTWARE
 *
 * QLogic ISP1x80/1x160 device driver for Linux 2.3.x (redhat 6.X).
 *
 * COPYRIGHT (C) 1999-2000 QLOGIC CORPORATION    
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the Qlogic's Linux Software License. See below.
 *
 * This program is WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistribution's or source code must retain the above copyright
 *    notice, this list of conditions, and the following disclaimer,
 *    without modification, immediately at the beginning of the file.
 * 2. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 ********************************************************************************/
 
/*****************************************************************************************
			QLOGIC CORPORATION SOFTWARE
           "GNU" GENERAL PUBLIC LICENSE
    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION
                 AND MODIFICATION

This GNU General Public License ("License") applies solely to QLogic Linux 
Software ("Software") and may be distributed under the terms of this License.  
 
1. You may copy and distribute verbatim copies of the Software's source code as 
you receive it, in any medium, provided that you conspicuously and appropriately 
publish on each copy an appropriate copyright notice and disclaimer of warranty;
keep intact all the notices that refer to this License and to the absence of any
warranty; and give any other recipients of the Software a copy of this License along
with the Software. 

You may charge a fee for the physical act of transferring a copy, and you may at your
option offer warranty protection in exchange for a fee.
 
2. You may modify your copy or copies of the Software or any portion of it, thus forming
a work based on the Software, and copy and distribute such modifications or work under
the terms of Section 1 above, provided that you also meet all of these conditions:
 
* a) You must cause the modified files to carry prominent notices stating that you
changed the files and the date of any change. 

* b) You must cause any work that you distribute or publish that in whole or in part
contains or is derived from the Software or any part thereof, to be licensed as a
whole at no charge to all third parties under the terms of this License. 

* c) If the modified Software normally reads commands interactively when run, you
must cause it, when started running for such interactive use in the most ordinary way,
to print or display an announcement including an appropriate copyright notice and a 
notice that there is no warranty (or else, saying that you provide a warranty) and that
users may redistribute the Software under these conditions, and telling the user how to
view a copy of this License. (Exception:if the Software itself is interactive but does 
not normally print such an announcement, your work based on the Software is not required
to print an announcement.) 

These requirements apply to the modified work as a whole. If identifiable sections of
that work are not derived from the Software, and can be reasonably considered independent
and separate works in themselves, then this License, and its terms, do not apply to those
sections when you distribute them as separate works. But when you distribute the same
sections as part of a whole which is a work based on the Software, the distribution of the
whole must be on the terms of this License, whose permissions for other licensees extend
to the entire whole, and thus to each and every part regardless of who wrote it. 

3. You may copy and distribute the Software (or a work based on it, under Section 2) in 
object code or executable form under the terms of Sections 1 and 2 above provided that
you also do one of the following: 

* a) Accompany it with the complete corresponding machine-readable source code, which must
be distributed under the terms of Sections 1 and 2 above on a medium customarily used for
software interchange; or, 

* b) Accompany it with a written offer, valid for at least three years, to give any third
party, for a charge no more than your cost of physically performing source distribution,
a complete machine-readable copy of the corresponding source code, to be distributed under
the terms of Sections 1 and 2 above on a medium customarily used for software interchange;
or,

* c) Accompany it with the information you received as to the offer to distribute 
corresponding source code. (This alternative is allowed only for noncommercial distribution
and only if you received the Software in object code or executable form with such an offer,
in accord with Subsection b above.) 

The source code for a work means the preferred form of the work for making modifications
to it. For an executable work, complete source code means all the source code for all 
modules it contains, plus any associated interface definition files, plus the scripts used
to control compilation and installation of the executable.     

If distribution of executable or object code is made by offering access to copy from a 
designated place, then offering equivalent access to copy the source code from the same
place counts as distribution of the source code, even though third parties are not 
compelled to copy the source along with the object code. 

4. You may not copy, modify, sublicense, or distribute the Software except as expressly 
provided under this License. Any attempt otherwise to copy, modify, sublicense or 
distribute the Software is void, and will automatically terminate your rights under this
License. However, parties who have received copies, or rights, from you under this License
will not have their licenses terminated so long as such parties remain in full compliance. 

5. This license grants you world wide, royalty free non-exclusive rights to modify or 
distribute the Software or its derivative works. These actions are prohibited by law 
if you do not accept this License. Therefore, by modifying or distributing the Software
(or any work based on the Software), you indicate your acceptance of this License to do
so, and all its terms and conditions for copying, distributing or modifying the Software
or works based on it.
 
6. Each time you redistribute the Software (or any work based on the Software), the 
recipient automatically receives a license from the original licensor to copy, distribute
or modify the Software subject to these terms and conditions. You may not impose any 
further restrictions on the recipients' exercise of the rights granted herein. You are
not responsible for enforcing compliance by third parties to this License.
 
7. If, as a consequence of a court judgment or allegation of patent infringement or for
any other reason (not limited to patent issues), conditions are imposed on you 
(whether by court order, agreement or otherwise) that contradict the conditions of this
License, they do not excuse you from the conditions of this License. If you cannot 
distribute so as to satisfy simultaneously your obligations under this License 
and any other pertinent obligations, then as a consequence you may not distribute the
Software at all.    

If any portion of this section is held invalid or unenforceable under any particular 
circumstance, the balance of the section is intended to apply and the section as a whole
is intended to apply in other circumstances. 
NO WARRANTY

11. THE SOFTWARE IS PROVIDED WITHOUT A WARRANTY OF ANY KIND. THERE IS NO 
WARRANTY FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. 
EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR 
OTHER PARTIES PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, 
EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE 
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH YOU. 
SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL 
NECESSARY SERVICING, REPAIR OR CORRECTION.
 
12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING 
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR 
REDISTRIBUTE THE SOFTWARE AS PERMITTED ABOVE, BE LIABLE TO YOU FOR 
DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL 
DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE SOFTWARE (INCLUDING 
BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR 
LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE SOFTWARE TO 
OPERATE WITH ANY OTHER SOFTWARES), EVEN IF SUCH HOLDER OR OTHER PARTY HAS 
BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. 
END OF TERMS AND CONDITIONS 

*******************************************************************************************/ 

/****************************************************************************
    Revision History:
    Rev. 3.00       Jan 17, 1999    DG  Qlogic
	   - Added 64-bit support.
    Rev. 2.07       Nov 9, 1999     DG  Qlogic
	   - Added new routine to set target parameters for ISP12160. 
    Rev. 2.06       Sept 10, 1999     DG  Qlogic
       - Added support for ISP12160 Ultra 3 chip.
    Rev. 2.03       August 3, 1999    Fred Lewis, Intel DuPont
	- Modified code to remove errors generated when compiling with
	  Cygnus IA64 Compiler.
        - Changed conversion of pointers to unsigned longs instead of integers.
        - Changed type of I/O port variables from uint32_t to unsigned long.
        - Modified OFFSET macro to work with 64-bit as well as 32-bit.
        - Changed sprintf and printk format specifiers for pointers to %p.
        - Changed some int to long type casts where needed in sprintf & printk.
        - Added l modifiers to sprintf and printk format specifiers for longs.
        - Removed unused local variables.
    Rev. 1.20       June 8, 1999      DG,  Qlogic
         Changes to support RedHat release 6.0 (kernel 2.2.5).  
       - Added SCSI exclusive access lock (io_request_lock) when accessing 
         the adapter.
       - Added changes for the new LINUX interface template. Some new error
         handling routines have been added to the template, but for now we
         will use the old ones.
    -   Initial Beta Release.  
*****************************************************************************/


#ifdef MODULE
#include <linux/module.h>
#endif

#define QLA1280_VERSION      " 3.00-Beta"

#include <stdarg.h>
#include <asm/io.h>
#include <asm/irq.h>
#include <asm/segment.h>
#include <asm/byteorder.h>
#include <linux/version.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/ioport.h>
#include <linux/delay.h>
#include <linux/timer.h>
#include <linux/sched.h>
#include <linux/pci.h>
#include <linux/proc_fs.h>
#include <linux/blk.h>
#include <linux/tqueue.h>
/* MRS #include <linux/tasks.h> */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
# include <linux/bios32.h>
#endif
#include "sd.h"
#include "scsi.h"
#include "hosts.h"
#define UNIQUE_FW_NAME
#include "qla1280.h"
#include "ql12160_fw.h"                     /* ISP RISC code */
#include "ql1280_fw.h"

#include <linux/stat.h>
#include <linux/malloc.h>        /* for kmalloc() */


#ifndef KERNEL_VERSION
#  define KERNEL_VERSION(x,y,z) (((x)<<16)+((y)<<8)+(z))
#endif


/*
 * Compile time Options: 
 *            0 - Disable and 1 - Enable 
 */
#define  QLA1280_64BIT_SUPPORT         1   /* 64-bit Support */
#define  QL1280_TARGET_MODE_SUPPORT    0   /* Target mode support */
#define  WATCHDOGTIMER                 0
#define  MEMORY_MAPPED_IO              0
#define  DEBUG_QLA1280_INTR            0
#define  USE_NVRAM_DEFAULTS	       0
#define  DEBUG_PRINT_NVRAM             0
#define  LOADING_RISC_ACTIVITY         0
#define  AUTO_ESCALATE_RESET           0   /* Automatically escalate resets */
#define  AUTO_ESCALATE_ABORT           0   /* Automatically escalate aborts */
#define  STOP_ON_ERROR                 0   /* Stop on aborts and resets  */
#define  STOP_ON_RESET                 0 
#define  STOP_ON_ABORT                 0 
#undef   DYNAMIC_MEM_ALLOC

#define  DEBUG_QLA1280                 0    /* Debugging  */
/* #define CHECKSRBSIZE */

/*
 * These macros to assist programming
 */

#define	BZERO(ptr, amt)		memset(ptr, 0, amt)
#define	BCOPY(src, dst, amt)	memcpy(dst, src, amt)
#define	KMALLOC(siz)	kmalloc((siz), GFP_ATOMIC)
#define	KMFREE(ip,siz)	kfree((ip))
#define	SYS_DELAY(x)		udelay(x);barrier()
#define QLA1280_DELAY(sec)  mdelay(sec * 1000)
#define VIRT_TO_BUS(a) virt_to_bus((a))
#if  QLA1280_64BIT_SUPPORT
#if  BITS_PER_LONG <= 32
#define  VIRT_TO_BUS_LOW(a) (uint32_t)virt_to_bus((a))
#define  VIRT_TO_BUS_HIGH(a) (uint32_t)(0x0)
#else
#define  VIRT_TO_BUS_LOW(a) (uint32_t)(0xffffffff & virt_to_bus((a)))
#define  VIRT_TO_BUS_HIGH(a) (uint32_t)(0xffffffff & (virt_to_bus((a))>>32))
#endif
#endif  /* QLA1280_64BIT_SUPPORT */

#define STATIC     

#define NVRAM_DELAY() udelay(500) /* 2 microsecond delay */
void qla1280_device_queue_depth(scsi_qla_host_t *, Scsi_Device *);

#define  CACHE_FLUSH(a) (RD_REG_WORD(a))
#define  INVALID_HANDLE    (MAX_OUTSTANDING_COMMANDS+1)

#define  MSW(x)          (uint16_t)((uint32_t)(x) >> 16)
#define  LSW(x)          (uint16_t)(x)
#define  MSB(x)          (uint8_t)((uint16_t)(x) >> 8)
#define  LSB(x)          (uint8_t)(x)

#if  BITS_PER_LONG <= 32
#define  LS_64BITS(x) (uint32_t)(x)
#define  MS_64BITS(x) (uint32_t)(0x0)
#else
#define  LS_64BITS(x) (uint32_t)(0xffffffff & (x))
#define  MS_64BITS(x) (uint32_t)(0xffffffff & ((x)>>32) )
#endif

/*
 *  QLogic Driver Support Function Prototypes.
 */
STATIC void   qla1280_done(scsi_qla_host_t *, srb_t **, srb_t **);
STATIC void   qla1280_next(scsi_qla_host_t *, scsi_lu_t *, uint8_t);
STATIC void   qla1280_putq_t(scsi_lu_t *, srb_t *);
STATIC void   qla1280_done_q_put(srb_t *, srb_t **, srb_t **);
STATIC void qla1280_select_queue_depth(struct Scsi_Host *, Scsi_Device *);
#ifdef  QLA1280_UNUSED 
static void qla1280_dump_regs(struct Scsi_Host *host);
#endif
#if  STOP_ON_ERROR 
static void qla1280_panic(char *, struct Scsi_Host *host);
#endif
void qla1280_print_scsi_cmd(Scsi_Cmnd *cmd);
STATIC void qla1280_abort_queue_single(scsi_qla_host_t *,uint32_t,uint32_t,uint32_t,uint32_t);

STATIC int qla1280_return_status( sts_entry_t *sts, Scsi_Cmnd       *cp);
STATIC void qla1280_removeq(scsi_lu_t *q, srb_t *sp);
STATIC void qla1280_mem_free(scsi_qla_host_t *ha);
static void qla1280_do_dpc(void *p);
#ifdef  QLA1280_UNUSED 
static void qla1280_set_flags(char * s);
#endif
static char	*qla1280_get_token(char *, char *);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
STATIC inline void mdelay(int);
#endif
static inline void qla1280_enable_intrs(scsi_qla_host_t *);
static inline void qla1280_disable_intrs(scsi_qla_host_t *);

/*
 *  QLogic ISP1280 Hardware Support Function Prototypes.
 */
STATIC uint8_t   qla1280_initialize_adapter(struct scsi_qla_host   *ha);
STATIC uint8_t   qla1280_enable_tgt(scsi_qla_host_t *, uint8_t);
STATIC uint8_t   qla1280_isp_firmware(scsi_qla_host_t *);
STATIC uint8_t   qla1280_pci_config(scsi_qla_host_t *);
STATIC uint8_t   qla1280_chip_diag(scsi_qla_host_t *);
STATIC uint8_t   qla1280_setup_chip(scsi_qla_host_t *);
STATIC uint8_t   qla1280_init_rings(scsi_qla_host_t *);
STATIC uint8_t   qla1280_nvram_config(scsi_qla_host_t *);
STATIC uint8_t   qla1280_mailbox_command(scsi_qla_host_t *, uint8_t, uint16_t *);
STATIC uint8_t   qla1280_bus_reset(scsi_qla_host_t *, uint8_t);
STATIC uint8_t   qla1280_device_reset(scsi_qla_host_t *, uint8_t, uint32_t);
STATIC uint8_t   qla1280_abort_device(scsi_qla_host_t *, uint8_t, uint32_t, uint32_t);
STATIC uint8_t   qla1280_abort_command(scsi_qla_host_t *, srb_t *),
#if  QLA1280_64BIT_SUPPORT
                 qla1280_64bit_start_scsi(scsi_qla_host_t *, srb_t *),
#endif
                 qla1280_32bit_start_scsi(scsi_qla_host_t *, srb_t *),
                 qla1280_abort_isp(scsi_qla_host_t *);
STATIC void      qla1280_nv_write(scsi_qla_host_t *, uint16_t),
                 qla1280_nv_delay(scsi_qla_host_t *),
                 qla1280_poll(scsi_qla_host_t *),
                 qla1280_reset_adapter(scsi_qla_host_t *),
                 qla1280_marker(scsi_qla_host_t *, uint8_t, uint32_t, uint32_t, uint8_t),
                 qla1280_isp_cmd(scsi_qla_host_t *),
                 qla1280_isr(scsi_qla_host_t *, srb_t **, srb_t **),
                 qla1280_rst_aen(scsi_qla_host_t *),
                 qla1280_status_entry(scsi_qla_host_t *, sts_entry_t *, srb_t **,
                                      srb_t **),
                 qla1280_error_entry(scsi_qla_host_t *, response_t *, srb_t **,
                                     srb_t **),
                 qla1280_restart_queues(scsi_qla_host_t *),
                 qla1280_abort_queues(scsi_qla_host_t *);
STATIC uint16_t  qla1280_get_nvram_word(scsi_qla_host_t *, uint32_t),
                 qla1280_nvram_request(scsi_qla_host_t *, uint32_t),
                 qla1280_debounce_register(volatile uint16_t *);
STATIC request_t *qla1280_req_pkt(scsi_qla_host_t *);
int  qla1280_check_for_dead_scsi_bus(scsi_qla_host_t *ha, srb_t *sp);
STATIC uint8_t qla1280_mem_alloc(scsi_qla_host_t *ha);
STATIC uint8_t  qla1280_register_with_Linux(scsi_qla_host_t *ha, uint8_t maxchannels);

STATIC uint8_t qla12160_set_target_parameters(scsi_qla_host_t *, uint32_t, uint32_t, uint32_t, nvram160_t *);
STATIC void qla12160_get_target_parameters(scsi_qla_host_t *, uint32_t, uint32_t, uint32_t);

#if QL1280_TARGET_MODE_SUPPORT
STATIC void      qla1280_enable_lun(scsi_qla_host_t *, uint8_t, uint32_t),
                 qla1280_notify_ack(scsi_qla_host_t *, notify_entry_t *),
                 qla1280_immed_notify(scsi_qla_host_t *, notify_entry_t *),
                 qla1280_accept_io(scsi_qla_host_t *, ctio_ret_entry_t *),
#if  QLA1280_64BIT_SUPPORT
                 qla1280_64bit_continue_io(scsi_qla_host_t *, atio_entry_t *, uint32_t,
                                     paddr32_t *),
#endif
                 qla1280_32bit_continue_io(scsi_qla_host_t *, atio_entry_t *, uint32_t,
                                     paddr32_t *),
                 qla1280_atio_entry(scsi_qla_host_t *, atio_entry_t *),
                 qla1280_notify_entry(scsi_qla_host_t *, notify_entry_t *);
#endif  /* QLA1280_TARGET_MODE_SUPPORT */

#ifdef QL_DEBUG_ROUTINES
/*
 *  Driver Debug Function Prototypes.
 */
STATIC uint8_t  qla1280_getbyte(uint8_t *);
STATIC uint16_t qla1280_getword(uint16_t *);
STATIC uint32_t qla1280_getdword(uint32_t *);
STATIC void     qla1280_putbyte(uint8_t *, uint8_t),
                qla1280_putword(uint16_t *, uint16_t),
                qla1280_putdword(uint32_t *, uint32_t),
                qla1280_print(caddr_t),
                qla1280_output_number(uint32_t, uint8_t),
                qla1280_putc(uint8_t),
                qla1280_dump_buffer(caddr_t, uint32_t);

char          debug_buff[80];
#if DEBUG_QLA1280 
STATIC uint8_t ql_debug_print = 1;
#else
STATIC uint8_t ql_debug_print = 0;
#endif
#endif

/*
 * insmod needs to find the variable and make it point to something
 */
#ifdef MODULE
static char *options = NULL;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,18)

/* insmod qla1280 options=verbose" */
MODULE_PARM(options, "s");  
#endif
/*
 * Just in case someone uses commas to separate items on the insmod
 * command line, we define a dummy buffer here to avoid having insmod
 * write wild stuff into our code segment
 */
static char dummy_buffer[60] = "Please don't add commas in your insmod command!!\n";

#endif

/* We use the Scsi_Pointer structure that's included with each command
 * SCSI_Cmnd as a scratchpad for our SRB.
 *
 * SCp will always point to the SRB structure (defined in qla1280.h).
 * It is define as follows:
 *  - SCp.ptr  -- > pointer back to the cmd
 *  - SCp.this_residual --> used as forward pointer to next srb
 *  - SCp.buffer --> used as backward pointer to next srb
 *  - SCp.buffers_residual --> used as flags field
 *  - SCp.have_data_in --> not used
 *  - SCp.sent_command --> not used
 *  - SCp.phase --> not used
 */

#define	CMD_SP(Cmnd)		(&(Cmnd)->SCp)
#define	CMD_XFRLEN(Cmnd)	(Cmnd)->request_bufflen
#define	CMD_CDBLEN(Cmnd)	(Cmnd)->cmd_len
#define	CMD_CDBP(Cmnd)		(Cmnd)->cmnd
#define	CMD_SNSP(Cmnd)		(Cmnd)->sense_buffer
#define	CMD_SNSLEN(Cmnd)	(sizeof (Cmnd)->sense_buffer)
#define	CMD_RESULT(Cmnd)	((Cmnd)->result)
#define	CMD_HANDLE(Cmnd)	((Cmnd)->host_scribble)

/*****************************************/
/*   ISP Boards supported by this driver */
/*****************************************/
#define QLA1280_VENDOR_ID   0x1077
#define QLA1080_DEVICE_ID   0x1080
#define QLA1240_DEVICE_ID   0x1240
#define QLA1280_DEVICE_ID   0x1280
#define QLA12160_DEVICE_ID  0x1216
#define QLA10160_DEVICE_ID  0x1016
#define NUM_OF_ISP_DEVICES        6

typedef struct _qlaboards 
{
   unsigned char   bdName[9];       /* Board ID String */
   unsigned long   device_id;       /* Device PCI ID   */
   int   numPorts;                  /* Number of SCSI ports */
   unsigned short   *fwcode;        /* pointer to FW array         */
   unsigned long    *fwlen;         /* number of words in array    */
   unsigned short   *fwstart;       /* start address for F/W       */
   unsigned char   *fwver;          /* Ptr to F/W version array    */
} qla_boards_t;

struct _qlaboards   QLBoardTbl[NUM_OF_ISP_DEVICES] = 
{
   /* Name ,  Board PCI Device ID,         Number of ports */
  {"QLA1080 ", QLA1080_DEVICE_ID,           1,        
               &fw1280ei_code01[0],  (unsigned long *)&fw1280ei_length01,&fw1280ei_addr01, &fw1280ei_version_str[0] },       
  {"QLA1240 ", QLA1240_DEVICE_ID,           2,       
               &fw1280ei_code01[0],  (unsigned long *)&fw1280ei_length01,&fw1280ei_addr01, &fw1280ei_version_str[0] },       
  {"QLA1280 ", QLA1280_DEVICE_ID,           2,      
               &fw1280ei_code01[0],  (unsigned long *)&fw1280ei_length01,&fw1280ei_addr01, &fw1280ei_version_str[0] },       
  {"QLA12160 ", QLA12160_DEVICE_ID,          2,      
               &fw12160i_code01[0],  (unsigned long *)&fw12160i_length01,&fw12160i_addr01, &fw12160i_version_str[0] },       
  {"QLA10160 ", QLA10160_DEVICE_ID,          1,      
               &fw12160i_code01[0],  (unsigned long *)&fw12160i_length01,&fw12160i_addr01, &fw12160i_version_str[0] },       
  {"        ",                 0,           0}
};

static unsigned long qla1280_verbose = 1L;
static scsi_qla_host_t *qla1280_hostlist = NULL;
#ifdef QLA1280_PROFILE
static int qla1280_buffer_size = 0;
static char *qla1280_buffer = NULL;
#endif

#ifdef QL_DEBUG_LEVEL_3 
#define ENTER(x)	sprintf(debug_buff,"qla1280 : Entering %s()\n\r", x); \
                        qla1280_print(debug_buff);
#define LEAVE(x)	sprintf(debug_buff,"qla1280 : Leaving %s()\n\r", x); \
                        qla1280_print(debug_buff);
#define ENTER_INTR(x)	sprintf(debug_buff,"qla1280 : Entering %s()\n\r", x); \
                        qla1280_print(debug_buff);
#define LEAVE_INTR(x)	sprintf(debug_buff,"qla1280 : Leaving %s()\n\r", x); \
                        qla1280_print(debug_buff);
#define DEBUG3(x)	x
#else
#define ENTER(x)
#define LEAVE(x)
#define ENTER_INTR(x)
#define LEAVE_INTR(x)
#define DEBUG3(x)
#endif

#if  DEBUG_QLA1280  
#define COMTRACE(x)
/* #define COMTRACE(x)     qla1280_putc(x); */
#define DEBUG(x)	x
#else
#define DEBUG(x)
#define COMTRACE(x)
#endif

#ifdef QL_DEBUG_LEVEL_2 
#define DEBUG2(x)	x
#else
#define DEBUG2(x)
#endif
#define DEBUG5(x)

#if (BITS_PER_LONG==64)
#   define OFFSET(w)   (((uint64_t) &w) & 0xFF)   /* 256 byte offsets */
#else
#   define OFFSET(w)   (((uint32_t) &w) & 0xFF)   /* 256 byte offsets */
#endif

#define SCSI_BUS_32(scp)   ((scp)->channel)
#define SCSI_TCN_32(scp)    ((scp)->target)
#define SCSI_LUN_32(scp)    ((scp)->lun)

/****************************************************************************/
/*  LINUX -  Loadable Module Functions.                                     */
/****************************************************************************/


/*************************************************************************
 *   qla1280_set_info
 *
 * Description:
 *   Set parameters for the driver from the /proc filesystem.
 *
 * Returns:
 *************************************************************************/
int
qla1280_set_info(char *buffer, int length, struct Scsi_Host *HBAptr)
{
  return (-ENOSYS);  /* Currently this is a no-op */
}

/*************************************************************************
 * qla1280_proc_info
 *
 * Description:
 *   Return information to handle /proc support for the driver.
 *
 * buffer - ptrs to a page buffer
 *
 * Returns:
 *************************************************************************/
#ifdef QLA1280_PROFILE
#define	PROC_BUF	(&qla1280_buffer[size])
#define LUN_ID       (targ_lun>>(MAX_T_BITS+MAX_L_BITS)),((targ_lun>>MAX_L_BITS)&0xf), targ_lun&0x7 
#endif
int
qla1280_proc_info ( char *buffer, char **start, off_t offset, int length, 
                    int hostno, int inout)
{
#ifdef QLA1280_PROFILE
  struct Scsi_Host *host;
  scsi_qla_host_t *ha;
  int    size = 0;
  int  targ_lun;
  scsi_lu_t  *up;
  int   no_devices;

  printk("Entering proc_info 0x%p,0x%lx,0x%x,0x%x\n",buffer,offset,length,hostno);
  host = NULL;
  /* find the host they want to look at */
  for(ha=qla1280_hostlist; (ha != NULL) && ha->host->host_no != hostno; ha=ha->next)
    ;

  if (!ha)
  {
    size += sprintf(buffer, "Can't find adapter for host number %d\n", hostno);
    if (size > length)
    {
      return (size);
    }
    else
    {
      return (length);
    }
  }

  host = ha->host;
  if (inout == TRUE) /* Has data been written to the file? */ 
  {
    return (qla1280_set_info(buffer, length, host));
  }

  /* compute number of active devices */
  no_devices = 0;
  for (targ_lun = 0; targ_lun < MAX_EQ; targ_lun++)
  {
          if( (up = ha->dev[targ_lun]) == NULL )
              continue;
          no_devices++;
  }
  /* size = 112 * no_devices; */
  size = 4096;
  /* round up to the next page */
  
  /* 
   * if our old buffer is the right size use it otherwise 
   * allocate a new one.
   */
  if (qla1280_buffer_size != size)
  {
    /* deallocate this buffer and get a new one */
    if (qla1280_buffer != NULL) 
    {
      kfree(qla1280_buffer);
      qla1280_buffer_size = 0;
    }
    qla1280_buffer = kmalloc(size, GFP_KERNEL);
  }
  if (qla1280_buffer == NULL)
  {
    size = sprintf(buffer, "qla1280 - kmalloc error at line %d\n",
        __LINE__);
    return size;
  }
  qla1280_buffer_size = size;

  size = 0;
  size += sprintf(PROC_BUF, "Qlogic 1280/1080 SCSI driver version: ");   /* 43 bytes */
  size += sprintf(PROC_BUF, "%5s, ", QLA1280_VERSION);                         /* 5        */
  size += sprintf(PROC_BUF, "Qlogic Firmware version: ");                     /* 25       */
  size += sprintf(PROC_BUF, "%2d.%2d.%2d",_firmware_version[0],           /* 8        */
                                          ql12_firmware_version[1],
                                          ql12_firmware_version[2]);
  size += sprintf(PROC_BUF, "\n");                                             /* 1       */
                           
  size += sprintf(PROC_BUF, "SCSI Host Adapter Information: %s\n", QLBoardTbl[ha->devnum].bdName);
  size += sprintf(PROC_BUF, "Request Queue = 0x%lx, Response Queue = 0x%lx\n",
                        ha->request_dma,
                        ha->response_dma);
  size += sprintf(PROC_BUF, "Request Queue count= 0x%x, Response Queue count= 0x%x\n",
                        REQUEST_ENTRY_CNT,
                        RESPONSE_ENTRY_CNT);
  size += sprintf(PROC_BUF,"Number of pending commands = 0x%lx\n", ha->actthreads);
  size += sprintf(PROC_BUF,"Number of queued commands = 0x%lx\n", ha->qthreads);
  size += sprintf(PROC_BUF,"Number of free request entries = %d\n",ha->req_q_cnt);
  size += sprintf(PROC_BUF, "\n");                                             /* 1       */
                        
  size += sprintf(PROC_BUF, "Attached devices:\n");
  /* scan for all equipment stats */ 
  for (targ_lun = 0; targ_lun < MAX_EQ; targ_lun++)
  {
      if( (up = ha->dev[targ_lun]) == NULL )
           continue;
      if( up->io_cnt == 0 )
      {
          size += sprintf(PROC_BUF,"(%2d:%2d:%2d) No stats\n",LUN_ID);
           continue;
      }
      /* total reads since boot */
      /* total writes since boot */
      /* total requests since boot  */
      size += sprintf(PROC_BUF, "Total requests %ld,",up->io_cnt);
      /* current number of pending requests */
      size += sprintf(PROC_BUF, "(%2d:%2d:%2d) pending requests %d,",LUN_ID,up->q_outcnt);
      /* avg response time */
      size += sprintf(PROC_BUF, "Avg response time %ld%%,",(up->resp_time/up->io_cnt)*100);
      
      /* avg active time */
      size += sprintf(PROC_BUF, "Avg active time %ld%%\n",(up->act_time/up->io_cnt)*100);
  }

  if (size >= qla1280_buffer_size)
  {
    printk(KERN_WARNING "qla1280: Overflow buffer in qla1280_proc.c\n");
  }

  if (offset > size - 1)
  {
    kfree(qla1280_buffer);
    qla1280_buffer = NULL;
    qla1280_buffer_size = length = 0;
    *start = NULL;
  }
  else
  {
    *start = &qla1280_buffer[offset];   /* Start of wanted data */
    if (size - offset < length)
    {
      length = size - offset;
    }
  }
#endif

  return (length);
}


/**************************************************************************
 * qla1280_detect
 *    This routine will probe for Qlogic 1280 SCSI host adapters.
 *    It returns the number of host adapters of a particular
 *    type that were found.	 It also initialize all data necessary for 
 *    the driver.  It is passed-in the host number, so that it
 *    knows where its first entry is in the scsi_hosts[] array.
 *
 * Input:
 *     template - pointer to SCSI template
 *
 * Returns:
 *  num - number of host adapters found.  
 **************************************************************************/
int
qla1280_detect(Scsi_Host_Template *template)
{
    int num_hosts = 0;
    struct Scsi_Host *host;
    scsi_qla_host_t *ha, *cur_ha;
    struct _qlaboards  *bdp;
    int i, j;
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,1,95)
    unsigned int piobase;
    unsigned char pci_bus, pci_devfn, pci_irq;
    config_reg_t   *cfgp = 0;
#endif
    device_reg_t   *reg;
    char   *cp;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,95)
    struct pci_dev *pdev = NULL;
#else
    int index;
#endif

    ENTER("qla1280_detect");

#ifdef CHECKSRBSIZE
    if (sizeof(srb_t) > sizeof(Scsi_Pointer) )
    {
      printk("Redefine SRB - its too big");
      return 0;
    }
#endif

#ifdef MODULE
	DEBUG(sprintf(debug_buff,"DEBUG: qla1280_detect starts at address = %p\n",qla1280_detect);)
	DEBUG(qla1280_print(debug_buff);)
    /*
    * If we are called as a module, the qla1280 pointer may not be null
    * and it would point to our bootup string, just like on the lilo
    * command line.  IF not NULL, then process this config string with
    * qla1280_setup
    *
    * Boot time Options
    * To add options at boot time add a line to your lilo.conf file like:
    * append="qla1280=verbose,max_tags:{{255,255,255,255},{255,255,255,255}}"
    * which will result in the first four devices on the first two
    * controllers being set to a tagged queue depth of 32.
    */
    if(options)
        qla1280_setup(options, NULL);
    if(dummy_buffer[0] != 'P')
        printk(KERN_WARNING "qla1280: Please read the file /usr/src/linux/drivers"
                "/scsi/README.qla1280\n"
                "qla1280: to see the proper way to specify options to the qla1280 "
                "module\n"
                "qla1280: Specifically, don't use any commas when passing arguments to\n"
                "qla1280: insmod or else it might trash certain memory areas.\n");
#endif

    if ((int) !pcibios_present())
    {
		printk("scsi: PCI not present\n");
		return 0;
    } /* end of IF */
    bdp = &QLBoardTbl[0];
    qla1280_hostlist = NULL;
#if 0
    template->proc_dir = &proc_scsi_qla1280;
#else
    template->proc_name = "qla1280";
#endif
    
	/* Try and find each different type of adapter we support */
	for( i=0; bdp->device_id != 0 && i < NUM_OF_ISP_DEVICES; i++, bdp++ ) {
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,95)
		while ((pdev = pci_find_device(QLA1280_VENDOR_ID,
			bdp->device_id, pdev ) ))  {
		if (pci_enable_device(pdev)) continue;
#else
		while (!(pcibios_find_device(QLA1280_VENDOR_ID,
			bdp->device_id,
			index++, &pci_bus, &pci_devfn)) )  {
#endif
                /* found a adapter */
		host = scsi_register(template, sizeof(scsi_qla_host_t));
		ha = (scsi_qla_host_t *) host->hostdata;
		/* Clear our data area */
		for( j =0, cp = (char *)ha;  j < sizeof(scsi_qla_host_t); j++)
			*cp = 0;
		/* Sanitize the information from PCI BIOS.  */
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,95)
		host->irq = pdev->irq;
		host->io_port = pci_resource_start(pdev, 0);
		ha->pci_bus = pdev->bus->number;
		ha->pci_device_fn = pdev->devfn;
		ha->pdev = pdev;
#else
		pcibios_read_config_byte(pci_bus, pci_devfn, OFFSET(cfgp->interrupt_line), &pci_irq);
		pcibios_read_config_dword(pci_bus, pci_devfn, OFFSET(cfgp->base_port), &piobase);
		host->irq = pci_irq;
		host->io_port = (unsigned int) piobase;
		host->io_port &= PCI_BASE_ADDRESS_IO_MASK;
		ha->pci_bus = pci_bus;
		ha->pci_device_fn = pci_devfn;
#endif
		ha->device_id = bdp->device_id;
    
                ha->devnum = i;
		if( qla1280_mem_alloc(ha) ) {
			printk(KERN_INFO "qla1280: Failed to allocate memory for adapter\n");
		}
                
                ha->ports = bdp->numPorts; 
                ha->iobase = (device_reg_t *) host->io_port;
                ha->host = host;
                ha->host_no = host->host_no;

                /* load the F/W, read paramaters, and init the H/W */
                if (qla1280_initialize_adapter(ha))
                {

                    printk(KERN_INFO "qla1280: Failed to initialized adapter\n");
                    qla1280_mem_free(ha);
                    scsi_unregister(host);
                    continue;
                }
                
                host->max_channel = bdp->numPorts-1; 
                ha->instance = num_hosts;
		/* Register our resources with Linux */
		if( qla1280_register_with_Linux(ha, bdp->numPorts-1) ) {
			printk(KERN_INFO "qla1280: Failed to register our resources\n");
			qla1280_mem_free(ha);
			scsi_unregister(host);
			continue;
		}


                reg = ha->iobase;
                /* Disable ISP interrupts. */
		qla1280_disable_intrs(ha);

                /* Insure mailbox registers are free. */
                WRT_REG_WORD(&reg->semaphore, 0);
                WRT_REG_WORD(&reg->host_cmd, HC_CLR_RISC_INT);
                WRT_REG_WORD(&reg->host_cmd, HC_CLR_HOST_INT);

                /* Enable chip interrupts. */
		qla1280_enable_intrs(ha);
               
                /* Insert new entry into the list of adapters */
                ha->next = NULL;
                if( qla1280_hostlist == NULL )
                {
                    cur_ha = qla1280_hostlist = ha;
                }
                else
                {
                    cur_ha = qla1280_hostlist;
                    while( cur_ha->next != NULL )
                        cur_ha = cur_ha->next;
                    cur_ha->next = ha;
                }
                num_hosts++;
            }  /* end of WHILE */
        } /* end of FOR */

    LEAVE("qla1280_detect");
    return num_hosts; 
}

/**************************************************************************
*   qla1280_register_with_Linux
*
* Description:
*   Free the passed in Scsi_Host memory structures prior to unloading the
*   module.
*
* Input:
*     ha - pointer to host adapter structure
*     maxchannels - MAX number of channels.
*
* Returns:
*  0 - Sucessfully reserved resources.
*  1 - Failed to reserved a resource.
**************************************************************************/
STATIC uint8_t  qla1280_register_with_Linux(scsi_qla_host_t *ha, uint8_t maxchannels)
{

	struct Scsi_Host *host = ha->host;

	host->can_queue = 0xfffff;  /* unlimited  */
	host->cmd_per_lun = 1;
       host->select_queue_depths = qla1280_select_queue_depth;
	host->n_io_port = 0xFF;
	host->base = (unsigned long) ha->mmpbase;
	host->max_channel = maxchannels; 
       host->max_lun = MAX_LUNS-1; 
	host->unique_id = ha->instance;
       host->max_id = MAX_TARGETS; 
       host->unique_id = ha->instance;

	/* set our host ID  (need to do something about our two IDs) */
       host->this_id = ha->bus_settings[0].id;
       /* Register the IRQ with Linux (sharable) */
       if ( request_irq(host->irq, qla1280_intr_handler, SA_INTERRUPT| SA_SHIRQ, "qla1280", ha))
       {
          printk("qla1280 : Failed to reserved interrupt %d already in use\n", host->irq);
          qla1280_mem_free(ha);
          scsi_unregister(host);
	     return 1;
       }

       /* Register the I/O space with Linux */
       if (check_region(host->io_port, 0xff))
       {
           printk("qla1280 : Failed to reserved i/o region 0x%04lx-0x%04lx already in use\n",
              host->io_port, host->io_port + 0xff);
           free_irq(host->irq, NULL);
           qla1280_mem_free(ha);
           scsi_unregister(host);
	     return 1;
       }

       request_region(host->io_port, 0xff, "qla1280");

	return 0;
}


/**************************************************************************
 *   qla1280_release
 *   Free the passed in Scsi_Host memory structures prior to unloading the
 *   module.
 **************************************************************************/
int
qla1280_release(struct Scsi_Host *host)
{
    scsi_qla_host_t *ha = (scsi_qla_host_t *) host->hostdata;

    ENTER("qla1280_release");

    if( !ha->flags.online )
        return(0);

    /* turn-off interrupts on the card */
    WRT_REG_WORD(&ha->iobase->ictrl, 0);

    /* Detach interrupts */
    if(host->irq)
        free_irq(host->irq, ha);

    /* release io space registers  */
    if( host->io_port )
        release_region(host->io_port, 0xff);

#if MEMORY_MAPPED_IO
    if(ha->mmpbase)
    {
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
        vfree((void *) (((unsigned long) ha->mmpbase) & PAGE_MASK));
#else
        iounmap((void *) (((unsigned long) ha->mmpbase) & PAGE_MASK));
#endif
    }
#endif /* MEMORY_MAPPED_IO */
    qla1280_mem_free(ha);

    ENTER("qla1280_release");
    return(0);
}

/**************************************************************************
 *   qla1280_info
 *     Return a string describing the driver.
 **************************************************************************/
const char *
qla1280_info(struct Scsi_Host *host)
{
    static char qla1280_buffer[125];
    char *bp;
    scsi_qla_host_t *ha;
    qla_boards_t   *bdp; 

    bp = &qla1280_buffer[0];
    ha = (scsi_qla_host_t *)host->hostdata;
    bdp = &QLBoardTbl[ha->devnum];
    memset(bp, 0, sizeof(qla1280_buffer));
    sprintf(bp,
            "QLogic %sPCI to SCSI Host Adapter: bus %d device %d irq %d\n"
            "       Firmware version: %2d.%02d.%02d, Driver version %s",
            (char *)&bdp->bdName[0], ha->pci_bus, (ha->pci_device_fn & 0xf8) >> 3, host->irq,
            bdp->fwver[0],bdp->fwver[1],bdp->fwver[2],
            QLA1280_VERSION);
    return(bp);
}

/**************************************************************************
 *   qla1200_queuecommand
 *     Queue a command to the controller.
 *
 * Note:
 * The mid-level driver tries to ensures that queuecommand never gets invoked
 * concurrently with itself or the interrupt handler (although the
 * interrupt handler may call this routine as part of request-completion
 * handling).   Unfortunely, it sometimes calls the scheduler in interrupt
 * context which is a big NO! NO!.
 **************************************************************************/
int
qla1280_queuecommand(Scsi_Cmnd *cmd, void (*fn)(Scsi_Cmnd *))
{
    scsi_qla_host_t *ha;
    srb_t  *sp;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif
    struct Scsi_Host *host;
    uint32_t        b, t, l;
    scsi_lu_t       *q;
    u_long          handle;

    ENTER("qla1280_queuecommand");
    COMTRACE('C') 

    host = cmd->host;
    ha = (scsi_qla_host_t *) host->hostdata;

    /* send command to adapter */
    sp = (srb_t *) CMD_SP(cmd);
    sp->cmd =  cmd;
    cmd->scsi_done = fn;
    if (cmd->flags == 0)  /* new command */
    {
        sp->flags = 0;
    }

    DEBUG5(qla1280_print_scsi_cmd(cmd));

    /* Generate LU queue on bus, target, LUN */
    b = SCSI_BUS_32(cmd);
    t = SCSI_TCN_32(cmd);
    l = SCSI_LUN_32(cmd);
    if((q = LU_Q(ha, b, t, l)) == NULL )
    {
        DRIVER_LOCK
        if( (q = (scsi_lu_t *)KMALLOC(sizeof(struct scsi_lu))) )
        {
           LU_Q(ha, b, t, l) = q;
           BZERO(q,sizeof(struct scsi_lu));
           DEBUG(sprintf(debug_buff,"Allocate new device queue 0x%x\n",q));
           DEBUG(qla1280_print(debug_buff));
           DRIVER_UNLOCK
        }
        else
        {
            CMD_RESULT(cmd) = (int) (DID_BUS_BUSY << 16);
            qla1280_done_q_put(sp, &ha->done_q_first, &ha->done_q_last);

            schedule_task(&ha->run_qla_bh);
            ha->flags.dpc_sched = TRUE;
            DRIVER_UNLOCK
            return(0);
        }
    }
    /* Set an invalid handle until we issue the command to ISP */
    /* then we will set the real handle value.                 */
    handle = INVALID_HANDLE;  
    CMD_HANDLE(cmd) = (unsigned char *)handle;

    /* Bookkeeping information */
    sp->r_start = jiffies;       /* time the request was recieved */
    sp->u_start = 0;              

    /* add the command to our queue */
    ha->qthreads++;
    qla1280_putq_t(q,sp);
    
    DEBUG(sprintf(debug_buff,"qla1280_queuecmd: queue pid=%d, hndl=0x%x\n\r",cmd->pid,handle));
    DEBUG(qla1280_print(debug_buff));

    /* send command to adapter */
    DRIVER_LOCK
        if (q->q_outcnt == 0)
            qla1280_restart_queues(ha);
    DRIVER_UNLOCK
    
    
    LEAVE("qla1280_queuecommand");
    return (0);
}

/**************************************************************************
 *   qla1200_abort
 *     Abort the speciifed SCSI command(s).
 **************************************************************************/
int
qla1280_abort(Scsi_Cmnd *cmd)
{
    scsi_qla_host_t *ha;
    srb_t  *sp;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif
    struct Scsi_Host *host;
    uint32_t        b, t, l;
    scsi_lu_t       *q;
    int return_status = SCSI_ABORT_SUCCESS;
    int found = 0;
    int i;
    u_long     handle;
    u_short    data;

    ENTER("qla1280_abort");
    COMTRACE('A')
    ha = (scsi_qla_host_t *) cmd->host->hostdata;
    host = cmd->host;
    DRIVER_LOCK

    /* Get the SCSI request ptr */
    sp = (srb_t *) CMD_SP(cmd);
    handle = (u_long) CMD_HANDLE(cmd);
    if (qla1280_verbose)
        printk("scsi(%d): ABORT Command=0x%lx, handle=0x%lx\n",(int)ha->host_no,(long)cmd,handle);

    /* Check for pending interrupts. */
    if( handle == 0L )
    {
    COMTRACE('a')
        /* we never got this command */
        printk(KERN_INFO "qla1280: Aborting a NULL handle\n");
        DRIVER_UNLOCK
                return(SCSI_ABORT_NOT_RUNNING);  /* no action - we don't have command */
    }
    data = qla1280_debounce_register(&ha->iobase->istatus);
    if( !(ha->flags.in_isr) && (data & RISC_INT) )
    {
        /* put any pending command in done queue */
        qla1280_isr(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
    }

    handle = (u_long) CMD_HANDLE(cmd);

    /* Generate LU queue on bus, target, LUN */
    b = SCSI_BUS_32(cmd);
    t = SCSI_TCN_32(cmd);
    l = SCSI_LUN_32(cmd);
    if((q = LU_Q(ha, b, t, l)) == NULL )
    {
    COMTRACE('a')
        /* No lun queue -- command must not be active */
        DRIVER_UNLOCK
        printk(KERN_WARNING "qla1280 (%d:%d:%d): No LUN queue for the specified device\n",(int)b,(int)t,(int)l);
        return(SCSI_ABORT_NOT_RUNNING);  /* no action - we don't have command */
    }

#if AUTO_ESCALATE_ABORT
    if ( (sp->flags & SRB_ABORTED) )
    {
        DRIVER_UNLOCK
        DEBUG(qla1280_print("qla1280_abort: Abort escalayted - returning SCSI_ABORT_SNOOZE.\n\r"));
        return(SCSI_ABORT_SNOOZE);
    }
#endif

    if ( (sp->flags & SRB_ABORT_PENDING) )
    {
    COMTRACE('a')
        DRIVER_UNLOCK
        if( qla1280_verbose  )
            printk("scsi(): Command has a pending abort message - ABORT_PENDING.\n");
        DEBUG(qla1280_print("qla1280: Command has a pending abort message - ABORT_PENDING.\n\r"));
        return(SCSI_ABORT_PENDING);
    }

#if  STOP_ON_ABORT 
    printk("Scsi layer issued a ABORT command= 0x%x\n",(int)cmd);
    DEBUG2(qla1280_print_scsi_cmd(cmd));
#endif

    ha->flags.in_abort = TRUE;
    /*
    * Normally, would would need to search our queue for the specified command
    * but; since our sp contains the cmd ptr, we can just remove it from our
    * LUN queue.
    */
    if( !(sp->flags&SRB_SENT) )
    { 
        found++;
        if( qla1280_verbose  )
            printk("scsi(): Command returned from queue aborted.\n");
        DEBUG(qla1280_print("qla1280: Command returned from queue aborted.\n\r"));
        /* Remove srb from SCSI LU queue. */
        qla1280_removeq(q, sp);
        sp->flags |=  SRB_ABORTED;
        CMD_RESULT(cmd) = DID_ABORT << 16; 
        qla1280_done_q_put(sp, &ha->done_q_first, &ha->done_q_last);
        return_status = SCSI_ABORT_SUCCESS;
    }
    else
    {  /* find the command in our active list */
        for (i = 1; i < MAX_OUTSTANDING_COMMANDS; i++)
        {
            if( sp == ha->outstanding_cmds[i] )
            {
                found++;
                DEBUG(qla1280_print("qla1280: RISC aborting command.\n\r"));
                qla1280_abort_command(ha,sp);
                return_status = SCSI_ABORT_PENDING;
                break;
            }
        }
    }

#if  STOP_ON_ABORT 
    qla1280_panic("qla1280_abort",ha->host);
#endif
    if ( found == 0 )
        return_status = SCSI_ABORT_NOT_RUNNING;  /* no action - we don't have command */

    DEBUG(sprintf(debug_buff, "qla1280_abort: Aborted status returned = 0x%x.\n\r",return_status));
    DEBUG(qla1280_print(debug_buff));

    if( ha->done_q_first )
       qla1280_done(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
    if ( found )
    {
        qla1280_restart_queues(ha);
    }
    ha->flags.in_abort = FALSE;
    DRIVER_UNLOCK

    LEAVE("qla1280_abort");
    COMTRACE('a')
    return(return_status);
}

/**************************************************************************
 * qla1200_reset
 *    The reset function will reset the SCSI bus and abort any executing
 *    commands. 
 *
 * Input:
 *      cmd = Linux SCSI command packet of the command that cause the
 *            bus reset.
 *      flags = SCSI bus reset option flags (see scsi.h)
 *
 * Returns:
 *      DID_RESET in cmd.host_byte of aborted command(s) 
 *
 * Note:
 *      Resetting the bus always succeeds - is has to, otherwise the
 *      kernel will panic! Try a surgical technique - sending a BUS
 *      DEVICE RESET message - on the offending target before pulling
 *      the SCSI bus reset line.
 **************************************************************************/
int
qla1280_reset(Scsi_Cmnd *cmd, unsigned int flags)
{
    scsi_qla_host_t *ha;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif
    uint32_t        b, t, l;
    srb_t  *sp;
    typedef enum
    {
        ABORT_DEVICE = 1, 
                DEVICE_RESET = 2, 
                BUS_RESET    = 3, 
                ADAPTER_RESET= 4,
                RESET_DELAYED= 5,
                FAIL         = 6
    } action_t;
    action_t     action = ADAPTER_RESET;
    u_short    data;
    scsi_lu_t       *q;
    int result;


    ENTER("qla1280_reset");
    COMTRACE('R')
    if (qla1280_verbose)
        printk("scsi(): Resetting Cmnd=0x%lx, Handle=0x%lx, flags=0x%x\n",(long)cmd,(long)CMD_HANDLE(cmd),flags);
    if ( cmd == NULL )
    {
        printk(KERN_WARNING "(scsi?:?:?:?) Reset called with NULL Scsi_Cmnd "
                "pointer, failing.\n");
        return(SCSI_RESET_SNOOZE);
    }
    ha = (scsi_qla_host_t *) cmd->host->hostdata;
    sp = (srb_t *) CMD_SP(cmd);

#if  STOP_ON_RESET 
    qla1280_panic("qla1280_reset",ha->host);
#endif 

    DRIVER_LOCK
    /* Check for pending interrupts. */
    data = qla1280_debounce_register(&ha->iobase->istatus);
    if( !(ha->flags.in_isr) && (data & RISC_INT) )
    {
        qla1280_isr(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
    }
    DRIVER_UNLOCK

    /*
    * Determine the suggested action that the mid-level driver wants
    * us to perform.
    */
    if( CMD_HANDLE(cmd) == (unsigned char *) 0  )
    {
        /* 
        * if mid-level driver called reset with a orphan SCSI_Cmnd 
        * (i.e. a command that's not pending ), so perform the 
        * function specified.
        */
        if( (flags & SCSI_RESET_SUGGEST_HOST_RESET) )
            action = ADAPTER_RESET;
        else 
            action = BUS_RESET;
    }
    else
    { /* 
        * Mid-level driver has called reset with this SCSI_Cmnd and 
        * its pending.
        */
        if( flags & SCSI_RESET_SUGGEST_HOST_RESET )
            action = ADAPTER_RESET;
        else if( flags & SCSI_RESET_SUGGEST_BUS_RESET )
            action = BUS_RESET;
        else 
            action = DEVICE_RESET;
    }

    b = SCSI_BUS_32(cmd);
    t = SCSI_TCN_32(cmd);
    l = SCSI_LUN_32(cmd);
    q = LU_Q(ha, b, t, l);

#if AUTO_ESCALATE_RESET
    if ( (action & DEVICE_RESET) && (q->q_flag & QLA1280_QRESET) )
    {
        printk(KERN_INFO "qla1280(%d): Bus device reset already sent to " "device, escalating.\n", (int)ha->host_no);
        action = BUS_RESET;
    }
    if ( (action & DEVICE_RESET) && (sp->flags & SRB_ABORT_PENDING) )
    {
        printk(KERN_INFO "qla1280(%d):Have already attempted to reach " "device with abort device\n", (int)ha->host_no);
        printk(KERN_INFO "qla1280(%d):message, will escalate to BUS " "RESET.\n",(int) ha->host_no);
        action = BUS_RESET;
    }
#endif

    /*
    *  By this point, we want to already know what we are going to do,
    *  so we only need to perform the course of action.
    */
    DRIVER_LOCK
    result = SCSI_RESET_ERROR;
    switch (action)
    {
        case FAIL:
            break;

        case RESET_DELAYED:
            result = SCSI_RESET_PENDING;
            break;

        case ABORT_DEVICE:
            ha->flags.in_reset = TRUE;
            if (qla1280_verbose)
                printk(KERN_INFO "scsi(%d:%d:%d:%d): Queueing abort device command.\n", (int)ha->host_no,(int)b,(int)t,(int)l); 
            qla1280_abort_queue_single(ha,b,t,l,DID_ABORT);
            if( qla1280_abort_device(ha, b, t, l) == 0)
                result = SCSI_RESET_PENDING;
            break;

        case DEVICE_RESET:
            if (qla1280_verbose)
                printk(KERN_INFO "scsi(%d:%d:%d:%d): Queueing device reset command.\n",(int) ha->host_no,(int)b,(int)t,(int)l); 
            ha->flags.in_reset = TRUE;
            for (l = 0; l < MAX_LUNS; l++)
                qla1280_abort_queue_single(ha,b,t,l,DID_ABORT);
            if( qla1280_device_reset(ha, b, t) == 0 ) 
                result = SCSI_RESET_PENDING;
            q->q_flag |= QLA1280_QRESET;
            break;

        case BUS_RESET:
                if (qla1280_verbose)
                    printk(KERN_INFO "qla1280(%d:%d:%d:%d): Issuing BUS DEVICE RESET.\n",(int) ha->host_no,(int)b,(int)t,(int)l); 
            ha->flags.in_reset = TRUE;
            for (t = 0; t < MAX_TARGETS; t++)
                for (l = 0; l < MAX_LUNS; l++)
                    qla1280_abort_queue_single(ha,b,t,l,DID_RESET);
                qla1280_bus_reset(ha, b);
                /*
                * The bus reset routine returns all the outstanding commands back
                * with "DID_RESET" in the status field after a short delay
                * by the firmware. If the mid-level time out the SCSI reset before
                * our delay we may need to ignore it.
                */
                /* result = SCSI_RESET_PENDING | SCSI_RESET_BUS_RESET; */
                result = SCSI_RESET_SUCCESS | SCSI_RESET_BUS_RESET;
                mdelay(4 * 1000); barrier();
                if( flags & SCSI_RESET_SYNCHRONOUS )
                {
                  CMD_RESULT(cmd) = (int) (DID_BUS_BUSY << 16);
                  (*(cmd)->scsi_done)(cmd); 
                }
                /* ha->reset_start = jiffies; */
                break;

            case ADAPTER_RESET:
            default:
                if (qla1280_verbose)
                {
                    printk(KERN_INFO "scsi(%d:%d:%d:%d): Issued an ADAPTER RESET.\n",(int) ha->host_no,(int)b,(int)t,(int)l); 
                    printk(KERN_INFO "scsi(%d:%d:%d:%d): I/O processing will continue automatically.\n",(int) ha->host_no,(int)b,(int)t,(int)l); 
                }
                ha->flags.reset_active = TRUE;
                /* 
                * We restarted all of the commands automatically, so the mid-level code can expect 
                * completions momentitarily.
                */
                if( qla1280_abort_isp(ha) == 0 )
                    result = SCSI_RESET_SUCCESS | SCSI_RESET_HOST_RESET;

                        ha->flags.reset_active = FALSE;
    }

    if( ha->done_q_first ) 
        qla1280_done(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
    qla1280_restart_queues(ha);
    ha->flags.in_reset = FALSE;
    
DRIVER_UNLOCK
    DEBUG(printk("RESET returning %d\n", result)); 

    COMTRACE('r')
    LEAVE("qla1280_reset");
    return( result );
}

/**************************************************************************
 * qla1200_biosparam
 *   Return the disk geometry for the given SCSI device.
 **************************************************************************/
int
qla1280_biosparam(Disk *disk, kdev_t dev, int geom[])
{
    int heads, sectors, cylinders;

            heads = 64;
    sectors = 32;
    cylinders = disk->capacity / (heads * sectors);
    if (cylinders > 1024)
    {
        heads = 255;
        sectors = 63;
        cylinders = disk->capacity / (heads * sectors);
        /* if (cylinders > 1023)
        cylinders = 1023; */
    }

    geom[0] = heads;
    geom[1] = sectors;
    geom[2] = cylinders;

    return (0);
}
/**************************************************************************
 * qla1280_intr_handler
 *   Handles the H/W interrupt
 **************************************************************************/
void qla1280_intr_handler(int irq, void *dev_id, struct pt_regs *regs)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif
    scsi_qla_host_t *ha;
    u_short    data;
    device_reg_t *reg;

    ENTER_INTR("qla1280_intr_handler");
    COMTRACE('I')
    ha = (scsi_qla_host_t *) dev_id;
    if(!ha)
    {
        printk(KERN_INFO "scsi(): Interrupt with NULL host ptr\n");
        COMTRACE('X')
        return;
    }
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,95)
    spin_lock_irqsave(&io_request_lock, cpu_flags);
    if(test_and_set_bit(QLA1280_IN_ISR_BIT, &ha->flags))
    {
        COMTRACE('X')
        return;
    }
    ha->isr_count++;
    reg = ha->iobase;
     /* disable our interrupt. */
    WRT_REG_WORD(&reg->ictrl, 0); 
    data = qla1280_debounce_register(&reg->istatus);
    /* Check for pending interrupts. */
    if ( !(data & RISC_INT) )
    {
        /* spurious interrupts can happen legally */
        DEBUG(printk("scsi(%d): Spurious interrupt - ignoring\n",(int)ha->host_no));
        COMTRACE('X')
    }
    else
      qla1280_isr(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
    if (ha->done_q_first)
        qla1280_done(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);

    clear_bit(QLA1280_IN_ISR_BIT, &ha->flags);
    spin_unlock_irqrestore(&io_request_lock, cpu_flags);
#else  /* LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95) */

    if( test_bit(QLA1280_IN_ISR_BIT, (int *)&ha->flags) )
    {
          COMTRACE('X')
          printk(KERN_INFO "scsi(%d): Already in interrupt - returning \n", (int)ha->host_no);
          return;
    }
    set_bit(QLA1280_IN_ISR_BIT, (int *)&ha->flags);
    ha->isr_count++;
    reg = ha->iobase;
     /* disable our interrupt. */
    WRT_REG_WORD(&reg->ictrl, 0); 

    data = qla1280_debounce_register(&reg->istatus);
    /* Check for pending interrupts. */
    if ( !(data & RISC_INT) )
    {
        /* spurious interrupts can happen legally */
        DEBUG(printk("scsi(%d): Spurious interrupt - ignoring\n",(int)ha->host_no));
        COMTRACE('X')
    }
    else
     qla1280_isr(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);

    /* if no work to do then call the SCSI mid-level right away */
    if( ha->done_q_first )
        qla1280_done(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);

    /* Schedule the DPC routine */
    if (ha->flags.isp_abort_needed || ha->flags.reset_marker ||
            ha->done_q_first        )
        {
            ha->run_qla_bh.data = (void *) ha;
            ha->run_qla_bh.routine = qla1280_do_dpc; 

             COMTRACE('P') 
            schedule_task(&ha->run_qla_bh);
            ha->flags.dpc_sched = TRUE;
        }
        clear_bit(QLA1280_IN_ISR_BIT, (int *)&ha->flags);
#endif
     /* enable our interrupt. */
        WRT_REG_WORD(&reg->ictrl, ISP_EN_INT + ISP_EN_RISC);

        COMTRACE('i')  
        LEAVE_INTR("qla1280_intr_handler");
}

/**************************************************************************
 *   qla1280_do_dpc
 *
 * Description:
 * This routine is a task that is schedule by the interrupt handler 
 * to perform the background processing for interrupts.  We put it 
 * on a task queue that is consumed whenever the scheduler runs; that's
 * so you can do anything (i.e. put the process to sleep etc).  In fact, the 
 * mid-level tries to sleep when it reaches the driver threshold 
 * "host->can_queue". This can cause a panic if we were in our interrupt
 * code .
 **************************************************************************/
static void qla1280_do_dpc(void *p)
{
    scsi_qla_host_t *ha = (scsi_qla_host_t *) p;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif

    COMTRACE('p')  
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,95)
    spin_lock_irqsave(&io_request_lock, cpu_flags);
#endif
    if (ha->flags.isp_abort_needed)
        qla1280_abort_isp(ha);

    if (ha->flags.reset_marker)
        qla1280_rst_aen(ha);

    if (ha->done_q_first)
        qla1280_done(ha, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
    ha->flags.dpc_sched = FALSE;
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,95)
    spin_unlock_irqrestore(&io_request_lock, cpu_flags);
#endif
}

/**************************************************************************
 *   qla1280_device_queue_depth
 *
 * Description:
 *   Determines the queue depth for a given device.  There are two ways
 *   a queue depth can be obtained for a tagged queueing device.  One
 *   way is the default queue depth which is determined by whether
 *   If it is defined, then it is used
 *   as the default queue depth.  Otherwise, we use either 4 or 8 as the
 *   default queue depth (dependent on the number of hardware SCBs).
 **************************************************************************/
STATIC void qla1280_device_queue_depth(scsi_qla_host_t *p, Scsi_Device *device)
{
    int default_depth = 3;
    int bus = device->channel;
    int target = device->id;

    device->queue_depth = default_depth;

    if (device->tagged_supported &&
        (p->bus_settings[bus].qtag_enables & (BIT_0 << target)) )
    {
        device->tagged_queue = 1;
        device->current_tag = 0;
        device->queue_depth = p->bus_settings[bus].hiwat; 
        /* device->queue_depth = 20; */
        printk(KERN_INFO "scsi(%d:%d:%d:%d): Enabled tagged queuing, queue depth %d.\n",
                (int)p->host_no, device->channel, device->id,
                device->lun, device->queue_depth);
    }
    qla12160_get_target_parameters(p, bus, target, device->lun);

}

/**************************************************************************
 *   qla1280_select_queue_depth
 *
 *   Sets the queue depth for each SCSI device hanging off the input
 *   host adapter.  We use a queue depth of 2 for devices that do not
 *   support tagged queueing.
 **************************************************************************/
STATIC void
qla1280_select_queue_depth(struct Scsi_Host *host, Scsi_Device *scsi_devs)
{
    Scsi_Device *device;
    scsi_qla_host_t  *p = (scsi_qla_host_t *) host->hostdata;

    ENTER("qla1280_select_queue_depth");
    for (device = scsi_devs; device != NULL; device = device->next)
    {
        if (device->host == host)
            qla1280_device_queue_depth(p, device);
    }
    LEAVE("qla1280_select_queue_depth");
}

/*--------------------------**
** Driver Support Routines  **
**--------------------------*/

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,0)
/*
 * mdelay
 *      Delay in milliseconds 
 *
 * Input:
 *      milliseconds  = delay 
 */
STATIC inline void mdelay(int milliseconds)
{
    int i;

    for(i=0; i<milliseconds; i++)
        udelay(1000);
}
#endif

/*
 * qla1280_done
 *      Process completed commands.
 *
 * Input:
 *      ha           = adapter block pointer.
 *      done_q_first = done queue first pointer.
 *      done_q_last  = done queue last pointer.
 */
STATIC void
qla1280_done(scsi_qla_host_t *ha, srb_t **done_q_first, srb_t **done_q_last)
{
    srb_t           *sp;
    scsi_lu_t       *q;
    uint32_t        b, t, l;
    Scsi_Cmnd  *cmd;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif

    ENTER("qla1280_done");
    COMTRACE('D') 

    DRIVER_LOCK 
    while (*done_q_first !=  NULL)
    {
        /* remove command from done list */
                sp = *done_q_first;
        if (!(*done_q_first = sp->s_next))
            *done_q_last = NULL;
        else
            (*done_q_first)->s_prev = NULL;
                cmd = sp->cmd;
        b = SCSI_BUS_32(cmd);
        t = SCSI_TCN_32(cmd);
        l = SCSI_LUN_32(cmd);
        q = LU_Q(ha, b, t, l);

        /* Decrement outstanding commands on device. */
        if (q->q_outcnt)
            q->q_outcnt--;
        if (q->q_outcnt < ha->bus_settings[b].hiwat)
        {
            q->q_flag &= ~QLA1280_QBUSY;
        }
        
        q->resp_time += jiffies - sp->r_start;                /* Lun bookkeeping information */
        q->act_time += jiffies - sp->u_start;
        q->io_cnt++;
        if( sp->dir & BIT_5 )
         q->r_cnt++;
        else
         q->w_cnt++;

        switch ( (CMD_RESULT(cmd)>>16))
        {
            case DID_RESET:
                q->q_flag &= ~QLA1280_QRESET;
                /* Issue marker command. */
                qla1280_marker(ha, b, t, 0, MK_SYNC_ID); 
                break;
            case DID_ABORT:
                sp->flags &= ~SRB_ABORT_PENDING;
                sp->flags |= SRB_ABORTED;
                if (sp->flags & SRB_TIMEOUT)
                    CMD_RESULT(sp->cmd)= DID_TIME_OUT << 16;
                break;
            default:
                break;
        }

        /* Call the mid-level driver interrupt handler */
        CMD_HANDLE(sp->cmd) = (unsigned char *) 0;
        ha->actthreads--;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
        sti(); 
        (*(cmd)->scsi_done)(cmd);
        cli(); 
#else
        (*(cmd)->scsi_done)(cmd);
#endif
        qla1280_next(ha, q, b);
    }
    DRIVER_UNLOCK 


    COMTRACE('d') 
    LEAVE("qla1280_done");
}

/*
 * Translates a ISP error to a Linux SCSI error
 */
STATIC int qla1280_return_status( sts_entry_t *sts, Scsi_Cmnd       *cp)
{
    int host_status = DID_ERROR;
#if DEBUG_QLA1280_INTR
    STATIC char *reason[] =
    {
        "DID_OK",
                "DID_NO_CONNECT",
                "DID_BUS_BUSY",
                "DID_TIME_OUT",
                "DID_BAD_TARGET",
                "DID_ABORT",
                "DID_PARITY",
                "DID_ERROR",
                "DID_RESET",
                "DID_BAD_INTR"
    };
#endif /* DEBUG_QLA1280_INTR */

    ENTER("qla1280_return_status");

#if DEBUG_QLA1280_INTR
    /*
    DEBUG(printk("qla1280_return_status: compl status = 0x%04x\n", sts->comp_status));
    */
#endif
    switch(sts->comp_status)
    {
        case CS_COMPLETE:
            host_status = DID_OK;
            break;
        case CS_INCOMPLETE:
            if (!(sts->state_flags & SF_GOT_BUS))
                host_status = DID_NO_CONNECT;
            else if (!(sts->state_flags & SF_GOT_TARGET))
                host_status = DID_BAD_TARGET;
            else if (!(sts->state_flags & SF_SENT_CDB))
                host_status = DID_ERROR;
            else if (!(sts->state_flags & SF_TRANSFERRED_DATA))
                host_status = DID_ERROR;
            else if (!(sts->state_flags & SF_GOT_STATUS))
                host_status = DID_ERROR;
            else if (!(sts->state_flags & SF_GOT_SENSE))
                host_status = DID_ERROR;
            break;
        case CS_RESET:
            host_status = DID_RESET;
            break;
        case CS_ABORTED:
            host_status = DID_ABORT;
            break;
        case CS_TIMEOUT:
            host_status = DID_TIME_OUT;
            break;
        case CS_DATA_OVERRUN:
#ifdef QL_DEBUG_LEVEL_2 
            printk("Data overrun 0x%x\n",(int)sts->residual_length);
            qla1280_print(
                        "\n\rqla1280_isr: response packet data\n\r");
                        qla1280_dump_buffer((caddr_t)sts,
                        RESPONSE_ENTRY_SIZE); 
#endif
            host_status = DID_ERROR;
            break;
        case CS_DATA_UNDERRUN:
            if ( (CMD_XFRLEN(cp) - sts->residual_length) < cp->underflow)    
            { 
              printk("scsi: Underflow detected - retrying command.\n");
              host_status = DID_ERROR;
            }
            else
                host_status = DID_OK;
            break;
        default:
            host_status = DID_ERROR;
            break;
    }

#if DEBUG_QLA1280_INTR
    sprintf(debug_buff, "qla1280 ISP status: host status (%s) scsi status %x\n\r", reason[host_status], sts->scsi_status);
    qla1280_print(debug_buff);
#endif

    LEAVE("qla1280_return_status");

    return (sts->scsi_status & 0xff) | (host_status << 16);
}

/*
 * qla1280_done_q_put
 *      Place SRB command on done queue.
 *
 * Input:
 *      sp           = srb pointer.
 *      done_q_first = done queue first pointer.
 *      done_q_last  = done queue last pointer.
 */
STATIC void
qla1280_done_q_put(srb_t *sp, srb_t **done_q_first, srb_t **done_q_last)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif
#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_put_done_q");
#endif
    /* Place block on done queue */
    DRIVER_LOCK
            sp->s_next = NULL;
    sp->s_prev = *done_q_last;
    if (!*done_q_first)
        *done_q_first = sp;
    else
        (*done_q_last)->s_next = sp;
    *done_q_last = sp;

    DRIVER_UNLOCK
#ifdef QL_DEBUG_LEVEL_3
            LEAVE("qla1280_put_done_q");
#endif
}

/*
 * qla1280_next
 *      Retrieve and process next job in the queue.
 *
 * Input:
 *      ha = adapter block pointer.
 *      q  = SCSI LU pointer.
 *      b  = SCSI bus number.
 *      SCSI_LU_Q lock must be already obtained and no other locks.
 *
 * Output:
 *      Releases SCSI_LU_Q upon exit.
 */
STATIC void
qla1280_next(scsi_qla_host_t *ha, scsi_lu_t *q, uint8_t b)
{
    srb_t   *sp;
    uint32_t cnt;
    uint8_t status;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif

    ENTER("qla1280_next");

    DRIVER_LOCK
    while ( ((sp = q->q_first) != NULL) &&     /* we have a queue pending */
        !(q->q_flag &  QLA1280_QBUSY) &&      /* device not busy */
        !ha->flags.abort_isp_active &&      /* not resetting the adapter */
        !(q->q_flag & QLA1280_QSUSP) )      /* device not suspended */
    {
        /* Remove srb from SCSI LU queue. */
        qla1280_removeq(q, sp);

        DEBUG(sprintf(debug_buff,"starting request 0x%p<-(0x%p)\n\r",q,sp));
        DEBUG(qla1280_print(debug_buff));
        {
            /* Set busy flag if reached high water mark. */
            q->q_outcnt++;
            if (q->q_outcnt >= ha->bus_settings[b].hiwat)
                q->q_flag |= QLA1280_QBUSY;

#if  QLA1280_64BIT_SUPPORT
            if (ha->flags.enable_64bit_addressing)
                status = qla1280_64bit_start_scsi(ha, sp);
            else
#endif
                status = qla1280_32bit_start_scsi(ha, sp);

            if (status)  /* if couldn't start the request */
            {
                if (q->q_outcnt == 1)
                {
                    /* Release SCSI LU queue specific lock */
                    QLA1280_SCSILU_UNLOCK(q);

                    /* Wait for 30 sec for command to be accepted. */
                    for (cnt = 6000000; cnt; cnt--)
                    {
#if  QLA1280_64BIT_SUPPORT
                        if (ha->flags.enable_64bit_addressing)
                            status = qla1280_64bit_start_scsi(ha, sp);
                        else
#endif
                            status = qla1280_32bit_start_scsi(ha, sp);

                        if (!status)
                        {
                            break;
                        }

                        /* Go check for pending interrupts. */
                        qla1280_poll(ha);

                        SYS_DELAY(5);  /* 10 */
                    }
                    if (!cnt)
                    {
                        /* Set timeout status */
                        CMD_RESULT(sp->cmd) = DID_TIME_OUT << 16;

#if WATCHDOGTIMER
                        /* Remove command from watchdog queue. */
                        if (sp->flags & SRB_WATCHDOG)
                            qla1280_timeout_remove(ha, sp);
#endif
                        COMTRACE('M') 
                        CMD_HANDLE(sp->cmd) = (unsigned char *) 0;

                        /* Call the mid-level driver interrupt handler */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
                        sti(); 
                        (*(sp->cmd)->scsi_done)(sp->cmd);
                        cli(); 
#else
                        (*(sp->cmd)->scsi_done)(sp->cmd);
#endif

                        /* Acquire LU queue specific lock */
                        QLA1280_SCSILU_LOCK(q);

                        if (q->q_outcnt)
                            q->q_outcnt--;
                    }
                    else
                        /* Acquire LU queue specific lock */
                        QLA1280_SCSILU_LOCK(q);
                }
                else
                {   /* Place request back on top of device queue. */
                    qla1280_putq_t(q, sp);

                    if (q->q_outcnt)
                        q->q_outcnt--;
                    if (q->q_outcnt < ha->bus_settings[b].hiwat)
                        q->q_flag &= ~QLA1280_QBUSY;
                    break;
                }
            }
        }
    }
   DRIVER_UNLOCK

    /* Release SCSI LU queue specific lock */
    QLA1280_SCSILU_UNLOCK(q);

    LEAVE("qla1280_next");
}

/*
 * qla1280_putq_t
 *      Add the standard SCB job to the top of standard SCB commands.
 *
 * Input:
 *      q  = SCSI LU pointer.
 *      sp = srb pointer.
 *      SCSI_LU_Q lock must be already obtained.
 */
STATIC void
qla1280_putq_t(scsi_lu_t *q, srb_t *sp)
{
    srb_t *srb_p;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_putq_t");
#endif
    DRIVER_LOCK
    DEBUG(sprintf(debug_buff,"Adding to device 0x%p<-(0x%p)\n\r",q,sp));
    DEBUG(qla1280_print(debug_buff));
    sp->s_next = NULL;
    if (!q->q_first)                  /* If queue empty */
    {
        sp->s_prev = NULL;
        q->q_first = sp;
        q->q_last = sp;
    }
    else
    {      
        srb_p = q->q_first;
        while (srb_p )
            srb_p = srb_p->s_next;

        if (srb_p)
        {
            sp->s_prev = srb_p->s_prev;
            if (srb_p->s_prev)
                srb_p->s_prev->s_next = sp;
            else
                q->q_first = sp;
            srb_p->s_prev = sp;
            sp->s_next = srb_p;
        }
        else
        {
            sp->s_prev = q->q_last;
            q->q_last->s_next = sp;
            q->q_last = sp;
        }
    }

    DRIVER_UNLOCK
#ifdef QL_DEBUG_LEVEL_3
            LEAVE("qla1280_putq_t");
#endif
}

/*
 * qla1280_removeq
 *      Function used to remove a command block from the
 *      LU queue.
 *
 * Input:
 *      q  = SCSI LU pointer.
 *      sp = srb pointer.
 *      SCSI_LU_Q lock must be already obtained.
 */
STATIC void
qla1280_removeq(scsi_lu_t *q, srb_t *sp)
{
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif
    DEBUG(sprintf(debug_buff,"Removing from device_q (0x%p)->(0x%p)\n\r",q,sp));
    DEBUG(qla1280_print(debug_buff));
    DRIVER_LOCK
            if (sp->s_prev)
    {
        if ((sp->s_prev->s_next = sp->s_next) != NULL)
            sp->s_next->s_prev = sp->s_prev;
        else
            q->q_last = sp->s_prev;
    }
    else if (!(q->q_first = sp->s_next))
        q->q_last = NULL;
    else
        q->q_first->s_prev = NULL;
    DRIVER_UNLOCK
}

/*
* qla1280_mem_alloc
*      Allocates adapter memory.
*
* Returns:
*      0  = success.
*      1  = failure.
*/
STATIC uint8_t
qla1280_mem_alloc(scsi_qla_host_t *ha)
{

	uint8_t   status = 1;

#ifdef QL_DEBUG_LEVEL_3
	ENTER("qla1280_mem_alloc");
#endif

#ifdef DYNAMIC_MEM_ALLOC
	ha->request_ring = qla1280_alloc_phys(REQUEST_ENTRY_SIZE * REQUEST_ENTRY_CNT,
	&ha->request_dma);
	if(ha->request_ring) {
		ha->response_ring = qla1280_alloc_phys(RESPONSE_ENTRY_SIZE * RESPONSE_ENTRY_CNT,
		&ha->response_dma);
		if(ha->response_ring) {
			status = 0;
		}
	}
#else
	ha->request_ring = &ha->req[0];
	ha->request_dma = VIRT_TO_BUS(&ha->req[0]);
	ha->response_ring = &ha->res[0];
	ha->response_dma = VIRT_TO_BUS(&ha->res[0]);
	status = 0;
#endif

	if(status) {
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
		qla1280_print("qla1280_mem_alloc: **** FAILED ****\n");
#endif
	}
#ifdef QL_DEBUG_LEVEL_3
	else
		LEAVE("qla1280_mem_alloc");
#endif
	return(status);
}

/*
 * qla1280_mem_free
 *      Frees adapter allocated memory.
 *
 * Input:
 *      ha = adapter block pointer.
 */
STATIC void
qla1280_mem_free(scsi_qla_host_t *ha)
{
    scsi_lu_t *q;
    uint32_t  b, t, l;

    ENTER("qlc1280_mem_free");
    if (ha)
    {
        /* Free device queues. */
        for (b = 0; b < MAX_BUSES; b++)
        {
            q = LU_Q(ha, b, ha->bus_settings[b].id, 0);
            for (t = 0; t < MAX_TARGETS; t++)
                for (l = 0; l < MAX_LUNS; l++)
                    if (LU_Q(ha, b, t, l) != NULL && LU_Q(ha, b, t, l) != q)
                        KMFREE(LU_Q(ha, b, t, l),sizeof(struct scsi_lu));
                    KMFREE(q, sizeof(struct scsi_lu));
        }
        for( b =0; b < MAX_EQ; b++ )
            ha->dev[b] =  (scsi_lu_t  *)NULL;
    }

    LEAVE("qlc1280_mem_free");
}




/****************************************************************************/
/*                QLogic ISP1280 Hardware Support Functions.                */
/****************************************************************************/

 /*
 * qla2100_enable_intrs
 * qla2100_disable_intrs
 *
 * Input:
 *      ha = adapter block pointer.
 *
 * Returns:
 *      None      
 */
    static inline void qla1280_enable_intrs(scsi_qla_host_t *ha) {
        device_reg_t *reg;

        reg = ha->iobase;
        ha->flags.interrupts_on = 1;
        /* enable risc and host interrupts */
        WRT_REG_WORD(&reg->ictrl, (ISP_EN_INT+ ISP_EN_RISC));
    }

    static inline void qla1280_disable_intrs(scsi_qla_host_t *ha) {
        device_reg_t *reg;

        reg = ha->iobase;
        ha->flags.interrupts_on = 0;
        /* disable risc and host interrupts */
        WRT_REG_WORD(&reg->ictrl, 0);
    }

/*
 * qla1280_initialize_adapter
 *      Initialize board.
 *
 * Input:
 *      ha = adapter block pointer.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_initialize_adapter(scsi_qla_host_t *ha)
{
    device_reg_t *reg;
    uint8_t      status;
    /* uint8_t      cnt; */
    uint8_t      b;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_initialize_adapter");
#endif

    /* Clear adapter flags. */
    ha->flags.online = FALSE;
    ha->flags.isp_abort_needed = FALSE;
    ha->flags.disable_host_adapter = FALSE;
    ha->flags.reset_active = FALSE;
    ha->flags.abort_isp_active = FALSE;
    ha->flags.watchdog_enabled = FALSE;

    DEBUG(printk("Configure PCI space for adapter...\n"));
    if (!(status = qla1280_pci_config(ha)))
    {
        reg = ha->iobase;

        /* Disable ISP interrupts. */
        WRT_REG_WORD(&reg->ictrl, 0);

        /* Insure mailbox registers are free. */
        WRT_REG_WORD(&reg->semaphore, 0);
        WRT_REG_WORD(&reg->host_cmd, HC_CLR_RISC_INT);
        WRT_REG_WORD(&reg->host_cmd, HC_CLR_HOST_INT);

        /* If firmware needs to be loaded */
        if (qla1280_verbose)
         printk("scsi(%d): Determining if RISC is loaded...\n",(int)ha->host_no);
        if (qla1280_isp_firmware(ha))
        {
            if (qla1280_verbose)
              printk("scsi(%d): Verifying chip...\n",(int)ha->host_no);
            if (!(status = qla1280_chip_diag(ha)))
            {
                if (qla1280_verbose)
                  printk("scsi(%d): Setup chip...\n",(int)ha->host_no);
                status = qla1280_setup_chip(ha); 
            }
        }

        if (!status)
        {
            /* Setup adapter based on NVRAM parameters. */
            if (qla1280_verbose)
              printk("scsi(%d): Configure NVRAM parameters...\n",(int)ha->host_no);
            qla1280_nvram_config(ha);

            if (!ha->flags.disable_host_adapter &&
                !qla1280_init_rings(ha))
            {
                /* Issue SCSI reset. */
                for (b = 0; b < ha->ports; b++)
                    if (!ha->bus_settings[b].disable_scsi_reset)
                    {
                      /* dg 03/13 if we can't reset twice then bus is dead */
                        if( qla1280_bus_reset(ha, b) )
                           if( qla1280_bus_reset(ha, b) )
                           {
                               ha->bus_settings[b].scsi_bus_dead = TRUE;
                            }
                    }

                    do
                    {
                        /* Issue marker command. */
                        ha->flags.reset_marker = FALSE;
                        for (b = 0; b < ha->ports; b++)
                        {
                            ha->bus_settings[b].reset_marker = FALSE;
                            qla1280_marker(ha, b, 0, 0, MK_SYNC_ALL);
                        }
                    }while (ha->flags.reset_marker);

                    ha->flags.online = TRUE;

                    /* Enable host adapter target mode. */
                    for (b = 0; b < ha->ports; b++)
                    {
                        if (!(status = qla1280_enable_tgt(ha, b)))
                        {
                            /* for (cnt = 0; cnt < MAX_LUNS; cnt++)
                            {
                                qla1280_enable_lun(ha, b, cnt);
                                 qla1280_poll(ha);
                            }*/
                        }
                        else
                            break;
                    }
            }
            else
                status = 1;
        }
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_initialize_adapter: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_initialize_adapter");
#endif
    return(status);
}

/*
 * qla1280_enable_tgt
 *      Enable target mode.
 *
 * Input:
 *      ha = adapter block pointer.
 *      b  = SCSI bus number.
 *
 * Returns:
 *      0 = success.
 */
STATIC uint8_t
qla1280_enable_tgt(scsi_qla_host_t *ha, uint8_t b)
{
    uint8_t     status = 0;
    /*  uint16_t    mb[MAILBOX_REGISTER_COUNT]; */

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_enable_tgt: entered\n\r");
#endif

    /* Enable target mode. */
    /*
    mb[0] = MBC_ENABLE_TARGET_MODE;
    mb[1] = BIT_15;
    mb[2] = (uint16_t)(b << 15);
    status = qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0, &mb[0]);
    */
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_enable_tgt: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_enable_tgt: exiting normally\n\r");
#endif
    return(status);
}

/*
 * ISP Firmware Test
 *      Checks if present version of RISC firmware is older than
 *      driver firmware.
 *
 * Input:
 *      ha = adapter block pointer.
 *
 * Returns:
 *      0 = firmware does not need to be loaded.
 */
STATIC uint8_t
qla1280_isp_firmware(scsi_qla_host_t *ha)
{
    nvram_t     *nv    = (nvram_t *)ha->response_ring; 
    uint16_t    *wptr;
    uint8_t     chksum;
    uint8_t     cnt;
    uint8_t     status = 0;    /* dg 2/27 always loads RISC */
    uint16_t    mb[MAILBOX_REGISTER_COUNT];

    ENTER("qla1280_isp_firmware");

    /* Verify valid NVRAM checksum. */
    wptr = (uint16_t *)ha->response_ring;
    DEBUG(printk("qla1280_isp_firmware: Reading NVRAM\n"));
    chksum = 0;
    for (cnt = 0; cnt < sizeof(nvram_t)/2; cnt++)
    {
        *wptr = qla1280_get_nvram_word(ha, cnt);
        chksum += (uint8_t)*wptr;
        chksum += (uint8_t)(*wptr >> 8);
        wptr++;
    }
    DEBUG(printk("qla1280_isp_firmware: Completed Reading NVRAM\n"));

#if defined(QL_DEBUG_LEVEL_3)
    sprintf(debug_buff,"qla1280_isp_firmware: NVRAM Magic ID= %c %c %c\n\r",(char *) nv->id[0],nv->id[1],nv->id[2]);
    qla1280_print(debug_buff);
#endif

    /* Bad NVRAM data, load RISC code. */
    if (chksum || nv->id[0] != 'I' || nv->id[1] != 'S' ||
        nv->id[2] != 'P' || nv->id[3] != ' ' || nv->version < 1)
    {
        printk(KERN_INFO "qla1280_isp_firmware: Bad checksum or magic number or version in NVRAM.\n");
        ha->flags.disable_risc_code_load = FALSE;
    }
    else
        ha->flags.disable_risc_code_load = nv->cntr_flags_1.disable_loading_risc_code;

    if (ha->flags.disable_risc_code_load)
    {
#if defined(QL_DEBUG_LEVEL_3)
        qla1280_print("qla1280_isp_firmware: Telling RISC to verify checksum of loaded BIOS code.\n\r");
#endif
        /* Verify checksum of loaded RISC code. */
        mb[0] = MBC_VERIFY_CHECKSUM;
        /* mb[1] = ql12_risc_code_addr01; */
        mb[1] = *QLBoardTbl[ha->devnum].fwstart;  

        if (!(status = qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0])))
        {
            /* Start firmware execution. */
#if defined(QL_DEBUG_LEVEL_3)
            qla1280_print("qla1280_isp_firmware: Startng F/W execution.\n\r");
#endif
            mb[0] = MBC_EXECUTE_FIRMWARE;
            /* mb[1] = ql12_risc_code_addr01; */
            mb[1] = *QLBoardTbl[ha->devnum].fwstart;  
            qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0]);
        }
        else
            printk(KERN_INFO "qla1280: RISC checksum failed.\n");
    }
    else
    {
        DEBUG(printk("qla1280: NVRAM configured to load RISC load.\n"));
        status = 1;
     }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print(
                "qla1280_isp_firmware: **** Load RISC code ****\n\r");
#endif
    LEAVE("qla1280_isp_firmware");
    return(status);
}

/*
 * PCI configuration
 *      Setup device PCI configuration registers.
 *
 * Input:
 *      ha = adapter block pointer.
 *
 * Returns:
 *      0 = success.
 */
STATIC uint8_t
qla1280_pci_config(scsi_qla_host_t *ha)
{
    uint8_t      status = 1;
    uint32_t     command;
#if MEMORY_MAPPED_IO
    uint32_t  page_offset, base;
    uint32_t   mmapbase;
#endif
    config_reg_t *creg = 0;
    uint16_t     buf_wd;

    ENTER("qla1280_pci_config");

    /* Get command register. */
    if (pci_read_config_word(ha->pdev,OFFSET(creg->command), &buf_wd) == PCIBIOS_SUCCESSFUL)
    {
        command = buf_wd;
        /*
        * Set Bus Master Enable, Memory Address Space Enable and
        * reset any error bits.
        */
        buf_wd &= ~0x7;
#if MEMORY_MAPPED_IO
        DEBUG(printk("qla1280: MEMORY MAPPED IO is enabled.\n"));
        buf_wd |= BIT_2 + BIT_1 + BIT_0;
#else
        buf_wd |= BIT_2 + BIT_0;
#endif
        if( pci_write_config_word(ha->pdev,OFFSET(creg->command), buf_wd) )
        {
            printk(KERN_WARNING "qla1280: Could not write config word.\n");
        }
        /* Get expansion ROM address. */
        if (pci_read_config_word(ha->pdev,OFFSET(creg->expansion_rom), &buf_wd) == PCIBIOS_SUCCESSFUL)
        {
            /* Reset expansion ROM address decode enable. */
            buf_wd &= ~BIT_0;
            if (pci_write_config_word(ha->pdev,OFFSET(creg->expansion_rom), buf_wd) == PCIBIOS_SUCCESSFUL)
            {
#if MEMORY_MAPPED_IO
                /* Get memory mapped I/O address. */
                pci_read_config_dword(ha->pdev,OFFSET(cfgp->mem_base_addr), &mmapbase);
                mmapbase &= PCI_BASE_ADDRESS_MEM_MASK;

                /* Find proper memory chunk for memory map I/O reg. */
                base = mmapbase & PAGE_MASK;
                page_offset = mmapbase - base;
                /* Get virtual address for I/O registers. */
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,0)
                ha->mmpbase = ioremap_nocache(base, page_offset + 256);
#else
                ha->mmpbase = vremap(base,page_offset + 256);
#endif
                if( ha->mmpbase )
                {
                    ha->mmpbase += page_offset;
                    /* ha->iobase = ha->mmpbase; */
                    status = 0;
                }
#else /* MEMORY_MAPPED_IO */
                status = 0;
#endif /* MEMORY_MAPPED_IO */
            }
        }
    }

    LEAVE("qla1280_pci_config");
    return(status);
}

/*
 * Chip diagnostics
 *      Test chip for proper operation.
 *
 * Input:
 *      ha = adapter block pointer.
 *
 * Returns:
 *      0 = success.
 */
STATIC uint8_t
qla1280_chip_diag(scsi_qla_host_t *ha)
{
    device_reg_t *reg   = ha->iobase;
    uint8_t      status = 0;
    uint16_t     data;
    uint32_t     cnt;
    uint16_t     mb[MAILBOX_REGISTER_COUNT];

#ifdef QL_DEBUG_LEVEL_3
    sprintf(debug_buff, "qla1280_chip_diag: testing device at 0x%p \n\r",&reg->id_l);
    qla1280_print(debug_buff);
#endif

    /* Soft reset chip and wait for it to finish. */
    WRT_REG_WORD(&reg->ictrl, ISP_RESET); 
    data = qla1280_debounce_register(&reg->ictrl);
    for (cnt = 6000000; cnt && data & ISP_RESET; cnt--)
    {
        SYS_DELAY(5);
        data = RD_REG_WORD(&reg->ictrl);
    }
    if (cnt)
    {
        /* Reset register not cleared by chip reset. */
#if defined(QL_DEBUG_LEVEL_3)
        qla1280_print("qla1280_chip_diag: reset register cleared by chip reset\n\r");
#endif
        WRT_REG_WORD(&reg->cfg_1, 0);

        /* Reset RISC and disable BIOS which
        allows RISC to execute out of RAM. */
        WRT_REG_WORD(&reg->host_cmd, HC_RESET_RISC);
        WRT_REG_WORD(&reg->host_cmd, HC_RELEASE_RISC);
        WRT_REG_WORD(&reg->host_cmd, HC_DISABLE_BIOS);
        data = qla1280_debounce_register(&reg->mailbox0);
        for (cnt = 6000000; cnt && data == MBS_BUSY; cnt--)
        {
            SYS_DELAY(5);
            data = RD_REG_WORD(&reg->mailbox0);
        }

        if (cnt)
        {
            /* Check product ID of chip */
#if defined(QL_DEBUG_LEVEL_3)
            qla1280_print("qla1280_chip_diag: Checking product ID of chip\n\r");
#endif
            if (RD_REG_WORD(&reg->mailbox1) != PROD_ID_1 ||
                (RD_REG_WORD(&reg->mailbox2) != PROD_ID_2 &&
                RD_REG_WORD(&reg->mailbox2) != PROD_ID_2a) ||
                RD_REG_WORD(&reg->mailbox3) != PROD_ID_3 ||
                RD_REG_WORD(&reg->mailbox4) != PROD_ID_4)
            {
                printk(KERN_INFO "qla1280: Wrong product ID = 0x%x,0x%x,0x%x,0x%x\n",
                        RD_REG_WORD(&reg->mailbox1),
                        RD_REG_WORD(&reg->mailbox2),
                        RD_REG_WORD(&reg->mailbox3),
                        RD_REG_WORD(&reg->mailbox4) );
                status = 1;
            }
            else
            {
                DEBUG(printk("qla1280_chip_diag: Checking mailboxes of chip\n"));
                /* Wrap Incoming Mailboxes Test. */
                mb[0] = MBC_MAILBOX_REGISTER_TEST;
                mb[1] = 0xAAAA;
                mb[2] = 0x5555;
                mb[3] = 0xAA55;
                mb[4] = 0x55AA;
                mb[5] = 0xA5A5;
                mb[6] = 0x5A5A;
                mb[7] = 0x2525;
                if (!(status = qla1280_mailbox_command(ha,
                    (BIT_7|BIT_6|BIT_5|BIT_4|BIT_3|BIT_2|BIT_1|BIT_0),
                    &mb[0])))
                {
                    if (mb[1] != 0xAAAA || mb[2] != 0x5555 ||
                        mb[3] != 0xAA55 || mb[4] != 0x55AA)
                        status = 1;
                    if (mb[5] != 0xA5A5 || mb[6] != 0x5A5A ||
                        mb[7] != 0x2525)
                        status = 1;
                    if( status == 1 )
                        printk(KERN_INFO "qla1280: Failed mailbox check\n");
                }
            }
        }
        else
            status = 1;
    }
    else
        status = 1;

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_chip_diag: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_chip_diag: exiting normally\n\r");
#endif
    return(status);
}

/*
 * Setup chip
 *      Load and start RISC firmware.
 *
 * Input:
 *      ha = adapter block pointer.
 *
 * Returns:
 *      0 = success.
 */
STATIC uint8_t
qla1280_setup_chip(scsi_qla_host_t *ha)
{
    uint8_t      status = 0;
    uint16_t     risc_address;
    uint16_t     *risc_code_address;
    long         risc_code_size;
    uint16_t     mb[MAILBOX_REGISTER_COUNT];
#ifdef QLA1280_UNUSED
    uint8_t	*sp;
    int i;
#endif
    uint16_t     cnt;
    int          num;
    uint8_t    *tbuf;
    u_long     p_tbuf;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_setup_chip");
#endif

	if( (tbuf = (uint8_t *)KMALLOC(8000) ) == NULL )
        {
            printk("setup_chip: couldn't alloacte memory\n");
            return(1);
        }
        p_tbuf =  VIRT_TO_BUS(tbuf);
    /* Load RISC code. */
    /* 
    risc_address      = ql12_risc_code_addr01;
    risc_code_address = &ql12_risc_code01[0];
    risc_code_size    = ql12_risc_code_length01;
    */
    risc_address = *QLBoardTbl[ha->devnum].fwstart;  
    risc_code_address = QLBoardTbl[ha->devnum].fwcode;  
    risc_code_size    = (long)(*QLBoardTbl[ha->devnum].fwlen & 0xffff);  

    DEBUG(printk("qla1280: DMAing RISC code (%d) words.\n",(int)risc_code_size));
    DEBUG(sprintf(debug_buff,"qla1280_setup_chip:  Loading RISC code size =(%ld).\n\r",risc_code_size);)
    DEBUG(qla1280_print(debug_buff));
    num =0;
    while (risc_code_size > 0 && !status)
    {
        cnt = 2000 >> 1;

        if ( cnt > risc_code_size ) 
            cnt = risc_code_size;

        DEBUG(sprintf(debug_buff,"qla1280_setup_chip:  loading risc @ =(0x%p),%d,%d(0x%x).\n\r",risc_code_address,cnt,num,risc_address);)
        DEBUG(qla1280_print(debug_buff));
        DEBUG(printk("qla1280_setup_chip:  loading risc @ =code=(0x%p),cnt=%d,seg=%d,addr=0x%x\n\r",risc_code_address,cnt,num,risc_address));
        BCOPY((caddr_t) risc_code_address,(caddr_t) ha->request_ring, (cnt <<1));
        mb[0] = MBC_LOAD_RAM; 
        /* mb[0] = MBC_LOAD_RAM_A64; */
        mb[1] = risc_address;
        mb[4] = cnt;
        mb[3] = (uint16_t)  ha->request_dma & 0xffff;
        mb[2] = (uint16_t) (ha->request_dma >> 16) & 0xffff;
        mb[7] = (uint16_t) (MS_64BITS(ha->request_dma) & 0xffff);
        mb[6] = (uint16_t) (MS_64BITS(ha->request_dma) >> 16) & 0xffff;
        DEBUG(printk("qla1280_setup_chip: op=%d  0x%lx = 0x%4x,0x%4x,0x%4x,0x%4x\n",mb[0],ha->request_dma,mb[6],mb[7],mb[2],mb[3]));
        if( (status = qla1280_mailbox_command(ha, BIT_4|BIT_3|BIT_2|BIT_1|BIT_0,
            &mb[0]))  )
        {
            printk("Failed to load partial segment of f/w\n");
            break;
        }
        /* dump it back */

#if 0
        mb[0] = MBC_DUMP_RAM_A64;
        mb[1] = risc_address;
        mb[4] = cnt;
        mb[3] = (uint16_t)  p_tbuf & 0xffff;
        mb[2] = (uint16_t) (p_tbuf >> 16) & 0xffff;
        mb[7] = (uint16_t) (p_tbuf >> 32) & 0xffff;
        mb[6] = (uint16_t) (p_tbuf >> 48) & 0xffff;

        if( (status = qla1280_mailbox_command(ha, BIT_4|BIT_3|BIT_2|BIT_1|BIT_0,
            &mb[0]))  )
        {
            printk("Failed to dump partial segment of f/w\n");
            break;
        }
        sp =  (uint8_t *)ha->request_ring;
        for (i = 0; i < (cnt<< 1) ; i++)
        {
            if( tbuf[i] != sp[i] )
            {
               printk("qla1280 : firmware compare error @ byte (0x%x)\n",i);
                break;
            }
        }

#endif
        risc_address += cnt;
        risc_code_size = risc_code_size - cnt;
        risc_code_address = risc_code_address + cnt;
        num++;
    }
#ifdef QLA1280_UNUSED
    DEBUG(ql_debug_print = 0;)
    {
        for (i = 0; i < ql12_risc_code_length01; i++)
        {
            mb[0] = 0x4;
            mb[1] = ql12_risc_code_addr01 + i;
            mb[2] = ql12_risc_code01[i];

            status = qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0,
                    &mb[0]);
            if (status)
            {
                printk("qla1280 : firmware load failure\n");
                break;
            }

            mb[0] = 0x5;
            mb[1] = ql12_risc_code_addr01 + i;
            mb[2] = 0;

            status = qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0,
                    &mb[0]);
            if (status)
            {
                printk("qla1280 : firmware dump failure\n");
                break;
            }
            if( mb[2] != ql12_risc_code01[i] )
                printk("qla1280 : firmware compare error @ (0x%x)\n",ql12_risc_code_addr01+i);
        }
    }
    DEBUG(ql_debug_print = 1;)
#endif

    /* Verify checksum of loaded RISC code. */
    if (!status)
    {
        DEBUG(printk("qla1280_setup_chip: Verifying checksum of loaded RISC code.\n");)
        mb[0] = MBC_VERIFY_CHECKSUM;
        /* mb[1] = ql12_risc_code_addr01; */
        mb[1] = *QLBoardTbl[ha->devnum].fwstart;  
        
        if (!(status = qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0])))
        {
            /* Start firmware execution. */
            DEBUG(qla1280_print("qla1280_setup_chip: start firmware running.\n\r");)
            mb[0] = MBC_EXECUTE_FIRMWARE;
            /* mb[1] = ql12_risc_code_addr01; */
            mb[1] = *QLBoardTbl[ha->devnum].fwstart;  
            qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0]);
        }
        else
            printk("qla1280_setup_chip: Failed checksum.\n");
    }

	KMFREE(tbuf,8000);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_setup_chip: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_setup_chip");
#endif
    return(status);
}

/*
 * Initialize rings
 *
 * Input:
 *      ha                = adapter block pointer.
 *      ha->request_ring  = request ring virtual address
 *      ha->response_ring = response ring virtual address
 *      ha->request_dma   = request ring physical address
 *      ha->response_dma  = response ring physical address
 *
 * Returns:
 *      0 = success.
 */
STATIC uint8_t
qla1280_init_rings(scsi_qla_host_t *ha)
{
    uint8_t     status = 0;
    uint16_t    cnt;
    uint16_t    mb[MAILBOX_REGISTER_COUNT];

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_init_rings");
#endif
    /* Clear outstanding commands array. */
    for (cnt = 0; cnt < MAX_OUTSTANDING_COMMANDS; cnt++)
        ha->outstanding_cmds[cnt] = 0;

    /* Initialize request queue. */
    ha->request_ring_ptr = ha->request_ring;
    ha->req_ring_index   = 0;
    ha->req_q_cnt        = REQUEST_ENTRY_CNT;
    /* mb[0] = MBC_INIT_REQUEST_QUEUE; */
    mb[0] = MBC_INIT_REQUEST_QUEUE_A64;
    mb[1] = REQUEST_ENTRY_CNT;
    mb[3] = (uint16_t)LS_64BITS(ha->request_dma);
    mb[2] = (uint16_t)( LS_64BITS(ha->request_dma) >> 16);
    mb[4] = 0;
    mb[7] = (uint16_t)MS_64BITS(ha->request_dma);
    mb[6] = (uint16_t)( MS_64BITS(ha->request_dma) >> 16);
    if (!(status = qla1280_mailbox_command(ha,
        BIT_7|BIT_6|BIT_4|BIT_3|BIT_2|BIT_1|BIT_0,
        &mb[0])))
    {
        /* Initialize response queue. */
        ha->response_ring_ptr = ha->response_ring;
        ha->rsp_ring_index    = 0;
        /* mb[0] = MBC_INIT_RESPONSE_QUEUE; */
        mb[0] = MBC_INIT_RESPONSE_QUEUE_A64;
        mb[1] = RESPONSE_ENTRY_CNT;
        mb[3] = (uint16_t)LS_64BITS(ha->response_dma);
        mb[2] = (uint16_t)(LS_64BITS(ha->response_dma) >> 16);
        mb[5] = 0;
        mb[7] = (uint16_t)MS_64BITS(ha->response_dma);
        mb[6] = (uint16_t)(MS_64BITS(ha->response_dma) >> 16);
        status = qla1280_mailbox_command(ha,
                BIT_7|BIT_6|BIT_5|BIT_3|BIT_2|BIT_1|BIT_0,
                &mb[0]);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_init_rings: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_init_rings");
#endif
    return(status);
}

/*
 * NVRAM configuration.
 *
 * Input:
 *      ha                = adapter block pointer.
 *      ha->request_ring  = request ring virtual address
 *
 * Output:
 *      host adapters parameters in host adapter block
 *
 * Returns:
 *      0 = success.
 */
STATIC uint8_t
qla1280_nvram_config(scsi_qla_host_t *ha)
{
    device_reg_t *reg   = ha->iobase;
    nvram_t      *nv    = (nvram_t *)ha->response_ring; 
    uint8_t      status = 0;
    uint32_t     b, t, l;
    uint16_t     *wptr;
    uint16_t     mb[MAILBOX_REGISTER_COUNT];
    uint8_t      cnt;
    uint8_t      chksum;
    uint32_t     nvsize;

#if defined(QL_DEBUG_ROUTINES) && !defined(QL_DEBUG_LEVEL_4)
    uint8_t      saved_print_status = ql_debug_print;
#endif
    ENTER("qla1280_nvram_config");
#if defined(QL_DEBUG_ROUTINES) && !defined(QL_DEBUG_LEVEL_4)
    ql_debug_print = FALSE;
#endif

    /* Verify valid NVRAM checksum. */
#if  USE_NVRAM_DEFAULTS
    chksum = 1;
#else
    wptr = (uint16_t *)ha->response_ring;
    chksum = 0;
    if( ha->device_id == QLA12160_DEVICE_ID ||  
        ha->device_id == QLA10160_DEVICE_ID )  
    nvsize = sizeof(nvram160_t)/2;
    else
    nvsize = sizeof(nvram_t)/2;
    for( cnt = 0; cnt < nvsize; cnt++ ) 
    {
        *wptr = qla1280_get_nvram_word(ha, cnt); 
        chksum += (uint8_t)*wptr;
        chksum += (uint8_t)(*wptr >> 8);
        wptr++;
    }
#endif


    /* Bad NVRAM data, set defaults parameters. */
    if (chksum || nv->id[0] != 'I' || nv->id[1] != 'S' ||
        nv->id[2] != 'P' || nv->id[3] != ' ' || nv->version < 1)
    {
#if  USE_NVRAM_DEFAULTS
        DEBUG(printk("Using defaults for NVRAM\n"));
#else
        DEBUG(printk("Using defaults for NVRAM: \n"));
        DEBUG(printk("checksum=0x%x, Id=%c, version=0x%x\n",chksum,nv->id[0],nv->version));
#if defined(QL_DEBUG_LEVEL_3)
        /* ql_debug_print = 1;
        qla1280_dump_buffer((caddr_t)ha->response_ring, REQUEST_ENTRY_SIZE);
        ql_debug_print = 0; */
#endif
                wptr = (uint16_t *)ha->response_ring;
        for (cnt = 0; cnt < sizeof(nvram_t)/2; cnt++)
            *wptr++ = 0;
#endif


        /* nv->cntr_flags_1.disable_loading_risc_code = 1; */
        nv->firmware_feature.w = BIT_0;
        nv->termination.f.scsi_bus_0_control = 3;
        nv->termination.f.scsi_bus_1_control = 3;
        nv->termination.f.auto_term_support = 1;

        for (b = 0; b < MAX_BUSES; b++)
        {
            nv->bus[b].config_1.initiator_id = 7;
            nv->bus[b].bus_reset_delay = 5;
            nv->bus[b].config_2.async_data_setup_time = 9;
            nv->bus[b].config_2.req_ack_active_negation = 1;
            nv->bus[b].config_2.data_line_active_negation = 1;
            nv->bus[b].selection_timeout = 250;
            nv->bus[b].max_queue_depth = 256;

            for (t = 0; t < MAX_TARGETS; t++)
            {
                nv->bus[b].target[t].parameter.f.auto_request_sense = 1;
                nv->bus[b].target[t].parameter.f.disconnect_allowed = 1;
                nv->bus[b].target[t].parameter.f.tag_queuing = 1;
                nv->bus[b].target[t].flags.device_enable = 1;
            }
        }

#if  USE_NVRAM_DEFAULTS
        status = 0;
#else
        status = 1;
#endif
    }
    else
    {
        /* Always force AUTO sense for LINUX SCSI */
        for (b = 0; b < MAX_BUSES; b++)
            for (t = 0; t < MAX_TARGETS; t++)
            {
                nv->bus[b].target[t].parameter.f.auto_request_sense = 1;
            }
    }
#if  DEBUG_PRINT_NVRAM
    ql_debug_print = 1;
    sprintf(debug_buff,"qla1280 : initiator scsi id bus[0]=%d\n\r",
            nv->bus[0].config_1.initiator_id);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : initiator scsi id bus[1]=%d\n\r",
            nv->bus[1].config_1.initiator_id);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : bus reset delay[0]=%d\n\r",
            nv->bus[0].bus_reset_delay);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : bus reset delay[1]=%d\n\r",
            nv->bus[1].bus_reset_delay);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : retry count[0]=%d\n\r",
            nv->bus[0].retry_count);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : retry delay[0]=%d\n\r",
            nv->bus[0].retry_delay);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : retry count[1]=%d\n\r",
            nv->bus[1].retry_count);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : retry delay[1]=%d\n\r",
            nv->bus[1].retry_delay);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : async data setup time[0]=%d\n\r",
            nv->bus[0].config_2.async_data_setup_time);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : async data setup time[1]=%d\n\r",
            nv->bus[1].config_2.async_data_setup_time);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : req/ack active negation[0]=%d\n\r",
            nv->bus[0].config_2.req_ack_active_negation);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : req/ack active negation[1]=%d\n\r",
            nv->bus[1].config_2.req_ack_active_negation);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : data line active negation[0]=%d\n\r",
            nv->bus[0].config_2.data_line_active_negation);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : data line active negation[1]=%d\n\r",
            nv->bus[1].config_2.data_line_active_negation);
    qla1280_print(debug_buff);


    sprintf(debug_buff,"qla1280 : disable loading risc code=%d\n\r",
            nv->cntr_flags_1.disable_loading_risc_code);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : enable 64bit addressing=%d\n\r",
            nv->cntr_flags_1.enable_64bit_addressing);
    qla1280_print(debug_buff);

    sprintf(debug_buff,"qla1280 : selection timeout limit[0]=%d\n\r",
            nv->bus[0].selection_timeout);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : selection timeout limit[1]=%d\n\r",
            nv->bus[1].selection_timeout);

    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : max queue depth[0]=%d\n\r",
            nv->bus[0].max_queue_depth);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"qla1280 : max queue depth[1]=%d\n\r",
            nv->bus[1].max_queue_depth);
    qla1280_print(debug_buff);
#endif

    DEBUG(ql_debug_print = 0;)

    /* Disable RISC load of firmware. */
    ha->flags.disable_risc_code_load =
            nv->cntr_flags_1.disable_loading_risc_code;
    /* Enable 64bit addressing. */
    ha->flags.enable_64bit_addressing =
            nv->cntr_flags_1.enable_64bit_addressing;

    /* Set ISP hardware DMA burst */
    mb[0] = nv->isp_config.c;
    WRT_REG_WORD(&reg->cfg_1, mb[0]);

    /* Set SCSI termination. */
    WRT_REG_WORD(&reg->gpio_enable, (BIT_3 + BIT_2 + BIT_1 + BIT_0));
    mb[0] = nv->termination.c & (BIT_3 + BIT_2 + BIT_1 + BIT_0);
    WRT_REG_WORD(&reg->gpio_data, mb[0]);

    /* ISP parameter word. */
    mb[0] = MBC_SET_SYSTEM_PARAMETER;
    mb[1] = nv->isp_parameter;
    status |= qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0]);

    /* Firmware feature word. */
    mb[0] = MBC_SET_FIRMWARE_FEATURES;
    mb[1] = nv->firmware_feature.w & (BIT_1|BIT_0);
    status |= qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0]);

    /* Retry count and delay. */
    mb[0] = MBC_SET_RETRY_COUNT;
    mb[1] = nv->bus[0].retry_count;
    mb[2] = nv->bus[0].retry_delay;
    mb[6] = nv->bus[1].retry_count;
    mb[7] = nv->bus[1].retry_delay;
    status |= qla1280_mailbox_command(ha, BIT_7|BIT_6|BIT_2|BIT_1|BIT_0, &mb[0]);

    /* ASYNC data setup time. */
    mb[0] = MBC_SET_ASYNC_DATA_SETUP;
    mb[1] = nv->bus[0].config_2.async_data_setup_time;
    mb[2] = nv->bus[1].config_2.async_data_setup_time;
    status |= qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0, &mb[0]);

    /* Active negation states. */
    mb[0] = MBC_SET_ACTIVE_NEGATION;
    mb[1] = 0;
    if (nv->bus[0].config_2.req_ack_active_negation)
        mb[1] |= BIT_5;
    if (nv->bus[0].config_2.data_line_active_negation)
        mb[1] |= BIT_4;
    mb[2] = 0;
    if (nv->bus[1].config_2.req_ack_active_negation)
        mb[2] |= BIT_5;
    if (nv->bus[1].config_2.data_line_active_negation)
        mb[2] |= BIT_4;
    status |= qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0, &mb[0]);

    /* Selection timeout. */
    mb[0] = MBC_SET_SELECTION_TIMEOUT;
    mb[1] = nv->bus[0].selection_timeout;
    mb[2] = nv->bus[1].selection_timeout;
    status |= qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0, &mb[0]);

    for (b = 0; b < ha->ports; b++)
    {
        /* SCSI Reset Disable. */
        ha->bus_settings[b].disable_scsi_reset = nv->bus[b].config_1.scsi_reset_disable;

        /* Initiator ID. */
        ha->bus_settings[b].id = nv->bus[b].config_1.initiator_id;
        mb[0] = MBC_SET_INITIATOR_ID;
        mb[1] = b ? ha->bus_settings[b].id | BIT_7 : ha->bus_settings[b].id;
        status |= qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0]);

        /* Reset Delay. */
        ha->bus_settings[b].bus_reset_delay = nv->bus[b].bus_reset_delay;

        /* Command queue depth per device. */
        ha->bus_settings[b].hiwat = nv->bus[b].max_queue_depth - 1;

        /* Set target parameters. */
        for (t = 0; t < MAX_TARGETS; t++)
        {
            if( ha->device_id == QLA12160_DEVICE_ID ||  
                ha->device_id == QLA10160_DEVICE_ID )  
            {
                status = qla12160_set_target_parameters(ha,b,t,0,(nvram160_t *)nv);
            }
			else
            {
                /* Set Target Parameters. */
                mb[0] = MBC_SET_TARGET_PARAMETERS;
                mb[1] = (uint16_t)(b ? t | BIT_7 :t);
                mb[1] <<= 8;
                mb[2] = nv->bus[b].target[t].parameter.c << 8;
                mb[2] |= TP_AUTO_REQUEST_SENSE;
                mb[2] &= ~TP_STOP_QUEUE;
                mb[3] = nv->bus[b].target[t].flags.sync_offset << 8;
                mb[3] |= nv->bus[b].target[t].sync_period;
                status |= qla1280_mailbox_command(ha, BIT_3|BIT_2|BIT_1|BIT_0,
                    &mb[0]);
            }

            /* Save Tag queuing enable flag. */
            mb[0] = BIT_0 << t;
            if (nv->bus[b].target[t].parameter.f.tag_queuing)
                ha->bus_settings[b].qtag_enables |= mb[0];

            /* Save Device enable flag. */
            if (nv->bus[b].target[t].flags.device_enable)
                ha->bus_settings[b].device_enables |= mb[0];

            /* Save LUN disable flag. */
            if (nv->bus[b].target[t].flags.lun_disable)
                ha->bus_settings[b].lun_disables |= mb[0];

            /* Set Device Queue Parameters. */
            for (l = 0; l < MAX_LUNS; l++)
            {
                mb[0] = MBC_SET_DEVICE_QUEUE;
                mb[1] = (uint16_t)(b ? t | BIT_7 :t);
                mb[1] = mb[1] << 8 | l;
                mb[2] = nv->bus[b].max_queue_depth;
                mb[3] = nv->bus[b].target[t].execution_throttle;
                status |= qla1280_mailbox_command(ha, BIT_3|BIT_2|BIT_1|BIT_0,
                        &mb[0]);
            }
        }
    }
    DEBUG(ql_debug_print = 0;)

#if defined(QL_DEBUG_ROUTINES) && !defined(QL_DEBUG_LEVEL_4)
    ql_debug_print = saved_print_status;
#endif

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    DEBUG(if (status))
        DEBUG(qla1280_print("qla1280_nvram_config: **** FAILED ****\n\r");)
#endif
    LEAVE("qla1280_nvram_config");
    return(status);
}

/*
 * Get NVRAM data word
 *      Calculates word position in NVRAM and calls request routine to
 *      get the word from NVRAM.
 *
 * Input:
 *      ha      = adapter block pointer.
 *      address = NVRAM word address.
 *
 * Returns:
 *      data word.
 */
STATIC uint16_t
qla1280_get_nvram_word(scsi_qla_host_t *ha, uint32_t address)
{
    uint32_t nv_cmd;
    uint16_t data;

#ifdef QL_DEBUG_ROUTINES
    uint8_t  saved_print_status = ql_debug_print;
#endif
#ifdef QL_DEBUG_LEVEL_4
    ENTER("qla1280_get_nvram_word");
#endif

    nv_cmd = address << 16;
    nv_cmd |= NV_READ_OP;

#ifdef QL_DEBUG_ROUTINES
    ql_debug_print = FALSE;
#endif
    data = qla1280_nvram_request(ha, nv_cmd);
#ifdef QL_DEBUG_ROUTINES
    ql_debug_print = saved_print_status;
#endif

#ifdef QL_DEBUG_LEVEL_4
    qla1280_print("qla1280_get_nvram_word: exiting normally NVRAM data = ");
    qla1280_output_number((uint32_t)data, 16);
    qla1280_print("\n\r");
#endif
    return(data);
}

/*
 * NVRAM request
 *      Sends read command to NVRAM and gets data from NVRAM.
 *
 * Input:
 *      ha     = adapter block pointer.
 *      nv_cmd = Bit 26     = start bit
 *               Bit 25, 24 = opcode
 *               Bit 23-16  = address
 *               Bit 15-0   = write data
 *
 * Returns:
 *      data word.
 */
STATIC uint16_t
qla1280_nvram_request(scsi_qla_host_t *ha, uint32_t nv_cmd)
{
    uint8_t      cnt;
    device_reg_t *reg = ha->iobase;
    uint16_t     data = 0;
    uint16_t     reg_data;

    /* Send command to NVRAM. */

    nv_cmd <<= 5;
    for (cnt = 0; cnt < 11; cnt++)
    {
        if (nv_cmd & BIT_31)
            qla1280_nv_write(ha, NV_DATA_OUT);
        else
            qla1280_nv_write(ha, 0);
        nv_cmd <<= 1;
    }

    /* Read data from NVRAM. */

    for (cnt = 0; cnt < 16; cnt++)
    {
        WRT_REG_WORD(&reg->nvram, NV_SELECT+NV_CLOCK);
        /* qla1280_nv_delay(ha); */
        NVRAM_DELAY();
        data <<= 1;
        reg_data = RD_REG_WORD(&reg->nvram);
        if (reg_data & NV_DATA_IN)
            data |= BIT_0;
        WRT_REG_WORD(&reg->nvram, NV_SELECT);
        /* qla1280_nv_delay(ha); */
        NVRAM_DELAY();
    }

    /* Deselect chip. */

    WRT_REG_WORD(&reg->nvram, NV_DESELECT);
    /* qla1280_nv_delay(ha); */
    NVRAM_DELAY();

    return(data);
}

STATIC void
qla1280_nv_write(scsi_qla_host_t *ha, uint16_t data)
{
    device_reg_t *reg = ha->iobase;

    WRT_REG_WORD(&reg->nvram, data | NV_SELECT);
    NVRAM_DELAY();
    /* qla1280_nv_delay(ha); */
    WRT_REG_WORD(&reg->nvram, data | NV_SELECT | NV_CLOCK);
    /* qla1280_nv_delay(ha); */
    NVRAM_DELAY();
    WRT_REG_WORD(&reg->nvram, data | NV_SELECT);
    /* qla1280_nv_delay(ha); */
    NVRAM_DELAY();
}

STATIC void
qla1280_nv_delay(scsi_qla_host_t *ha)
{
    device_reg_t *reg = ha->iobase;
    int          cnt  = NV_DELAY_COUNT;
    uint16_t     data = 0;

    while (cnt--)
        data |= RD_REG_WORD(&reg->nvram);
}

/*
 * Mailbox Command
 *      Issue mailbox command and waits for completion.
 *
 * Input:
 *      ha = adapter block pointer.
 *      mr = mailbox registers to load.
 *      mb = data pointer for mailbox registers.
 *
 * Output:
 *      mb[MAILBOX_REGISTER_COUNT] = returned mailbox data.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_mailbox_command(scsi_qla_host_t *ha, uint8_t mr, uint16_t *mb)
{
    device_reg_t *reg   = ha->iobase;
    uint8_t      status = 0;
    uint32_t     cnt;
    uint16_t     *optr, *iptr;
    uint16_t     data;
    srb_t        *done_q_first = 0;
    srb_t        *done_q_last = 0;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_mailbox_command");
#endif

    /* Acquire interrupt specific lock */
    QLA1280_INTR_LOCK(ha);
    DRIVER_LOCK
            ha->flags.mbox_busy = TRUE;

    /* Load mailbox registers. */
    optr = (uint16_t *)&reg->mailbox0;
    iptr = mb;
    for (cnt = 0; cnt < MAILBOX_REGISTER_COUNT; cnt++)
    {
        if (mr & BIT_0)
        {
            WRT_REG_WORD(optr, (*iptr));
        }

        mr >>= 1;
        optr++;
        iptr++;
    }
    /* Issue set host interrupt command. */
    ha->flags.mbox_int = FALSE;
    WRT_REG_WORD(&reg->host_cmd, HC_SET_HOST_INT);
    data = qla1280_debounce_register(&reg->istatus);

    /* Wait for 30 seconds for command to finish. */
    for (cnt = 30000000; cnt > 0 && !ha->flags.mbox_int; cnt--)
    {
        /* Check for pending interrupts. */
        if (data & RISC_INT)
        {
            qla1280_isr(ha, (srb_t **)&done_q_first, (srb_t **)&done_q_last);
        }
        SYS_DELAY(1);
        data = RD_REG_WORD(&reg->istatus);
    }

    /* Check for mailbox command timeout. */
    if ( !cnt )
    {
#ifdef QL_DEBUG_LEVEL_2
        qla1280_print(
                "qla1280_mailbox_command: **** Command Timeout, mailbox0 = ");
        qla1280_output_number((uint32_t)mb[0], 16);
        qla1280_print(" ****\n\r");
#endif
        ha->flags.isp_abort_needed = TRUE; 
        status = 1;
    }
    else if (ha->mailbox_out[0] != MBS_CMD_CMP)
        status = 1;

    /* Load return mailbox registers. */
    optr = mb;
    iptr = (uint16_t *)&ha->mailbox_out[0];
    mr = MAILBOX_REGISTER_COUNT;
    while (mr--)
        *optr++ = *iptr++;

    /* Go check for any response interrupts pending. */
    ha->flags.mbox_busy = FALSE;
    qla1280_isr(ha, (srb_t **)&done_q_first, (srb_t **)&done_q_last);

    /* Release interrupt specific lock */
    QLA1280_INTR_UNLOCK(ha);
    DRIVER_UNLOCK

            if (ha->flags.isp_abort_needed)
        qla1280_abort_isp(ha);

    if (ha->flags.reset_marker)
        qla1280_rst_aen(ha);

    if (done_q_first)
        qla1280_done(ha, (srb_t **)&done_q_first, (srb_t **)&done_q_last);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
    {
        qla1280_print("qla1280_mailbox_command: **** FAILED, mailbox0 = ");
        qla1280_output_number((uint32_t)mb[0], 16);
        qla1280_print(" ****\n\r");
    }
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_mailbox_command");
#endif
    return(status);
}

/*
 * qla1280_poll
 *      Polls ISP for interrupts.
 *
 * Input:
 *      ha = adapter block pointer.
 */
STATIC void
qla1280_poll(scsi_qla_host_t *ha)
{
    device_reg_t    *reg   = ha->iobase;
    uint16_t        data;
    srb_t           *done_q_first = 0;
    srb_t           *done_q_last = 0;

#ifdef QL_DEBUG_LEVEL_3
    /* ENTER("qla1280_poll"); */
#endif

    /* Acquire interrupt specific lock */
    QLA1280_INTR_LOCK(ha);

    /* Check for pending interrupts. */
    data = RD_REG_WORD(&reg->istatus);
    if (data & RISC_INT)
        qla1280_isr(ha, (srb_t **)&done_q_first, (srb_t **)&done_q_last);

    /* Release interrupt specific lock */
    QLA1280_INTR_UNLOCK(ha);

    if (!ha->flags.mbox_busy)
    {
        if (ha->flags.isp_abort_needed)
            qla1280_abort_isp(ha);
        if (ha->flags.reset_marker)
            qla1280_rst_aen(ha);
    }

    if (done_q_first)
        qla1280_done(ha, (srb_t **)&done_q_first, (srb_t **)&done_q_last);

#ifdef QL_DEBUG_LEVEL_3
    /* LEAVE("qla1280_poll"); */
#endif
}

/*
 * qla1280_bus_reset
 *      Issue SCSI bus reset.
 *
 * Input:
 *      ha = adapter block pointer.
 *      b  = SCSI bus number.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_bus_reset(scsi_qla_host_t *ha, uint8_t b)
{
    uint8_t     status;
    uint16_t    mb[MAILBOX_REGISTER_COUNT];

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_bus_reset: entered\n\r");
#endif
   if( qla1280_verbose )
   {
    printk("scsi(%d): Resetting SCSI BUS (%d)\n",(int)ha->host_no,b);
   }

    mb[0] = MBC_BUS_RESET;
    mb[1] = ha->bus_settings[b].bus_reset_delay;
    mb[2] = (uint16_t)b;
    status = qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0, &mb[0]);

    if (status)
    {
        if (ha->bus_settings[b].failed_reset_count > 2)                  /* dg - 03/13/99 */
            ha->bus_settings[b].scsi_bus_dead = TRUE;
        ha->bus_settings[b].failed_reset_count++;
    }
	else
    {
       QLA1280_DELAY(4);
       ha->bus_settings[b].scsi_bus_dead = FALSE;                         /* dg - 03/13/99 */
       ha->bus_settings[b].failed_reset_count = 0;
       /* Issue marker command. */
       qla1280_marker(ha, b, 0, 0, MK_SYNC_ALL);
    }
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_bus_reset: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_bus_reset: exiting normally\n\r");
#endif
    return(status);
}

/*
 * qla1280_device_reset
 *      Issue bus device reset message to the target.
 *
 * Input:
 *      ha = adapter block pointer.
 *      b  = SCSI BUS number.
 *      t  = SCSI ID.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_device_reset(scsi_qla_host_t *ha, uint8_t b, uint32_t t)
{
    uint8_t     status;
    uint16_t    mb[MAILBOX_REGISTER_COUNT];

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_device_reset");
#endif

    mb[0] = MBC_ABORT_TARGET;
    mb[1] = (b ? (t | BIT_7) : t) << 8;
    mb[2] = 1;
    status = qla1280_mailbox_command(ha, BIT_2|BIT_1|BIT_0, &mb[0]);

    /* Issue marker command. */
    qla1280_marker(ha, b, t, 0, MK_SYNC_ID);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_device_reset: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_device_reset");
#endif
    return(status);
}

/*
 * qla1280_abort_device
 *      Issue an abort message to the device
 *
 * Input:
 *      ha = adapter block pointer.
 *      b  = SCSI BUS.
 *      t  = SCSI ID.
 *      l  = SCSI LUN.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_abort_device(scsi_qla_host_t *ha, uint8_t b, uint32_t t, uint32_t l)
{
    uint8_t     status;
    uint16_t    mb[MAILBOX_REGISTER_COUNT];

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_abort_device");
#endif

    mb[0] = MBC_ABORT_DEVICE;
    mb[1] = (b ? t | BIT_7 : t) << 8 | l;
    status = qla1280_mailbox_command(ha, BIT_1|BIT_0, &mb[0]);

    /* Issue marker command. */
    qla1280_marker(ha, b, t, l, MK_SYNC_ID_LUN);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_abort_device: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_abort_device");
#endif
    return(status);
}

/*
 * qla1280_abort_command
 *      Abort command aborts a specified IOCB.
 *
 * Input:
 *      ha = adapter block pointer.
 *      sp = SB structure pointer.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_abort_command(scsi_qla_host_t *ha, srb_t *sp)
{
    uint8_t         status;
    uint16_t        mb[MAILBOX_REGISTER_COUNT];
    uint32_t        b, t, l;
    uint32_t        handle;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_abort_command");
#endif

    /* Locate handle number. */
    for (handle = 0; handle < MAX_OUTSTANDING_COMMANDS; handle++)
        if (ha->outstanding_cmds[handle] == sp)
            break;

                b  = SCSI_BUS_32(sp->cmd);
        t  = SCSI_TCN_32(sp->cmd);
        l  = SCSI_LUN_32(sp->cmd);

        mb[0] = MBC_ABORT_COMMAND;
        mb[1] = (b ? t | BIT_7 : t) << 8 | l;
        mb[2] = handle >> 16;
        mb[3] = (uint16_t)handle;
        status = qla1280_mailbox_command(ha, BIT_3|BIT_2|BIT_1|BIT_0, &mb[0]);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
        if (status)
            qla1280_print("qla1280_abort_command: **** FAILED ****\n\r");
#endif
        sp->flags |= SRB_ABORT_PENDING;

        LEAVE("qla1280_abort_command");
        return(status);
}

/*
 * qla1280_reset_adapter
 *      Reset adapter.
 *
 * Input:
 *      ha = adapter block pointer.
 */
STATIC void
qla1280_reset_adapter(scsi_qla_host_t *ha)
{
    device_reg_t *reg = ha->iobase;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_reset_adapter");
#endif

    /* Disable ISP chip */
    ha->flags.online = FALSE;
    WRT_REG_WORD(&reg->ictrl, ISP_RESET);
    WRT_REG_WORD(&reg->host_cmd, HC_RESET_RISC);
    WRT_REG_WORD(&reg->host_cmd, HC_RELEASE_RISC);
    WRT_REG_WORD(&reg->host_cmd, HC_DISABLE_BIOS);

#ifdef QL_DEBUG_LEVEL_3
    LEAVE("qla1280_reset_adapter");
#endif
}

/*
 *  Issue marker command.
 *      Function issues marker IOCB.
 *
 * Input:
 *      ha   = adapter block pointer.
 *      b    = SCSI BUS number
 *      t    = SCSI ID
 *      l    = SCSI LUN
 *      type = marker modifier
 */
STATIC void
qla1280_marker(scsi_qla_host_t *ha, uint8_t b, uint32_t t, uint32_t l, uint8_t type)
{
    mrk_entry_t     *pkt;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_marker");
#endif

    /* Get request packet. */
    if ( (pkt = (mrk_entry_t *)qla1280_req_pkt(ha)) )
    {
        pkt->entry_type = MARKER_TYPE;
        pkt->lun = (uint8_t)l;
        pkt->target = (uint8_t)(b ? (t | BIT_7) : t);
        pkt->modifier = type;

        /* Issue command to ISP */
        qla1280_isp_cmd(ha);
    }

#ifdef QL_DEBUG_LEVEL_3
    LEAVE("qla1280_marker");
#endif
}

#if  QLA1280_64BIT_SUPPORT
/*
 * qla1280_64bit_start_scsi
 *      The start SCSI is responsible for building request packets on
 *      request ring and modifying ISP input pointer.
 *
 * Input:
 *      ha = adapter block pointer.
 *      sp = SB structure pointer.
 *
 * Returns:
 *      0 = success, was able to issue command.
 */
STATIC uint8_t
qla1280_64bit_start_scsi(scsi_qla_host_t *ha, srb_t *sp)
{
    device_reg_t    *reg   = ha->iobase;
    uint8_t         status = 0;
    Scsi_Cmnd       *cmd = sp->cmd;
    uint32_t        cnt;
    cmd_a64_entry_t     *pkt;
    uint16_t        req_cnt;
    uint16_t        seg_cnt;
    struct scatterlist    *sg = (struct scatterlist *) NULL;
    uint32_t        *dword_ptr;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_64bit_start_scsi:");
#endif

    if( qla1280_check_for_dead_scsi_bus(ha, sp) )
    {
        return(0);
    }

    /* Calculate number of entries and segments required. */
    seg_cnt = 0;
    req_cnt = 1;
    if (cmd->use_sg)
    {
        seg_cnt =  cmd->use_sg;
        sg = (struct scatterlist *) cmd->request_buffer;
            
        if (seg_cnt > 2)
        {
           req_cnt += (uint16_t)(seg_cnt - 2) / 5;
           if ((uint16_t)(seg_cnt - 2) % 5)
               req_cnt++;
        }
    }
    else if (cmd->request_bufflen)  /* If data transfer. */
    {
        DEBUG(printk("Single data transfer (0x%x)\n",cmd->request_bufflen));
        seg_cnt = 1;
    }

    /* Acquire ring specific lock */
    QLA1280_RING_LOCK(ha);

    if ((uint16_t)(req_cnt + 2) >= ha->req_q_cnt)
    {
        /* Calculate number of free request entries. */
        cnt = RD_REG_WORD(&reg->mailbox4);
        if (ha->req_ring_index < cnt)
            ha->req_q_cnt = cnt - ha->req_ring_index;
        else
            ha->req_q_cnt = REQUEST_ENTRY_CNT - (ha->req_ring_index - cnt);
    }

    /* If room for request in request ring. */
    if ((uint16_t)(req_cnt + 2) < ha->req_q_cnt)
    {
        /* Check for room in outstanding command list. */
        for (cnt = 1; cnt < MAX_OUTSTANDING_COMMANDS &&
            ha->outstanding_cmds[cnt] != 0; cnt++)
            ;

        if (cnt < MAX_OUTSTANDING_COMMANDS)
        {
            ha->outstanding_cmds[cnt] = sp;
            ha->req_q_cnt -= req_cnt;
            CMD_HANDLE(sp->cmd) = (unsigned char *) (u_long) cnt;

            /*
            * Build command packet.
            */
            pkt = (cmd_a64_entry_t *)ha->request_ring_ptr;

            pkt->entry_type = COMMAND_A64_TYPE;
            pkt->entry_count = (uint8_t)req_cnt;
            pkt->sys_define = (uint8_t)ha->req_ring_index;
            pkt->handle = (uint32_t)cnt;

            /* Zero out remaining portion of packet. */
            dword_ptr = (uint32_t *)pkt + 2;
            for (cnt = 2; cnt < REQUEST_ENTRY_SIZE/4; cnt++)
                *dword_ptr++ = 0;

            /* Set ISP command timeout. */
            pkt->timeout = (uint16_t)30;

            /* Set device target ID and LUN */
            pkt->lun = SCSI_LUN_32(cmd);
            pkt->target = SCSI_BUS_32(cmd) ?
                    (SCSI_TCN_32(cmd) | BIT_7) : SCSI_TCN_32(cmd);

            /* Enable simple tag queuing if device supports it. */
            if (cmd->device->tagged_queue )
                pkt->control_flags |= BIT_3;

            /* Load SCSI command packet. */
            pkt->cdb_len = (uint16_t)CMD_CDBLEN(cmd);
            BCOPY(&(CMD_CDBP(cmd)), pkt->scsi_cdb, pkt->cdb_len);
            DEBUG(printk("Build packet for command[0]=0x%x\n",pkt->scsi_cdb[0]));

            /*
            * Load data segments.
            */
            if (seg_cnt)                /* If data transfer. */
            {
                /* Set transfer direction. */
                if ( (cmd->data_cmnd[0] == WRITE_6) )
                    pkt->control_flags |= BIT_6;
                else
                    pkt->control_flags |= (BIT_5|BIT_6);
                    
                sp->dir = pkt->control_flags & (BIT_5|BIT_6);

                /* Set total data segment count. */
                pkt->dseg_count = seg_cnt;

                /* Setup packet address segment pointer. */
                dword_ptr = (uint32_t *)&pkt->dseg_0_address;

                if (cmd->use_sg)              /* If scatter gather */
                {
                    /* Load command entry data segments. */
                    for (cnt = 0; cnt < 2 && seg_cnt; cnt++, seg_cnt--)
                    {
                        DEBUG(sprintf(debug_buff,"SG Segment ap=0x%p, len=0x%x\n\r",sg->address,sg->length));
                        DEBUG(qla1280_print(debug_buff));
                        *dword_ptr++ = cpu_to_le32(VIRT_TO_BUS_LOW(sg->address));
                        *dword_ptr++ = cpu_to_le32(VIRT_TO_BUS_HIGH(sg->address));
                        *dword_ptr++ = sg->length;
                        sg++;
                    }
#ifdef QL_DEBUG_LEVEL_5
                    qla1280_print(
                            "qla1280_64bit_start_scsi: Scatter/gather command packet data - ");
                    qla1280_print("b ");
                    qla1280_output_number((uint32_t)SCSI_BUS_32(cmd), 10);
                    qla1280_print(" t ");
                    qla1280_output_number((uint32_t)SCSI_TCN_32(cmd), 10);
                    qla1280_print(" d ");
                    qla1280_output_number((uint32_t)SCSI_LUN_32(cmd), 10);
                    qla1280_print("\n\r");
                    qla1280_dump_buffer((caddr_t)pkt, REQUEST_ENTRY_SIZE);
#endif
                    /*
                    * Build continuation packets.
                    */
                    while (seg_cnt > 0)
                    {
                        /* Adjust ring index. */
                        ha->req_ring_index++;
                        if (ha->req_ring_index == REQUEST_ENTRY_CNT)
                        {
                            ha->req_ring_index = 0;
                            ha->request_ring_ptr = ha->request_ring;
                        }
                        else
                            ha->request_ring_ptr++;

                        pkt = (cmd_a64_entry_t *)ha->request_ring_ptr;

                        /* Zero out packet. */
                        dword_ptr = (uint32_t *)pkt;
                        for (cnt = 0;cnt < REQUEST_ENTRY_SIZE/4; cnt++)
                            *dword_ptr++ = 0;

                        /* Load packet defaults. */
                        ((cont_a64_entry_t *)pkt)->entry_type =
                                CONTINUE_A64_TYPE;
                        ((cont_a64_entry_t *)pkt)->entry_count = 1;
                        ((cont_a64_entry_t *)pkt)->sys_define = (uint8_t)
                                ha->req_ring_index;

                        /* Setup packet address segment pointer. */
                        dword_ptr = (uint32_t *)
                                &((cont_a64_entry_t *)pkt)->dseg_0_address;

                        /* Load continuation entry data segments. */
                        for (cnt = 0; cnt < 5 && seg_cnt; cnt++, seg_cnt--)
                        {
                            *dword_ptr++ = cpu_to_le32(VIRT_TO_BUS_LOW(sg->address));
                            *dword_ptr++ = cpu_to_le32(VIRT_TO_BUS_HIGH(sg->address));
                            *dword_ptr++ = sg->length;
                            sg++;
                        }
#ifdef QL_DEBUG_LEVEL_5
                        qla1280_print(
                                "qla1280_64bit_start_scsi: continuation packet data - c");
                        qla1280_print(" b ");
                        qla1280_output_number((uint32_t)SCSI_BUS_32(cmd), 10);

                        qla1280_print(" t ");
                        qla1280_output_number((uint32_t)SCSI_TCN_32(cmd), 10);
                        qla1280_print(" d ");
                        qla1280_output_number((uint32_t)SCSI_LUN_32(cmd), 10);
                        qla1280_print("\n\r");
                        qla1280_dump_buffer((caddr_t)pkt, REQUEST_ENTRY_SIZE);
#endif
                    }
                }
                else                    /* No scatter gather data transfer */
                {
                    *dword_ptr++ = cpu_to_le32(VIRT_TO_BUS_LOW(cmd->request_buffer));
                    *dword_ptr++ = cpu_to_le32(VIRT_TO_BUS_HIGH(cmd->request_buffer));
                    *dword_ptr = (uint32_t) cmd->request_bufflen;
#ifdef QL_DEBUG_LEVEL_5
                    qla1280_print(
                            "qla1280_64bit_start_scsi: No scatter/gather command packet data - c");
                    qla1280_print(" b ");
                    qla1280_output_number((uint32_t)SCSI_BUS_32(cmd), 10);
                    qla1280_print(" t ");
                    qla1280_output_number((uint32_t)SCSI_TCN_32(cmd), 10);
                    qla1280_print(" d ");
                    qla1280_output_number((uint32_t)SCSI_LUN_32(cmd), 10);
                    qla1280_print("\n\r");
                    qla1280_dump_buffer((caddr_t)pkt, REQUEST_ENTRY_SIZE);
#endif
                }
            }
#ifdef QL_DEBUG_LEVEL_5
            else                            /* No data transfer */
            {
                *dword_ptr++ = (uint32_t) 0;
                *dword_ptr++ = (uint32_t) 0;
                *dword_ptr = (uint32_t)  0;
                qla1280_print(
                        "qla1280_64bit_start_scsi: No data, command packet data - c");
                qla1280_print(" b ");
                qla1280_output_number((uint32_t)SCSI_BUS_32(cmd), 10);
                qla1280_print(" t ");
                qla1280_output_number((uint32_t)SCSI_TCN_32(cmd), 10);
                qla1280_print(" d ");
                qla1280_output_number((uint32_t)SCSI_LUN_32(cmd), 10);
                qla1280_print("\n\r");
                qla1280_dump_buffer((caddr_t)pkt, REQUEST_ENTRY_SIZE);
            }
#endif
            /* Adjust ring index. */
            ha->req_ring_index++;
            if (ha->req_ring_index == REQUEST_ENTRY_CNT)
            {
                ha->req_ring_index = 0;
                ha->request_ring_ptr = ha->request_ring;
            }
            else
                ha->request_ring_ptr++;

            /* Set chip new ring index. */
            WRT_REG_WORD(&reg->mailbox4, ha->req_ring_index);
        }
        else
        {
            status = 1;
#ifdef QL_DEBUG_LEVEL_2
            qla1280_print(
                    "qla1280_64bit_start_scsi: NO ROOM IN OUTSTANDING ARRAY\n\r");
            qla1280_print(" req_q_cnt=");
            qla1280_output_number((uint32_t)ha->req_q_cnt, 16);
#endif
        }
    }
    else
    {
        status = 1;
#ifdef QL_DEBUG_LEVEL_2
        qla1280_print("qla1280_64bit_start_scsi: in-ptr=");
        qla1280_output_number((uint32_t)ha->req_ring_index, 16);
        qla1280_print(" req_q_cnt=");
        qla1280_output_number((uint32_t)ha->req_q_cnt, 16);
        qla1280_print(" req_cnt=");
        qla1280_output_number((uint32_t)req_cnt, 16);
        qla1280_print("\n\r");
#endif
    }

    /* Release ring specific lock */
    QLA1280_RING_UNLOCK(ha);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (status)
        qla1280_print("qla1280_64bit_start_scsi: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_64bit_start_scsi: exiting normally\n\r");
#endif
    return(status);
}
#endif  /* QLA1280_64BIT_SUPPORT */

/*
 * qla1280_32bit_start_scsi
 *      The start SCSI is responsible for building request packets on
 *      request ring and modifying ISP input pointer.
 *
 *      The Qlogic firmware interface allows every queue slot to have a SCSI
 *      command and up to 4 scatter/gather (SG) entries.  If we need more
 *      than 4 SG entries, then continuation entries are used that can 
 *      hold another 7 entries each.  The start routine determines if there
 *      is eought empty slots then build the combination of requests to 
 *      fulfill the OS request.
 *
 * Input:
 *      ha = adapter block pointer.
 *      sp = SCSI Request Block structure pointer.
 *
 * Returns:
 *      0 = success, was able to issue command.
 */
STATIC uint8_t
qla1280_32bit_start_scsi(scsi_qla_host_t *ha, srb_t *sp)
{
    device_reg_t    *reg   = ha->iobase;
    uint8_t         status = 0;
    Scsi_Cmnd       *cmd = sp->cmd;
    uint32_t        cnt;
    cmd_entry_t     *pkt;
    uint16_t        req_cnt;
    uint16_t        seg_cnt;
    struct scatterlist    *sg = (struct scatterlist *) NULL;
    uint8_t        *data_ptr;
    uint32_t        *dword_ptr;

    ENTER("qla1280_32bit_start_scsi");


    if( qla1280_check_for_dead_scsi_bus(ha, sp) )
    {
        return(0);
    }

    /* Calculate number of entries and segments required. */
    req_cnt = 1;
    if (cmd->use_sg)
    {
        /*
        * We must build an SG list in adapter format, as the kernel's SG list
        * cannot be used directly because of data field size (__alpha__)
        * differences and the kernel SG list uses virtual addresses where
        * we need physical addresses.
        */
        seg_cnt =  cmd->use_sg;
        sg = (struct scatterlist *) cmd->request_buffer;
        /* 
        * if greater than four sg entries then we need to allocate
        * continuation entries
        */
        if (seg_cnt > 4)
        {
            req_cnt += (uint16_t)(seg_cnt - 4) / 7;
            if ((uint16_t)(seg_cnt - 4) % 7)
                req_cnt++;
        }
        DEBUG(sprintf(debug_buff,"S/G for data transfer -num segs(%d), req blk cnt(%d)\n\r",seg_cnt,req_cnt));
        DEBUG(qla1280_print(debug_buff));
    }
    else if (cmd->request_bufflen)  /* If data transfer. */
    {
        DEBUG(printk("Single data transfer (0x%x)\n",cmd->request_bufflen));
        seg_cnt = 1;
    }
    else
    {
        DEBUG(printk("No data transfer \n"));
        seg_cnt = 0;
    }

    /* Acquire ring specific lock */
    QLA1280_RING_LOCK(ha);

    if ((uint16_t)(req_cnt + 2) >= ha->req_q_cnt)
    {
        /* Calculate number of free request entries. */
        cnt = RD_REG_WORD(&reg->mailbox4);
        if (ha->req_ring_index < cnt)
            ha->req_q_cnt = cnt - ha->req_ring_index;
        else
            ha->req_q_cnt = REQUEST_ENTRY_CNT - (ha->req_ring_index - cnt);
    }

    DEBUG(sprintf(debug_buff,"Number of free entries = (%d)\n\r",ha->req_q_cnt));
    DEBUG(qla1280_print(debug_buff));
    /* If room for request in request ring. */
    if ((uint16_t)(req_cnt + 2) < ha->req_q_cnt)
    {
        /* Check for empty slot in outstanding command list. */
        for (cnt = 1; cnt < MAX_OUTSTANDING_COMMANDS &&
            (ha->outstanding_cmds[cnt] != 0); cnt++)
            ;

        if (cnt < MAX_OUTSTANDING_COMMANDS)
        {
            CMD_HANDLE(sp->cmd) = (unsigned char *)(unsigned long)cnt;
            ha->outstanding_cmds[cnt] = sp;
            ha->req_q_cnt -= req_cnt;

            /*
            * Build command packet.
            */
            pkt = (cmd_entry_t *)ha->request_ring_ptr;

            pkt->entry_type = COMMAND_TYPE;
            pkt->entry_count = (uint8_t)req_cnt;
            pkt->sys_define = (uint8_t)ha->req_ring_index;
            pkt->handle = (uint32_t)cnt;

            /* Zero out remaining portion of packet. */
            dword_ptr = (uint32_t *)pkt + 2;
            for (cnt = 2; cnt < REQUEST_ENTRY_SIZE/4; cnt++)
                *dword_ptr++ = 0;

            /* Set ISP command timeout. */
            pkt->timeout = (uint16_t)30;

            /* Set device target ID and LUN */
            pkt->lun = SCSI_LUN_32(cmd);
            pkt->target = SCSI_BUS_32(cmd) ?
                    (SCSI_TCN_32(cmd) | BIT_7) : SCSI_TCN_32(cmd);

            /* Enable simple tag queuing if device supports it. */
            if (cmd->device->tagged_queue )
                pkt->control_flags |= BIT_3;

            /* Load SCSI command packet. */
            pkt->cdb_len = (uint16_t)CMD_CDBLEN(cmd);
            data_ptr = (uint8_t *) &(CMD_CDBP(cmd));
            for (cnt = 0; cnt < pkt->cdb_len; cnt++)
                pkt->scsi_cdb[cnt] = *data_ptr++;
            DEBUG(printk("Build packet for command[0]=0x%x\n",pkt->scsi_cdb[0]));
            /*
            * Load data segments.
            */
            if (seg_cnt)
            {
                DEBUG(printk("loading data segments..\n"));
                /* Set transfer direction (READ and WRITE) */
                /* Linux doesn't tell us                   */

                /*
                * 3/10 dg - Normally, we should need this check with our F/W
                * but because of a small issue with it we do.
                *
                * For block devices, cmd->request.cmd has the operation 
                * For character devices, this isn't always set properly, so
                * we need to check data_cmnd[0].  This catches the conditions
                * for st.c, but not sg. Generic commands are pass down to us.
                */
                if ( (cmd->data_cmnd[0] == WRITE_6) )
                    pkt->control_flags |= BIT_6;
                else
                    pkt->control_flags |= (BIT_5|BIT_6);
                    
                sp->dir = pkt->control_flags & (BIT_5|BIT_6);

                /* Set total data segment count. */
                pkt->dseg_count = seg_cnt;

                /* Setup packet address segment pointer. */
                dword_ptr = (uint32_t *)&pkt->dseg_0_address;

                if (cmd->use_sg)     /* If scatter gather */
                {
                    DEBUG(qla1280_print("Building S/G data segments..\n\r"));
                    DEBUG(qla1280_dump_buffer((caddr_t)sg, 4*16 ));
                    /* Load command entry data segments. */
                    for (cnt = 0; cnt < 4 && seg_cnt; cnt++, seg_cnt--)
                    {
                        *dword_ptr++ = (uint32_t) cpu_to_le32(VIRT_TO_BUS(sg->address));
                        *dword_ptr++ = sg->length;
                        DEBUG(sprintf(debug_buff,"SG Segment ap=0x%p, len=0x%x\n\r",sg->address,sg->length));
                        DEBUG(qla1280_print(debug_buff));
                        sg++;
                    }
                    /*
                    * Build continuation packets.
                    */
                    while (seg_cnt > 0)
                    {
                        /* Adjust ring index. */
                        ha->req_ring_index++;
                        if (ha->req_ring_index == REQUEST_ENTRY_CNT)
                        {
                            ha->req_ring_index = 0;
                            ha->request_ring_ptr = ha->request_ring;
                        }
                        else
                            ha->request_ring_ptr++;

                        pkt = (cmd_entry_t *)ha->request_ring_ptr;

                        /* Zero out packet. */
                        dword_ptr = (uint32_t *)pkt;
                        for (cnt = 0;cnt < REQUEST_ENTRY_SIZE/4; cnt++)
                            *dword_ptr++ = 0;

                        /* Load packet defaults. */
                        ((cont_entry_t *)pkt)->entry_type =
                                CONTINUE_TYPE;
                        ((cont_entry_t *)pkt)->entry_count = 1;

                        ((cont_entry_t *)pkt)->sys_define = (uint8_t)
                                ha->req_ring_index;

                        /* Setup packet address segment pointer. */
                        dword_ptr = (uint32_t *)
                                &((cont_entry_t *)pkt)->dseg_0_address;

                        /* Load continuation entry data segments. */
                        for (cnt = 0; cnt < 7 && seg_cnt; cnt++, seg_cnt--)
                        {
                            *dword_ptr++ = (u_int) cpu_to_le32(VIRT_TO_BUS(sg->address));
                            *dword_ptr++ = sg->length;
                            sg++;
                        }
#ifdef QL_DEBUG_LEVEL_5
                        qla1280_print(
                                "qla1280_32bit_start_scsi: continuation packet data - scsi(");
                        qla1280_output_number((uint32_t)SCSI_BUS_32(cmd), 10);
                        qla1280_print(":");
                        qla1280_output_number((uint32_t)SCSI_TCN_32(cmd), 10);
                        qla1280_print(":");
                        qla1280_output_number((uint32_t)SCSI_LUN_32(cmd), 10);
                        qla1280_print(")\n\r");
                        qla1280_dump_buffer((caddr_t)pkt, REQUEST_ENTRY_SIZE);
#endif
                    }
                }
                else                    /* No scatter gather data transfer */
                {
                    *dword_ptr++ = (uint32_t) cpu_to_le32(VIRT_TO_BUS(cmd->request_buffer));
                    *dword_ptr = (uint32_t) cmd->request_bufflen;
                    DEBUG(printk("Single Segment ap=0x%p, len=0x%x\n",cmd->request_buffer,cmd->request_bufflen));
                }
            }
            else                            /* No data transfer */
            {
                *dword_ptr++ = (uint32_t) 0;
                *dword_ptr = (uint32_t)  0;
#ifdef QL_DEBUG_LEVEL_5
                qla1280_print(
                        "qla1280_32bit_start_scsi: No data, command packet data - ");
                qla1280_print("\n\r");
                qla1280_dump_buffer((caddr_t)pkt, REQUEST_ENTRY_SIZE);
#endif
            }
#ifdef QL_DEBUG_LEVEL_5
            qla1280_print("qla1280_32bit_start_scsi: First IOCB block:\n\r");
            qla1280_dump_buffer((caddr_t)ha->request_ring_ptr, REQUEST_ENTRY_SIZE);
#endif
            /* Adjust ring index. */
            ha->req_ring_index++;
            if (ha->req_ring_index == REQUEST_ENTRY_CNT)
            {
                ha->req_ring_index = 0;
                ha->request_ring_ptr = ha->request_ring;
            }
            else
                ha->request_ring_ptr++;

            /* Set chip new ring index. */
            DEBUG(qla1280_print("qla1280_32bit_start_scsi: Wakeup RISC for pending command\n\r"));
            ha->qthreads--;
            sp->u_start = jiffies;
            sp->flags |= SRB_SENT;
            ha->actthreads++;
            /* qla1280_output_number((uint32_t)ha->actthreads++, 16); */
            WRT_REG_WORD(&reg->mailbox4, ha->req_ring_index);
        }
        else
        {
            status = 1;
#ifdef QL_DEBUG_LEVEL_2
            qla1280_print(
                    "qla1280_32bit_start_scsi: NO ROOM IN OUTSTANDING ARRAY\n\r");
            qla1280_print(" req_q_cnt=");
            qla1280_output_number((uint32_t)ha->req_q_cnt, 16);
            qla1280_print("\n\r");
#endif
        }
    }
    else
    {
        status = 1;
#ifdef QL_DEBUG_LEVEL_2
        /*  qla1280_print("qla1280_32bit_start_scsi: in-ptr=");
        qla1280_output_number((uint32_t)ha->req_ring_index, 16);
        qla1280_print(" req_q_cnt=");
        qla1280_output_number((uint32_t)ha->req_q_cnt, 16);
        qla1280_print(" req_cnt=");
        qla1280_output_number((uint32_t)req_cnt, 16);
        qla1280_print("\n\r"); */
#endif
    }

    /* Release ring specific lock */
    QLA1280_RING_UNLOCK(ha);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    /* if (status)
    qla1280_print("qla1280_32bit_start_scsi: **** FAILED ****\n\r"); */
#endif
#ifdef QL_DEBUG_LEVEL_3
            LEAVE("qla1280_32bit_start_scsi");
#endif
    return(status);
}

/*
 * qla1280_req_pkt
 *      Function is responsible for locking ring and
 *      getting a zeroed out request packet.
 *
 * Input:
 *      ha  = adapter block pointer.
 *
 * Returns:
 *      0 = failed to get slot.
 */
STATIC request_t *
qla1280_req_pkt(scsi_qla_host_t *ha)
{
    device_reg_t    *reg = ha->iobase;
    request_t       *pkt = 0;
    uint16_t        cnt;
    uint32_t        *dword_ptr;
    uint32_t        timer;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_req_pkt");
#endif

    /* Wait for 30 seconds for slot. */
    for (timer = 15000000; timer; timer--)
    {
        /* Acquire ring specific lock */
        QLA1280_RING_LOCK(ha);

        if (ha->req_q_cnt > 0)
        {
            /* Calculate number of free request entries. */
            cnt = RD_REG_WORD(&reg->mailbox4);
            if (ha->req_ring_index < cnt)
                ha->req_q_cnt = cnt - ha->req_ring_index;
            else
                ha->req_q_cnt = REQUEST_ENTRY_CNT - (ha->req_ring_index - cnt);
        }

        /* Found empty request ring slot? */
        if (ha->req_q_cnt > 0)
        {
            ha->req_q_cnt--;
            pkt = ha->request_ring_ptr;

            /* Zero out packet. */
            dword_ptr = (uint32_t *)pkt;
            for (cnt = 0; cnt < REQUEST_ENTRY_SIZE/4; cnt++)
                *dword_ptr++ = 0;

            /* Set system defined field. */
            pkt->sys_define = (uint8_t)ha->req_ring_index;

            /* Set entry count. */
            pkt->entry_count = 1;

            break;
        }

        /* Release ring specific lock */
        QLA1280_RING_UNLOCK(ha);

        SYS_DELAY(2);   /* 10 */ 

        /* Check for pending interrupts. */
        qla1280_poll(ha);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_req_pkt: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_req_pkt: exiting normally\n\r");
#endif
    return(pkt);
}

/*
 * qla1280_isp_cmd
 *      Function is responsible for modifying ISP input pointer.
 *      Releases ring lock.
 *
 * Input:
 *      ha  = adapter block pointer.
 */
STATIC void
qla1280_isp_cmd(scsi_qla_host_t *ha)
{
    device_reg_t    *reg = ha->iobase;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_isp_cmd");
#endif

#ifdef QL_DEBUG_LEVEL_5
    qla1280_print("qla1280_isp_cmd: IOCB data:\n\r");
    qla1280_dump_buffer((caddr_t)ha->request_ring_ptr, REQUEST_ENTRY_SIZE); 
#endif

    /* Adjust ring index. */
    ha->req_ring_index++;
    if (ha->req_ring_index == REQUEST_ENTRY_CNT)
    {
        ha->req_ring_index = 0;
        ha->request_ring_ptr = ha->request_ring;
    }
    else
        ha->request_ring_ptr++;

    /* Set chip new ring index. */
    WRT_REG_WORD(&reg->mailbox4, ha->req_ring_index);

    /* Release ring specific lock */
    QLA1280_RING_UNLOCK(ha);

#ifdef QL_DEBUG_LEVEL_3
    LEAVE("qla1280_isp_cmd");
#endif
}

/*
 * qla1280_enable_lun
 *      Issue enable LUN entry IOCB.
 *
 * Input:
 *      ha = adapter block pointer.
 *      b  = SCSI BUS number.
 *      l  = LUN number.
 */
STATIC void
qla1280_enable_lun(scsi_qla_host_t *ha, uint8_t b, uint32_t l)
{
    elun_entry_t    *pkt;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_enable_lun: entered\n\r");
#endif

    /* Get request packet. */
    /*
    if (pkt = (elun_entry_t *)qla1280_req_pkt(ha))
    {
    pkt->entry_type = ENABLE_LUN_TYPE;
    pkt->lun = (uint16_t)(b ? l | BIT_15 : l);
    pkt->command_count = 32;
    pkt->immed_notify_count = 1;
    pkt->group_6_length = MAX_CMDSZ;
    pkt->group_7_length = MAX_CMDSZ;
    pkt->timeout = 0x30;

    qla1280_isp_cmd(ha);
    }
    */
    pkt = (elun_entry_t *)1;

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_enable_lun: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_enable_lun: exiting normally\n\r");
#endif
}

#if QL1280_TARGET_MODE_SUPPORT
/****************************************************************************/
/*                      Target Mode Support Functions.                      */
/****************************************************************************/

/*
 * qla1280_notify_ack
 *      Issue notify acknowledge IOCB.
 *      If sequence ID is zero, acknowledgement of
 *      SCSI bus reset or bus device reset is assumed.
 *
 * Input:
 *      ha      = adapter block pointer.
 *      inotify = immediate notify entry pointer.
 */
STATIC void
qla1280_notify_ack(scsi_qla_host_t *ha, notify_entry_t *inotify)
{
    nack_entry_t    *pkt;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_notify_ack: entered\n\r");
#endif

    /* Get request packet. */
    if (pkt = (nack_entry_t *)qla1280_req_pkt(ha))
    {
        pkt->entry_type = NOTIFY_ACK_TYPE;
        pkt->lun = inotify->lun;
        pkt->initiator_id = inotify->initiator_id;
        pkt->target_id = inotify->target_id;
        if (inotify->seq_id == 0)
            pkt->event = BIT_7;
        else
            pkt->seq_id = inotify->seq_id;

        /* Issue command to ISP */
        qla1280_isp_cmd(ha);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_notify_ack: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_notify_ack: exiting normally\n\r");
#endif
}

/*
 * qla1280_immed_notify
 *      Issue immediate notify IOCB for LUN 0.
 *
 * Input:
 *      ha      = adapter block pointer.
 *      inotify = immediate notify entry pointer.
 */
STATIC void
qla1280_immed_notify(scsi_qla_host_t *ha, notify_entry_t *inotify)
{
    notify_entry_t    *pkt;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_immed_notify: entered\n\r");
#endif

    /* Get request packet. */
    if (pkt = (notify_entry_t *)qla1280_req_pkt(ha))
    {
        pkt->entry_type = IMMED_NOTIFY_TYPE;
        pkt->lun = inotify->lun;
        pkt->initiator_id = inotify->initiator_id;
        pkt->target_id = inotify->target_id;
        pkt->status = 1;

        /* Issue command to ISP */
        qla1280_isp_cmd(ha);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_immed_notify: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_immed_notify: exiting normally\n\r");
#endif
}

/*
 * qla1280_accept_io
 *      Issue accept target I/O IOCB for LUN 0.
 *
 * Input:
 *      ha = adapter block pointer.
 *      ctio = ctio returned entry pointer.
 */
STATIC void
qla1280_accept_io(scsi_qla_host_t *ha, ctio_ret_entry_t *ctio)
{
    atio_entry_t    *pkt;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_accept_io: entered\n\r");
#endif

    /* Get request packet. */
    if (pkt = (atio_entry_t *)qla1280_req_pkt(ha))
    {
        pkt->entry_type = ACCEPT_TGT_IO_TYPE;
        pkt->lun = ctio->lun;
        pkt->initiator_id = ctio->initiator_id;
        pkt->target_id = ctio->target_id;
        pkt->tag_value = ctio->tag_value;
        pkt->status = 1;

        /* Issue command to ISP */
        qla1280_isp_cmd(ha);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_accept_io: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_accept_io: exiting normally\n\r");
#endif
}

/*
 * qla1280_64bit_continue_io
 *      Issue continue target I/O IOCB.
 *
 * Input:
 *      ha   = adapter block pointer.
 *      atio = atio pointer.
 *      len  = total bytecount.
 *      addr = physical address pointer.
 */
STATIC void
qla1280_64bit_continue_io(scsi_qla_host_t *ha, atio_entry_t *atio, uint32_t len,
                    paddr32_t *addr)
{
    ctio_a64_entry_t *pkt;
    uint32_t         *dword_ptr;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_64bit_continue_io: entered\n\r");
#endif

    /* Get request packet. */
    if (pkt = (ctio_a64_entry_t *)qla1280_req_pkt(ha))
    {
        pkt->entry_type = CTIO_A64_TYPE;
        pkt->lun = atio->lun;
        pkt->initiator_id = atio->initiator_id;
        pkt->target_id = atio->target_id;
        pkt->option_flags = atio->option_flags;
        pkt->tag_value = atio->tag_value;
        pkt->scsi_status = atio->scsi_status;

        if (len)
        {
            pkt->dseg_count = 1;
            pkt->transfer_length = len;
            pkt->dseg_0_length = len;
            dword_ptr = (uint32_t *)addr;
            pkt->dseg_0_address[0] = *dword_ptr++;
            pkt->dseg_0_address[1] = *dword_ptr;
        }

        /* Issue command to ISP */
        qla1280_isp_cmd(ha);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_64bit_continue_io: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_64bit_continue_io: exiting normally\n\r");
#endif
}

/*
 * qla1280_32bit_continue_io
 *      Issue continue target I/O IOCB.
 *
 * Input:
 *      ha   = adapter block pointer.
 *      atio = atio pointer.
 *      len  = total bytecount.
 *      addr = physical address pointer.
 */
STATIC void
qla1280_32bit_continue_io(scsi_qla_host_t *ha, atio_entry_t *atio, uint32_t len,
                    paddr32_t *addr)
{
    ctio_entry_t *pkt;
    uint32_t     *dword_ptr;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_32bit_continue_io: entered\n\r");
#endif

    /* Get request packet. */
    if (pkt = (ctio_entry_t *)qla1280_req_pkt(ha))
    {
        pkt->entry_type = CONTINUE_TGT_IO_TYPE;
        pkt->lun = atio->lun;
        pkt->initiator_id = atio->initiator_id;
        pkt->target_id = atio->target_id;
        pkt->option_flags = atio->option_flags;
        pkt->tag_value = atio->tag_value;
        pkt->scsi_status = atio->scsi_status;

        if (len)
        {
            pkt->dseg_count = 1;
            pkt->transfer_length = len;
            pkt->dseg_0_length = len;
            dword_ptr = (uint32_t *)addr;
            pkt->dseg_0_address = *dword_ptr;
        }

        /* Issue command to ISP */
        qla1280_isp_cmd(ha);
    }

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
    if (!pkt)
        qla1280_print("qla1280_32bit_continue_io: **** FAILED ****\n\r");
#endif
#ifdef QL_DEBUG_LEVEL_3
    else
        qla1280_print("qla1280_32bit_continue_io: exiting normally\n\r");
#endif
}
#endif /* QL1280_TARGET_MODE_SUPPORT */

/****************************************************************************/
/*                        Interrupt Service Routine.                        */
/****************************************************************************/

/****************************************************************************
 *  qla1280_isr
 *      Calls I/O done on command completion.
 *
 * Input:
 *      ha           = adapter block pointer.
 *      done_q_first = done queue first pointer.
 *      done_q_last  = done queue last pointer.
 *      INTR_LOCK must be already obtained.
 ****************************************************************************/
STATIC void
qla1280_isr(scsi_qla_host_t *ha, srb_t **done_q_first, srb_t **done_q_last)
{
    device_reg_t    *reg = ha->iobase;
    response_t      *pkt;
    srb_t           *sp;
    uint16_t        mailbox[MAILBOX_REGISTER_COUNT];
    uint16_t        *wptr;
    uint32_t        index;

    ENTER("qla1280_isr");


    /* Save mailbox register 5 */
    mailbox[5] = RD_REG_WORD(&reg->mailbox5);

    /* Check for mailbox interrupt. */

    mailbox[0] = RD_REG_WORD(&reg->semaphore);
    if (mailbox[0] & BIT_0)
    {
        /* Get mailbox data. */

        wptr = &mailbox[0];
        *wptr++ = RD_REG_WORD(&reg->mailbox0);
        *wptr++ = RD_REG_WORD(&reg->mailbox1);
        *wptr = RD_REG_WORD(&reg->mailbox2);
        if (mailbox[0] != MBA_SCSI_COMPLETION)
        {
            wptr++;
            *wptr++ = RD_REG_WORD(&reg->mailbox3);
            *wptr++ = RD_REG_WORD(&reg->mailbox4);
            wptr++;
            *wptr++ = RD_REG_WORD(&reg->mailbox6);
            *wptr   = RD_REG_WORD(&reg->mailbox7);
        }

        /* Release mailbox registers. */

        WRT_REG_WORD(&reg->semaphore, 0);
        WRT_REG_WORD(&reg->host_cmd, HC_CLR_RISC_INT);

#ifdef QL_DEBUG_LEVEL_5
        qla1280_print("qla1280_isr: mailbox interrupt mailbox[0] = ");
        qla1280_output_number((uint32_t)mailbox[0], 16);
        qla1280_print("\n\r");
#endif

        /* Handle asynchronous event */

        switch (mailbox[0])
        {
            case MBA_SCSI_COMPLETION:   /* Response completion */
#ifdef QL_DEBUG_LEVEL_5
                qla1280_print("qla1280_isr: mailbox response completion\n\r");
#endif
                if (ha->flags.online)
                {
                    /* Get outstanding command index. */
                    index = (uint32_t)(mailbox[2] << 16 | mailbox[1]);

                    /* Validate handle. */
                    if (index < MAX_OUTSTANDING_COMMANDS)
                        sp = ha->outstanding_cmds[index];
                    else
                        sp = 0;

                    if (sp)
                    {
                        /* Free outstanding command slot. */
                        ha->outstanding_cmds[index] = 0;

                        /* Save ISP completion status */
                        CMD_RESULT(sp->cmd) = 0;

                        /* Place block on done queue */
                        sp->s_next = NULL;
                        sp->s_prev = *done_q_last;
                        if (!*done_q_first)
                            *done_q_first = sp;
                        else
                            (*done_q_last)->s_next = sp;
                        *done_q_last = sp;
                    }
                    else
                    {
#ifdef QL_DEBUG_LEVEL_2
                            qla1280_print("qla1280_isr: ISP invalid handle\n\r");
#endif
                            printk(KERN_WARNING "qla1280: ISP invalid handle");
                            ha->flags.isp_abort_needed = TRUE;
                    }
                }
                break;
            case MBA_BUS_RESET:         /* SCSI Bus Reset */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print("qla1280_isr: asynchronous BUS_RESET\n\r");
#endif
                ha->flags.reset_marker = TRUE;
                index = mailbox[6] & BIT_0;
                ha->bus_settings[index].reset_marker = TRUE;
                break;
            case MBA_SYSTEM_ERR:        /* System Error */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print("qla1280_isr: ISP System Error - mbx1=");
                qla1280_output_number((uint32_t)mailbox[1], 16);
                qla1280_print(", mbx2=");
                qla1280_output_number((uint32_t)mailbox[2], 16);
                qla1280_print(", mbx3=");
                qla1280_output_number((uint32_t)mailbox[3], 16);
                qla1280_print("\n\r");
#endif
                printk(KERN_WARNING
                        "qla1280: ISP System Error - mbx1=%xh, mbx2=%xh, mbx3=%xh\n",
                        mailbox[1], mailbox[2], mailbox[3]);
                ha->flags.isp_abort_needed = TRUE;
                break;
            case MBA_REQ_TRANSFER_ERR:  /* Request Transfer Error */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print("qla1280_isr: ISP Request Transfer Error\n\r");
#endif
                printk(KERN_WARNING "qla1280: ISP Request Transfer Error\n");
                ha->flags.isp_abort_needed = TRUE;
                break;
            case MBA_RSP_TRANSFER_ERR:  /* Response Transfer Error */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print("qla1280_isr: ISP Response Transfer Error\n\r");
#endif
                printk(KERN_WARNING "qla1280: ISP Response Transfer Error\n");
                ha->flags.isp_abort_needed = TRUE;
                break;
            case MBA_WAKEUP_THRES:      /* Request Queue Wake-up */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print("qla1280_isr: asynchronous WAKEUP_THRES\n\r");
#endif
                break;
            case MBA_TIMEOUT_RESET:     /* Execution Timeout Reset */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print("qla1280_isr: asynchronous TIMEOUT_RESET\n\r");
#endif
                break;
            case MBA_DEVICE_RESET:         /* Bus Device Reset */
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print(
                        "qla1280_isr: asynchronous BUS_DEVICE_RESET\n\r");
#endif
                ha->flags.reset_marker = TRUE;
                index = mailbox[6] & BIT_0;
                ha->bus_settings[index].reset_marker = TRUE;
                break;
            case MBA_BUS_MODE_CHANGE:
#ifdef QL_DEBUG_LEVEL_2
                qla1280_print(
                        "qla1280_isr: asynchronous BUS_MODE_CHANGE\n\r");
#endif
                break;
            default:
                if (mailbox[0] < MBA_ASYNC_EVENT)
                {
                        wptr = &mailbox[0];
                        ha->mailbox_out[0] = *wptr++;
                        ha->mailbox_out[1] = *wptr++;
                        ha->mailbox_out[2] = *wptr++;
                        ha->mailbox_out[3] = *wptr++;
                        ha->mailbox_out[4] = *wptr++;
                        ha->mailbox_out[5] = *wptr++;
                        ha->mailbox_out[6] = *wptr++;
                        ha->mailbox_out[7] = *wptr;
                        ha->flags.mbox_int = TRUE;
                }
                break;
        }
    }
    else
        WRT_REG_WORD(&reg->host_cmd, HC_CLR_RISC_INT);

    /*
    * Response ring
    */
    if (ha->flags.online && !ha->flags.mbox_busy)
    {
        if (mailbox[5] < RESPONSE_ENTRY_CNT)
        {
            while (ha->rsp_ring_index != mailbox[5])
            {
                pkt = ha->response_ring_ptr;

#ifdef QL_DEBUG_LEVEL_5
                qla1280_print("qla1280_isr: ha->rsp_ring_index = ");
                qla1280_output_number((uint32_t)ha->rsp_ring_index, 16);
                qla1280_print(" mailbox[5] = ");
                qla1280_output_number((uint32_t)mailbox[5], 16);
                qla1280_print("\n\rqla1280_isr: response packet data\n\r");
                qla1280_dump_buffer((caddr_t)pkt, RESPONSE_ENTRY_SIZE);
#endif

#if defined(QL_DEBUG_LEVEL_2) && !defined(QL_DEBUG_LEVEL_5)
                if (pkt->entry_type == STATUS_TYPE)
                {
                    if ((uint8_t)(pkt->scsi_status) || pkt->comp_status ||
                        pkt->entry_status)
                    {
                        DEBUG(qla1280_print("qla1280_isr: ha->rsp_ring_index = ");)
                        DEBUG(qla1280_output_number((uint32_t)ha->rsp_ring_index,
                                16);)
                        DEBUG(qla1280_print(" mailbox[5] = ");)
                        DEBUG(qla1280_output_number((uint32_t)mailbox[5], 16);)
                        DEBUG(qla1280_print( "\n\r comp_status = ");)
                        DEBUG(qla1280_output_number((uint32_t)pkt->comp_status,16);)
                        DEBUG(qla1280_print( ", ");)
                        DEBUG(qla1280_print( " scsi_status = ");)
                        DEBUG(qla1280_output_number((uint32_t)pkt->scsi_status,16);)
                        DEBUG(qla1280_print( "\n\r");)
                        /* qla1280_print(
                        "\n\rqla1280_isr: response packet data\n\r");
                        qla1280_dump_buffer((caddr_t)pkt,
                        RESPONSE_ENTRY_SIZE); */
                    }
                }
                else
                {
                    qla1280_print("qla1280_isr: ha->rsp_ring_index = ");
                    qla1280_output_number((uint32_t)ha->rsp_ring_index, 16);
                    qla1280_print(" mailbox[5] = ");
                    qla1280_output_number((uint32_t)mailbox[5], 16);
                    qla1280_print(
                            "\n\rqla1280_isr: response packet data\n\r");
                    qla1280_dump_buffer((caddr_t)pkt, RESPONSE_ENTRY_SIZE);
                }
#endif
                if (pkt->entry_type == STATUS_TYPE || pkt->entry_status)
                {
                    if (pkt->entry_type == STATUS_TYPE)
                        qla1280_status_entry(ha, (sts_entry_t *)pkt,
                                done_q_first, done_q_last);
                    else
                        qla1280_error_entry(ha, pkt,
                                done_q_first, done_q_last);

                    /* Adjust ring index. */
                    ha->rsp_ring_index++;
                    if (ha->rsp_ring_index == RESPONSE_ENTRY_CNT)
                    {
                        ha->rsp_ring_index = 0;
                        ha->response_ring_ptr = ha->response_ring;
                    }
                    else
                        ha->response_ring_ptr++;
                    WRT_REG_WORD(&reg->mailbox5, ha->rsp_ring_index);
                }
#if QLA1280_TARGET_MODE_SUPPORT
                else
                {
                    pkt = &response_entry;

                    /* Copy packet. */
                    dptr1 = (uint32_t *)ha->response_ring_ptr;
                    dptr2 = (uint32_t *)pkt;
                    for (index = 0; index < RESPONSE_ENTRY_SIZE/4; index++)
                        *dptr2++ = *dptr1++;

                    /* Adjust ring index. */
                    ha->rsp_ring_index++;
                    if (ha->rsp_ring_index == RESPONSE_ENTRY_CNT)
                    {
                        ha->rsp_ring_index = 0;
                        ha->response_ring_ptr = ha->response_ring;
                    }
                    else
                        ha->response_ring_ptr++;
                    WRT_REG_WORD(&reg->mailbox5, ha->rsp_ring_index);

                    /* Release interrupt specific lock */
                    QLA1280_INTR_UNLOCK(ha);

                    switch (pkt->entry_type)
                    {
                        case ACCEPT_TGT_IO_TYPE:
                            qla1280_atio_entry(ha, (atio_entry_t *)pkt);
                            break;
                        case IMMED_NOTIFY_TYPE:
                            qla1280_notify_entry(ha, (notify_entry_t *)pkt);
                            break;
                        case CTIO_RET_TYPE:
                            qla1280_accept_io(ha, (ctio_ret_entry_t *)pkt);
                            break;
                        default:
                            break;
                    }

                    /* Acquire interrupt specific lock */
                    QLA1280_INTR_LOCK(ha);
                }
#endif
            }
        }
        else
        {
            ha->flags.isp_abort_needed = TRUE;
#ifdef QL_DEBUG_LEVEL_2
            qla1280_print("qla1280_isr: Response pointer Error\n");
#endif
        }
    }

    LEAVE("qla1280_isr");
}

/*
 *  qla1280_rst_aen
 *      Processes asynchronous reset.
 *
 * Input:
 *      ha  = adapter block pointer.
 */
STATIC void
qla1280_rst_aen(scsi_qla_host_t *ha)
{
#if QL1280_TARGET_MODE_SUPPORT
    notify_entry_t  nentry;
#endif
    uint8_t         b;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_rst_aen");
#endif

    if (ha->flags.online && !ha->flags.reset_active &&
        !ha->flags.abort_isp_active)
    {
        ha->flags.reset_active = TRUE;
        while (ha->flags.reset_marker)
        {
            /* Issue marker command. */
            ha->flags.reset_marker = FALSE;
            for (b = 0; b < ha->ports && !ha->flags.reset_marker; b++)
            {
                if (ha->bus_settings[b].reset_marker)
                {
                    ha->bus_settings[b].reset_marker = FALSE;
                    qla1280_marker(ha, b, 0, 0, MK_SYNC_ALL);

                    if (!ha->flags.reset_marker)
                    {
#if QL1280_TARGET_MODE_SUPPORT
                        /* Issue notify acknowledgement command. */
                        bzero((caddr_t)&nentry, sizeof(notify_entry_t));

                        nentry.initiator_id = nentry.target_id = b ?
                                ha->bus_settings[b].id | BIT_7 :
                        ha->bus_settings[b].id;
                        qla1280_notify_entry(ha, &nentry);
#endif

                        /* Asynchronous event notification */
                    }
                }
            }
        }
    }

#ifdef QL_DEBUG_LEVEL_3
    LEAVE("qla1280_rst_aen");
#endif
}

#if QL1280_TARGET_MODE_SUPPORT
/*
 *  qla1280_atio_entry
 *      Processes received ISP accept target I/O entry.
 *
 * Input:
 *      ha  = adapter block pointer.
 *      pkt = entry pointer.
 */
STATIC void
qla1280_atio_entry(scsi_qla_host_t *ha, atio_entry_t *pkt)
{
    uint64_t    *a64;
    uint64_t    *end_a64;
    paddr32_t   phy_addr[2];
    paddr32_t   end_addr[2];
    uint32_t    len;
    uint32_t    offset;
    uint8_t     t;
    uint8_t     *sense_ptr;

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_atio_entry: entered\n\r");
#endif

    t = pkt->initiator_id;
    sense_ptr = ha->tsense + t * TARGET_SENSE_SIZE;
    a64 = (uint64_t *)&phy_addr[0];
    end_a64 = (uint64_t *)&end_addr[0];

    switch (pkt->status & ~BIT_7)
    {
        case 7:                         /* Path invalid */
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
            qla1280_print("qla1280_atio_entry: Path invalid\n\r");
#endif
            break;
        case 0x14:                  /* Target Bus Phase Sequence Failure */
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
            qla1280_print(
                    "qla1280_atio_entry: Target Bus Phase Sequence Failure\n\r");
#endif
            if (pkt->status & BIT_7)
            {
                BCOPY((caddr_t)&pkt->sense_data, sense_ptr,TARGET_SENSE_SIZE);
            }
            else
            {
                    bzero(sense_ptr, TARGET_SENSE_SIZE);
                    *sense_ptr = 0x70;
                    *(sense_ptr+2) = SD_HARDERR;
                    *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                    *(sense_ptr+12) = SC_SELFAIL;
            }
            pkt->scsi_status = S_CKCON;
            pkt->option_flags |= (uint32_t)OF_SSTS | (uint32_t)OF_NO_DATA;
            if (ha->flags.enable_64bit_addressing)
                qla1280_64bit_continue_io(ha, pkt, 0, 0);
            else
                qla1280_32bit_continue_io(ha, pkt, 0, 0);
            break;
        case 0x16:                  /* Requested Capability Not Available */
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
            qla1280_print(
                    "qla1280_atio_entry: Target Bus Phase Sequence Failure\n\r");
#endif
            break;
        case 0x17:                  /* Bus Device Reset Message Received */
#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
            qla1280_print(
                    "qla1280_atio_entry: Target Bus Phase Sequence Failure\n\r");
#endif
            break;
        case 0x3D:                  /* CDB Received */

            /* Check for invalid LUN */
            if (pkt->lun && pkt->cdb[0] != SS_INQUIR &&
                pkt->cdb[0] != SS_REQSEN)
                pkt->cdb[0] = SS_TEST;

            switch (pkt->cdb[0])
            {
                case SS_TEST:
#ifdef QL_DEBUG_LEVEL_3
                    qla1280_print("qla1280_atio_entry: SS_TEST\n\r");
#endif
                    bzero(sense_ptr, TARGET_SENSE_SIZE);
                    len = 0;
                    if (pkt->lun == 0)
                        pkt->scsi_status = S_GOOD;
                    else
                    {
                        *sense_ptr = 0x70;
                        *(sense_ptr+2) = SD_ILLREQ;
                        *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                        *(sense_ptr+12) = SC_INVLUN;
                        pkt->scsi_status = S_CKCON;
                    }

                    pkt->option_flags |= (uint32_t)OF_SSTS |
                            (uint32_t)OF_NO_DATA;
                    break;
                case SS_REQSEN:
#ifdef QL_DEBUG_LEVEL_3
                    qla1280_print("qla1280_atio_entry: SS_REQSEN\n\r");
#endif
                    phy_addr[0] = ha->tsense_dma;
                    phy_addr[1] = 0;
                    *a64 += t * TARGET_SENSE_SIZE;
                    if (pkt->cdb[4] > TARGET_SENSE_SIZE)
                        len = TARGET_SENSE_SIZE;
                    else
                        len = pkt->cdb[4];
                    pkt->scsi_status = S_GOOD;
                    pkt->option_flags |= (uint32_t)OF_SSTS |
                            (uint32_t)OF_DATA_IN;
                    break;
                case SS_INQUIR:
#ifdef QL_DEBUG_LEVEL_3
                    qla1280_print("qla1280_atio_entry: SS_INQUIR\n\r");
#endif
                    bzero(sense_ptr, TARGET_SENSE_SIZE);
                    phy_addr[0] = ha->tbuf_dma;
                    phy_addr[1] = 0;
                    *a64 += TARGET_INQ_OFFSET;

                    if (pkt->lun == 0)
                    {
                            ha->tbuf->inq.id_type = ID_PROCESOR;
                            ha->tbuf->inq.id_pqual = ID_QOK;
                    }
                    else
                    {
                            ha->tbuf->inq.id_type = ID_NODEV;
                            ha->tbuf->inq.id_pqual = ID_QNOLU;
                    }

                    if (pkt->cdb[4] > sizeof(struct ident))
                        len = sizeof(struct ident);
                    else
                        len = pkt->cdb[4];
                    pkt->scsi_status = S_GOOD;
                    pkt->option_flags |= (uint32_t)OF_SSTS |
                            (uint32_t)OF_DATA_IN;
                    break;
                case SM_WRDB:
                    bzero(sense_ptr, TARGET_SENSE_SIZE);
                    offset = pkt->cdb[5];
                    offset |= pkt->cdb[4] << 8;
                    offset |= pkt->cdb[3] << 16;
                    len = pkt->cdb[8];
                    len |= pkt->cdb[7] << 8;
                    len |= pkt->cdb[6] << 16;
                    end_addr[0] = phy_addr[0] = ha->tbuf_dma;
                    end_addr[1] = phy_addr[1] = 0;
                    *end_a64 += TARGET_DATA_OFFSET + TARGET_DATA_SIZE;
                    switch (pkt->cdb[1] & 7)
                    {
                        case RW_BUF_HDATA:
#ifdef QL_DEBUG_LEVEL_3
                            qla1280_print("qla1280_atio_entry: SM_WRDB, RW_BUF_HDATA\n\r");
#endif
                            if (len > TARGET_DATA_SIZE + 4)
                            {
#ifdef QL_DEBUG_LEVEL_2
                                qla1280_print("qla1280_atio_entry: SM_WRDB, length > buffer size\n\r");
#endif
                                *sense_ptr = 0x70;
                                *(sense_ptr+2) = SD_ILLREQ;
                                *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                                *(sense_ptr+12) = SC_ILLCDB;
                                pkt->scsi_status = S_CKCON;
                                pkt->option_flags |= (uint32_t)OF_SSTS |
                                        (uint32_t)OF_NO_DATA;
                                len = 0;
                            }
                            else if (len)
                            {
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_DATA_OUT;
#ifdef QL_DEBUG_LEVEL_3
                                    qla1280_print("qla1280_atio_entry: Issuing SDI_TARMOD_WRCOMP\n\r");
#endif
                                    sdi_xaen(SDI_TARMOD_WRCOMP, ha->cntlr,
                                            pkt->target_id, pkt->lun, 0, offset);
                            }
                            else
                            {
#ifdef QL_DEBUG_LEVEL_2
                                    qla1280_print("qla1280_atio_entry: SM_WRDB, zero length\n\r");
#endif
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_NO_DATA;
                            }

                            break;
                        case RW_BUF_DATA:
#ifdef QL_DEBUG_LEVEL_3
                            qla1280_print("qla1280_atio_entry: SM_WRDB, RW_BUF_DATA\n\r");
#endif
                            *a64 += offset + TARGET_DATA_OFFSET;
                            if (pkt->cdb[2] != 0 || *a64 >= *end_a64 ||
                                *a64 + len > *end_a64)
                            {
#ifdef QL_DEBUG_LEVEL_2
                                    qla1280_print("qla1280_atio_entry: SM_WRDB, RW_BUF_DATA BAD\n\r");
                                    qla1280_print("buf_id=");
                                    qla1280_output_number((uint32_t)pkt->cdb[2], 16);
                                    qla1280_print(", offset=");
                                    qla1280_output_number((uint32_t)offset, 16);
                                    qla1280_print(", length=");
                                    qla1280_output_number((uint32_t)len, 16);
                                    qla1280_print("\n\r");
#endif
                                    *sense_ptr = 0x70;
                                    *(sense_ptr+2) = SD_ILLREQ;
                                    *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                                    *(sense_ptr+12) = SC_ILLCDB;
                                    len = 0;
                                    pkt->scsi_status = S_CKCON;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_NO_DATA;
                            }
                            else if (len)
                            {
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_DATA_OUT;
#ifdef QL_DEBUG_LEVEL_3
                                    qla1280_print("qla1280_atio_entry: Issuing SDI_TARMOD_WRCOMP\n\r");
#endif
                                    sdi_xaen(SDI_TARMOD_WRCOMP, ha->cntlr,
                                            pkt->target_id, pkt->lun, 0, offset);
                            }
                            else
                            {
#ifdef QL_DEBUG_LEVEL_2
                                    qla1280_print("qla1280_atio_entry: SM_WRDB, zero length\n\r");
#endif
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_NO_DATA;
                            }
                            break;
                        default:
#ifdef QL_DEBUG_LEVEL_2
                            qla1280_print("qla1280_atio_entry: SM_WRDB unknown mode\n\r");
#endif
                            *sense_ptr = 0x70;
                            *(sense_ptr+2) = SD_ILLREQ;
                            *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                            *(sense_ptr+12) = SC_ILLCDB;
                            len = 0;
                            pkt->scsi_status = S_CKCON;
                            pkt->option_flags |= (uint32_t)OF_SSTS |
                                    (uint32_t)OF_NO_DATA;
                            break;
                    }
                    break;
                case SM_RDDB:
                    bzero(sense_ptr, TARGET_SENSE_SIZE);
                    offset = pkt->cdb[5];
                    offset |= pkt->cdb[4] << 8;
                    offset |= pkt->cdb[3] << 16;
                    len = pkt->cdb[8];
                    len |= pkt->cdb[7] << 8;
                    len |= pkt->cdb[6] << 16;
                    end_addr[0] = phy_addr[0] = ha->tbuf_dma;
                    end_addr[1] = phy_addr[1] = 0;
                    *end_a64 += TARGET_DATA_OFFSET + TARGET_DATA_SIZE;
                    switch (pkt->cdb[1] & 7)
                    {
                        case RW_BUF_HDATA:
#ifdef QL_DEBUG_LEVEL_3
                            qla1280_print("qla1280_atio_entry: SM_RDDB, RW_BUF_HDATA\n\r");
#endif
                            if (len)
                            {
                                ha->tbuf->hdr[0] = 0;
                                ha->tbuf->hdr[1] =
                                        (uint8_t)(TARGET_DATA_SIZE >> 16);
                                ha->tbuf->hdr[2] =
                                        (uint8_t)(TARGET_DATA_SIZE >> 8);
                                ha->tbuf->hdr[3] = (uint8_t)TARGET_DATA_SIZE;
                                if (len > TARGET_DATA_SIZE + 4)
                                    len = TARGET_DATA_SIZE + 4;
                                pkt->scsi_status = S_GOOD;
                                pkt->option_flags |= (uint32_t)OF_SSTS |
                                        (uint32_t)OF_DATA_IN;
                            }
                            else
                            {
#ifdef QL_DEBUG_LEVEL_2
                                    qla1280_print("qla1280_atio_entry: SM_RDDB, zero length\n\r");
#endif
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_NO_DATA;
                            }
                            break;
                        case RW_BUF_DATA:
#ifdef QL_DEBUG_LEVEL_3
                            qla1280_print("qla1280_atio_entry: SM_RDDB, RW_BUF_DATA\n\r");
#endif
                            *a64 += offset + TARGET_DATA_OFFSET;
                            if (pkt->cdb[2] != 0 || *a64 >= *end_a64)
                            {
#ifdef QL_DEBUG_LEVEL_2
                                    qla1280_print("qla1280_atio_entry: SM_RDDB, RW_BUF_DATA BAD\n\r");
                                    qla1280_print("buf_id=");
                                    qla1280_output_number((uint32_t)pkt->cdb[2], 16);
                                    qla1280_print(", offset=");
                                    qla1280_output_number((uint32_t)offset, 16);
                                    qla1280_print("\n\r");
#endif
                                    *sense_ptr = 0x70;
                                    *(sense_ptr+2) = SD_ILLREQ;
                                    *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                                    *(sense_ptr+12) = SC_ILLCDB;
                                    len = 0;
                                    pkt->scsi_status = S_CKCON;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_NO_DATA;
                            }
                            else
                            {
                                    if (*a64 + len > *end_a64)
                                        len = *end_a64 - *a64;
                                    if (len)
                                    {
                                        pkt->scsi_status = S_GOOD;
                                        pkt->option_flags |= (uint32_t)OF_SSTS |
                                                (uint32_t)OF_DATA_IN;
                                    }
                                    else
                                    {
#ifdef QL_DEBUG_LEVEL_2
                                            qla1280_print("qla1280_atio_entry: SM_RDDB, zero length\n\r");
#endif
                                            pkt->scsi_status = S_GOOD;
                                            pkt->option_flags |= (uint32_t)OF_SSTS |
                                                    (uint32_t)OF_NO_DATA;
                                    }
                            }
                            break;
                        case RW_BUF_DESC:
#ifdef QL_DEBUG_LEVEL_3
                            qla1280_print("qla1280_atio_entry: SM_RDDB, RW_BUF_DESC\n\r");
#endif
                            if (len)
                            {
                                    if (len > 4)
                                        len = 4;

                                    ha->tbuf->hdr[0] = 0;
                                    if (pkt->cdb[2] != 0)
                                    {
                                        ha->tbuf->hdr[1] = 0;
                                        ha->tbuf->hdr[2] = 0;
                                        ha->tbuf->hdr[3] = 0;
                                    }
                                    else
                                    {
                                            ha->tbuf->hdr[1] =
                                                    (uint8_t)(TARGET_DATA_SIZE >> 16);
                                            ha->tbuf->hdr[2] =
                                                    (uint8_t)(TARGET_DATA_SIZE >> 8);
                                            ha->tbuf->hdr[3] =
                                                    (uint8_t)TARGET_DATA_SIZE;
                                    }
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_DATA_IN;
                            }
                            else
                            {
#ifdef QL_DEBUG_LEVEL_2
                                    qla1280_print("qla1280_atio_entry: SM_RDDB, zero length\n\r");
#endif
                                    pkt->scsi_status = S_GOOD;
                                    pkt->option_flags |= (uint32_t)OF_SSTS |
                                            (uint32_t)OF_NO_DATA;
                            }
                            break;
                        default:
#ifdef QL_DEBUG_LEVEL_2
                            qla1280_print("qla1280_atio_entry: SM_RDDB unknown mode\n\r");
#endif
                            *sense_ptr = 0x70;
                            *(sense_ptr+2) = SD_ILLREQ;
                            *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                            *(sense_ptr+12) = SC_ILLCDB;
                            len = 0;
                            pkt->scsi_status = S_CKCON;
                            pkt->option_flags |= (uint32_t)OF_SSTS |
                                    (uint32_t)OF_NO_DATA;
                            break;
                    }
                    break;
                default:
#ifdef QL_DEBUG_LEVEL_2
                    qla1280_print("qla1280_atio_entry: Unknown SCSI command\n\r");
                    qla1280_dump_buffer((caddr_t)&pkt->cdb[0], pkt->cdb_len);
#endif
                    bzero(sense_ptr, TARGET_SENSE_SIZE);
                    *sense_ptr = 0x70;
                    *(sense_ptr+2) = SD_ILLREQ;
                    *(sense_ptr+7) = TARGET_SENSE_SIZE-8;
                    *(sense_ptr+12) = SC_INVOPCODE;
                    len = 0;
                    pkt->scsi_status = S_CKCON;
                    pkt->option_flags |= (uint32_t)OF_SSTS |
                            (uint32_t)OF_NO_DATA;
                    break;
            }
            if (ha->flags.enable_64bit_addressing)
                qla1280_64bit_continue_io(ha, pkt, len, (paddr32_t *)&phy_addr);
            else
                qla1280_32bit_continue_io(ha, pkt, len, (paddr32_t *)&phy_addr);
            break;
        default:
            break;
    }

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_atio_entry: exiting normally\n\r");
#endif
}

/*
 *  qla1280_notify_entry
 *      Processes received ISP immediate notify entry.
 *
 * Input:
 *      ha  = adapter block pointer.
 *      pkt = entry pointer.
 */
STATIC void
qla1280_notify_entry(scsi_qla_host_t *ha, notify_entry_t *pkt)
{
#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_notify_entry: entered\n\r");
#endif

    /* Acknowledge immediate notify */
    qla1280_notify_ack(ha, pkt);

    /* Issue notify entry to increment resource count */
    qla1280_immed_notify(ha, pkt);

#ifdef QL_DEBUG_LEVEL_3
    qla1280_print("qla1280_notify_entry: exiting normally\n\r");
#endif
}

#endif  /* QLA1280_TARGET_MODE_SUPPORT */
/*
 *  qla1280_status_entry
 *      Processes received ISP status entry.
 *
 * Input:
 *      ha           = adapter block pointer.
 *      pkt          = entry pointer.
 *      done_q_first = done queue first pointer.
 *      done_q_last  = done queue last pointer.
 */
STATIC void
qla1280_status_entry(scsi_qla_host_t *ha, sts_entry_t *pkt, srb_t **done_q_first,
                     srb_t **done_q_last)
{
    uint32_t        b, t, l;
    uint8_t         sense_sz = 0;
    srb_t           *sp;
    scsi_lu_t       *q;
    Scsi_Cmnd       *cp;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_status_entry");
#endif

    /* Validate handle. */
    if (pkt->handle < MAX_OUTSTANDING_COMMANDS)
        sp = ha->outstanding_cmds[pkt->handle];
    else
        sp = 0;

    if (sp)
    {
        /* Free outstanding command slot. */
        ha->outstanding_cmds[pkt->handle] = 0;

        cp = sp->cmd;
        /* Generate LU queue on cntrl, target, LUN */
        b = SCSI_BUS_32(cp);
        t = SCSI_TCN_32(cp);
        l = SCSI_LUN_32(cp);
        q = LU_Q(ha, b, t, l);
        if( pkt->comp_status || pkt->scsi_status )
        {
            DEBUG(qla1280_print( "scsi: comp_status = ");)
            DEBUG(qla1280_output_number((uint32_t)pkt->comp_status,16);)
            DEBUG(qla1280_print( ", ");)
            DEBUG(qla1280_print( " scsi_status = ");)
            DEBUG(qla1280_output_number((uint32_t)pkt->scsi_status,16);)
            DEBUG(qla1280_print( "\n\r");)
            DEBUG(qla1280_print(", handle = ");)
            DEBUG(qla1280_output_number((uint32_t)pkt->handle, 16);)
            DEBUG(qla1280_print("\n\r");)
        }

        /* Target busy */
        if ( pkt->scsi_status & SS_BUSY_CONDITION &&
            pkt->scsi_status != SS_RESERVE_CONFLICT   )
        {
            CMD_RESULT(cp) = (int) (DID_BUS_BUSY << 16) |
                    (pkt->scsi_status & 0xff);
        }
        else
        {

            /* Save ISP completion status */
            CMD_RESULT(cp) = qla1280_return_status( pkt, cp );

            if (pkt->scsi_status & SS_CHECK_CONDITION)
            {
                BZERO(cp->sense_buffer, CMD_SNSLEN(cp));
                if (pkt->comp_status != CS_ARS_FAILED)
                {
                    if ( pkt->req_sense_length < CMD_SNSLEN(cp)  )
                        sense_sz = pkt->req_sense_length;
                    else
                        sense_sz = CMD_SNSLEN(cp) - 1;

                    BCOPY((caddr_t)&pkt->req_sense_data, cp->sense_buffer, sense_sz);

                }
#ifdef QL_DEBUG_LEVEL_2
                DEBUG(qla1280_print(
                        "qla1280_status_entry: Check condition Sense data, b");)
                DEBUG(qla1280_output_number((uint32_t)b, 10);)
                DEBUG(qla1280_print("t");)
                DEBUG(qla1280_output_number((uint32_t)t, 10);)
                DEBUG(qla1280_print("d");)
                DEBUG(qla1280_output_number((uint32_t)l, 10);)
                DEBUG(qla1280_print("\n\r");)
                DEBUG(if (sense_sz))
                    DEBUG(qla1280_dump_buffer(cp->sense_buffer, sense_sz);)
#endif
            }
        }
        /* Place command on done queue. */
        qla1280_done_q_put(sp, done_q_first, done_q_last);
    }
    else
    {
#ifdef QL_DEBUG_LEVEL_2
        qla1280_print("qla1280_status_entry: ISP Invalid handle\n\r");
#endif
        printk(KERN_WARNING "qla1280: Status Entry invalid handle\n");
        ha->flags.isp_abort_needed = TRUE;
    }
#ifdef QL_DEBUG_LEVEL_3
    LEAVE("qla1280_status_entry");
#endif
}

/*
 *  qla1280_error_entry
 *      Processes error entry.
 *
 * Input:
 *      ha           = adapter block pointer.
 *      pkt          = entry pointer.
 *      done_q_first = done queue first pointer.
 *      done_q_last  = done queue last pointer.
 */
STATIC void
qla1280_error_entry(scsi_qla_host_t *ha, response_t *pkt, srb_t **done_q_first,
                    srb_t **done_q_last)
{
    srb_t   *sp;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_error_entry");
#endif

#ifdef QL_DEBUG_LEVEL_2
    if (pkt->entry_status & BIT_3)
        qla1280_print("qla1280_error_entry: BAD PAYLOAD flag error\n\r");
    else if (pkt->entry_status & BIT_2)
        qla1280_print("qla1280_error_entry: BAD HEADER flag error\n\r");
    else if (pkt->entry_status & BIT_1)
        qla1280_print("qla1280_error_entry: FULL flag error\n\r");
    else
        qla1280_print("qla1280_error_entry: UNKNOWN flag error\n\r");
#endif

    /* Validate handle. */
    if (pkt->handle < MAX_OUTSTANDING_COMMANDS)
        sp = ha->outstanding_cmds[pkt->handle];
    else
        sp = 0;

    if (sp)
    {
        /* Free outstanding command slot. */
        ha->outstanding_cmds[pkt->handle] = 0;

        /* Bad payload or header */
        if (pkt->entry_status & (BIT_3 + BIT_2))
        {
            /* Bad payload or header, set error status. */
            /* CMD_RESULT(sp->cmd) = CS_BAD_PAYLOAD; */
            CMD_RESULT(sp->cmd) = (int) DID_ERROR << 16;
        }
        else if (pkt->entry_status & BIT_1 ) /* FULL flag */
        {
            CMD_RESULT(sp->cmd) = (int) DID_BUS_BUSY << 16;
        }
        else
        {
            /* Set error status. */
            CMD_RESULT(sp->cmd) =(int)  DID_ERROR << 16;
        }
        /* Place command on done queue. */
        qla1280_done_q_put(sp, done_q_first, done_q_last);
    }
#if  QLA1280_64BIT_SUPPORT
    else if (pkt->entry_type == COMMAND_A64_TYPE)
    {
#ifdef QL_DEBUG_LEVEL_2
        qla1280_print("qla1280_error_entry: ISP Invalid handle\n\r");
#endif
        printk(KERN_WARNING "!qla1280: Error Entry invalid handle");
        ha->flags.isp_abort_needed = TRUE;
    }
#endif

#ifdef QL_DEBUG_LEVEL_3
    LEAVE("qla1280_error_entry");
#endif
}

/*
 *  qla1280_abort_isp
 *      Resets ISP and aborts all outstanding commands.
 *
 * Input:
 *      ha           = adapter block pointer.
 *
 * Returns:
 *      0 = success
 */
STATIC uint8_t
qla1280_abort_isp(scsi_qla_host_t *ha)
{
    device_reg_t    *reg = ha->iobase;
    uint8_t         status = 0;
    uint16_t        cnt;
    srb_t           *sp;
    scsi_lu_t       *q;
    uint32_t        b, t, l;
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
    unsigned long cpu_flags = 0;
#endif

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_abort_isp");
#endif

    DRIVER_LOCK
            ha->flags.isp_abort_needed = FALSE;
    if (!ha->flags.abort_isp_active && ha->flags.online)
    {
        ha->flags.abort_isp_active = TRUE;

        /* Disable ISP interrupts. */
        WRT_REG_WORD(&reg->ictrl, 0);

        /* Dequeue all commands in outstanding command list. */
        for (cnt = 1; cnt < MAX_OUTSTANDING_COMMANDS; cnt++)
        {
            sp = ha->outstanding_cmds[cnt];
            if (sp)
            {
                ha->outstanding_cmds[cnt] = 0;

                /* Generate LU queue on controller, target, LUN */
                b = SCSI_BUS_32(sp->cmd);
                t = SCSI_TCN_32(sp->cmd);
                l = SCSI_LUN_32(sp->cmd);

                q = (scsi_lu_t       *)LU_Q(ha, b, t, l);

                /* Reset outstanding command count. */
                q->q_outcnt = 0;
                q->q_flag &= ~QLA1280_QBUSY;
                q->q_flag =  0;

                /* Adjust watchdog timer for command. */
                /* if (sp->flags & SRB_WATCHDOG)
                sp->timeout += 2; */

                /* Place request back on top of device queue. */
                /* sp->flags &= ~(SRB_SENT | SRB_TIMEOUT); */ 
                sp->flags = 0;
                qla1280_putq_t(q, sp);
            }
        }

        /* If firmware needs to be loaded */
        if (qla1280_isp_firmware(ha))
        {
            if (!(status = qla1280_chip_diag(ha)))
                status = qla1280_setup_chip(ha); 
        }

        if (!status)
        {
            /* Setup adapter based on NVRAM parameters. */
            qla1280_nvram_config(ha); 

            if (!(status = qla1280_init_rings(ha)))
            {
                /* Issue SCSI reset. */
                for (b = 0; b < ha->ports; b++)
                {
                    qla1280_bus_reset(ha, b);
                }
                do
                {
                    /* Issue marker command. */
                    ha->flags.reset_marker = FALSE;
                    for (b = 0; b < ha->ports; b++)
                    {
                        ha->bus_settings[b].reset_marker = FALSE;
                        qla1280_marker(ha, b, 0, 0, MK_SYNC_ALL);
                    }
                }while (ha->flags.reset_marker);

                /* Enable host adapter target mode. */
                for (b = 0; b < ha->ports; b++)
                {
                    if (!(status = qla1280_enable_tgt(ha, b)))
                    {
                        for (cnt = 0; cnt < MAX_LUNS; cnt++)
                        {
                            /* qla1280_enable_lun(ha, b, cnt); */
                            qla1280_poll(ha);
                        }
                    }
                    else
                        break;
                }

                if (!status)
                { 
                    /* Enable ISP interrupts. */
                    WRT_REG_WORD(&reg->ictrl, ISP_EN_INT + ISP_EN_RISC);
                    ha->flags.abort_isp_active = FALSE;
                    /* Restart queues that may have been stopped. */
                    qla1280_restart_queues(ha);
                }
            }
        }
    }

    if (status)
    {
        printk(KERN_WARNING
                "qla1280: ISP error recovery failed, board disabled");
        qla1280_reset_adapter(ha);
        qla1280_abort_queues(ha);

#if defined(QL_DEBUG_LEVEL_2) || defined(QL_DEBUG_LEVEL_3)
        qla1280_print("qla1280_abort_isp: **** FAILED ****\n\r");
#endif
    }
#ifdef QL_DEBUG_LEVEL_3
    else
        LEAVE("qla1280_abort_isp");
#endif
    DRIVER_UNLOCK

            return(status);
}

/*
 *  qla1280_restart_queues
 *      Restart all device queues.
 *
 * Input:
 *      ha = adapter block pointer.
 */
STATIC void
qla1280_restart_queues(scsi_qla_host_t *ha)
{
    scsi_lu_t *q;
    uint32_t  b, t, l;

#ifdef QL_DEBUG_LEVEL_3
    ENTER("qla1280_restart_queues");
#endif

    for (b = 0; b < ha->ports; b++)
        for (t = 0; t < MAX_TARGETS; t++)
            for (l = 0; l < MAX_LUNS; l++)
            {
                q = (scsi_lu_t *) LU_Q(ha, b, t, l);
                if (q != NULL)
                {
                    /* Acquire LU queue specific lock */
                    QLA1280_SCSILU_LOCK(q);

                    if (q->q_first)
                        qla1280_next(ha, q, b);
                    else
                        /* Release LU queue specific lock */
                        QLA1280_SCSILU_UNLOCK(q);
                }
            }
#ifdef QL_DEBUG_LEVEL_3
            qla1280_print("qla1280_restart_queues: exiting normally\n");
#endif
}

/*
 *  qla1280_abort_queue_single
 *      Abort all commands on a device queues.
 *
 * Input:
 *      ha = adapter block pointer.
 */
STATIC void qla1280_abort_queue_single(scsi_qla_host_t *ha,uint32_t b,uint32_t t,uint32_t l,uint32_t stat)
{
    scsi_lu_t *q;
    srb_t     *sp, *sp_next; 

    ENTER("qla1280_abort_queue_single");
    q = (scsi_lu_t * )LU_Q(ha, b, t, l);
    if (q != NULL)
    {
        /* Acquire LU queue specific lock */
        QLA1280_SCSILU_LOCK(q);

        sp = q->q_first;
        q->q_first = q->q_last = NULL;

        QLA1280_SCSILU_UNLOCK(q);

        while (sp)
        {
            sp_next = sp->s_next;
            CMD_RESULT(sp->cmd) = stat;
            qla1280_done_q_put(sp, (srb_t **)&ha->done_q_first, (srb_t **)&ha->done_q_last);
            sp = sp_next;
        }
    }
    LEAVE("qla1280_abort_queue_single");
}

/*
 *  qla1280_abort_queues
 *      Abort all commands on device queues.
 *
 * Input:
 *      ha = adapter block pointer.
 */
STATIC void
qla1280_abort_queues(scsi_qla_host_t *ha)
{
    uint32_t  b, t, l;

    ENTER("qla1280_abort_queues");

    for (b = 0; b < ha->ports; b++)
        for (t = 0; t < MAX_TARGETS; t++)
            for (l = 0; l < MAX_LUNS; l++)
                qla1280_abort_queue_single(ha,b,t,l,DID_RESET);

            LEAVE("qla1280_abort_queues");
}

/*
 * qla1280_debounce_register
 *      Debounce register.
 *
 * Input:
 *      port = register address.
 *
 * Returns:
 *      register value.
 */
STATIC uint16_t
qla1280_debounce_register(volatile uint16_t *addr)
{
    volatile uint16_t ret;
    volatile uint16_t ret2;

    do
    {
        ret = RD_REG_WORD(addr);
        ret2 = RD_REG_WORD(addr);
    }while (ret != ret2);

    return(ret);
}


/*
 * Declarations for load module
 */
static Scsi_Host_Template driver_template = QLA1280_LINUX_TEMPLATE;

#include "scsi_module.c"

/************************************************************************
 * qla1280_check_for_dead_scsi_bus                                      *
 *                                                                      *
 *    This routine checks for a dead SCSI bus                           *
 ************************************************************************/
#define SET_SXP_BANK            0x0100
#define SCSI_PHASE_INVALID      0x87FF
int  qla1280_check_for_dead_scsi_bus(scsi_qla_host_t *ha, srb_t *sp)
{
    uint16_t  config_reg, scsi_control;
    device_reg_t    *reg = ha->iobase;
    uint32_t  b;
    Scsi_Cmnd       *cp;

    /*
     * If SCSI Bus is Dead because of bad termination,
     * we will return a status of Selection timeout.
     */
 
     cp = sp->cmd;
     b = SCSI_BUS_32(cp);
    if (ha->bus_settings[b].scsi_bus_dead)
    {
        WRT_REG_WORD(&reg->host_cmd, HC_PAUSE_RISC);
        config_reg = RD_REG_WORD(&reg->cfg_1);
        WRT_REG_WORD(&reg->cfg_1,SET_SXP_BANK);
        scsi_control = RD_REG_WORD(&reg->scsiControlPins);
        WRT_REG_WORD(&reg->cfg_1,config_reg);
        WRT_REG_WORD(&reg->host_cmd, HC_RELEASE_RISC);

        if (scsi_control == SCSI_PHASE_INVALID)
        {
            CMD_RESULT(cp) = DID_NO_CONNECT << 16;
            CMD_HANDLE(cp) = (unsigned char *) 0;
            /* ha->actthreads--; */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,1,95)
            sti(); 
            (*(cp)->scsi_done)(cp);
            cli(); 
#else
            (*(cp)->scsi_done)(cp);
#endif
            return(TRUE);   /* bus is dead */
        }
        else
        {
            ha->bus_settings[b].scsi_bus_dead = FALSE;
            ha->bus_settings[b].failed_reset_count= 0; 
        }
    }
    return(FALSE);   /* bus is not dead */
}

STATIC uint8_t
qla12160_set_target_parameters(scsi_qla_host_t *ha, uint32_t b, uint32_t t, uint32_t l, nvram160_t *nv)
{
    uint16_t        mb[MAILBOX_REGISTER_COUNT];

    /* Set Target Parameters. */
    mb[0] = MBC_SET_TARGET_PARAMETERS;
    mb[1] = (uint16_t)(b ? t | BIT_7 :t);
    mb[1] <<= 8;
    mb[2] = nv->bus[b].target[t].parameter.c << 8;
    mb[2] |= TP_AUTO_REQUEST_SENSE;
    mb[2] &= ~TP_STOP_QUEUE;
    mb[2] |=  (nv->bus[b].target[t].flags.enable_ppr << 5);
    mb[3] = nv->bus[b].target[t].flags.sync_offset << 8;
    mb[3] |= nv->bus[b].target[t].sync_period;

    mb[6] = nv->bus[b].target[t].flags.ppr_options << 8;
    mb[6] |= nv->bus[b].target[t].flags.ppr_bus_width;
    return( qla1280_mailbox_command(ha, BIT_6|BIT_3|BIT_2|BIT_1|BIT_0, &mb[0]) ) ;
}

STATIC void
qla12160_get_target_parameters(scsi_qla_host_t *ha, uint32_t b, uint32_t t, uint32_t l)
{
    uint16_t        mb[MAILBOX_REGISTER_COUNT];

    mb[0] = MBC_GET_TARGET_PARAMETERS;
    mb[1] = (uint16_t)(b ? t | BIT_7 :t);
    mb[1] <<= 8;
    qla1280_mailbox_command(ha, BIT_6|BIT_3|BIT_2|BIT_1|BIT_0, &mb[0]);
    if( mb[3] != 0 )
    printk(KERN_INFO "scsi(%d:%d:%d:%d): Synchronous tranfer at period %d, offset %d. \n",
        (int)ha->host_no, b, t, l, (mb[3] &0xff), (mb[3] >> 8));

    if ( (mb[2] & BIT_5) &&  ((mb[6] >> 8) & 0xff) >= 2 )
            printk(KERN_INFO "scsi(%d:%d:%d:%d): Dual Transition enabled.\n",
                          (int)ha->host_no, b, t, l);
}


#ifdef QL_DEBUG_ROUTINES
/****************************************************************************/
/*                         Driver Debug Functions.                          */
/****************************************************************************/

/*
 *  Get byte from I/O port
 */
STATIC uint8_t
qla1280_getbyte(uint8_t *port)
{
    uint8_t ret;

#if MEMORY_MAPPED_IO
    ret = *port;
#else
    ret = inb((int)port);
#endif

    if (ql_debug_print)
    {
        qla1280_print("qla1280_getbyte: address = ");
        qla1280_output_number((uint32_t)port, 16);
        qla1280_print(" data = 0x");
        qla1280_output_number((uint32_t)ret, 16);
        qla1280_print("\n\r");
    }

    return(ret);
}

/*
 *  Get word from I/O port
 */
STATIC uint16_t
qla1280_getword(uint16_t *port)
{
    uint16_t ret;

#if MEMORY_MAPPED_IO
    ret = *port;
#else
    ret = inw((int)port);
#endif

    if (ql_debug_print)
    {
        qla1280_print("qla1280_getword: address = ");
        qla1280_output_number((uint32_t)port, 16);
        qla1280_print(" data = 0x");
        qla1280_output_number((uint32_t)ret, 16);
        qla1280_print("\n\r");
    }

    return(ret);
}

/*
 *  Get double word from I/O port
 */
STATIC uint32_t
qla1280_getdword(uint32_t *port)
{
    uint32_t ret;

#if MEMORY_MAPPED_IO
    ret = *port;
#else
    ret = inl((int)port);
#endif

    if (ql_debug_print)
    {
        qla1280_print("qla1280_getdword: address = ");
        qla1280_output_number((uint32_t)port, 16);
        qla1280_print(" data = 0x");
        qla1280_output_number((uint32_t)ret, 16);
        qla1280_print("\n\r");
    }

    return(ret);
}

/*
 *  Send byte to I/O port
 */
STATIC void
qla1280_putbyte(uint8_t *port, uint8_t data)
{
#if MEMORY_MAPPED_IO
    *port = data;
#else
    outb(data, (int)port);
#endif

    if (ql_debug_print)
    {
        qla1280_print("qla1280_putbyte: address = ");
        qla1280_output_number((uint32_t)port, 16);
        qla1280_print(" data = 0x");
        qla1280_output_number((uint32_t)data, 16);
        qla1280_print("\n\r");
    }
}

/*
 *  Send word to I/O port
 */
STATIC void
qla1280_putword(uint16_t *port, uint16_t data)
{
#if MEMORY_MAPPED_IO
    *port = data;
#else
#ifdef _LINUX_IOPORTS
    outw(data, (int)port);
#else
    outw((int)port, data);
#endif
#endif

    if (ql_debug_print)
    {
        qla1280_print("qla1280_putword: address = ");
        qla1280_output_number((uint32_t)port, 16);
        qla1280_print(" data = 0x");
        qla1280_output_number((uint32_t)data, 16);
        qla1280_print("\n\r");
    }
}

/*
 *  Send double word to I/O port
 */
STATIC void
qla1280_putdword(uint32_t *port, uint32_t data)
{
#if MEMORY_MAPPED_IO
    *port = data;
#else
#ifdef _LINUX_IOPORTS
    outl(data,(int)port);
#else
    outl((int)port, data);
#endif
#endif

    if (ql_debug_print)
    {
        qla1280_print("qla1280_putdword: address = ");
        qla1280_output_number((uint32_t)port, 16);
        qla1280_print(" data = 0x");
        qla1280_output_number((uint32_t)data, 16);
        qla1280_print("\n\r");
    }
}

/*
 * Dummy function to prevent warnings for
 * declared and unused debug functions
 */
void
qla1280_debug(void)
{
    qla1280_getbyte(0);
    qla1280_getword(0);
    qla1280_getdword(0);
    qla1280_putbyte(0, 0);
    qla1280_putword(0, 0);
    qla1280_putdword(0, 0);
}

/*
 *  Out character to COM2 port.
 *      PORT must be at standard address for COM2 = 0x2F8,
 *      or COM1 = 0x3F8
 */
#define OUTB(addr,data)   outb((data),(addr))

STATIC void
qla1280_putc(uint8_t c)
{
#ifdef QL_DEBUG_CONSOLE
    printk("%c", c);
#else
    int     com_addr              = 0x2f8;
    int     hardware_flow_control = 1;
    int     software_flow_control = 0;
    uint8_t data;

    /* Wait for transmitter holding and shift registers for empty. */
    do
    {
        data = inb(com_addr+5);
    }while (!(data & BIT_6));

    /*
    * Set BAUD rate for COM2 to 19200 (0x6)
    */

    /* Select rate divisor. */
    OUTB(com_addr+3, 0x83); 

    /* BAUD rate divisor LSB. */
    OUTB(com_addr, 0xc);                    /* 0xC = 9600 baud */

    /* BAUD rate divisor MSB. */
    OUTB(com_addr+1, 0);

    /* Set No parity, 8 bits, 1 stop bit and
    select interrupt enable register. */
    OUTB(com_addr+3, 3);

    /* Disable interrupts. */
    OUTB(com_addr+1, 0);

    /* Set data terminal ready and request to send */
    OUTB(com_addr+4,3);

    if (hardware_flow_control)
    {
        /* Wait for clear-to-send and data-set-ready */
        do
        {
            data = inb(com_addr+6) & (BIT_5 + BIT_4);
        }while (data != (BIT_5 + BIT_4));
    }
    else if (software_flow_control)
    {
        /* Test for data ready. */
        data = inb(com_addr+5);
        if (data & BIT_0)
        {
            /* If XOFF */
            data = inb(com_addr);
            if (data == '\023')
            {
                /* Wait for XON */
                do
                {
                    /* Wait for char */
                    do
                    {
                        data = inb(com_addr+5);
                    }while (!(data & BIT_0));
                    data = inb(com_addr);
                }while (data != '\021');
            }
        }
    }

    /* Output character. */
    OUTB(com_addr, c);
#endif
}

/*
 *  Out NULL terminated string to COM port.
 */
STATIC void
qla1280_print(caddr_t s)
{
    if (ql_debug_print)
    {
#ifdef QL_DEBUG_CONSOLE
        printk("%s",s);
#else
        /* Output string. */
        while (*s)
            qla1280_putc(*s++);
#endif
    }
}

/*
 *  Output long number to COM port.
 */
STATIC void
qla1280_output_number(uint32_t n, uint8_t base)
{
    int8_t str[12];
    int8_t *s     = &str[11];
    int8_t output = 0;
    int8_t hex    = FALSE;

    if (ql_debug_print)
    {
        if (base == 10 || base == 16)
        {
            if (base == 16 && n > 9)
                hex = TRUE;

            *s = 0;
            do
            {
                s--;
                *s = n % base;
                if (*s > 9)
                    *s += 55;
                else
                    *s += '0';
                n /= base;
            }while (n);

            for (; *s; s++)
            {
                if (*s != '0')
                    output = 1;
                if (output)
                    qla1280_putc(*s);
            }
            if (!output)
                qla1280_putc(*--s);

            if (hex)
                qla1280_putc('h');
        }
    }
}

STATIC void
qla1280_dump_buffer(caddr_t b, uint32_t size)
{
    uint32_t cnt;
    uint8_t c;

    if (ql_debug_print)
    {
        qla1280_print(
                " 0   1   2   3   4   5   6   7   8   9   Ah  Bh  Ch  Dh  Eh  Fh\n\r");
        qla1280_print(
                "---------------------------------------------------------------\n\r");

        for (cnt = 0; cnt < size; )
        {
            c = *b++;
            if (c < 16)
                qla1280_putc(' ');
            qla1280_output_number((uint32_t)c, 16);
            cnt++;
            if (!(cnt % 16))
                qla1280_print("\n\r");
            else if (c < 10)
                qla1280_print("  ");
            else
                qla1280_putc(' ');
        }
        if (cnt % 16)
            qla1280_print("\n\r");
    }
}
/**************************************************************************
 *   ql1280_print_scsi_cmd
 *
 **************************************************************************/
void qla1280_print_scsi_cmd(Scsi_Cmnd *cmd)
{
    scsi_qla_host_t *ha;
    struct Scsi_Host  *host = cmd->host;
    srb_t           *sp;
   /* struct scatterlist *sg; */

    int i;
    ha = (scsi_qla_host_t *) host->hostdata;

    ql_debug_print = 1;
    sp = (srb_t *) CMD_SP(cmd);
    sprintf(debug_buff,"SCSI Command @= 0x%p, Handle=0x%p\n\r", cmd, CMD_HANDLE(cmd));
    qla1280_print(debug_buff);
    sprintf(debug_buff,"  chan=%d, target = 0x%02x, lun = 0x%02x, cmd_len = 0x%02x\n\r",
            cmd->channel, cmd->target, cmd->lun, cmd->cmd_len);
    qla1280_print(debug_buff);
    qla1280_print(" CDB = ");
    for (i = 0; i < cmd->cmd_len; i++)
    {
        sprintf(debug_buff,"0x%02x ", cmd->cmnd[i]);
        qla1280_print(debug_buff);
    }
    sprintf(debug_buff,"  seg_cnt =%d\n\r",cmd->use_sg);
    qla1280_print(debug_buff);
    sprintf(debug_buff,"  request buffer=0x%p, request buffer len=0x%x\n\r",cmd->request_buffer,cmd->request_bufflen);
    qla1280_print(debug_buff);
    /* if( cmd->use_sg )
    {
       sg = (struct scatterlist *) cmd->request_buffer;
       qla1280_print("  SG buffer: \n\r");
       qla1280_dump_buffer((caddr_t)sg, (cmd->use_sg*sizeof(struct scatterlist)) );
    } */
    sprintf(debug_buff,"  tag=%d, flags=0x%x, transfersize=0x%x \n\r", 
            cmd->tag, cmd->flags,cmd->transfersize );
    qla1280_print(debug_buff);
    sprintf(debug_buff,"  Pid=%d, SP=0x%p\n\r", (int)cmd->pid, CMD_SP(cmd));
    qla1280_print(debug_buff);
    sprintf(debug_buff,"  r_start=0x%lx, u_start=0x%lx\n\r",sp->r_start,sp->u_start);
    qla1280_print(debug_buff);
    sprintf(debug_buff," underflow size = 0x%x, direction=0x%x, req.cmd=0x%x \n\r", cmd->underflow, sp->dir,cmd->request.cmd);    
    qla1280_print(debug_buff);
}
/**************************************************************************
 *   ql1280_dump_device
 *
 **************************************************************************/
void
ql1280_dump_device(scsi_qla_host_t *ha)
{

    Scsi_Cmnd       *cp;
    srb_t           *sp;
    int i;
    qla1280_print("Outstanding Commands on controller:\n\r");   
    for ( i=0; i < MAX_OUTSTANDING_COMMANDS; i++ )
    {
        if( (sp = ha->outstanding_cmds[i]) == NULL )
            continue;
        if( (cp = sp->cmd) == NULL )
            continue;
        qla1280_print_scsi_cmd(cp);
    }

}
#endif

#ifdef  QLA1280_UNUSED 
/**************************************************************************
 *   ql1280_dump_regs
 *
 **************************************************************************/
static void qla1280_dump_regs(struct Scsi_Host *host)
{
    printk("Mailbox registers:\n");
    printk("qla1280 : mbox 0 0x%04x \n", inw(host->io_port + 0x70));
    printk("qla1280 : mbox 1 0x%04x \n", inw(host->io_port + 0x72));
    printk("qla1280 : mbox 2 0x%04x \n", inw(host->io_port + 0x74));
    printk("qla1280 : mbox 3 0x%04x \n", inw(host->io_port + 0x76));
    printk("qla1280 : mbox 4 0x%04x \n", inw(host->io_port + 0x78));
    printk("qla1280 : mbox 5 0x%04x \n", inw(host->io_port + 0x7a));
}
#endif



#if  STOP_ON_ERROR 
/**************************************************************************
 *   ql1280_panic
 *
 **************************************************************************/
static void qla1280_panic(char *cp, struct Scsi_Host *host)
{
    scsi_qla_host_t *ha;
    long  *fp;

    ha = (scsi_qla_host_t *) host->hostdata;
    printk("qla1280 - PANIC:  %s\n",cp);
    printk("Current time=0x%lx\n", jiffies);
    printk("Number of pending commands =0x%lx\n", ha->actthreads);
    printk("Number of SCSI queued commands =0x%lx\n", ha->qthreads);
    printk("Number of free entries = (%d)\n",ha->req_q_cnt);
    printk("Request Queue @ 0x%lx, Response Queue @ 0x%lx\n",
                        ha->request_dma,
                        ha->response_dma);
    printk("Request In Ptr %d\n", ha->req_ring_index );
    fp = (long *) &ha->flags;
    printk("HA flags =0x%lx\n", *fp);
    DEBUG2(ql_debug_print = 1;)
    /* DEBUG2(ql1280_dump_device((scsi_qla_host_t *) host->hostdata)); */
#ifdef  QLA1280_UNUSED 
    qla1280_dump_regs(host);
#endif
    sti();  
    panic("Ooops");  
    /* cli(); 
    for(;;)
    { 
        barrier();
    sti();  
    }
    */
}
#endif

#ifdef  QLA1280_UNUSED 
static void qla1280_set_flags(char * s)
{
}
#endif

/**************************************************************************
 *   qla1280_setup
 *
 *   Handle Linux boot parameters. This routine allows for assigning a value
 *   to a parameter with a ':' between the parameter and the value.
 *   ie. qla1280=max_reqs:0x0A,verbose
 **************************************************************************/
void
qla1280_setup(char *s, int *dummy)
{
    char *end, *str, *cp;

#ifdef  QLA1280_UNUSED 
    static struct
    {
        const char *name;
        int      siz;
        void  (*func)();
        int   arg;
    } options[] =
    {
        { "dump_regs", 9,  &qla1280_dump_regs, 0 
        },
        { "verbose", 7, &qla1280_set_flags, 0x1 
        },
        { "",    0, NULL, 0 
        }
    };
#endif

    printk("scsi: Processing Option str = %s\n", s);
    end = strchr(s, '\0');
    /* locate command */
    str = s;
    for( cp = s; *cp && cp != end; cp++ ) 
    {
       cp = qla1280_get_token(cp, str);
       printk("scsi: token str = %s\n", str);
       /* if found execute routine */

    }
    
}

static char	*qla1280_get_token(char *cmdline, char *str )
{
    register	char 	*cp = cmdline;

        /* skip preceeding spaces */
        while(strchr(cp,' '))
            ++cp;
        /* symbol starts here */
        str = cp;
        /* skip char if not a space or : */
        while (*cp && !( strchr(cp,' ') || strchr(cp,':'))  )
            cp++;
        *cp = '\0';
        return( cp );
}

/*
 * Overrides for Emacs so that we almost follow Linus's tabbing style.
 * Emacs will notice this stuff at the end of the file and automatically
 * adjust the settings for this buffer only.  This must remain at the end
 * of the file.
 * ---------------------------------------------------------------------------
 * Local variables:
 * c-indent-level: 2
 * c-brace-imaginary-offset: 0
 * c-brace-offset: -2
 * c-argdecl-indent: 2
 * c-label-offset: -2
 * c-continued-statement-offset: 2
 * c-continued-brace-offset: 0
 * indent-tabs-mode: nil
 * tab-width: 8
 * End:
 */

