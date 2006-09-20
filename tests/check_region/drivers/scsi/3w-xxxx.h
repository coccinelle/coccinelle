/* 
   3w-xxxx.h -- 3ware Storage Controller device driver for Linux.
   
   Written By: Adam Radford <linux@3ware.com>
   Modifications By: Joel Jacobson <linux@3ware.com>

   Copyright (C) 1999 3ware Inc.

   Kernel compatablity By:	Andre Hedrick <andre@suse.com>
   Non-Copyright (C) 2000	Andre Hedrick <andre@suse.com>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; version 2 of the License.

   This program is distributed in the hope that it will be useful,           
   but WITHOUT ANY WARRANTY; without even the implied warranty of            
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             
   GNU General Public License for more details.                              

   NO WARRANTY                                                               
   THE PROGRAM IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OR        
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED INCLUDING, WITHOUT      
   LIMITATION, ANY WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT,      
   MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Each Recipient is    
   solely responsible for determining the appropriateness of using and       
   distributing the Program and assumes all risks associated with its        
   exercise of rights under this Agreement, including but not limited to     
   the risks and costs of program errors, damage to or loss of data,         
   programs or equipment, and unavailability or interruption of operations.  

   DISCLAIMER OF LIABILITY                                                   
   NEITHER RECIPIENT NOR ANY CONTRIBUTORS SHALL HAVE ANY LIABILITY FOR ANY   
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL        
   DAMAGES (INCLUDING WITHOUT LIMITATION LOST PROFITS), HOWEVER CAUSED AND   
   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR     
   TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE    
   USE OR DISTRIBUTION OF THE PROGRAM OR THE EXERCISE OF ANY RIGHTS GRANTED  
   HEREUNDER, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGES             

   You should have received a copy of the GNU General Public License         
   along with this program; if not, write to the Free Software               
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

   Bugs/Comments/Suggestions should be mailed to:                            
   linux@3ware.com
   
   For more information, goto:
   http://www.3ware.com
*/

#ifndef _3W_XXXX_H
#define _3W_XXXX_H

#include <linux/version.h>
#include <linux/types.h>
#include <linux/kdev_t.h>

/* Control register bit definitions */
#define TW_CONTROL_CLEAR_HOST_INTERRUPT	       0x00080000
#define TW_CONTROL_CLEAR_ATTENTION_INTERRUPT   0x00040000
#define TW_CONTROL_MASK_COMMAND_INTERRUPT      0x00020000
#define TW_CONTROL_MASK_RESPONSE_INTERRUPT     0x00010000
#define TW_CONTROL_UNMASK_COMMAND_INTERRUPT    0x00008000
#define TW_CONTROL_UNMASK_RESPONSE_INTERRUPT   0x00004000
#define TW_CONTROL_CLEAR_ERROR_STATUS	       0x00000200
#define TW_CONTROL_ISSUE_SOFT_RESET	       0x00000100
#define TW_CONTROL_ENABLE_INTERRUPTS	       0x00000080
#define TW_CONTROL_DISABLE_INTERRUPTS	       0x00000040
#define TW_CONTROL_ISSUE_HOST_INTERRUPT	       0x00000020

/* Status register bit definitions */
#define TW_STATUS_MAJOR_VERSION_MASK	       0xF0000000
#define TW_STATUS_MINOR_VERSION_MASK	       0x0F000000
#define TW_STATUS_PCI_PARITY_ERROR	       0x00800000
#define TW_STATUS_QUEUE_ERROR		       0x00400000
#define TW_STATUS_MICROCONTROLLER_ERROR	       0x00200000
#define TW_STATUS_PCI_ABORT		       0x00100000
#define TW_STATUS_HOST_INTERRUPT	       0x00080000
#define TW_STATUS_ATTENTION_INTERRUPT	       0x00040000
#define TW_STATUS_COMMAND_INTERRUPT	       0x00020000
#define TW_STATUS_RESPONSE_INTERRUPT	       0x00010000
#define TW_STATUS_COMMAND_QUEUE_FULL	       0x00008000
#define TW_STATUS_RESPONSE_QUEUE_EMPTY	       0x00004000
#define TW_STATUS_MICROCONTROLLER_READY	       0x00002000
#define TW_STATUS_COMMAND_QUEUE_EMPTY	       0x00001000
#define TW_STATUS_ALL_INTERRUPTS	       0x000F0000
#define TW_STATUS_CLEARABLE_BITS	       0x00D00000
#define TW_STATUS_EXPECTED_BITS		       0x00002000
#define TW_STATUS_UNEXPECTED_BITS	       0x00F80000

/* RESPONSE QUEUE BIT DEFINITIONS */
#define TW_RESPONSE_ID_MASK		       0x00000FF0

/* PCI related defines */
#define TW_IO_ADDRESS_RANGE		       0xD
#define TW_DEVICE_NAME			       "3ware Storage Controller"
#define TW_VENDOR_ID (0x13C1)	/* 3ware */
#define TW_DEVICE_ID (0x1000)	/* Storage Controller */

/* Command packet opcodes */
#define TW_OP_NOP	      0x0
#define TW_OP_INIT_CONNECTION 0x1
#define TW_OP_READ	      0x2
#define TW_OP_WRITE	      0x3
#define TW_OP_VERIFY	      0x4
#define TW_OP_GET_PARAM	      0x12
#define TW_OP_SET_PARAM	      0x13
#define TW_OP_SECTOR_INFO     0x1a
#define TW_OP_AEN_LISTEN      0x1c
#define TW_CMD_PACKET         0x1d

/* Asynchronous Event Notification (AEN) Codes */
#define TW_AEN_QUEUE_EMPTY       0x0000
#define TW_AEN_SOFT_RESET        0x0001
#define TW_AEN_DEGRADED_MIRROR   0x0002
#define TW_AEN_CONTROLLER_ERROR  0x0003
#define TW_AEN_REBUILD_FAIL      0x0004
#define TW_AEN_REBUILD_DONE      0x0005
#define TW_AEN_QUEUE_FULL        0x00ff
#define TW_AEN_TABLE_UNDEFINED   0x15

/* Misc defines */
#define TW_ALIGNMENT			      0x200 /* 16 D-WORDS */
#define TW_MAX_UNITS			      16
#define TW_COMMAND_ALIGNMENT_MASK	      0x1ff
#define TW_INIT_MESSAGE_CREDITS		      0x100
#define TW_INIT_COMMAND_PACKET_SIZE	      0x3
#define TW_POLL_MAX_RETRIES        	      10000
#define TW_MAX_SGL_LENGTH		      62
#define TW_Q_LENGTH			      256
#define TW_Q_START			      0
#define TW_MAX_SLOT			      32
#define TW_MAX_PCI_BUSES		      255
#define TW_MAX_RESET_TRIES		      3
#define TW_UNIT_INFORMATION_TABLE_BASE	      0x300
#define TW_MAX_CMDS_PER_LUN		      (TW_Q_LENGTH-2)/TW_MAX_UNITS
#define TW_BLOCK_SIZE			      0x200 /* 512-byte blocks */
#define TW_IOCTL                              0x80
#define TW_MAX_AEN_TRIES                      100
#define TW_UNIT_ONLINE                        1
#define TW_IN_INTR                            1

/* Macros */
#define TW_STATUS_ERRORS(x) \
	(((x & TW_STATUS_PCI_ABORT) || \
	(x & TW_STATUS_PCI_PARITY_ERROR) || \
	(x & TW_STATUS_QUEUE_ERROR) || \
	(x & TW_STATUS_MICROCONTROLLER_ERROR)) && \
	(x & TW_STATUS_MICROCONTROLLER_READY))

#ifdef TW_DEBUG
#define dprintk(msg...) printk(msg)
#else
#define dprintk(msg...) do { } while(0);
#endif

/* Scatter Gather List Entry */
typedef struct TAG_TW_SG_Entry {
	unsigned long address;
	unsigned long length;
} TW_SG_Entry;

typedef unsigned char TW_Sector[512];

/* Command Packet */
typedef struct TW_Command {
	/* First DWORD */
	struct {
		unsigned char opcode:5;
		unsigned char sgl_offset:3;
	} byte0;
	unsigned char size;
	unsigned char request_id;
	struct {
		unsigned char unit:4;
		unsigned char host_id:4;
	} byte3;
	/* Second DWORD */
	unsigned char status;
	unsigned char flags;
	union {
		unsigned short block_count;
		unsigned short parameter_count;
		unsigned short message_credits;
	} byte6;
	union {
		struct {
			unsigned long lba;
			TW_SG_Entry sgl[TW_MAX_SGL_LENGTH];
			unsigned long padding;	/* pad to 512 bytes */
		} io;
		struct {
			TW_SG_Entry sgl[TW_MAX_SGL_LENGTH];
			unsigned long padding[2];
		} param;
		struct {
			unsigned long response_queue_pointer;
			unsigned long padding[125];
		} init_connection;
		struct {
			char version[504];
		} ioctl_miniport_version;
	} byte8;
} TW_Command;

typedef struct TAG_TW_Ioctl {
	int buffer;
	unsigned char opcode;
	unsigned short table_id;
	unsigned char parameter_id;
	unsigned char parameter_size_bytes;
	unsigned char data[1];
} TW_Ioctl;

/* GetParam descriptor */
typedef struct {
	unsigned short	table_id;
	unsigned char	parameter_id;
	unsigned char	parameter_size_bytes;
	unsigned char	data[1];
} TW_Param, *PTW_Param;

/* Response queue */
typedef union TAG_TW_Response_Queue {
	struct {
		u32 undefined_1: 4;
		u32 response_id: 8;
		u32 undefined_2: 20;
	} u;
	u32 value;
} TW_Response_Queue;

typedef struct TAG_TW_Registers {
	u32 base_addr;
	u32 control_reg_addr;
	u32 status_reg_addr;
	u32 command_que_addr;
	u32 response_que_addr;
} TW_Registers;

typedef struct TAG_TW_Info {
	char *buffer;
	int length;
	int offset;
	int position;
} TW_Info;

typedef enum TAG_TW_Cmd_State {
	TW_S_INITIAL,		/* Initial state */
	TW_S_STARTED,		/* Id in use */
	TW_S_POSTED,		/* Posted to the controller */
	TW_S_PENDING,		/* Waiting to be posted in isr */
	TW_S_COMPLETED,		/* Completed by isr */
	TW_S_FINISHED,		/* I/O completely done */
} TW_Cmd_State;

typedef struct TAG_TW_Device_Extension {
	TW_Registers		registers;
	u32			*alignment_virtual_address[TW_Q_LENGTH];
	u32			alignment_physical_address[TW_Q_LENGTH];
	int			is_unit_present[TW_MAX_UNITS];
	int			num_units;
	u32			*command_packet_virtual_address[TW_Q_LENGTH];
	u32			command_packet_physical_address[TW_Q_LENGTH];
	struct pci_dev		*tw_pci_dev;
	Scsi_Cmnd		*srb[TW_Q_LENGTH];
	unsigned char		free_queue[TW_Q_LENGTH];
	unsigned char		free_head;
	unsigned char		free_tail;
	unsigned char		pending_queue[TW_Q_LENGTH];
	unsigned char		pending_head;
	unsigned char		pending_tail;
	TW_Cmd_State		state[TW_Q_LENGTH];
	u32			posted_request_count;
	u32			max_posted_request_count;
	u32			request_count_marked_pending;
	u32			pending_request_count;
	u32			max_pending_request_count;
	u32			max_sgl_entries;
	u32			sgl_entries;
	u32			num_aborts;
	u32			num_resets;
	u32			sector_count;
	u32			max_sector_count;
	struct Scsi_Host	*host;
	spinlock_t		tw_lock;
	unsigned char		ioctl_size[TW_Q_LENGTH];
	unsigned short		aen_queue[TW_Q_LENGTH];
	unsigned char		aen_head;
	unsigned char		aen_tail;
	u32			flags;
} TW_Device_Extension;

/* Function prototypes */
int tw_aen_complete(TW_Device_Extension *tw_dev, int request_id);
int tw_aen_drain_queue(TW_Device_Extension *tw_dev);
int tw_aen_read_queue(TW_Device_Extension *tw_dev, int request_id);
int tw_allocate_memory(TW_Device_Extension *tw_dev, int request_id, int size, int which);
int tw_check_bits(u32 status_reg_value);
int tw_check_errors(TW_Device_Extension *tw_dev);
void tw_clear_attention_interrupt(TW_Device_Extension *tw_dev);
void tw_clear_host_interrupt(TW_Device_Extension *tw_dev);
void tw_disable_interrupts(TW_Device_Extension *tw_dev);
int tw_empty_response_que(TW_Device_Extension *tw_dev);
void tw_enable_interrupts(TW_Device_Extension *tw_dev);
int tw_findcards(Scsi_Host_Template *tw_host);
void tw_free_device_extension(TW_Device_Extension *tw_dev);
int tw_initconnection(TW_Device_Extension *tw_dev, int message_credits);
int tw_initialize_device_extension(TW_Device_Extension *tw_dev);
int tw_initialize_units(TW_Device_Extension *tw_dev);
int tw_ioctl(TW_Device_Extension *tw_dev, int request_id);
int tw_ioctl_complete(TW_Device_Extension *tw_dev, int request_id);
void tw_mask_command_interrupt(TW_Device_Extension *tw_dev);
int tw_poll_status(TW_Device_Extension *tw_dev, u32 flag, int seconds);
int tw_post_command_packet(TW_Device_Extension *tw_dev, int request_id);
int tw_reset_device_extension(TW_Device_Extension *tw_dev);
int tw_reset_sequence(TW_Device_Extension *tw_dev);
int tw_scsi_biosparam(Disk *disk, kdev_t dev, int geom[]);
int tw_scsi_detect(Scsi_Host_Template *tw_host);
int tw_scsi_eh_abort(Scsi_Cmnd *SCpnt);
int tw_scsi_eh_reset(Scsi_Cmnd *SCpnt);
int tw_scsi_proc_info(char *buffer, char **start, off_t offset, int length, int inode, int inout);
int tw_scsi_queue(Scsi_Cmnd *cmd, void (*done) (Scsi_Cmnd *));
int tw_scsi_release(struct Scsi_Host *tw_host);
int tw_scsiop_inquiry(TW_Device_Extension *tw_dev, int request_id);
int tw_scsiop_inquiry_complete(TW_Device_Extension *tw_dev, int request_id);
int tw_scsiop_read_capacity(TW_Device_Extension *tw_dev, int request_id);
int tw_scsiop_read_capacity_complete(TW_Device_Extension *tw_dev, int request_id);
int tw_scsiop_read_write(TW_Device_Extension *tw_dev, int request_id);
int tw_scsiop_test_unit_ready(TW_Device_Extension *tw_dev, int request_id);
int tw_setfeature(TW_Device_Extension *tw_dev, int parm, int param_size, 
		  unsigned char *val);
int tw_setup_irq(TW_Device_Extension *tw_dev);
int tw_shutdown_device(TW_Device_Extension *tw_dev);
void tw_soft_reset(TW_Device_Extension *tw_dev);
int tw_state_request_finish(TW_Device_Extension *tw_dev,int request_id);
int tw_state_request_start(TW_Device_Extension *tw_dev, int *request_id);
void tw_unmask_command_interrupt(TW_Device_Extension *tw_dev);

/* Scsi_Host_Template Initializer */
#define TWXXXX {					\
	next : NULL,					\
	module : NULL,					\
	proc_name : "3w-xxxx",				\
	proc_info : tw_scsi_proc_info,			\
	name : "3ware Storage Controller",		\
	detect : tw_scsi_detect,			\
	release : tw_scsi_release,			\
	info : NULL,					\
	ioctl : NULL,                  			\
	command : NULL,					\
	queuecommand : tw_scsi_queue,			\
	eh_strategy_handler : NULL,			\
	eh_abort_handler : tw_scsi_eh_abort,		\
	eh_device_reset_handler : NULL,			\
	eh_bus_reset_handler : NULL,			\
	eh_host_reset_handler : tw_scsi_eh_reset,	\
	abort : NULL,					\
	reset : NULL,					\
	slave_attach : NULL,				\
	bios_param : tw_scsi_biosparam,			\
	can_queue : TW_Q_LENGTH,			\
	this_id: -1,					\
	sg_tablesize : TW_MAX_SGL_LENGTH,		\
	cmd_per_lun: TW_MAX_CMDS_PER_LUN,		\
	present : 0,					\
	unchecked_isa_dma : 0,				\
	use_clustering : ENABLE_CLUSTERING,		\
 	use_new_eh_code : 1,				\
	emulated : 1					\
}
#endif /* _3W_XXXX_H */
