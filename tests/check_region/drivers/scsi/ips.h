/*****************************************************************************/
/* ips.h -- driver for the IBM ServeRAID controller                          */
/*                                                                           */
/* Written By: Keith Mitchell, IBM Corporation                               */
/*                                                                           */
/* Copyright (C) 1999 IBM Corporation                                        */
/*                                                                           */
/* This program is free software; you can redistribute it and/or modify      */
/* it under the terms of the GNU General Public License as published by      */
/* the Free Software Foundation; either version 2 of the License, or         */
/* (at your option) any later version.                                       */
/*                                                                           */
/* This program is distributed in the hope that it will be useful,           */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of            */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             */
/* GNU General Public License for more details.                              */
/*                                                                           */
/* NO WARRANTY                                                               */
/* THE PROGRAM IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OR        */
/* CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED INCLUDING, WITHOUT      */
/* LIMITATION, ANY WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT,      */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Each Recipient is    */
/* solely responsible for determining the appropriateness of using and       */
/* distributing the Program and assumes all risks associated with its        */
/* exercise of rights under this Agreement, including but not limited to     */
/* the risks and costs of program errors, damage to or loss of data,         */
/* programs or equipment, and unavailability or interruption of operations.  */
/*                                                                           */
/* DISCLAIMER OF LIABILITY                                                   */
/* NEITHER RECIPIENT NOR ANY CONTRIBUTORS SHALL HAVE ANY LIABILITY FOR ANY   */
/* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL        */
/* DAMAGES (INCLUDING WITHOUT LIMITATION LOST PROFITS), HOWEVER CAUSED AND   */
/* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR     */
/* TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE    */
/* USE OR DISTRIBUTION OF THE PROGRAM OR THE EXERCISE OF ANY RIGHTS GRANTED  */
/* HEREUNDER, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGES             */
/*                                                                           */
/* You should have received a copy of the GNU General Public License         */
/* along with this program; if not, write to the Free Software               */
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */
/*                                                                           */
/* Bugs/Comments/Suggestions should be mailed to:                            */
/*      ipslinux@us.ibm.com                                                  */
/*                                                                           */
/*****************************************************************************/

#ifndef _IPS_H_
   #define _IPS_H_

   #include <asm/uaccess.h>
   #include <asm/io.h>

   /* Prototypes */
   extern int ips_detect(Scsi_Host_Template *);
   extern int ips_release(struct Scsi_Host *);
   extern int ips_eh_abort(Scsi_Cmnd *);
   extern int ips_eh_reset(Scsi_Cmnd *);
   extern int ips_queue(Scsi_Cmnd *, void (*) (Scsi_Cmnd *));
   extern int ips_biosparam(Disk *, kdev_t, int *);
   extern const char * ips_info(struct Scsi_Host *);
   extern void do_ipsintr(int, void *, struct pt_regs *);

   /*
    * Some handy macros
    */
   #ifndef LinuxVersionCode
      #define LinuxVersionCode(x,y,z)  (((x)<<16)+((y)<<8)+(z))
   #endif

   #define IPS_HA(x)                   ((ips_ha_t *) x->hostdata)
   #define IPS_COMMAND_ID(ha, scb)     (int) (scb - ha->scbs)
   #define IPS_IS_TROMBONE(ha)         (((ha->device_id == IPS_COPPERHEAD_DEVICEID) && \
                                         (ha->revision_id >= IPS_REVID_TROMBONE32) && \
                                         (ha->revision_id <= IPS_REVID_TROMBONE64)) ? 1 : 0)
   #define IPS_IS_CLARINET(ha)         (((ha->device_id == IPS_COPPERHEAD_DEVICEID) && \
                                         (ha->revision_id >= IPS_REVID_CLARINETP1) && \
                                         (ha->revision_id <= IPS_REVID_CLARINETP3)) ? 1 : 0)
   #define IPS_IS_MORPHEUS(ha)         (ha->device_id == IPS_MORPHEUS_DEVICEID)
   #define IPS_USE_I2O_DELIVER(ha)     ((IPS_IS_MORPHEUS(ha) || \
                                         (IPS_IS_TROMBONE(ha) && \
                                          (ips_force_i2o))) ? 1 : 0)
   #define IPS_USE_I2O_STATUS(ha)      (IPS_IS_MORPHEUS(ha))
   #define IPS_USE_MEMIO(ha)           ((IPS_IS_MORPHEUS(ha) || \
                                         ((IPS_IS_TROMBONE(ha) || IPS_IS_CLARINET(ha)) && \
                                          (ips_force_memio))) ? 1 : 0)

   #ifndef VIRT_TO_BUS
      #define VIRT_TO_BUS(x)           (unsigned int)virt_to_bus((void *) x)
   #endif

   #ifndef UDELAY
      #define UDELAY udelay
   #endif

   #ifndef MDELAY
      #define MDELAY mdelay
   #endif

   #ifndef verify_area_20
      #define verify_area_20(t,a,sz)   (0) /* success */
   #endif

   #ifndef PUT_USER
      #define PUT_USER                 put_user
   #endif

   #ifndef __PUT_USER
      #define __PUT_USER               __put_user
   #endif

   #ifndef GET_USER
      #define GET_USER                 get_user
   #endif
   
   #ifndef __GET_USER
      #define __GET_USER               __get_user
   #endif
   
   /*
    * Lock macros
    */
   #define IPS_SCB_LOCK(cpu_flags)      spin_lock_irqsave(&ha->scb_lock, cpu_flags)
   #define IPS_SCB_UNLOCK(cpu_flags)    spin_unlock_irqrestore(&ha->scb_lock, cpu_flags)
   #define IPS_QUEUE_LOCK(queue)        spin_lock_irqsave(&(queue)->lock, (queue)->cpu_flags)
   #define IPS_QUEUE_UNLOCK(queue)      spin_unlock_irqrestore(&(queue)->lock, (queue)->cpu_flags)
   #define IPS_HA_LOCK(cpu_flags)       spin_lock_irqsave(&ha->ips_lock, cpu_flags)
   #define IPS_HA_UNLOCK(cpu_flags)     spin_unlock_irqrestore(&ha->ips_lock, cpu_flags)

   /*
    * Adapter address map equates
    */
   #define IPS_REG_HISR                 0x08    /* Host Interrupt Status Reg   */
   #define IPS_REG_CCSAR                0x10    /* Cmd Channel System Addr Reg */
   #define IPS_REG_CCCR                 0x14    /* Cmd Channel Control Reg     */
   #define IPS_REG_SQHR                 0x20    /* Status Q Head Reg           */
   #define IPS_REG_SQTR                 0x24    /* Status Q Tail Reg           */
   #define IPS_REG_SQER                 0x28    /* Status Q End Reg            */
   #define IPS_REG_SQSR                 0x2C    /* Status Q Start Reg          */
   #define IPS_REG_SCPR                 0x05    /* Subsystem control port reg  */
   #define IPS_REG_ISPR                 0x06    /* interrupt status port reg   */
   #define IPS_REG_CBSP                 0x07    /* CBSP register               */
   #define IPS_REG_FLAP                 0x18    /* Flash address port          */
   #define IPS_REG_FLDP                 0x1C    /* Flash data port             */
   #define IPS_REG_NDAE                 0x38    /* Anaconda 64 NDAE Register   */
   #define IPS_REG_I2O_INMSGQ           0x40    /* I2O Inbound Message Queue   */
   #define IPS_REG_I2O_OUTMSGQ          0x44    /* I2O Outbound Message Queue  */
   #define IPS_REG_I2O_HIR              0x30    /* I2O Interrupt Status        */
   #define IPS_REG_I960_IDR             0x20    /* i960 Inbound Doorbell       */
   #define IPS_REG_I960_MSG0            0x18    /* i960 Outbound Reg 0         */
   #define IPS_REG_I960_MSG1            0x1C    /* i960 Outbound Reg 1         */
   #define IPS_REG_I960_OIMR            0x34    /* i960 Oubound Int Mask Reg   */

   /*
    * Adapter register bit equates
    */
   #define IPS_BIT_GHI                  0x04    /* HISR General Host Interrupt */
   #define IPS_BIT_SQO                  0x02    /* HISR Status Q Overflow      */
   #define IPS_BIT_SCE                  0x01    /* HISR Status Channel Enqueue */
   #define IPS_BIT_SEM                  0x08    /* CCCR Semaphore Bit          */
   #define IPS_BIT_ILE                  0x10    /* CCCR ILE Bit                */
   #define IPS_BIT_START_CMD            0x101A  /* CCCR Start Command Channel  */
   #define IPS_BIT_START_STOP           0x0002  /* CCCR Start/Stop Bit         */
   #define IPS_BIT_RST                  0x80    /* SCPR Reset Bit              */
   #define IPS_BIT_EBM                  0x02    /* SCPR Enable Bus Master      */
   #define IPS_BIT_EI                   0x80    /* HISR Enable Interrupts      */
   #define IPS_BIT_OP                   0x01    /* OP bit in CBSP              */
   #define IPS_BIT_I2O_OPQI             0x08    /* General Host Interrupt      */
   #define IPS_BIT_I960_MSG0I           0x01    /* Message Register 0 Interrupt*/
   #define IPS_BIT_I960_MSG1I           0x02    /* Message Register 1 Interrupt*/

   /*
    * Adapter Command ID Equates
    */
   #define IPS_CMD_GET_LD_INFO          0x19
   #define IPS_CMD_GET_SUBSYS           0x40
   #define IPS_CMD_READ_CONF            0x38
   #define IPS_CMD_RW_NVRAM_PAGE        0xBC
   #define IPS_CMD_READ                 0x02
   #define IPS_CMD_WRITE                0x03
   #define IPS_CMD_FFDC                 0xD7
   #define IPS_CMD_ENQUIRY              0x05
   #define IPS_CMD_FLUSH                0x0A
   #define IPS_CMD_READ_SG              0x82
   #define IPS_CMD_WRITE_SG             0x83
   #define IPS_CMD_DCDB                 0x04
   #define IPS_CMD_DCDB_SG              0x84
   #define IPS_CMD_CONFIG_SYNC          0x58
   #define IPS_CMD_ERROR_TABLE          0x17

   /*
    * Adapter Equates
    */
   #define IPS_CSL                      0xFF
   #define IPS_POCL                     0x30
   #define IPS_NORM_STATE               0x00
   #define IPS_MAX_ADAPTERS             16
   #define IPS_MAX_IOCTL                1
   #define IPS_MAX_IOCTL_QUEUE          8
   #define IPS_MAX_QUEUE                128
   #define IPS_BLKSIZE                  512
   #define IPS_MAX_SG                   17
   #define IPS_MAX_LD                   8
   #define IPS_MAX_CHANNELS             4
   #define IPS_MAX_TARGETS              15
   #define IPS_MAX_CHUNKS               16
   #define IPS_MAX_CMDS                 128
   #define IPS_MAX_XFER                 0x10000
   #define IPS_NVRAM_P5_SIG             0xFFDDBB99
   #define IPS_MAX_POST_BYTES           0x02
   #define IPS_MAX_CONFIG_BYTES         0x02
   #define IPS_GOOD_POST_STATUS         0x80
   #define IPS_SEM_TIMEOUT              2000
   #define IPS_IOCTL_COMMAND            0x0D
   #define IPS_IOCTL_NEW_COMMAND        0x81
   #define IPS_INTR_ON                  0
   #define IPS_INTR_IORL                1
   #define IPS_INTR_HAL                 2
   #define IPS_ADAPTER_ID               0xF
   #define IPS_VENDORID                 0x1014
   #define IPS_COPPERHEAD_DEVICEID      0x002E
   #define IPS_MORPHEUS_DEVICEID        0x01BD
   #define IPS_IOCTL_SIZE               8192
   #define IPS_STATUS_SIZE              4
   #define IPS_STATUS_Q_SIZE            (IPS_MAX_CMDS+1) * IPS_STATUS_SIZE
   #define IPS_MEMMAP_SIZE              128
   #define IPS_ONE_MSEC                 1
   #define IPS_ONE_SEC                  1000

   /*
    * Geometry Settings
    */
   #define IPS_COMP_HEADS               128
   #define IPS_COMP_SECTORS             32
   #define IPS_NORM_HEADS               254
   #define IPS_NORM_SECTORS             63

   /*
    * Adapter Basic Status Codes
    */
   #define IPS_BASIC_STATUS_MASK        0xFF
   #define IPS_GSC_STATUS_MASK          0x0F
   #define IPS_CMD_SUCCESS              0x00
   #define IPS_CMD_RECOVERED_ERROR      0x01
   #define IPS_INVAL_OPCO               0x03
   #define IPS_INVAL_CMD_BLK            0x04
   #define IPS_INVAL_PARM_BLK           0x05
   #define IPS_BUSY                     0x08
   #define IPS_CMD_CMPLT_WERROR         0x0C
   #define IPS_LD_ERROR                 0x0D
   #define IPS_CMD_TIMEOUT              0x0E
   #define IPS_PHYS_DRV_ERROR           0x0F

   /*
    * Adapter Extended Status Equates
    */
   #define IPS_ERR_SEL_TO               0xF0
   #define IPS_ERR_OU_RUN               0xF2
   #define IPS_ERR_HOST_RESET           0xF7
   #define IPS_ERR_DEV_RESET            0xF8
   #define IPS_ERR_RECOVERY             0xFC
   #define IPS_ERR_CKCOND               0xFF

   /*
    * Operating System Defines
    */
   #define IPS_OS_WINDOWS_NT            0x01
   #define IPS_OS_NETWARE               0x02
   #define IPS_OS_OPENSERVER            0x03
   #define IPS_OS_UNIXWARE              0x04
   #define IPS_OS_SOLARIS               0x05
   #define IPS_OS_OS2                   0x06
   #define IPS_OS_LINUX                 0x07
   #define IPS_OS_FREEBSD               0x08

   /*
    * Adapter Revision ID's
    */
   #define IPS_REVID_SERVERAID          0x02
   #define IPS_REVID_NAVAJO             0x03
   #define IPS_REVID_SERVERAID2         0x04
   #define IPS_REVID_CLARINETP1         0x05
   #define IPS_REVID_CLARINETP2         0x07
   #define IPS_REVID_CLARINETP3         0x0D
   #define IPS_REVID_TROMBONE32         0x0F
   #define IPS_REVID_TROMBONE64         0x10

   /*
    * Adapter Command/Status Packet Definitions
    */
   #define IPS_SUCCESS                  0x01 /* Successfully completed       */
   #define IPS_SUCCESS_IMM              0x02 /* Success - Immediately        */
   #define IPS_FAILURE                  0x04 /* Completed with Error         */

   /*
    * Logical Drive Equates
    */
   #define IPS_LD_OFFLINE               0x02
   #define IPS_LD_OKAY                  0x03
   #define IPS_LD_FREE                  0x00
   #define IPS_LD_SYS                   0x06
   #define IPS_LD_CRS                   0x24

   /*
    * DCDB Table Equates
    */
   #define IPS_NO_DISCONNECT            0x00
   #define IPS_DISCONNECT_ALLOWED       0x80
   #define IPS_NO_AUTO_REQSEN           0x40
   #define IPS_DATA_NONE                0x00
   #define IPS_DATA_UNK                 0x00
   #define IPS_DATA_IN                  0x01
   #define IPS_DATA_OUT                 0x02
   #define IPS_TRANSFER64K              0x08
   #define IPS_NOTIMEOUT                0x00
   #define IPS_TIMEOUT10                0x10
   #define IPS_TIMEOUT60                0x20
   #define IPS_TIMEOUT20M               0x30

   /*
    * Host adapter Flags (bit numbers)
    */
   #define IPS_IN_INTR                  0
   #define IPS_IN_ABORT                 1
   #define IPS_IN_RESET                 2

   /*
    * SCB Flags
    */
   #define IPS_SCB_ACTIVE               0x00001
   #define IPS_SCB_WAITING              0x00002

   /*
    * Passthru stuff
    */
   #define IPS_COPPUSRCMD              (('C'<<8) | 65)
   #define IPS_COPPIOCCMD              (('C'<<8) | 66)
   #define IPS_NUMCTRLS                (('C'<<8) | 68)
   #define IPS_CTRLINFO                (('C'<<8) | 69)
   #define IPS_FLASHBIOS               (('C'<<8) | 70)

   /* time oriented stuff */
   #define IPS_IS_LEAP_YEAR(y)           (((y % 4 == 0) && ((y % 100 != 0) || (y % 400 == 0))) ? 1 : 0)
   #define IPS_NUM_LEAP_YEARS_THROUGH(y) ((y) / 4 - (y) / 100 + (y) / 400)

   #define IPS_SECS_MIN                 60
   #define IPS_SECS_HOUR                3600
   #define IPS_SECS_8HOURS              28800
   #define IPS_SECS_DAY                 86400
   #define IPS_DAYS_NORMAL_YEAR         365
   #define IPS_DAYS_LEAP_YEAR           366
   #define IPS_EPOCH_YEAR               1970

   /*
    * Scsi_Host Template
    */
#if LINUX_VERSION_CODE < LinuxVersionCode(2,3,27)
 #define IPS {                            \
    next : NULL,                          \
    module : NULL,                        \
    proc_info : NULL,                     \
    proc_dir : NULL,                      \
    name : NULL,                          \
    detect : ips_detect,                  \
    release : ips_release,                \
    info : ips_info,                      \
    command : NULL,                       \
    queuecommand : ips_queue,             \
    eh_strategy_handler : NULL,           \
    eh_abort_handler : ips_eh_abort,      \
    eh_device_reset_handler : NULL,       \
    eh_bus_reset_handler : NULL,          \
    eh_host_reset_handler : ips_eh_reset, \
    abort : NULL,                         \
    reset : NULL,                         \
    slave_attach : NULL,                  \
    bios_param : ips_biosparam,           \
    can_queue : 0,                        \
    this_id: -1,                          \
    sg_tablesize : IPS_MAX_SG,            \
    cmd_per_lun: 16,                      \
    present : 0,                          \
    unchecked_isa_dma : 0,                \
    use_clustering : ENABLE_CLUSTERING,   \
    use_new_eh_code : 1                   \
}
#else
 #define IPS {                            \
    next : NULL,                          \
    module : NULL,                        \
    proc_info : NULL,                     \
    name : NULL,                          \
    detect : ips_detect,                  \
    release : ips_release,                \
    info : ips_info,                      \
    command : NULL,                       \
    queuecommand : ips_queue,             \
    eh_strategy_handler : NULL,           \
    eh_abort_handler : ips_eh_abort,      \
    eh_device_reset_handler : NULL,       \
    eh_bus_reset_handler : NULL,          \
    eh_host_reset_handler : ips_eh_reset, \
    abort : NULL,                         \
    reset : NULL,                         \
    slave_attach : NULL,                  \
    bios_param : ips_biosparam,           \
    can_queue : 0,                        \
    this_id: -1,                          \
    sg_tablesize : IPS_MAX_SG,            \
    cmd_per_lun: 16,                      \
    present : 0,                          \
    unchecked_isa_dma : 0,                \
    use_clustering : ENABLE_CLUSTERING,   \
    use_new_eh_code : 1                   \
}
#endif

/*
 * IBM PCI Raid Command Formats
 */
typedef struct {
   u8        op_code;
   u8        command_id;
   u8        log_drv;
   u8        sg_count;
   u32       lba;
   u32       sg_addr;
   u16       sector_count;
   u16       reserved;
   u32       ccsar;
   u32       cccr;
} IPS_IO_CMD, *PIPS_IO_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u16       reserved;
   u32       reserved2;
   u32       buffer_addr;
   u32       reserved3;
   u32       ccsar;
   u32       cccr;
} IPS_LD_CMD, *PIPS_LD_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u8        reserved;
   u8        reserved2;
   u32       reserved3;
   u32       buffer_addr;
   u32       reserved4;
} IPS_IOCTL_CMD, *PIPS_IOCTL_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u16       reserved;
   u32       reserved2;
   u32       dcdb_address;
   u32       reserved3;
   u32       ccsar;
   u32       cccr;
} IPS_DCDB_CMD, *PIPS_DCDB_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u8        channel;
   u8        source_target;
   u32       reserved;
   u32       reserved2;
   u32       reserved3;
   u32       ccsar;
   u32       cccr;
} IPS_CS_CMD, *PIPS_CS_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u8        log_drv;
   u8        control;
   u32       reserved;
   u32       reserved2;
   u32       reserved3;
   u32       ccsar;
   u32       cccr;
} IPS_US_CMD, *PIPS_US_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u8        reserved;
   u8        state;
   u32       reserved2;
   u32       reserved3;
   u32       reserved4;
   u32       ccsar;
   u32       cccr;
} IPS_FC_CMD, *PIPS_FC_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u8        reserved;
   u8        desc;
   u32       reserved2;
   u32       buffer_addr;
   u32       reserved3;
   u32       ccsar;
   u32       cccr;
} IPS_STATUS_CMD, *PIPS_STATUS_CMD;

typedef struct {
   u8        op_code;
   u8        command_id;
   u8        page;
   u8        write;
   u32       reserved;
   u32       buffer_addr;
   u32       reserved2;
   u32       ccsar;
   u32       cccr;
} IPS_NVRAM_CMD, *PIPS_NVRAM_CMD;

typedef struct {
   u8     op_code;
   u8     command_id;
   u8     reset_count;
   u8     reset_type;
   u8     second;
   u8     minute;
   u8     hour;
   u8     day;
   u8     reserved1[4];
   u8     month;
   u8     yearH;
   u8     yearL;
   u8     reserved2;
} IPS_FFDC_CMD, *PIPS_FFDC_CMD;

typedef union {
   IPS_IO_CMD        basic_io;
   IPS_LD_CMD        logical_info;
   IPS_IOCTL_CMD     ioctl_info;
   IPS_DCDB_CMD      dcdb;
   IPS_CS_CMD        config_sync;
   IPS_US_CMD        unlock_stripe;
   IPS_FC_CMD        flush_cache;
   IPS_STATUS_CMD    status;
   IPS_NVRAM_CMD     nvram;
   IPS_FFDC_CMD      ffdc;
} IPS_HOST_COMMAND, *PIPS_HOST_COMMAND;

typedef struct {
   u8         logical_id;
   u8         reserved;
   u8         raid_level;
   u8         state;
   u32        sector_count;
} IPS_DRIVE_INFO, *PIPS_DRIVE_INFO;

typedef struct {
   u8             no_of_log_drive;
   u8             reserved[3];
   IPS_DRIVE_INFO drive_info[IPS_MAX_LD];
} IPS_LD_INFO, *PIPS_LD_INFO;

typedef struct {
   u8         device_address;
   u8         cmd_attribute;
   u16        transfer_length;
   u32        buffer_pointer;
   u8         cdb_length;
   u8         sense_length;
   u8         sg_count;
   u8         reserved;
   u8         scsi_cdb[12];
   u8         sense_info[64];
   u8         scsi_status;
   u8         reserved2[3];
} IPS_DCDB_TABLE, *PIPS_DCDB_TABLE;

typedef union {
   struct {
      volatile u8  reserved;
      volatile u8  command_id;
      volatile u8  basic_status;
      volatile u8  extended_status;
   } fields;

   volatile u32    value;
} IPS_STATUS, *PIPS_STATUS;

typedef struct {
   IPS_STATUS           status[IPS_MAX_CMDS + 1];
   volatile PIPS_STATUS p_status_start;
   volatile PIPS_STATUS p_status_end;
   volatile PIPS_STATUS p_status_tail;
   volatile u32         hw_status_start;
   volatile u32         hw_status_tail;
   IPS_LD_INFO          logical_drive_info;
} IPS_ADAPTER, *PIPS_ADAPTER;

typedef struct {
   u8        ucLogDriveCount;
   u8        ucMiscFlag;
   u8        ucSLTFlag;
   u8        ucBSTFlag;
   u8        ucPwrChgCnt;
   u8        ucWrongAdrCnt;
   u8        ucUnidentCnt;
   u8        ucNVramDevChgCnt;
   u8        CodeBlkVersion[8];
   u8        BootBlkVersion[8];
   u32       ulDriveSize[IPS_MAX_LD];
   u8        ucConcurrentCmdCount;
   u8        ucMaxPhysicalDevices;
   u16       usFlashRepgmCount;
   u8        ucDefunctDiskCount;
   u8        ucRebuildFlag;
   u8        ucOfflineLogDrvCount;
   u8        ucCriticalDrvCount;
   u16       usConfigUpdateCount;
   u8        ucBlkFlag;
   u8        reserved;
   u16       usAddrDeadDisk[IPS_MAX_CHANNELS * IPS_MAX_TARGETS];
} IPS_ENQ, *PIPS_ENQ;

typedef struct {
   u8        ucInitiator;
   u8        ucParameters;
   u8        ucMiscFlag;
   u8        ucState;
   u32       ulBlockCount;
   u8        ucDeviceId[28];
} IPS_DEVSTATE, *PIPS_DEVSTATE;

typedef struct {
   u8        ucChn;
   u8        ucTgt;
   u16       ucReserved;
   u32       ulStartSect;
   u32       ulNoOfSects;
} IPS_CHUNK, *PIPS_CHUNK;

typedef struct {
   u16       ucUserField;
   u8        ucState;
   u8        ucRaidCacheParam;
   u8        ucNoOfChunkUnits;
   u8        ucStripeSize;
   u8        ucParams;
   u8        ucReserved;
   u32       ulLogDrvSize;
   IPS_CHUNK chunk[IPS_MAX_CHUNKS];
} IPS_LD, *PIPS_LD;

typedef struct {
   u8        board_disc[8];
   u8        processor[8];
   u8        ucNoChanType;
   u8        ucNoHostIntType;
   u8        ucCompression;
   u8        ucNvramType;
   u32       ulNvramSize;
} IPS_HARDWARE, *PIPS_HARDWARE;

typedef struct {
   u8             ucLogDriveCount;
   u8             ucDateD;
   u8             ucDateM;
   u8             ucDateY;
   u8             init_id[4];
   u8             host_id[12];
   u8             time_sign[8];

   struct {
      u32         usCfgDrvUpdateCnt:16;
      u32         ConcurDrvStartCnt:4;
      u32         StartupDelay:4;
      u32         auto_rearrange:1;
      u32         cd_boot:1;
      u32         cluster:1;
      u32         reserved:5;
   } UserOpt;

   u16            user_field;
   u8             ucRebuildRate;
   u8             ucReserve;
   IPS_HARDWARE   hardware_disc;
   IPS_LD         logical_drive[IPS_MAX_LD];
   IPS_DEVSTATE   dev[IPS_MAX_CHANNELS][IPS_MAX_TARGETS+1];
   u8             reserved[512];

} IPS_CONF, *PIPS_CONF;

typedef struct {
   u32        signature;
   u8         reserved;
   u8         adapter_slot;
   u16        adapter_type;
   u8         bios_high[4];
   u8         bios_low[4];
   u16        reserved2;
   u8         reserved3;
   u8         operating_system;
   u8         driver_high[4];
   u8         driver_low[4];
   u8         reserved4[100];
} IPS_NVRAM_P5, *PIPS_NVRAM_P5;

typedef struct _IPS_SUBSYS {
   u32        param[128];
} IPS_SUBSYS, *PIPS_SUBSYS;

/*
 * Inquiry Data Format
 */
typedef struct {
   u8        DeviceType:5;
   u8        DeviceTypeQualifier:3;
   u8        DeviceTypeModifier:7;
   u8        RemoveableMedia:1;
   u8        Versions;
   u8        ResponseDataFormat;
   u8        AdditionalLength;
   u16       Reserved;
   u8        SoftReset:1;
   u8        CommandQueue:1;
   u8        Reserved2:1;
   u8        LinkedCommands:1;
   u8        Synchronous:1;
   u8        Wide16Bit:1;
   u8        Wide32Bit:1;
   u8        RelativeAddressing:1;
   u8        VendorId[8];
   u8        ProductId[16];
   u8        ProductRevisionLevel[4];
   u8        VendorSpecific[20];
   u8        Reserved3[40];
} IPS_INQ_DATA, *PIPS_INQ_DATA;

/*
 * Read Capacity Data Format
 */
typedef struct {
   u32       lba;
   u32       len;
} IPS_CAPACITY;

/*
 * Sense Data Format
 */
typedef struct {
   u8        pg_pc:6;       /* Page Code                    */
   u8        pg_res1:2;     /* Reserved                     */
   u8        pg_len;        /* Page Length                  */
   u16       pg_trk_z;      /* Tracks per zone              */
   u16       pg_asec_z;     /* Alternate sectors per zone   */
   u16       pg_atrk_z;     /* Alternate tracks per zone    */
   u16       pg_atrk_v;     /* Alternate tracks per volume  */
   u16       pg_sec_t;      /* Sectors per track            */
   u16       pg_bytes_s;    /* Bytes per physical sectors   */
   u16       pg_intl;       /* Interleave                   */
   u16       pg_trkskew;    /* Track skew factor            */
   u16       pg_cylskew;    /* Cylinder Skew Factor         */
   u32       pg_res2:27;    /* Reserved                     */
   u32       pg_ins:1;      /* Inhibit Slave                */
   u32       pg_surf:1;     /* Allocate Surface Sectors     */
   u32       pg_rmb:1;      /* Removeable                   */
   u32       pg_hsec:1;     /* Hard sector formatting       */
   u32       pg_ssec:1;     /* Soft sector formatting       */
} IPS_DADF;

typedef struct {
   u8        pg_pc:6;        /* Page Code                     */
   u8        pg_res1:2;      /* Reserved                      */
   u8        pg_len;         /* Page Length                   */
   u16       pg_cylu;        /* Number of cylinders (upper)   */
   u8        pg_cyll;        /* Number of cylinders (lower)   */
   u8        pg_head;        /* Number of heads               */
   u16       pg_wrpcompu;    /* Write precomp (upper)         */
   u32       pg_wrpcompl:8;  /* Write precomp (lower)         */
   u32       pg_redwrcur:24; /* Reduced write current         */
   u32       pg_drstep:16;   /* Drive step rate               */
   u32       pg_landu:16;    /* Landing zone cylinder (upper) */
   u32       pg_landl:8;     /* Landing zone cylinder (lower) */
   u32       pg_res2:24;     /* Reserved                      */
} IPS_RDDG;

struct ips_blk_desc {
   u8       bd_dencode;
   u8       bd_nblks1;
   u8       bd_nblks2;
   u8       bd_nblks3;
   u8       bd_res;
   u8       bd_blen1;
   u8       bd_blen2;
   u8       bd_blen3;
};

typedef struct {
   u8       plh_len;   /* Data length             */
   u8       plh_type;  /* Medium type             */
   u8       plh_res:7; /* Reserved                */
   u8       plh_wp:1;  /* Write protect           */
   u8       plh_bdl;   /* Block descriptor length */
} ips_sense_plh_t;

typedef struct {
   ips_sense_plh_t     plh;
   struct ips_blk_desc blk_desc;

   union {
      IPS_DADF      pg3;
      IPS_RDDG      pg4;
   } pdata;
} ips_mdata_t;

/*
 * Scatter Gather list format
 */
typedef struct ips_sglist {
   u32       address;
   u32       length;
} IPS_SG_LIST, *PIPS_SG_LIST;

typedef struct _IPS_INFOSTR {
   char *buffer;
   int   length;
   int   offset;
   int   pos;
   int   localpos;
} IPS_INFOSTR;

typedef struct {
   char *option_name;
   int  *option_flag;
   int   option_value;
} IPS_OPTION;

/*
 * Status Info
 */
typedef struct ips_stat {
   u32       residue_len;
   u32       scb_addr;
} ips_stat_t;

/*
 * SCB Queue Format
 */
typedef struct ips_scb_queue {
   struct ips_scb *head;
   struct ips_scb *tail;
   u32             count;
   u32             cpu_flags;
   spinlock_t      lock;
} ips_scb_queue_t;

/*
 * Wait queue_format
 */
typedef struct ips_wait_queue {
   Scsi_Cmnd      *head;
   Scsi_Cmnd      *tail;
   u32             count;
   u32             cpu_flags;
   spinlock_t      lock;
} ips_wait_queue_t;

typedef struct ips_copp_wait_item {
   Scsi_Cmnd                 *scsi_cmd;
   struct semaphore          *sem;
   struct ips_copp_wait_item *next;
} ips_copp_wait_item_t;

typedef struct ips_copp_queue {
   struct ips_copp_wait_item *head;
   struct ips_copp_wait_item *tail;
   u32                        count;
   u32                        cpu_flags;
   spinlock_t                 lock;
} ips_copp_queue_t;

/* forward decl for host structure */
struct ips_ha;

typedef struct {
   int  (*reset)(struct ips_ha *);
   int  (*issue)(struct ips_ha *, struct ips_scb *);
   int  (*isinit)(struct ips_ha *);
   int  (*isintr)(struct ips_ha *);
   int  (*init)(struct ips_ha *);
   int  (*erasebios)(struct ips_ha *);
   int  (*programbios)(struct ips_ha *, char *, int);
   int  (*verifybios)(struct ips_ha *, char *, int);
   u32  (*statupd)(struct ips_ha *);
   void (*statinit)(struct ips_ha *);
   void (*intr)(struct ips_ha *);
   void (*enableint)(struct ips_ha *);
} ips_hw_func_t;

typedef struct ips_ha {
   u8                 ha_id[IPS_MAX_CHANNELS+1];
   u32                dcdb_active[IPS_MAX_CHANNELS];
   u32                io_addr;            /* Base I/O address           */
   u8                 irq;                /* IRQ for adapter            */
   u8                 ntargets;           /* Number of targets          */
   u8                 nbus;               /* Number of buses            */
   u8                 nlun;               /* Number of Luns             */
   u16                ad_type;            /* Adapter type               */
   u16                host_num;           /* Adapter number             */
   u32                max_xfer;           /* Maximum Xfer size          */
   u32                max_cmds;           /* Max concurrent commands    */
   u32                num_ioctl;          /* Number of Ioctls           */
   ips_stat_t         sp;                 /* Status packer pointer      */
   struct ips_scb    *scbs;               /* Array of all CCBS          */
   struct ips_scb    *scb_freelist;       /* SCB free list              */
   ips_wait_queue_t   scb_waitlist;       /* Pending SCB list           */
   ips_copp_queue_t   copp_waitlist;      /* Pending PT list            */
   ips_scb_queue_t    scb_activelist;     /* Active SCB list            */
   IPS_IO_CMD        *dummy;              /* dummy command              */
   IPS_ADAPTER       *adapt;              /* Adapter status area        */
   IPS_ENQ           *enq;                /* Adapter Enquiry data       */
   IPS_CONF          *conf;               /* Adapter config data        */
   IPS_NVRAM_P5      *nvram;              /* NVRAM page 5 data          */
   IPS_SUBSYS        *subsys;             /* Subsystem parameters       */
   char              *ioctl_data;         /* IOCTL data area            */
   u32                ioctl_datasize;     /* IOCTL data size            */
   u32                cmd_in_progress;    /* Current command in progress*/
   long               flags;              /* HA flags                   */
   u8                 waitflag;           /* are we waiting for cmd     */
   u8                 active;
   u16                reset_count;        /* number of resets           */
   u32                last_ffdc;          /* last time we sent ffdc info*/
   u8                 revision_id;        /* Revision level             */
   u16                device_id;          /* PCI device ID              */
   u8                 reserved;
   u32                mem_addr;           /* Memory mapped address      */
   u32                io_len;             /* Size of IO Address         */
   u32                mem_len;            /* Size of memory address     */
   char              *mem_ptr;            /* Memory mapped Ptr          */
   char              *ioremap_ptr;        /* ioremapped memory pointer  */
   ips_hw_func_t      func;               /* hw function pointers       */
   struct pci_dev    *pcidev;             /* PCI device handle          */
   spinlock_t         scb_lock;
   spinlock_t         copp_lock;
   spinlock_t         ips_lock;
} ips_ha_t;

typedef void (*ips_scb_callback) (ips_ha_t *, struct ips_scb *);

/*
 * SCB Format
 */
typedef struct ips_scb {
   IPS_HOST_COMMAND  cmd;
   IPS_DCDB_TABLE    dcdb;
   u8                target_id;
   u8                bus;
   u8                lun;
   u8                cdb[12];
   u32               scb_busaddr;
   u32               data_busaddr;
   u32               timeout;
   u8                basic_status;
   u8                extended_status;
   u16               breakup;
   u32               data_len;
   u32               sg_len;
   u32               flags;
   u32               op_code;
   IPS_SG_LIST      *sg_list;
   Scsi_Cmnd        *scsi_cmd;
   struct ips_scb   *q_next;
   ips_scb_callback  callback;
   struct semaphore *sem;
} ips_scb_t;

typedef struct ips_scb_pt {
   IPS_HOST_COMMAND  cmd;
   IPS_DCDB_TABLE    dcdb;
   u8                target_id;
   u8                bus;
   u8                lun;
   u8                cdb[12];
   u32               scb_busaddr;
   u32               data_busaddr;
   u32               timeout;
   u8                basic_status;
   u8                extended_status;
   u16               breakup;
   u32               data_len;
   u32               sg_len;
   u32               flags;
   u32               op_code;
   IPS_SG_LIST      *sg_list;
   Scsi_Cmnd        *scsi_cmd;
   struct ips_scb   *q_next;
   ips_scb_callback  callback;
} ips_scb_pt_t;

/*
 * Passthru Command Format
 */
typedef struct {
   u8            CoppID[4];
   u32           CoppCmd;
   u32           PtBuffer;
   u8           *CmdBuffer;
   u32           CmdBSize;
   ips_scb_pt_t  CoppCP;
   u32           TimeOut;
   u8            BasicStatus;
   u8            ExtendedStatus;
   u16           reserved;
} ips_passthru_t;

#endif

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
