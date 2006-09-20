#ifndef _SCSI_DEBUG_H

#include <linux/types.h>
#include <linux/kdev_t.h>

int scsi_debug_detect(Scsi_Host_Template *);
int scsi_debug_command(Scsi_Cmnd *);
int scsi_debug_queuecommand(Scsi_Cmnd *, void (*done) (Scsi_Cmnd *));
int scsi_debug_abort(Scsi_Cmnd *);
int scsi_debug_biosparam(Disk *, kdev_t, int[]);
int scsi_debug_reset(Scsi_Cmnd *, unsigned int);
int scsi_debug_proc_info(char *, char **, off_t, int, int, int);

#ifndef NULL
#define NULL 0
#endif


#define SCSI_DEBUG_MAILBOXES 1

/*
 * Allow the driver to reject commands.  Thus we accept only one, but
 * and the mid-level will queue the remainder.
 */
#define SCSI_DEBUG_CANQUEUE  255

#define SCSI_DEBUG {proc_info:         scsi_debug_proc_info,	\
		    name:              "SCSI DEBUG",		\
		    detect:            scsi_debug_detect,	\
		    queuecommand:      scsi_debug_queuecommand, \
		    abort:             scsi_debug_abort,	\
		    reset:             scsi_debug_reset,	\
		    bios_param:        scsi_debug_biosparam,	\
		    can_queue:         SCSI_DEBUG_CANQUEUE,	\
		    this_id:           7,			\
		    sg_tablesize:      16,			\
		    cmd_per_lun:       3,			\
		    unchecked_isa_dma: 0,			\
		    use_clustering:    ENABLE_CLUSTERING,	\
		    use_new_eh_code:   1,			\
}
#endif
