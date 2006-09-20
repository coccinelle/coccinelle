#ifndef _GDTH_PROC_H
#define _GDTH_PROC_H

/* gdth_proc.h 
 * $Id$
 */

static int gdth_set_info(char *buffer,int length,int vh,int hanum,int busnum);
static int gdth_set_asc_info(char *buffer,int length,int hanum,Scsi_Cmnd * scp);
static int gdth_set_bin_info(char *buffer,int length,int hanum,Scsi_Cmnd * scp);
static int gdth_get_info(char *buffer,char **start,off_t offset,
                         int length,int vh,int hanum,int busnum);

static int gdth_ioctl_alloc(int hanum, ushort size);
static void gdth_ioctl_free(int hanum);
static void gdth_wait_completion(int hanum, int busnum, int id);
static void gdth_stop_timeout(int hanum, int busnum, int id);
static void gdth_start_timeout(int hanum, int busnum, int id);
static int gdth_update_timeout(int hanum, Scsi_Cmnd *scp, int timeout);

static void gdth_do_cmd(Scsi_Cmnd *scp,gdth_cmd_str *cmd,int timeout);
void gdth_scsi_done(Scsi_Cmnd *scp);

#endif

