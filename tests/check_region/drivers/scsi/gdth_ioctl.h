#ifndef _GDTH_IOCTL_H
#define _GDTH_IOCTL_H

/* gdth_ioctl.h
 * $Id$
 */

/* IOCTLs */
#define GDTIOCTL_MASK       ('J'<<8)
#define GDTIOCTL_GENERAL    (GDTIOCTL_MASK | 0) /* general IOCTL */
#define GDTIOCTL_DRVERS     (GDTIOCTL_MASK | 1) /* get driver version */
#define GDTIOCTL_CTRTYPE    (GDTIOCTL_MASK | 2) /* get controller type */
#define GDTIOCTL_OSVERS     (GDTIOCTL_MASK | 3) /* get OS version */
#define GDTIOCTL_CTRCNT     (GDTIOCTL_MASK | 5) /* get controller count */
#define GDTIOCTL_LOCKDRV    (GDTIOCTL_MASK | 6) /* lock host drive */
#define GDTIOCTL_LOCKCHN    (GDTIOCTL_MASK | 7) /* lock channel */
#define GDTIOCTL_EVENT      (GDTIOCTL_MASK | 8) /* read controller events */

#define GDTIOCTL_MAGIC      0xaffe0001UL


/* IOCTL structure (write) */
typedef struct {
    ulong32                 magic;              /* IOCTL magic */
    ushort                  ioctl;              /* IOCTL */
    ushort                  ionode;             /* controller number */
    ushort                  service;            /* controller service */
    ushort                  timeout;            /* timeout */
    union {
        struct {
            unchar          command[512];       /* controller command */
            unchar          data[1];            /* add. data */
        } general;
        struct {
            unchar          lock;               /* lock/unlock */
            unchar          drive_cnt;          /* drive count */
            ushort          drives[35];         /* drives */
        } lockdrv;
        struct {
            unchar          lock;               /* lock/unlock */
            unchar          channel;            /* channel */
        } lockchn;
        struct {
            int             erase;              /* erase event ? */
            int             handle;
            unchar          evt[34];            /* event structure */
        } event;
    } iu;
} gdth_iowr_str;

/* IOCTL structure (read) */
typedef struct {
    ulong32                 size;               /* buffer size */
    ulong32                 status;             /* IOCTL error code */
    union {
        struct {
            unchar          data[1];            /* data */
        } general;
        struct {
            ushort          version;            /* driver version */
        } drvers;
        struct {
            unchar          type;               /* controller type */
            ushort          info;               /* slot etc. */
            ushort          oem_id;             /* OEM ID */
            ushort          bios_ver;           /* not used */
            ushort          access;             /* not used */
            ushort          ext_type;           /* extended type */
        } ctrtype;
        struct {
            unchar          version;            /* OS version */
            unchar          subversion;         /* OS subversion */
            ushort          revision;           /* revision */
        } osvers;
        struct {
            ushort          count;              /* controller count */
        } ctrcnt;
        struct {
            int             handle;
            unchar          evt[34];            /* event structure */
        } event;
    } iu;
} gdth_iord_str;


#endif

