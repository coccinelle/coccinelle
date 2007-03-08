
TW_Device_Extension *tw_device_extension_list[TW_MAX_SLOT];

static int mbox_post_sync_cmd_fast(adapter_t *, int foo[]);

static int mbox_post_sync_cmd_fast(adapter_t *, uint8_t []);

static void udf_merge_extents(struct inode *,
 	 long_ad [EXTENT_MERGE_SIZE], int *);


typedef struct asc_board {
    int                  id;                    /* Board Id */
    uint                 flags;                 /* Board flags */
    union {
        ASC_DVC_VAR      asc_dvc_var;           /* Narrow board */
        ADV_DVC_VAR      adv_dvc_var;           /* Wide board */
    } dvc_var;
    union {
        ASC_DVC_CFG      asc_dvc_cfg;           /* Narrow board */
        ADV_DVC_CFG      adv_dvc_cfg;           /* Wide board */
    } dvc_cfg;
    asc_queue_t          active;                /* Active command queue */
    asc_queue_t          waiting;               /* Waiting command queue */
    asc_queue_t          done;                  /* Done command queue */
    ADV_SCSI_BIT_ID_TYPE init_tidmask;          /* Target init./valid mask */
    Scsi_Device          *device[ADV_MAX_TID+1]; /* Mid-Level Scsi Device */
};


