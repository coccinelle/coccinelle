@@
mddev_t *mddev;
@@

(
- mddev->sb->major_version
+ mddev->major_version
|
- mddev->sb->minor_version
+ mddev->minor_version
|
- mddev->sb->patch_version
+ mddev->patch_version
|
- mddev->sb->persistent
+ mddev->persistent
|
- mddev->sb->chunk_size
+ mddev->chunk_size
|
- mddev->sb->ctime
+ mddev->ctime
|
- mddev->sb->utime
+ mddev->utime
|
- mddev->sb->level
+ mddev->level
|
- mddev->sb->layout
+ mddev->layout
|
- mddev->sb->raid_disks
+ mddev->raid_disks
|
- mddev->sb->state
+ mddev->state
|
- mddev->sb->size
+ mddev->size
|
- mddev->sb->events
+ mddev->events
|
- mddev->sb->uuid
+ mddev->uuid
)

@@
mddev_t *mddev;
mdp_super_t *sbx;
expression E1, E2;
@@

sbx = mddev->sb;
<... when != \( sb = E1; \| mddev = E2; \)
(
- sbx->major_version
+ mddev->major_version
|
- sbx->minor_version
+ mddev->minor_version
|
- sbx->patch_version
+ mddev->patch_version
|
- sbx->persistent
+ mddev->persistent
|
- sbx->chunk_size
+ mddev->chunk_size
|
- sbx->ctime
+ mddev->ctime
|
- sbx->utime
+ mddev->utime
|
- sbx->level
+ mddev->level
|
- sbx->layout
+ mddev->layout
|
- sbx->raid_disks
+ mddev->raid_disks
|
- sbx->state
+ mddev->state
|
- sbx->size
+ mddev->size
|
- sbx->events
+ mddev->events
|
- sbx->uuid
+ mddev->uuid
)
...>