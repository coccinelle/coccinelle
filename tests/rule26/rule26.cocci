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
mdp_super_t *sb;
expression E1, E2;
@@

sb = mddev->sb;
<... when != \( sb = E1; \| mddev = E2; \)
(
- sb->major_version
+ mddev->major_version
|
- sb->minor_version
+ mddev->minor_version
|
- sb->patch_version
+ mddev->patch_version
|
- sb->persistent
+ mddev->persistent
|
- sb->chunk_size
+ mddev->chunk_size
|
- sb->ctime
+ mddev->ctime
|
- sb->utime
+ mddev->utime
|
- sb->level
+ mddev->level
|
- sb->layout
+ mddev->layout
|
- sb->raid_disks
+ mddev->raid_disks
|
- sb->state
+ mddev->state
|
- sb->size
+ mddev->size
|
- sb->events
+ mddev->events
|
- sb->uuid
+ mddev->uuid
)
...>