static inline secno ea_sec(struct extended_attribute *ea)
{
    return le32_to_cpu(get_unaligned((__le32 *)((char *)ea + 9 +
        ea->namelen)));
}

void hpfs_prefetch_sectors(struct super_block *s, unsigned secno, int n)
{
	struct buffer_head *bh;
	struct blk_plug plug;

	if (n <= 0 || unlikely(secno >= hpfs_sb(s)->sb_fs_size))
		return;
}
