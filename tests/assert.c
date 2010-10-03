xfs_dir2_data_free_t *
xfs_dir2_data_freefind(
	xfs_dir2_data_t		*d,		/* data block */
	xfs_dir2_data_unused_t	*dup)		/* data unused entry */
{
  if (off < be16_to_cpu(dfp->offset))
    ASSERT(off + be16_to_cpu(dup->length) <= be16_to_cpu(dfp->offset));
  else
    ASSERT(be16_to_cpu(dfp->offset) + be16_to_cpu(dfp->length) <= off);
}
