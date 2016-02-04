static int cm206_block_ioctl(struct inode *inode, struct file *file,
				unsigned cmd, unsigned long arg)
{
	return cdrom_ioctl(&cm206_info, inode, cmd, arg);
}

static struct block_device_operations cm206_bdops =
{
	.ioctl		= cm206_block_ioctl
};
