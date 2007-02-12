static int md_ioctl(struct inode *inode, struct file *file,
			unsigned int cmd, unsigned long arg)
{

  if(1) { goto abort_unlock; }
  if(1) { goto done; }
  if(1) { goto abort; }

done_unlock:
abort_unlock:
	mddev_unlock(mddev);

	return err;
done:
	if (err)
		MD_BUG();
abort:
	return err;
}
