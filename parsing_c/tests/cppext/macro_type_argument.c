int main(int i) { 

#define IOCTL_IN(arg, type, in)					\
	if (copy_from_user(&(in), (type *) (arg), sizeof (in)))	\
		return -EFAULT;

#define IOCTL_OUT(arg, type, out) \
	if (copy_to_user((type *) (arg), &(out), sizeof (out)))	\
		return -EFAULT;


  IOCTL_IN(arg, struct cdrom_multisession, ms_info);


  DEB(int i);

  m3_t *chip = snd_magic_cast(m3_t, dev_id, );

}
