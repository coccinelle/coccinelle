@fn@
identifier xyz_ioctl;
identifier xyz_ops;
@@

struct file_operations xyz_ops = {
	.ioctl = xyz_ioctl,
};

@safe@
identifier fn.xyz_ioctl;
identifier i;
@@

static int xyz_ioctl(struct inode *i, ...)
{
  ... when != i
}

@count disable braces1, braces2, braces3, braces4@
identifier fn.xyz_ioctl;
statement S;
@@

int xyz_ioctl(...) {
  <+...
(
   {
    ... when strict
    return ...; }
|
   if (...) return ...; else S
)
  ...+>
}

@one depends on safe && !count@
identifier fn.xyz_ioctl;
identifier i, f, cmd, arg;
identifier ret;
constant cret;
statement S,S1;
@@

-xyz_ioctl(struct inode *i, struct file *f, unsigned cmd, unsigned long arg)
+xyz_ioctl(struct file *f, unsigned cmd, unsigned long arg)
{
  ... when != S1
+ lock_kernel();
  S
  ...
(
+ unlock_kernel();
  return ret;
|
+ unlock_kernel();
  return cret;
)
}

@call depends on one@
identifier fn.xyz_ioctl;
expression E1, E2, E3, E4;
@@

- xyz_ioctl(E1, E2, E3, E4)
+ xyz_ioctl(E2, E3, E4)


// be sure the changes can be made before transforming
// prototype has to be more complicated, because unsigned int can be
// just unsigned
@decl depends on one@
identifier xyz_ioctl;
identifier xyz_ops;
@@

struct file_operations xyz_ops = {
-	.ioctl = xyz_ioctl,
+	.unlocked_ioctl = xyz_ioctl,
};
