// can constraint to specific function by using multiple rules

@@
struct file_operations x;
identifier ioctl_fn;
@@

x.ioctl = ioctl_fn;


@@
@@

 ioctl_fn(...) 
{

- foo(1);
+ foo(2);
}

