

struct file_operations fops;

void init(int i) 
{

  fops.ioctl = my_ioctl;
}



void my_ioctl(int i)
{
  foo(1);
}



void not_ioctl(int i)
{
  foo(1);
}
