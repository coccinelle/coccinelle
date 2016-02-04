
struct SHT template = { 
  .field1 = 1,
  .proc_info = my_proc_info,
  .field2 = 2,
};


int my_proc_info(int i) 
{
}


int foo(struct SHT * tpnt) 
{
  tpnt->proc_info = my_proc_info2;
}

int my_proc_info2(int i) 
{
}


int not_proc_info(int i) 
{
}
