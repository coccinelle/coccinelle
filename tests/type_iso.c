
struct SHT ops1;
struct SHT2 notops1;


void main(int i)
{
  ops1.proc_info =1;
  notops1.proc_info =1;

}


typedef struct SHT SHT_t;

SHT_t ops2;
SHT2_t notops2;

void main(int i)
{
  ops2.proc_info =1;
  notops2.proc_info =1;

}
