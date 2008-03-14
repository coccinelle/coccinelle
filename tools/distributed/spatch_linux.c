#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <sys/time.h>
#include <signal.h>

#define MAX 9

#define DONE_SEM 0 // index of the semaphore on which to wait for children

#define HOME "/home/julia/coccinelle/tools/distributed/"

void inc_sem(int sem, int sem_num, int inc) {
  struct sembuf sops;
  sops.sem_num = sem_num;
  sops.sem_op = inc;
  sops.sem_flg = 0;
  semop(sem,&sops,1);
}

void dec_sem(int sem, int sem_num) {
  struct sembuf sops;
  sops.sem_num = sem_num;
  sops.sem_op = -1;
  sops.sem_flg = 0;
  semop(sem,&sops,1);
}

void wait_sem(int sem, int sem_num) {
  struct sembuf sops;
  sops.sem_num = sem_num;
  sops.sem_op = 0;
  sops.sem_flg = 0;
  semop(sem,&sops,1);
}

void do_child(int sem, int id, unsigned int argc, char **argv) {
  int pid,status;
  if (!(pid=fork())) {
    // child
    int i;
    char **new_args = malloc(sizeof(char*) * (argc + 3));
    char string[50];
    for(i=1; i!=argc; i++) {
      new_args[i+2] = argv[i];
    }
    new_args[i+2] = NULL;
    new_args[0] = "nothing";
    new_args[1] = new_args[3];  // cocci file must be first
    new_args[2] = "-index";
    sprintf(string, "%d", id);
    new_args[3] = string;  // processor number must be third
    execvp(HOME "spatch_linux_script",new_args);
    printf("tried to execute %s\n",HOME "spatch_linux_script");
    perror("exec failure");
    exit(0);
  }
  wait(&status);
  dec_sem(sem,DONE_SEM);  // indicate that this child is done
}

void cleanup(char **argv) {
  char **new_args = malloc(sizeof(char*) * 3);
  new_args[0] = "nothing";
  new_args[1] = argv[1];
  new_args[2] = NULL;
  execvp(HOME "cleanup_script",new_args);
}

int main(unsigned int argc, char **argv) {
  int pid, i, start=0;
  // initialize the semaphore
  int sem = semget(0,1/* only one sem */,(IPC_CREAT|0666));
  int max = MAX;
  if (argv[1] == "-processes") {max = atoi(argv[2]); start = 2;}
  if (argv[1] == "--help") {
    printf("spatch_linux [-processes n] foo.cocci ...\n");
    exit (0);
  }

  inc_sem(sem,0,max);

  // run the child processes
  for(i=0;i!=max;i++) {
    if (!(pid=fork())) {
      // child
      do_child(sem,i,argc-start,&argv[start]);
      exit(0);
    }
  }

  wait_sem(sem,DONE_SEM); // wait for the children to end
  cleanup(&argv[start]);
}
