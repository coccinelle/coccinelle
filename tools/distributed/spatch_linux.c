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

int sem;

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
  int err;
  sops.sem_num = sem_num;
  sops.sem_op = 0;
  sops.sem_flg = 0;
  err = semop(sem,&sops,1);
  if (err < 0) {printf("error in %d\n",sem);perror("wait_sem");}
}

void exit_sighandler(int x) {
  semctl(sem,DONE_SEM,IPC_RMID);
  exit(0);
}

void do_child(int sem, int id, unsigned int argc, char **argv, int max) {
  int pid,status;
  if (!(pid=fork())) {
    // child
    int i;
    char **new_args = malloc(sizeof(char*) * (argc + 5));
    char string1[50],string2[50];
    for(i=1; i!=argc; i++) {
      new_args[i+4] = argv[i];
    }
    new_args[i+4] = NULL;
    new_args[0] = "nothing";
    new_args[1] = new_args[5];  // cocci file must be first
    new_args[2] = "-index";
    sprintf(string1, "%d", id);
    new_args[3] = string1;  // processor number must be third
    new_args[4] = "-max";
    sprintf(string2, "%d", max);
    new_args[5] = string2;
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
  printf ("doing cleanup on %s\n",argv[1]);
  execvp(HOME "cleanup_script",new_args);
}

int main(unsigned int argc, char **argv) {
  int pid, i, start=0, max;
  // initialize the semaphore
  sem = semget(0,1/* only one sem */,(IPC_CREAT|0666));
  if (sem < 0) { perror("semget"); exit(0); }
  // set up signal handlers so we can delete the semaphore
  signal(SIGTERM,exit_sighandler); // kill
  signal(SIGHUP,exit_sighandler);  // kill -HUP  /  xterm closed
  signal(SIGINT,exit_sighandler);  // Interrupt from keyboard
  signal(SIGQUIT,exit_sighandler); // Quit from keyboard
  // interpret the arguments
  max = MAX;
  if (!strcmp(argv[1],"-processes")) {max = atoi(argv[2]); start = 2;}
  if (!strcmp(argv[1],"--help")) {
    printf("spatch_linux [-processes n] foo.cocci ...\n");
    exit (0);
  }

  inc_sem(sem,0,max);

  // run the child processes
  for(i=0;i!=max;i++) {
    if (!(pid=fork())) {
      // child
      do_child(sem,i,argc-start,&argv[start],max);
      exit(0);
    }
  }

  wait_sem(sem,DONE_SEM); // wait for the children to end
  int err = semctl(sem,DONE_SEM,IPC_RMID);
  if (err < 0) perror ("couldn't remove");
  cleanup(&argv[start]);
}
