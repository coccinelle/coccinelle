#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>

#define MAX 9

#ifndef HOME
#define HOME "/home/julia/coccinelle/tools/distributed/"
#endif

void do_child(int id, unsigned int argc, char **argv, int max,
	      char *script) {
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
    execvp(script,new_args);
    printf("tried to execute %s\n",HOME "spatch_linux_script");
    perror("exec failure");
    _exit(0);
}

void cleanup(char **argv) {
  char **new_args = malloc(sizeof(char*) * 3);
  new_args[0] = "nothing";
  new_args[1] = argv[1];
  new_args[2] = NULL;
  printf ("doing cleanup on %s\n",argv[1]);
  execvp(HOME "cleanup",new_args);
}

int main(int argc, char **argv) {
  int i, start=0, max;
  char script[150];
  // interpret the arguments
  max = MAX;
  if (!strcmp(argv[1],"-processes")) {max = atoi(argv[2]); start = 2;}
  if (!strcmp(argv[1],"-script")) {
    strcpy(script,HOME);
    strcat(script,argv[2]);
    start = 2;
  } else strcpy(script,HOME "spatch_linux_script");
  if (!strcmp(argv[1],"--help")) {
    printf("spatch_linux [-processes n] foo.cocci ...\n");
    exit (0);
  }

  // run the child processes
  int pid;
  for(i=0;i!=max;i++) {
    if (!(pid=fork())) {
      // child
      do_child(i,argc-start,&argv[start],max,script);
    }
    else if (pid > 0) {
  //  	printf("Child born: %d\n", pid);
    }
    else
    	printf("*** forking error ***\n");
  }
  int status;
  for(i=0;i!=max;i++) {
  	pid = wait(&status);
  //  	printf("Child dead: %d -- %d\n", pid,status);
  }
  cleanup(&argv[start]);
}
