#include <mpi.h>
int main(int argc, char **argv)
{
  MPI_Comm comm;
  MPI_Init(&argc, &argv);
  comm = MPI_COMM_WORLD;
}
