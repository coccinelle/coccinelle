// author: Michele Martone
// description: example remove MPI function calls
@@
identifier V;
identifier mpi_function_call =~ "^MPI_";
expression list AL;
@@
(
-V = mpi_function_call(AL);
|
-    mpi_function_call(AL);
)

// vim:number:syntax=diff
