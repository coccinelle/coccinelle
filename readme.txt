Coccinelle allows programmers to easily write some complex
style-preserving source-to-source transformations on C source code,
like for instance to perform some refactorings.

To install Coccinelle from its source, see the instructions in install.txt.
For more information on Coccinelle see the files in the docs/ directory.

Once you have installed Coccinelle (either from the source or from one
of the binary form available on the Coccinelle website), You may have
to setup a few environment variables so that the Coccinelle program
know where to find its configuration files.
For bash do:

 source env.sh

For tcsh do:

 source env.csh 



You can then test coccinelle with:

 spatch -sp_file demos/simple.cocci demos/simple.c

If you downloaded the bytecode version of spatch you may first 
have to install OCaml (which contains the 'ocamlrun' bytecode interpreter,
the equivalent of 'java', the Java virtual machine, but for OCaml) and then do:

 ocamlrun spatch -sp_file demos/simple.cocci demos/simple.c

