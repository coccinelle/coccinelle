                            Coccinelle 


Coccinelle allows programmers to easily write some complex
style-preserving source-to-source transformations on C source code,
like for instance to perform some refactorings.

To install Coccinelle from its source, see the instructions in install.txt.
Once you have installed coccinelle, there is a script 'spatch' in /usr/bin
or /usr/local/bin that invokes the Coccinelle program.

If you want to run Coccinelle without installing it, you can run the
Coccinelle program directly from the download/build directory. You may then
have to setup a few environment variables so that the Coccinelle program
knows where to find its configuration files.
For bash do:

  $ source env.sh

For tcsh do:

  $ source env.csh 


You can test coccinelle with:

  $ spatch -sp_file demos/simple.cocci demos/simple.c -o /tmp/new_simple.c

If you haven't installed coccinelle, run then ./spatch or ./spatch.opt 



If you downloaded the bytecode version of spatch you may first 
have to install OCaml (which contains the 'ocamlrun' bytecode interpreter,
the equivalent of 'java', the Java virtual machine, but for OCaml) and then do:

  $ ocamlrun spatch -sp_file demos/simple.cocci demos/simple.c -o /tmp/new_simple.c


For more information on Coccinelle, type 'make docs' and have a look at the 
files in the docs/ directory. You may need to install the texlive-fonts-extra
packages from your distribution to compile some of the LaTeX documentation
files.

 ** Runtime dependencies under Debian/Ubuntu**

 - For the OCaml scripting feature in SmPL
	ocaml-native-compilers
     or ocaml-nox
