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

  $ spatch --sp-file demos/simple.cocci demos/simple.c -o /tmp/new_simple.c

If you haven't installed coccinelle, run then ./spatch or ./spatch.opt



If you downloaded the bytecode version of spatch you may first
have to install OCaml (which contains the 'ocamlrun' bytecode interpreter,
the equivalent of 'java', the Java virtual machine, but for OCaml) and then do:

  $ ocamlrun spatch --sp-file demos/simple.cocci demos/simple.c -o /tmp/new_simple.c


For more information on Coccinelle, type 'make docs' and have a look at the
files in the docs/ directory. You may need to install the texlive-fonts-extra
packages from your distribution to compile some of the LaTeX documentation
files.

 ** Runtime dependencies under Debian/Ubuntu**

 - For the OCaml scripting feature in SmPL
	ocaml-native-compilers
     or ocaml-nox

 - For the Python scripting feature in SmPL: python3-dev
   Note python3-dev is only a runtime dependency: it is _not_ required for
   building coccinelle.

---------------------------------------------------------------------------

Contributing:

Contributions are welcome.  Please sign your contributions, according to
the following text extracted from Documentation/SubmittingPatches.txt of
the Linux kernel:

The sign-off is a simple line at the end of the explanation for the
patch, which certifies that you wrote it or otherwise have the right to
pass it on as an open-source patch.  The rules are pretty simple: if you
can certify the below:

        Developer's Certificate of Origin 1.1

        By making a contribution to this project, I certify that:

        (a) The contribution was created in whole or in part by me and I
            have the right to submit it under the open source license
            indicated in the file; or

        (b) The contribution is based upon previous work that, to the best
            of my knowledge, is covered under an appropriate open source
            license and I have the right under that license to submit that
            work with modifications, whether created in whole or in part
            by me, under the same open source license (unless I am
            permitted to submit under a different license), as indicated
            in the file; or

        (c) The contribution was provided directly to me by some other
            person who certified (a), (b) or (c) and I have not modified
            it.

	(d) I understand and agree that this project and the contribution
	    are public and that a record of the contribution (including all
	    personal information I submit with it, including my sign-off) is
	    maintained indefinitely and may be redistributed consistent with
	    this project or the open source license(s) involved.

then you just add a line saying

	Signed-off-by: Random J Developer <random@developer.example.org>

using your real name (sorry, no pseudonyms or anonymous contributions.)
