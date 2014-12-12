SGEN
====

Description:
------------
This is a tool to generate context and printing modes for a Coccinelle script.


Installation:
-------------
You need to have Coccinelle and all of Coccinelle's dependencies installed. 
Installation relies on the project being in the tools/sgen folder of the 
Coccinelle source code (if not, change the COCCIDIR path in the makefile).

1.  Run the command 'make'.
2.  Run 'make install' (needs superuser permissions).
3.  Test the program e.g. with

	sgen examples/tiny.cocci

    or

	sgen &lt;your\_cocci\_script&gt;.cocci

    The output should be a Coccinelle script with equivalent functionality to the original one, but with added virtual modes patch, context, org, and report.

Uninstallation:
---------------
To uninstall, just run 'make uninstall'.


Usage:
------
After installation, run e.g.

	sgen file.cocci --config file.config

to generate the file named file.cocci with file.config (sgen config). Or

	sgen file.cocci --interactive

to run the program in interactive mode where the program will generate a config
file for you. If running

	sgen file.cocci

with no flags, the program will use file.config per default if it exists, or
else start in interactive mode.

For all options, see

	sgen -help


Contents:
---------
The home directory contains this file, a Makefile, and the directories
mentioned below.  
The documentation directory contains the documentation tex files.  
The source directory contains the OCaml source code for sgen.  
The scripts directory contains the script used for installation.  
The examples directory contains examples of Coccinelle scripts and
corresponding sgen config files as well as C files to test on.
