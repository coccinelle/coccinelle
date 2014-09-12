SGEN
====

Description:
------------
This is a tool to generate context and printing modes for a Coccinelle script.

Installation:
-------------

You need to have Coccinelle and all of Coccinelle's dependencies installed.

1.  Change the COCCIDIR path in the Makefile to point at the Coccinelle source
code (default is within the tools directory of the Coccinelle directory).
2.  Run the command 'make'.
3.  Run 'make install' (needs superuser permissions).

Usage:
------
After installation, run e.g.

	sgen file.cocci --config file.config

to generate the file, using file.config (sgen config). Or

	sgen file.cocci --interactive

to run the program in interactive mode where the program will generate a config
file for you.

For all options, see

	sgen -help

Contents:
---------
The home directory contains the source code, this file, a Makefile, and the
directories mentioned below.
The scripts directory contains the script used for installation.
The examples directory contains examples of Coccinelle scripts and
corresponding sgen config files as well as C files to test on.
