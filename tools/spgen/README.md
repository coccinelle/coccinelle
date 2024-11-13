SPGEN
====

Description
-----------
This is a tool to harden Coccinelle scripts by generating context and printing modes.

Example:
Given a semantic patch (examples/addvoid.cocci)

	@@
	identifier f;
	@@

	f(
	+ void
	) { ... }

and metadata about the script through the commandline in interactive mode or a config file (examples/addvoid.config)

	// Generated config from interactive mode.
	description = Adds void to function headers with no arguments.
	limitations = If a matched function has a prototype declaration, the script will not match the prototype.
	confidence = High
	options = --recursive-includes
	url = https://coccinelle.gitlabpages.inria.fr/website/
	1:addvoid =
	  org:"WARNING: Zero-argument function \"%s\" should have void declaration." % (f)

the program outputs a hardened semantic patch with virtual rules patch, context, org, and report (examples/addvoid_.cocci)

	/// Adds void to function headers with no arguments.
	//# If a matched function has a prototype declaration, the script will not
	//# match the prototype.
	///
	// Confidence: High
	// URL: https://coccinelle.gitlabpages.inria.fr/website/
	// Options: --recursive-includes

	virtual patch
	virtual context
	virtual org
	virtual report

	@addvoid depends on patch && !context && !org && !report@
	identifier f;
	@@

	f(
	+ void
	 ) { ... }

	// ----------------------------------------------------------------------------

	@addvoid_context depends on !patch && (context || org || report)@
	identifier f;
	position j0;
	@@

	* f@j0(
	 ) { ... }

	// ----------------------------------------------------------------------------

	@script:python addvoid_org depends on org@
	f << addvoid_context.f;
	j0 << addvoid_context.j0;
	@@

	msg = "WARNING: Zero-argument function \"%s\" should have void declaration. " % (f)
	coccilib.org.print_safe_todo(j0[0], msg)

	// ----------------------------------------------------------------------------

	@script:python addvoid_report depends on report@
	f << addvoid_context.f;
	j0 << addvoid_context.j0;
	@@

	msg = "WARNING: Zero-argument function \"%s\" should have void declaration. " % (f)
	coccilib.report.print_report(j0[0], msg)

This script can then be run on C files in the same manner as the original, by specifying the virtual rule when running spatch, e.g. for context mode

	spatch --sp-file addvoid_.cocci addvoid.c -D context


Installation
------------
You need to have Coccinelle and all of Coccinelle's dependencies installed.
Installation relies on the project being in the tools/spgen folder of the
Coccinelle source code (if not, change the COCCIDIR path in the makefile).

1.  Run the command

    	make all

    to compile the code.
2.  Run

    	make install

    to install the program.
3.  Test the program e.g. with

    	spgen examples/addvoid.cocci

    or

    	spgen <your_cocci_script>.cocci

    The output should be a Coccinelle script with equivalent functionality to the original one, but with added virtual modes patch, context, org, and report.


Uninstallation
--------------
To uninstall, just run

	make uninstall


Usage
-----
After installation, run e.g.

	spgen file.cocci --config file.config

to generate the file named file.cocci with file.config (spgen config). Or

	spgen file.cocci --interactive

to run the program in interactive mode where the program will generate a config
file for you. If running

	spgen file.cocci

with no flags, the program will use file.config per default if it exists, or
else start in interactive mode.

For all options, see

	spgen -help


Contents
--------
The home directory contains this file, a Makefile, and the directories
mentioned below.

The documentation directory contains the documentation tex files.

The examples directory contains examples of Coccinelle scripts and
corresponding spgen config files as well as C files to test on.

The scripts directory contains the script used for installation.

The source directory contains the OCaml source code for spgen.

The tests directory contains test files.
