#!/usr/bin/perl

# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

#
# ARGV: 0 = replacement text, 1 = file w. list of files
#

$debug = 0;
$retain = 1;			# 1 = retain old status

#----------------------------------------------------------------------
$header = 0;  # 0 = not seen header yet; 1 = seen beginning; 2 = seen end

$currentfile = "";
$currentstatus = "";
$oldstatus = "";
@files = ();

# Get replacement text
$replace = shift;

# Get files of interest
open(FILES,shift);
@files = <FILES>;
close(FILES);

# Remove file name suffixes
foreach $_ (@files) {
    s/^([^.]+)\..+$/\1/;
    chop;
    print "--> added [$_]\n" if $debug;
}

# Process std. input
while(<>) {

    # Find an ignore header
    if(/^-+$/) {
	$header = $header + 1;
    }

    #
    if($header > 1) {

	# Filename
	if(/^([0-9a-zA-Z_-]+)\.c\s*$/) {
	    $currentfile = $1;
	    $currentstatus = "";
	    print "--> currentfile: [$currentfile]\n" if $debug;
	}

	# Status code
	if(/^(\s+\*\s+)\[status\]([ \t\f]*)(\S*)$/) {
	    $currentstatus = $3;

	    print "--> $currentfile [$currentstatus]\n" if $debug;

	    if(grep {/^$currentfile$/} @files) {
		s/^(\s+\*\s+\[status\])[ \t\f]*(.*)$/\1 $replace/;
		print "==>" if $debug;

		if($retain && ($currentstatus ne "")) {
		    $oldstatus = "  * [old-status] $currentstatus\n";
		}
	    }

	    $currentfile ="";

	}

    }

    #
    print "$_";

    if($oldstatus ne "") {
	print $oldstatus;
	$oldstatus = "";
    }

}
