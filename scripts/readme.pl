# Copyright 2012-2015, Inria
# Julia Lawall, Gilles Muller
# Copyright 2010-2011, INRIA, University of Copenhagen
# Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
# Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
# Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
# This file is part of Coccinelle.
#
# Coccinelle is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Coccinelle is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Coccinelle under other licenses.


#!/usr/bin/perl
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
