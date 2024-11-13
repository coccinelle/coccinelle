#!/usr/bin/perl

# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/

#usage:  ./extract_examples.pl ~/week-end/working-documents/examples.tex

my $ex = 0;
my $are_in = 0;
while(<>) {

    if(/\\section{/) {
        $ex++;
        open TMP, ">$ex.cocci" or die "$!";
    }

    if(/begin{verbatim}/) {
        $are_in = 1;
        #old: open TMP, ">$ex.cocci" or die "$!";
    } elsif(/end{verbatim}/) {
        $are_in = 0;
        #old: $ex++;
    } else {
        if($are_in) {  print TMP "$_"; }
    }
}
