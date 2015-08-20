#!/usr/bin/perl
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
