#!/usr/bin/perl

use strict;
use diagnostics;


#------------------------------------------------------------------------------
# Helpers 
#------------------------------------------------------------------------------

my $debug = 0; 

sub pr2 { print STDERR "@_\n"; }
sub pr { print "@_\n"; }
sub mylog { print STDERR "@_\n" if $debug; }

#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

my $ok = 0;
my $so = 0; #spatch ok 
my $fa = 0; #failed 
my $gu = 0; #gave up

my $nbfiles = 0;

my $maxtime = 0;
#my $mintime = 0;
my $sumtime = 0;

my $sumlinefiles = 0;

my $errors = 0;

my $sumlineP = 0; #whole git
my $sumlineP2 = 0;
my $sumlinePchange = 0;

my $spfile = "";
my $ruleno = "??";
my $sizeSP = 0;

my $cedescr = "";

my @cfiles = ();

#------------------------------------------------------------------------------
# SP
#------------------------------------------------------------------------------
$spfile = `make sp_file`;
chomp $spfile;

if($spfile =~ /(rule|mega|bt)(\d+)\.cocci/) { $ruleno = "$2"; }

#------------------------------------------------------------------------------
# CE
#------------------------------------------------------------------------------

$cedescr = `make ce_descr`;
chomp $cedescr;
$cedescr =~ s//\\f/g;
$cedescr =~ s/\t/\\t/g;

#------------------------------------------------------------------------------
# List c files 
#------------------------------------------------------------------------------
my $files = `make source_files`;
chomp $files;
@cfiles = split /\s+/, $files;

$nbfiles = scalar(@cfiles);

#------------------------------------------------------------------------------
# Size files (lines)
#------------------------------------------------------------------------------
map { 
  my ($linefile) = `wc -l $_`; 
  chomp $linefile;
  die "wierd wc output" unless $linefile =~ /^(\d+) /;
  $sumlinefiles += $1;
  mylog "filesize $_ $1";
} @cfiles;


#------------------------------------------------------------------------------
# Size SP
#------------------------------------------------------------------------------
$sizeSP = 
  `cat $spfile | perl -p -e "s/\\/\\/.*//g;" | grep -v '^[ \t]*\$' | wc -l`;
chomp $sizeSP;


#------------------------------------------------------------------------------
# Bugs
#------------------------------------------------------------------------------
if(!(-e "README")) { pr2 "no README file ?"; }
else {
  open TMP, "README" or die "no README file ?";
  while(<TMP>) { 
    if (/\[bug\]/ || /status\]\s*bug/ || /status\]\s*BUG/ ) { 
      #pr2 "OLD BUG FORMAT: $_"; 
      $errors++ 
    }
  }
}



#------------------------------------------------------------------------------
# Size P (total)
#------------------------------------------------------------------------------

if(-e "gitinfo") { 
  ($sumlineP) = `cat gitinfo |wc -l`;
  chomp $sumlineP;
} else {
  pr2 "no GIT INFO?";
}

#------------------------------------------------------------------------------
# Size P (only change for .c in drivers/ or sounds/ (the test files))
#------------------------------------------------------------------------------


foreach my $c (@cfiles) {
  die "wierd: $c, with $spfile" unless ($c =~ /(.*)\.c$/);
  my $base = $1;
  my $bef = "$base.c";
  my $aft = "$base.res"; 
  if(-e "corrected_$base.res") { 
    $aft = "corrected_$base.res";
    mylog "found corrected";
  }
  my $onlychange = 0;
  open TMP, "diff -u -b -B $bef $aft |";
  
  my $count = 0;
  while(<TMP>) {
    $count++;

    if (/^\+[^+]/) { $onlychange++; }
    if (/^\-[^-]/) { $onlychange++; }
  }
  $sumlinePchange += $onlychange;
  $sumlineP2 += $count;
}

#------------------------------------------------------------------------------
# Time
#------------------------------------------------------------------------------
foreach my $c (@cfiles) {
  die "" unless ($c =~ /(.*)\.c$/);
  my $base = $1;
  
  my $diagnosefile = "";
  mylog "$base";

  if(-e "$base.c.ok")        { $ok++; $diagnosefile = "$base.c.ok"; }
  if(-e "$base.c.failed")    { $fa++; $diagnosefile = "$base.c.failed"; }
  if(-e "$base.c.spatch_ok") { $so++; $diagnosefile = "$base.c.spatch_ok"; }
  if(-e "$base.c.gave_up")   { $gu++; $diagnosefile = "$base.c.gave_up"; }

  open TMP, $diagnosefile or die "no diagnose $base: $diagnosefile";
  my $found = 0;
  my $time = 0;
  while(<TMP>) {
# before -test_okfailed
#    if (/real[ \t]+([0-9])+m([0-9]+)[.]([0-9]+)/) {
#      $found++;
#
#      $time = $1 * 60.0;   # minutes
#      $time += $2;         # seconds
#      $time += $3 / 1000.0; # 1/1000th sec.
#
#      pr2 (sprintf "%4.1fs\n", $time);
#      printf "I: %15s  & %4.1fs\n", $c, $time;
#      
#    }
    if (/time: (.*)/) {
      $found++;

      $time = $1;

      mylog (sprintf "%4.1fs\n", $time);
      printf "I: %15s  & %4.1fs\n", $c, $time;
      
    }


  }
  die "not found time information in $diagnosefile"  unless $found == 1;

  $sumtime += $time;
  $maxtime = $time if $time > $maxtime;
  
}

#------------------------------------------------------------------------------
# Computations
#------------------------------------------------------------------------------

my $correct = $ok + $so;

my $pourcentcorrect = ($correct * 100.0) / $nbfiles;
my $avglines = $sumlinefiles / $nbfiles;
my $avgtime = $sumtime / $nbfiles;


my $ratioPvsSP = $sumlineP / $sizeSP;
my $ratioPvsSP2 = $sumlineP2 / $sizeSP;


#------------------------------------------------------------------------------
# Results
#------------------------------------------------------------------------------


pr "SP = $spfile";
mylog "FILES = \n";
map { mylog "\t$_"; } @cfiles;
pr "----------------------------------------";


pr "!!Total files = $nbfiles";
printf "!!AvgLine = %.1fl\n", $avglines;

#pr "  Correct number = $correct";
printf "!!Correct = %.1f%s\n", $pourcentcorrect, "%";

pr "!!Human errors = $errors";

pr "!!Size SP = $sizeSP";
pr "!!Size P = $sumlineP";
pr "!!Size P (change only) = $sumlinePchange";

printf "!!Ratio P/SP = %3.1f\n", $ratioPvsSP;


printf "!!RunTime = %.1fs\n", $sumtime;
printf "!!MaxTime = %.1fs\n", $maxtime;
printf "!!AvgTime = %.1fs\n", $avgtime;

my $totalstatus = $ok + $fa + $so + $gu;
mylog "----------------------------------------------------------------";
mylog "Sanity checks: nb files vs total status: $nbfiles =? $totalstatus";



printf "L: %20s (r%3s) & %5.1f%% & %5dfi & %2de & %6.1fx & %6.1fs \n", 
 $cedescr, $ruleno, $pourcentcorrect, $nbfiles, $errors, $ratioPvsSP, $sumtime;


# Mega, Complex, Bluetooth
 



#printf "C: %60s & %5d & %5d(%d) %3d & & %6.1fx & %6.1fs(%.1fs) & %2d & %5.0f\\%% \\\\\\hline%% SP: %s  \n", 
# $cedescr, $nbfiles, $sizeSP, $sumlineP2, $sumlinePchange, $ratioPvsSP2,
# $avgtime, $maxtime, $errors, $pourcentcorrect, $spfile;


printf "C: %60s & %5d & %5d (%d) & %3d & %6.0fx & %6.1fs (%.1fs) & %2d & %5.0f\\%% \\\\\\hline%% SP: %s  \n", 
 $cedescr, $nbfiles, $sumlineP2, $sumlinePchange, $sizeSP, $ratioPvsSP2,
 $avgtime, $maxtime, $errors, $pourcentcorrect, $spfile;

printf "M: %60s & %5d & %5d (%d) & %2d & %3d & %6.0fx & %6.1fs (%.1fs)  & %5.0f\\%% \\\\\\hline%% SP: %s  \n", 
 $cedescr, $nbfiles, $sumlineP, $sumlinePchange, $errors, $sizeSP, $ratioPvsSP,
 $avgtime, $maxtime, $pourcentcorrect, $spfile;

printf "B: %60s & %5d & %5d (%d) & %3d & %6.0fx & %6.1fs (%.1fs) & %5.0f\\%% \\\\\\hline%% SP: %s  \n", 
 $cedescr, $nbfiles, $sumlineP, $sumlinePchange, $sizeSP, $ratioPvsSP,
 $avgtime, $maxtime, $pourcentcorrect, $spfile;


