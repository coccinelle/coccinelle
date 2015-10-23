#!/usr/bin/perl

# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

use strict;
use diagnostics;

#use Data::Dumper;
#use Date::Manip qw(ParseDate UnixDate); #sudo apt-get install libdate-manip-perl
#use Date::Calc qw(Delta_Days); #sudo apt-get install libdate-calc-perl

#------------------------------------------------------------------------------
# Helpers
#------------------------------------------------------------------------------

my $debug = 0;

sub pr2 { print STDERR "@_\n"; }
sub pr { print "@_\n"; }
sub mylog { print STDERR "@_\n" if $debug; }

sub plural {
  my ($n) = @_;
  $n > 1 ? "s" : "";
}

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

my $numauthors = 0;
my $duration = 0; # in days

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

#$cedescr = `make ce_descr`;
#chomp $cedescr;
#print STDERR (Dumper($cedescr));
open TMP, "Makefile" or die "no Makefile file ?";
while(<TMP>) {
  if(/^(CE)?DESCRIPTION=["'](.*)["']/) {  $cedescr = $2; }
}



#$cedescr =~ s/\\/\\\\/g;
#$cedescr =~ s//\\f/g;
#$cedescr =~ s/\t/\\t/g;

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
    if (/\[bug\]/ || /status\]\s*bug/ ||  /status\]\s*BUG/ ) {

      # can also look for [semibug] but it's often related to [corrected_c] kind of pbs
      #|| /status\]\s*semi-bug/

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
# Number of authors and duration
#------------------------------------------------------------------------------


if(-e "gitinfo") {

  open TMP, "gitinfo" or die "no gitinfo file ?";

  #for authors
  my $h = {};

  #for duration
  my @mindate = ();
  my @maxdate = ();
  my $nodateyet = 1;

  while(<TMP>) {
    #can also do: egrep "^Author" gitinfo | sort | uniq | wc -l
    if (/^Author: (.*)/) {
      $h->{$1}++;
    }

#    if(/^Date: (.*) ([-+]\d+)?/) {
#      my $date = ParseDate($1);
#      if (!$date) { die "bad date" }
#      else {
#        my ($year, $month, $day) = UnixDate($date, "%Y", "%m", "%d");
#        my @current = ($year, $month, $day);
#        if($nodateyet) {
#          @mindate = @current;
#          @maxdate = @current;
#          $nodateyet = 0;
#        } else {
#          my $diff1 = Delta_Days(@mindate, @current);
#          if($diff1 < 0) { @mindate = @current; }
#          my $diff2 = Delta_Days(@current, @maxdate);
#          if($diff2 < 0) { @maxdate = @current; }
#
#          #pr2 "$diff1, $diff2";
#        }
#      }
#    }



  }

#  my $diff = Delta_Days(@mindate, @maxdate);
#  if($diff == 1 || $diff == 0) {
#    $duration = "1 day";
#  }
#  elsif($diff < 31) {
#    $duration = "$diff days";
#  }
#  elsif($diff > 365) {
#    my $years = int($diff / 365);
#    my $s = plural($years);
#    $duration = "$years year$s";
#  }
#  elsif($diff > 31) {
#    my $months = int($diff / 31);
#    my $s = plural($months);
#    $duration = "$months month$s";
#  }
#  else { die "impossible"; }

  $duration = "xxx months";

  $numauthors = scalar(keys %{$h});
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

printf "M: %60s & %5d & %6d (%d) & %2d & %s & %2d & %3d & %6.0fx & %6.1fs (%.1fs)  & %5.0f\\%% \\\\\\hline%% SP: %s  \n",
 $cedescr, $nbfiles, $sumlineP, $sumlinePchange, $numauthors, $duration, $errors, $sizeSP, $ratioPvsSP,
 $avgtime, $maxtime, $pourcentcorrect, $spfile;

printf "C: %60s & %5d & %6d (%d) & %2d & %3d & %6.0fx & %6.1fs (%.1fs)  & %5.0f\\%% \\\\\\hline%% SP: %s  \n",
 $cedescr, $nbfiles, $sumlineP, $sumlinePchange, $errors, $sizeSP, $ratioPvsSP,
 $avgtime, $maxtime, $pourcentcorrect, $spfile;

printf "B: %60s & %5d & %5d (%d) & %3d & %6.0fx & %6.1fs (%.1fs) & %5.0f\\%% \\\\\\hline%% SP: %s  \n",
 $cedescr, $nbfiles, $sumlineP, $sumlinePchange, $sizeSP, $ratioPvsSP,
 $avgtime, $maxtime, $pourcentcorrect, $spfile;
