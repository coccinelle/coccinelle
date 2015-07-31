#!/usr/bin/perl

#usage:
# cd tests-big;
# ~/coccinelle/scripts/stat_directories.pl bluetooth/* rules/* megas/*

printf "%-20s  %10s %10s %4s\n", "dir/", "failed" , "total", "%ok";
print "------------------------------------------------------\n";

$totalfailed = 0;
$total = 0;

foreach my $dir (@ARGV) {

  if(-e "$dir/") {
    my ($ok) = `find $dir -name "*.c.*ok" | wc -l`;
#    my ($ok) = `find $dir -name "*ok" | wc -l`;
    chomp $ok;
    my ($failed) = `find $dir -name "*.c.failed" | wc -l`;
#    my ($failed) = `find $dir -name "*failed" | wc -l`;
    chomp $failed;
    $totalfailed += $failed;
    my $sum = $failed + $ok;
    $total += $sum;
    if ($sum == 0) {
      print "$dir/ have 0 sum\n";
    } else {
      my $pourcent = ($ok * 100.0) / ($sum);
      printf "%-20s  %10d %10d %5.1f%%\n", "$dir/", $failed, $sum, $pourcent;
    }
  }


}

my $pourcent = (($total - $totalfailed) * 100.0) / ($total);

print "------------------------------------------------------\n";
printf "total failed = %10d / %10d % 3.1f%%\n",
  $totalfailed, $total, $pourcent;
