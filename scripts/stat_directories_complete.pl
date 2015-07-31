#!/usr/bin/perl

#usage:

if(@ARGV < 1) { die "usage: stat_directories_complete.pl  [M|C|B]";}
my $kind = "$ARGV[0]";


my $subdirs = `make subdirs`;
#my $subdirs = "rule9";
#my $subdirs = "rule1";
chomp $subdirs;
@subdirs = split /\s+/, $subdirs;


my $i = 0;
foreach my $dir (@subdirs) {
  if(-e "$dir/") {
    #print "RULE: $dir\n";

    my ($s) =
      `cd $dir; ~/coccinelle/scripts/stat_directory_complete.pl | grep $kind:`;
    chomp $s;
    $i++;
    #print "M$i.$s\n";
    $s =~ s/$kind:/$kind$i./;
    print "$s\n";
  }

}
