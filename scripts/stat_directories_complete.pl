#!/usr/bin/perl 

#usage: cd tests/; ../scripts/stat_rules.pl 


my $subdirs = `make subdirs`;
#my $subdirs = "rule9";
#my $subdirs = "rule1";
chomp $subdirs;
@subdirs = split /\s+/, $subdirs;


my $i = 0;
foreach my $dir (@subdirs) {
  if(-e "$dir/") {
    #print "RULE: $dir\n";
    my $kind = "M";
    my ($s) = 
      `cd $dir; ~/coccinelle/scripts/stat_directory_complete.pl | grep $kind:`;
    chomp $s;
    $i++;
    #print "M$i.$s\n";
    $s =~ s/$kind:/$kind$i./;
    print "$s\n";
  }

}
