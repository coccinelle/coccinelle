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
    my ($s) = 
      `cd $dir; ~/mobile/coccinelle/scripts/stat_directory.pl | grep C:`;
    chomp $s;
    $i++;
    #print "M$i.$s\n";
    $s =~ s/C:/C$i./;
    print "$s\n";
  }

}
