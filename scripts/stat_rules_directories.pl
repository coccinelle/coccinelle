#!/usr/bin/perl 

#usage: cd tests/; ../scripts/stat_rules.pl 


my $subdirs = `make subdirs`;
#my $subdirs = "rule9";
#my $subdirs = "rule1";
chomp $subdirs;
@subdirs = split /\s+/, $subdirs;


foreach my $dir (@subdirs) {
  if(-e "$dir/") {
    print "RULE: $dir\n";
    system("cd $dir; ~/mobile/coccinelle/scripts/stat_directory.pl");
  }

}
