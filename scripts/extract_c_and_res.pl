#!/usr/bin/perl -w
use strict;

sub pr2 { print "$_[0]\n"; }
sub mylog { print @_;}


# to be launched from the git directory
die "usage: $0 commithashafter [commithashbefore]" 
  if(@ARGV <= 0 || @ARGV >= 3);


my $target_dir = "/tmp/extract_c_and_res/$ARGV[0]";
`mkdir -p $target_dir`;
my $old_dir = "/tmp/extract_c_and_res/$ARGV[0]_old";
`mkdir -p $old_dir`;
my $new_dir = "/tmp/extract_c_and_res/$ARGV[0]_new";
`mkdir -p $new_dir`;

my $commit1 = $ARGV[0];
my $commit2 = $ARGV[1] || "$commit1^"; # default parent 

my $tmpfile = "$target_dir/$commit1.gitinfo";



`git show $commit1 > $tmpfile `;

open FILE, "$tmpfile" or die "$!";

my @files = ();
my $files = {};

while(<FILE>) {

  # allow other dir ? # fs|mm   there is drivers under arch/ too
  if(/^diff --git a\/((drivers|sound)\/.*?\.[ch]) b/){ 
        mylog "  $1\n";
        push @files, $1;
        $files->{$1} = 1;                
                        
    }
    #if(/^diff --git a\/(sound\/.*?) b/){}
    elsif(/^diff --git a\//) {
      mylog " not driver:$_";
    }
    elsif(/^diff/) {
      die "strange diff line: $_";
    }
}

my $counter=0;

# to be able to later find the corresponding local included header file
my $kerneldir_of_file = {};

foreach my $f (@files) {
  my ($base) = `basename $f`;
  chomp $base;
  my $res = $base;
  if($base =~ /\.c$/) {
    $res =~ s/\.c$/.res/;
  } 
  if($base =~ /\.h$/) {
    $res =~ s/\.h$/.h.res/;
  } 

  pr2 "processing: $f $base $res";
  if(-e "$target_dir/$base") {
    $counter++;                              
    $base = "${counter}_$base";
    $res = "${counter}_$res";
    pr2 "try transform one file because already exist: $base";
    if($base =~ /\.h$/) {
      die "Two header files share the same name: $base.";
    }

  }                         
  die "one of the file already exist: $base" if (-e "$target_dir/$base");

  `git-cat-file blob $commit2:$f > $target_dir/$base`;
  `git-cat-file blob $commit1:$f > $target_dir/$res`;
  `git-cat-file blob $commit2:$f > $old_dir/$base`;
  `git-cat-file blob $commit1:$f > $new_dir/$base`;

  $kerneldir_of_file->{$base} = `dirname $f`;
  chomp $kerneldir_of_file->{$base};


}

# compute other local headers

my @headers  = `cd $target_dir; grep -E \"#include +\\".*\\"\" *.c *.h`;

my $hfiles = {};
foreach my $line (@headers) {
  chomp $line;
  #pr2 ($line);
  if($line =~ /^(.*)?:#include *"(.*)"/) {

    my ($file, $header) = ($1, $2);
    my $dir = $kerneldir_of_file->{$file};

    my $fullheader = "$dir/$header";
    #pr2 ($fullheader);

    if($files->{$fullheader}) {
      pr2 "INFO: $fullheader was already in commit";
    } else {
      $hfiles->{$fullheader} = 1;
    }
    
  } else { die "pb regexp: $line"; }
  
}


foreach my $h (keys %{$hfiles}) {
  my ($base) = `basename $h`;
  chomp $base;
  pr2 "processing additionnal header file: $h $base";

  if(-e "$target_dir/$base") {
    die "local header $base already exists";
  } else {

    `git-cat-file blob $commit2:$h > $target_dir/$base`;
  }
  
}
