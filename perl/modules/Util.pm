# Perl package MJE::Util
# Provide general utility functions
# Written by Martin Ebourne, 06/08/2001
#
# Usage:
#
#   use MJE::Util qw(:all);  # Note this imports all symbols

package MJE::Util;

require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw();
@EXPORT_OK = qw(
		&getFileList
		&printHash
		&sortFileListByTime
		&valid
		);
%EXPORT_TAGS = ( all => \@EXPORT_OK );

use strict;
use warnings;

use File::stat;


# Validate a data item. Returns the first of $value, $default, or "" which is defined
sub valid {
  my ($value,$default)=@_;

  if(defined($value)) {
    return $value;
  } elsif(defined($default)) {
    return $default;
  } else {
    return "";
  }
}


### File listing

# Returns an array containing the list of files in the given directory which
# match the given regular expression (applied to leaf name only). Files
# returned with full paths
sub getFileList {
  my ($dir, $matcher)=@_;

  # Read all filenames from the directory
  opendir(DIR, $dir) || die "Couldn't open directory $dir - $!" ;
  my @files=readdir DIR;
  close DIR;

  # List only matching files
  @files=grep(/$matcher/,@files);

  # Prepend path name to file
  @files=map {"$dir/$_"} @files;

  return @files;

}


# Sort the given list of files (including full paths) by increasing last
# modification time
sub sortFileListByTime {
  my (@files)=@_;

  # Sort by last modification time
  @files=sort {
    my $stata=stat($a);
    my $statb=stat($b);
    $stata->mtime <=> $statb->mtime;
  } @files;

  return @files;
}


### These are useful for debugging

# Print a (possibly recursive) hash reference in a friendly manner
sub printHash {
  my ($hash,$indent)=@_;
  $indent=valid($indent);

  for my $key (sort(keys(%$hash))) {
    my $value=$hash->{$key};
    if(ref($value)) {
      print "$indent$key = {\n";
      printHash($value,"$indent  ");
      print "$indent}\n";
    } else {
      print "$indent$key = $value\n";
    }
  }
}

return 1;
