#!/usr/local/bin/perl -w
#
# Parse type definition file generated from parseCpp.pl
# Written by Martin Ebourne. Started 23/02/01
#
# Usage: typeInfo.pl [-r] [-s] [-d <dir-name>] <definition-file> <type-name>

use strict;
use IO::File;

# These are the settings controlled by parameters
my $recurse=0;
my $short=0;
my $all=0;
my @preferredDirs;

# Handle the option parameters
my $got=1;
while(@ARGV>2 && $got) {
  if($ARGV[0] eq "-r") {
    $recurse=1;
    shift @ARGV;
  } elsif($ARGV[0] eq "-s") {
    $short=1;
    shift @ARGV;
  } elsif($ARGV[0] eq "-a") {
    $all=1;
    shift @ARGV;
  } elsif($ARGV[0] eq "-d") {
    shift @ARGV;
    my $dir=shift @ARGV;
    if($dir!~/^\//) {
      $dir="/" . $dir;
    }
    if($dir!~/\/$/) {
      $dir=$dir . "/";
    }
    $preferredDirs[@preferredDirs]=$dir;
  } else {
    $got=0;
  }
}

# Check the syntax
die "Usage: typeInfo.pl [-r] [-s] [-d <dir-name>] <definition-file> <type-name>" if @ARGV!=2;

# Handle the mandatory parameters
my ($defsFileName,$typeName)=@ARGV;

# Load in the definitions file
my @defs;
&loadFile($defsFileName,\@defs);

if(!$all) {
  # User did not request -a option to list all types

  if($short) {
    # User requested short (hierarchical) display

    print "Base classes:\n";
    &shortTypeInfo(\@defs,[],$typeName,1,"",1);

    print "\nDerived classes:\n";
    &reverseTypeInfo(\@defs,[],$typeName,"",1);
  } else {
    # User requested standard display

    if(!&printTypeInfo(\@defs,[],$typeName,$recurse,"")) {
      print "No definition for $typeName found\n";
    }
  }
} else {
  # User requested -a for all types (debugging functionality)

  for my $def (@defs) {
    my ($fileName,$recordType,$type,@params)=split(/\#/,$def);
    &printTypeInfo(\@defs,[],$type,$recurse,"");
  }
}

exit;


# Load the given file into the given array. If it ends in the extension '.gz' then ungzip it as
# we go
sub loadFile {
  my ($file,$array)=@_;

  my $string;
  if($file=~/\.gz$/) {
    $string="gunzip - < $file |";
  } else {
    $string="<$file";
  }

  my $fh=new IO::File $string;
  die "Cannot open file $file" if !defined($fh);

  chomp(@$array=<$fh>);
}

# Given more than one match, filter for the most preferred one if it is unique, else don't touch
sub preferredMatches {
  my ($preferredDirs,$matches)=@_;

  # Iterate over each of the preferred directories until one is found where only one result is
  # matched
  for(my $i=0; $i<@$preferredDirs && @$matches>1; $i++) {
    my @newMatches=grep(/^[^\#]*$preferredDirs->[$i]/,@$matches);
    if(@newMatches==1) {
      @$matches=@newMatches;
    }
  }

  # Return new number of matches
  return @$matches;
}

# Filter a list of matches which may have duplicate classes so that only the preferred entry
# for each class remains. Raises error if cannot find a preferred one
sub arrangeMatches {
  my($preferredDirs,$matches)=@_;

  # Get a unique list of types in the matches array supplied
  my %types;
  for my $match (@$matches) {
    my ($fileName,$recordType,$type,$trailing)=split(/\#/,$match);
    $types{$type}=1;
  }

  # Iterate over this unique list (sorted) to extract all matches for that type
  my @newMatches;
  for my $type (sort(keys(%types))) {
    my @selectMatches=grep(/^[^\#]*\#[^\#]*\#$type\#/,@$matches);

    # Handle any duplicates
    my $numMatches=&preferredMatches(\@preferredDirs,\@selectMatches);
    if($numMatches>1) {
      die "Got $numMatches matches for $typeName. Can only handle one in short mode.";
    }

    # Build a new list without duplicates
    push @newMatches, @selectMatches;
  }

  # Replace the given list with the new one
  @$matches=@newMatches;
}

# Print verbose information about a given type
sub printTypeInfo {
  my ($defs,$done,$typeName,$recurse,$leader)=@_;

  # Check type has not already been reported (to trap for recursive loops)
  my $numMatches=0;
  if((grep { $_ eq $typeName } @$done)==0) {
    push @$done, $typeName;

    # Look for definitions of this type
    my @matches=grep(/^[^\#]*\#[^\#]*\#$typeName\#/,@$defs);
    $numMatches=@matches;

    # Handle too many definitions
    if($numMatches>1) {
      $numMatches=&preferredMatches(\@preferredDirs,\@matches);

      if($numMatches>1) {
	print "$leader*** Got $numMatches matches for $typeName. Listing all...\n";
      }
    }

    # Display all definitions
    for my $lineInfo (@matches) {
      &printLineInfo($defs,$done,$recurse,$leader,$lineInfo);
    }
  }

  return $numMatches;
}

# Print verbose information about the type described in the definition line given
sub printLineInfo {
  my ($defs,$done,$recurse,$leader,$lineInfo)=@_;

  # Decompose definition into fields
  my ($fileName,$recordType,$type,@params)=split(/\#/,$lineInfo);

  # Check for record type to handle
  if($recordType eq "typedef") {
    print "$leader$type is defined as $params[0] in $fileName\n";

    # If recursion enabled then display info on all types present in this typedef
    if($recurse) {
      my (@possibleTypes)=$params[0]=~/\w+/g;
      for my $possibility (@possibleTypes) {
	&printTypeInfo($defs,$done,$possibility,$recurse,$leader . "  ");
      }
    }
  } elsif($recordType eq "class") {

    # Get template definition if present
    my $template=shift @params;
    if(!defined($template)) {
      $template="";
    }

    print "$leader$type is defined as class $type$template in $fileName\n";

    # Iterate over any base classes
    for my $base (@params) {
      print "$leader  Derived from $base\n";

      # If recursion enabled then display info on all types present in this derivation. (There
      # may be many in the case of a template base class for instance.)
      if($recurse) {
	my (@possibleTypes)=$base=~/\w+/g;
	for my $possibility (@possibleTypes) {
	  &printTypeInfo($defs,$done,$possibility,$recurse,$leader . "    ");
	}
      }
    }
  }
}


# Print summary information about a given type. Basically just prints the class hierarchy
sub shortTypeInfo {
  my ($defs,$done,$typeName,$recurse,$leader,$last)=@_;

  # Check type has not already been reported (to trap for recursive loops)
  my $numMatches=0;
  if((grep { $_ eq $typeName } @$done)==0) {
    push @$done, $typeName;

    # Look for definitions of this type
    my @matches=grep(/^[^\#]*\#[^\#]*\#$typeName\#/,@$defs);
    $numMatches=@matches;

    # Handle too many definitions
    if($numMatches>1) {
      $numMatches=&preferredMatches(\@preferredDirs,\@matches);

      if($numMatches>1) {
        die "Got $numMatches matches for $typeName. Can only handle one in short mode.";
      }
    }

    # If we got one definition just display it. Zero then report undefined
    if($numMatches==1) {
      &shortLineInfo($defs,$done,$recurse,$leader,$last,$matches[0]);
    } else {
      if($leader ne "") {
	$leader=$leader . "+-";
      } else {
	$leader="  ";
      }
      print "${leader}undefined $typeName\n";
    }
  }

  return $numMatches;
}

# Print summary information about the type described in the definition line given
sub shortLineInfo {
  my ($defs,$done,$recurse,$leader,$last,$lineInfo)=@_;

  # Decompose definition into fields
  my ($fileName,$recordType,$type,@params)=split(/\#/,$lineInfo);

  # Calculate line leader for when we print our info
  my $thisLeader=$leader;
  if($thisLeader ne "") {
    $thisLeader=$thisLeader . "+-";
  } else {
    $thisLeader="  ";
  }

  # Check for record type to handle
  if($recordType eq "typedef") {

    # Typedefs are just displayed in summary, and never recursed
    print "$thisLeader$type : $params[0]\n";
  } elsif($recordType eq "class") {

    # Get template definition if present
    my $template=shift @params;
    if(!defined($template)) {
      $template="";
    }

    print "${thisLeader}class $type$template\n";

    # If recursion enabled then display info on all base classes. Note that this behaviour is
    # different to printTypeInfo
    if($recurse) {
      while(@params) {
	my $base=shift @params;
	my ($baseName)=$base=~/(\w+) *(<.*>|)$/g;
	&shortTypeInfo($defs,$done,$baseName,$recurse,
		       $leader . ($last ? " " : "|") . "   ",@params==0);
      }
    }
  }
}


# Print summary information about a given type, but traversing the tree the opposite way. That
# is to say, prints the class hierarchy of derived classes as opposed to base classes
sub reverseTypeInfo {
  my ($defs,$done,$typeName,$leader,$last)=@_;

  # Use the short type display without recursion to describe the type
  &shortTypeInfo($defs,[],$typeName,0,$leader,1);

  # Check type has not already been reported (to trap for recursive loops)
  if((grep { $_ eq $typeName } @$done)==0) {
    push @$done, $typeName;

    # Look for class definitions where this type is a base class
    my @matches=grep(/^[^\#]*\#class\#[^\#]*\#[^\#]*\#.*\b$typeName\b/,@$defs);

    &arrangeMatches(\@preferredDirs,\@matches);

    # Display info on all classes derived from this one
    while(@matches) {
      my $lineInfo=shift @matches;
      my ($fileName,$recordType,$type)=split(/\#/,$lineInfo);
      &reverseTypeInfo($defs,$done,$type,$leader . ($last ? " " : "|") . "   ",@matches==0);
    }
  }
}
