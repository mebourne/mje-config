#!/usr/local/bin/perl -w
#
# Parse C++ header files for typedef & class definitions
# Written by Martin Ebourne. Started 23/02/01
# $Id$

use strict;
use IO::File;
use MJE::ParseOpts;

use vars qw(@files);

my $opts=new MJE::ParseOpts (<<'EOF') || exit 1;
Description:
Parse C++ header files for typedef & class definitions to generate a
sourceinfo database.

Usage:
typeInfoGenerate.pl [options] <header-file> ...

Options:
  -h, --help			Provide this help
				# --help | -h

Arguments:
  <header-file> ...		A list of C++ header files to be parsed
	   			# @files += [1,*] header-file : file
EOF

my $result=0;

for my $file (@files) {
  processFile($file);
}

exit $result;


sub processFile {
  my ($file)=@_;

  my $fh=new IO::File "<$file";
  if(!defined($fh)) {
    print STDERR "Cannot open file $file\n";
    $result=1;
  }

  my @text=<$fh>;
  my $text="@text";

  $text=~s/\\\n//g;
  $text=~s:/(/.*?$|\*.*?\*/)::gms;
  $text=~s/^[ \t]*#.*//gm;
  $text=~s/\s+/ /g;
  $text=";" . $text;

  # These horrible hacks for horrible RW (& persistence)
  $text=~s/ RWBehavior//g;
  $text=~s/ RWMemoryPool_OPTION//g;
  $text=~s/ COLON_PS_NEWDELETE//g;
  $text=~s/ PUBLIC_PS_NEWDELETE//g;

#  print "$text\n";
  findTypedefs($file,$text);
  findClasses($file,$text);
}

sub findTypedefs {
  my ($file, $text)=@_;

  my @typedefs=$text=~/(?<=[;}]) *typedef ([^{]*?) *;/g;
  for my $typedef (@typedefs) {
    my ($origType,$ptrType,$newType,$arrayType)=$typedef=~/^(.*?) ?([*&]*)(\w+) *(?:(\[.*\])|)$/;

    if(defined($origType)) {
      # Implicit int
      if($origType eq "") {
	$origType="int";
      }

      # Combine ptr & array components onto base type
      $origType=$origType . $ptrType;
      if(defined($arrayType)) {
	$origType=$origType . " " . $arrayType;
      }

      # Output definition of the form:
      # <filename>#typedef#<defined type>#<original type>
      addField($file,"typedef",$newType,$origType);
    } elsif($typedef=~/\(.*\)$/) {
      # Function typedef. Ignore them for now
    } else {
      print STDERR "Cannot parse typedef '$typedef' in $file\n";
      $result=1;
    }
  }
}

sub findClasses {
  my ($file, $text)=@_;

  my @classes=$text=~/(?<=[;}]) *((?:template *<[^{;]*?> *|)class [^{;]*?) *\{/g;
  for my $class (@classes) {
    # This line has horrible hacks for horrible RW. eg. class RWGExport RWGDlist(type) {}
    my ($template,$decl,$name,$templParam,$base)=
	$class=~/^(?:template *(<.*?>) *|)class( *\w*) (\w+) *(?:\(.*\)|)(<.*?>|) *(?:: *(.*)|)$/;

    if(defined($name)) {
      if(!defined($template)) {
	$template="";
      }
  
      my @bases;
      if(defined($base)) {
	my $finished=0;
	while(!$finished) {
	  my ($left,$right)=$base=~/^(.*?) *, *((public|protected|private|virtual).*)/;
	  if(defined($left)) {
	    push @bases, $left;
	    $base=$right;
	  } else {
	    $finished=1;
	  }
	}
	push @bases, $base;
      }
  
      # Output definition of the form:
      # <filename>#class#<defined class>#<template params>[#<base class>#...]
      addField($file,"class",$name,$template,@bases);
    } else {
      print STDERR "Cannot parse class '$class' in $file\n";
      $result=1;
    }
  }
}

sub addField {
  my $line=join("#",@_);

  print "$line\n";
}
