#!/usr/local/bin/perl -w
#
# Parse C++ header files for typedef & class definitions
# Written by Martin Ebourne. Started 23/02/01
#
# Usage: typeInfoGenerate.pl <header-file> ...

use strict;
use IO::File;

for my $file (@ARGV) {
  &processFile($file);
}

exit;


sub processFile {
  my ($file)=@_;

  my $fh=new IO::File "<$file";
  die "Cannot open file $file" if !defined($fh);

  my @text=<$fh>;
  my $text="@text";

  $text=~s/\\\n//g;
  $text=~s:/(/.*?$|\*.*?\*/)::gms;
  $text=~s/^[ \t]*#.*//gm;
  $text=~s/\s+/ /g;
  $text=";" . $text;

#  print "$text\n";
  &findTypedefs($file,$text);
  &findClasses($file,$text);
}

sub findTypedefs {
  my ($file, $text)=@_;

  my @typedefs=$text=~/(?<=[;}]) *typedef (.*?) *;/g;
  for my $typedef (@typedefs) {
    my ($origType,$newType,$arrayType)=$typedef=~/^(.*?) (\w+) *(?:(\[.*\])|)$/;

    if(defined($origType)) {
      if(defined($arrayType)) {
	$origType=$origType . " " . $arrayType;
      }

      # Output definition of the form:
      # <filename>#typedef#<defined type>#<original type>
      &addField($file,"typedef",$newType,$origType);
    } elsif($typedef=~/\(.*\).*\(.*\)$/) {
      # Function typedef. Ignore them for now
    } else {
      die "Cannot parse typedef '$typedef' in $file";
    }
  }
}

sub findClasses {
  my ($file, $text)=@_;

  my @classes=$text=~/(?<=[;}]) *((?:template *<[^{;]*?> *|)class [^{;]*?) *\{/g;
  for my $class (@classes) {
    my ($template,$decl,$name,$templParam,$base)=
	$class=~/^(?:template *(<.*?>) *|)class( *\w*) (\w+)( *<.*?>|) *(?:: *(.*)|)$/;

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
      &addField($file,"class",$name,$template,@bases);
    } else {
      die "Cannot parse class '$class' in $file";
    }
  }
}

sub addField {
  my $line=join("#",@_);

  print "$line\n";
}
