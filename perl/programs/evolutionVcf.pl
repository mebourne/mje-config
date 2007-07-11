#!/usr/bin/env perl
#
# Convert Psion VCF files for Evolution
# Written by Martin Ebourne. Started 02/07/03
# $Id$

use strict;
use warnings;

my $line;
while(defined($line=<>)) {
  $line=~s/;X-.*?(?=[;:])//g;
  $line=~s/^TEL;HOME;CELL/TEL;CELL/;
  if($line=~/^N:/) {
    my $n = $line;
    $n=~s/^N://;
    my ($ln, $fn, $mn, $t, $s) = split(/;/,$n);
    print "FN:$fn $ln\n";
  }
  print $line;
}
