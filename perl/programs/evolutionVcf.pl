#!/usr/bin/perl -w
#
# Convert Psion VCF files for Evolution
# Written by Martin Ebourne. Started 02/07/03
# $Id: evolutionVcf.pl 792 2003-09-22 11:47:18Z martin $

use strict;

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
