#!/usr/local/bin/perl -w
#
# Perform various release labelling tasks
# Written by Martin Ebourne
# $Id: releaseLabel.pl 811 2003-10-17 15:45:46Z martin $

use strict;
use Data::Dumper;
use English;
use Env;
use File::stat;
use MJE::ParseOpts;

# Parse the command line options & give help
my $opts=new MJE::ParseOpts (<<'EOF') || exit 1;
Description:
Perform various release labelling tasks. Acts on a list of files which should
be provided on stdin.

Usage:
releaseLabel.pl [options] <label> ...

Options:
  -c, --check			Check given versions are labelled
				# --check | -c
  -h, --help			Provide this help
				# --help | -h
  -l, --label			Label given versions
				# --label | -l

Arguments:
  <label>			Clearcase label for release
				# [release] = label : string
EOF

my %filedata;

while(<>) {
  chomp;

  # Ignore comment lines starting with '#'
  if(!/^\s*\#/) {

    # Convert
    #   Checked in "qm/src/workflowmanagement/etc/workflowmanagement.cfg" version "/main/17".
    # to
    #   qm/src/workflowmanagement/etc/workflowmanagement.cfg@@/main/17
    s/^.*\"(.*)\".*\"(.*)\".*$/$1\@\@$2/;

    # Decode
    #   qm/src/workflowmanagement/etc/workflowmanagement.cfg@@/main/17
    # into file, branch & version
    my ($file, $branch, $version) = /^(.*)\@\@(.*)\/([^\/]*)$/;

    # Ignore any non-matching lines
    if(defined($file)) {

      if($version!~/^(\d+)$/) {
	print STDERR "\nERROR: Version appears to be a label for file $file ($version)\n";
	exit 1;
      }

      # Make pathnames absolute
      if($file!~/^\//) {
	$file=$ENV{PWD} . "/" . $file;
	$file=~s/\/\.\//\//g;
      }

      # Select latest version
      if(!exists($filedata{$file})) {
	$filedata{$file}={ name   => $file,
			   branch => $branch,
			   chosen => $version };
      } elsif($branch ne $filedata{$file}{branch}) {
	print STDERR "\nERROR: Branch mismatch for file $file ($branch and $filedata{$file}{branch}\n";
	exit 1;
      } elsif($version>$filedata{$file}{chosen}) {
	$filedata{$file}{chosen}=$version;
      }
    }
  }
}

print "\nThese are the requested versions:\n\n";
for my $file (sort(keys(%filedata))) {
  my $data = $filedata{$file};

  print &fullName($data), "\n";

  &readVersions($data);

  if($data->{chosen}>$data->{latest}) {
    print STDERR "\nERROR: Version for file $file does not exist ($data->{chosen}, latest is $data->{latest})\n";
    exit 1;
  }
}

if($opts->{check} || $opts->{label}) {
  print "\nLooking for out of date versions...\n\n";
  for my $data (values(%filedata)) {
    if($data->{chosen}<$data->{latest}) {
      print &fullName($data), " is superseded by version $data->{latest}\n";
    }
  }
}

if($opts->{check}) {
  print "\nChecking for unlabelled versions...\n\n";
  for my $data (values(%filedata)) {
    if(exists($data->{labels}{$opts->{release}})) {
      my $labelled = $data->{versions}{$data->{labels}{$opts->{release}}};
      if($data->{chosen}<$labelled) {
        print &fullName($data), " is selected (label at version $labelled)\n";
      } elsif($data->{chosen}>$labelled) {
        print "WARNING: ", &fullName($data), " is *NOT* selected (label at version $labelled)\n";
      }
    } else {
        print "WARNING: ", &fullName($data), " is *NOT* labelled at any version\n";
    }
  }
}

if($opts->{label}) {
  print "\nLabelling files...\n\n";
  for my $data (values(%filedata)) {
    my $doLabel = 0;
    if(exists($data->{labels}{$opts->{release}})) {
      my $labelled = $data->{versions}{$data->{labels}{$opts->{release}}};
      if($data->{chosen}<$labelled) {
        print &fullName($data), " is already selected (label at version $labelled)\n";
      } elsif($data->{chosen}>$labelled) {
        $doLabel=1;
        if($data->{chosen}>$labelled+1) {
          print "WARNING: ", &fullName($data), " will jump label by " . ($data->{chosen}-$labelled) . " versions\n";
        }
      } else {
        print &fullName($data), " is already selected\n";
      }
    } else {
      $doLabel=1;
    }

    if($doLabel) {
#      print "cleartool mklabel -replace $opts->{release} \"" . &fullName($data) . "\"\n";
      system("cleartool mklabel -replace $opts->{release} \"" . &fullName($data) . "\"");
    }
  }
}

exit 0;

sub readVersions {
  my ($data) = @_;

  # List the 'version directory' by using the version extended pathname
  # For each version get its inode - we can use this as a unique ID for
  # mapping between labels & versions
  my $versionDir = "$data->{name}\@\@$data->{branch}";
  opendir(DIR, $versionDir) || die "Couldn't open clearcase version directory $versionDir - $!" ;
  for my $version (readdir DIR) {
    my $stat=stat("$versionDir/$version");
    $data->{labels}{$version}=$stat->ino;

    if($version=~/^(\d+)$/) {
      $data->{versions}{$stat->ino}=int($version);
    }
  }
  close DIR;

  $data->{latest}=$data->{versions}{$data->{labels}{LATEST}};
}

sub fullName {
  my ($data) = @_;

  return "$data->{name}\@\@$data->{branch}/$data->{chosen}";
}
