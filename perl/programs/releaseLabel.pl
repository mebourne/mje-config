#!/usr/bin/env perl
#
# Perform various release labelling tasks
# Written by Martin Ebourne
# $Id$

use strict;
use warnings;

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
  -d, --diff			Show diff output for each version
				# --diff | -d
  -g, --log			Display log entry for each version
				# --log | -g
  -h, --help			Provide this help
				# --help | -h
  -l, --label			Label given versions
				# --label | -l
  -m, --monochrome		Do not use colour output
				# --monochrome | -m

Arguments:
  <label>			Clearcase label for release
				# [release] = label : string
EOF

my %colours=(
  reset         => "\e[00m",
  bold          => "\e[01m",
  darkred       => "\e[31m",
  darkgreen     => "\e[32m",
  darkyellow    => "\e[33m",
  darkblue      => "\e[34m",
  darkmagenta   => "\e[35m",
  darkcyan      => "\e[36m",
  grey          => "\e[37m",
  lightred      => "\e[38;5;9m",
  lightgreen    => "\e[38;5;10m",
  lightyellow   => "\e[38;5;11m",
  lightblue     => "\e[38;5;12m",
  lightmagenta  => "\e[38;5;13m",
  lightcyan     => "\e[38;5;14m",
  white         => "\e[38;5;15m",
);

if($opts->{monochrome}) {
  for my $key (keys(%colours)) {
    $colours{$key} = "";
  }
}

my %filedata;
my %logEntries;

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
    my ($file, $branch, $version) = /^\s*(.*)\@\@(.*)\/([^\/]*?)\s*$/;

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
        $file=~s/\/\.$//g;
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

  print fullName($data), "\n";

  readVersions($data);

  if($data->{chosen}>$data->{latest}) {
    print STDERR "\nERROR: Version for file $file does not exist ($data->{chosen}, latest is $data->{latest})\n";
    exit 1;
  }

  if($opts->{log}) {
    my $command = "cleartool describe -fmt \"%Nc\" \"" . fullName($data) . "\"";
    my $log = qx{$command};
    $data->{log}=$log;
    if(exists($logEntries{$log})) {
      push @{$logEntries{$log}}, $data;
    } else {
      $logEntries{$log}=[$data];
    }
  }
}

if($opts->{log}) {
  print "\nLog entries:\n";
  for my $log (keys(%logEntries)) {
    print "\n";
    for my $data (@{$logEntries{$log}}) {
      print fullName($data), ":\n";
    }
    print "  $colours{bold}\"$log\"$colours{reset}\n";
  }
}

if($opts->{diff}) {
  print "\nDifferences:\n\n";
  for my $file (sort(keys(%filedata))) {
    if(-f $file) {
      my $data = $filedata{$file};
      my %tempData = %$data;
      $tempData{chosen}--;
      my $command = "diff -uw \"" . fullName(\%tempData) . "\" \"" . fullName($data) . "\"";
      my @text = qx{$command};
      for(my $i = 0; $i<@text; $i++) {
        $_=$text[$i];
        if(/^[-+][-+]/) {
          my ($start, $middle, $end) = /^(\S*\s*)(\S*)(.*)$/;
          $_=$start . $colours{bold} . $middle . $colours{reset} . $end . "\n";
        } elsif(/^-/) {
          $_=$colours{lightmagenta} . $_ . $colours{reset};
        } elsif(/^\+/) {
          $_=$colours{lightcyan} . $_ . $colours{reset};
        } elsif(/^\@/) {
          $_=$colours{darkblue} . $_ . $colours{reset};
        } else {
          $_=$colours{grey} . $_ . $colours{reset};
        }
        $text[$i]=$_;
      }
      print @text;
    }
  }
}

if($opts->{check} || $opts->{label}) {
  print "\nLooking for out of date versions...\n\n";
  for my $data (values(%filedata)) {
    if($data->{chosen}<$data->{latest}) {
      print fullName($data), " is superseded by version $data->{latest}\n";
    }
  }
}

if($opts->{check}) {
  print "\nChecking for unlabelled versions...\n\n";
  for my $data (values(%filedata)) {
    if(exists($data->{labels}{$opts->{release}})) {
      my $labelled = $data->{versions}{$data->{labels}{$opts->{release}}};
      if($data->{chosen}<$labelled) {
        print fullName($data), " is selected (label at version $labelled)\n";
      } elsif($data->{chosen}>$labelled) {
        print "$colours{bold}$colours{lightred}WARNING: ", fullName($data);
        print " is *NOT* selected (label at version $labelled)$colours{reset}\n";
      }
    } else {
        print "$colours{bold}$colours{lightred}WARNING: ", fullName($data);
        print " is *NOT* labelled at any version$colours{reset}\n";
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
        print fullName($data), " is already selected (label at version $labelled)\n";
      } elsif($data->{chosen}>$labelled) {
        $doLabel=1;
        if($data->{chosen}>$labelled+1) {
          print "$colours{bold}$colours{lightred}WARNING: ", fullName($data);
	  print " will jump label by ";
          print $data->{chosen}-$labelled, " versions$colours{reset}\n";
        }
      } else {
        print fullName($data), " is already selected\n";
      }
    } else {
      $doLabel=1;
    }

    if($doLabel) {
      system("cleartool mklabel -replace $opts->{release} \"" . fullName($data) . "\"");
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
