#!/usr/bin/perl -w
#
# Report changes in installed and available packages using APT
# Written by Martin Ebourne. Started 03/06/04
# $Id$

use strict;
use Data::Dumper;
use English;
use AptPkg::Cache;
use AptPkg::Config;
use AptPkg::System;
use AptPkg::Version;
use IO::File;
use POSIX qw(strftime);
use MJE::ParseOpts;
use MJE::Table qw(:all);

# Parse the command line options & give help
my $opts=new MJE::ParseOpts (<<'EOF') || exit 1;
Description:
Report changes in installed and available packages using APT.

Usage:
apt-new.pl [options]

Options:
  -d, --dry-run			Report only, do not update the cache file
				# --dry-run | -d
  -f <file>, --cache-file=<file>
				Use the given cache file and report differences
				# --cache-file | -f : ? file
  -h, --help			Provide this help
				# --help | -h
  -m <addr>, --mail=<addr>	Email the results to the given address
				# --mail | -m : string
  --no-installable		Do not report on installable packages
				# --no-installable
  --no-installed		Do not report on currently installed packages
				# --no-installed
  --no-upgradeable		Do not report on upgradeable packages
				# --no-upgradeable
  -q, --quiet			Do not report to screen
				# --quiet | -q
EOF

# Initialise the package cache
my $cache = new AptPkg::Cache;

# Get a list of package names. Exclude any non-real packages
my @packages = grep { defined($_->{VersionList}) }
               values(%$cache);

my $comparator = $AptPkg::System::_system->versioning;
my $describer = $cache->packages;

my %upgradeable;
my %installable;
my %installed;

# Process each package getting details
for my $package (@packages) {

  # Get the latest available version for the package
  my $latest;
  for my $v (@{$package->{VersionList}}) {
    if(!defined($latest) || $comparator->compare($v->{VerStr}, $latest)>0) {
      $latest = $v->{VerStr};
    }
  }

  my $details = $describer->lookup($package->{Name});
  if($package->{CurrentState} eq "Installed") {

    # If the package has a newer version then it is upgradeable
    if($comparator->compare($latest, "$package->{CurrentVer}->{VerStr}")>0) {
      $upgradeable{$package->{Name}}={
	name => $package->{Name},
	current => $package->{CurrentVer}->{VerStr},
	new => $latest,
	desc => $details->{ShortDesc}
      };
    } else {
      $installed{$package->{Name}}={
	name => $package->{Name},
	current => $package->{CurrentVer}->{VerStr},
	desc => $details->{ShortDesc}
      };
    }
  } else {
    $installable{$package->{Name}}={
      name => $package->{Name},
      new => $latest,
      desc => $details->{ShortDesc}
    };
  }
}

my (%upgradeable_prev, %installable_prev, %installed_prev);

# Read in the previous cache if there was one
if(exists($opts->{"cache-file"})) {
  my $fh = new IO::File $opts->{"cache-file"}, "r";
  if(defined($fh)) {
    my $state = join('',<$fh>);
    eval $state;
  }
}

my (%upgradeable_new, %installable_new, %installed_new);

# Process package status as requested
if(!exists($opts->{"no-upgradeable"})) {
  %upgradeable_new = newEntries(\%upgradeable_prev, \%upgradeable);
}
if(!exists($opts->{"no-installable"})) {
  %installable_new = newEntries(\%installable_prev, \%installable);
}
if(!exists($opts->{"no-installed"})) {
  %installed_new = newEntries(\%installed_prev, \%installed);
}

# Prepare output if there will be any
if(%upgradeable_new || %installed_new || %installable_new) {
  my $output;
  $output.="Package changes for " . strftime("%A %e %B %Y", localtime) . "\n";

  if(%upgradeable_new) {
    $output.="\nNewly upgradable packages:\n\n";
    $output.=formatTable([["Package name", "Current version", "New version", "Description"],
			  ["------- ----", "------- -------", "--- -------", "-----------"],
			  @{convertToTable(\%upgradeable_new, qw(name current new desc))}]);
    $output.="\n";
  }

  if(%installed_new) {
    $output.="\nNewly installed packages:\n\n";
    $output.=formatTable([["Package name", "Version", "Description"],
			  ["------- ----", "-------", "-----------"],
			  @{convertToTable(\%installed_new, qw(name current desc))}]);
    $output.="\n";
  }

  if(%installable_new) {
    $output.="\nNew packages:\n\n";
    $output.=formatTable([["Package name", "Version", "Description"],
			  ["------- ----", "-------", "-----------"],
			  @{convertToTable(\%installable_new, qw(name new desc))}]);
    $output.="\n";
  }

  # Write output to screen
  if(!exists($opts->{"quiet"})) {
    print $output;
  }

  if(exists($opts->{mail})) {
    my $host = qx{hostname -s};
    chomp $host;
    my $fh = new IO::File "|mail -s 'Package report for $host: "
			  . strftime("%A %e %B %Y", localtime)
	                  . "' '" . $opts->{mail} . "'";
    print $fh $output;
  }
} else {
  if(!exists($opts->{"quiet"})) {
    print "No changed packages\n";
  }
}

# Update the cache file contents if requested
if(exists($opts->{"cache-file"}) && !exists($opts->{"dry-run"})) {
  my $fh = new IO::File $opts->{"cache-file"}, "w";
  if(defined($fh)) {
    print $fh Data::Dumper->Dump([\%upgradeable,
				  \%installable,
				  \%installed],
				 [qw(*upgradeable_prev
				     *installable_prev
				     *installed_prev)]);
  } else {
    print STDERR "ERROR: Cannot write cache file '" . $opts->{"cache-file"} . "'\n";
    exit 1;
  }
}

exit 0;

# Return only the new entries in current vs. previous
sub newEntries {
  my ($previous, $current) = @_;
  my %new;

  for my $key (keys(%$current)) {
    if(!exists($previous->{$key})) {
      $new{$key}=$current->{$key};
    }
  }

  return %new;
}
