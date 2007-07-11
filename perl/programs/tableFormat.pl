#!/usr/bin/env perl
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01

use strict;
use warnings;

use Data::Dumper;
use English;
use Env;
use MJE::ParseOpts;
use MJE::TableFormatter;

# Generate a simple hash from style name to description for use with
# ParseOpts. Also generate summary for main help page
use vars qw(%stylesHelp);
my $stylesSummary=MJE::TableFormatter::getStylesHelp(\%stylesHelp);

# Options for control of colour
use vars qw(%colourHelp);
%colourHelp=(
  off  => "No colour",
  auto => "Only colour if outputting to tty",
  on   => "Always colour"# (if applicable)"
);

# Parse the command line options & give help
use vars qw(@statements);
@statements=();
my $opts=new MJE::ParseOpts (
'Description:
Execute an SQL command against a database and format the output in a fully
configurable manner.

Usage:
sqlExecute.pl [options] <sql-statement> ...

Options:
  -c <when>, --colour=<when>	Colour output. One of:
				# --colour | -c : values=%colourHelp
				off	No colour
				auto	Only colour if outputting to tty
				on	Always colour (if applicable)
  --column-width=<count>	Specify the maximum column width to be shown
				# --column-width : posinteger
  --count			Display the affected row count
				# --count
  -h, --help			Provide this help
				# --help | -h
  -r <rows>, --max-rows=<rows>	Specify the maximum number of rows to retrieve
				# --max-rows | -r : posinteger
  -s <style>, --style=<style>	The style of formatting to perform. One of:
'.$stylesSummary.'		# --style | -s : values=%stylesHelp
  -w <columns>, --width=<columns>
				Specify the terminal width in characters
				# --width | -w : posinteger
  --width-auto			With --width, only apply if output to tty
				# --width-auto
') || exit 1;

# Note if we are outputting to a tty or a file
my $tty=-t STDOUT;

# Determine if we should use colour. Must be allowed by style, and requested
# by user (possibly dependant on if tty)
my $colour=0;
if(exists($opts->{colour})
   && ($opts->{colour} eq "on"
       || $opts->{colour} eq "auto" && $tty)) {
  $colour=1;
}

# Determine screen width to use
my $screenWidth=$opts->{width};
if(exists($opts->{"width-auto"}) && $opts->{"width-auto"} && !$tty) {
  $screenWidth=undef;
}

&main();

exit;

sub main {
  # Create this now so we can query it
  my $formatter=new MJE::TableFormatter;

  # Configure formatter as per user options
  $formatter->setStyle(exists($opts->{style}) ? $opts->{style} : "none");
  $formatter->{colour}=$colour;
  $formatter->{screenWidth}=$screenWidth;
  $formatter->{maxColumnWidth}=$opts->{"column-width"};

  # Figure out data length for retrieving columns
  my $dataLength = $opts->{"column-width"};
  if(!defined($dataLength) || !$formatter->getStyleInfo("truncate")) {
    $dataLength=64*1024;
  }

  # List the columns for the formatter
  my $line = <>;
  chomp $line;
  for my $heading (split(/\t/,$line)) {
    $formatter->addColumn($heading, 5, "string");
  }

  # Write the data to the formatter
  my $rows=0;
  for $line (<>) {
    if(exists($opts->{"max-rows"}) && $rows>=$opts->{"max-rows"}) {
      last;
    }

    chomp $line;
    $formatter->addRow(split(/\t/,$line));
    $rows++;

    # Report number of rows received if we have a lot of results
    if(($rows%100)==0 && $tty) {
      my $old=$|;
      $|=1;
      print "Received $rows rows...\r";
      $|=$old;
    }
  }

  # Display the table
  $formatter->display;

  if(exists($opts->{count}) && $formatter->getStyleInfo("rowcount")) {
    print "\n", $rows, " row(s) returned.\n";
  }
}
