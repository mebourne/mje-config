#!/usr/local/bin/perl -w
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01

use strict;
use English;
use MJE::ParseOpts;

# Note if we are outputting to a tty or a file
my $tty=-t STDOUT;

use vars qw(%styles);
%styles=(
  simple  => "Simple formatting keeping the default output style",
  squash  => "As simple but reduces column widths to save on screen space",
  bcp     => "BCP compatible output, tab separated columns with no header",
  record  => "Record style output, row by row",
  emacs   => "Output directly to Emacs in forms mode"
);

my $opts=new MJE::ParseOpts (<<'EOF') || exit 1;
Description:
Format SQL output to make it easier to read.

Usage:
sqlformat.pl [options] <style>

Options:
  -c, --colour			Colour output (ignored for bcp and emacs)
				# --colour | -c
  -h, --help			Provide this help
				# --help | -h

Arguments:
  <style>			The style of formatting to perform. One of:
				simple   Simple formatting keeping the default output style
				squash   As simple but reduces column widths to save on screen space
				bcp      BCP compatible output, tab separated columns with no header
				record   Record style output, row by row
				emacs    Output directly to Emacs in forms mode
	   			# style : values=%styles
EOF

my $inputLineNum=0;
my $maxFieldNameLen=0;

&main();

exit;

sub main {
  my $tempFileBase="/tmp/sqlformat.$PID.";
  my $tempFileCount=1;

  # Read the input two lines at a time (but looping once per line), looking for a table header
  my $thisLine=&getNextLine(0);
  while(defined($thisLine)) {
    my $nextLine=&getNextLine(0);

    # If both lines are the same length and the second one consists only of
    # spaces and dashes then assume we've found the start of a select output
    if(defined($nextLine) && length($thisLine)==length($nextLine)
       && $nextLine=~/^ *-[- ]*$/) {

      # Read the table in
      my @columns;
      my @rows;
      &readHeader(\@columns,$thisLine,$nextLine);
      &readData(\@columns, \@rows, length($thisLine), \$nextLine);
      
      # Processing
      if($opts->{style} eq "squash") {
	&trimLength(\@columns);
      }
      
      # Write the output
      if($opts->{style} eq "simple" || $opts->{style} eq "squash") {
	&simple_printHeader(\@columns);
	if(@rows) {
	  my $format=&simple_generateFormat(\@columns);
	  &simple_printData($format,\@rows);
	}
      } elsif ($opts->{style} eq "bcp") {
	if(@rows) {
	  my $format=&bcp_generateFormat(\@columns);
	  &bcp_printData($format,\@rows);
	}
      } elsif ($opts->{style} eq "record") {
	if(@rows) {
	  my $format=&record_generateFormat(\@columns);
	  &record_printData($format,\@rows);
	}
      } elsif ($opts->{style} eq "emacs") {
	&emacs_writeControl(\@columns,$tempFileBase . $tempFileCount);
	&emacs_writeData(\@rows,$tempFileBase . $tempFileCount);
	$tempFileCount++;
      }

      # Ensure a newline is present. Helps make desc output easier to read
      if($nextLine ne "\n" && $opts->{style} ne "emacs") {
	print "\n";
      }
    } else {
      # Not a table header, so just print the line
      if($opts->{style} ne "bcp" && $opts->{style} ne "emacs") {
	print "$thisLine";
      }
    }
    $thisLine=$nextLine;
  }

  if($opts->{style} eq "emacs") {
    my ($formFiles, $allFiles);
    for(my $i=1;$i<$tempFileCount;$i++) {
      $formFiles.=" $tempFileBase$i.form";
      $allFiles.=" $tempFileBase$i.form $tempFileBase$i.data";
    }

    system("emacsclient --no-wait $formFiles");
    system("( sleep 60 ; rm -f $allFiles ) &");
  }
}

sub getNextLine {
  my ($feedback)=@_;

  my $line=<STDIN>;
  $inputLineNum++;
  if(($inputLineNum%100)==0 && $feedback && $tty) {
    my $old=$|;
    $|=1;
    print "Received $inputLineNum lines...\r";
    $|=$old;
  }

  return $line;
}

# Trim the given parameter for leading and trailing spaces, and return it
sub trim {
  my ($result)=$_[0]=~/^ *(.*?) *$/;
  return $result;
}

# Read the first two lines of the input to get the column information
sub readHeader {
  my ($columns,$header,$format)=@_;

  # First line is like  'columnname1 columnname2'
  # Second line is like '----------- -----------'

  # Iterate over the blocks of '---' in the format line
  my $index=0;
  while($format=~/^( *)(-+)/) {
    my ($gap, $dashes)=($1, $2);

    # Calculate the position of the '---'
    my $start=length($gap);
    my $length=length($dashes);
    my $end=$start+$length;

    # Extract the column name from the corresponding part of the header line
    my $name=&trim(substr($header,$start, $length));

    # Create a hash for this column, in the column list
    push @$columns, { name => $name, start => $index+$start, length => $length, datalength => 0 };

    if(length($name)>$maxFieldNameLen) {
      $maxFieldNameLen=length($name);
    }

    # Modify header & format lines for next column
    $format=substr($format,$end);
    $header=substr($header,$end);
    $index+=$end;
  }
}

# Read all of the data lines
sub readData {
  my ($columns, $rows, $lineLength, $nextLine)=@_;

  # Loop for each line until the length doesn't match, or we run out
  my $line;
  while(defined($line=&getNextLine(1)) && length($line)==$lineLength) {
    chomp $line;
    my $row=[];

    # Iterate over each column
    my $x;
    for($x=0;$x<@$columns;$x++) {
	my $column=$$columns[$x];

	# Extract the value
	my $value=substr($line,$$column{start},$$column{length});

	# Check if column alignment has already been decoded. If not have a go
	if(!defined($$column{align})) {
	  if($value=~/^ .*[^ ]$/) {
	    $$column{align}=1;
	  } elsif($value=~/^[^ ].* $/) {
	    $$column{align}=-1;
	  }
	}

	# Trim the data value (can't do this before alignment check), and
	# check for the maximum datalength
	$value=&trim($value);
	if(length($value)>$$column{datalength}) {
	  $$column{datalength}=length($value);
	}

	# Check if column datatype has already been decoded. If not have a go
	if(!defined($$column{type})) {
	  if(defined($$column{align}) && $$column{align}<0) {
	    $$column{type}="string";
	  } elsif($value=~/^[0-9.Ee+-]+$/) {
	    $$column{type}="number";
	  } elsif($value=~/[A-Z][a-z]{2} [0-9]{2} [0-9]{4}/
		  || $value=~/[0-9]{2}:[0-9]{2}/) {
	    $$column{type}="datetime";
	  }
	}

	# Store the value
	push @$row, $value;
    }

    # Store the row
    push @$rows, $row;
  }

  # If outputting to tty ensure line has been blanked of any 'Received...' output
  if($tty) {
    print "\e[K";
  }

  # Pass the unmatched line back for possible reuse as a header
  $$nextLine=$line;
}

# For each column shrink length such that it will still fit all data and
# column title
sub trimLength {
  my ($columns)=@_;

  my $x;
  for($x=0;$x<@$columns;$x++) {
    my $column=$$columns[$x];
    if(length($$column{name})>$$column{datalength}) {
      $$column{length}=length($$column{name});
    } else {
      $$column{length}=$$column{datalength};
    }
  }
}

# Generate a printf format string for printing a row
sub simple_generateFormat {
  my ($columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    if($x) {
      if($opts->{colour}) {
	$format.="\e[38;5;4m\xb7";
      } else {
	$format.=" ";
      }
    }

    # Write a '%<num>s' entry as appropriate
    my $column=$$columns[$x];
    my $align=$$column{align};
    $align=-1 if !defined($align);

    if($opts->{colour}) {
      my $type=$$column{type};
      $type="string" if !defined($type);
      if($type eq "string") {
	$format.="\e[38;5;11m";
      } elsif($type eq "number") {
	$format.="\e[38;5;214m";
      } elsif($type eq "datetime") {
	$format.="\e[38;5;14m";
      }
    }
    $format.="%" . $align*$$column{length} . "s";
  }
  $format.="\e[38;5;15m" if $opts->{colour};

  return $format;
}

# Print the header
sub simple_printHeader {
  my ($columns)=@_;

  my $header;
  my $underline;

  # Process each column
  my $x;
  for($x=0;$x<@$columns;$x++) {
    my $column=$$columns[$x];

    # Get padded column name
    my $value=sprintf("%*s", -$$column{length}, $$column{name});

    if($x) {
      $header.=" ";
      $underline.=" ";
    }

    # Add to header line and convert to '---' before adding to underline line
    $header.=$value;
    $value=~s/./-/g;
    $underline.=$value;
  }

  # Output header in green
  $opts->{colour} && print "\e[38;5;10m";
  print "$header\n$underline\n";
  $opts->{colour} && print "\e[38;5;15m";
}

# Print the data
sub simple_printData {
  my ($format, $rows)=@_;

  # Print each row with the precalculated format
  my $background=0;
  for my $row (@$rows) {
    if($opts->{colour}) {
      if($background) {
	print "\e[48;5;233m";
      }
      $background=!$background;
    }
    printf $format, @$row;

    print "\e[48;5;0m" if $opts->{colour};
    print "\n";
  }
}

# Generate a printf format string for printing a row
sub bcp_generateFormat {
  my ($columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    if($x) {
      $format.="\t";
    }
    $format.="%s";
  }

  return $format;
}

# Print the data
sub bcp_printData {
  my ($format, $rows)=@_;

  # Print each row with the precalculated format
  for my $row (@$rows) {
    printf $format, @$row;
    print "\n";
  }
}

# Generate a printf format string for printing a row
sub record_generateFormat {
  my ($columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    # Write a '%<num>s' entry as appropriate
    my $column=$$columns[$x];
    $format.="\e[38;5;10m" if $opts->{colour};
    $format.=sprintf("%-*s",$maxFieldNameLen,$$column{name});
    $format.="\e[38;5;15m" if $opts->{colour};
    $format.=": ";
    if($opts->{colour}) {
      my $type=$$column{type};
      $type="string" if !defined($type);
      if($type eq "string") {
	$format.="\e[38;5;11m";
      } elsif($type eq "number") {
	$format.="\e[38;5;214m";
      } elsif($type eq "datetime") {
	$format.="\e[38;5;14m";
      }
      $format.="\e[48;5;233m";
    }
    $format.="%-" . $$column{datalength} . "s";
    $format.="\e[00m" if $opts->{colour};
    $format.="\n";
  }

  return $format;
}

# Print the data
sub record_printData {
  my ($format, $rows)=@_;

  # Print each row with the precalculated format
  my $nl=0;
  for my $row (@$rows) {
    print "\n" if $nl;
    $nl=1;
    printf "$format", @$row;
  }
}

# Print the header
sub emacs_writeControl {
  my ($columns,$tempFileBase)=@_;

  # Process each column
  my $format=
";; -*- forms -*- AUTO GENERATED by sqlformat.pl
(setq forms-file \"$tempFileBase.data\")
(setq forms-read-only t)
(setq forms-number-of-fields " . @$columns . ")
(setq forms-format-list
      (list
\"Record from manual query:

";

  my $x;
  for($x=0;$x<@$columns;$x++) {
    # Write a '%<num>s' entry as appropriate
    my $column=$$columns[$x];
    $format.=sprintf("%-*s: \" %d \"\n",$maxFieldNameLen,$$column{name}, $x+1);
  }

  $format.="\"))\n";

  open FILE, ">$tempFileBase.form";
  print FILE $format;
  close FILE;
}

# Print the data
sub emacs_writeData {
  my ($rows,$tempFileBase)=@_;

  open FILE, ">$tempFileBase.data";

  # Print each row with the precalculated format
  for my $row (@$rows) {
    print FILE join("\t",@$row), "\n";
  }

  close FILE;
}
