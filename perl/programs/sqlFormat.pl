#!/usr/local/bin/perl -w
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01
#
# Usage: sqlformat.pl [-s] [-c]

use strict;

my $squash=0;
my $colour=0;

# Decode command line options
for my $option (@ARGV) {
  if($option eq "-s") {
    $squash=1;
  } elsif($option eq "-c") {
    $colour=1;
  } else {
    print STDERR
"Syntax: sqlformat.pl [-s] [-c]
  -c	Colour output
  -s	Squash output columns
";
    exit 1;
  }
}

# Read the input two lines at a time (but looping once per line), looking for a table header
my $thisLine=<STDIN>;
while(defined($thisLine)) {
  my $nextLine=<STDIN>;

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
    if($squash) {
      &trimLength(\@columns);
    }
    
    # Write the output
    &printHeader(\@columns);
    if(@rows) {
      my $format=&generateFormat(\@columns);
      &printData($format,\@rows);
    }

    # Ensure a newline is present. Helps make desc output easier to read
    if($nextLine ne "\n") {
      print "\n";
    }
  } else {
    # Not a table header, so just print the line
    print "$thisLine";
  }
  $thisLine=$nextLine;
}

exit;


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
  while(defined($line=<STDIN>) && length($line)==$lineLength) {
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
sub generateFormat {
  my ($columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    $format.=" " if $x;

    # Write a '%<num>s' entry as appropriate
    my $column=$$columns[$x];
    my $align=$$column{align};
    $align=-1 if !defined($align);

    if($colour) {
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
    $format.="\e[38;5;15m" if $colour;
  }

  return $format;
}

# Print the header
sub printHeader {
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
  $colour && print "\e[38;5;10m";
  print "$header\n$underline\n";
  $colour && print "\e[38;5;15m";
}

# Print the data
sub printData {
  my ($format, $rows)=@_;

  # Print each row with the precalculated format
  my $background=0;
  for my $row (@$rows) {
    if($colour) {
      if($background) {
	print "\e[48;5;233m";
      }
      $background=!$background;
    }
    printf $format, @$row;

    print "\e[48;5;0m" if $colour;
    print "\n";
  }
}
