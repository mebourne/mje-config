#!/usr/local/bin/perl -w
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01

use strict;
use English;
use MJE::ParseOpts;

# Settings for styles
# Mandatory:
#   headerline 	 - True to write out header lines
#   underline  	 - True to underline header lines (if header line output)
#   otherline  	 - True to write out non-table lines
#   newline    	 - True to write newline after table output for readability
#   align      	 - True to line up columns by padding with spaces
#   squash     	 - True to squash column widths to minimum for title/data
#   compress     - True to compress column widths to minimum for data (truncate title)
#   separator  	 - Separator character/string used between columns
#   colour     	 - True if colour allowed
#   controlFn  	 - Control function
#   headerFn   	 - Function to print header line & underline
#   formatFn   	 - Function to generate data line format
#   dataFn     	 - Function to print data line
# Optional:
#   extend     	 - Inherit settings from given parent style (recursively)
#   description  - A description of this style for use with ParseOpts. Not present for
#                  intermediate styles which the user is not allowed to select
#   escapeRegexp - Regexp to match data characters which need to be escaped (by doubling up)
#   quoteRegexp  - Regexp to match data which needs to be quoted (surrounded in double quotes)
#   exitFn       - Function to be called when all processing is complete
my %styles=(
  default => {
    headerline   => 1,
    underline    => 1,
    otherline    => 1,
    newline      => 1,
    align        => 1,
    squash       => 0,
    compress     => 0,
    separator    => " ",
    colour       => 1,
    controlFn    => \&default_control,
    headerFn     => \&default_printHeader,
    formatFn     => \&default_generateFormat,
    dataFn       => \&default_printData,
  },
  simple => {
    extend       => "default",
    description  => "Simple formatting keeping the default output style",
  },
  squash => {
    extend       => "simple",
    description  => "As simple but reduces column widths to save on screen space",
    squash       => 1,
  },
  compress => {
    extend       => "simple",
    description  => "As squash but also truncates long titles",
    compress     => 1,
  },
  tsv => {
    extend       => "default",
    description  => "Tab separated value output with header",
    otherline    => 0,
    underline    => 0,
    align        => 0,
    separator    => "\t",
  },
  csv => {
    extend       => "tsv",
    description  => "Comma separated value output with header",
    escapeRegexp => "\"",
    quoteRegexp  => "[ \t,\"\']",
    separator    => ",",
  },
  bcp => {
    extend       => "tsv",
    description  => "BCP compatible output, tab separated columns with no header",
    headerline   => 0,
    colour       => 0,
  },
  record => {
    extend       => "default",
    description  => "Record style output, row by row",
    headerline   => 0,
    formatFn     => \&record_generateFormat,
    dataFn       => \&record_printData,
  },
  rows => {
    extend       => "record",
    description  => "Record style output with multiple rows to a record line",
    separator    => "  ",
    controlFn    => \&rows_control,
  },
  emacs => {
    extend       => "default",
    description  => "Output directly to Emacs in forms mode",
    headerline   => 0,
    otherline    => 0,
    newline      => 0,
    colour       => 0,
    controlFn    => \&emacs_control,
    exitFn       => \&emacs_exit,
  },
);

# Generate a simple hash from style name to description for use with
# ParseOpts. Ignore any styles which don't have a description and hence are
# intermediates. Also generate summary for main help page
use vars qw(%stylesHelp);
%stylesHelp=();
my $stylesSummary;
for my $styleName (keys(%styles)) {
  $styles{$styleName}->{name}=$styleName;
  if(exists($styles{$styleName}->{description})) {
    $stylesHelp{$styleName}=$styles{$styleName}->{description};
    $stylesSummary.="				$styleName	$styles{$styleName}->{description}\n";
  }
}

# Options for control of colour
use vars qw(%colourHelp);
%colourHelp=(
  off  => "No colour",
  auto => "Only colour if outputting to tty",
  on   => "Always colour"# (if applicable)"
);

# Parse the command line options & give help
my $opts=new MJE::ParseOpts (
'Description:
Format SQL output to make it easier to read.

Usage:
sqlformat.pl [options] <style>

Options:
  -c <when>, --colour=<when>	Colour output. One of:
				# --colour | -c : values=%colourHelp
				off	No colour
				auto	Only colour if outputting to tty
				on	Always colour (if applicable)
  -h, --help			Provide this help
				# --help | -h
  -w <columns>, --width=<columns>
				Specify the terminal width in characters
				# --width | -w : posinteger
  --width-auto			With --width, only apply if output to tty
				# --width-auto

Arguments:
  <style>			The style of formatting to perform. One of:
'.$stylesSummary.'##
	   			# style : values=%stylesHelp
') || exit 1;

# Get the user selected style
my $style=$styles{$opts->{style}};

# Expand the selected style to include all inherited settings from its base styles
for(my $extends=exists($style->{extend}) ? $styles{$style->{extend}} : undef;
    defined($extends);
    $extends=exists($extends->{extend}) ? $styles{$extends->{extend}} : undef) {
  for my $key (keys(%$extends)) {
    if(!exists($style->{$key})) {
      $style->{$key}=$extends->{$key};
    }
  }
}

# Note if we are outputting to a tty or a file
my $tty=-t STDOUT;

# Determine if we should use colour. Must be allowed by style, and requested
# by user (possibly dependant on if tty)
my $colour=0;
if($style->{colour}) {
  if($opts->{colour} eq "on"
     || $opts->{colour} eq "auto" && $tty) {
    $colour=1;
  }
}

# Determine screen width to use
my $screenWidth=$opts->{width};
if($opts->{"width-auto"} && !$tty) {
  $screenWidth=undef;
}

# These used by emacs style
my $tempFileBase="/tmp/sqlformat.$PID.";
my $tempFileCount=1;

# Naughty globals
my $inputLineNum=0;
my $maxFieldNameLen=0;

&main();

exit;

sub main {

  # Read the input two lines at a time (but looping once per line), looking for a table header
  my $thisLine=&getNextLine(0);
  while(defined($thisLine)) {
    my $nextLine=&getNextLine(0);

    # Check to see if we've found the start of a select output
    my ($minLength,$maxLength);
    if(defined($nextLine) && &isHeader($thisLine, $nextLine, \$minLength, \$maxLength)) {

      # Read the table in
      my @columns;
      my @rows;
      &readHeader(\@columns,$thisLine,$nextLine);
      &readData(\@columns, \@rows, $minLength, $maxLength, \$nextLine);

      # Processing
      if($style->{squash}) {
	&trimLength(\@columns);
      }
      if($style->{compress}) {
	&compressLength(\@columns);
      }

      # Write the output
      &{$style->{controlFn}}(\@columns,\@rows);

      # Ensure a newline is present. Helps make desc output easier to read
      if($nextLine ne "\n" && $style->{newline}) {
	print "\n";
      }
    } else {
      # Not a table header, so just print the line
      if($style->{otherline}) {
	print "$thisLine";
      }
    }
    $thisLine=$nextLine;
  }

  # Call any exit function
  if(exists($style->{exitFn})) {
    &{$style->{exitFn}}();
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

# Return true if these two lines constitute a table header. Also return min
# and max lengths for valid data lines
sub isHeader {
  my ($first,$second,$minLength,$maxLength)=@_;

  my $isHeader=0;
  my $secondLen=length($second);

  # Second line must consist only of spaces and dashes
  if($second=~/^ *-[- ]*$/) {

    # Trim the last group of dashes down to one to get minimum length
    $second=~s/-*$/-/;

    # Check first line is within acceptable length range
    if(length($first)>=length($second) && length($first)<=$secondLen) {
      $isHeader=1;
    }
  }

  # Return the acceptable length range
  $$minLength=length($second);
  $$maxLength=$secondLen;

  return $isHeader;
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
    push @$columns, { name        => $name,
		      start       => $index+$start,
		      length      => $length,
		      datalength  => 0,
		      needsquote  => 0
		    };

    if(length($name)>$maxFieldNameLen) {
      $maxFieldNameLen=length($name);
    }

    # Modify header & format lines for next column
    $format=substr($format,$end);
    $header=substr($header,$end < length($header) ? $end : length($header));
    $index+=$end;
  }
}

# Read all of the data lines
sub readData {
  my ($columns, $rows, $minLength, $maxLength, $nextLine)=@_;

  # Loop for each line until the length doesn't match, or we run out
  my $line;
  while(defined($line=&getNextLine(1)) && length($line)>=$minLength && length($line)<=$maxLength) {
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
	  } elsif($value=~/[A-Z][a-z]{2} [ 0-9]?[0-9] [0-9]{4}/
		  || $value=~/[0-9]{2}:[0-9]{2}/) {
	    $$column{type}="datetime";
	  }
	}

	# Check if any special characters which may need quoting are present
	if(defined($style->{quoteRegexp}) && !$$column{needsquote}) {
	  $$column{needsquote}=$value=~/$style->{quoteRegexp}/;
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

# For each column shrink length such that it will still fit all data, reducing
# title if required
sub compressLength {
  my ($columns)=@_;

  my $x;
  for($x=0;$x<@$columns;$x++) {
    my $column=$$columns[$x];

    $$column{length}=$$column{datalength};
    if($$column{length}<1) {
      $$column{length}=1;
    }
    $$column{name}=substr($$column{name},0,$$column{length});
  }
}

sub default_control {
  my ($columns, $rows)=@_;

  if($style->{headerline}) {
    &{$style->{headerFn}}($columns);
  }
  if(@$rows) {
    my $format=&{$style->{formatFn}}($columns);
    &{$style->{dataFn}}($format,$rows);
  }
}

# Generate a printf format string for printing a row
sub default_generateFormat {
  my ($columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    if($x) {
      if($colour) {
	$format.="\e[38;5;4m";

	if($style->{separator} eq " ") {
	  $format.="\xb7";
	} else {
	  $format.=$style->{separator};
	}
      } else {
	$format.=$style->{separator};
      }
    }

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

    if($$column{needsquote}) {
      $format.="\"";
    }
    $format.="%";
    if($style->{align}) {
      $format.=$align*$$column{length};
    }
    $format.="s";
    if($$column{needsquote}) {
      $format.="\"";
    }
  }
  $format.="\e[38;5;15m" if $colour;

  return $format;
}

# Print the header
sub default_printHeader {
  my ($columns)=@_;

  my $header;
  my $underline;

  # Process each column
  my $x;
  for($x=0;$x<@$columns;$x++) {
    my $column=$$columns[$x];

    # Get padded column name
    my $value=sprintf("%*s", $style->{align} ? -$$column{length} : 0, $$column{name});

    if($x) {
      $header.=$style->{separator};
      $underline.=$style->{separator};
    }

    # Add to header line and convert to '---' before adding to underline line
    $header.=$value;
    $value=~s/./-/g;
    $underline.=$value;
  }

  # Output header in green
  $colour && print "\e[38;5;10m";
  print "$header\n";
  if($style->{underline}) {
    print "$underline\n";
  }
  $colour && print "\e[38;5;15m";
}

# Print the data
sub default_printData {
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

    if(defined($style->{escapeRegexp})) {
      my @newRow=@$row;
      map { s/($style->{escapeRegexp})/$1$1/g } @newRow;
      $row=\@newRow;
    }
    printf $format, @$row;

    print "\e[48;5;0m" if $colour;
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
    $format.="\e[38;5;10m" if $colour;
    $format.=sprintf("%-*s",$maxFieldNameLen,$$column{name});
    $format.="\e[38;5;15m" if $colour;
    $format.=": ";
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
      $format.="\e[48;5;233m";
    }
    $format.="%-" . $$column{datalength} . "s";
    $format.="\e[00m" if $colour;
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

# Control function for rows style
sub rows_control {
  my ($columns, $rows)=@_;

  my $rowWidth=1;
  for my $column (@$columns) {
    if($rowWidth<$column->{datalength}) {
      $rowWidth=$column->{datalength};
    }
  }

  my $separator=$style->{separator};

  my $rowsPerLine=int(($screenWidth-$maxFieldNameLen-2+length($separator))
		      /($rowWidth+length($separator)));
  $rowsPerLine=1 if $rowsPerLine<1;

  my $nl=0;
  # Process each block of rows
  for(my $rowBlock=0;$rowBlock<@$rows;$rowBlock+=$rowsPerLine) {
    print "\n" if $nl;
    $nl=1;

    # Process each column
    my $x;
    for(my $colNum=0;$colNum<@$columns;$colNum++) {
      my $column=$$columns[$colNum];

      my $format="";
      $format.="\e[38;5;10m" if $colour;
      $format.="%-*s";
      $format.="\e[38;5;15m" if $colour;
      $format.=": ";
      printf "$format", $maxFieldNameLen, $$column{name};

      # Process this column for each row in this block
      for(my $rowNum=$rowBlock;$rowNum<$rowBlock+$rowsPerLine && $rowNum<@$rows;$rowNum++) {
	$format="";
	if($rowNum!=$rowBlock) {
	  $format.=$separator;
	}
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
	  $format.="\e[48;5;233m";
	}
	$format.="%-*s";
	$format.="\e[00m" if $colour;
	$format.=" " x ($rowWidth - $column->{datalength});
	printf "$format", $column->{datalength}, $rows->[$rowNum]->[$colNum];
      }
      print "\n";
    }
  }
}

# Control function for emacs style
sub emacs_control {
  my ($columns, $rows)=@_;

  &emacs_writeControl($columns,$tempFileBase . $tempFileCount);
  &emacs_writeData($rows,$tempFileBase . $tempFileCount);
  $tempFileCount++;
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

# Send the generated files to Emacs via emacsclient when we have finished
sub emacs_exit {
  my ($formFiles, $allFiles);
  for(my $i=1;$i<$tempFileCount;$i++) {
    $formFiles.=" $tempFileBase$i.form";
    $allFiles.=" $tempFileBase$i.form $tempFileBase$i.data";
  }

  system("emacsclient --no-wait $formFiles");
  system("( sleep 60 ; rm -f $allFiles ) &");
}
