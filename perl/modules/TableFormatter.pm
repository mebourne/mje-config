# Perl package MJE::TableFormatter
# Sophisticated table formatting and display
# Written by Martin Ebourne, 27/05/2002
#
# Usage:
#
#   use MJE::TableFormatter;
#
#   my $formatter=new MJE::TableFormatter (<<EOF);
#   $formatter->...;

package MJE::TableFormatter;

use strict;

use Data::Dumper;


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


# Create a new MJE::TableFormatter object
sub new {
  my ($proto)=@_;
  my $class=ref($proto) || $proto;
  my $self={};
  my $obj=bless($self,$class);

  $self->{_debug}=0;

  $self->{colour}=1;
  $self->{screenWidth}=undef;
  $self->(maxFieldNameLen}=0;

  return $obj;
}

# Set the style to use
sub setStyle {
  my ($self, $style)=@_;

  $self->{style}=$styles{$style};

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
}


sub default_control {
  my ($self, $columns, $rows)=@_;

  if($self->{style}->{headerline}) {
    &{$self->{style}->{headerFn}}($columns);
  }
  if(@$rows) {
    my $format=&{$self->{style}->{formatFn}}($columns);
    &{$self->{style}->{dataFn}}($format,$rows);
  }
}

# Generate a printf format string for printing a row
sub default_generateFormat {
  my ($self, $columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    if($x) {
      if($self->{colour}) {
	$format.="\e[38;5;4m";

	if($self->{style}->{separator} eq " ") {
	  $format.="\xb7";
	} else {
	  $format.=$self->{style}->{separator};
	}
      } else {
	$format.=$self->{style}->{separator};
      }
    }

    # Write a '%<num>s' entry as appropriate
    my $column=$$columns[$x];
    my $align=$$column{align};
    $align=-1 if !defined($align);

    if($self->{colour}) {
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
    if($self->{style}->{align}) {
      $format.=$align*$$column{length};
    }
    $format.="s";
    if($$column{needsquote}) {
      $format.="\"";
    }
  }
  $format.="\e[38;5;15m" if $self->{colour};

  return $format;
}

# Print the header
sub default_printHeader {
  my ($self, $columns)=@_;

  my $header;
  my $underline;

  # Process each column
  my $x;
  for($x=0;$x<@$columns;$x++) {
    my $column=$$columns[$x];

    # Get padded column name
    my $value=sprintf("%*s", $self->{style}->{align} ? -$$column{length} : 0, $$column{name});

    if($x) {
      $header.=$self->{style}->{separator};
      $underline.=$self->{style}->{separator};
    }

    # Add to header line and convert to '---' before adding to underline line
    $header.=$value;
    $value=~s/./-/g;
    $underline.=$value;
  }

  # Output header in green
  $self->{colour} && print "\e[38;5;10m";
  print "$header\n";
  if($self->{style}->{underline}) {
    print "$underline\n";
  }
  $self->{colour} && print "\e[38;5;15m";
}

# Print the data
sub default_printData {
  my ($self, $format, $rows)=@_;

  # Print each row with the precalculated format
  my $background=0;
  for my $row (@$rows) {
    if($self->{colour}) {
      if($background) {
	print "\e[48;5;233m";
      }
      $background=!$background;
    }

    if(defined($self->{style}->{escapeRegexp})) {
      my @newRow=@$row;
      map { s/($self->{style}->{escapeRegexp})/$1$1/g } @newRow;
      $row=\@newRow;
    }
    printf $format, @$row;

    print "\e[48;5;0m" if $self->{colour};
    print "\n";
  }
}

# Generate a printf format string for printing a row
sub record_generateFormat {
  my ($self, $columns)=@_;

  # Process each column
  my $format="";
  my $x;
  for($x=0;$x<@$columns;$x++) {
    # Write a '%<num>s' entry as appropriate
    my $column=$$columns[$x];
    $format.="\e[38;5;10m" if $self->{colour};
    $format.=sprintf("%-*s",$self->{maxFieldNameLen},$$column{name});
    $format.="\e[38;5;15m" if $self->{colour};
    $format.=": ";
    if($self->{colour}) {
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
    $format.="\e[00m" if $self->{colour};
    $format.="\n";
  }

  return $format;
}

# Print the data
sub record_printData {
  my ($self, $format, $rows)=@_;

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
  my ($self, $columns, $rows)=@_;

  my $rowWidth=1;
  for my $column (@$columns) {
    if($rowWidth<$column->{datalength}) {
      $rowWidth=$column->{datalength};
    }
  }

  my $separator=$self->{style}->{separator};

  my $rowsPerLine=int(($self->{screenWidth}-$self->{maxFieldNameLen}-2+length($separator))
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
      $format.="\e[38;5;10m" if $self->{colour};
      $format.="%-*s";
      $format.="\e[38;5;15m" if $self->{colour};
      $format.=": ";
      printf "$format", $self->{maxFieldNameLen}, $$column{name};

      # Process this column for each row in this block
      for(my $rowNum=$rowBlock;$rowNum<$rowBlock+$rowsPerLine && $rowNum<@$rows;$rowNum++) {
	$format="";
	if($rowNum!=$rowBlock) {
	  $format.=$separator;
	}
	if($self->{colour}) {
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
	$format.="\e[00m" if $self->{colour};
	$format.=" " x ($rowWidth - $column->{datalength});
	printf "$format", $column->{datalength}, $rows->[$rowNum]->[$colNum];
      }
      print "\n";
    }
  }
}

# Control function for emacs style
sub emacs_control {
  my ($self, $columns, $rows)=@_;

  &emacs_writeControl($columns,$tempFileBase . $tempFileCount);
  &emacs_writeData($rows,$tempFileBase . $tempFileCount);
  $tempFileCount++;
}

# Print the header
sub emacs_writeControl {
  my ($self, $columns,$tempFileBase)=@_;

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
    $format.=sprintf("%-*s: \" %d \"\n",$self->{maxFieldNameLen},$$column{name}, $x+1);
  }

  $format.="\"))\n";

  open FILE, ">$tempFileBase.form";
  print FILE $format;
  close FILE;
}

# Print the data
sub emacs_writeData {
  my ($self, $rows,$tempFileBase)=@_;

  open FILE, ">$tempFileBase.data";

  # Print each row with the precalculated format
  for my $row (@$rows) {
    print FILE join("\t",@$row), "\n";
  }

  close FILE;
}

# Send the generated files to Emacs via emacsclient when we have finished
sub emacs_exit {
  my ($self, $formFiles, $allFiles);
  for(my $i=1;$i<$tempFileCount;$i++) {
    $formFiles.=" $tempFileBase$i.form";
    $allFiles.=" $tempFileBase$i.form $tempFileBase$i.data";
  }

  system("emacsclient --no-wait $formFiles");
  system("( sleep 60 ; rm -f $allFiles ) &");
}

return 1;
