# Perl package MJE::Table
# Provide general utility functions
# Written by Martin Ebourne, 06/08/2001
#
# Usage:
#
#   use MJE::Table qw(:all);  # Note this imports all symbols

package MJE::Table;

require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw();
@EXPORT_OK = qw(
		&formatTable
		&formatTableFields
		&formatTableHtml
		&sortByField
		);
%EXPORT_TAGS = ( all => \@EXPORT_OK );

use strict;
use HTML::Entities;
use MJE::Util;


### These turn things into useful strings

# Format a 2d textual table into a string, sizing the columns as appropriate
sub formatTable() {
  my ($array)=@_;
  my $result="";

  # Calculate column widths
  my @widths;
  for(my $y=0;$y<@$array;$y++) {
    my $line=$array->[$y];
    for(my $x=0;$x<@$line;$x++) {
      my $length=length($line->[$x]);

      if(defined($length) && (!defined($widths[$x]) || $length>$widths[$x])) {
	$widths[$x]=$length;
      }
    }
  }

  # Write out contents
  for(my $y=0;$y<@$array;$y++) {
    my $line=$array->[$y];
    for(my $x=0;$x<@$line;$x++) {
      my $value=$line->[$x];

      $result.=sprintf("%-*s",$widths[$x]+3,defined($value) ? $value : "");
    }
    $result.="\n";
  }

  return $result;
}


# Format a 2d textual table into a string with tab separated columns. The tab
# can be overridden by providing a different string
sub formatTableFields() {
  my ($array, $separator)=@_;
  my $result="";

  # Default to tab
  if(!defined($separator)) {
    $separator="\t";
  }

  # Write out contents
  for my $line (@$array) {
    my $tab=0;
    for my $value (@$line) {
      if($tab) {
	$result.=$separator;
      }
      $tab=1;

      if(defined($value)) {
	$result.=$value;
      }
    }
    $result.="\n";
  }

  return $result;
}


# Format a 2d textual table into a HTML string. First arg is array reference,
# second arg is has reference of options (all optional). options are:
#   header - 1 if first line is to be treated as table header
#   encode - 1 if table contents should be encoded to protect special characters
#   table  - HTML attributes for table tag
#   row    - HTML attributes for row tag
#   cell   - HTML attributes for cell tag
sub formatTableHtml() {
  my ($array, $options)=@_;
  my $result="";

  # True/false options
  my $header=&valid($options->{header},0);
  my $encode=&valid($options->{encode},0);

  # String options for HTML tags
  my $tableExtra=&valid($options->{table});
  my $rowExtra=&valid($options->{row});
  my $cellExtra=&valid($options->{cell});

  $result.="<TABLE $tableExtra>\n";

  # Iterate over each row
  for(my $y=0;$y<@$array;$y++) {
    my $line=$array->[$y];

    # If header line then output header cells instead of data
    my $cell=$header ? "TH" : "TD";

    $result.=" <TR $rowExtra>\n";

    # Iterate over each column within the row
    for(my $x=0;$x<@$line;$x++) {
      my $value=$line->[$x];

      # Output cell tags & data
      $result.="  <$cell $cellExtra>";
      if(defined($value)) {
	if($encode) {
	  $result.=encode_entities($value);
	} else {
	  $result.=$value;
	}
      }
      $result.="</$cell>\n";
    }
    $result.=" <TR>\n";

    # Ensure only first line is treated as header
    $header=0;
  }
  $result.="</TABLE>\n";

  return $result;
}


### Array processing

# Sort a 2d array by the given field
# array      - reference of source data array (not changed)
# field      - number of the field to sort by (starting from 0)
# type       - 'number' or 'string' relating to the data in the given sort field
# reverse    - if non-zero sorts in reverse order (optional)
# skipHeader - number of rows at the top to exclude from the sort (optional)
sub sortByField() {
  my ($array, $field, $type, $reverse, $skipHeader)=@_;
  my @result;

  # Turn reverse flag into multiplier
  if(defined($reverse) && $reverse) {
    $reverse=-1;
  } else {
    $reverse=1;
  }

  # Copy the header across if any
  if(defined($skipHeader) && $skipHeader) {
    push @result, @$array[0..$skipHeader-1];
  } else {
    $skipHeader=0;
  }

  # Sort remainder of the input as appropriate
  if($type eq "number") {
    push @result, sort { ($a->[$field] <=> $b->[$field])*$reverse } @$array[$skipHeader..$#$array];
  } elsif($type eq "string") {
    push @result, sort { ($a->[$field] cmp $b->[$field])*$reverse } @$array[$skipHeader..$#$array];
  } else {
    die "Invalid type '$type'";
  }

  return @result;
}

return 1;
