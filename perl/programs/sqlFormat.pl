#!/usr/local/bin/perl -w

use strict;

die "Usage: sqlcompress.pl" if @ARGV>0;

my @columns;
my @rows;

&readHeader(\@columns);
&readData(\@columns, \@rows);

&trimLength(\@columns);

&printHeader(\@columns);
&printData(\@columns,\@rows);

print "\n";
while(<STDIN>) {
  print;
}

exit;


sub trim {
  my ($result)=$_[0]=~/^ *(.*?) *$/;
  return $result;
}

sub readHeader {
  my ($columns)=@_;

  my $header=<STDIN>;
  my $format=<STDIN>;
  
  #print $header, $format;
  
  my $index=0;
  while($format=~/^( *)(-+)/) {
    my ($gap, $dashes)=($1, $2);
    my $name=substr($header,length($gap), length($dashes));
    $name=&trim($name);
  
    my $start=length($gap);
    my $length=length($dashes);
    my $end=$start+$length;
  
    push @$columns, { name => $name, start => $index+$start, length => $length, datalength => 0 };

    #print $name, ":", $index+$start, ":", $length, "\n";
  
    $format=substr($format,$end);
    $header=substr($header,$end);
    $index+=$end;
  }
}

sub readData {
  my ($columns, $rows)=@_;

  my $line="x";
  while($line ne '' && defined($line=<STDIN>)) {
    chomp $line;
    if($line ne '') {
      my $row=[];

      my $x;
      for($x=0;$x<@$columns;$x++) {

	my $column=$$columns[$x];
	my $value=substr($line,$$column{start},$$column{length});

	if(!defined($$column{align})) {
	  if($value=~/^ .*[^ ]$/) {
	    $$column{align}=1;
	  } elsif($value=~/^[^ ].* $/) {
	    $$column{align}=-1;
	  }
	}

	$value=&trim($value);
	if(length($value)>$$column{datalength}) {
	  $$column{datalength}=length($value);
	}

	push @$row, $value;
      }

      push @$rows, $row;
    }
  }
}

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

sub printHeader {
  my ($columns)=@_;

  my $header;
  my $underline;

  my $x;
  for($x=0;$x<@$columns;$x++) {
    my $column=$$columns[$x];
    my $value=sprintf("%*s", -$$column{length}, $$column{name});

    if($x) {
      $header.=" ";
      $underline.=" ";
    }

    $header.=$value;
    $value=~s/./-/g;
    $underline.=$value;
  }

  print "$header\n$underline\n";
}

sub printData {
  my ($columns, $rows)=@_;

  for my $row (@$rows) {
    my $x;
    for($x=0;$x<@$columns;$x++) {
      print " " if $x;

      my $column=$$columns[$x];
      printf "%*s", $$column{align}*$$column{length}, $$row[$x];
    }

    print "\n";
  }
}
