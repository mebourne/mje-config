#!/app/perl5005/bin/perl -w
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01

use strict;
use Data::Dumper;
use DBI;
use English;
use Env;
use MJE::ParseOpts;
use MJE::TableFormatter;

BEGIN {
  $ENV{ORACLE_HOME}="/app/oracle/product/client/8.1.6";
}

# Map from database types to TableFormatter types
my %types=(DBI::SQL_CHAR		, "string",
	   DBI::SQL_LONGVARCHAR		, "string",
	   DBI::SQL_VARCHAR		, "string",
	   DBI::SQL_WCHAR		, "string",
	   DBI::SQL_WLONGVARCHAR	, "string",
	   DBI::SQL_WVARCHAR		, "string",
	   -9112                        , "string",  # Oracle CLOB

	   DBI::SQL_BIGINT		, "number",
	   DBI::SQL_DECIMAL		, "number",
	   DBI::SQL_DOUBLE		, "number",
	   DBI::SQL_FLOAT		, "number",
	   DBI::SQL_INTEGER		, "number",
	   DBI::SQL_NUMERIC		, "number",
	   DBI::SQL_REAL		, "number",
	   DBI::SQL_SMALLINT		, "number",
	   DBI::SQL_TINYINT		, "number",

	   DBI::SQL_DATE		, "datetime",
	   DBI::SQL_TIME		, "datetime",
	   DBI::SQL_TIMESTAMP		, "datetime",

	   DBI::SQL_BINARY		, "binary",
	   DBI::SQL_BIT			, "binary",
	   DBI::SQL_LONGVARBINARY	, "binary",
	   DBI::SQL_VARBINARY		, "binary",
	   -9113                        , "binary",  # Oracle BLOB
	   );

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

# Available DBI backends
use vars qw(@drivers);
@drivers=DBI->available_drivers;

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
  -C <connect>, --connect=<connect>
				The DBI connect string to use (see individual
	 	 	 	driver documentation)
				# --connect | -C : string
  --column-width=<count>	Specify the maximum column width to be shown
				# --column-width : posinteger
  --count			Display the affected row count
				# --count
  -D <driver>, --driver=<driver>
				The DBI driver to access the database with
				# --driver | -D : values=@drivers
  -h, --help			Provide this help
				# --help | -h
  -P <password>, --password=<password>
				The password to log on with
				# --password | -P : string
  -r <rows>, --max-rows=<rows>	Specify the maximum number of rows to retrieve
				# --max-rows | -r : posinteger
  -s <style>, --style=<style>	The style of formatting to perform. One of:
'.$stylesSummary.'		# --style | -s : values=%stylesHelp
  -U <user>, --user=<user>	The user name to log on with
				# --user | -U : string
  -w <columns>, --width=<columns>
				Specify the terminal width in characters
				# --width | -w : posinteger
  --width-auto			With --width, only apply if output to tty
				# --width-auto

Arguments:
  <sql-statement> ...		SQL statement(s) to execute. If a statement is
				in parentheses then the row count will not be
				reported for it
	   			# @statements += [*] sql : string
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

if(!exists($opts->{driver}) || !exists($opts->{user})) {
  die "Must specify database driver and user";
}

&main();

exit;

sub raiseError {
  my ($dbh, $errh, $function, $sql)=@_;

  print STDERR "\n";

  print STDERR "Database error: ";
  print STDERR $errh->errstr, "\n";

  print STDERR "In DBI call   : $function\n";

  if(defined($sql)) {
    print STDERR "Processing SQL: $sql\n";
  }

  $errh->finish if $errh!=$dbh;
  $dbh->rollback;
  $dbh->disconnect;
  print STDERR "Changes rolled back.\n";

  exit 1;
}

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

  # Connect to the database
  my $dbh = DBI->connect("dbi:$opts->{driver}:" . (exists($opts->{connect})
						   ? $opts->{connect}
						   : ""),
			 $opts->{user},
			 $opts->{password},
			 { AutoCommit  => 0,
			   ChopBlanks  => 1,
			   LongReadLen => $dataLength,
			   LongTruncOk => 1,
			   PrintError  => 0,
			   RaiseError  => 0 })
      || die "Can't connect to $opts->{driver}: $DBI::errstr";

  # Iterate over all of the statements
  for my $statement (@statements) {

    my $report = 1;
    if($statement=~/^\((.*)\)$/) {
      $statement=$1;
      $report=0;
    }

    # Prepare the statement
    my $sth = $dbh->prepare($statement)
	|| &raiseError($dbh,$dbh,"dbh->prepare",$statement);

    # Execute the statement
    $sth->execute || &raiseError($dbh,$sth,"sth->execute",$statement);

    # Output results if a select statement
    if($sth->{NUM_OF_FIELDS}) {

      # List the columns for the formatter
      for(my $i=0;$i<$sth->{NUM_OF_FIELDS};$i++) {

	if(!exists($types{$sth->{TYPE}->[$i]})) {
	  print STDERR "Warning: Unknown database type $sth->{TYPE}->[$i]\n";
	}

	$formatter->addColumn($sth->{NAME}->[$i],
			      $sth->{PRECISION}->[$i],
			      $types{$sth->{TYPE}->[$i]});
      }

      # Write the data to the formatter
      my $rows=0;
      while(my @data = $sth->fetchrow_array) {
	if(exists($opts->{"max-rows"}) && $rows>=$opts->{"max-rows"}) {
	  last;
	}

	$formatter->addRow(@data);
	$rows++;

	# Report number of rows received if we have a lot of results
	if(($rows%100)==0 && $tty) {
	  my $old=$|;
	  $|=1;
	  print "Received $rows rows...\r";
	  $|=$old;
	}
      }
      if(defined($sth->err))
      {
	&raiseError($dbh,$sth,"sth->fetchrow_array",$statement);
      }

      # Display the table
      $formatter->display;

      if($report && exists($opts->{count}) && $formatter->getStyleInfo("rowcount")) {
	print "\n", $rows, " row(s) returned.\n";
      }
    } else {
      # Non-select statement. Simply output row count
      if($report && exists($opts->{count})) {
	print $sth->rows, " row(s) affected.\n";
      }
    }

    # Tidy up
    $sth->finish || &raiseError($dbh,$sth,"sth->finish",$statement);
  }

  $dbh->commit || &raiseError($dbh,$dbh,"dbh->commit");

  $dbh->disconnect || &raiseError($dbh,$dbh,"dbh->disconnect");
}
