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
	   DBI::SQL_VARBINARY		, "binary"
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

# Parse the command line options & give help
my $opts=new MJE::ParseOpts (
'Description:
Execute an SQL command against a database and format the output in a fully
configurable manner.

Usage:
sqlExecute.pl [options] <sql>

Options:
  -c <when>, --colour=<when>	Colour output. One of:
				# --colour | -c : values=%colourHelp
				off	No colour
				auto	Only colour if outputting to tty
				on	Always colour (if applicable)
  -h, --help			Provide this help
				# --help | -h
  -s <style>, --style=<style>	The style of formatting to perform. One of:
'.$stylesSummary.'		# --style | -s : values=%stylesHelp
  -w <columns>, --width=<columns>
				Specify the terminal width in characters
				# --width | -w : posinteger
  --width-auto			With --width, only apply if output to tty
				# --width-auto

Arguments:
  <sql> ...			SQL statement to execute
	   			# [sql] += [*] sql : string
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

  # Connect to the database
  my $dbh = DBI->connect("dbi:Oracle:DLNTRC01", "trcadm", "trcadm",
			 { AutoCommit  => 0,
			   ChopBlanks  => 1,
			   LongTruncOk => 1,
			   RaiseError  => 1 })
      || die "Can't connect to Oracle: $DBI::errstr";

  # Prepare the statement
  my $sth = $dbh->prepare($opts->{sql});

  # Execute the statement
  $sth->execute;

  # Output results if a select statement
  if($sth->{NUM_OF_FIELDS}) {
    my $formatter=new MJE::TableFormatter;

    # Configure formatter as per user options
    $formatter->setStyle($opts->{style});
    $formatter->{colour}=$colour;
    $formatter->{screenWidth}=$screenWidth;

    # List the columns for the formatter
    for(my $i=0;$i<$sth->{NUM_OF_FIELDS};$i++) {
      $formatter->addColumn($sth->{NAME}->[$i],
			    $sth->{PRECISION}->[$i],
			    $types{$sth->{TYPE}->[$i]});
    }

    # Write the data to the formatter
    while(my @data = $sth->fetchrow_array) {
      $formatter->addRow(@data);
    }

    # Display the table
    $formatter->display;
  } else {
    # Non-select statement. Simply output row count
    print $sth->rows, " row(s) affected.\n";
  }

  # Tidy up
  $sth->finish;
  $dbh->commit;
  $dbh->disconnect;
}
