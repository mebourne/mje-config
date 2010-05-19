#!/usr/bin/env perl
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01
# $Id$

use strict;
use warnings;

use Data::Dumper;
use DBI ();
use English;
use Env;
use MJE::ParseOpts;
use MJE::TableFormatter;

BEGIN {
#  $ENV{ORACLE_HOME}="/app/oracle/product/client/8.1.6";
}

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
  -a, --auto-commit		Enable auto-commit (default is single
				transaction)
				# --auto-commit | -a
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
  --kerberos=<server>		Use Kerberos login instead of username/password
				# --kerberos : string
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

if(!exists($opts->{driver}) || !(exists($opts->{user}) || exists($opts->{kerberos}))) {
  die "Must specify database driver and user or kerberos";
}

my $kerberos = "";
if(exists($opts->{kerberos}))
{
  eval '
    use sybGetPrinc qw(sybGetPrinc);
    $kerberos = ";kerberos=" . sybGetPrinc($opts->{kerberos});
  ' or die $@;
  $opts->{user} = "";
  $opts->{password} = "";
}

require "DBD/$opts->{driver}.pm";

# Map from database types to TableFormatter types
no strict qw(subs);
my %types=(DBI::SQL_CHAR                          , "string",
           DBI::SQL_CLOB                          , "string",
           DBI::SQL_LONGVARCHAR                   , "string",
           DBI::SQL_VARCHAR                       , "string",
           DBI::SQL_WCHAR                         , "string",
           DBI::SQL_WLONGVARCHAR                  , "string",
           DBI::SQL_WVARCHAR                      , "string",
           DBD::Oracle::ORA_CHAR                  , "string",
           DBD::Oracle::ORA_CHARZ                 , "string",
           DBD::Oracle::ORA_CLOB                  , "string",
           DBD::Oracle::ORA_LONG                  , "string",
           DBD::Oracle::ORA_STRING                , "string",
           DBD::Oracle::ORA_VARCHAR2              , "string",
           DBD::Pg::PG_BPCHAR                     , "string",
           DBD::Pg::PG_CHAR                       , "string",
           DBD::Pg::PG_TEXT                       , "string",
           DBD::Pg::PG_VARCHAR                    , "string",

           DBI::SQL_BOOLEAN                       , "number",
           DBI::SQL_DECIMAL                       , "number",
           DBI::SQL_DOUBLE                        , "number",
           DBI::SQL_FLOAT                         , "number",
           DBI::SQL_INTEGER                       , "number",
           DBI::SQL_NUMERIC                       , "number",
           DBI::SQL_REAL                          , "number",
           DBI::SQL_SMALLINT                      , "number",
           DBI::SQL_TINYINT                       , "number",
           DBD::Oracle::ORA_NUMBER                , "number",
           DBD::Oracle::ORA_ROWID                 , "number",
           DBD::Pg::PG_BOOL                       , "number",
           DBD::Pg::PG_FLOAT4                     , "number",
           DBD::Pg::PG_FLOAT8                     , "number",
           DBD::Pg::PG_INT2                       , "number",
           DBD::Pg::PG_INT4                       , "number",
           DBD::Pg::PG_INT8                       , "number",
           DBD::Pg::PG_OID                        , "number",

           DBI::SQL_DATE                          , "datetime",
           DBI::SQL_DATETIME                      , "datetime",
           DBI::SQL_INTERVAL                      , "datetime",
           DBI::SQL_INTERVAL_DAY                  , "datetime",
           DBI::SQL_INTERVAL_DAY_TO_HOUR          , "datetime",
           DBI::SQL_INTERVAL_DAY_TO_MINUTE        , "datetime",
           DBI::SQL_INTERVAL_DAY_TO_SECOND        , "datetime",
           DBI::SQL_INTERVAL_HOUR                 , "datetime",
           DBI::SQL_INTERVAL_HOUR_TO_MINUTE       , "datetime",
           DBI::SQL_INTERVAL_HOUR_TO_SECOND       , "datetime",
           DBI::SQL_INTERVAL_MINUTE               , "datetime",
           DBI::SQL_INTERVAL_MINUTE_TO_SECOND     , "datetime",
           DBI::SQL_INTERVAL_MONTH                , "datetime",
           DBI::SQL_INTERVAL_SECOND               , "datetime",
           DBI::SQL_INTERVAL_YEAR                 , "datetime",
           DBI::SQL_INTERVAL_YEAR_TO_MONTH        , "datetime",
           DBI::SQL_TIME                          , "datetime",
           DBI::SQL_TIMESTAMP                     , "datetime",
           DBI::SQL_TYPE_DATE                     , "datetime",
           DBI::SQL_TYPE_TIME                     , "datetime",
           DBI::SQL_TYPE_TIMESTAMP                , "datetime",
           DBI::SQL_TYPE_TIMESTAMP_WITH_TIMEZONE  , "datetime",
           DBI::SQL_TYPE_TIME_WITH_TIMEZONE       , "datetime",
           DBD::Oracle::ORA_DATE                  , "datetime",
           DBD::Pg::PG_ABSTIME                    , "datetime",
           DBD::Pg::PG_DATE                       , "datetime",
           DBD::Pg::PG_DATETIME                   , "datetime",
           DBD::Pg::PG_RELTIME                    , "datetime",
           DBD::Pg::PG_TIME                       , "datetime",
           DBD::Pg::PG_TIMESPAN                   , "datetime",
           DBD::Pg::PG_TIMESTAMP                  , "datetime",
           DBD::Pg::PG_TINTERVAL                  , "datetime",

           DBI::SQL_BINARY                        , "binary",
           DBI::SQL_BIT                           , "binary",
           DBI::SQL_BLOB                          , "binary",
           DBI::SQL_LONGVARBINARY                 , "binary",
           DBI::SQL_VARBINARY                     , "binary",
           DBD::Oracle::ORA_BLOB                  , "binary",
           DBD::Oracle::ORA_LONGRAW               , "binary",
           DBD::Oracle::ORA_RAW                   , "binary",
           DBD::Pg::PG_BYTEA                      , "binary",

           # What to do with these?
           DBI::SQL_GUID                          , "unknown",
           DBI::SQL_UNKNOWN_TYPE                  , "unknown",
           DBI::SQL_UDT                           , "unknown",
           DBI::SQL_UDT_LOCATOR                   , "unknown",
           DBI::SQL_ROW                           , "unknown",
           DBI::SQL_REF                           , "unknown",
           DBI::SQL_BLOB_LOCATOR                  , "unknown",
           DBI::SQL_CLOB_LOCATOR                  , "unknown",
           DBI::SQL_ARRAY                         , "unknown",
           DBI::SQL_ARRAY_LOCATOR                 , "unknown",
           DBI::SQL_MULTISET                      , "unknown",
           DBI::SQL_MULTISET_LOCATOR              , "unknown",
           DBD::Oracle::ORA_MLSLABEL              , "unknown",
           DBD::Oracle::ORA_NTY                   , "unknown",
           DBD::Oracle::ORA_RSET                  , "unknown",
           );
use strict qw(subs);

# Sybase helpfully returns a 'BIT' as an ASCII character
if($opts->{driver} eq "Sybase") {
  $types{DBI::SQL_BIT}="number";
}

main();

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
  if(!exists($opts->{"auto-commit"})) {
    $dbh->rollback;
    print STDERR "Changes rolled back.\n";
  }
  $dbh->disconnect;

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
                                                   : "") . $kerberos,
                         $opts->{user},
                         $opts->{password},
                         { AutoCommit  => exists($opts->{"auto-commit"}),
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
        || raiseError($dbh,$dbh,"dbh->prepare",$statement);

    # Execute the statement
    $sth->execute || raiseError($dbh,$sth,"sth->execute",$statement);

    # Output results if a select statement
    if($sth->{NUM_OF_FIELDS}) {
      do {

        if(exists($sth->{syb_result_type})
           && $sth->{syb_result_type}==eval("DBD::Sybase::CS_STATUS_RESULT")) {
          my @result = $sth->fetchrow_array;
          print "(return status = $result[0])\n";
          $sth->fetchrow_array;
        } else {
          $formatter->reset;

          # List the columns for the formatter
          for(my $i=0;$i<$sth->{NUM_OF_FIELDS};$i++) {

            if(!exists($types{$sth->{TYPE}->[$i]}) || $types{$sth->{TYPE}->[$i]} eq "unknown") {
              print STDERR "Warning: Unknown database type $sth->{TYPE}->[$i] ";
              print STDERR "used in column $sth->{NAME}->[$i]\n";
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
            raiseError($dbh,$sth,"sth->fetchrow_array",$statement);
          }

          # Display the table
          $formatter->display;

          if($report && exists($opts->{count}) && $formatter->getStyleInfo("rowcount")) {
            print "\n", $rows, " row(s) returned.\n";
          }

          if(exists($sth->{syb_more_results}) && $sth->{syb_more_results}) {
            print "\n";
          }
        }
      } while(exists($sth->{syb_more_results}) && $sth->{syb_more_results});
    } else {
      # Non-select statement. Simply output row count
      if($report && exists($opts->{count})) {
        print $sth->rows, " row(s) affected.\n";
      }
    }

    # Tidy up
    $sth->finish || raiseError($dbh,$sth,"sth->finish",$statement);
  }

  if(!exists($opts->{"auto-commit"})) {
    $dbh->commit || raiseError($dbh,$dbh,"dbh->commit");
  }

  $dbh->disconnect || raiseError($dbh,$dbh,"dbh->disconnect");
}
