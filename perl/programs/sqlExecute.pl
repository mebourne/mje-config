#!/app/perl5005/bin/perl -w
#
# Format SQL output to make it easier to read
# Written by Martin Ebourne. Started 22/05/01

use strict;
use DBI;
use English;
use Env;
use MJE::ParseOpts;
use MJE::TableFormatter;

BEGIN {
  $ENV{ORACLE_HOME}="/app/oracle/product/client/8.1.6";
}

my $dbh = DBI->connect("dbi:Oracle:DLNTRC01", "trcadm", "trcadm")
    || die "Can't connect to Oracle: $DBI::errstr";

$dbh->{LongTruncOk}=1;
$dbh->{ChopBlanks}=1;
$dbh->{AutoCommit}=0;

my $sth = $dbh->prepare( q{
  SELECT *
      FROM CAS_T_TRAN
    }) || die "Can't prepare statement: $DBI::errstr";

my $rc = $sth->execute
    || die "Can't execute statement: $DBI::errstr";

print "<Columns number=$sth->{NUM_OF_FIELDS}>\n";
for my $column (@{$sth->{NAME}}) {
  print " <Column>$column</Column>\n";
}
print "</Columns>\n";

print "<Data>\n";
while (my @data = $sth->fetchrow_array) {
  print " <Row>\n";
  for my $value (@data) {
    #print "  <Value>$value<\Value>\n";
    print "  <Value>", DBI::neat($value), "<\Value>\n";
  }
  print " </Row>\n";
}
print "</Data>\n";

# check for problems which may have terminated the fetch early
die $sth->errstr if $sth->err;

$sth->finish;
$dbh->commit;
$dbh->disconnect;


&main();

exit;

sub main {
}
