# Perl package MJE::Db
# Provide general utility functions
# Written by Martin Ebourne, 06/08/2001
# $Id: Database.pm 792 2003-09-22 11:47:18Z martin $
#
# Usage:
#
#   use MJE::Db qw(:all);  # Note this imports all symbols

package MJE::Db;

require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw();
@EXPORT_OK = qw(
		&sqlExec
		);
%EXPORT_TAGS = ( all => \@EXPORT_OK );

use strict;
use Sybase::DBlib;


### Database related

# Execute the given statements against the database in the given descriptor.
# descriptor is a hash reference containing: server, database, user, password.
# Returns a 2d array of the results of the last statement
sub sqlExec() {
  my ($db, @statements)=@_;

  die "Invalid database descriptor" if !defined($db);

  my $connection=new Sybase::DBlib($db->{user},$db->{password},$db->{server});
  $connection->dbuse($db->{database});

  for my $statement (@statements) {
    $connection->dbcmd($statement . "\n");
    $connection->dbsqlexec;
  }
  $connection->dbresults;
  my @results;
  while(my @row=$connection->dbnextrow)
  {
    push @results, \@row;
  }

  return @results;
}

return 1;
