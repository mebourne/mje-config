package MJE::XmlHash;

use UNIVERSAL qw(isa);
use vars qw($KeyAttribute);

$KeyAttribute = undef;

sub Init {
  my $expat = shift;
  $expat->{Stack} = [];
  $expat->{Curhash} = $expat->{Tree} = {};
}

sub Start {
  my $expat = shift;
  my $tag = shift;
  my $newelement = { @_ };

  my $key;

  if(defined($KeyAttribute) && $KeyAttribute ne "") {
    $key=$newelement->{$KeyAttribute};
  }

  if(defined($key)) {
    if(exists($expat->{Curhash}->{$tag})) {
      if(isa($expat->{Curhash}->{$tag}, "HASH")) {
	$expat->{Curhash}->{$tag}->{$key}=$newelement;
      } else {
	$expat->xpcroak("Expected element with key attribute");
      }
    } else {
      $expat->{Curhash}->{$tag}={ $key => $newelement };
    }
  } else {
    if(exists($expat->{Curhash}->{$tag})) {
      if(isa($expat->{Curhash}->{$tag}, "ARRAY")) {
	push @{$expat->{Curhash}->{$tag}}, $newelement;
      } else {
	$expat->xpcroak("Expected element without key attribute");
      }
    } else {
      $expat->{Curhash}->{$tag}=[ $newelement ];
    }
  }

  push @{$expat->{Stack}}, $expat->{Curhash};
  $expat->{Curhash} = $newelement;
}

sub End {
  my $expat = shift;
  my $tag = shift;
  $expat->{Curhash} = pop @{$expat->{Stack}};
}

sub Char {
  my $expat = shift;
  my $text = shift;

  if(exists($expat->{Curhash}->{Text})) {
    $expat->{Curhash}->{Text}.=$text;
  } else {
    if($text!~/^\s*$/) {
      $expat->{Curhash}->{Text}=$text;
    }
  }
}

sub Final {
  my $expat = shift;
  delete $expat->{Curhash};
  delete $expat->{Stack};
  return $expat->{Tree};
}

return 1;
