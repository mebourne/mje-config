# Perl package MJE::ParseOpts
# Sophisticated and easy to use command line argument parsing
# Written by Martin Ebourne, 06/08/2001
#
# Usage:
#
#   use MJE::ParseOpts;
#
#   my $opts=new ACE::ParseOpts (<<EOF);
#   <help text>
#   EOF

package MJE::ParseOpts;

use strict;

use Data::Dumper;


# Create a new ACE::Config object
sub new {
  my ($proto, $helpText, $argv)=@_;
  my $class=ref($proto) || $proto;
  my $self={};
  my $obj=bless($self,$class);

  $self->{_debug}=0;

  if(defined($helpText)) {
    $obj->processArray($helpText,$argv) || undef $obj;
  }

  return $obj;
}


sub processArray {
  my ($self, $helpText, $argv)=@_;

  if(defined($argv)) {
    $self->{_argv}=$argv;
  } else {
    $self->{_argv}=\@ARGV;
  }

  $self->{_types}={};
  $self->{_options}={};
  $self->{_arguments}=[];
  $self->{_minargs}=0;

  # Parse the default definitions for types etc.
  $self->setupDefinitions || return 0;

  # Parse the help string we've got to determine the type of options
  $self->parseHelp($helpText) || return 0;

  # Useful for debugging
  if($self->{_debug}) {
    print Dumper($self), "\n";
  }

  if($self->doOptions) {

#print Dumper($self), "\n";

    if($self->{help}) {
      print $self->{_helptext};
      return 0;
    }

    if($self->{"zsh-completion"}) {
      $self->generateCompletion;
      return 0;
    }

    if(!$self->doArguments) {

      # There was an error in processing. Mention the --help option
      print STDERR "Use --help for more information\n";
      return 0;
    }

  } else {

    # There was an error in processing. Mention the --help option
    print STDERR "Use --help for more information\n";
    return 0;
  }

  return 1;
}


sub setupDefinitions() {
  my ($self)=@_;

  $self->parseHelp('
## These are extensions of the basic built in types
#type switchoff     constant=0                       ""                         ""
#type integer       regexp=([-+]|)\d+                "signed integer number"    " "
#type posinteger    regexp=\d+                       "positive integer number"  " "
#type text          string                           text                       " "

## These are file-like things validated with test
#type directory     exec=-d                          directory                  _directories
#type file          exec=-f                          file                       _files

# --help
# --zsh-completion
') || return 0;
}


sub parseHelp {
  my ($self, $help)=@_;

  my $helpText="";
  my $helpLine="";
  for my $line (split(/\n/,$help)) {
    chomp $line;
    if($line=~/^\s*\#/) {
      my @fields=$self->splitByField($line);

      my $command=shift @fields;
      if($command eq "#") {

	# An option description line
	$self->parseOption($line,$helpLine,@fields) || return 0;
      } elsif($command eq "#type") {

	# A type description line
	$self->parseType(@fields) || return 0;
      } elsif($command eq "##") {

        # A comment
      } else {

	# A configuration error
	print STDERR "Invalid command: '$command'\n";
	return 0;
      }
    } else {
      # Doesn't start with a # so append it to help
      $helpText.="$line\n";

      # If we have a line which has non-whitespace before any tabs it's an
      # option or argument or something similar. So we reset our current
      # helpline because the description will come after it
      if($line=~/^ *\S/) {
	$helpLine="";
      }

      # If the line has a tab on it then it might have a description after the tab
      if($line=~/\t/) {

        # Extract the text after any tabs, and trim it of whitespace
	$line=~s/^.*\t\s*(.*?)\s*$/$1/;
	if($line ne "") {

          # Add to current helpline
	  if($helpLine ne "") {
	    $helpLine.=" $line";
	  } else {
	    $helpLine=$line;
	  }
	}
      }
    }
  }

  $self->{_helptext}=$helpText;

  return 1;
}


# This function handles the parsing of option definitions for parseHelp
sub parseOption {
  my ($self, $line, $helpLine, @fields)=@_;

  # Extract the type information if there is any
  my @type;
  my $validate=1;
  my $index=$self->findEntry(":",@fields);
  if($index>=0) {
    @type=@fields[$index+1..$#fields];
    @fields=@fields[0..$index-1];

    # Set validate flag - false if '?' preceeds type
    if($type[0] eq "?") {
      shift @type;
      $validate=0;
    }
  }

  # Decode whether append or not (ie. where '+=' instead of '=')
  my $append=$self->findEntry("+=",@fields)>=0 ? 1 : 0;

  # Extract the tag information if there is any
  my @tag;
  $index=$self->findEntry($append ? "+=" : "=",@fields);
  if($index>=0) {
    @tag=@fields[0..$index-1];
    @fields=@fields[$index+1..$#fields];
  }

  # Handle the optional frequency definition
  # (default is [1] for arguments, [0,1] for options)
  my $frequency;
  if($fields[0]=~/^\[.*\]$/) {
    ($frequency)=$fields[0]=~/^\[(.*)\]$/;
    shift @fields;
  }

  # Options is remainder less the '|'
  my @options=grep($_ ne "|",@fields);

  # Check we've got at last one option/argument name
  if(!@options) {
    print STDERR "Missing option/argument name in definition: '$line'\n";
    return 0;
  }

  # If tag not provided then default to first option name with any leading '-'s removed
  if(!@tag) {
    @tag=$options[0]=~/^-*(.*)$/;
    $tag[0]="[$tag[0]]";
  }

  # Assign all the details for the current options/argument
  for my $option (@options) {
    my (@aliases, @excludes);

    # Establish defaults for missing values
    if($option=~/^-/) {
      if(!defined($frequency)) {
	$frequency="0,1";
      }
      if(!@type) {
	@type=("switch");
      }

      # Aliases, excluding this option
      @aliases=grep($_ ne $option,@options);

      # Excludes. Only if the max frequency of this option is 1, same as aliases
      my ($min, $max);
      $self->getMinMaxFrequency(\$min,\$max,$frequency) || return 0;
      if($max==1) {
	@excludes=@aliases;
      }
    } else {

      if(!defined($frequency)) {
	$frequency="1";
      }
      if(!@type) {
	@type=("string");
      }

      # Calculate the minimum number of arguments we need for the command to
      # be able to run. We need this later to know how many arguments are
      # spare when we have a choice on how many to use
      my ($min, $max);
      $self->getMinMaxFrequency(\$min,\$max,$frequency) || return 0;
      $self->{_minargs}+=$min;

      # Record the option in list of arguments
      push @{$self->{_arguments}}, $option;
    }

    # Report info on option if debugging enabled
    if($self->{_debug}) {
      print "option = '$option' {\n";
      print "  tag       = '@tag'\n";
      print "  append    = $append\n";
      print "  frequency = '$frequency'\n";
      print "  type      = '@type'\n";
      print "  validate  = $validate\n";
      print "  help      = '$helpLine'\n";
      print "  excludes  = '@excludes'\n";
      print "  aliases   = '@aliases'\n";
      print "}\n";
    }

    # Store option details in hash
    $self->{_options}->{$option}={
      option      => $option,
      tag         => "@tag",
      append      => $append,
      frequency   => $frequency,
      type        => "@type",
      validate    => $validate,
      excludes    => \@excludes,
      aliases     => \@aliases,
      help        => $helpLine
	};
  }

  return 1;
}

# This function handles the parsing of type definitions for parseHelp
sub parseType {
  my ($self, @fields)=@_;

  my ($type, $baseType, $description, $action)=@fields;

  $description=$self->unQuote($description);
  $action=$self->unQuote($action);

  # Report info on type if debugging enabled
  if($self->{_debug}) {
    print "type = '$type' {\n";
    print "  basetype    = '$baseType'\n";
    print "  description = '$description'\n";
    print "  action      = '$action'\n";
    print "}\n";
  }

  # Store type details in hash
  $self->{_types}->{$type}={
    type        => $type,
    base        => $baseType,
    description => $description,
    action      => $action
      };

  return 1;
}

sub doOptions {
  my ($self)=@_;

  # Process each argument while it is an option
  my $argv=$self->{_argv};
  while(@$argv && $argv->[0]=~/^-/) {
    my $option=shift @$argv;

    if($option eq "--") {

      # End of option list

      # Nothing else to do - return success
      return 1;
    } elsif($option=~/^--/) {

      # Long argument

      # Check to see if the option exists
      my ($value)=$option=~/=(.*)$/;
      $option=~s/=.*$//;
      if(exists($self->{_options}->{$option})) {
	$self->handleType($option,$value) || return 0;
      } else {
	print STDERR "Invalid option: $option\n";
	return 0;
      }
    } elsif($option=~/^-./) {

      # Short option (may be grouped)
      my $optionGroup=substr($option,1);
      while($optionGroup ne "") {
	my $option="-" . substr($optionGroup,0,1);
	$optionGroup=substr($optionGroup,1);

	if(exists($self->{_options}->{$option})) {
	  $self->handleType($option) || return 0;
	} else {
	  print STDERR "Invalid option: $option\n";
	  return 0;
	}
      }
    } else {

      # Must be a lone -
      print STDERR "Missing option after -\n";
      return 0;
    }
  }

  return 1;
}

sub doArguments {
  my ($self)=@_;

  my $spareArgs=@{$self->{_argv}} - $self->{_minargs};
  if($spareArgs<0) {
    print STDERR "Insufficient arguments\n";
    return 0;
  }

  # Process each argument specification
  for my $argument (@{$self->{_arguments}}) {
    my $option=$self->{_options}->{$argument};

    # Ensure number of args to use is within required range
    my ($min, $max);
    $self->getMinMaxFrequency(\$min,\$max,$option->{frequency}) || return 0;

    # Try to grab all the spare arguments
    my $useArgs=$min + $spareArgs;

    # Restrict this if that's too many
    if($max>=0 && $useArgs>$max) {
      $useArgs=$max;
    }

    # Recalculate how many arguments are spare for the next option
    $spareArgs-=$useArgs-$min;

    # This shouldn't happen due to being checked above
    if($useArgs<$min || @{$self->{_argv}}<$useArgs) {
      print STDERR "Argument processing error\n";
      return 0;
    }

    # Process each of them according to the type
    for (1..$useArgs) {
      $self->handleType($argument,shift @{$self->{_argv}}) || return 0;
    }
  }

  # If there's any surplus arguments that's an error
  if(@{$self->{_argv}}) {
    print STDERR "Too many arguments\n";
    return 0;
  }
  
  return 1;
}

sub handleType {
  my ($self, $option, $givenValue)=@_;

  my $info=$self->{_options}->{$option};
  my $validate=$info->{validate};

  # Recursively convert user defined type into base type
  my $type=$info->{type};
  while(exists($self->{_types}->{$type})) {
    $type=$self->{_types}->{$type}->{base};
  }

  my ($param)=$type=~/=(.*)$/;
  $param=$self->unQuote($param) if defined($param);
  $type=~s/=.*$//;
  my $value;
  my $isValid=1;

  if($type eq "switch") {

    # Simple switch. Mark its presence
    $value=1;
  } elsif($type eq "constant") {

    # Constant to be assigned (after quote removal). eg. constant=value or constant="more values"
    $value=$param;
  } elsif($type eq "string") {

    # Takes a parameter with no validation
    $self->getNextValue(\$value,$givenValue) || return 0;
  } elsif($type eq "values") {

    # Takes a parameter, validated by list of values from external array
    $self->getNextValue(\$value,$givenValue) || return 0;
    if($validate) {
      my @values;
      $self->expandValues($param,\@values) || return 0;

      if(!grep($_ eq $value,@values)) {
	$isValid=0;
      }
    }
  } elsif($type eq "regexp") {

    # Takes a parameter which is a regexp for validation
    $self->getNextValue(\$value,$givenValue) || return 0;
    if($validate) {
      if($value!~/^$param$/) {
	$isValid=0;
      }
    }
  } elsif($type eq "exec") {

    # Takes a parameter which is a command for validation
    $self->getNextValue(\$value,$givenValue) || return 0;
    if($validate) {
      if(! eval "$param \$value") {
	$isValid=0;
      }
    }
  } else {

    # This is a configuration error
    print STDERR "Invalid option type for option $option: '$type'\n";
    return 0;
  }

  if(!$isValid) {
    print STDERR "Invalid value for option $option: '$value'\n";
    return 0;
  }

  $self->store($option,$value);

  return 1;
}

sub getNextValue {
  my ($self, $var, $param)=@_;

  if(defined($param)) {
    $$var=$param;
  } elsif(@{$self->{_argv}}) {
    $$var=shift @{$self->{_argv}};
  } else {
    print STDERR "Missing parameter to option '\$name'\n"; # FIXME
    return 0;
  }

  return 1;
}

sub store {
  my ($self, $option, $value)=@_;

  my $tag=$self->{_options}->{$option}->{tag};
  my $append=$self->{_options}->{$option}->{append};

  if($tag=~/^\[.*\]$/) {

    # Store in the default options associative array
    $tag=~s/^\[(.*)\]$/$1/;
    if($append && defined($self->{$tag})) {
      $self->{$tag}.=" $value";
    } else {
      $self->{$tag}=$value;
    }
  } elsif($tag=~/^@/) {
    no strict 'refs';

    my $arrayRef=substr($tag,1);
    $arrayRef="::" . $arrayRef if $arrayRef!~/^::/;

    if($append) {
      push @$arrayRef, $value;
    } else {
      @$arrayRef=($value);
    }
  } else {
    # ...
  }

  return 1;
}

sub expandValues {
  my ($self, $param, $list, $hash)=@_;

  if($param=~/^\(.*\)$/) {

    # Literal list
    $param=~s/^\((.*)\)$/$1/;
    @$list=eval $param;
  } elsif($param=~/^@/) {

    # Array variable substitution
    @$list=eval $param;
  } elsif($param=~/^%/) {

    # Hash variable substitution
    @$list=eval "keys($param)";
    if(defined($hash)) {
      %$hash=eval $param;
    }
  } else {
    
    # String variable substitution
    # ...
  }

  return 1;
}

sub generateCompletion {
  my ($self)=@_;

  my @specs;

  # Process each option to build a list of specifications for _arguments.
  # Careful to skip our magic --zsh-completion option
  for my $optionName (grep(/^-/,keys(%{$self->{_options}})), @{$self->{_arguments}}) {
    my $option=$self->{_options}->{$optionName};
    if($optionName eq "--zsh-completion") {
      next;
    }

    my ($min, $max);
    $self->getMinMaxFrequency(\$min,\$max,$option->{frequency}) || return 0;

    # Try to determine a description/completer from the type of this option
    my ($description, $action);
    $self->generateCompletionType(\$description,\$action,$option) || return 0;

    my $spec="";
    if($optionName=~/^-/) {

      # Generate excludes clause if we have any
      if(@{$option->{excludes}}) {
	$spec.="(@{$option->{excludes}})";
      }

      # Add repeating option flag if max frequency is not 1
      if($max!=1) {
	$spec.="*";
      }

      # Add the main option name section
      $spec.=$optionName;

      # If we have a long option which takes a parameter then add the '=' flag
      # to say --option=value is valid
      if($optionName=~/^--/ && $action ne "") {
	$spec.="=";
      }
      
      # Add the description of the option if one was successfully extracted from the help text
      if($option->{help} ne "") {
	my ($initial)=$option->{help};
	$initial=~s/\. .*$//;
        $spec.="[$initial]";
      }

      # Now add the description and completer for the parameter, if we have one
      if($action ne "") {
	$spec.=":$description:$action";
      }

      push @specs, "'$spec'";
    } else {

      if($max<0) {
	$spec.="*";
      }

      $spec.=":" . $description;

      # Add the description of the argument if one was successfully extracted from the help text
      if($option->{help} ne "") {
	my ($initial)=$option->{help};
	$initial=~s/\. .*$//;
	$initial=~s/:/\\:/g;
        $spec.=" - $initial";
      }

      $spec.=":$action";

      # Collect the specifications
      if($min<0 || $max<0 || $min>$max) {
	push @specs, "'$spec'";
      } else {
	for my $i (1..$min) {
	  push @specs, "'$spec'";
	}
	for my $i ($min+1..$max) {
	  push @specs, "':$spec'";
	}
      }
    }
  }

  # Only output the _arguments call on success
  # -s - Allow option grouping for single letter options
  # -w - Arguments follow single letter options, one for each relevant option
  # -S - Handle -- to terminate options
  # -A - No more options after first non-option argument
  print "_arguments -s -w -S -A '-*' @specs\n";

  return 1;
}

sub generateCompletionType {
  my ($self, $descVar, $actionVar, $option)=@_;

  my $description="";
  my $action="";
  my $type=$option->{type};

  # Recursively convert user defined type into base type
  while(exists($self->{_types}->{$type})) {
    my $newType=$self->{_types}->{$type};

    # Take any settings supplied from the user defined type which we haven't already got
    if($description eq "") {
      $description=$newType->{description};
    }
    if($action eq "") {
      $action=$newType->{action};
    }

    $type=$newType->{base};
  }

  # Decode built in type to set anything not done already
  my ($param)=$type=~/=(.*)$/;
  $type=~s/=.*$//;

  if($type eq "switch" || $type eq "constant") {

    # No parameters, leave blank
  } elsif($type eq "string") {

    # Takes a parameter with no validation
    $description="string" if $description eq "";
    $action="_files" if $action eq "";
  } elsif($type eq "values") {

    # Takes a parameter, validated by list of values
    $description="value" if $description eq "";
    if($action eq "") {
      my @values;
      my %hash;
      $self->expandValues($param,\@values,\%hash) || return 0;

      if(scalar(keys(%hash))) {
	$action="((";
	for my $key (keys(%hash)) {
	  $action.=$self->quote($key) . "\\:" . $self->quote($hash{$key});
	}
	$action.="))";
      } else {
	@values=map { $self->quote($_); } @values;
	$action="(@values)";
      }
    }
  } elsif($type eq "regexp") {

    # Takes a parameter which is a regexp for validation
    $description="value" if $description eq "";
    $action=" " if $action eq "";
  } elsif($type eq "exec") {

    # Takes a parameter which is a command for validation
    $description="value" if $description eq "";
    $action=" " if $action eq "";
  } else {

    # This is a configuration error
    print STDERR "Invalid option type for option $option: '$type'\n";
    return 0;
  }

  $$descVar=$description;
  $$actionVar=$action;

  return 1;
}

# This function decodes a frequency expression into min and max values. The
# variables given to minvar & maxvar will be updated with the results of
# processing the given expression substituting -1 for `*'
sub getMinMaxFrequency {
  my ($self, $minVar, $maxVar, $expression)=@_;

  if($expression=~/^\d+,.*$/) {

    # Separate min & max values
    my $value;
    $self->getFrequencyValue($minVar,0,$expression=~/^(.*),/) || return 0;
    $self->getFrequencyValue($maxVar,-1,$expression=~/,(.*)$/) || return 0;
  } elsif($expression=~/^.*,.*$/) {

    # Must be a number in the first field, this is invalid
    print STDERR "Unrecognised frequency expression: '$expression'\n";
    return 0;
  } else {

    # Min & max values the same
    $self->getFrequencyValue($minVar,0,$expression) || return 0;
    $self->getFrequencyValue($maxVar,-1,$expression) || return 0;
  }

  return 1;
}

sub getFrequencyValue {
  my ($self, $var, $number, $expression)=@_;

  if($expression eq "*") {
    $$var=$number;
  } elsif($expression=~/^\d+$/) {
    $$var=$expression;
  } else {
    print STDERR "Unrecognised frequency: '$expression'\n";
    return 0;
  }

  return 1;
}

sub extractField {
  my ($self, $text)=@_;
  my $result="";

  # Remove leading whitespace
  $text=~s/^\s*//;

  # Until we get to unquoted whitespace or end, extract chunks
  while($text!~/^(\s.*|)$/) {
    my $chunk;

    # If starts with a quote then extract a quoted chunk, else extract up to a
    # non-escaped quote or whitespace
    if($text=~/^[\'\"]/) {
      my ($delim)=substr($text,0,1);
      ($chunk, undef, undef, $text)=$text=~/($delim([^$delim\\]|\\[$delim\\])*($delim|))(.*)$/;
    } else {
      ($chunk, undef, $text)=$text=~/^(([^\'\"\\\s]|\\.)*)(.*)$/;
    }

    # Collect the chunks to form the result
    $result.=$chunk;
  }

  return ($result, $text);
}

sub splitByField {
  my ($self, $text)=@_;

  # Extract each field into an array until we have an empty string
  my @result;
  while($text!~/^\s*$/) {
    my $field;
    ($field,$text)=$self->extractField($text);
    push @result,$field;
  }

  return @result;
}

# Return the index of the given entry in to the array, or -1 if not found
sub findEntry {
  my ($self, $entry, @array)=@_;

  my $result=-1;
  for(my $i=0;$result==-1 && $i<@array;$i++) {
    if($array[$i] eq $entry) {
      $result=$i;
    }
  }

  return $result;
}

# Unquote a string, in a similar manner to the shell
sub unQuote {
  my ($self, $string)=@_;

  # Remove start and end matching quotes if any
  my $delim;
  if($string=~/^[\'\"]/) {
    ($delim)=substr($string,0,1);
    if($string=~/$delim$/) {
      $string=substr($string,1,-1);
    }
  }

  # Unless in single quotes, remove escapes
  if(!defined($delim) || $delim eq '"') {
    $string=~s/\\(.)/$1/g;
  }

  return $string;
}

# Quote the spaces in a string (ie. prefix them with '\')
sub quote {
  my ($self, $string)=@_;

  $string=~s/ /\\ /g;

  return $string;
}

return 1;
