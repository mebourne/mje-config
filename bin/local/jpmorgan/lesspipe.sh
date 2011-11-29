#!/home/odyssey/mebourne/local/bin/sun4u/zsh
# Fancy input pipe script for use with less
# Written by Martin Ebourne, 05/04/2001
#
# To enable:
#   export LESSOPEN="|lesspipe.sh %s"
#
# To use:
# - Will automatically process compressed files, archives, and many other types of
#   file producing them in a useful readable format, with usage hints.
# - To view files within an archive, append `:<filename-within>' to the archive filename.
#   This can be repeated indefinitely.
# - To prevent the leaf file from being formatted/processed on output append `:' to the
#   filename (may still be decompressed).
# - To prevent the leaf file from being decompressed and formatted/processed append `::'
#   to the filename.


### Command tables ###

# These are the commands for uncompressing files.
# Must use $input but not $component. Result to stdout
uncompress_cmds() {
  case $argv[1] in
    *bzip*)			cmd='do_bzip $input';;
    *gzip*)			cmd='do_gzip $input';;
    *compress*)			cmd='uncompress -c $input';;
  esac
}

# These are the commands for extracting files from archives.
# Must use $input and $component. Result to stdout
extract_cmds() {
  case $argv[1] in
    *tar*)			cmd='do_gtar Oxf $input $component';;
    *Zip*)			cmd='unzip -avp $input $component';;
    *ar\ archive*)		cmd='ar p $input $component';;
  esac
}

# These are the commands for processing the end file.
# Must use $input but not $component. Result to stdout
convert_cmds() {
  case $argv[1] in
    *tar*)
      cmd='tar tvf $input'
      comment=$'listing archive, append `:\' to see archive or `:file\' to view contained file';;

    *Zip*)
      cmd='unzip -lv $input'
      comment=$'listing archive, append `:\' to see archive or `:file\' to view contained file';;

    *ar\ archive*)
      cmd='ar vt $input'
      comment=$'listing archive, append `:\' to see archive or `:file\' to view contained file';;

    *directory*)
      cmd='ls -alFhb $input'
      comment=$'displaying directory with ls';;

    *roff*)
      cmd='do_nroff $input'
      comment=$'displaying manpage with nroff, append `:\' to see source';;

    *script*) # This stops scripts being caught by *executable* below
      ;;

    *executable*)
      cmd='strings $input'
      comment=$'displaying executable with strings, append `:\' to see binary';;

    *shared*)
      cmd='nm $input'
      comment=$'displaying library with nm, append `:\' to see binary';;

    *dynamic\ lib*)
      cmd='nm $input'
      comment=$'displaying library with nm, append `:\' to see binary';;

    *relocatable*)
      cmd='nm $input'
      comment=$'displaying object with nm, append `:\' to see binary';;

    *HTML*)
      cmd='do_lynx $input'
      comment=$'displaying HTML with lynx, append `:\' to see source';;

    *PDF*)
      cmd='do_pdftotext $input'
      comment=$'displaying PDF with pdftotext, append `:\' to see binary';;

    *Microsoft\ (Word|Office)*)
      cmd='do_antiword $input'
      comment=$'displaying MSWord with antiword, append `:\' to see binary';;

    *data*)
      cmd="strings $input"
      comment=$'displaying binary data file with strings, append `:\' to see binary';;
  esac
}


### Complex commands ###

# Execute bzip checking for availability
do_bzip() {
  if whence -p bzip2 >/dev/null
  then
    bzip2 -c -d "$argv[@]"
  else
    fatal_error "Cannot process bzip file - bzip2 command missing"
  fi
}

# Execute gzip checking for availability
do_gzip() {
  if whence -p gzip >/dev/null
  then
    gzip -c -d "$argv[@]"
  else
    fatal_error "Cannot process gzip file - gzip command missing"
  fi
}

# Execute GNU version of tar checking for availability
do_gtar() {
  if whence -p gtar >/dev/null && [[ $(gtar --version 2>&1) == *GNU* ]]
  then
    gtar "$argv[@]"
  elif [[ $(tar --version 2>&1) == *GNU* ]]
  then
    tar "$argv[@]"
  else
    fatal_error "Cannot process tar file - GNU tar command missing"
  fi
}

# Execute groff or nroff as available
do_nroff() {
  if whence -p groff >/dev/null
  then
    groff -s -p -t -e -Tascii -mandoc "$argv[@]"
  else
    # sed to compress blank lines
    nroff -man "$argv[@]" | sed -n '
/./ {
  p
  d
}
/^$/ p
:Empty
/^$/ {
  N
  s/.//
  b Empty
}
p
'
  fi
}

# Execute lynx checking for availability
do_lynx() {
  if whence -p lynx >/dev/null
  then
    lynx -dump "$argv[@]"
  else
    fatal_error "Cannot process HTML file - lynx command missing"
  fi
}

# Execute pdftotext checking for availability
do_pdftotext() {
  if whence -p pdftotext >/dev/null
  then
    pdftotext -dump "$argv[@]"
  else
    fatal_error "Cannot process PDF file - pdftotext command missing"
  fi
}

# Execute antiword checking for availability
do_antiword() {
  if whence -p antiword >/dev/null
  then
    antiword -dump "$argv[@]"
  else
    fatal_error "Cannot process Micrsoft Word file - antiword command missing"
  fi
}


### The program ###

# Cleanup function
cleanup() {

  # Delete any temporary files, careful not to cause error
  rm -f $tmpprefix*(N)

  # Return with the original error code
  return $(( argv[1] ? 128 + argv[1] : 0 ))
}

# Abort with a fatal error
fatal_error() {

  # Report the error on both streams since only one of them may be available at the time
  echo "ERROR: $argv[*]"
  echo "ERROR: $argv[*]" 1>&2

  # Don't give a failure return code because then less will revert to trying to display the
  # original file, rather than stopping
  exit
}

comment() {
  if [[ -n "$argv[*]" ]]
  then
    echotc mr
    echo "$argv[*]"
    echotc me
  fi
}

# The processing routine where all the work is done
# decode_components <num-trailing> <components> ...
decode_components() {
  integer trailing=$argv[1]
  local input="$argv[2]" component="$argv[3]"

  local cmd comment
  while true
  do
    # Execute 'file' command to determine the file type
    local contents="$(eval $filecmd $input || echo error)"

    # Strip filename off the start to avoid false matches
    contents=${contents##*:[$IFS]}

    case $contents in
      error)
        fatal_error "Command 'file' failed"
	;;

      *No\ such\ file*)
        # less will report this for us
        exit 1
        ;;

      *)
        # First check to see if the file is compressed. If so we'll need to uncompress
	# it & then try again
        cmd=""
	uncompress_cmds $contents
	if [[ -n $cmd ]]
	then

	  # This needs uncompressing unless the user has given `::' and it's the leaf
	  # component
	  if (( $#component || trailing < 2 ))
	  then
	    if [[ -z $component ]]
	    then
              comment "uncompressing, append \`::' to view raw file"
	    fi
	    output=$tmpprefix$RANDOM
	    eval $cmd > $output
	    input=$output
	  else
	    cat $input
	    return
	  fi
	elif [[ -n $component ]]
	then

	  # Wasn't compressed, but we have a component so user think's it an archive.
	  # Either we manage to extract a file or we fail here
	  extract_cmds $contents
	  if [[ -n $cmd ]]
	  then
	    output=$tmpprefix$RANDOM
	    eval $cmd > $output
	    input=$output
	    shift
	    component=$argv[3]
	  else
	    fatal_error "Cannot extract file from type '$contents'"
	  fi
	else

	  # Not compressed and leaf component, so display it however
	  convert_cmds $contents
	  if (( $#cmd && !trailing ))
	  then

	    # No trailing `:' and found a processing command, so use it
	    comment "$comment"
	    eval $cmd
	  elif [[ $input == $tmpprefix* || $trailing -ne 0 ]]
	  then

	    # Trailing `:' or no processing command but already been processed (eg.
	    # extracted from archive). Need to cat else less can't find the file
	    cat $input
	  fi
	  # Else less will find the file direct, which should be more efficient

	  return
	fi
	;;
    esac
  done
}

main() {
  local file="$argv[1]"

  # Need this for some of our expansions
  setopt extended_glob

  # Default prefix for our temporary files
  typeset -g tmpprefix="${TMPPREFIX:-/tmp/zsh}.lp.$$."

  # Determine which sort of file command we have. BSD doesn't follow links but has an
  # option so it does, Sun does follow links but has an option so it doesn't. Good
  # stuff.
  local filecmd="file -L"
  if ! eval $filecmd /dev/null >/dev/null 2>&1
  then
    filecmd="file"
  fi

  # Clean up before quitting
  trap cleanup EXIT INT QUIT

  # Split the : separated argument up into normal space separated ones and call the
  # processing routine
  decode_components ${#${(M)file%%:#}} ${(s-:-)file}
}

main "$argv[@]"
