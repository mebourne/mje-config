#! /bin/sh
case "$1" in
*.tar.Z) uncompress -c $1 2>/dev/null | tar tf -
	  ;;
*.tar.gz) gunzip -c $1 2>/dev/null | tar tf -
	  ;;
*.Z) uncompress -c $1 2>/dev/null
     ;;
*.gz) gunzip -c $1 2>/dev/null
      ;;
*.[0-9]) # Check for a line near the top starting with a '.' command
	 head -n 3 $1 | grep -q '^\.' && nroff -man $1 2>/dev/null
         ;;
esac
