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
*.[0-9]) nroff -man $1 2>/dev/null
         ;;
esac
