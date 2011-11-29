#!/bin/sh
# Install config into home account

ask() {
  echo "$1? (y/N)"
  read answer
  answer=`echo "$answer" | tr -s '[:upper:]' '[:lower:]' | tr -cd 'yn'`
  /usr/bin/test "x$answer" = "xy"
}

backup() {
  file="$1"
  suffix=".save"
  if /usr/bin/test -e "$file" ||
     /usr/bin/test -h "$file" 2>/dev/null ||
     /usr/bin/test -L "$file" 2>/dev/null
  then
    echo "WARNING: Renaming old $file file to $file$suffix"
    rm -rf "$file$suffix"
    mv "$file" "$file$suffix"
  fi
}

scriptdir=`dirname $0`
configdir=`cd $scriptdir/..; pwd`

cd "$HOME"

echo "Using config in $configdir"
echo "Installing into $HOME"
echo

if ask "Install ZSH files"
then
  backup .zsh
  backup .zshenv
  backup .zsh_user
  echo "Creating .zsh"
  mkdir .zsh
  ln -s .zsh/.zshenv .zshenv
  ln -s "$configdir/shell/zlogout"  .zsh/.zlogout
  ln -s "$configdir/shell/zprofile" .zsh/.zprofile
  ln -s "$configdir/shell/zshrc"    .zsh/.zshrc
  ln -s "$configdir/shell/zshenv"   .zsh/.zshenv

  if ask "Install your existing ZSH user directory"
  then
    echo "Available user directories:"
    ( cd "$configdir/shell/user"; echo * )
    userdir=""
    while /usr/bin/test "x$userdir" = "x"
    do
      echo "Enter user directory name:"
      read userdir
      if /usr/bin/test ! -d "$configdir/shell/user/$userdir"
      then
        echo "Invalid directory"
	userdir=""
      fi
    done
    echo "Creating .zsh_user"
    ln -s "$configdir/shell/user/$userdir" .zsh_user
  elif ask "Create private ZSH user directory"
  then
    echo "Creating .zsh_user"
    mkdir .zsh_user
    mkdir .zsh_user/functions

    cat <<EOF > .zsh_user/login
# ZSH configuration file
# Executed for login shells via zprofile
# User's own setup for login shells. Run after all other login setup has been done

EOF

    cat <<EOF > .zsh_user/interactive
# ZSH configuration file
# Executed for interactive shells via zshrc
# User's own setup for interactive shells. Run after all other setup has been done

ttyctl -f
EOF
  fi
fi

if ask "Install Emacs files"
then
  backup .emacs
  backup .emacs.el
  if ask "Create private Emacs user file"
  then
    echo "Creating .emacs.el"
    cat <<EOF > .emacs.el
;; Emacs configuration file
;; .emacs.el, main user startup file

(load "$configdir/emacs/startup")
(setq custom-file "~/.custom.el")
(load custom-file)
EOF
  else
    echo "Creating .emacs.el"
    ln -s "$configdir/emacs/user/emacs.el" .emacs.el
    backup .gnus
    echo "Creating .gnus"
    ln -s "$configdir/emacs/user/gnus.el" .gnus
  fi
fi

if ask "Install X files"
then
  backup .Xdefaults
  backup .Xresources
  backup .dbxrc
  echo "Creating .Xdefaults"
  ln -s "$configdir/window/Xdefaults" .Xdefaults
  echo "Creating .Xresources"
  ln -s "$configdir/window/Xdefaults" .Xresources
  echo "Creating .dbxrc"
  ln -s "$configdir/tools/dbxrc" .dbxrc
fi

if ask "Install Sawfish files"
then
  backup .sawfishrc
  backup .sawfish
  echo "Creating .sawfishrc"
  ln -s "$configdir/window/sawfish/sawfishrc" .sawfishrc
  echo "Creating .sawfish"
  mkdir .sawfish
  ln -s "$configdir/window/sawfish/lisp" .sawfish/lisp
  ln -s "$configdir/window/sawfish/themes" .sawfish/themes
  if ask "Create private Sawfish user file"
  then
    :
  else
    echo "Creating .sawfish/custom"
    ln -s "$configdir/window/sawfish/custom" .sawfish/custom
  fi
fi

if ask "Install FVWM2 files"
then
  backup .fvwm2rc
  echo "Creating .fvwm2rc"
  ln -s "$configdir/window/fvwm/fvwm2rc" .fvwm2rc
fi


if ask "Install Gnome files"
then
  if /usr/bin/test ! -d .themes
  then
    echo "Creating .themes"
    mkdir .themes
  fi
  for theme in `cd $configdir/window/gnome/themes; echo *`
  do
    backup .themes/$theme
    echo "Creating .themes/$theme"
    ln -s "$configdir/window/gnome/themes/$theme" .themes/$theme
  done
fi

if ask "Install RPM files"
then
  backup .rpmmacros
  echo "Creating .rpmmacros"
  ln -s "$configdir/tools/rpmmacros" .rpmmacros
fi

echo "Install completed"
