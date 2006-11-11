#!/bin/zsh

musicDir=/srv/media/audio
linksDir=/srv/media/audio-myth

getLetter() {
  local artist="$1" letter=""

  if [[ ${artist:t:u} == THE* ]]
  then
    letter=The
  else
    letter=${${artist:t}[1]:u}
  fi

  echo $letter
}

makeLinks() {
  local group="$1"

  local dir="$linksDir/$group"

  mkdir $linksDir/$group
  while [[ -n $group ]]
  do
    local letter=""
    if [[ $group == The* ]]
    then
      letter=The
    else
      letter=$group[1]
    fi
    group=${group#$letter}

    for artist in $artists
    do
      if [[ $artist != *lost\+found && $artist == $musicDir/$letter* ]]
      then
        if [[ $letter == $(getLetter $artist) ]]
	then
	  ln -s $artist $dir/
	fi
      fi
    done
  done
}

mkdir $linksDir/$$
rm -f $linksDir/*/*(@N)
rmdir $linksDir/*(/N)

typeset -a artists
typeset -A letters
artists=($musicDir/*(/))
for artist in $artists
do
  if [[ $artist != *lost\+found ]]
  then
    letter=$(getLetter $artist)
    (( letters[$letter]++ ))
  fi
done

local group="" count=0
for letter in ${(ko)letters}
do
  if (( count + letters[$letter] < 15 ))
  then
    group="$group$letter"
    (( count += letters[$letter] ))
  else
    makeLinks $group
    group=$letter
    count=$letters[$letter]
  fi
done
makeLinks $group
