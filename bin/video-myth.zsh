#!/bin/zsh

mountsDir=/srv/media/video-mounts
videoDir=/srv/media/video

umask 002

typeset -a videos

for video in $mountsDir/**/*(.N)
do
  relsrc=${video#$mountsDir/}
  dst=$videoDir/${relsrc#*/}
  mkdir -p ${dst:h}

  [[ -L $dst ]] && rm -f $dst
  ln -s $video $dst
done

rm -f $videoDir/**/*(N@^-./)
mkdir $videoDir/$$
rmdir $videoDir/**/*(/^F)
