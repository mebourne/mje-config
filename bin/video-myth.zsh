#!/bin/zsh

mountsDir=/srv/media/video-mounts
videoDir=/srv/media/video

umask 002

typeset -a videos

for video in $mountsDir/video*/{Childrens,Films,Music,Ours,TV}/**/*.{avi,iso,mkv,mp4,mpg,vob,webm}(.N)
do
  relsrc=${video#$mountsDir/}
  dst=$videoDir/${relsrc#*/}
  mkdir -p ${dst:h}

  [[ -L $dst ]] && rm -f $dst
  ln -s $video $dst
done

rm -f $videoDir/**/^n*(N@^-./)
mkdir $videoDir/$$
rmdir $videoDir/**/*(/^F)
