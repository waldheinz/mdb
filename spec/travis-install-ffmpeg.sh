#!/bin/sh

set -xe

if $(ffmpeg -version | grep -q $FFMPEG_VERSION)
then
  echo "FFMpeg already at version ${FFMPEG_VERSION}, good."
else
  echo "Installing FFMpeg ${FFMPEG_VERSION}..."
  export CORES=$(nproc)
  echo "Using $CORES cores for compiling..."

  cd /tmp
  curl -L -O "https://github.com/FFmpeg/FFmpeg/archive/n${FFMPEG_VERSION}.tar.gz"
  tar xvf "n${FFMPEG_VERSION}.tar.gz"
  cd "FFmpeg-n${FFMPEG_VERSION}"
  ./configure --prefix=$HOME/opt
  make -j$CORES
  make install
  $HOME/opt/bin/ffmpeg -version

  cd $TRAVIS_BUILD_DIR;
fi
