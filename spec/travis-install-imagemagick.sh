#!/bin/sh

convert -version | grep $IMAGEMAGICK_VERSION || {
  export CORES=$(nproc) &&
  echo "Using $CORES cores for compiling..." &&

  cd /tmp &&
  curl -O https://www.imagemagick.org/download/ImageMagick-$IMAGEMAGICK_VERSION.tar.gz &&
  tar xvzf ImageMagick-$IMAGEMAGICK_VERSION.tar.gz &&
  cd ImageMagick-* &&
  ./configure --prefix=$HOME/opt &&
  make -j$CORES &&
  make install -j$CORES &&
  $HOME/opt/bin/convert -version | grep $IMAGEMAGICK_VERSION &&
  cd $TRAVIS_BUILD_DIR; }
