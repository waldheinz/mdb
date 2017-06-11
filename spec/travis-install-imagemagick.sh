#!/bin/sh

convert -version | grep $IMAGEMAGICK_VERSION || {
  export CORES=$(nproc) &&
  echo "Using $CORES cores for compiling..." &&
  cd /tmp &&
  curl -O https://storage.googleapis.com/downloads.webmproject.org/releases/webp/libwebp-$LIBWEBP_VERSION.tar.gz &&
  tar xvzf libwebp-$LIBWEBP_VERSION.tar.gz &&
  cd libwebp-* &&
  ./configure --prefix=$HOME/opt &&
  make -j$CORES &&
  make install -j$CORES &&
  cd /tmp &&
  curl -O https://www.imagemagick.org/download/ImageMagick-$IMAGEMAGICK_VERSION.tar.gz &&
  tar xvzf ImageMagick-$IMAGEMAGICK_VERSION.tar.gz &&
  cd ImageMagick-* &&
  ./configure --prefix=$HOME/opt &&
  make -j$CORES &&
  make install -j$CORES &&
  $HOME/opt/bin/convert -version | grep $IMAGEMAGICK_VERSION &&
  cd $TRAVIS_BUILD_DIR; }
