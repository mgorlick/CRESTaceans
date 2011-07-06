#! /bin/sh

svn checkout http://fastlz.googlecode.com/svn/trunk/ fastlz
cd fastlz
cc -O3 -fPIC -shared -o fastlz.so fastlz.c
mv fastlz.so $RACKET_LIBS
echo "Installed fastlz.so in" $RACKET_LIBS
