#! /bin/sh

svn checkout http://fastlz.googlecode.com/svn/trunk/ fastlz
cd fastlz
if [ "$(uname)" = "Darwin" ]; then
	cc -m32 -c -fPIC -O3 -fomit-frame-pointer -o fastlz.o fastlz.c
	cc -m32 -dynamiclib -o fastlz.dylib -dylib fastlz.o
	mv fastlz.dylib $RACKET_LIBS
	echo "Installed fastlz.dylib in" $RACKET_LIBS
else
	cc -fPIC -O3 -fomit-frame-pointer -shared -o fastlz.so fastlz.c
	mv fastlz.so $RACKET_LIBS
	echo "Installed fastlz.so in" $RACKET_LIBS
fi
