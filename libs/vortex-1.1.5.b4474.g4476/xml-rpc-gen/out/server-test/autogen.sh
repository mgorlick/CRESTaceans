#!/bin/sh

PACKAGE="Server test implementation XML-RPC component"
(automake --version) < /dev/null > /dev/null 2>&1 || {
	echo;
	echo "You must have automake installed to compile $PACKAGE";
	echo;
	exit;
}

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
	echo;
	echo "You must have autoconf installed to compile $PACKAGE";
	echo;
	exit;
}

echo "Generating configuration files for $PACKAGE, please wait...."
echo;

touch NEWS README AUTHORS ChangeLog
libtoolize --force --copy
aclocal $ACLOCAL_FLAGS;
autoheader;
automake --add-missing --gnu;
autoconf;

./configure $@ --enable-maintainer-mode --enable-compile-warnings
