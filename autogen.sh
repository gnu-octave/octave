#! /bin/sh
# autogen.sh
# Run this to generate all the initial makefiles, etc.

# copied from the accelerated glx project

echo "calling autoconf and autoheader..."

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have autoconf installed to build Octave."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

(autoheader --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have autoheader installed to build Octave."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

for i in `find . -name configure.in -print`; do (
    dir=`dirname $i`
    cd $dir
    pwd
    if [ -f skip-autoconf ]; then
      echo "skipping autoconf in $dir"
    else
      autoconf --force
    fi
    if [ -f skip-autoheader ]; then
      echo "skipping autoheader in $dir"
    else
      autoheader --force
    fi
); done

echo done
