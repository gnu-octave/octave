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
    cd `dirname $i`
    pwd
    autoconf 
    autoheader
); done

echo done

if [ -f cvs.motd ]; then
  echo "ATTENTION CVS Users!"
  echo ""
  cat cvs.motd
  echo ""
fi
