#! /bin/sh
# autogen.sh
# Run this to generate all the initial makefiles, etc.

set -e

# Originally copied from the accelerated glx project.

acincludeflags="-I m4"

echo "calling libtoolize..."

(libtoolize --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have libtoolize (part of the libtool package)"
	echo "installed to build Octave.  Download the appropriate"
	echo "package for your distribution, or get the source"
	echo "tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

libtoolize

echo "calling aclocal..."

(aclocal --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have aclocal (part of the automake package)"
	echo "installed to build Octave.  Download the appropriate"
	echo "package for your distribution, or get the source"
	echo "tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

aclocal $acincludeflags

echo "generating source lists for liboctave/Makefile..."

(cd liboctave; ./config-ops.sh)

echo "generating doc/interpreter/images.mk..."

(cd doc/interpreter; ./config-images.sh)

echo "generating src/DLD-FUNCTIONS/module.mk..."

(cd src/DLD-FUNCTIONS; ./config-module.sh)

echo "calling autoheader..."

(autoheader --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have autoheader (part of the autoconf package)"
	echo "installed to build Octave.  Download the appropriate"
	echo "package for your distribution, or get the source"
	echo "tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

autoheader $acincludeflags --force

echo "calling automake..."

(automake --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have automake installed to build Octave."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

automake --warnings=no-portability --add-missing \
  Makefile \
  doc/Makefile \
  doc/faq/Makefile \
  doc/interpreter/Makefile \
  doc/liboctave/Makefile \
  doc/refcard/Makefile \
  examples/Makefile \
  libcruft/Makefile \
  liboctave/Makefile \
  scripts/Makefile \
  src/Makefile \
  test/Makefile

echo "calling autoconf..."

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
	echo
        echo "You must have autoconf installed to build Octave."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
}

autoconf $acincludeflags --force
