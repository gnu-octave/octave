#!/bin/sh
#
# doinstall.sh -- install script for binary distributions.
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

prefix=/usr/local
if test $# -eq 1 ; then
  prefix=$1
else
  if test $# -gt 1 ; then
    echo "usage: doinstall.sh [prefix-directory]"
    exit 1
  fi
fi

# ask octave to tell us the version number
version=`./octave -v 2>/dev/null | sed -e 's/[^0-9.]*\([0-9.]*\).*/\1/' -e q`

if test -z "$version" ; then
  echo "doinstall.sh: unable to extract version number from Octave!"
  exit 1
fi

# where to install binaries.
bindir=$prefix/bin

# where to install M-files
libsubdir=$prefix/lib/octave/$version

# where to install Info files
infodir=$prefix/info

cat << EOF
Installing octave in subdirectories of $prefix:

    Binaries: $bindir
     M-files: $libsubdir
  Info files: $infodir

EOF

for d in $bindir $libsubdir $infodir ; do
  if test -d $d ; then
    true
  else
    echo "making $d"
    ./mkpath $d
  fi
done

if test "$prefix" = /usr/local ; then
  echo "installing ./octave as $bindir/octave"
  cp ./octave $bindir/octave
  chmod 755 $bindir/octave
else
  echo "installing octave.sh as $bindir/octave"
  sed "s|@OCTAVE_HOME@|$prefix|" octave.sh > octave.tmp
  cp octave.tmp $bindir/octave
  chmod 755 $bindir/octave

  echo "installing ./octave as $bindir/octave.bin"
  cp ./octave $bindir/octave.bin
  chmod 755 $bindir/octave.bin
fi

echo "installing M-files in $libsubdir"
for f in scripts/*.m ; do
  file=`basename $f`
  echo $file
  cp $f $libsubdir/$file
  chmod 644 $libsubdir/$file
done

echo "installing info files in $infodir"
for f in doc/octave.info* ; do
  file=`basename $f`
  echo $file
  cp $f $infodir/$file
  chmod 644 $infodir/$file
done

exit 0
