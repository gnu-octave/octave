#!/bin/sh
#
# doinstall.sh -- install script for binary distributions.
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

# ask octave to tell us the version number
version=`./octave -v 2>/dev/null | awk '/version/ { print $NF }'`

if test -z "$version"
then
  echo "doinstall.sh: unable to extract version number from Octave!"
  exit 1
fi

# ask octave to tell us the target host type
target_host_type=`echo computer | ./octave -q`

if test -z "$target_host_type"
then
  echo "doinstall.sh: unable to extract host type from Octave!"
  exit 1
fi

# ==================== Where To Install Things ====================

# The default location for installation.  Everything is placed in
# subdirectories of this directory.  The default values for many of
# the variables below are expressed in terms of this one, so you may
# not need to change them.  This defaults to /usr/local.
prefix=/usr/local

prefix="/usr/local"
if test $# -eq 1
then
  prefix=$1
else
  if test $# -gt 1
  then
    echo "usage: doinstall.sh [prefix-directory]"
    exit 1
  fi
fi

# Like `prefix', but used for architecture-specific files.
exec_prefix="$prefix"

# Where to install Octave and other binaries that people will want to
# run directly.
bindir="$exec_prefix/bin"

# Where to install architecture-independent data files.  ${fcnfiledir}
# and ${localfcnfiledir} are subdirectories of this.
datadir="$prefix/lib"

# Where to install and expect libraries like libcruft.a, liboctave.a,
# and libreadline.a, executable files to be run by Octave rather than
# directly by users, and other architecture-dependent data.
# ${archlibdir} is a subdirectory of this. 
libdir="$exec_prefix/lib"

# Where to install Octave's include files.  The default is
# ${prefix}/include/octave
includedir="$prefix/include/octave"

# Where to install Octave's man pages, and what extension they should
# have.  The default is ${prefix}/man/man1
mandir="$prefix/man/man1"
manext="1"

# Where to install and expect the info files describing Octave..
infodir="$prefix/info"

# ==================== Octave-specific directories ====================

# These variables hold the values Octave will actually use.  They are
# based on the values of the standard Make variables above.

# Where to install the function file distributed with
# Octave.  This includes the Octave version, so that the
# function files for different versions of Octave will install
# themselves in separate directories.
fcnfiledir="$datadir/octave/$version/m"

# Directories Octave should search for function files specific
# to this site (i.e. customizations), before consulting
# ${fcnfiledir}.  This should be a colon-separated list of
# directories.
localfcnfiledir="$datadir/octave/site/m"
localfcnfilepath="$localfcnfiledir//"

# Where to put executables to be run by Octave rather than
# the user.  This path usually includes the Octave version
# and configuration name, so that multiple configurations
# for multiple versions of Octave may be installed at once.
archlibdir="$libdir/octave/$version/exec/$target_host_type"

# Where to put object files that will by dynamically loaded.
# This path usually includes the Octave version and configuration
# name, so that multiple configurations for multiple versions of
# Octave may be installed at once. 
octfiledir="$libdir/octave/$version/oct/$target_host_type"

# Directories Octave should search for object files that will be
# dynamically loaded and that are specific to this site
# (i.e. customizations), before consulting ${octfiledir}.  This should
# be a colon-separated list of directories.
localoctfiledir="$datadir/octave/site/oct/$target_host_type"
localoctfilepath="$localoctfiledir//"

# Where Octave will search to find its function files.  Before
# changing this, check to see if your purpose wouldn't
# better be served by changing localfcnfilepath.  This
# should be a colon-separated list of directories.
fcnfilepath=".:$localoctfilepath:$localfcnfilepath:$octfiledir//:$fcnfiledir//"

# Where Octave will search to find image files.
imagedir="$datadir/octave/$version/imagelib"
imagepath=".:$imagedir//"

cat << EOF
Installing octave in subdirectories of $prefix.

       Binaries: $bindir
 Function files: $fcnfiledir
    Demo images: $imagedir
     Info files: $infodir
       Man page: $mandir

EOF

DIRS_TO_MAKE="$bindir $datadir $libdir $includedir $mandir $infodir \
  $fcnfiledir $archlibdir $octfiledir $imagedir \
  $localfcnfiledir $localoctfiledir"

./mkinstalldirs $DIRS_TO_MAKE
chmod 755 $DIRS_TO_MAKE

if test "$prefix" = /usr/local
then
  echo "installing ./octave as $bindir/octave"
  cp octave $bindir/octave
  chmod 755 $bindir/octave
else
  echo "installing octave.sh as $bindir/octave"
  sed "s|@OCTAVE_HOME@|$prefix|" octave.sh > octave.tmp
  cp octave.tmp $bindir/octave
  chmod 755 $bindir/octave

  echo "installing ./octave as $bindir/octave.bin"
  cp octave $bindir/octave.bin
  chmod 755 $bindir/octave.bin
fi

echo "installing octtopnm as $archlibdir/octtopnm"
cp octtopnm $archlibdir/octtopnm
chmod 755 $archlibdir/octtopnm

echo "installing octave-bug as $bindir/octave-bug"
cp octave-bug $bindir/octave-bug
chmod 755 $bindir/octave-bug

echo "installing function files in $fcnfiledir"
( cd scripts
  ../mkinstalldirs `find . -type d -print | sed -e 's,^\./,,' -e "s,^,$fcnfiledir/,"`
  for f in `find . \( -name '*.m' -o -name octaverc \) -print`
  do
    cp $f $fcnfiledir/$f
    chmod 644 $fcnfiledir/$f
  done )

echo "installing image files in $imagedir"
( cd scripts
  for f in `find . -name '*.img' -print`
  do
    file=`basename $f`
    cp $f $imagedir/$file
    chmod 644 $imagedir/$file
  done )

echo "installing info files in $infodir"
for f in doc/octave.info*
do
  file=`basename $f`
  cp $f $infodir/$file
  chmod 644 $infodir/$file
done

echo "installing man page in $mandir"
cp doc/octave.1 $mandir/octave.$manext
chmod 644 $mandir/octave.$manext

exit 0
