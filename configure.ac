dnl Process this file with autoconf to produce a configure script.
dnl
########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

### Initialize Autoconf
AC_PREREQ([2.65])
AC_INIT([GNU Octave], [8.2.0], [https://octave.org/bugs.html], [octave],
        [https://www.gnu.org/software/octave/])

### Declare version numbers

dnl Note that the version number is duplicated here and in AC_INIT because
dnl AC_INIT requires it to be static, not computed from shell variables.

## The description of the Octave version number in the etc/HACKING.md
## explains how to update these numbers for release and development
## versions.

OCTAVE_MAJOR_VERSION=8
OCTAVE_MINOR_VERSION=2
OCTAVE_PATCH_VERSION=0

dnl PACKAGE_VERSION is set by the AC_INIT VERSION argument.
OCTAVE_VERSION="$PACKAGE_VERSION"

OCTAVE_COPYRIGHT="Copyright (C) 1993-2023 The Octave Project Developers."

OCTAVE_RELEASE_DATE="2023-04-13"

## The "API version" is used as a way of checking that interfaces in the
## liboctave and libinterp libraries haven't changed in a backwardly
## incompatible way when loading .oct files.  A better way to do this is with
## library versioning, but not all systems support it.
##
## NOTE: This macro will be removed in a future version of Octave!
## If you insist on checking for features using a version number, use the
## OCTAVE_MAJOR_VERSION, OCTAVE_MINOR_VERSION, and OCTAVE_PATCH_VERSION
## macros instead.
dnl
dnl FIXME: Since we also set libtool versions for liboctave and libinterp,
dnl perhaps we should be computing the "api version" from those versions numbers
dnl in some way instead of setting it independently here.
OCTAVE_API_VERSION="api-v58"

AC_SUBST(OCTAVE_MAJOR_VERSION)
AC_SUBST(OCTAVE_MINOR_VERSION)
AC_SUBST(OCTAVE_PATCH_VERSION)
AC_SUBST(OCTAVE_VERSION)
AC_SUBST(OCTAVE_COPYRIGHT)
AC_SUBST(OCTAVE_RELEASE_DATE)
AC_SUBST(OCTAVE_API_VERSION)

dnl FIXME: We should auto-insert the Mercurial changeset ID into the
dnl        AC_REVISION field whenever configure.ac is modified.
dnl AC_REVISION($Revision: 1.603 $)
AC_CONFIG_SRCDIR([libinterp/octave.cc])
AC_CONFIG_HEADERS([config.h:config.in.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

### Initialize Automake
AM_INIT_AUTOMAKE([1.14 -Wno-portability -Wno-override tar-ustar subdir-objects dist-lzip dist-xz])

## Add option to enable silent rules and make silent the default behavior.
AM_SILENT_RULES([yes])

OCTAVE_CANONICAL_HOST

AC_DEFINE(OCTAVE_SOURCE, 1, [Define to 1 if this is Octave.])

AC_USE_SYSTEM_EXTENSIONS

### Make configure args available in variable form for other uses.

config_opts=$ac_configure_args
AC_SUBST(config_opts)

### Set default file locations.

## Where to install libraries like liboctave.a, liboctinterp.a, and other
## architecture-dependent binaries.
OCTAVE_SET_DEFAULT([octlibdir], '${libdir}/octave/${version}')

## Where to put executables to be run by Octave, rather than the user.
## This path usually includes the Octave version and configuration name, so
## that configurations for multiple versions of Octave may be installed at once.
## Windows search paths and relocation oddities require all binaries must be
## installed in the same directory.
case $host_os in
  mingw* | msdosmsvc)
    OCTAVE_SET_DEFAULT([archlibdir], '${bindir}') ;;
  *)
    OCTAVE_SET_DEFAULT([archlibdir],
      '${libexecdir}/octave/${version}/exec/${canonical_host_type}') ;;
esac

## Where to put executables to be run by Octave, rather than the user, that
## are specific to this site.
OCTAVE_SET_DEFAULT([localarchlibdir],
  '${libexecdir}/octave/site/exec/${canonical_host_type}')

OCTAVE_SET_DEFAULT([localapiarchlibdir],
  '${libexecdir}/octave/${api_version}/site/exec/${canonical_host_type}')

OCTAVE_SET_DEFAULT([localverarchlibdir],
  '${libexecdir}/octave/${version}/site/exec/${canonical_host_type}')

## Where to put object files that will by dynamically loaded.
## This path includes the Octave version and configuration name, so that
## configurations for multiple versions of Octave may be installed at once.
OCTAVE_SET_DEFAULT([octfiledir],
  '${libdir}/octave/${version}/oct/${canonical_host_type}')

## Directories that Octave should search for object files that will be
## dynamically loaded and that are specific to this site (i.e., customizations),
## before consulting ${octfiledir}.  This should be a colon-separated list of
## directories.
OCTAVE_SET_DEFAULT([localoctfiledir],
  '${libdir}/octave/site/oct/${canonical_host_type}')

OCTAVE_SET_DEFAULT([localapioctfiledir],
  '${libdir}/octave/site/oct/${api_version}/${canonical_host_type}')

OCTAVE_SET_DEFAULT([localveroctfiledir],
  '${libdir}/octave/${version}/site/oct/${canonical_host_type}')

## Where to install Octave's include files.
OCTAVE_SET_DEFAULT([octincludedir], '${includedir}/octave-${version}/octave')

## Where to install the function files distributed with Octave.
## This includes the Octave version, so that the function files for different
## versions of Octave will be installed in separate directories.
OCTAVE_SET_DEFAULT([fcnfiledir], '${datadir}/octave/${version}/m')

## Directories that Octave should search for function files specific to this
## site (i.e., customizations), before consulting ${fcnfiledir}.
## This should be a colon-separated list of directories.
OCTAVE_SET_DEFAULT([localfcnfiledir], '${datadir}/octave/site/m')

OCTAVE_SET_DEFAULT([localapifcnfiledir],
  '${datadir}/octave/site/${api_version}/m')

OCTAVE_SET_DEFAULT([localverfcnfiledir], '${datadir}/octave/${version}/site/m')

## Where to install extra files like NEWS and doc-cache.
OCTAVE_SET_DEFAULT([octetcdir], '${datadir}/octave/${version}/etc')

## Where to install the language files for the GUI.
OCTAVE_SET_DEFAULT([octlocaledir], '${datadir}/octave/${version}/locale')

## The full path to the default doc-cache file.
OCTAVE_SET_DEFAULT([doc_cache_file], '${octetcdir}/doc-cache')

## Where to install test files.
OCTAVE_SET_DEFAULT([octtestsdir], '${octetcdir}/tests')

## The full path to the default texi macros file.
OCTAVE_SET_DEFAULT([texi_macros_file], '${octetcdir}/macros.texi')

## Where Octave will search to find image files.
OCTAVE_SET_DEFAULT([imagedir], '${datadir}/octave/${version}/imagelib')

## Where Octave will search for example data files shipped with distribution.
OCTAVE_SET_DEFAULT([octdatadir], '${datadir}/octave/${version}/data')

## Where Octave will search for Qt help files shipped with distribution.
OCTAVE_SET_DEFAULT([octdocdir], '${datadir}/octave/${version}/doc')

## Where Octave will search for fallback font files shipped with distribution.
OCTAVE_SET_DEFAULT([octfontsdir], '${datadir}/octave/${version}/fonts')

## Where Octave will look for startup files.
OCTAVE_SET_DEFAULT([startupfiledir], '${fcnfiledir}/startup')
OCTAVE_SET_DEFAULT([localstartupfiledir], '${localfcnfiledir}/startup')

## Where Octave will look for man and info files.
OCTAVE_SET_DEFAULT([man1dir], '${mandir}/man1')
OCTAVE_SET_DEFAULT([man1ext], '.1')
OCTAVE_SET_DEFAULT([infofile], '${infodir}/octave.info')

### Check for programs used in building, installing, and running Octave.

## Programs used in configuring Octave.
dnl Find pkg-config executable (sets $PKG_CONFIG)
PKG_PROG_PKG_CONFIG
## And where we will install our own .pc files.
PKG_INSTALLDIR
AC_SUBST([liboctave_pkgconfigdir], [$pkgconfigdir])
AC_SUBST([libinterp_pkgconfigdir], [$pkgconfigdir])

## Programs used in Makefiles.
AC_PROG_AWK
AC_PROG_GREP
AC_PROG_EGREP
OCTAVE_PROG_FIND
OCTAVE_PROG_SED
OCTAVE_PROG_PERL

## Programs used to build parts of Octave.
OCTAVE_PROG_GPERF

OCTAVE_PROG_FLEX
AC_SUBST([LEX_OUTPUT_ROOT], [lex.octave_])

OCTAVE_PROG_BISON

OCTAVE_PROG_MAKEINFO
OCTAVE_PROG_TEXI2DVI
OCTAVE_PROG_TEXI2PDF

OCTAVE_PROG_TAR_REPRODUCIBLE

## Programs used when installing Octave.
AC_PROG_LN_S
AC_PROG_MKDIR_P

AC_PROG_INSTALL
INSTALL_SCRIPT="${INSTALL}"
AC_SUBST(INSTALL_SCRIPT)

## Programs used when running Octave.
OCTAVE_PROG_GHOSTSCRIPT
OCTAVE_PROG_GNUPLOT
OCTAVE_PROG_PAGER
OCTAVE_PROG_PYTHON

## Programs used to generate icons file formats.
OCTAVE_PROG_ICOTOOL
OCTAVE_PROG_RSVG_CONVERT
AM_CONDITIONAL([AMCOND_HAVE_ICON_TOOLS],
  [test -n "$ICOTOOL" && test -n "$RSVG_CONVERT"])

### Determine path separator.

sepchar=':'
AC_ARG_WITH([sepchar],
  [AS_HELP_STRING([--with-sepchar=<char>],
    [use <char> as the path separation character])])
case $with_sepchar in
  yes | "")
    case $host_os in
      mingw* | msdosmsvc)
        sepchar=';' ;;
    esac
  ;;
  no)
    AC_MSG_ERROR([You are required to define a path separation character])
    ;;
  *)
    sepchar=$with_sepchar
  ;;
esac
AC_SUBST(sepchar)
AC_DEFINE_UNQUOTED(SEPCHAR, ['$sepchar'],
  [Define this to be the path separator for your system, as a character constant.])
AC_DEFINE_UNQUOTED(SEPCHAR_STR, ["$sepchar"],
  [Define this to be the path separator for your system, as a string.])

### Define the path to the shell on the host system.
dnl Most systems will ensure /bin/sh is the default shell so this can be safely
dnl ignored by almost everyone.  However, there are exceptions, such as
dnl Android, where this will need to be set.
SHELL_PATH=/bin/sh
AC_ARG_WITH([shell],
  [AS_HELP_STRING([--with-shell=SHELL],
    [use SHELL as the shell interpreter (default: /bin/sh)])])
case $with_shell in
  no)
    AC_MSG_ERROR([A shell interpreter is required])
  ;;
  yes | "")
  ;;
  *)
    SHELL_PATH=$with_shell
  ;;
esac
AC_DEFINE_UNQUOTED([SHELL_PATH], ["$SHELL_PATH"],
  [Define this to be the path to the shell command interpreter.])

### Determine which C++ compiler to use (we expect to find g++).

AC_PROG_CXX
AC_PROG_CXXCPP

ENABLE_STD_PMR_POLYMORPHIC_ALLOCATOR=no
AC_ARG_ENABLE(std-pmr-polymorphic-allocator,
  [AS_HELP_STRING([--enable-std-pmr-polymorphic-allocator],
    [build libraries requiring std::pmr::polymorphic_allocator if possible.  This allows more efficient transfer of arrays at the .mex file interface.  That also means that all libraries including Octave headers (e.g., .oct files) must be able to compile with that C++17 feature.])],
  [case $enableval in
     yes) ENABLE_STD_PMR_POLYMORPHIC_ALLOCATOR=yes ;;
     no) ENABLE_STD_PMR_POLYMORPHIC_ALLOCATOR=no ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-std-pmr-polymorphic-allocator]) ;;
   esac])

### Determine compiler capabilities.

if test $ENABLE_STD_PMR_POLYMORPHIC_ALLOCATOR = yes; then
  ## Test for flags to fully support C++17.
  ## Preferably with GNU extensions if flags are required.
  AX_CXX_COMPILE_STDCXX(17, [], [])
else
  HAVE_CXX17=0
fi
if test "$HAVE_CXX17" = 0; then
  ## Ensure that the C++ compiler fully supports C++11.
  ## Preferably with GNU extensions if flags are required.
  AX_CXX_COMPILE_STDCXX(11, [], mandatory)
fi

if test $ENABLE_STD_PMR_POLYMORPHIC_ALLOCATOR = yes; then
  OCTAVE_CHECK_STD_PMR_POLYMORPHIC_ALLOCATOR
fi

### Determine which C compiler to use (we expect to find gcc).

AC_PROG_CC
AC_PROG_CPP
AC_PROG_GCC_TRADITIONAL

## Save and restore CFLAGS and CXXFLAGS globally.
dnl These variables are for users, so we shouldn't be touching them.
dnl Instead, we should set the corresponding AM_ flags (currently by way of the
dnl XTRA_ variables).  However, for the duration of the configure script, we
dnl may set CFLAGS and CXXFLAGS so that subsequent tests succeed.  Temporary
dnl settings like this are currently done for pthreads and openmp, for example.
original_octave_configure_CFLAGS="$CFLAGS"
original_octave_configure_CXXFLAGS="$CXXFLAGS"

## Check for MSVC
have_msvc=no
case $host_os in
  msdosmsvc)
    have_msvc=yes
  ;;
  mingw*)
    AC_MSG_CHECKING([for MSVC compiler])
    AC_PREPROC_IFELSE([AC_LANG_SOURCE([[
        #if ! defined (_MSC_VER)
        #error "Not MSVC compiler"
        #endif
        ]])],
      have_msvc=yes, have_msvc=no)
    AC_MSG_RESULT([$have_msvc])
  ;;
esac

### gnulib initialization: part 1
dnl Must take place immediately after a compiler is determined

gl_EARLY

### Check version number when using g++.

GXX_VERSION=
if test "$GXX" = yes; then
  AC_CACHE_CHECK([g++ compiler version number],
    [octave_cv_gxx_version],
    [octave_cv_gxx_version=[`$CXX -v 2>&1 | $GREP "^.*g.. version" | $SED -e 's/^.*g.. version *//' -e 's/cygnus-//' -e 's/egcs-//' -e 's/ .*//'`]
  ])

  AX_COMPARE_VERSION([$octave_cv_gxx_version], [lt], [4.1],
    [warn_gxx_version="g++ version $octave_cv_gxx_version is likely to cause problems"
     OCTAVE_CONFIGURE_WARNING([warn_gxx_version])])

  GXX_VERSION=$octave_cv_gxx_version
fi
AC_SUBST(GXX_VERSION)

## Workaround for broken STL algorithm library.
OCTAVE_CHECK_BROKEN_STL_ALGO_H
AM_CONDITIONAL([AMCOND_HAVE_BROKEN_STL_ALGO_H],
  [test $octave_cv_broken_stl_algo_h = yes])

if test $octave_cv_broken_stl_algo_h = yes; then
  warn_stl_algo_h="Found nth_element broken in g++ $GXX_VERSION.  Attempting to repair by using local patched version of bits/stl_algo.h."
  OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
fi

### Check version number when using gcc.
dnl It might be different from the g++ version number.

GCC_VERSION=
if test "$GCC" = yes; then
  AC_CACHE_CHECK([gcc compiler version number],
    [octave_cv_gcc_version],
    [octave_cv_gcc_version=[`$CC -v 2>&1 | $GREP "^.*gcc version" | $SED -e 's/^.*g.. version *//' -e 's/cygnus-//' -e 's/egcs-//' -e 's/ .*//'`]
  ])

  AX_COMPARE_VERSION([$octave_cv_gcc_version], [lt], [3],
    [warn_gcc_version="gcc version $octave_cv_gcc_version is likely to cause problems"
     OCTAVE_CONFIGURE_WARNING([warn_gcc_version])])

  GCC_VERSION=$octave_cv_gcc_version
fi
AC_SUBST(GCC_VERSION)

## It seems that there are some broken inline assembly functions in GNU libc.
## Since it is uncertain how to test whether Octave is linked against GNU libc,
## just disable them for all platforms.

AC_MSG_NOTICE([defining __NO_MATH_INLINES avoids buggy GNU libc exp function])
AC_DEFINE(__NO_MATH_INLINES, 1,
  [Define to 1 if your version of GNU libc has buggy inline assembly code for math functions like exp.])

### Determine the compiler flag necessary to create dependencies.

## Assume GCC.
INCLUDE_DEPS=yes
DEPEND_FLAGS="-M"
DEPEND_EXTRA_SED_PATTERN=""
if test "$GCC" != yes; then
  case $canonical_host_type in
    sparc-sun-solaris2* | i386-pc-solaris2*)
      DEPEND_FLAGS="-xM1"
      DEPEND_EXTRA_SED_PATTERN="-e '/\/opt\/SUNWspro/d'"
    ;;
    *-*-msdosmsvc)
    ;;
    *-*-mingw*)
      if test $have_msvc = no; then
        INCLUDE_DEPS=no
      fi
    ;;
    *)
      INCLUDE_DEPS=no
    ;;
  esac
fi
AC_SUBST(INCLUDE_DEPS)
AC_SUBST(DEPEND_FLAGS)
AC_SUBST(DEPEND_EXTRA_SED_PATTERN)

### Check for math library.
dnl If found, this will add -lm to LIBS.

dnl Keep this check before the check for the Fortran compiler,
dnl in case -lm is needed to compile Fortran programs.
AC_CHECK_LIB(m, sin)

### Determine the Fortran compiler and how to invoke it.

## Prefer gfortran, but the user's F77 environment variable will override.
AC_PROG_F77([gfortran])
if test -z "$F77"; then
  ## No gfortran found, search for any other installed compiler.
  AC_PROG_F77
fi
if test "$F77" = g77; then
  AC_MSG_ERROR([g77 is not a supported Fortran compiler.  Select another compiler by setting the environment variable F77 and re-running configure.])
fi

AC_MSG_CHECKING([whether a usable Fortran compiler was found])
if test -n "$F77"; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([a Fortran compiler is required to build Octave])
fi

AC_F77_LIBRARY_LDFLAGS
AC_F77_DUMMY_MAIN
AC_F77_WRAPPERS

## Must appear after checks for Fortran compiler.
OCTAVE_CHECK_FORTRAN_SYMBOL_AND_CALLING_CONVENTIONS

## Must appear after checks for all compilers.
OCTAVE_DEFINE_MKOCTFILE_DYNAMIC_LINK_OPTIONS

### Use a 64-bit integer type for array dimensions and indexing, if possible.

AC_CHECK_SIZEOF([int])
AC_DEFINE_UNQUOTED(OCTAVE_SIZEOF_INT, [$ac_cv_sizeof_int],
  [Define to the size of int.])

AC_CHECK_SIZEOF([void *])
if test $ac_cv_sizeof_void_p -ge 8; then
  ENABLE_64=yes
else
  ENABLE_64=no
fi

AC_ARG_ENABLE(64,
  [AS_HELP_STRING([--disable-64],
    [don't use 64-bit integers for array dimensions and indexing])],
  [case $enableval in
     yes) ENABLE_64=yes ;;
     no) ENABLE_64=no ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-64]) ;;
   esac])

if test $ENABLE_64 = yes; then
  if test $ac_cv_sizeof_void_p -ge 8; then
    OCTAVE_IDX_TYPE=int64_t
  else
    warn_64_bit="--enable-64 option given but pointers are less than 64-bits wide; disabling 64-bit indexing"
    OCTAVE_CONFIGURE_WARNING([warn_64_bit])
    OCTAVE_IDX_TYPE=int32_t
    ENABLE_64=no
  fi
else
  OCTAVE_IDX_TYPE=int32_t
fi

AC_SUBST(ENABLE_64)
if test $ENABLE_64 = yes; then
  octave_sizeof_octave_idx_type=8
  AC_DEFINE(OCTAVE_ENABLE_64, 1,
    [Define to 1 to use 64-bit integers for array dimensions and indexing.])
else
  octave_sizeof_octave_idx_type=4
fi

AC_DEFINE_UNQUOTED(OCTAVE_SIZEOF_IDX_TYPE, [$octave_sizeof_octave_idx_type],
  [Define to the size of the octave_idx_type (8 or 4).])

AC_SUBST(OCTAVE_IDX_TYPE)
AC_DEFINE_UNQUOTED(OCTAVE_IDX_TYPE, [$OCTAVE_IDX_TYPE],
  [Define to the type of octave_idx_type (64 or 32 bit signed integer).])

### Check for LLVM or Apple libc++ library.

OCTAVE_LLVM_LIBCXX

### Check for pthread library.

AX_PTHREAD
dnl Include pthread libs and flags early in case other config tests need them.
dnl They seem to be required for the OpenGL tests on Debian systems.
LIBS="$PTHREAD_LIBS $LIBS"
XTRA_CFLAGS="$XTRA_CFLAGS $PTHREAD_CFLAGS"
XTRA_CXXFLAGS="$XTRA_CXXFLAGS $PTHREAD_CFLAGS"
dnl Set these for any other tests that may require them.
dnl They will be reset before output files are generated.
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"

dnl Check if glibc uses wrong stack size
OCTAVE_CHECK_BROKEN_PTHREAD_STACKSIZE
AM_CONDITIONAL([OCTAVE_CHECK_BROKEN_PTHREAD_STACKSIZE],
  [test $octave_cv_broken_pthread_stacksize = yes])

### Test whether the compiler supports OpenMP.
dnl This is enabled by default to allow the option of using OpenMP in
dnl loadable modules.

ENABLE_OPENMP=no
check_for_openmp=yes
AC_ARG_ENABLE([openmp],
  [AS_HELP_STRING([--disable-openmp],
    [disable OpenMP SMP multi-threading])],
  [if test "$enableval" = no; then check_for_openmp=no; fi], [])
if test $check_for_openmp = yes; then
  AC_LANG_PUSH(C)
  AX_OPENMP([XTRA_CFLAGS="$XTRA_CFLAGS $OPENMP_CFLAGS"; ENABLE_OPENMP=yes], [])
  AC_LANG_POP(C)
  AC_LANG_PUSH(C++)
  AX_OPENMP([XTRA_CXXFLAGS="$XTRA_CXXFLAGS $OPENMP_CXXFLAGS"; ENABLE_OPENMP=yes], [])
  AC_LANG_POP(C++)
fi

dnl Set these for any other tests that may require them.
dnl They will be reset before output files are generated.
CFLAGS="$CFLAGS $OPENMP_CFLAGS"
CXXFLAGS="$CXXFLAGS $OPENMP_CXXFLAGS"

dnl Define here since it is skipped if the first argument to AX_OPENMP
dnl is not empty.
if test $ENABLE_OPENMP = yes; then
  AC_CHECK_HEADERS([omp.h])
  AC_CHECK_FUNCS([omp_get_num_threads])
  AC_DEFINE(OCTAVE_ENABLE_OPENMP, 1, [Define to 1 if OpenMP is enabled])
fi

### Start determination of shared vs. static libraries.

## Use -static if compiling on Alpha OSF/1 1.3 systems.
case $canonical_host_type in
  alpha*-dec-osf1.3)
    LD_STATIC_FLAG=-static
  ;;
esac
if test -n "$LD_STATIC_FLAG"; then
  AC_MSG_NOTICE([defining LD_STATIC_FLAG to be $LD_STATIC_FLAG])
fi
AC_SUBST(LD_STATIC_FLAG)

## Check for programs necessary for shared libraries (ar, libtool)
OCTAVE_PROG_AR

ifdef([LT_INIT], [], [
  errprint([error: you must have libtool 2.2.2 or a more recent version
])
  m4exit([1])])

LT_PREREQ([2.2.2])
LT_INIT([disable-static dlopen win32-dll])

if test $enable_shared = yes; then
  SHARED_LIBS=yes
else
  SHARED_LIBS=no
fi
AC_SUBST(SHARED_LIBS)

if test $enable_static = yes; then
  STATIC_LIBS=yes
else
  STATIC_LIBS=no
fi
AC_SUBST(STATIC_LIBS)

XTRA_EXTERNAL_SH_LDFLAGS=
case $host_os in
  msdosmsvc | mingw* | cygwin*)
    FLIBS="$FLIBS -lkernel32"
    XTRA_EXTERNAL_SH_LDFLAGS="-Wl,liboctave/external/external.def"
  ;;
esac
AC_SUBST(XTRA_EXTERNAL_SH_LDFLAGS)

if test $STATIC_LIBS = no && test $SHARED_LIBS = no; then
  AC_MSG_ERROR([You can't disable building both static AND shared libraries!])
fi

### More configure argument checking related to linking

case $host_os in
  msdosmsvc | mingw* | cygwin* | AIX | BeOS | OS/2 )
    DEFAULT_NO_UNDEFINED_LDFLAG="-no-undefined"
  ;;
  *)
    DEFAULT_NO_UNDEFINED_LDFLAG=""
  ;;
esac
AC_ARG_ENABLE([no-undefined],
  [AS_HELP_STRING([--enable-no-undefined],
    [pass -no-undefined to libtool when linking Octave and its shared libraries.  This is done by default for msdosmsvc, mingw*, cygwin*, AIX, BeOS, or OS/2 hosts.])],
  [case $enableval in
     yes) NO_UNDEFINED_LDFLAG="-no-undefined" ;;
     no)  NO_UNDEFINED_LDFLAG="" ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-no-undefined]) ;;
   esac],
  [NO_UNDEFINED_LDFLAG="$DEFAULT_NO_UNDEFINED_LDFLAG"])
AC_SUBST(NO_UNDEFINED_LDFLAG)

case $host_os in
  cygwin* )
    DEFAULT_LINK_ALL_DEPS="yes"
  ;;
  *)
    DEFAULT_LINK_ALL_DEPS="no"
  ;;
esac
AC_ARG_ENABLE([link-all-dependencies],
  [AS_HELP_STRING([--enable-link-all-dependencies],
    [link Octave and its shared libraries with all dependencies, not just those immediately referenced (should not be needed on most systems).  This is done by default for cygwin* or darwin* hosts.])],
  [case $enableval in
     yes) link_all_deps=yes ;;
     no)  link_all_deps=no ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-link-all-dependencies])
     ;;
   esac],
  [link_all_deps="$DEFAULT_LINK_ALL_DEPS"])
AM_CONDITIONAL([AMCOND_LINK_ALL_DEPS], [test $link_all_deps = yes])

### Check for BLAS and LAPACK libraries.

## Need to adjust FFLAGS to include correct integer size.
save_FFLAGS="$FFLAGS"
FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"

OCTAVE_BLAS
AX_LAPACK

## Restore FFLAGS.
FFLAGS="$save_FFLAGS"

## If necessary, try again with -ff2c in FFLAGS
if test $ax_blas_ok = no; then
  save_FFLAGS="$FFLAGS"
  FFLAGS="-ff2c $FFLAGS $F77_INTEGER_8_FLAG"

  OCTAVE_BLAS
  AX_LAPACK

  ## Restore FFLAGS, with -ff2c if that was helpful
  if test $ax_blas_ok = yes; then
    FFLAGS="-ff2c $save_FFLAGS"
  else
    FFLAGS="$save_FFLAGS"
  fi
fi

## On OSX, try again with a wrapper library (without -ff2c!)
if test $ax_blas_ok = no; then
  case $host_os in
    darwin*)
      ## test if wrapper functions help
      octave_blaswrap_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -DUSE_BLASWRAP"
      AC_LANG_PUSH(C)
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
          #include "liboctave/util/blaswrap.c"
        ]])],
        [mv conftest.$ac_objext blaswrap.$ac_objext
         octave_blaswrap_save_BLAS_LIBS="$BLAS_LIBS"
         BLAS_LIBS="blaswrap.$ac_objext -framework vecLib"

         save_FFLAGS="$FFLAGS"
         FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"

         OCTAVE_BLAS
         AX_LAPACK

         ## Restore FFLAGS.
         FFLAGS="$save_FFLAGS"

         dnl remove temp file
         rm -f blaswrap.$ac_objext],
        [AC_MSG_FAILURE([cannot compile liboctave/util/blaswrap.c])])
      AC_LANG_POP(C)
      CFLAGS="$octave_blaswrap_save_CFLAGS"

      if test $ax_blas_ok = no; then
        BLAS_LIBS="$octave_blaswrap_save_BLAS_LIBS"
      else
        ## wrapper in liboctave/util, remove from BLAS_LIBS
        BLAS_LIBS=`echo $BLAS_LIBS | $SED -e 's/blaswrap.[[^ ]]* //g'`
        AC_DEFINE(USE_BLASWRAP, 1,
          [Define to 1 if BLAS functions need to be wrapped (potentially needed for 64-bit OSX only).])
      fi
    ;;
  esac
fi

if test $ax_blas_ok = no || test $ax_lapack_ok = no; then
  AC_MSG_ERROR([BLAS and LAPACK libraries are required])
fi

case $ax_cv_blas_integer_size in
  4)
    HAVE_64_BIT_BLAS=no
  ;;
  8)
    HAVE_64_BIT_BLAS=yes
  ;;
  *)
    AC_MSG_ERROR([unrecognized BLAS library integer size])
  ;;
esac

OCTAVE_CHECK_SIZEOF_FORTRAN_INTEGER
if test $octave_cv_sizeof_fortran_integer -ne $ax_cv_blas_integer_size; then
  if test $ax_cv_blas_integer_size -eq 8; then
    case $F77 in
      *gfortran*)
        case $F77_INTEGER_8_FLAG in
          *-fdefault-integer-8*)
          ;;
          *)
            case $FFLAGS in
              *-fdefault-integer-8*)
                AC_MSG_NOTICE([setting -fdefault-integer-8 in F77_INTEGER_8_FLAG instead of FFLAGS])
                FFLAGS=`echo $FFLAGS | $SED 's/-fdefault-integer-8//g'`
                F77_INTEGER_8_FLAG="-fdefault-integer-8"
              ;;
              *)
                AC_MSG_NOTICE([adding -fdefault-integer-8 to F77_INTEGER_8_FLAG])
                F77_INTEGER_8_FLAG="-fdefault-integer-8"
                ## Invalidate the cache and try again.
                $as_unset octave_cv_sizeof_fortran_integer
              ;;
            esac
          ;;
        esac
      ;;
    esac
    if test -z "$octave_cv_sizeof_fortran_integer"; then
      OCTAVE_CHECK_SIZEOF_FORTRAN_INTEGER
    fi
  fi
  ## We intentionally don't attempt to fix things up if the default
  ## Fortran integer size is 8 but BLAS appears to use 4-byte integers.
  if test $octave_cv_sizeof_fortran_integer -ne $ax_cv_blas_integer_size; then
    AC_MSG_ERROR([your Fortran compiler must have an option for setting the default integer size to be the same size as your BLAS library uses ($ax_cv_blas_integer_size bytes).  See the file INSTALL for more information.])
  fi
fi
AC_SUBST(F77_INTEGER_8_FLAG)

case $octave_cv_sizeof_fortran_integer in
  8)
    OCTAVE_F77_INT_TYPE=int64_t
  ;;
  4)
    OCTAVE_F77_INT_TYPE=int32_t
  ;;
esac
AC_SUBST(OCTAVE_F77_INT_TYPE)
AC_DEFINE_UNQUOTED(OCTAVE_F77_INT_TYPE, [$OCTAVE_F77_INT_TYPE],
  [Define to the type of octave_f77_int_type (64 or 32 bit signed integer).])
AC_DEFINE_UNQUOTED(OCTAVE_SIZEOF_F77_INT_TYPE,
  [$octave_cv_sizeof_fortran_integer],
  [Define to the size of the octave_f77_int_type (8 or 4).])

OCTAVE_F77_FLAG([-ffloat-store], [
  AC_MSG_RESULT([setting F77_FLOAT_STORE_FLAG to -ffloat-store])
  F77_FLOAT_STORE_FLAG=-ffloat-store
  AC_SUBST(F77_FLOAT_STORE_FLAG)
])

if test $ac_cv_f77_compiler_gnu = yes; then
  OCTAVE_F77_FLAG([-std=legacy])
fi

BUILD_EXTERNAL_LIBXERBLA=
case $host_os in
  msdosmsvc | mingw*)
    BUILD_EXTERNAL_LIBXERBLA=ues
  ;;
esac

AM_CONDITIONAL([AMCOND_BUILD_EXTERNAL_LIBXERBLA],
  [test -n "$BUILD_EXTERNAL_LIBXERBLA"])

### Set options for dynamic linking if enabled.
dnl Options set only only if we are building shared libs and some API for
dnl dynamic linking has been detected.

dnl FIXME: A lot of the following duplicates the functionality of
dnl code generated by the dlopen option for LT_INIT.

RDYNAMIC_FLAG=
DL_API_MSG=""
dlopen_api=no
loadlibrary_api=no

case $lt_cv_dlopen in
  dlopen)
    dlopen_api=yes
    DL_API_MSG="dlopen"
    AC_DEFINE(HAVE_DLOPEN_API, 1,
      [Define to 1 if system has dlopen, dlsym, dlerror, and dlclose for dynamic linking.])
    OCTAVE_CXX_FLAG([-rdynamic], [RDYNAMIC_FLAG=-rdynamic])
  ;;
  LoadLibrary)
    loadlibrary_api=yes
    DL_API_MSG="LoadLibrary"
    AC_DEFINE(HAVE_LOADLIBRARY_API, 1,
      [Define to 1 if system has LoadLibrary for dynamic linking.])
  ;;
  *)
    AC_MSG_ERROR([Octave requires some way to perform dynamic linking.])
  ;;
esac

DL_LIBS="$lt_cv_dlopen_libs"

if test $SHARED_LIBS = yes; then
  LIBOCTINTERP="-loctinterp"
  LIBOCTAVE="-loctave"
else
  LIBOCTINTERP="${top_builddir}/libinterp/liboctinterp.a"
  LIBOCTAVE="${top_builddir}/liboctave/liboctave.a"
fi

AC_SUBST(RDYNAMIC_FLAG)
AC_SUBST(LIBOCTINTERP)
AC_SUBST(LIBOCTAVE)

if test "$cross_compiling" = yes && test -n "$ac_tool_prefix"; then
  CROSS_TOOL_PREFIX="$ac_tool_prefix"
  MKOCTFILE_AR=`echo "$AR" | $SED "s,$CROSS_TOOL_PREFIX,,"`
  MKOCTFILE_CC=`echo "$CC" | $SED "s,$CROSS_TOOL_PREFIX,,"`
  MKOCTFILE_CXX=`echo "$CXX" | $SED "s,$CROSS_TOOL_PREFIX,,"`
  MKOCTFILE_F77=`echo "$F77" | $SED "s,$CROSS_TOOL_PREFIX,,"`
  MKOCTFILE_RANLIB=`echo "$RANLIB" | $SED "s,$CROSS_TOOL_PREFIX,,"`
else
  MKOCTFILE_AR="$AR"
  MKOCTFILE_CC="$CC"
  MKOCTFILE_CXX="$CXX"
  MKOCTFILE_F77="$F77"
  MKOCTFILE_RANLIB="$RANLIB"
fi
AC_MSG_NOTICE([defining CROSS_TOOL_PREFIX to be $CROSS_TOOL_PREFIX])
AC_MSG_NOTICE([defining MKOCTFILE_AR to be $MKOCTFILE_AR])
AC_MSG_NOTICE([defining MKOCTFILE_CC to be $MKOCTFILE_CC])
AC_MSG_NOTICE([defining MKOCTFILE_CXX to be $MKOCTFILE_CXX])
AC_MSG_NOTICE([defining MKOCTFILE_F77 to be $MKOCTFILE_F77])
AC_MSG_NOTICE([defining MKOCTFILE_RANLIB to be $MKOCTFILE_RANLIB])
AC_SUBST(CROSS_TOOL_PREFIX)
AC_SUBST(MKOCTFILE_AR)
AC_SUBST(MKOCTFILE_CC)
AC_SUBST(MKOCTFILE_CXX)
AC_SUBST(MKOCTFILE_F77)
AC_SUBST(MKOCTFILE_RANLIB)

### Enable float truncation to work around issues with x87 co-processors

dnl When compiling math for x87, problems may arise in some code comparing
dnl floating-point intermediate results.  The root cause is the extra precision
dnl (~80 bits) of x87 co-processor registers versus the IEEE standard 64 bits.
dnl Generally, storing the result in a local volatile variable forces a
dnl truncation back to 64 bits, but it also degrades performance.
dnl However, this performance degradation is very minimal, if indeed measurable.
dnl Therefore, it has been enabled for all platforms and compilers.
dnl Reported bugs indicate that --enable-float-truncate is required for MinGW
dnl and Cygwin platforms and for GCC compilers >= 5.0.  It should not be
dnl necessary for non-x87 targets or when using modern SSE math.
ENABLE_FLOAT_TRUNCATE=yes
AC_ARG_ENABLE([float-truncate],
  [AS_HELP_STRING([--disable-float-truncate],
    [truncate intermediate FP results])],
  [if test "$enableval" = no; then ENABLE_FLOAT_TRUNCATE=no; fi], [])
if test $ENABLE_FLOAT_TRUNCATE = yes; then
  AC_DEFINE(OCTAVE_ENABLE_FLOAT_TRUNCATE, 1,
    [Define to 1 to truncate intermediate FP results.])
fi

### Determine extra CFLAGS, CXXFLAGS that may be necessary for Octave.

## On Intel systems with gcc, we need to compile with -mieee-fp to get full
## support for IEEE floating point.
##
## On Alpha/OSF systems, we require -mieee or -ieee for full support.

ieee_fp_flag=
case $canonical_host_type in
  i[[3456789]]86-*-*)
    if test "$GCC" = yes; then
      OCTAVE_CC_FLAG([-mieee-fp], [
        ieee_fp_flag=-mieee-fp
        XTRA_CFLAGS="$XTRA_CFLAGS -mieee-fp"
        AC_MSG_NOTICE([adding -mieee-fp to XTRA_CFLAGS])])
    fi
    if test "$GXX" = yes; then
      OCTAVE_CXX_FLAG([-mieee-fp], [
        ieee_fp_flag=-mieee-fp
        XTRA_CXXFLAGS="$XTRA_CXXFLAGS -mieee-fp"
        AC_MSG_NOTICE([adding -mieee-fp to XTRA_CXXFLAGS])])
    fi
  ;;
  alpha*-*-*)
    if test "$GCC" = yes; then
      OCTAVE_CC_FLAG([-mieee], [
        ieee_fp_flag=-mieee
        XTRA_CFLAGS="$XTRA_CFLAGS -mieee"
        AC_MSG_NOTICE([adding -mieee to XTRA_CFLAGS])])
    else
      OCTAVE_CC_FLAG([-ieee], [
        ieee_fp_flag=-ieee
        XTRA_CFLAGS="$XTRA_CFLAGS -ieee"
        AC_MSG_NOTICE([adding -ieee to XTRA_CFLAGS])])
    fi
    if test "$GXX" = yes; then
      OCTAVE_CXX_FLAG([-mieee], [
        ieee_fp_flag=-mieee
        XTRA_CXXFLAGS="$XTRA_CXXFLAGS -mieee"
        AC_MSG_NOTICE([adding -mieee to XTRA_CXXFLAGS])])
    else
      OCTAVE_CXX_FLAG([-ieee], [
        ieee_fp_flag=-ieee
        XTRA_CXXFLAGS="$XTRA_CXXFLAGS -ieee"
        AC_MSG_NOTICE([adding -ieee to XTRA_CXXFLAGS])])
    fi
  ;;
  *ibm-aix4*)
    OCTAVE_CC_FLAG([-mminimal-toc], [
      XTRA_CFLAGS="$XTRA_CFLAGS -mminimal-toc"])

    OCTAVE_CXX_FLAG([-mminimal-toc], [
      XTRA_CXXFLAGS="$XTRA_CXXFLAGS -mminimal-toc"])
  ;;
esac

AC_SUBST(XTRA_CFLAGS)
AC_SUBST(XTRA_CXXFLAGS)

### Defaults for cross compiling.
dnl BUILD_CC and BUILD_CXX are the compilers that we use for building tools
dnl on the build system.  For now, we assume that the only cross compiling we
dnl can do is with gcc on a Unixy system, but the dedicated hacker can
dnl override these.

if test "$cross_compiling" = yes; then
  BUILD_CC="gcc"
  BUILD_CFLAGS="-O2 -g"
  BUILD_CXX="g++"
  BUILD_CXXFLAGS="-O2 -g -std=c++11"
  BUILD_LDFLAGS=""
  BUILD_EXEEXT=""
else
  BUILD_CC="${CC}"
  BUILD_CFLAGS="${CFLAGS}"
  BUILD_CXX="${CXX}"
  BUILD_CXXFLAGS="${CXXFLAGS}"
  BUILD_LDFLAGS="${LDFLAGS}"
  BUILD_EXEEXT="${EXEEXT}"
fi

AC_ARG_VAR([BUILD_CC],
  [build system C compiler (used if cross compiling)])
AC_ARG_VAR([BUILD_CFLAGS],
  [build system C compiler flags (used if cross compiling)])
AC_ARG_VAR([BUILD_CXX],
  [build system C++ compiler (used if cross compiling)])
AC_ARG_VAR([BUILD_CXXFLAGS],
  [build system C++ compiler flags (used if cross compiling)])
AC_ARG_VAR([BUILD_LDFLAGS],
  [build system C++ compiler link flags (used if cross compiling)])
AC_ARG_VAR([BUILD_EXEEXT],
  [build system executable extension (used if cross compiling)])

### Determine whether to create cross-mkoctfile and other cross-tools.

cross_tools=no
AC_ARG_ENABLE([cross-tools],
  [AS_HELP_STRING([--enable-cross-tools],
    [build cross tools (mkoctfile, octave-config) if cross compiling])],
  [if test "$enableval" = yes; then cross_tools=yes; fi])

if test "$cross_tools" = yes; then
  if test "$cross_compiling" = no; then
    AC_MSG_WARN([ignoring --enable-cross-tools when not cross compiling])
    cross_tools=no
  fi
fi
AM_CONDITIONAL([AMCOND_CROSS_TOOLS], [test $cross_tools = yes])

### Check compiler characteristics.
dnl FIXME: How many of these are necessary now that C++11 is required?

## Check if C++ compiler can auto allocate variable sized arrays.
OCTAVE_CXX_DYNAMIC_AUTO_ARRAYS

## Check that C compiler and libraries support IEEE754 data format.
OCTAVE_IEEE754_DATA_FORMAT

## Are bit_and, bit_or, and bit_xor defined as templated operators?
OCTAVE_CXX_BITWISE_OP_TEMPLATES

## Can complex class set components independently?
OCTAVE_CXX_COMPLEX_SETTERS

## Are there functions to access real/imag parts of numbers via references?
OCTAVE_CXX_COMPLEX_REFERENCE_ACCESSORS

## Does the C compiler handle alloca and const correctly?
AC_FUNC_ALLOCA

## Does the C compiler support Automake subdir-objects option?
AM_PROG_CC_C_O

## Set the target Windows version early so that gnulib can use all functions
## that are available in that version.
case $host_os in
  msdosmsvc | mingw*)
    AC_MSG_CHECKING([for required _WIN32_WINNT])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <windows.h>
        #if _WIN32_WINNT < 0x0601
        #error "Wrong version"
        #endif
        ]], [])],
      [AC_MSG_RESULT([none])],
      [AC_DEFINE(_WIN32_WINNT, 0x0601,
        [Define to 0x0601 to access SetCurrentProcessExplicitAppUserModelID.])
       AC_MSG_RESULT([0x0601])])
  ;;
esac

### gnulib initialization: part 2
dnl Must take place after part 1, and after standard compiler options and
dnl search paths have been established, and at the same priority level as
dnl system library function and header checks, but before checks for all
dnl external library dependencies.

gl_INIT

### Checks for header files.

AC_HEADER_SYS_WAIT

## C headers

dnl Use multiple AC_CHECKs to avoid line continuations '\' in list.
AC_CHECK_HEADERS([dlfcn.h floatingpoint.h fpu_control.h grp.h])
AC_CHECK_HEADERS([ieeefp.h pthread.h pwd.h sys/ioctl.h])
AC_CHECK_HEADERS([stropts.h sys/stropts.h])

## Some versions of GCC fail when using -fopenmp and including
## stdatomic.h, so we try to work around that.  Use the compile_ifelse
## macro because we are trying to test the case of the header file
## existing but not being usable.  The default warning from the
## check_headers macro is not appropriate here.
AC_CACHE_CHECK([whether stdatomic.h can be compiled],
  [octave_cv_stdatomic_h_ok],
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
    #include <stdatomic.h>
    ]])],
    octave_cv_stdatomic_h_ok=yes,
    octave_cv_stdatomic_h_ok=no)
  ])
if test $octave_cv_stdatomic_h_ok = yes; then
  AC_DEFINE(OCTAVE_STDATOMIC_H_OK, 1,
    [Define to 1 if stdatomic.h can be compiled.])
fi

## Find a termio header to include.

AC_CHECK_HEADERS([termios.h], have_termios_h=yes, have_termios_h=no)
AC_CHECK_HEADERS([termio.h], have_termio_h=yes, have_termio_h=no)
AC_CHECK_HEADERS([sgtty.h], have_sgtty_h=yes, have_sgtty_h=no)
AC_CHECK_HEADERS([conio.h], have_conio_h=yes, have_conio_h=no)

if test $have_termios_h != yes \
    && test $have_termio_h != yes \
    && test $have_sgtty_h != yes; then
  AC_MSG_WARN([couldn't find one of termios.h, termio.h, or sgtty.h!])
fi

## For MSVC compilers, avoid #define of min/max from windows.h header.
if test $have_msvc = yes; then
  AC_DEFINE(NOMINMAX, 1, [Define to 1 to avoid min/max macro definition in Windows headers.])
fi

### Determine types and size of types.

AC_TYPE_INT64_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UID_T
AC_TYPE_UINT64_T
AC_CHECK_TYPES([dev_t, ino_t])
AC_CHECK_TYPES([ptrdiff_t])

AC_CHECK_TYPES([long long int])
if test $ac_cv_type_long_long_int = yes; then
  AC_DEFINE(OCTAVE_HAVE_LONG_LONG_INT, 1,
    [Define to 1 if the system has the type 'long long int'.])
fi

AC_CHECK_TYPES([unsigned long long int])
if test $ac_cv_type_unsigned_long_long_int = yes; then
  AC_DEFINE(OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT, 1,
    [Define to 1 if the system has the type 'unsigned long long int'.])
fi

## Check for long double type (may be used for 64-bit integer computations).
AC_CHECK_SIZEOF([long double])

### Check structures and existence of necessary members.

AC_CHECK_MEMBERS([struct stat.st_blksize, struct stat.st_blocks,
                  struct stat.st_rdev])
AC_CHECK_MEMBERS([struct group.gr_passwd])

AC_STRUCT_TIMEZONE

## Check if C++ compiler allows overload of char, int8_t, and uint8_t.
AC_CACHE_CHECK([whether ${CXX-g++} allows overload of char, int8_t, and uint8_t],
  [octave_cv_overload_char_int8_t],
  [AC_LANG_PUSH(C++)
  AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
    #include <stdint.h>
    void somefunc(char x) {};
    void somefunc(int8_t x) {};
    void somefunc(uint8_t x) {};
    ]])],
    octave_cv_overload_char_int8_t=yes,
    octave_cv_overload_char_int8_t=no)
  AC_LANG_POP(C++)
  ])
if test $octave_cv_overload_char_int8_t = yes; then
  AC_DEFINE(OCTAVE_HAVE_OVERLOAD_CHAR_INT8_TYPES, 1,
    [Define to 1 if C++ allows overload of char, int8_t, and uint8_t types.])
fi

## Check for MIPS-specific non-standard encoding of NaN value
OCTAVE_MIPS_NAN

### Checks for functions and variables.

dnl The list of functions is short because we use gnulib to guarantee
dnl valid implementations for many common problematic functions.

dnl These checks define/undefine HAVE_FUNCNAME in config.h.
dnl Code tests HAVE_FUNCNAME and either uses function or provides workaround.
dnl Use multiple AC_CHECKs to avoid line continuations '\' in list
AC_CHECK_FUNCS([ctermid dup2])
AC_CHECK_FUNCS([endgrent endpwent execvp fork])
AC_CHECK_FUNCS([getegid geteuid getgid getgrent getgrgid getgrnam])
AC_CHECK_FUNCS([getpgrp getpid getppid getpwent getpwuid getuid])
AC_CHECK_FUNCS([isascii kill])
AC_CHECK_FUNCS([lgamma_r lgammaf_r])
AC_CHECK_FUNCS([realpath resolvepath])
AC_CHECK_FUNCS([select setgrent setpwent setsid siglongjmp strsignal])
AC_CHECK_FUNCS([tcgetattr tcsetattr toascii])
AC_CHECK_FUNCS([umask waitpid])
AC_CHECK_FUNCS([_getch _kbhit])

## Check for math defines such as M_LN2 in math.h
AC_CACHE_CHECK([for MATH DEFINES in math.h],
  [octave_cv_header_math_defines],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
    #include <math.h>
    ]], [[
    double x = M_LN2;]])],
    octave_cv_header_math_defines=yes,
    octave_cv_header_math_defines=no)
  ])

if test $octave_cv_header_math_defines = no; then
  ## Check again and try defining _USE_MATH_DEFINES
  AC_CACHE_CHECK([whether _USE_MATH_DEFINES needs to be defined],
    [octave_cv_header__use_math_defines],
    [save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -D_USE_MATH_DEFINES"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
      #include <math.h>
      ]], [[
      double x = M_LN2;]])],
      octave_cv_header__use_math_defines=yes,
      octave_cv_header__use_math_defines=no)
    CPPFLAGS="$save_CPPFLAGS"
    ])
  if test $octave_cv_header__use_math_defines = yes; then
    octave_cv_header_math_defines=yes
    AC_DEFINE(_USE_MATH_DEFINES, 1,
      [Define to 1 if _USE_MATH_DEFINES is required to get math constants like M_LN2.])
    CPPFLAGS="$CPPFLAGS -D_USE_MATH_DEFINES"
  fi
fi

if test $octave_cv_header_math_defines = yes; then
  AC_DEFINE(HAVE_MATH_DEFINES, 1,
    [Define to 1 if defines such as M_PI are available in math.h])
else
  AC_MSG_ERROR([MATH DEFINES in math.h such as M_PI are required to build Octave])
fi

## Use Unicode aware functions on Windows
case $host_os in
  msdosmsvc | mingw*)
    AC_DEFINE(_UNICODE, 1, [Use Unicode CRT functions on Windows by default.])
    AC_DEFINE(UNICODE, 1, [Use Windows Unicode API by default.])
  ;;
esac

OCTAVE_UNICODE_EXE_LDFLAGS=""
case $host_os in
  mingw*)
    OCTAVE_UNICODE_EXE_LDFLAGS="-municode"
  ;;
esac

AC_SUBST(OCTAVE_UNICODE_EXE_LDFLAGS)

## Windows-specific use of functions
case $host_os in
  msdosmsvc | mingw*)
    AC_CHECK_FUNCS([setvbuf], [],
                   [AC_MSG_ERROR([Missing function required to build Octave])])

    ## We need this for GetDeviceCaps.  Should we actually check for
    ## the function and library?
    LIBS="-lgdi32 $LIBS"
  ;;
esac

## Windows-specific module used for winqueryreg function
case $host_os in
  msdosmsvc | mingw*)
    AC_CHECK_HEADERS([psapi.h],
      [CPPFLAGS="-DPSAPI_VERSION=2 $CPPFLAGS"],
      [AC_MSG_ERROR([Missing PSAPI required to build for Windows target.])],
      [#include <windows.h>])
  ;;
esac

## Windows-specific module used for canonical UNC paths
case $host_os in
  msdosmsvc | mingw*)
    AC_CHECK_HEADERS([shlwapi.h],
      [LIBS="-lshlwapi $LIBS"],
      [AC_MSG_ERROR([Missing SHLWAPI required to build for Windows target.])],
      [#include <windows.h>])
  ;;
esac

## Cygwin kluge for getrusage.
AC_CHECK_FUNCS([getrusage])
case $host_os in
  cygwin*)
    AC_DEFINE(RUSAGE_TIMES_ONLY, 1,
      [Define to 1 if the struct rusage only has time information.])
  ;;
esac

AC_CHECK_FUNCS([getpwnam], [], [AC_CHECK_LIB([sun], [getpwnam])])

AC_FUNC_CLOSEDIR_VOID

## Check return type of matherr
AC_CACHE_CHECK([for struct exception in math.h],
  [octave_cv_func_matherr_type],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
      #include <math.h>
      ]], [[
      struct exception *x;
      x->type;
      x->name;
      ]])],
    octave_cv_func_matherr_type=yes,
    octave_cv_func_matherr_type=no)
  ])
if test $octave_cv_func_matherr_type = yes; then
  AC_DEFINE(EXCEPTION_IN_MATH, 1,
    [Define to 1 if math.h declares struct exception for matherr.])
fi

## Signal stuff.

AC_CHECK_DECLS([sys_siglist], [], [],
[[#include <signal.h>
/* NetBSD declares sys_siglist in unistd.h.  */
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
]])

### Configure Bison as push or pull parser.

ENABLE_COMMAND_LINE_PUSH_PARSER=yes
AC_ARG_ENABLE([command-line-push-parser],
  [AS_HELP_STRING([--disable-command-line-push-parser],
    [don't use Bison's push parser interface in the command line REPL])],
  [if test "$enableval" = no; then ENABLE_COMMAND_LINE_PUSH_PARSER=no; fi], [])
if test $ENABLE_COMMAND_LINE_PUSH_PARSER = yes; then
  AC_DEFINE(OCTAVE_ENABLE_COMMAND_LINE_PUSH_PARSER, 1,
    [Define to 1 to use Bison's push parser interface in the command line REPL.])
fi

### Configure compilation of *experimental* Virtual Machine evaluator.

ENABLE_VM_EVALUATOR=yes
AC_ARG_ENABLE([vm-evaluator],
  [AS_HELP_STRING([--disable-vm-evaluator],
    [don't compile *experimental* virtual machine evaluator])],
  [if test "$enableval" = no; then ENABLE_VM_EVALUATOR=no; fi], [])
if test $ENABLE_VM_EVALUATOR = yes; then
  AC_DEFINE(OCTAVE_ENABLE_VM_EVALUATOR, 1,
    [Define to 1 to build experimental Virtual Machine evaluator.])
fi

### Check for PCRE2 or PCRE regex library, requiring one to exist.

have_pcre2=no
have_pcre=no
save_CPPLAGS="$CPPFLAGS"
CPPFLAGS="-DPCRE2_CODE_UNIT_WIDTH=8 $CPPFLAGS"
OCTAVE_CHECK_LIB(pcre2, PCRE2,
  [], [pcre2.h pcre2/pcre2.h], [pcre2_compile_8], [], [],
  [OCTAVE_CHECK_LIB_PCRE2_OK([have_pcre2=yes],
    [AC_MSG_ERROR([PCRE2 library must be built with UTF support (--enable-utf)])])],
  [libpcre2-8])
CPPFLAGS="$save_CPPFLAGS"

if test $have_pcre2 = no; then
  OCTAVE_CHECK_LIB(pcre, PCRE,
    [], [pcre.h pcre/pcre.h], [pcre_compile], [], [],
    [OCTAVE_CHECK_LIB_PCRE_OK([have_pcre=yes],
      [AC_MSG_ERROR([PCRE library must be built with UTF support (--enable-utf)])])],
    [libpcre])
fi

if test $have_pcre2 = yes; then
  AC_DEFINE(HAVE_PCRE2, 1, [Define to 1 if PCRE2 is available.])

  ## Only one of PCRE2 or PCRE is used, so avoid having to define and use
  ## both PCRE2_* and PCRE_* variables everywhere.

  PCRE_CPPFLAGS="$PCRE2_CPPFLAGS"
  PCRE_LDFLAGS="$PCRE2_LDFLAGS"
  PCRE_LIBS="$PCRE2_LIBS"

elif test $have_pcre = yes; then
  AC_DEFINE(HAVE_PCRE, 1, [Define to 1 if PCRE is available.])
else
  AC_MSG_ERROR([to build Octave, you must have the PCRE or PCRE2 library and header files installed])
fi

### Check for Qhull library.

QHULL_CPPFLAGS=
QHULL_LDFLAGS=
QHULL_LIBS=
OCTAVE_CHECK_LIB(qhull_r, QHull,
  [Qhull library not found.  This will result in loss of functionality for some geometry functions.],
  [libqhull_r/libqhull_r.h libqhull_r.h],
  [qh_qhull], [], [],
  [warn_qhull_r=
  OCTAVE_CHECK_QHULL_VERSION
  OCTAVE_CHECK_LIB_QHULL_OK(
    [AC_DEFINE(HAVE_QHULL, 1, [Define to 1 if Qhull is available.])
     QHULL_CPPFLAGS="$QHULL_R_CPPFLAGS"
     QHULL_LDFLAGS="$QHULL_R_LDFLAGS"
     QHULL_LIBS="$QHULL_R_LIBS"],
    [warn_qhull_r="Qhull library found, but does not seem to work properly.  This will result in loss of functionality for some geometry functions.  Please try recompiling the library with -fno-strict-aliasing."])])
AC_SUBST(QHULL_CPPFLAGS)
AC_SUBST(QHULL_LDFLAGS)
AC_SUBST(QHULL_LIBS)

### Check for RapidJSON header-only library.

check_for_rapidjson=yes
AC_ARG_ENABLE([rapidjson],
  [AS_HELP_STRING([--disable-rapidjson],
    [disable rapidjson])],
  [if test "$enableval" = no; then check_for_rapidjson=no; fi], [])
warn_rapidjson=
if test $check_for_rapidjson = yes; then
  AC_LANG_PUSH(C++)
  AC_CHECK_HEADER([rapidjson/rapidjson.h],
                  [have_rapidjson=yes],
                  [have_rapidjson=no
                   warn_rapidjson="RapidJSON library not found.  Octave will not be able to read or write JSON files."])

  if test $have_rapidjson = yes; then
    AC_DEFINE(HAVE_RAPIDJSON, 1, [Define to 1 if RapidJSON is available.])

    ## Additional check on RapidJSON library that was found
    ## Some features of the version of RapidJSON that is packaged in many
    ## distributions do not build correctly with newer compilers.
    ## See also:
    ## https://github.com/Tencent/rapidjson/issues/909
    ## https://github.com/Tencent/rapidjson/issues/1718
    AC_CACHE_CHECK([for working PrettyWriter in RapidJSON],
      [octave_cv_rapidjson_has_prettywriter],
      [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <rapidjson/prettywriter.h>
        ]], [[
        rapidjson::StringBuffer json;
        rapidjson::PrettyWriter<rapidjson::StringBuffer, rapidjson::UTF8<>,
                                rapidjson::UTF8<>, rapidjson::CrtAllocator,
                                rapidjson::kWriteNanAndInfFlag> writer (json);
        ]])],
        [octave_cv_rapidjson_has_prettywriter=yes],
        [octave_cv_rapidjson_has_prettywriter=no])
      ])
    if test $octave_cv_rapidjson_has_prettywriter = yes; then
      AC_DEFINE(HAVE_RAPIDJSON_PRETTYWRITER, 1,
        [Define to 1 if the RapidJSON PrettyWriter function is available.])
    else
      warn_rapidjson='RapidJSON library found, but some features do not seem to work properly.  The "PrettyWriter" option in jsonencode will be disabled.'
    fi
  fi
  AC_LANG_POP([C++])
fi

if test -n "$warn_rapidjson"; then
  OCTAVE_CONFIGURE_WARNING([warn_rapidjson])
fi

### Check for readline library.

OCTAVE_ENABLE_READLINE

### Check for ZLIB library.

OCTAVE_CHECK_LIB(z, ZLIB,
  [ZLIB library not found.  Octave will not be able to load or save compressed data files or HDF5 files.],
  [zlib.h], [gzclearerr])

## Also define HAVE_ZLIB if libz is found.
if test -n "$Z_LIBS"; then
  AC_DEFINE(HAVE_ZLIB, 1, [Define to 1 if ZLIB is available.])
fi

### Check for BZIP2 library.

OCTAVE_CHECK_LIB(bz2, BZIP2,
  [BZIP2 library not found.  Octave will not be able to compress or decompress bzip2 files.],
  [bzlib.h], [BZ2_bzCompressInit])

### Check for HDF5 library.

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$Z_CPPFLAGS $CPPFLAGS"
LIBS="$Z_LDFLAGS $Z_LIBS $LIBS"
OCTAVE_CHECK_LIB(hdf5, HDF5,
  [HDF5 library not found.  Octave will not be able to load or save HDF5 data files.],
  [hdf5.h], [H5Gget_num_objs], [], [],
  [warn_hdf5=
   OCTAVE_CHECK_HDF5_HAS_VER_16_API
   OCTAVE_CHECK_HDF5_HAS_UTF8_API
   AC_DEFINE(HAVE_HDF5, 1,
     [Define to 1 if HDF5 is available and newer than version 1.6.])
   if test $have_msvc = yes; then
     OCTAVE_CHECK_LIB_HDF5_DLL
   fi
  ])
CPPFLAGS="$save_CPPFLAGS"
LIBS="$save_LIBS"

dnl FIXME: Should we check for this now, or wait until some version of HDF5
dnl actually supports this feature?
have_hdf5_int2float_conversions=no
if test $have_hdf5_int2float_conversions = yes; then
  AC_DEFINE(HAVE_HDF5_INT2FLOAT_CONVERSIONS, 1,
    [Define to 1 if/when HDF5 supports automatic conversion between integer and floating-point binary data.])
fi

### Check for FFTW library.
dnl Default to Fortran FFTPACK if it is not available.

## Check for FFTW header and library.
OCTAVE_CHECK_LIB(fftw3, FFTW3,
  [FFTW3 library not found.  Octave will not be able to perform fft calculations.],
  [fftw3.h], [fftw_plan_dft_1d])

OCTAVE_CHECK_LIB(fftw3f, FFTW3F,
  [FFTW3F library not found.  Octave will not be able to perform fft calculations.],
  [fftw3.h], [fftwf_plan_dft_1d])

## Check command line for the option to disable multi-threaded FFTW.
build_fftw_threads=yes
AC_ARG_ENABLE([fftw-threads],
  [AS_HELP_STRING([--disable-fftw-threads],
    [disable Multi-threaded FFTW])],
  [if test "$enableval" = no; then
     build_fftw_threads=no
   fi],
  [])

dnl Octave is currently unable to use FFTW unless both float and double
dnl versions are available.

AM_CONDITIONAL([AMCOND_HAVE_FFTW],
  [test -n "$FFTW3_LIBS" && test -n "$FFTW3F_LIBS"])

if test -n "$FFTW3_LIBS" && test -n "$FFTW3F_LIBS"; then
  AC_DEFINE(HAVE_FFTW, 1, [Define to 1 if both FFTW3 and FFTW3F libraries are available.])
else
  dnl --without-fftw3 given, or one of the FFTW3 libs not installed.
  dnl Don't check for FFTW threads as this is now pointless.
  build_fftw_threads=no
fi

## Check for multithreaded FFTW library.
## Fallback to singlethreaded if not found or disabled.
if test $build_fftw_threads = yes; then
  OCTAVE_CHECK_FFTW_THREADS(fftw3, fftw_plan_with_nthreads)
  OCTAVE_CHECK_FFTW_THREADS(fftw3f, fftwf_plan_with_nthreads)
fi

## Collections of flags.
dnl These are here instead of just in the Makefile.am file because we
dnl substitute some of them into other source files like mkoctfile.

## Order matters, at least on some systems (Cygwin, for example).

FFTW_XCPPFLAGS="$FFTW3_CPPFLAGS $FFTW3F_CPPFLAGS"

FFTW_XLDFLAGS="$FFTW3_LDFLAGS $FFTW3F_LDFLAGS"

FFTW_XLIBS="$FFTW3_LIBS $FFTW3F_LIBS"

AC_SUBST(FFTW_XCPPFLAGS)
AC_SUBST(FFTW_XLDFLAGS)
AC_SUBST(FFTW_XLIBS)

## Subdirectory of liboctave/external to build if FFTW is not found.
FFT_DIR="fftpack"
AC_SUBST(FFT_DIR)

### Check for GLPK library and header.

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$Z_CPPFLAGS $CPPFLAGS"
LIBS="$Z_LDFLAGS $Z_LIBS $LIBS"
OCTAVE_CHECK_LIB(glpk, GLPK,
  [GLPK library not found.  The glpk function for solving linear programs will be disabled.],
  [glpk.h glpk/glpk.h], [glp_simplex], [], [],
  [warn_glpk=
   OCTAVE_CHECK_LIB_GLPK_OK(
    [AC_DEFINE(HAVE_GLPK, 1, [Define to 1 if GLPK is available.])],
    [warn_glpk="GLPK library found, but does not seem to work properly; disabling glpk function"])])
LIBS="$save_LIBS"
CPPFLAGS="$save_CPPFLAGS"

### Check for cURL library existence and functionality.

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$Z_CPPFLAGS $CPPFLAGS"
LIBS="$Z_LDFLAGS $Z_LIBS $LIBS"
OCTAVE_CHECK_LIB(curl, cURL,
  [cURL library not found.  The ftp objects, urlread, and urlwrite functions will be disabled.],
  [curl/curl.h], [curl_easy_escape])
if test -n "$CURL_LIBS"; then
  ## Additional check on cURL library that was found
  AC_CACHE_CHECK([for CURLOPT_DIRLISTONLY in curl/curl.h],
    [octave_cv_curl_has_curlopt_dirlistonly],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
      #include <curl/curl.h>
      ]], [[
      curl_easy_setopt ((CURL*)NULL, CURLOPT_DIRLISTONLY, 0);
      ]])],
      [octave_cv_curl_has_curlopt_dirlistonly=yes],
      [octave_cv_curl_has_curlopt_dirlistonly=no])
    ])
  if test $octave_cv_curl_has_curlopt_dirlistonly = no; then
    AC_DEFINE(CURLOPT_DIRLISTONLY, CURLOPT_FTPLISTONLY,
      [Define to the legacy option name if using an older version of cURL.])
  fi
fi
LIBS="$save_LIBS"
CPPFLAGS="$save_CPPFLAGS"

### Check for sndfile library.

OCTAVE_CHECK_LIB(sndfile, sndfile,
  [sndfile library not found.  The audioinfo, audioread, and audiowrite functions will be disabled.],
  [sndfile.h], [sf_open],
  [], [don't use sndfile library, disable audio file I/O],
  [warn_sndfile=
   OCTAVE_CHECK_LIB_SNDFILE_OK(
    [AC_DEFINE(HAVE_SNDFILE, 1, [Define to 1 if sndfile is available.])],
    [warn_sndfile="sndfile library found, but does not seem to work properly; disabling audio file I/O functions"])])

### Check for PortAudio library.

OCTAVE_CHECK_LIB(portaudio, PortAudio,
  [PortAudio library not found.  The audioplayer, audiorecorder, and audiodevinfo functions will be disabled.],
  [portaudio.h], [Pa_GetDeviceCount],
  [], [don't use PortAudio library, disable audio playback and recording],
  [], [portaudio-2.0])

### Check for either of GraphicsMagick++/ImageMagick++ libraries.

check_magick=yes
use_magick=no
AC_ARG_WITH([magick],
  [AS_HELP_STRING([--with-magick=LIB],
    [select library to use for image I/O (options: GraphicsMagick++(default) or ImageMagick++)])],
  [if test x"$withval" = xno; then
     check_magick=no
   else
     magick="$withval"
   fi], [magick="GraphicsMagick++"])

if test $check_magick = yes; then

  MAGICK_CPPFLAGS=
  MAGICK_LDFLAGS=
  MAGICK_LIBS=

  PKG_CHECK_EXISTS([$magick], [
    dnl We are using --cflags now instead of --cflags-only-I because some
    dnl Graphics/ImageMagick++ packages will provide -D options for
    dnl MAGICKCORE_HDRI_ENABLE and MAGICKCORE_QUANTUM_DEPTH that are needed
    dnl for building with a particular verison of the library.  If there
    dnl are other flags in the output from pkg-config --cflags that cause
    dnl trouble, then we may need to go back to using --cflags-only-I and
    dnl have a configure test to determine the proper definitions of
    dnl MAGICKCORE_HDRI_ENABLE and MAGICKCORE_QUANTUM_DEPTH.
    dnl See also bug #49563.
    MAGICK_CPPFLAGS="$($PKG_CONFIG --cflags $magick | $SED -e 's/^ *$//')"
    MAGICK_LDFLAGS="$($PKG_CONFIG --libs-only-L $magick | $SED -e 's/^ *$//')"
    MAGICK_LIBS="$($PKG_CONFIG --libs-only-l $magick | $SED -e 's/^ *$//')"

    warn_magick="$magick library fails tests.  The imread, imwrite, and imfinfo functions for reading and writing image files will not be fully functional."

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    CPPFLAGS="$MAGICK_CPPFLAGS $CPPFLAGS"
    LIBS="$MAGICK_LDFLAGS $MAGICK_LIBS $LIBS"
    AC_LANG_PUSH(C++)
    AC_CHECK_HEADER([Magick++.h], [
      AC_CACHE_CHECK([for Magick::ColorRGB and Magick::PixelPacket in Magick++.h],
        [octave_cv_func_magick],
        [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <Magick++.h>
          ]], [[
          Magick::ColorRGB c;
          Magick::PixelPacket pix;
          ]])],
          octave_cv_func_magick=yes,
          octave_cv_func_magick=no)
        ])
      if test $octave_cv_func_magick = yes; then
        use_magick=yes
        warn_magick=
      fi
    ])
    AC_LANG_POP(C++)
    CPPFLAGS="$save_CPPFLAGS"
    LIBS="$save_LIBS"
  ],
  [use_magick=no
   warn_magick="$magick library not found.  The imread, imwrite, and imfinfo functions for reading and writing image files will not be fully functional."])
fi

if test $use_magick = yes; then
  AC_DEFINE(HAVE_MAGICK, 1,
    [Define to 1 if Graphics/ImageMagick++ is available.])
else
  if test -n "$warn_magick"; then
    OCTAVE_CONFIGURE_WARNING([warn_magick])
  fi
  MAGICK_CPPFLAGS=
  MAGICK_LDFLAGS=
  MAGICK_LIBS=
fi
AC_SUBST(MAGICK_CPPFLAGS)
AC_SUBST(MAGICK_LDFLAGS)
AC_SUBST(MAGICK_LIBS)

### Check for X11 libraries.

AC_PATH_X
if test "$have_x" = yes; then
  AC_DEFINE(HAVE_X_WINDOWS, 1, [Define to 1 if X11 is available.])

  if test "$x_includes" != "NONE"; then
    X11_INCFLAGS="$x_includes"
  fi
  AC_SUBST(X11_INCFLAGS)

  if test -z "$x_libraries"; then
    AC_CHECK_LIB([X11], XrmInitialize, [X11_LIBS="-lX11"], [X11_LIBS=])
  elif test "$x_libraries" != "NONE"; then
    AC_CHECK_LIB([X11], XrmInitialize,
      [X11_LIBS="-L$x_libraries -lX11"], [X11_LIBS=], "-L$x_libraries")
  fi
  AC_SUBST(X11_LIBS)
fi

### Check for the Carbon framework on macOS systems.

OCTAVE_HAVE_FRAMEWORK([Carbon],
  [[#include <Carbon/Carbon.h>]], [[CGMainDisplayID ()]],
  [have_framework_carbon=yes], [have_framework_carbon=no])
if test $have_framework_carbon = yes; then
  AC_DEFINE(HAVE_FRAMEWORK_CARBON, 1,
    [Define to 1 if framework CARBON is available.])
  CARBON_LIBS="-framework Carbon"
  AC_MSG_NOTICE([adding -framework Carbon to CARBON_LIBS])
  AC_SUBST(CARBON_LIBS)
fi

## Check for the CGDisplayBitsPerPixel function.
if test $have_framework_carbon = yes; then
  OCTAVE_CARBON_CGDISPLAYBITSPERPIXEL
fi

### Check for OpenGL and helper libraries used by OpenGL graphics renderer.

check_opengl=yes
AC_ARG_WITH([opengl],
  [AS_HELP_STRING([--without-opengl],
    [don't use OpenGL libraries, disable OpenGL graphics])],
  [if test x"$withval" = xno; then
     check_opengl=no
   fi])

## Check for OpenGL library
if test $check_opengl = yes; then
  OCTAVE_CHECK_LIB_OPENGL
  if test -z "$OPENGL_LIBS"; then
    warn_opengl_libs="OpenGL libs (GL and GLU) not found.  OpenGL graphics will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_opengl_libs])
  fi
fi

## Check for FreeType 2 library

check_freetype=yes
warn_freetype=
AC_ARG_WITH([freetype],
  [AS_HELP_STRING([--without-freetype],
    [don't use FreeType library, OpenGL graphics will not be fully functional])],
  [if test x"$withval" = xno; then
     check_freetype=no
   fi])

if test $check_freetype = yes; then
  PKG_CHECK_MODULES([FT2], [freetype2], [
    min_ft2_version=9.03
    AC_MSG_CHECKING([for FreeType2 version >= $min_ft2_version])
    $PKG_CONFIG freetype2 --atleast-version=$min_ft2_version
    ac_status=$?
    if test "$ac_status" = 0; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_FREETYPE, 1, [Define to 1 if Freetype is available.])
      save_LIBS="$LIBS"
      LIBS="$FT2_LIBS $LIBS"
      AC_CHECK_FUNCS([FT_Reference_Face])
      LIBS="$save_LIBS"
    else
      AC_MSG_RESULT(no)
      warn_freetype="FreeType library >= 9.03 not found.  OpenGL graphics will not be fully functional."
    fi],
    [warn_freetype="FreeType library not found.  OpenGL graphics will not be fully functional."])
fi

if test -n "$warn_freetype"; then
  FT2_CFLAGS=
  FT2_LIBS=
  OCTAVE_CONFIGURE_WARNING([warn_freetype])
else
  dnl Alias CPPFLAGS to CFLAGS.  This is closer to the true meaning
  dnl of `pkg-config --cflags` output.
  FT2_CPPFLAGS="$FT2_CFLAGS"
  AC_SUBST(FT2_CPPFLAGS)
fi

## Check for fontconfig library

OCTAVE_CHECK_LIB(fontconfig, fontconfig,
  [Fontconfig library not found.  OpenGL graphics will not be fully functional.],
  [fontconfig.h fontconfig/fontconfig.h], [FcInit],
  [], [don't use fontconfig library, OpenGL graphics will not be fully functional])

### GUI/Qt related tests.

QT_VERSIONS="5"

AC_ARG_WITH([qt],
  [AS_HELP_STRING([--with-qt=VER], [use the Qt major version VER])
dnl Second help string must not be indented for correct alignment
AS_HELP_STRING([--without-qt], [don't use Qt libraries, disable Qt GUI])],
  [case $withval in
     yes | "")
     ;;
     no)
       QT_VERSIONS=
     ;;
     *)
       QT_VERSIONS="$withval"
     ;;
   esac])

check_qscintilla=yes
AC_ARG_WITH([qscintilla],
  [AS_HELP_STRING([--without-qscintilla], [disable QScintilla editor])],
  [if test x"$withval" = xno; then
     check_qscintilla=no
   fi])

OCTAVE_CHECK_QT([$QT_VERSIONS])

## Default terminal font for the GUI.

case $host_os in
  mingw* | msdosmsvc)
    DEFAULT_TERMINAL_FONT="Lucida Console"
  ;;
  *)
    DEFAULT_TERMINAL_FONT="Courier"
  ;;
esac
DEFAULT_TERMINAL_FONT_SIZE=10
AC_SUBST(DEFAULT_TERMINAL_FONT)
AC_SUBST(DEFAULT_TERMINAL_FONT_SIZE)

### Check for FLTK library.

check_fltk=yes
build_fltk_graphics=no

AC_ARG_WITH([fltk],
  [AS_HELP_STRING([--without-fltk],
    [don't use FLTK libraries, disable OpenGL graphics with FLTK widgets])],
  [if test x"$withval" = xno; then
     check_fltk=no
   fi])

FLTK_CPPFLAGS=
FLTK_LDFLAGS=
FLTK_LIBS=

if test $check_fltk = yes; then
  AC_ARG_WITH([fltk-prefix],
    [AS_HELP_STRING([--with-fltk-prefix=PFX],
      [prefix where FLTK is installed (optional)])],
    [fltk_prefix="$withval"],
    [fltk_prefix=""])

  AC_ARG_WITH([fltk-exec-prefix],
    [AS_HELP_STRING([--with-fltk-exec-prefix=PFX],
      [exec prefix where FLTK is installed (optional)])],
    [fltk_exec_prefix="$withval"],
    [fltk_exec_prefix=""])

  if test "x$fltk_exec_prefix" != x; then
    fltk_args="$fltk_args --exec-prefix=$fltk_exec_prefix"
    if test "x${FLTK_CONFIG+set}" != xset ; then
      FLTK_CONFIG="$fltk_exec_prefix/bin/fltk-config"
    fi
  fi

  if test "x$fltk_prefix" != x; then
    fltk_args="$fltk_args --prefix=$fltk_prefix"
    if test x${FLTK_CONFIG+set} != xset ; then
      FLTK_CONFIG="$fltk_prefix/bin/fltk-config"
    fi
  fi

  AC_PATH_PROG([FLTK_CONFIG], [fltk-config], [no])

  warn_fltk_config=
  warn_fltk_opengl=

  if test "$FLTK_CONFIG" = no; then
    warn_fltk_config="FLTK config script not found.  FLTK toolkit will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_fltk_config])
  else
    ## Only include -I options in FLTK_CPPFLAGS
    fltk_cflags=`$FLTK_CONFIG $fltkconf_args --use-gl --cflags`
    for fltk_option in $fltk_cflags; do
      case $fltk_option in
        -I*)
          FLTK_CPPFLAGS="$FLTK_CPPFLAGS $fltk_option"
        ;;
      esac
    done
    FLTK_CPPFLAGS=`echo "$FLTK_CPPFLAGS" | $SED -e 's/^ \+//'`

    ## Split -L and -l options into FLTK_LDFLAGS and FLTK_LIBS
    fltk_ldflags=`$FLTK_CONFIG $fltkconf_args --use-gl --ldflags`
    for fltk_option in $fltk_ldflags; do
      case $fltk_option in
        -l*)
          FLTK_LIBS="$FLTK_LIBS $fltk_option"
        ;;
        *)
          FLTK_LDFLAGS="$FLTK_LDFLAGS $fltk_option"
        ;;
      esac
    done
    FLTK_LIBS=`echo "$FLTK_LIBS" | $SED -e 's/^ \+//'`
    FLTK_LDFLAGS=`echo "$FLTK_LDFLAGS" | $SED -e 's/^ \+//'`

    case $host_os in
      mingw*)
        FLTK_LDFLAGS=`echo $FLTK_LDFLAGS | $SED -e 's/-mwindows//g'`
      ;;
    esac
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([for OpenGL support in FLTK],
      [octave_cv_fltk_opengl_support],
      [save_CPPFLAGS="$CPPFLAGS"
      CPPFLAGS="$CFLAGS $FLTK_CPPFLAGS"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <FL/gl.h>
        ]], [[
        int nothing = 0;
        ]])],
        octave_cv_fltk_opengl_support=yes,
        octave_cv_fltk_opengl_support=no)
      CPPFLAGS="$save_CPPFLAGS"
      ])
    if test $octave_cv_fltk_opengl_support = no; then
      warn_fltk_opengl="FLTK does not have OpenGL support.  OpenGL graphics with FLTK widgets will be disabled."
    else
      build_fltk_graphics=yes
      AC_DEFINE(HAVE_FLTK, 1, [Define to 1 if FLTK is available.])
    fi
    AC_LANG_POP(C++)

    if test -n "$warn_fltk_opengl"; then
      OCTAVE_CONFIGURE_WARNING([warn_fltk_opengl])
    fi
  fi
fi

AC_SUBST(FLTK_CPPFLAGS)
AC_SUBST(FLTK_LDFLAGS)
AC_SUBST(FLTK_LIBS)

### Final determination of whether OpenGL graphics can be built.
if test $build_qt_graphics = no && test $build_fltk_graphics = no; then
  opengl_graphics=no
else
  opengl_graphics=yes
fi

## Check for gl2ps which is required for printing with OpenGL graphics.

if test $opengl_graphics = yes; then
  AC_CHECK_HEADERS([gl2ps.h],
    [GL2PS_LIBS="-lgl2ps"],
    [warn_gl2ps="gl2ps library not found.  Printing of OpenGL graphics will be disabled."])

  if test -n "$warn_gl2ps"; then
    OCTAVE_CONFIGURE_WARNING([warn_gl2ps])
  else
    save_LIBS="$LIBS"
    LIBS="$GL2PS_LIBS $LIBS"
    AC_CHECK_FUNCS([gl2psLineJoin])
    LIBS="$save_LIBS"
  fi

  AC_SUBST(GL2PS_LIBS)
fi

### Use the system-wide installation of GNU FreeFont if the user requests it.
dnl By default, Octave includes its own copy of the FreeSans font which it
dnl installs in its own directory for use as a fallback default font.  If this
dnl configuration option is used, then do not install our own copy of the fonts
dnl and assume they can be found in the provided system directory.
SYSTEM_FREEFONT_DIR=
AC_ARG_WITH([system-freefont],
  [AS_HELP_STRING([--with-system-freefont=DIR],
    [use FreeSans fonts in DIR (default: install and use internal copy)])])
case $with_system_freefont in
  no | "")
    SYSTEM_FREEFONT_DIR=
    ;;
  yes)
    AC_MSG_ERROR([directory argument required for --with-system-freefont])
    ;;
  *)
    SYSTEM_FREEFONT_DIR=$with_system_freefont
    ;;
esac
if test -n "$SYSTEM_FREEFONT_DIR"; then
  AC_MSG_NOTICE([configuring Octave to use system fonts in $SYSTEM_FREEFONT_DIR])
  AC_DEFINE_UNQUOTED([SYSTEM_FREEFONT_DIR], ["$SYSTEM_FREEFONT_DIR"],
    [Define this to be the system directory containing the GNU FreeFont fonts.])
fi
AM_CONDITIONAL([AMCOND_INSTALL_INTERNAL_FONT_FILES],
  [test -z "$SYSTEM_FREEFONT_DIR"])

### Check for qrupdate library.

dnl No need to adjust FFLAGS because only link is attempted.
dnl Must supply proper LIBS, however.
save_LIBS="$LIBS"
LIBS="$LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS"
OCTAVE_CHECK_LIB(qrupdate, qrupdate,
  [qrupdate not found.  The QR & Cholesky updating functions will be slow.],
  [],
  [sqr1up],
  [Fortran 77], [don't use qrupdate, disable QR & Cholesky updating functions])

## Additional check to see if qrupdate lib found supports LU updates
if test -n "$QRUPDATE_LIBS"; then
  AC_CACHE_CHECK([for slup1up in $QRUPDATE_LIBS],
    [octave_cv_func_slup1up],
    [LIBS="$LIBS $QRUPDATE_LIBS"
    AC_LANG_PUSH([Fortran 77])
    AC_LINK_IFELSE([AC_LANG_CALL([], [slup1up])],
      octave_cv_func_slup1up=yes, octave_cv_func_slup1up=no)
    AC_LANG_POP([Fortran 77])
  ])
  if test $octave_cv_func_slup1up = yes; then
    AC_DEFINE(HAVE_QRUPDATE_LUU, 1, [Define to 1 if qrupdate supports LU updates.])
  fi
fi
LIBS="$save_LIBS"

if test $ENABLE_64 = yes; then
  CHOLMOD_TAG="_l_"
  CXSPARSE_TAG="_dl_"
  UMFPACK_TAG="_zl_"
else
  CHOLMOD_TAG="_"
  CXSPARSE_TAG="_di_"
  UMFPACK_TAG="_zi_"
fi

## On some systems, explicitly linking with -lsuitesparseconfig is needed
## when using SuiteSparse libraries.  If so, the checks below for specific
## SuiteSparse libraries may fail without it.  Include LIB_CLOCK_GETTIME (from
## gnulib) on systems where needed.

save_LIBS="$LIBS"
LIBS="$LIB_CLOCK_GETTIME $LIBS"
OCTAVE_CHECK_LIB(suitesparseconfig, SUITESPARSECONFIG,
  [], [], [SuiteSparse_time], [],
  [don't link directly with suitesparseconfig library])
LIBS="$save_LIBS"

if test -n "$SUITESPARSECONFIG_LIBS" && test -n "$LIB_CLOCK_GETTIME"; then
  SUITESPARSECONFIG_LIBS="$SUITESPARSECONFIG_LIBS $LIB_CLOCK_GETTIME"
fi

## Save and restore LIBS after all checks for SuiteSparse libraries.
suitesparse_save_LIBS="$LIBS"
LIBS="$SUITESPARSECONFIG_LIBS $LIBS"

### Check for AMD library.

OCTAVE_CHECK_LIB(amd, AMD,
  [AMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [amd.h suitesparse/amd.h amd/amd.h],
  [amd_postorder],
  [], [don't use AMD library, disable some sparse matrix functionality])

### Check for CAMD library.

OCTAVE_CHECK_LIB(camd, CAMD,
  [CAMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [camd.h suitesparse/camd.h camd/camd.h],
  [camd_postorder],
  [], [don't use CAMD library, disable some sparse matrix functionality])

### Check for COLAMD library.

OCTAVE_CHECK_LIB(colamd, COLAMD,
  [COLAMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [colamd.h suitesparse/colamd.h colamd/colamd.h],
  [colamd],
  [], [don't use COLAMD library, disable some sparse matrix functionality])

### Check for CCOLAMD library.

OCTAVE_CHECK_LIB(ccolamd, CCOLAMD,
  [CCOLAMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [ccolamd.h suitesparse/ccolamd.h ccolamd/ccolamd.h],
  [ccolamd],
  [], [don't use CCOLAMD library, disable some sparse matrix functionality])

### Check for CHOLMOD library.
dnl If your cholmod library requires cblas, then you will need to configure
dnl with --with-cholmod="-lcholmod -lcblas".

save_LIBS="$LIBS"
LIBS="$COLAMD_LDFLAGS $COLAMD_LIBS $AMD_LDFLAGS $AMD_LIBS $LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS"
OCTAVE_CHECK_LIB(cholmod, CHOLMOD,
  [CHOLMOD library not found.  This will result in some lack of functionality for sparse matrices.],
  [cholmod.h suitesparse/cholmod.h cholmod/cholmod.h],
  [cholmod${CHOLMOD_TAG}start],
  [], [don't use CHOLMOD library, disable some sparse matrix functionality])
LIBS="$save_LIBS"

### Check for SPQR library.

save_LIBS="$LIBS"
LIBS="$CHOLMOD_LIBS $LIBS"
OCTAVE_CHECK_LIB(spqr, SPQR,
  [SPQR library not found.  This will result in some lack of functionality for sparse matrices.],
  [SuiteSparseQR.hpp suitesparse/SuiteSparseQR.hpp],
  [SuiteSparseQR_C],
  [C++], [don't use SPQR library, disable some sparse matrix functionality])
LIBS="$save_LIBS"

### Check for CXSparse library.

OCTAVE_CHECK_LIB(cxsparse, CXSparse,
  [CXSparse library not found.  This will result in some lack of functionality for sparse matrices.],
  [cs.h suitesparse/cs.h cxsparse/cs.h],
  [cs${CXSPARSE_TAG}sqr],
  [C++], [don't use CXSparse library, disable some sparse matrix functionality])

if test -n "$CXSPARSE_LIBS"; then
  ## Additional check for CXSparse version >= 2.2
  if test $octave_cv_lib_cxsparse = yes; then
    OCTAVE_CHECK_CXSPARSE_VERSION_OK
    if test $octave_cv_cxsparse_version_ok = no; then
      AC_MSG_ERROR([CXSparse library is too old (< version 2.2).  Upgrade CXSparse (SuiteSparse) or configure Octave with --without-cxsparse"])
    fi
  fi
fi

### Check for UMFPACK library.

save_LIBS="$LIBS"
save_CPPFLAGS="$CPPFLAGS"
LIBS="$AMD_LDFLAGS $AMD_LIBS $BLAS_LIBS $FLIBS $LIBS"
CPPFLAGS="$AMD_CPPFLAGS $CPPFLAGS"
OCTAVE_CHECK_LIB([umfpack], UMFPACK,
  [UMFPACK not found.  This will result in some lack of functionality for sparse matrices.],
  [umfpack.h suitesparse/umfpack.h umfpack/umfpack.h],
  [umfpack${UMFPACK_TAG}get_determinant],
  [], [don't use UMFPACK, disable some sparse matrix functionality])
CPPFLAGS="$save_CPPFLAGS"
LIBS="$save_LIBS"

if test -z "$UMFPACK_LIBS"; then
  ## Invalidate the cache and try again with -lcblas.
  $as_unset ac_cv_lib_umfpack_umfpack${UMFPACK_TAG}get_determinant
  $as_unset octave_cv_lib_umfpack
  save_LIBS="$LIBS"
  LIBS="-lcblas $AMD_LDFLAGS $AMD_LIBS $BLAS_LIBS $FLIBS $LIBS"
  OCTAVE_CHECK_LIB([umfpack], UMFPACK,
    [UMFPACK not found.  This will result in some lack of functionality for sparse matrices.],
    [umfpack.h suitesparse/umfpack.h umfpack/umfpack.h],
    [umfpack${UMFPACK_TAG}get_determinant],
    [], [don't use UMFPACK, disable some sparse matrix functionality])
  if test -n "$UMFPACK_LIBS"; then
    UMFPACK_LIBS="$UMFPACK_LIBS -lcblas"
  fi
  LIBS="$save_LIBS"
fi

### Check for KLU library and header.

OCTAVE_CHECK_LIB(klu, KLU,
  [KLU library not found.  This will result in some lack of functionality for sparse matrices.],
  [klu.h suitesparse/klu.h klu/klu.h], [klu_solve],
  [], [don't use KLU library, disable some sparse matrix functionality])

## Restore LIBS after all suitesparse configuration tests
LIBS="$suitesparse_save_LIBS"

## Collections of flags.
dnl These are here instead of just in the Makefile.am file because we
dnl substitute some of them into other source files like mkoctfile.

## Order matters, at least on some systems (Cygwin, for example).

SPARSE_XCPPFLAGS="$CHOLMOD_CPPFLAGS $UMFPACK_CPPFLAGS $AMD_CPPFLAGS $CAMD_CPPFLAGS $COLAMD_CPPFLAGS $CCOLAMD_CPPFLAGS $CXSPARSE_CPPFLAGS $SPQR_CPPFLAGS"

SPARSE_XLDFLAGS="$CHOLMOD_LDFLAGS $UMFPACK_LDFLAGS $AMD_LDFLAGS $CAMD_LDFLAGS $COLAMD_LDFLAGS  $CCOLAMD_LDFLAGS $CXSPARSE_LDFLAGS $SPQR_LDFLAGS"

SPARSE_XLIBS="$CHOLMOD_LIBS $UMFPACK_LIBS $AMD_LIBS $CAMD_LIBS $COLAMD_LIBS $CCOLAMD_LIBS $CXSPARSE_LIBS $SUITESPARSECONFIG_LIBS $SPQR_LIBS"

AC_SUBST(SPARSE_XCPPFLAGS)
AC_SUBST(SPARSE_XLDFLAGS)
AC_SUBST(SPARSE_XLIBS)

## Check for UMFPACK separately split complex matrix and RHS.
if test -n "$UMFPACK_LIBS"; then
  save_CPPFLAGS="$CPPFLAGS"
  save_LDFLAGS="$LDFLAGS";
  save_LIBS="$LIBS";

  CPPFLAGS="$SPARSE_XCPPFLAGS $CPPFLAGS"
  LDFLAGS="$SPARSE_XLDFLAGS $LDFLAGS"
  LIBS="$SPARSE_XLIBS $LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS $xtra_libs"

  OCTAVE_UMFPACK_SEPARATE_SPLIT

  CPPFLAGS="$save_CPPFLAGS"
  LDFLAGS="$save_LDFLAGS"
  LIBS="$save_LIBS"
fi

### Check for SUNDIALS NVECTOR serial library and header.

OCTAVE_CHECK_LIB(sundials_nvecserial, [SUNDIALS NVECTOR],
  [SUNDIALS NVECTOR serial library not found.  The solvers ode15i and ode15s will be disabled.],
  [nvector_serial.h nvector/nvector_serial.h], [N_VNew_Serial],
  [C++], [don't use SUNDIALS NVECTOR library, disable solvers ode15i and ode15s])

### Check for SUNDIALS IDA library and header.

if test -n "$SUNDIALS_NVECSERIAL_LIBS"; then

  save_CPPFLAGS="$CPPFLAGS"
  save_LDFLAGS="$LDFLAGS"
  save_LIBS="$LIBS"
  LIBS="$SUNDIALS_NVECSERIAL_LIBS $KLU_LIBS $BLAS_LIBS $FLIBS $LIBS"
  LDFLAGS="$SUNDIALS_NVECSERIAL_LDFLAGS $KLU_LDFLAGS $BLAS_LDFLAGS $LDFLAGS"
  CPPFLAGS="$SUNDIALS_NVECSERIAL_CPPFLAGS $KLU_CPPFLAGS $BLAS_CPPFLAGS $CPPFLAGS"
  OCTAVE_CHECK_LIB(sundials_ida, [SUNDIALS IDA],
    [SUNDIALS IDA library not found.  The solvers ode15i and ode15s will be disabled.],
    [ida.h ida/ida.h], [IDAInit],
    [], [don't use SUNDIALS IDA library, disable solvers ode15i and ode15s])
  CPPFLAGS="$save_CPPFLAGS"
  LDFLAGS="$save_LDFLAGS"
  LIBS="$save_LIBS"
fi

### Check for SUNDIALS library features, some required, some optional.

if test -n "$SUNDIALS_IDA_LIBS" && test -n "$SUNDIALS_NVECSERIAL_LIBS"; then

  CPPFLAGS="$SUNDIALS_IDA_CPPFLAGS $SUNDIALS_NVECSERIAL_CPPFLAGS $KLU_CPPFLAGS $BLAS_CPPFLAGS $CPPFLAGS"
  LDFLAGS="$SUNDIALS_IDA_LDFLAGS $SUNDIALS_NVECSERIAL_LDFLAGS $KLU_LDFLAGS $BLAS_LDFLAGS $LDFLAGS"
  LIBS="$SUNDIALS_IDA_LIBS $SUNDIALS_NVECSERIAL_LIBS $KLU_LIBS $BLAS_LIBS $FLIBS $LIBS"
  if test -z "$warn_sundials_nvecserial" && test -z "$warn_sundials_ida"; then
    dnl Any of the following tests could determine that SUNDIALS is
    dnl incompatible and should be disabled.  In that event, they all populate
    dnl the same variable with appropriate warning messages, and further tests
    dnl should be skipped if a warning message has already been generated that
    dnl SUNDIALS is disabled.
    warn_sundials_disabled=
    if test -z "$warn_sundials_disabled"; then
      OCTAVE_CHECK_SUNDIALS_COMPATIBLE_API
    fi
    if test -z "$warn_sundials_disabled"; then
      OCTAVE_CHECK_SUNDIALS_SIZEOF_REALTYPE
    fi
    if test -z "$warn_sundials_disabled"; then
      OCTAVE_CHECK_SUNDIALS_SUNLINSOL_DENSE
    fi
    dnl The following tests determine whether certain optional features are
    dnl present in the SUNDIALS libraries, but will not disable using SUNDIALS.
    if test -z "$warn_sundials_disabled"; then
      OCTAVE_CHECK_SUNDIALS_SUNLINSOL_KLU
    fi
  fi
  CPPFLAGS="$save_CPPFLAGS"
  LDFLAGS="$save_LDFLAGS"
  LIBS="$save_LIBS"
fi

dnl Define this way instead of with an #if in oct-conf-post-private.h so that
dnl the build features script will get the correct value.
dnl
dnl FIXME: The test on the sunlinsol_klu.h header is a bit of a kluge.
dnl How can we do a better job here?  Do we need to disable sundials
dnl any tests fail, or can we fix __ode15__.cc so that it still partially
dnl works when some things are missing (for example, KLU)?
if test -n "$SUNDIALS_IDA_LIBS" && test -n "$SUNDIALS_NVECSERIAL_LIBS"; then
  if test "x$octave_cv_sundials_sunlinsol_dense" = xyes \
      && test "x$octave_cv_sundials_realtype_is_double" = xyes \
      && test "x$octave_have_sundials_compatible_api" = xyes; then
    AC_DEFINE(HAVE_SUNDIALS, 1, [Define to 1 if SUNDIALS is available.])

    ## Options needed to build with SUNDIALS and its dependencies.
    SUNDIALS_XCPPFLAGS="$SUNDIALS_IDA_CPPFLAGS $SUNDIALS_SUNLINSOLKLU_CPPFLAGS $SUNDIALS_NVECSERIAL_CPPFLAGS $KLU_CPPFLAGS"
    SUNDIALS_XLDFLAGS="$SUNDIALS_IDA_LDFLAGS $SUNDIALS_SUNLINSOLKLU_LDFLAGS $SUNDIALS_NVECSERIAL_LDFLAGS $KLU_LDFLAGS"
    SUNDIALS_XLIBS="$SUNDIALS_IDA_LIBS $SUNDIALS_SUNLINSOLKLU_LIBS $SUNDIALS_NVECSERIAL_LIBS $KLU_LIBS"
  else
    SUNDIALS_IDA_CPPFLAGS=
    SUNDIALS_IDA_LDFLAGS=
    SUNDIALS_IDA_LIBS=
    SUNDIALS_SUNLINSOLKLU_CPPFLAGS=
    SUNDIALS_SUNLINSOLKLU_LDFLAGS=
    SUNDIALS_SUNLINSOLKLU_LIBS=
    SUNDIALS_NVECSERIAL_CPPFLAGS=
    SUNDIALS_NVECSERIAL_LDFLAGS=
    SUNDIALS_NVECSERIAL_LIBS=
    SUNDIALS_XCPPFLAGS=
    SUNDIALS_XLDFLAGS=
    SUNDIALS_XLIBS=
    dnl Emit a fallback warning message in case SUNDIALS has been disabled for
    dnl some reason that hasn't already generated one of these known warnings.
    if test -z "$warn_sundials_nvecserial" && test -z "$warn_sundials_ida" \
        && test -z "$warn_sundials_disabled"; then
      warn_sundials_disabled="SUNDIALS libraries are missing some feature.  The solvers ode15i and ode15s will be disabled."
      OCTAVE_CONFIGURE_WARNING([warn_sundials_disabled])
    fi
  fi
fi

AC_SUBST(SUNDIALS_XCPPFLAGS)
AC_SUBST(SUNDIALS_XLDFLAGS)
AC_SUBST(SUNDIALS_XLIBS)

### Check for ARPACK library.

save_LIBS="$LIBS"
LIBS="$LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS"
OCTAVE_CHECK_LIB([arpack], ARPACK,
  [ARPACK not found.  The eigs function will be disabled.],
  [],
  [dseupd],
  [Fortran 77], [don't use the ARPACK library, disable eigs function],
  [warn_arpack=
   OCTAVE_CHECK_LIB_ARPACK_OK_1(
     [AC_DEFINE(HAVE_ARPACK, 1, [Define to 1 if ARPACK is available.])],
     [warn_arpack="ARPACK library found, but does not seem to work properly; disabling eigs function"])
   if test -n "$ARPACK_LIBS"; then
     OCTAVE_CHECK_LIB_ARPACK_OK_2([],
       [AC_MSG_WARN([ARPACK library found, but is buggy; upgrade library (>= v3.3.0) for better results])])
   fi
   ])
LIBS="$save_LIBS"

### Check for Java.

build_java=yes
AC_ARG_ENABLE([java],
  [AS_HELP_STRING([--disable-java],
    [disable Java interface])],
  [if test "$enableval" = no; then
     build_java=no
   fi],
  [])

AC_ARG_WITH([java-homedir],
  [AS_HELP_STRING([--with-java-homedir=DIR],
    [Java JDK directory in DIR])],
  [JAVA_HOMEDIR="$withval"])

AC_ARG_WITH([java-includedir],
  [AS_HELP_STRING([--with-java-includedir=DIR],
    [look for java include file <jni.h> in DIR])],
  [JAVA_CPPFLAGS="$withval"], [JAVA_CPPFLAGS=""])

AC_ARG_WITH([java-libdir],
  [AS_HELP_STRING([--with-java-libdir=DIR],
    [look for java library libjvm in DIR])],
  [JAVA_LDPATH="$withval"], [JAVA_LDPATH=""])

## Grab JAVA_HOME from environment variable if it exists
AC_ARG_VAR([JAVA_HOME], [path to Java JDK installation])
## But --with-java-homedir option overrides environment variable
if test -n "$JAVA_HOMEDIR"; then
  JAVA_HOME=$JAVA_HOMEDIR
fi
JAVA=
JAVAC=
JAR=
JAVA_LIBS=

dnl Fake loop so that "break" can be used to skip code blocks.
warn_java=
while test $build_java = yes
do
  ## Unset build_java.  Variable is set only if all configuration tests pass.
  build_java=no

  ## Warn if JAVA_HOME is unset.  It is *strongly* advised to specify JAVA_HOME.
  if test -z "$JAVA_HOME"; then
    warn_java_home="JAVA_HOME environment variable not initialized.  Auto-detection will proceed but is unreliable."
    OCTAVE_CONFIGURE_WARNING([warn_java_home])
  fi

  ## Search for a viable Java executable.
  if test -z "$JAVA_HOME"; then
    JAVA_PATH="$PATH"
  else
    JAVA_PATH="${JAVA_HOME}$PATH_SEPARATOR${JAVA_HOME}/jre/bin$PATH_SEPARATOR${JAVA_HOME}/bin$PATH_SEPARATOR${JAVA_HOME}/../bin$PATH_SEPARATOR${PATH}"
  fi
  AC_PATH_PROG(JAVA, java, [], [$JAVA_PATH])

  if test -z "$JAVA"; then
    warn_java="No Java executable found.  Octave will not be able to call Java methods."
    break
  fi

  if test -z "$JAVA_HOME"; then
    ## Find JAVA_HOME for JRE by running java and querying properties.
    JAVA_TMP_HOME=`"$JAVA" -classpath ${srcdir}/build-aux OctJavaQry JAVA_HOME`
    ## Strip directory back to top-level installation dir (JAVA_HOME for JDK).
    JAVA_HOME=`echo $JAVA_TMP_HOME | $SED -e 's|[[/\\]]bin[[/\\]]\?$||' | $SED -e 's|[[/\\]]jre[[/\\]]\?$||'`
  fi

  case $build_os in
    mingw* | msdosmsvc)
      ## Under Win32 platform, we want JAVA_HOME to be in MSYS format; that is,
      ## without colon and backslashes, as it is also used as path separator.
      ## Use quoted paths as Java may be installed in a path with whitespaces
      ## (e.g., C:\Program Files\Java\...).
      if test -n "$JAVA_HOME"; then
        JAVA_HOME=`cd "$JAVA_HOME" && pwd`
      fi
    ;;
  esac

  ## Amend search path for JAVAC and JAR.
  if test -z "$JAVA_HOME"; then
    JAVA_PATH="$PATH"
  else
    JAVA_PATH="${JAVA_HOME}$PATH_SEPARATOR${JAVA_HOME}/bin$PATH_SEPARATOR${JAVA_HOME}/../bin$PATH_SEPARATOR${PATH}"
  fi

  AC_PATH_PROG(JAVAC, javac, [], [$JAVA_PATH])
  AC_PATH_PROG(JAR, jar, [], [$JAVA_PATH])

  if test -z "$JAVAC" || test -z "$JAR"; then
    warn_java="No javac compiler or jar executable found.  Octave will not be able to call Java methods."
    break
  fi

  ## Check Java version is recent enough.
  AC_CACHE_CHECK([for Java version],
    [octave_cv_java_version],
    [octave_cv_java_version=[`"$JAVA" -version 2>&1 | $SED -n -e 's/^[^ ]* version[^0-9"]*"\([^"]*\)".*/\1/p'`]
  ])

  java_major=[`echo $octave_cv_java_version | $SED -e 's/^\([0-9][0-9]*\)\.\([0-9][0-9]*\)\..*$/\1/'`]
  java_minor=[`echo $octave_cv_java_version | $SED -e 's/^\([0-9][0-9]*\)\.\([0-9][0-9]*\)\..*$/\2/'`]

  if test $java_major -gt 1 || (test $java_major -eq 1 && test $java_minor -ge 5); then
    :  # Version is OK.  Do nothing.
  else
    warn_java="Java version is too old (< 1.5).  Octave will not be able to call Java methods."
    break
  fi

  ## At this point Win32 systems have enough configuration data.
  ## We assume that all appropriate variables (e.g., INCLUDE and LIB) already
  ## contain the required paths to compile and link against JDK.
  case $host_os in
    msdosmsvc)
      build_java=yes
      JAVA_LIBS=-ladvapi32
      AC_DEFINE(HAVE_JAVA, 1,
        [Define to 1 if Java is available and is at least version 1.5])
      break
    ;;
    mingw* | cygwin*)
      build_java=yes
      JAVA_LIBS=-ladvapi32
      if test $have_msvc = no; then
        if test -n "$JAVA_CPPFLAGS"; then
          JAVA_CPPFLAGS="-I\"${JAVA_CPPFLAGS}\" -I\"${JAVA_CPPFLAGS}/win32\""
        else
          JAVA_CPPFLAGS="-I\"${JAVA_HOME}/include\" -I\"${JAVA_HOME}/include/win32\""
        fi
      fi
      AC_DEFINE(HAVE_JAVA, 1,
        [Define to 1 if Java is available and is at least version 1.5])
      break
    ;;
  esac

  ## Determine which library filename to search for.
  case $host_os in
    darwin*)
      jvmlib=libjvm.dylib
    ;;
    *)
      jvmlib=libjvm.so
    ;;
  esac

  AC_MSG_CHECKING([for $jvmlib])

  AC_CACHE_VAL([octave_cv_java_ldpath],[
    if test -z "$JAVA_LDPATH"; then
      ## Run Java to try and determine library path to libjvm.so.
      JAVA_TMP_LDPATH=`$JAVA -classpath ${srcdir}/build-aux OctJavaQry JAVA_LDPATH`
      JAVA_TMP_LDPATH=`echo $JAVA_TMP_LDPATH | $SED -e "s/${PATH_SEPARATOR}/ /g"`
      for dir in $JAVA_TMP_LDPATH; do
        if test -f "$dir/$jvmlib"; then
          JAVA_LDPATH=$dir
          break
        fi
      done
    fi

    if test -z "$JAVA_LDPATH"; then
      ## Nothing found.  Try Java again using bootpath argument.
      JAVA_TMP_LDPATH=`$JAVA -classpath ${srcdir}/build-aux OctJavaQry JAVA_BOOTPATH`
      JAVA_TMP_LDPATH="${JAVA_TMP_LDPATH} ${JAVA_TMP_LDPATH}/client ${JAVA_TMP_LDPATH}/server"
      for dir in $JAVA_TMP_LDPATH; do
        if test -f "$dir/$jvmlib"; then
          JAVA_LDPATH=$dir
          break
        fi
      done
    fi

    if test -z "$JAVA_LDPATH"; then
      ## Java failed to find its own library path.  Guess wildly.
      JAVA_TMP_LDPATH=`ls -d $JAVA_HOME/jre/lib/*/client`
      JAVA_TMP_LDPATH="${JAVA_TMP_LDPATH} `ls -d $JAVA_HOME/jre/lib/*/server`"
      ## Add some paths that might work on Macs.
      JAVA_TMP_LDPATH="${JAVA_TMP_LDPATH} ${JAVA_HOME}/../Libraries ${JAVA_HOME}/Libraries"
      ## Add some paths that might work on MinGW
      JAVA_TMP_LDPATH="${JAVA_TMP_LDPATH} ${JAVA_HOME}/bin/client ${JAVA_HOME}/bin/server"
      for dir in $JAVA_TMP_LDPATH; do
        if test -f "$dir/$jvmlib"; then
          JAVA_LDPATH=$dir
          break
        fi
      done
    fi

    ## Verify value passed in option --with-java-libdir
    if test -n "$JAVA_LDPATH"; then
      if test -f "${JAVA_LDPATH}/$jvmlib"; then
        :  # libjvm found
      else
        JAVA_LDPATH=""
      fi
    fi

    octave_cv_java_ldpath=$JAVA_LDPATH
  ])
  JAVA_LDPATH=$octave_cv_java_ldpath
  if test -z "$JAVA_LDPATH"; then
    AC_MSG_RESULT([not found])
    warn_java="Library $jvmlib not found.  Octave will not be able to call Java methods."
    break
  else
    AC_MSG_RESULT([$JAVA_LDPATH])
  fi

  AC_MSG_CHECKING([for include file <jni.h>])

  ## Java and JVM found.  Set up flags.
  case $host_os in
    darwin*)
      if test -n "$JAVA_CPPFLAGS"; then
        JAVA_CPPFLAGS="-I${JAVA_CPPFLAGS} -I${JAVA_CPPFLAGS}/darwin"
      else
        JAVA_CPPFLAGS="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/darwin"
      fi
    ;;
    *)
      if test -n "$JAVA_CPPFLAGS"; then
        JAVA_CPPFLAGS="-I${JAVA_CPPFLAGS} -I${JAVA_CPPFLAGS}/linux"
      else
        JAVA_CPPFLAGS="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux"
      fi
    ;;
  esac

  ## Verify jni.h include file exists.
  AC_CACHE_VAL([octave_cv_java_have_jni],[
    JNI_PATH=`echo $JAVA_CPPFLAGS | $SED -e 's/-I//g'`
    octave_cv_java_have_jni=no
    for dir in $JNI_PATH; do
      if test -f "${dir}/jni.h"; then
        octave_cv_java_have_jni=yes
        octave_cv_java_jni_h_path=$dir
        break
      fi
    done
  ])
  if test $octave_cv_java_have_jni = yes; then
    AC_MSG_RESULT([$octave_cv_java_jni_h_path])
  else
    AC_MSG_RESULT([not found])
    warn_java="Include file <jni.h> not found.  Octave will not be able to call Java methods."
    break
  fi

  ## Passed all configuration tests.  A workable Java installation was found.
  build_java=yes
  AC_DEFINE(HAVE_JAVA, 1,
    [Define to 1 if Java is available and is at least version 1.5])
  break
done
if test -n "$warn_java"; then
  OCTAVE_CONFIGURE_WARNING([warn_java])
fi

AM_CONDITIONAL([AMCOND_HAVE_JAVA], [test $build_java = yes])
AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAR)
AC_SUBST(JAVA_CPPFLAGS)
AC_SUBST(JAVA_LIBS)
AC_DEFINE_UNQUOTED([JAVA_HOME], ["$JAVA_HOME"], [Java home (top-level installation dir)])
AC_DEFINE_UNQUOTED([JAVA_LDPATH], ["$JAVA_LDPATH"], [Java library path (libjvm)])

### Determine if documentation should and can be built.

ENABLE_DOCS=yes
AC_ARG_ENABLE([docs],
  [AS_HELP_STRING([--disable-docs], [don't build documentation files])],
  [if test "$enableval" = no; then
     ENABLE_DOCS=no
     warn_docs="building documentation disabled; make dist will fail."
     OCTAVE_CONFIGURE_WARNING([warn_docs])
   fi], [])

if test $ENABLE_DOCS = yes; then
  if test $opengl_graphics = no || test "$have_qt_opengl_offscreen" = no; then
    if test -n "$warn_gnuplot"; then
      ENABLE_DOCS=no
      warn_docs_graphics="building documentation disabled because no suitable graphics toolkit is available; make dist will fail."
      OCTAVE_CONFIGURE_WARNING([warn_docs_graphics])
    fi
  fi
  if test -n "$warn_makeinfo"; then
    ENABLE_DOCS=no
    warn_docs_makeinfo="building documentation disabled because makeinfo was not found or does not support @sortas; make dist will fail."
    OCTAVE_CONFIGURE_WARNING([warn_docs_makeinfo])
  fi
fi
if test $ENABLE_DOCS = yes; then
  AC_DEFINE(ENABLE_DOCS, 1,
    [Define to 1 to build Octave documentation files.])
fi

AM_CONDITIONAL([AMCOND_BUILD_DOCS], [test $ENABLE_DOCS = yes])

AM_CONDITIONAL([AMCOND_BUILD_QT_DOCS],
               [test $ENABLE_DOCS = yes && test $build_qt_gui = yes])

OCTAVE_RELOCATE_ALL=no
AC_ARG_ENABLE([relocate-all],
  [AS_HELP_STRING([--enable-relocate-all],
    [assume that all dependencies that are installed under the same prefix are relocated when Octave is relocated])],
  [if test "$enableval" = yes; then OCTAVE_RELOCATE_ALL=yes; fi], [])
AC_SUBST(OCTAVE_RELOCATE_ALL)
if test $OCTAVE_RELOCATE_ALL = yes; then
  AC_DEFINE(OCTAVE_REPLACE_PREFIX, 1, [Replace prefix in mkoctfile flags.])
fi
AM_CONDITIONAL([AMCOND_RELOCATE_ALL], [test $OCTAVE_RELOCATE_ALL = yes])

### Determine whether Mercurial ID should be embedded in library binaries.

ENABLE_HG_ID=yes
AC_ARG_ENABLE([hg-id],
  [AS_HELP_STRING([--disable-hg-id],
    [disable embedding of hg id in libraries])],
  [if test "$enableval" = no; then ENABLE_HG_ID=no; fi], [])
AM_CONDITIONAL([AMCOND_ENABLE_HG_ID], [test $ENABLE_HG_ID = yes])

### Determine whether to install build logs with Octave.

install_build_logs=no
AC_ARG_ENABLE([install-build-logs],
  [AS_HELP_STRING([--enable-install-build-logs],
    [install build logs (i.e., config.log) with Octave])],
  [if test "$enableval" = yes; then install_build_logs=yes; fi])
AM_CONDITIONAL([AMCOND_INSTALL_BUILD_LOGS], [test $install_build_logs = yes])

### Determine whether libraries should be linked with visibility attributes.

ENABLE_LIB_VISIBILITY_FLAGS=no
AC_ARG_ENABLE(lib-visibility-flags,
  [AS_HELP_STRING([--enable-lib-visibility-flags],
    [build libraries with visibility flags (export only symbols from API)])],
  [case $enableval in
     yes) ENABLE_LIB_VISIBILITY_FLAGS=yes ;;
     no) ENABLE_LIB_VISIBILITY_FLAGS=no ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-lib-visibility-flags]) ;;
   esac])

case $host_os in
  msdosmsvc | mingw* | cygwin*)
    # Assume that symbol export is working on that platform
  ;;
  *)
    if test $ENABLE_LIB_VISIBILITY_FLAGS = yes && test $HAVE_VISIBILITY = 0; then
      AC_MSG_NOTICE([cannot build with visibility flags on this platform])
      ENABLE_LIB_VISIBILITY_FLAGS=no
    fi
  ;;
esac

if test $ENABLE_LIB_VISIBILITY_FLAGS = yes; then
  AC_DEFINE(OCTAVE_ENABLE_LIB_VISIBILITY_FLAGS, 1,
    [Define to 1 if building libraries with visibility flags])
else
  case $host_os in
    msdosmsvc | mingw* | cygwin*)
      LDFLAGS="$LDFLAGS -Wl,--export-all-symbols"
    ;;
  esac
fi

AM_CONDITIONAL([AMCOND_LIB_VISIBILITY_FLAGS],
  [test $ENABLE_LIB_VISIBILITY_FLAGS = yes])

### Set flags for symbol visibility.

if test $ENABLE_LIB_VISIBILITY_FLAGS = yes; then
  EXTERNAL_DLL_DEFS="-DEXTERNAL_DLL"
  OCTAVE_DLL_DEFS="-DOCTAVE_DLL"
  OCTINTERP_DLL_DEFS="-DOCTINTERP_DLL"
  OCTGUI_DLL_DEFS="-DOCTGUI_DLL"
  OCTGRAPHICS_DLL_DEFS="-DOCTGRAPHICS_DLL"
else
  EXTERNAL_DLL_DEFS=
  OCTAVE_DLL_DEFS=
  OCTINTERP_DLL_DEFS=
  OCTGUI_DLL_DEFS=
  OCTGRAPHICS_DLL_DEFS=
fi
AC_MSG_NOTICE([defining EXTERNAL_DLL_DEFS to be $EXTERNAL_DLL_DEFS])
AC_MSG_NOTICE([defining OCTAVE_DLL_DEFS to be $OCTAVE_DLL_DEFS])
AC_MSG_NOTICE([defining OCTINTERP_DLL_DEFS to be $OCTINTERP_DLL_DEFS])
AC_MSG_NOTICE([defining OCTGUI_DLL_DEFS to be $OCTGUI_DLL_DEFS])
AC_MSG_NOTICE([defining OCTGRAPHICS_DLL_DEFS to be $OCTGRAPHICS_DLL_DEFS])

AC_SUBST(EXTERNAL_DLL_DEFS)
AC_SUBST(OCTAVE_DLL_DEFS)
AC_SUBST(OCTINTERP_DLL_DEFS)
AC_SUBST(OCTGUI_DLL_DEFS)
AC_SUBST(OCTGRAPHICS_DLL_DEFS)

### Add extra compiler flags now that feature testing is complete.

## Add warning flags

dnl Don't add -Wshadow for GCC 4.x.
case "$GCC_VERSION" in
  *4*) ;;
  *)   GCC_WSHADOW_OPTION=-Wshadow ;;
esac

GCC_EXTRA_FLAGS="-Wall -W $GCC_WSHADOW_OPTION -Wformat -Wpointer-arith -Wmissing-prototypes -Wstrict-prototypes -Wwrite-strings -Wcast-align -Wcast-qual"
GXX_EXTRA_FLAGS="-Wall -W $GCC_WSHADOW_OPTION -Woverloaded-virtual -Wold-style-cast -Wformat -Wpointer-arith -Wwrite-strings -Wcast-align -Wcast-qual"

try_extra_warning_flags=yes
AC_ARG_ENABLE([extra-warning-flags],
  [AS_HELP_STRING([--disable-extra-warning-flags],
    [don't add -Wall, -W, -Wformat, -Wold-style-cast, and other warning options to CFLAGS and CXXFLAGS])],
  [if test "$enableval" = no; then
     try_extra_warning_flags=no
   fi],
  [])

if test $try_extra_warning_flags = yes; then
  for flag in $GCC_EXTRA_FLAGS; do
    OCTAVE_CC_FLAG([$flag], [
      WARN_CFLAGS="$WARN_CFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_CFLAGS])])
  done
  for flag in $GXX_EXTRA_FLAGS; do
    OCTAVE_CXX_FLAG([$flag], [
      WARN_CXXFLAGS="$WARN_CXXFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_CXXFLAGS])])
  done
fi

## Add strict warning flags
GCC_STRICT_FLAGS="-Wconversion"
GXX_STRICT_FLAGS="-Wconversion -Weffc++"

try_strict_warning_flags=no
AC_ARG_ENABLE([strict-warning-flags],
  [AS_HELP_STRING([--enable-strict-warning-flags],
    [add extra strict warning options to CFLAGS and CXXFLAGS])],
  [if test "$enableval" = yes; then
     try_strict_warning_flags=yes
   fi],
  [])

if test $try_strict_warning_flags = yes; then
  for flag in $GCC_STRICT_FLAGS; do
    OCTAVE_CC_FLAG([$flag], [
      WARN_CFLAGS="$WARN_CFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_CFLAGS])])
  done
  for flag in $GXX_STRICT_FLAGS; do
    OCTAVE_CXX_FLAG([$flag], [
      WARN_CXXFLAGS="$WARN_CXXFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_CXXFLAGS])])
  done
fi

## Add sanitizer flags
GCC_ADDRESS_SANITIZER_FLAGS="-fsanitize=address -fno-omit-frame-pointer"
GXX_ADDRESS_SANITIZER_FLAGS="-fsanitize=address -fno-omit-frame-pointer"
LD_ADDRESS_SANITIZER_FLAGS="-fsanitize=address"

try_address_sanitizer_flags=no
AC_ARG_ENABLE([address-sanitizer-flags],
  [AS_HELP_STRING([--enable-address-sanitizer-flags],
    [add extra -fsanitize=address and -fno-omit-frame-pointer options to CFLAGS, CXXFLAGS, and LDFLAGS])],
  [if test "$enableval" = yes; then
     try_address_sanitizer_flags=yes
   fi],
  [])

if test $try_address_sanitizer_flags = yes; then
  ADDRESS_SANITIZER_OPTIONS="symbolize=1"
  AC_SUBST(ADDRESS_SANITIZER_OPTIONS)
fi

AM_CONDITIONAL([AMCOND_ADDRESS_SANITIZER_ENABLED],
  [test $try_address_sanitizer_flags = yes])

ADDRESS_SANITIZER_ENABLED=no
if test $try_address_sanitizer_flags = yes; then
  ADDRESS_SANITIZER_ENABLED=yes
  for flag in $GCC_ADDRESS_SANITIZER_FLAGS; do
    OCTAVE_CC_FLAG([$flag], [
      WARN_CFLAGS="$WARN_CFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_CFLAGS])])
  done
  for flag in $GXX_ADDRESS_SANITIZER_FLAGS; do
    OCTAVE_CXX_FLAG([$flag], [
      WARN_CXXFLAGS="$WARN_CXXFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_CXXFLAGS])])
  done
  dnl FIXME: do we really need an LD-specific test, or is this good enough?
  for flag in $LD_ADDRESS_SANITIZER_FLAGS; do
    OCTAVE_CC_FLAG([$flag], [
      WARN_LDFLAGS="$WARN_LDFLAGS $flag";
      AC_MSG_RESULT([adding $flag to WARN_LDFLAGS])])
  done
fi
AC_SUBST(ADDRESS_SANITIZER_ENABLED)

AC_SUBST(WARN_CFLAGS)
AC_SUBST(WARN_CXXFLAGS)
AC_SUBST(WARN_LDFLAGS)

### Check for GCC-specific pragmas to control warnings.

AC_LANG_PUSH(C++)
AC_CACHE_CHECK([for @%:@pragma GCC diagnostic push/pop/ignored],
  [octave_cv_gcc_has_pragma_GCC_diagnostic],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
  int three = (int) 3.1415926;
#pragma GCC diagnostic pop
    ]])],
    [octave_cv_gcc_has_pragma_GCC_diagnostic=yes],
    [octave_cv_gcc_has_pragma_GCC_diagnostic=no])],
  ])
AC_LANG_POP(C++)

if test $octave_cv_gcc_has_pragma_GCC_diagnostic = yes; then
  AC_DEFINE(HAVE_PRAGMA_GCC_DIAGNOSTIC, 1,
    [Define to 1 if compiler supports @%:@pragma GCC diagnostic ...])
fi

AC_CACHE_CHECK([for @%:@pragma GCC visibility push/pop],
  [octave_cv_gcc_has_pragma_GCC_visiblity],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
#pragma GCC visibility push(default)
  int three = 3;
#pragma GCC visiblity pop(default)
    ]])],
    [octave_cv_gcc_has_pragma_GCC_visiblity=yes],
    [octave_cv_gcc_has_pragma_GCC_visiblity=no])],
  ])

if test $octave_cv_gcc_has_pragma_GCC_visiblity = yes; then
  AC_DEFINE(HAVE_PRAGMA_GCC_VISIBILITY, 1,
    [Define to 1 if compiler supports @%:@pragma GCC visiblity ...])
fi

## Check for one unusual warning used in pragmas that is not always available
if test $octave_cv_gcc_has_pragma_GCC_diagnostic = yes; then
  OCTAVE_CC_FLAG([-Wimplicit-fallthrough],
    [AC_DEFINE(HAVE_WARN_IMPLICIT_FALLTHROUGH, 1,
      [Define to 1 if compiler supports -Wimplicit-fallthrough warning])])
fi

### Recommendations from the gnulib bootstrap script.

GNULIB_LINK_DEPS="$FREXPF_LIBM $FREXP_LIBM $GETHOSTNAME_LIB $LIBSOCKET $LIB_CLOCK_GETTIME $LIB_CRYPTO $LIB_GETLOGIN $LIB_NANOSLEEP $LIB_SELECT $LTLIBICONV $LTLIBINTL $LTLIBTHREAD $INTL_MACOSX_LIBS"

## FIXME: This is a kluge to transform $libdir/libiconv.dll.a to -liconv.
## It would probably be better to fix gnulib to not give us an absolute
## file name...

case $host_os in
  mingw*)
    ## Don't use $libdir here because that's defined to be
    ## ${exec_prefix}/lib and at this point, ${exec_prefix} is "NONE"!
    tmp_libiconv_dll="$LIBICONV_PREFIX/lib/libiconv.dll.a"
    MKOCTFILE_LIBICONV=`echo $LIBICONV | sed "s|$tmp_libiconv_dll|-liconv|"`
  ;;
  *)
    MKOCTFILE_LIBICONV="$LIBICONV"
  ;;
esac

MKOCTFILE_GNULIB_LINK_DEPS="$FREXPF_LIBM $FREXP_LIBM $GETHOSTNAME_LIB $LIBSOCKET $LIB_CLOCK_GETTIME $LIB_CRYPTO $LIB_GETLOGIN $LIB_NANOSLEEP $LIB_SELECT $MKOCTFILE_LIBICONV $LIBINTL $LIBTHREAD $INTL_MACOSX_LIBS"

AC_SUBST(GNULIB_LINK_DEPS)

### Set variables for link dependencies and options.

BASE_LIBOCTAVE_LINK_DEPS="$CURL_LIBS $SPARSE_XLIBS $ARPACK_LIBS $QRUPDATE_LIBS $FFTW_XLIBS $LAPACK_LIBS $BLAS_LIBS $LTLIBREADLINE $LIBGLOB $PCRE_LIBS $DL_LIBS $PTHREAD_LIBS $FLIBS $LIBS"

LIBOCTAVE_LINK_DEPS="$BASE_LIBOCTAVE_LINK_DEPS $GNULIB_LINK_DEPS"

MKOCTFILE_LIBOCTAVE_LINK_DEPS="$BASE_LIBOCTAVE_LINK_DEPS $MKOCTFILE_GNULIB_LINK_DEPS"

LIBOCTAVE_LINK_OPTS="$CURL_LDFLAGS $SPARSE_XLDFLAGS $ARPACK_LDFLAGS $QRUPDATE_LDFLAGS $FFTW_XLDFLAGS $PCRE_LDFLAGS"

AC_SUBST(LIBOCTAVE_LINK_DEPS)
AC_SUBST(LIBOCTAVE_LINK_OPTS)

LIBOCTINTERP_LINK_DEPS="$FT2_LIBS $HDF5_LIBS $MAGICK_LIBS $Z_LIBS $SPARSE_XLIBS $FFTW_XLIBS $OPENGL_LIBS $FONTCONFIG_LIBS $FREETYPE_LIBS $X11_LIBS $CARBON_LIBS $GL2PS_LIBS $JAVA_LIBS $LAPACK_LIBS"

LIBOCTINTERP_LINK_OPTS="$FT2_LDFLAGS $HDF5_LDFLAGS $MAGICK_LDFLAGS $Z_LDFLAGS $SPARSE_XLDFLAGS $FFTW_XLDFLAGS"

OCTAVE_LINK_DEPS=""
OCTAVE_LINK_OPTS=""

OCT_LINK_DEPS=""
OCT_GUI_LINK_DEPS=""

OCT_LINK_OPTS="$LDFLAGS"
OCT_GUI_LINK_OPTS="$LDFLAGS"

if test $link_all_deps = yes; then
  AC_DEFINE(OCTAVE_LINK_ALL_DEPS, 1, [Link with all dependencies.])

  OCT_LINK_DEPS="libinterp/liboctinterp.la liboctave/liboctave.la"
  OCT_GUI_LINK_DEPS="libgui/liboctgui.la libinterp/liboctinterp.la liboctave/liboctave.la"

  MKOCTFILE_OCTAVE_LINK_DEPS="$LIBOCTINTERP_LINK_DEPS $MKOCTFILE_LIBOCTAVE_LINK_DEPS"
  MKOCTFILE_OCT_LINK_DEPS="$LIBOCTINTERP $LIBOCTAVE"

  LIBOCTINTERP_LINK_DEPS="$LIBOCTINTERP_LINK_DEPS $LIBOCTAVE_LINK_DEPS"
  LIBOCTINTERP_LINK_OPTS="$LIBOCTINTERP_LINK_OPTS $LIBOCTAVE_LINK_OPTS"

  OCTAVE_LINK_DEPS="$LIBOCTINTERP_LINK_DEPS"
  OCTAVE_LINK_OPTS="$LIBOCTINTERP_LINK_OPTS"
else
  case $host_os in
    mingw* | msdosmsvc)
      OCT_LINK_DEPS="libinterp/liboctinterp.la liboctave/liboctave.la"
      OCT_GUI_LINK_DEPS="libgui/liboctgui.la libinterp/liboctinterp.la liboctave/liboctave.la"
    ;;
  esac
fi

AC_SUBST(LIBOCTINTERP_LINK_DEPS)
AC_SUBST(LIBOCTINTERP_LINK_OPTS)

AC_SUBST(OCTAVE_LINK_DEPS)
AC_SUBST(OCTAVE_LINK_OPTS)

AC_SUBST(OCT_LINK_DEPS)
AC_SUBST(OCT_LINK_OPTS)

AC_SUBST(OCT_GUI_LINK_DEPS)
AC_SUBST(OCT_GUI_LINK_OPTS)

AC_SUBST(MKOCTFILE_OCTAVE_LINK_DEPS)
AC_SUBST(MKOCTFILE_OCT_LINK_DEPS)

LIBOCTGUI_LINK_DEPS=""
LIBOCTGUI_LINK_OPTS=""

OCTAVE_GUI_LINK_DEPS=""
OCTAVE_GUI_LINK_OPTS=""

if test $build_qt_gui = yes; then
  LIBOCTGUI_LINK_DEPS="$QT_LIBS $QT_OPENGL_LIBS"
  LIBOCTGUI_LINK_OPTS="$QT_LDFLAGS $QT_OPENGL_LDFLAGS"

  if test $link_all_deps = yes || test -n "$QT_LDFLAGS"; then
    LIBOCTGUI_LINK_DEPS="$LIBOCTGUI_LINK_DEPS $LIBOCTINTERP_LINK_DEPS"
    LIBOCTGUI_LINK_OPTS="$LIBOCTGUI_LINK_OPTS $LIBOCTINTERP_LINK_OPTS"

    OCTAVE_GUI_LINK_DEPS="$OCTAVE_GUI_LINK_DEPS $LIBOCTGUI_LINK_DEPS"
    OCTAVE_GUI_LINK_OPTS="$OCTAVE_GUI_LINK_OPTS $LIBOCTGUI_LINK_OPTS"
  fi
fi

AC_SUBST(LIBOCTGUI_LINK_DEPS)
AC_SUBST(LIBOCTGUI_LINK_OPTS)

AC_SUBST(OCTAVE_GUI_LINK_DEPS)
AC_SUBST(OCTAVE_GUI_LINK_OPTS)

##############################################################################
### Configuration tests done.

### Restore original compilation flags.
CFLAGS="$original_octave_configure_CFLAGS"
CXXFLAGS="$original_octave_configure_CXXFLAGS"

### Add footer to config.h

AH_BOTTOM([#include "oct-conf-post-private.h"])
AH_BOTTOM([#include "oct-conf-post-public.h"])

### Make all AC_DEFINES available to testif feature of test.m function.
dnl This must reside at the bottom of configure.ac after all AC_DEFINES
dnl have been made.

AC_OUTPUT_MAKE_DEFS

AC_CONFIG_COMMANDS_PRE([
  ## Fully expand all directory variables.  These are normally left
  ## unexpanded (as explained and recommended by the autoconf manual).
  ## But we store configuration info in files and expect whatever
  ## is set at configure time to stay the same when Make is executed.
  ## Doing things like
  ##
  ##   configure --prefix=/some/where
  ##   make --prefix=/some/where/else
  ##
  ## won't work properly and is not allowed when building Octave.

  eval prefix="\"$prefix\""
  eval exec_prefix="\"$exec_prefix\""
  eval bindir="\"$bindir\""
  eval sbindir="\"$sbindir\""
  eval libexecdir="\"$libexecdir\""
  eval datarootdir="\"$datarootdir\""
  eval datadir="\"$datadir\""
  eval sysconfdir="\"$sysconfdir\""
  eval sharedstatedir="\"$sharedstatedir\""
  eval localstatedir="\"$localstatedir\""
  eval runstatedir="\"$runstatedir\""
  eval includedir="\"$includedir\""
  eval oldincludedir="\"$oldincludedir\""
  eval docdir="\"$docdir\""
  eval infodir="\"$infodir\""
  eval htmldir="\"$htmldir\""
  eval dvidir="\"$dvidir\""
  eval pdfdir="\"$pdfdir\""
  eval psdir="\"$psdir\""
  eval libdir="\"$libdir\""
  eval localedir="\"$localedir\""
  eval mandir="\"$mandir\""

  version="$OCTAVE_VERSION"
  api_version="$OCTAVE_API_VERSION"

  eval octlibdir="\"$octlibdir\""
  eval archlibdir="\"$archlibdir\""
  eval localarchlibdir="\"$localarchlibdir\""
  eval localapiarchlibdir="\"$localapiarchlibdir\""
  eval localverarchlibdir="\"$localverarchlibdir\""
  eval octfiledir="\"$octfiledir\""
  eval localoctfiledir="\"$localoctfiledir\""
  eval localapioctfiledir="\"$localapioctfiledir\""
  eval localveroctfiledir="\"$localveroctfiledir\""
  eval octincludedir="\"$octincludedir\""
  eval fcnfiledir="\"$fcnfiledir\""
  eval localfcnfiledir="\"$localfcnfiledir\""
  eval localapifcnfiledir="\"$localapifcnfiledir\""
  eval localverfcnfiledir="\"$localverfcnfiledir\""
  eval octetcdir="\"$octetcdir\""
  eval octlocaledir="\"$octlocaledir\""
  eval doc_cache_file="\"$doc_cache_file\""
  eval octtestsdir="\"$octtestsdir\""
  eval texi_macros_file="\"$texi_macros_file\""
  eval imagedir="\"$imagedir\""
  eval octdatadir="\"$octdatadir\""
  eval octdocdir="\"$octdocdir\""
  eval octfontsdir="\"$octfontsdir\""
  eval startupfiledir="\"$startupfiledir\""
  eval localstartupfiledir="\"$localstartupfiledir\""
  eval man1dir="\"$man1dir\""
  eval man1ext="\"$man1ext\""
  eval infofile="\"$infofile\""

  ## Also expand MKOCTFILE_DL_LDFLAGS, because it may depend on the values
  ## of variables expanded above.

  eval MKOCTFILE_DL_LDFLAGS="\"$MKOCTFILE_DL_LDFLAGS\""
])

### Do substitutions in Makefiles and other files that hold configuration info.

AUTOCONF_SUBST_VARS="`echo $ac_subst_vars`"

AC_SUBST(AUTOCONF_SUBST_VARS)
AC_SUBST(ac_config_files)
AC_SUBST(ac_config_headers)

AC_CONFIG_FILES([
  Makefile
  build-aux/check-subst-vars.sh:build-aux/check-subst-vars.in.sh
  doc/doxyhtml/Doxyfile
  libgnu/Makefile
  test/Makefile
  liboctave/octave.pc:liboctave/octave.in.pc
  libinterp/octinterp.pc:libinterp/octinterp.in.pc])

dnl We use a .in.h file for oct-conf-post-private.h and
dnl oct-conf-post-public.h simply to copy it to the build tree
dnl so that we don't have to add the -I${top_srcdir} to any CPPFLAGS variables.

AC_CONFIG_FILES([
  oct-conf-post-private.h-tmp:oct-conf-post-private.in.h
  oct-conf-post-public.h-tmp:oct-conf-post-public.in.h])

AC_CONFIG_COMMANDS([oct-conf-post-private.h],
  [$SHELL $srcdir/build-aux/move-if-change oct-conf-post-private.h-tmp oct-conf-post-private.h])

AC_CONFIG_COMMANDS([oct-conf-post-public.h],
  [$SHELL $srcdir/build-aux/move-if-change oct-conf-post-public.h-tmp oct-conf-post-public.h])

OCTAVE_CONFIG_MOVE_IF_CHANGE_FILES([
  liboctave/external/mk-f77-def.sh
  liboctave/mk-version-h.sh
  libinterp/corefcn/mk-mxtypes-h.sh
  build-aux/subst-config-vals.sh
  build-aux/subst-cross-config-vals.sh
  build-aux/subst-script-vals.sh])

AC_OUTPUT

### Print a summary of the build configuration.

AC_MSG_NOTICE([

Octave is now configured for $canonical_host_type

  Source directory:              $srcdir
  Installation prefix:           $prefix
  C compiler:                    $CC $XTRA_CFLAGS $WARN_CFLAGS $CFLAGS
  C++ compiler:                  $CXX $XTRA_CXXFLAGS $WARN_CXXFLAGS $CXXFLAGS
  Fortran compiler:              $F77 $FFLAGS
  CPPFLAGS:                      $CPPFLAGS
  Fortran libraries:             $FLIBS
  Lex libraries:                 $LEXLIB
  LIBS:                          $LIBS
  LDFLAGS:                       $LDFLAGS
  Extra LDFLAGS:                 $WARN_LDFLAGS

  AMD CPPFLAGS:                  $AMD_CPPFLAGS
  AMD LDFLAGS:                   $AMD_LDFLAGS
  AMD libraries:                 $AMD_LIBS
  ARPACK CPPFLAGS:               $ARPACK_CPPFLAGS
  ARPACK LDFLAGS:                $ARPACK_LDFLAGS
  ARPACK libraries:              $ARPACK_LIBS
  BLAS libraries:                $BLAS_LIBS
  BZ2 CPPFLAGS:                  $BZ2_CPPFLAGS
  BZ2 LDFLAGS:                   $BZ2_LDFLAGS
  BZ2 libraries:                 $BZ2_LIBS
  CAMD CPPFLAGS:                 $CAMD_CPPFLAGS
  CAMD LDFLAGS:                  $CAMD_LDFLAGS
  CAMD libraries:                $CAMD_LIBS
  CARBON libraries:              $CARBON_LIBS
  CCOLAMD CPPFLAGS:              $CCOLAMD_CPPFLAGS
  CCOLAMD LDFLAGS:               $CCOLAMD_LDFLAGS
  CCOLAMD libraries:             $CCOLAMD_LIBS
  CHOLMOD CPPFLAGS:              $CHOLMOD_CPPFLAGS
  CHOLMOD LDFLAGS:               $CHOLMOD_LDFLAGS
  CHOLMOD libraries:             $CHOLMOD_LIBS
  COLAMD CPPFLAGS:               $COLAMD_CPPFLAGS
  COLAMD LDFLAGS:                $COLAMD_LDFLAGS
  COLAMD libraries:              $COLAMD_LIBS
  CURL CPPFLAGS:                 $CURL_CPPFLAGS
  CURL LDFLAGS:                  $CURL_LDFLAGS
  CURL libraries:                $CURL_LIBS
  CXSPARSE CPPFLAGS:             $CXSPARSE_CPPFLAGS
  CXSPARSE LDFLAGS:              $CXSPARSE_LDFLAGS
  CXSPARSE libraries:            $CXSPARSE_LIBS
  DL libraries:                  $DL_LIBS
  FFTW3 CPPFLAGS:                $FFTW3_CPPFLAGS
  FFTW3 LDFLAGS:                 $FFTW3_LDFLAGS
  FFTW3 libraries:               $FFTW3_LIBS
  FFTW3F CPPFLAGS:               $FFTW3F_CPPFLAGS
  FFTW3F LDFLAGS:                $FFTW3F_LDFLAGS
  FFTW3F libraries:              $FFTW3F_LIBS
  FLTK CPPFLAGS:                 $FLTK_CPPFLAGS
  FLTK LDFLAGS:                  $FLTK_LDFLAGS
  FLTK libraries:                $FLTK_LIBS
  fontconfig CPPFLAGS:           $FONTCONFIG_CPPFLAGS
  fontconfig libraries:          $FONTCONFIG_LIBS
  FreeType2 CPPFLAGS:            $FT2_CPPFLAGS
  FreeType2 libraries:           $FT2_LIBS
  GLPK CPPFLAGS:                 $GLPK_CPPFLAGS
  GLPK LDFLAGS:                  $GLPK_LDFLAGS
  GLPK libraries:                $GLPK_LIBS
  HDF5 CPPFLAGS:                 $HDF5_CPPFLAGS
  HDF5 LDFLAGS:                  $HDF5_LDFLAGS
  HDF5 libraries:                $HDF5_LIBS
  Java home:                     $JAVA_HOME
  Java JVM path:                 $JAVA_LDPATH
  Java CPPFLAGS:                 $JAVA_CPPFLAGS
  Java libraries:                $JAVA_LIBS
  KLU CPPFLAGS:                  $KLU_CPPFLAGS
  KLU LDFLAGS:                   $KLU_LDFLAGS
  KLU libraries:                 $KLU_LIBS
  LAPACK libraries:              $LAPACK_LIBS
  Magick++ CPPFLAGS:             $MAGICK_CPPFLAGS
  Magick++ LDFLAGS:              $MAGICK_LDFLAGS
  Magick++ libraries:            $MAGICK_LIBS
  OpenGL libraries:              $OPENGL_LIBS
  PCRE CPPFLAGS:                 $PCRE_CPPFLAGS
  PCRE LDFLAGS:                  $PCRE_LDFLAGS
  PCRE libraries:                $PCRE_LIBS
  PortAudio CPPFLAGS:            $PORTAUDIO_CPPFLAGS
  PortAudio LDFLAGS:             $PORTAUDIO_LDFLAGS
  PortAudio libraries:           $PORTAUDIO_LIBS
  PTHREAD flags:                 $PTHREAD_CFLAGS
  PTHREAD libraries:             $PTHREAD_LIBS
  QHULL CPPFLAGS:                $QHULL_CPPFLAGS
  QHULL LDFLAGS:                 $QHULL_LDFLAGS
  QHULL libraries:               $QHULL_LIBS
  QRUPDATE CPPFLAGS:             $QRUPDATE_CPPFLAGS
  QRUPDATE LDFLAGS:              $QRUPDATE_LDFLAGS
  QRUPDATE libraries:            $QRUPDATE_LIBS
  Qt CPPFLAGS:                   $QT_CPPFLAGS
  Qt LDFLAGS:                    $QT_LDFLAGS
  Qt GUI libraries:              $QT_LIBS
  Qt OpenGL libraries:           $QT_OPENGL_LIBS
  Qt moc:                        $MOC $MOCFLAGS
  Qt uic:                        $UIC $UICFLAGS
  Qt rcc:                        $RCC $RCCFLAGS
  Qt lrelease:                   $LRELEASE $LRELEASEFLAGS
  Qt qcollectiongenerator:       $QCOLLECTIONGENERATOR $QCOLLECTIONGENERATORFLAGS
  Qt qhelpgenerator:             $QHELPGENERATOR $QHELPGENERATORFLAGS
  READLINE libraries:            $LIBREADLINE
  Sndfile CPPFLAGS:              $SNDFILE_CPPFLAGS
  Sndfile LDFLAGS:               $SNDFILE_LDFLAGS
  Sndfile libraries:             $SNDFILE_LIBS
  SPQR CPPFLAGS:                 $SPQR_CPPFLAGS
  SPQR LDFLAGS:                  $SPQR_LDFLAGS
  SPQR libraries:                $SPQR_LIBS
  SuiteSparse config libraries:  $SUITESPARSECONFIG_LIBS
  SUNDIALS IDA CPPFLAGS:         $SUNDIALS_IDA_CPPFLAGS
  SUNDIALS IDA LDFLAGS:          $SUNDIALS_IDA_LDFLAGS
  SUNDIALS IDA libraries:        $SUNDIALS_IDA_LIBS
  SUNDIALS NVECTOR CPPFLAGS:     $SUNDIALS_NVECSERIAL_CPPFLAGS
  SUNDIALS NVECTOR LDFLAGS:      $SUNDIALS_NVECSERIAL_LDFLAGS
  SUNDIALS NVECTOR libraries:    $SUNDIALS_NVECSERIAL_LIBS
  SUNLINSOL KLU CPPFLAGS:        $SUNDIALS_SUNLINSOLKLU_CPPFLAGS
  SUNLINSOL KLU LDFLAGS:         $SUNDIALS_SUNLINSOLKLU_LDFLAGS
  SUNLINSOL KLU libraries:       $SUNDIALS_SUNLINSOLKLU_LIBS
  UMFPACK CPPFLAGS:              $UMFPACK_CPPFLAGS
  UMFPACK LDFLAGS:               $UMFPACK_LDFLAGS
  UMFPACK libraries:             $UMFPACK_LIBS
  X11 include flags:             $X11_INCFLAGS
  X11 libraries:                 $X11_LIBS
  Z CPPFLAGS:                    $Z_CPPFLAGS
  Z LDFLAGS:                     $Z_LDFLAGS
  Z libraries:                   $Z_LIBS

  Default pager:                 $DEFAULT_PAGER
  gnuplot:                       $GNUPLOT_BINARY

  Build Octave Qt GUI:                  $BUILD_QT_SUMMARY_MSG
  Build Java interface:                 $build_java
  Build static libraries:               $STATIC_LIBS
  Build shared libraries:               $SHARED_LIBS
  Dynamic Linking API:                  $DL_API_MSG
  Include support for GNU readline:     $USE_READLINE
  Use push parser in command line REPL: $ENABLE_COMMAND_LINE_PUSH_PARSER
  64-bit array dims and indexing:       $ENABLE_64
  64-bit BLAS array dims and indexing:  $HAVE_64_BIT_BLAS
  OpenMP SMP multithreading:            $ENABLE_OPENMP
  Truncate intermediate FP results:     $ENABLE_FLOAT_TRUNCATE
  Build cross tools:                    $cross_tools
  Build docs:                           $ENABLE_DOCS
])

warn_msg_printed=false

OCTAVE_CONFIGURE_WARNING_SUMMARY

if test $opengl_graphics = no; then
  AC_MSG_WARN([])
  AC_MSG_WARN([The libraries needed for OpenGL graphics were not found.])
  AC_MSG_WARN([Creating plots is still possible if gnuplot is installed.])
  warn_msg_printed=true
fi

if $warn_msg_printed; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([NOTE: Libraries or auxiliary programs may be skipped if they are not found])
  AC_MSG_NOTICE([NOTE: OR if they are missing required features on your system.])
fi

### End of configure.
