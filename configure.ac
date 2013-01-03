dnl Process this file with autoconf to produce a configure script.
dnl
dnl Copyright (C) 1993-2012 John W. Eaton
### 
### This file is part of Octave.
### 
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by the
### Free Software Foundation; either version 3 of the License, or (at
### your option) any later version.
### 
### Octave is distributed in the hope that it will be useful, but WITHOUT
### ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
### FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
### for more details.
### 
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, see
### <http://www.gnu.org/licenses/>.

AC_PREREQ([2.62])
AC_INIT([GNU Octave], [3.7.1], [http://octave.org/bugs.html], [octave])

dnl PACKAGE_VERSION is set by the AC_INIT VERSION arg
OCTAVE_VERSION="$PACKAGE_VERSION"
OCTAVE_API_VERSION_NUMBER="48"
OCTAVE_API_VERSION="api-v$OCTAVE_API_VERSION_NUMBER+"
OCTAVE_RELEASE_DATE="2013-01-02"
OCTAVE_COPYRIGHT="Copyright (C) 2012 John W. Eaton and others."
AC_SUBST(OCTAVE_VERSION)
AC_SUBST(OCTAVE_API_VERSION_NUMBER)
AC_SUBST(OCTAVE_API_VERSION)
AC_SUBST(OCTAVE_RELEASE_DATE)
AC_SUBST(OCTAVE_COPYRIGHT)

dnl FIXME: We should auto-insert the Mercurial changeset ID into the
dnl        AC_REVISION field whenever configure.ac is modified.
dnl AC_REVISION($Revision: 1.603 $)
AC_CONFIG_SRCDIR([libinterp/octave.cc])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 tar-ustar])

OCTAVE_CANONICAL_HOST

AC_DEFINE(OCTAVE_SOURCE, 1, [Define to 1 if this is Octave.])
  
AC_USE_SYSTEM_EXTENSIONS

### Make configure args available for other uses.

config_opts=$ac_configure_args
AC_SUBST(config_opts)

### Set default file locations

OCTAVE_SET_DEFAULT([octlibdir], '$(libdir)/octave/$(version)')
OCTAVE_SET_DEFAULT([archlibdir],
  '$(libexecdir)/octave/$(version)/exec/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([localarchlibdir],
  '$(libexecdir)/octave/site/exec/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([localapiarchlibdir],
  '$(libexecdir)/octave/$(api_version)/site/exec/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([localverarchlibdir],
  '$(libexecdir)/octave/$(version)/site/exec/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([octfiledir],
  '$(libdir)/octave/$(version)/oct/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([localoctfiledir],
  '$(libdir)/octave/site/oct/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([localapioctfiledir],
  '$(libdir)/octave/site/oct/$(api_version)/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([localveroctfiledir],
  '$(libdir)/octave/$(version)/site/oct/$(canonical_host_type)')
OCTAVE_SET_DEFAULT([octincludedir], '$(includedir)/octave-$(version)/octave')
OCTAVE_SET_DEFAULT([fcnfiledir], '$(datadir)/octave/$(version)/m')
OCTAVE_SET_DEFAULT([localfcnfiledir], '$(datadir)/octave/site/m')
OCTAVE_SET_DEFAULT([localapifcnfiledir],
  '$(datadir)/octave/site/$(api_version)/m')
OCTAVE_SET_DEFAULT([localverfcnfiledir], '$(datadir)/octave/$(version)/site/m')
OCTAVE_SET_DEFAULT([octetcdir], '$(datadir)/octave/$(version)/etc')
OCTAVE_SET_DEFAULT([doc_cache_file], '$(octetcdir)/doc-cache')
OCTAVE_SET_DEFAULT([texi_macros_file], '$(octetcdir)/macros.texi')
OCTAVE_SET_DEFAULT([imagedir], '$(datadir)/octave/$(version)/imagelib')
OCTAVE_SET_DEFAULT([man1dir], '$(mandir)/man1')
OCTAVE_SET_DEFAULT([man1ext], '.1')
OCTAVE_SET_DEFAULT([infofile], '$(infodir)/octave.info')

### Check for programs used in building, installing, and running Octave.

## Programs used in configuring Octave.
## Find pkg-config executable (sets $PKG_CONFIG)
PKG_PROG_PKG_CONFIG

## Programs used in Makefiles.
AC_PROG_AWK
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

## Programs used when installing Octave.
AC_PROG_LN_S
AC_PROG_MKDIR_P

AC_PROG_INSTALL
INSTALL_SCRIPT='${INSTALL}'
AC_SUBST(INSTALL_SCRIPT)

OCTAVE_PROG_DESKTOP_FILE_INSTALL

## Programs used when running Octave
OCTAVE_PROG_GHOSTSCRIPT
OCTAVE_PROG_GNUPLOT
OCTAVE_PROG_PAGER
OCTAVE_PROG_PYTHON

### Path separator.

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

### Define the path to the shell on the host system.  Most systems will
### ensure /bin/sh is the default shell so this can be safely ignored by
### almost everyone.  However, when building for Android, for example,
### this will need to be set.
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

### Enable bounds checking on element references within Octave's array and
### matrix classes.  This slows down some operations a bit, so it is turned off
### by default.

BOUNDS_CHECKING=false
AC_ARG_ENABLE([bounds-check],
  [AS_HELP_STRING([--enable-bounds-check],
    [enable bounds checking for indexing in internal array classes])],
  [if test "$enableval" = yes; then BOUNDS_CHECKING=true; fi], [])
if $BOUNDS_CHECKING; then
  AC_DEFINE(BOUNDS_CHECKING, 1, [Define to 1 to use internal bounds checking.])
fi

### Use Octave's built-in memory allocator rather than straightforward malloc.
### Disabled by default.

USE_OCTAVE_ALLOCATOR=false
AC_ARG_ENABLE([octave-allocator],
  [AS_HELP_STRING([--enable-octave-allocator],
    [use the obsolete octave_allocator class for many of Octave's objects (mostly octave_value types).  You probably do NOT want to enable this feature.])],
  [if test "$enableval" = yes; then USE_OCTAVE_ALLOCATOR=true; fi], [])
if $USE_OCTAVE_ALLOCATOR; then
  AC_DEFINE(USE_OCTAVE_ALLOCATOR, 1,
    [Define to 1 to use octave_allocator class.])
fi

### Use atomic operations for internal reference counting.  This is required
### for thread-safe behavior but incurs a significant slowdown, and is thus
### disabled by default.

USE_ATOMIC_REFCOUNT=false
AC_ARG_ENABLE([atomic-refcount],
  [AS_HELP_STRING([--enable-atomic-refcount],
    [use atomic operations for internal reference counting.  This is required for thread-safe behavior but does not by itself make Octave internals thread safe.])],
  [if test "$enableval" = yes; then USE_ATOMIC_REFCOUNT=true; fi], [])
if $USE_ATOMIC_REFCOUNT; then
  AC_DEFINE(USE_ATOMIC_REFCOUNT, 1,
    [Define to 1 to use atomic operations for reference counting.])
fi

### Disable running Make in the doc directory.
### This is useful, for example, when building Octave on systems without TeX.

DOCDIR=doc
AC_ARG_ENABLE([docs],
  [AS_HELP_STRING([--disable-docs], [don't build documentation files])],
  [if test "$enableval" = no; then
     DOCDIR=
     warn_docs="building documentation disabled; make dist will fail"
     OCTAVE_CONFIGURE_WARNING([warn_docs])
   fi],
  [])
AC_SUBST(DOCDIR)

### If possible, use a 64-bit integer type for array dimensions and indexing.

USE_64_BIT_IDX_T=false
OCTAVE_IDX_TYPE=int
AC_ARG_ENABLE(64,
  [AS_HELP_STRING([--enable-64],
    [(EXPERIMENTAL) use 64-bit integers for array dimensions and indexing])],
  [if test "$enableval" = yes; then USE_64_BIT_IDX_T=true; fi], [])
if $USE_64_BIT_IDX_T; then
  AC_CHECK_SIZEOF([void *])
  AC_CHECK_SIZEOF([int])
  AC_CHECK_SIZEOF([long])
  if test $ac_cv_sizeof_void_p -eq 8; then
    if test $ac_cv_sizeof_int -eq 8; then
      OCTAVE_IDX_TYPE=int
    elif test $ac_cv_sizeof_long -eq 8; then
      OCTAVE_IDX_TYPE=long
      AC_DEFINE(IDX_TYPE_LONG, 1, [Define to 1 if octave index type is long.])
    else
      warn_64_bit="no suitable type found for octave_idx_type so disabling 64-bit features"
      OCTAVE_CONFIGURE_WARNING([warn_64_bit])
      USE_64_BIT_IDX_T=false
    fi
  else
    warn_64_bit="pointers are not 64-bits wide; disabling 64-bit features"
    OCTAVE_CONFIGURE_WARNING([warn_64_bit])
    USE_64_BIT_IDX_T=false
  fi
fi
AC_SUBST(OCTAVE_IDX_TYPE)
AC_DEFINE_UNQUOTED(OCTAVE_IDX_TYPE, [$OCTAVE_IDX_TYPE],
  [Define to the type of octave_idx_type (64 or 32 bit signed integer).])
if $USE_64_BIT_IDX_T; then
  AC_DEFINE(USE_64_BIT_IDX_T, 1,
    [Define to 1 if using 64-bit integers for array dimensions and indexing.])
fi
AC_SUBST(USE_64_BIT_IDX_T)

### It seems that there are some broken inline assembly functions in
### the GNU libc.  Since I'm not sure how to test whether we are using
### GNU libc, just disable them for all platforms.

AC_MSG_NOTICE([defining __NO_MATH_INLINES avoids buggy GNU libc exp function])
AC_DEFINE(__NO_MATH_INLINES, 1,
  [Define to 1 if your version of GNU libc has buggy inline assembly code for math functions like exp.])

### Determine which C++ compiler to use (we expect to find g++).

AC_PROG_CXX
AC_PROG_CXXCPP

### Check version number when using g++.

GXX_VERSION=
if test "$GXX" = yes; then
  gxx_version=`$CXX -v 2>&1 | grep "^.*g.. version" | \
    sed -e 's/^.*g.. version *//' -e 's/cygnus-//' -e 's/egcs-//' -e 's/ .*//'`

  AX_COMPARE_VERSION([$gxx_version], [lt], [3.5],
    [AC_MSG_ERROR([g++ version $gxx_version will probably fail to compile Octave])])

  GXX_VERSION=$gxx_version
fi
AC_SUBST(GXX_VERSION)

## FIXME: CXX_VERSION is deprecated and should be removed in Octave version 3.12
CXX_VERSION=$gxx_version
AC_SUBST(CXX_VERSION)

### Determine which C compiler to use (we expect to find gcc).

AC_PROG_CC
AC_PROG_CPP
AC_PROG_GCC_TRADITIONAL

## Check for MSVC
have_msvc=no
case $host_os in
  msdosmsvc)
    have_msvc=yes
  ;;
  mingw*)
    AC_MSG_CHECKING([for MSVC compiler])
    AC_PREPROC_IFELSE([AC_LANG_SOURCE([[
        #ifndef _MSC_VER
        #error "Not MSVC compiler"
        #endif
        ]])],
      have_msvc=yes, have_msvc=no)
    AC_MSG_RESULT([$have_msvc])
  ;;
esac

### gnulib initialization: part 1
### Must take place immediately after a compiler is determined

gl_EARLY

### Check version number when using gcc.

GCC_VERSION=
if test "$GCC" = yes; then
  gcc_version=`$CC -v 2>&1 | grep "^.*gcc version" | \
    sed -e 's/^.*g.. version *//' -e 's/cygnus-//' -e 's/egcs-//' -e 's/ .*//'`

  AX_COMPARE_VERSION([$gcc_version], [lt], [3],
    [warn_gcc_version="gcc version $gcc_version is likely to cause problems"
     OCTAVE_CONFIGURE_WARNING([warn_gcc_version])])

  GCC_VERSION=$gcc_version
fi
AC_SUBST(CC_VERSION)

## FIXME: CC_VERSION is deprecated and should be removed in Octave version 3.12
CC_VERSION=$gcc_version
AC_SUBST(GCC_VERSION)

### Determine the compiler flag necessary to create dependencies

## Assume GCC.
INCLUDE_DEPS=true
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
        INCLUDE_DEPS=false
      fi
    ;;
    *)
      INCLUDE_DEPS=false
    ;;
  esac
fi
AC_SUBST(INCLUDE_DEPS)
AC_SUBST(DEPEND_FLAGS)
AC_SUBST(DEPEND_EXTRA_SED_PATTERN)

### Check for pthread library

AX_PTHREAD
## Include pthread libs and flags early in case other tests need them.
## They seem to be required for the OpenGL tests on Debian systems.
LIBS="$PTHREAD_LIBS $LIBS"
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"

### When compiling math for x87, problems may arise in some code comparing
### floating-point intermediate results.
### Generally, it helps to store the result in a local volatile variable,
### but it also degrades performance.
### Thus, we provide a FLOAT_TRUNCATE macro that may be defined to "volatile"
### when compiling for x87 target, or left empty for modern SSE math, that
### doesn't suffer from this problem at all.
AC_ARG_ENABLE([float-truncate],
  [AS_HELP_STRING([--enable-float-truncate],
    [enables truncating intermediate FP results.])],
  [if test "$enableval" = yes; then
     ac_float_truncate=volatile
   else
     ac_float_truncate=
   fi],
  [ac_float_truncate=])

AC_DEFINE_UNQUOTED(FLOAT_TRUNCATE, [$ac_float_truncate], 
  [Define to volatile if you need to truncate intermediate FP results.])

### Determine extra CFLAGS that may be necessary for Octave.

## On Intel systems with gcc, we may need to compile with -mieee-fp
## to get full support for IEEE floating point.
##
## On Alpha/OSF systems, we need -mieee.

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

### Test whether the compiler supports OpenMP.  This is experimental so disable
### it by default.  Enable it with the flag --enable-openmp.

USE_OPENMP=false
AC_ARG_ENABLE([openmp],
  [AS_HELP_STRING([--enable-openmp],
    [(EXPERIMENTAL) use OpenMP SMP multi-threading])],
  [if test "$enableval" = yes; then USE_OPENMP=true; fi], [])
if $USE_OPENMP; then
  case $host_os in
    mingw* | cygwin* | *-gnu*)
      OCTAVE_CHECK_OPENMP(-fopenmp)
    ;;
    msdosmsvc)
      ## FIXME: is this the right flag for MSVC?
      OCTAVE_CHECK_OPENMP(-openmp)
    ;;
    ## Add other compilers supporting OpenMP here
  esac
fi

### Defaults for cross compiling.  BUILD_CC and BUILD_CXX are
### the compilers that we use for building tools on the build system.
### For now, we assume that the only cross compiling we can do is
### with gcc on a Unixy system, but the dedicated hacker can override these.

if test "$cross_compiling" = yes; then
  BUILD_CC="gcc"
  BUILD_CFLAGS="-O2 -g"
  BUILD_CXX="g++"
  BUILD_CXXFLAGS="-O2 -g"
  BUILD_LDFLAGS=""
  BUILD_EXEEXT=""
else
  BUILD_CC='$(CC)'
  BUILD_CFLAGS='$(CFLAGS)'
  BUILD_CXX='$(CXX)'
  BUILD_CXXFLAGS='$(CXXFLAGS)'
  BUILD_LDFLAGS='$(LDFLAGS)'
  BUILD_EXEEXT='$(EXEEXT)'
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


### Look for math library.  If found, this will add -lm to LIBS.

dnl Keep this check before the check for the Fortran compiler,
dnl in case -lm is needed to compile Fortran programs.
AC_CHECK_LIB(m, sin)

### Determine the Fortran compiler and how to invoke it

## Default FFLAGS is -O.
if test x"$FFLAGS" = x""; then
  FFLAGS="-O"
fi

## the F77 variable, if set, overrides AC_PROG_F77 automatically
AC_PROG_F77
AC_F77_LIBRARY_LDFLAGS
AC_F77_DUMMY_MAIN
AC_F77_WRAPPERS

F77_TOLOWER=true
F77_APPEND_UNDERSCORE=true
F77_APPEND_EXTRA_UNDERSCORE=true

case $ac_cv_f77_mangling in
  "upper case") F77_TOLOWER=false ;;
esac
case $ac_cv_f77_mangling in
  "no underscore") F77_APPEND_UNDERSCORE=false ;;
esac
case $ac_cv_f77_mangling in
  "no extra underscore") F77_APPEND_EXTRA_UNDERSCORE=false ;;
esac

case $canonical_host_type in
  i[[3456789]]86-*-*)
    if test $ac_cv_f77_compiler_gnu = yes; then
      OCTAVE_F77_FLAG([-mieee-fp])
    fi
  ;;
  alpha*-*-*)
    if test $ac_cv_f77_compiler_gnu = yes; then
      OCTAVE_F77_FLAG([-mieee])
    else
      OCTAVE_F77_FLAG([-ieee])
      OCTAVE_F77_FLAG([-fpe1])
    fi
  ;;
  powerpc-apple-machten*)
    FFLAGS=
  ;;
esac

if test -n "$FFLAGS"; then
  AC_MSG_NOTICE([defining FFLAGS to be $FFLAGS])
fi

AC_SUBST(F77_TOLOWER)
AC_SUBST(F77_APPEND_UNDERSCORE)
AC_SUBST(F77_APPEND_EXTRA_UNDERSCORE)

if test -z "$F77"; then
  AC_MSG_ERROR([in order to build Octave, you must have a compatible Fortran compiler or wrapper script for f2c that functions as a Fortran compiler installed and in your path.  See the file INSTALL for more information.])
fi

OCTAVE_CHECK_FUNC_FORTRAN_ISNAN
F77_ISNAN_MACRO=
if test $octave_cv_func_fortran_isnan = no; then
  AC_MSG_NOTICE([substituting ISNAN(X) with X.NE.X in Fortran sources])
  F77_ISNAN_MACRO="s|ISNAN(\(@<:@^)@:>@*\))|(\1.NE.\1)|"
fi
AC_SUBST(F77_ISNAN_MACRO)

OCTAVE_CHECK_SIZEOF_FORTRAN_INTEGER
if test $octave_cv_sizeof_fortran_integer = no; then
  if $USE_64_BIT_IDX_T; then
    case $F77 in
      *gfortran*)
        case $F77_INTEGER_8_FLAG in
          *-fdefault-integer-8*)
          ;;
          *)
            case $FFLAGS in
              *-fdefault-integer-8*)
                AC_MSG_NOTICE([setting -fdefault-integer-8 in F77_INTEGER_8_FLAG instead of FFLAGS])
                FFLAGS=`echo $FFLAGS | sed 's/-fdefault-integer-8//g'`
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
    if test $octave_cv_sizeof_fortran_integer = no; then
      AC_MSG_ERROR([in order to build Octave with 64-bit indexing support your Fortran compiler must have an option for setting the default integer size to 8 bytes.  See the file INSTALL for more information.])
    fi
  else
    AC_MSG_ERROR([your Fortran compiler must have an option to make integers the same size as octave_idx_type ($OCTAVE_IDX_TYPE).  See the file INSTALL for more information.])
  fi
fi
AC_SUBST(F77_INTEGER_8_FLAG)

## FIXME: Is this really used?  Makefile seems to use $F77 for compiler
FC=$F77
AC_SUBST(FC)

OCTAVE_F77_FLAG([-ffloat-store], [
  AC_MSG_RESULT([setting F77_FLOAT_STORE_FLAG to -ffloat-store])
  F77_FLOAT_STORE_FLAG=-ffloat-store
  AC_SUBST(F77_FLOAT_STORE_FLAG)
])

### Check for the Qhull library

OCTAVE_CHECK_LIB(qhull, QHull,
  [Qhull library not found -- this will result in loss of functionality of some geometry functions.],
  [libqhull/libqhull.h qhull/libqhull.h libqhull.h qhull/qhull.h qhull.h],
  [qh_qhull], [], [],
  [warn_qhull=
  OCTAVE_CHECK_QHULL_VERSION
  OCTAVE_CHECK_LIB_QHULL_OK(
    [TEXINFO_QHULL="@set HAVE_QHULL"
    AC_DEFINE(HAVE_QHULL, 1, [Define to 1 if Qhull is available.])],
    [warn_qhull="Qhull library found, but does not seem to work properly -- this will result in loss of functionality of some geometry functions.  Please try recompiling the library with -fno-strict-aliasing."])])

### Check for PCRE regex library.

REGEX_LIBS=

pcre_fail_msg="to build Octave, you must have the PCRE library and header files installed"

AC_CHECK_HEADERS([pcre.h pcre/pcre.h])

AC_CACHE_CHECK([whether pcre.h defines the macros we need],
  [ac_cv_pcre_h_macros_present],
  [AC_EGREP_CPP([PCRE_HAS_MACROS_WE_NEED], [
    #if defined (HAVE_PCRE_H)
    # include <pcre.h>
    #elif defined (HAVE_PCRE_PCRE_H)
    # include <pcre/pcre.h>
    #error "NO PCRE HEADER"
    #endif
    #if defined (PCRE_INFO_NAMECOUNT) \
      && defined (PCRE_INFO_NAMEENTRYSIZE) \
      && defined (PCRE_INFO_NAMETABLE)
      PCRE_HAS_MACROS_WE_NEED
    #endif],
    ac_cv_pcre_h_macros_present=yes, ac_cv_pcre_h_macros_present=no)])

if test $ac_cv_pcre_h_macros_present = yes; then
  ## check for pcre-config, and if so, set XTRA_CXXFLAGS appropriately
  AC_CHECK_PROG(HAVE_PCRE_CONFIG, pcre-config, [yes], [no])
  if test $HAVE_PCRE_CONFIG = yes; then
    XTRA_CXXFLAGS="$XTRA_CXXFLAGS `pcre-config --cflags`"
    REGEX_LIBS=`pcre-config --libs`
  else
    REGEX_LIBS="-lpcre"
  fi
  save_LIBS="$LIBS"
  LIBS="$REGEX_LIBS $LIBS"
  AC_CHECK_FUNCS([pcre_compile],
    [AC_SUBST(REGEX_LIBS)],
    [AC_MSG_ERROR([$pcre_fail_msg])])
  LIBS="$save_LIBS"
else
  AC_MSG_ERROR([$pcre_fail_msg])
fi

### Check for ZLIB library.

OCTAVE_CHECK_LIB(z, ZLIB,
  [ZLIB library not found.  Octave will not be able to save or load compressed data files or HDF5 files.],
  [zlib.h], [gzclearerr])

### Check for the LLVM library

build_jit=false
AC_ARG_ENABLE([jit],
  [AS_HELP_STRING([--enable-jit],
    [(EXPERIMENTAL) enable JIT compiler])],
  [if test "$enableval" = yes; then
     build_jit=true
   fi],
  [])

LLVM_CXXFLAGS=
LLVM_CPPFLAGS=
LLVM_LDFLAGS=
LLVM_LIBS=

if test $build_jit = true; then

  ## Find llvm-config program from environment variable or by searching
  AC_ARG_VAR([LLVM_CONFIG], [path to llvm-config utility])
  AC_CHECK_PROG([LLVM_CONFIG], llvm-config, llvm-config, [])

  if test -z "$LLVM_CONFIG"; then
    warn_llvm="llvm-config utility not found.  JIT compiler is disabled."
  else
    dnl Preset warning message in case compile fails
    warn_llvm="LLVM was not found or is to old.  JIT compiler is disabled."

    save_CPPFLAGS="$CPPFLAGS"
    save_CXXFLAGS="$CXXFLAGS"

    ## Use -isystem if available because we don't want to see warnings in LLVM
    LLVM_INCLUDE_FLAG=-I
    OCTAVE_CC_FLAG([-isystem .], [
      LLVM_INCLUDE_FLAG=-isystem
      AC_MSG_NOTICE([using -isystem for LLVM headers])])

    dnl Use -isystem so we don't get warnings from llvm headers
    LLVM_CPPFLAGS="$LLVM_INCLUDE_FLAG `$LLVM_CONFIG --includedir`"
    LLVM_CXXFLAGS=
    LLVM_LDFLAGS="-L`$LLVM_CONFIG --libdir`"
    LLVM_LIBS=`$LLVM_CONFIG --libs`

    dnl
    dnl Define some extra flags that LLVM requires in order to include headers.
    dnl Ideally we should get these from llvm-config, but llvm-config isn't
    dnl very helpful.
    dnl
    CPPFLAGS="-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS $LLVM_CPPFLAGS $CPPFLAGS"
    CXXFLAGS="$LLVM_CXXFLAGS $CXXFLAGS"
    AC_LANG_PUSH(C++)
    AC_CHECK_HEADER([llvm/Support/TargetSelect.h], [
      warn_llvm=
      XTRA_CXXFLAGS="$XTRA_CXXFLAGS $LLVM_CXXFLAGS $LLVM_CPPFLAGS"])
    AC_LANG_POP(C++)
    CPPFLAGS="$save_CPPFLAGS"
    CXXFLAGS="$save_CXXFLAGS"
  fi

  if test -z "$warn_llvm"; then
    AC_DEFINE(HAVE_LLVM, 1, [Define to 1 if LLVM is available.])
  else
    build_jit=false
    LLVM_CPPFLAGS=
    LLVM_CXXFLAGS=
    LLVM_LDFLAGS=
    LLVM_LIBS=
    OCTAVE_CONFIGURE_WARNING([warn_llvm])
  fi
dnl FIXME: Re-instate when JIT is enabled by default
dnl else
dnl   ## JIT build disabled
dnl   warn_llvm="JIT compiler disabled, some performance loss for loops"
dnl   OCTAVE_CONFIGURE_WARNING([warn_llvm])
fi

AC_SUBST(LLVM_CPPFLAGS)
AC_SUBST(LLVM_CXXFLAGS)
AC_SUBST(LLVM_LDFLAGS)
AC_SUBST(LLVM_LIBS)

### Check for HDF5 library.

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$Z_CPPFLAGS $CPPFLAGS"
LIBS="$Z_LDFLAGS $Z_LIBS $LIBS"
OCTAVE_CHECK_LIB(hdf5, HDF5,
  [HDF5 library not found.  Octave will not be able to save or load HDF5 data files.],
  [hdf5.h], [H5Gget_num_objs], [], [],
  [warn_hdf5=
   OCTAVE_CHECK_HDF5_HAS_VER_16_API
   TEXINFO_HDF5="@set HAVE_HDF5"
   AC_DEFINE(HAVE_HDF5, 1,
     [Define to 1 if HDF5 is available and newer than version 1.6.])
   if test $have_msvc = yes; then
     OCTAVE_CHECK_LIB_HDF5_DLL
   fi
  ])
CPPFLAGS="$save_CPPFLAGS"
LIBS="$save_LIBS"

### Check for FFTW library.  Default to Fortran FFTPACK if it is not available.

## Check for FFTW header and library.
OCTAVE_CHECK_LIB(fftw3, FFTW3,
  [FFTW3 library not found.  The slower FFTPACK library will be used instead.],
  [fftw3.h], [fftw_plan_dft_1d])

OCTAVE_CHECK_LIB(fftw3f, FFTW3F,
  [FFTW3F library not found.  The slower FFTPACK library will be used instead.],
  [fftw3.h], [fftwf_plan_dft_1d])

AM_CONDITIONAL([AMCOND_HAVE_FFTW],
  [test -n "$FFTW3_LIBS" && test -n "$FFTW3F_LIBS"])

## Subdirectory of liboctave/cruft to build if FFTW is not found.
FFT_DIR="fftpack"
AC_SUBST(FFT_DIR)

### Check for GLPK library and header.

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$Z_CPPFLAGS $CPPFLAGS"
LIBS="$Z_LDFLAGS $Z_LIBS $LIBS"
OCTAVE_CHECK_LIB(glpk, GLPK,
  [GLPK library not found.  The glpk function for solving linear programs will be disabled.],
  [glpk/glpk.h glpk.h], [_glp_lpx_simplex])
LIBS="$save_LIBS"
CPPFLAGS="$save_CPPFLAGS"

### Checks for cURL header and library.

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$Z_CPPFLAGS $CPPFLAGS"
LIBS="$Z_LDFLAGS $Z_LIBS $LIBS"
OCTAVE_CHECK_LIB(curl, cURL,
  [cURL library not found.  The ftp objects, urlread and urlwrite functions will be disabled.],
  [curl/curl.h], [curl_easy_escape])
if test -z "$warn_curl"; then
  ## Additional check on cURL library that was found
  AC_CACHE_CHECK([for CURLOPT_DIRLISTONLY in curl/curl.h],
    [octave_cv_header_define_curlopt_dirlistonly],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
      #include <curl/curl.h>
      ]], [[
      curl_easy_setopt ((CURL*)NULL, CURLOPT_DIRLISTONLY, 0);
      ]])],
      [octave_cv_header_define_curlopt_dirlistonly=no],
      [octave_cv_header_define_curlopt_dirlistonly=yes])
    ])
  if test $octave_cv_header_define_curlopt_dirlistonly = yes; then
    AC_DEFINE(CURLOPT_DIRLISTONLY, CURLOPT_FTPLISTONLY,
      [Define to the legacy option name if using an older version of cURL.])]
  fi
fi
LIBS="$save_LIBS"
CPPFLAGS="$save_CPPFLAGS"

### Check for either of Graphics/ImageMagick++ libraries

AC_ARG_WITH([magick],
  [AS_HELP_STRING([--with-magick=LIB],
    [select library to use for image I/O (options: GraphicsMagick(default) or ImageMagick)])],
  [magick="$withval"],
  [magick="GraphicsMagick"])

warn_magick="$magick++ library not found.  The imread function for reading image files will not be fully functional."

MAGICK_CPPFLAGS=
MAGICK_LDFLAGS=
MAGICK_LIBS=

PKG_CHECK_EXISTS([$magick++], [
  ## Make sure we only get -I, -L, and -l flags.  Some Graphics/ImageMagick++
  ## packages add extra flags that are useful when building
  ## Graphics/ImageMagick++ extentions.  These extra flags break the
  ## Octave build. 
  MAGICK_LDFLAGS=`$PKG_CONFIG --libs-only-L $magick++`
  MAGICK_LIBS=`$PKG_CONFIG --libs-only-l $magick++`
  MAGICK_CPPFLAGS=`$PKG_CONFIG --cflags-only-I $magick++`

  warn_magick="$magick++ library fails tests.  The imread function for reading image files will not be fully functional."

  save_CPPFLAGS="$CPPFLAGS"
  save_LIBS="$LIBS"
  CPPFLAGS="$MAGICK_CPPFLAGS $CPPFLAGS"
  LIBS="$MAGICK_LDFLAGS $MAGICK_LIBS $LIBS"
  AC_LANG_PUSH(C++)
  AC_CHECK_HEADER([Magick++.h], [
    AC_CACHE_CHECK([for Magick::ColorRGB in Magick++.h],
      [octave_cv_func_magick_colorrgb],
      [AC_PREPROC_IFELSE([AC_LANG_SOURCE([[
        #include <Magick++.h>
        ]], [[
        Magick::ColorRGB c;
        ]])],
        octave_cv_func_magick_colorrgb=yes,
        octave_cv_func_magick_colorrgb=no)
      ])
    if test $octave_cv_func_magick_colorrgb = yes; then
      warn_magick=
    fi
  ])
  AC_LANG_POP(C++)
  CPPFLAGS="$save_CPPFLAGS"
  LIBS="$save_LIBS"

  AC_CHECK_FUNCS([setlocale], [],
                 [warn_magick="$magick++ requires setlocale function.  The imread function for reading image files will not be fully functional."])
])

if test -z "$warn_magick"; then
  AC_DEFINE(HAVE_MAGICK, 1,
    [Define to 1 if Graphics/ImageMagick++ is available.])
else
  MAGICK_CPPFLAGS=
  MAGICK_LDFLAGS=
  MAGICK_LIBS=
fi
AC_SUBST(MAGICK_CPPFLAGS)
AC_SUBST(MAGICK_LDFLAGS)
AC_SUBST(MAGICK_LIBS)

### Check for X11 libraries

AC_PATH_X
if test "$have_x" = yes; then
  AC_DEFINE(HAVE_X_WINDOWS, 1, [Define to 1 if you have X11.])

  if test "$x_includes" != "NONE"; then
    X11_INCFLAGS="$x_includes"
  fi
  AC_SUBST(X11_INCFLAGS)

  if test -z "$x_libraries"; then
    AC_CHECK_LIB([X11], XrmInitialize, [X11_LIBS="-lX11"], [X11_LIBS=])
  elif test $x_libraries != "NONE"; then
    AC_CHECK_LIB([X11], XrmInitialize, 
      [X11_LIBS="-L$x_libraries -lX11"], [X11_LIBS=], "-L$x_libraries")
  fi
  AC_SUBST(X11_LIBS)
fi

### Check for the Carbon framework on MacOSX systems
OCTAVE_HAVE_FRAMEWORK([Carbon],
  [[#include <Carbon/Carbon.h>]], [[CGMainDisplayID ()]],
  [have_framework_carbon=yes], [have_framework_carbon=no])
if test $have_framework_carbon = yes; then
  AC_DEFINE(HAVE_FRAMEWORK_CARBON, 1,
    [Define to 1 if framework CARBON is available.])
  CARBON_LIBS="-Wl,-framework -Wl,Carbon"
  AC_MSG_NOTICE([adding -Wl,-framework -Wl,Carbon to CARBON_LIBS])
  AC_SUBST(CARBON_LIBS)
fi

### Check for list of libraries needed for native graphics renderer.

native_graphics=true
warn_freetype=""

check_opengl=false
AC_ARG_WITH([opengl],
  [AS_HELP_STRING([--without-opengl],
    [don't use OpenGL libraries, disable native graphics])],
  [if test x"$withval" = x"no"; then
     native_graphics=false
     warn_opengl="--without-opengl specified.  Native graphics will be disabled."
     OCTAVE_CONFIGURE_WARNING([warn_opengl])
   else
     check_opengl=true
   fi],
  [check_opengl=true])

## Check for OpenGL library
if $check_opengl; then
  OCTAVE_CHECK_LIB_OPENGL
fi

GRAPHICS_LIBS=
GRAPHICS_CFLAGS=

if test -z "$OPENGL_LIBS"; then
  if $check_opengl; then
    native_graphics=false
    warn_fltk_opengl="OpenGL libs (GL and GLU) not found.  Native graphics will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_fltk_opengl])
  fi
fi

if test -n "$OPENGL_LIBS"; then
  AC_DEFINE(HAVE_OPENGL, 1, [Define to 1 if OpenGL is available.])

  ## Check for FreeType 2 library

  PKG_CHECK_MODULES([FT2], [freetype2], [
    min_ft2_version=9.03
    AC_MSG_CHECKING([for FreeType -- version >= $min_ft2_version])
    $PKG_CONFIG freetype2 --atleast-version=$min_ft2_version
    ac_status=$?
    if test $ac_status = 0; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_FREETYPE, 1, [Define to 1 if you have Freetype library.])
      XTRA_CXXFLAGS="$XTRA_CXXFLAGS $FT2_CFLAGS"
    else
      AC_MSG_RESULT(no)
      warn_freetype="FreeType library not found.  Native graphics will be disabled."
    fi])

  if test -n "$warn_freetype"; then
    OCTAVE_CONFIGURE_WARNING([warn_freetype])
    native_graphics=false
  fi

  ## Check for fontconfig library

  warn_fontconfig=""
  if test -z "$warn_freetype"; then
    PKG_CHECK_MODULES(FONTCONFIG, [fontconfig],
      [have_fontconfig=yes
       OPENGL_LIBS="$FONTCONFIG_LIBS $OPENGL_LIBS"
       XTRA_CXXFLAGS="$XTRA_CXXFLAGS $FONTCONFIG_CFLAGS"
       AC_DEFINE(HAVE_FONTCONFIG, 1, [Define to 1 if fontconfig is present.])],
      [have_fontconfig=no
       warn_fontconfig="Fontconfig library not found.  Native graphics will be disabled."])
  fi

  if test -n "$warn_fontconfig"; then
    native_graphics=false
    OCTAVE_CONFIGURE_WARNING([warn_fontconfig])
  fi

  ## Check for FLTK (www.fltk.org) library

  AC_ARG_WITH([fltk-prefix], [
    AS_HELP_STRING([--with-fltk-prefix=PFX],
      [prefix where FLTK is installed (optional)])],
    [fltk_prefix="$withval"],
    [fltk_prefix=""])

  AC_ARG_WITH([fltk-exec-prefix], [
    AS_HELP_STRING([--with-fltk-exec-prefix=PFX],
      [exec prefix where FLTK is installed (optional)])],
    [fltk_exec_prefix="$withval"],
    [fltk_exec_prefix=""])

  if test x"$fltk_exec_prefix" != x""; then
    fltk_args="$fltk_args --exec-prefix=$fltk_exec_prefix"
    if test "x${FLTK_CONFIG+set}" != xset ; then
      FLTK_CONFIG="$fltk_exec_prefix/bin/fltk-config"
    fi
  fi

  if test x"$fltk_prefix" != x""; then
    fltk_args="$fltk_args --prefix=$fltk_prefix"
    if test x${FLTK_CONFIG+set} != xset ; then
      FLTK_CONFIG="$fltk_prefix/bin/fltk-config"
    fi
  fi

  AC_PATH_PROG([FLTK_CONFIG], [fltk-config], [no])

  warn_fltk_config=""
  warn_fltk_opengl=""

  if test "$FLTK_CONFIG" = no; then
    native_graphics=false
    warn_fltk_config="FLTK config script not found.  Native graphics will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_fltk_config])
  else
    FLTK_CFLAGS=`$FLTK_CONFIG $fltkconf_args --use-gl --cflags`
    FLTK_LDFLAGS=`$FLTK_CONFIG $fltkconf_args --use-gl --ldflags`

    case $host_os in
      mingw*)
        FLTK_LDFLAGS=`echo $FLTK_LDFLAGS | sed -e 's/-mwindows//g'`
      ;;
    esac

    AC_CACHE_CHECK([for OpenGL support in FLTK],
      [octave_cv_fltk_opengl_support],
      [save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS $FLTK_CFLAGS"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <FL/gl.h>
        ]], [[
        int nothing = 0;
        ]])],
        octave_cv_fltk_opengl_support=no,
        octave_cv_fltk_opengl_support=yes)
      CFLAGS="$save_CFLAGS"
      ])
    if test $octave_cv_fltk_opengl_support = no; then
      warn_fltk_opengl="FLTK does not have OpenGL support.  Native graphics will be disabled."
    else
      AC_DEFINE(HAVE_FLTK, 1, [Define to 1 if FLTK is available.])
    fi 

    if test -z "$warn_fltk_opengl"; then
      GRAPHICS_CFLAGS="$FLTK_CFLAGS"
      GRAPHICS_LIBS="$FLTK_LDFLAGS"
    else
      native_graphics=false
      OCTAVE_CONFIGURE_WARNING([warn_fltk_opengl])
    fi
  fi
fi

AC_SUBST(GRAPHICS_CFLAGS)
AC_SUBST(GRAPHICS_LIBS)

### Start determination of shared vs. static libraries

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

OCTAVE_PROG_AR

ifdef([LT_INIT], [], [
  errprint([error: you must have libtool 2.2.2 or a more recent version
])
  m4exit([1])])

LT_PREREQ([2.2.2])
LT_INIT([disable-static dlopen win32-dll])

if test x"$enable_shared" = x"yes"; then
  SHARED_LIBS=true
else
  SHARED_LIBS=false
fi

if test x"$enable_static" = x"yes"; then
  STATIC_LIBS=true
else
  STATIC_LIBS=false
fi

XTRA_CRUFT_SH_LDFLAGS=
if test $have_msvc = yes; then
  FLIBS="$FLIBS -lkernel32"
  XTRA_CRUFT_SH_LDFLAGS="-Wl,-def:cruft/cruft.def"
fi
AC_SUBST(XTRA_CRUFT_SH_LDFLAGS)

### Check for BLAS and LAPACK libraries:

## Need to adjust FFLAGS to include correct integer size.
save_FFLAGS="$FFLAGS"
FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"

AX_BLAS_WITH_F77_FUNC([:], [:])
AX_LAPACK([:], [:])

## Restore FFLAGS.
FFLAGS="$save_FFLAGS"

## If necessary, try again with -ff2c in FFLAGS
if test $ax_blas_f77_func_ok = no; then
  save_FFLAGS="$FFLAGS"
  FFLAGS="-ff2c $FFLAGS $F77_INTEGER_8_FLAG"

  AX_BLAS_WITH_F77_FUNC([:], [:])
  AX_LAPACK([:], [:])

  ## Restore FFLAGS, with -ff2c if that was helpful

  if test $ax_blas_f77_func_ok = no; then
    FFLAGS="$save_FFLAGS"
  else
    FFLAGS="-ff2c $save_FFLAGS"
  fi
fi

## On OSX, try again with a wrapper library (without -ff2c!)
if test $ax_blas_f77_func_ok = no; then
  case $host_os in
    darwin*)
      ## test if wrapper functions help
      octave_blaswrap_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -DUSE_BLASWRAP"
      AC_LANG_PUSH(C)
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
          #include "liboctave/cruft/misc/blaswrap.c"
        ]])],
        [mv conftest.$ac_objext blaswrap.$ac_objext
         octave_blaswrap_save_BLAS_LIBS="$BLAS_LIBS"
         BLAS_LIBS="blaswrap.$ac_objext -framework vecLib"

         save_FFLAGS="$FFLAGS"
         FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"

         AX_BLAS_WITH_F77_FUNC([:], [:])
         AX_LAPACK([:], [:])

         ## Restore FFLAGS.
         FFLAGS="$save_FFLAGS"

         ## remove temp file
         rm -f blaswrap.$ac_objext],
        [AC_MSG_FAILURE([cannot compile liboctave/cruft/misc/blaswrap.c])])
      AC_LANG_POP(C)
      CFLAGS="$octave_blaswrap_save_CFLAGS"

      if test $ax_blas_f77_func_ok = no; then
        BLAS_LIBS="$octave_blaswrap_save_BLAS_LIBS"
      else
        ## wrapper in cruft, remove from BLAS_LIBS
        BLAS_LIBS=`echo $BLAS_LIBS | sed -e 's/blaswrap.[[^ ]]* //g'`
        AC_DEFINE(USE_BLASWRAP, 1,
          [Define to 1 if BLAS functions need to be wrapped (potentially needed for 64-bit OSX only).])
      fi
    ;;
  esac
fi

if test $ax_blas_f77_func_ok = no; then
  if $USE_64_BIT_IDX_T && test $ax_blas_integer_size_ok = no; then
    ## Attempt to be more informative.
    AC_MSG_ERROR([BLAS doesn't seem to support 64-bit integers.  This is incompatible with --enable-64.])
  else
    AC_MSG_ERROR([A BLAS library was detected but found incompatible with your Fortran 77 compiler settings.])
  fi
fi

if test $ax_blas_ok = no || test $ax_lapack_ok = no; then
  AC_MSG_ERROR([BLAS and LAPACK libraries are required])
fi

### Check for the qrupdate library

## No need to adjust FFLAGS because only link is attempted. 
## Must supply proper LIBS, however.
save_LIBS="$LIBS"
LIBS="$LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS"
OCTAVE_CHECK_LIB(qrupdate, qrupdate,
  [qrupdate not found.  The QR & Cholesky updating functions will be slow.],
  [],
  [sqr1up],
  [Fortran 77], [don't use qrupdate, disable QR & Cholesky updating functions])

if test $octave_cv_lib_qrupdate = yes; then
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

### Check for AMD library

OCTAVE_CHECK_LIB(amd, AMD,
  [AMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/amd.h ufsparse/amd.h amd/amd.h amd.h],
  [amd_postorder],
  [], [don't use AMD library, disable some sparse matrix functionality])

### Check for CAMD library

OCTAVE_CHECK_LIB(camd, CAMD,
  [CAMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/camd.h ufsparse/camd.h amd/camd.h camd.h],
  [camd_postorder],
  [], [don't use CAMD library, disable some sparse matrix functionality])

### Check for COLAMD library

OCTAVE_CHECK_LIB(colamd, COLAMD,
  [COLAMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/colamd.h ufsparse/colamd.h amd/colamd.h colamd.h],
  [colamd],
  [], [don't use COLAMD library, disable some sparse matrix functionality])

### Check for CCOLAMD library

OCTAVE_CHECK_LIB(ccolamd, CCOLAMD,
  [CCOLAMD library not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/ccolamd.h ufsparse/ccolamd.h amd/ccolamd.h ccolamd.h],
  [ccolamd],
  [], [don't use CCOLAMD library, disable some sparse matrix functionality])

### Check for CHOLMOD library.
### If your cholmod library requires cblas, then you will need to
### configure with --with-cholmod="-lcholmod -lcblas".

save_LIBS="$LIBS"
LIBS="$COLAMD_LDFLAGS $COLAMD_LIBS $AMD_LDFLAGS $AMD_LIBS $LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS"
OCTAVE_CHECK_LIB(cholmod, CHOLMOD,
  [CHOLMOD library not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/cholmod.h ufsparse/cholmod.h cholmod/cholmod.h cholmod.h],
  [cholmod_start],
  [], [don't use CHOLMOD library, disable some sparse matrix functionality])
LIBS="$save_LIBS"

### Check for CXSparse library

OCTAVE_CHECK_LIB(cxsparse, CXSparse,
  [CXSparse library not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/cs.h ufsparse/cs.h amd/cs.h cs.h],
  [cs_di_sqr],
  [C++], [don't use CXSparse library, disable some sparse matrix functionality])

### Check for UMFPACK library.

save_LIBS="$LIBS"
save_CPPFLAGS="$CPPFLAGS"
LIBS="$AMD_LDFLAGS $AMD_LIBS $BLAS_LIBS $FLIBS $LIBS"
CPPFLAGS="$AMD_CPPFLAGS $CPPFLAGS"
OCTAVE_CHECK_LIB([umfpack], UMFPACK,
  [UMFPACK not found.  This will result in some lack of functionality for sparse matrices.],
  [suitesparse/umfpack.h ufsparse/umfpack.h umfpack/umfpack.h umfpack.h],
  [umfpack_zi_get_determinant],
  [], [don't use UMFPACK, disable some sparse matrix functionality])
CPPFLAGS="$save_CPPFLAGS"
LIBS="$save_LIBS"

if test -z "$UMFPACK_LIBS"; then
  ## Invalidate the cache and try again with -lcblas.
  $as_unset ac_cv_lib_umfpack_umfpack_zi_get_determinant
  $as_unset octave_cv_lib_umfpack
  save_LIBS="$LIBS"
  LIBS="-lcblas $AMD_LDFLAGS $AMD_LIBS $BLAS_LIBS $FLIBS $LIBS"
  OCTAVE_CHECK_LIB([umfpack], UMFPACK,
    [UMFPACK not found.  This will result in some lack of functionality for sparse matrices.],
    [suitesparse/umfpack.h ufsparse/umfpack.h umfpack/umfpack.h umfpack.h],
    [umfpack_zi_get_determinant],
    [], [don't use UMFPACK, disable some sparse matrix functionality])
  if test -n "$UMFPACK_LIBS"; then
    UMFPACK_LIBS="$UMFPACK_LIBS -lcblas"
  fi
  LIBS="$save_LIBS"
fi

## Test features of the installed UMFPACK library

if test -n "$UMFPACK_LIBS"; then
  ## SuiteSparse >= 4.0 needs additional link library for SuiteSparse_time()
  save_LIBS="$LIBS";
  LIBS="$UMFPACK_LIBS $AMD_LDFLAGS $AMD_LIBS $BLAS_LIBS $FLIBS $LIBS"
  xtra_libs=
  OCTAVE_UMFPACK_NEED_SUITESPARSE_TIME
  if test $octave_cv_umfpack_need_suitesparse_time = yes; then
    AC_CHECK_LIB([rt], [clock_gettime], [xtra_libs="-lrt"], [xtra_libs=])
    ## FIXME: This library list is only accurate for Linux, Mac OS X.
    ##        Possibly need other library names for MinGW, Cygwin.
    AC_SEARCH_LIBS([SuiteSparse_time],
                   [suitesparseconfig SuiteSparse],
                   [], [], [$xtra_libs])
    case $ac_cv_search_SuiteSparse_time in
      -l*)  
        UMFPACK_LIBS="$UMFPACK_LIBS $ac_cv_search_SuiteSparse_time"
      ;;
      no)
        UMFPACK_LIBS=
        AC_MSG_WARN([UMFPACK library found but is missing SuiteSparse_time functionality.])
        AC_MSG_WARN([UMFPACK library will be disabled.])
      ;;
    esac
  fi
  LIBS="$save_LIBS"

  ## Check for UMFPACK separately split complex matrix and RHS.
  if test -n "$UMFPACK_LIBS"; then
    save_LIBS="$LIBS";
    LIBS="$UMFPACK_LIBS $CHOLMOD_LDFLAGS $CHOLMOD_LIBS $AMD_LDFLAGS $AMD_LIBS $COLAMD_LDFLAGS $COLAMD_LIBS $LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS $xtra_libs"
    OCTAVE_UMFPACK_SEPARATE_SPLIT
    LIBS="$save_LIBS"
  fi
fi

### Check for ARPACK library.

save_LIBS="$LIBS"
LIBS="$LAPACK_LIBS $BLAS_LIBS $FLIBS $LIBS"
OCTAVE_CHECK_LIB([arpack], ARPACK,
  [ARPACK not found.  The eigs function will be disabled.],
  [],
  [dseupd],
  [Fortran 77], [don't use the ARPACK library, disable eigs function],
  [warn_arpack=
   OCTAVE_CHECK_LIB_ARPACK_OK(
     [AC_DEFINE(HAVE_ARPACK, 1, [Define to 1 if ARPACK is available.])],
     [warn_arpack="ARPACK library found, but does not seem to work properly -- disabling eigs function"])])
LIBS="$save_LIBS"

### Check for readline library.

OCTAVE_ENABLE_READLINE

### Enable dynamic linking.  --enable-shared implies this, so
### --enable-dl is only need if you are only building static libraries
### and want to try dynamic linking too (works on some systems, for
### example, OS X and Windows).

AC_ARG_ENABLE([dl],
  [AS_HELP_STRING([--disable-dl],
    [disable loading of dynamically linked modules])],
  [case $enableval in
     yes) ENABLE_DYNAMIC_LINKING=true ;;
     no) ENABLE_DYNAMIC_LINKING=false ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-dl]) ;;
   esac],
  [ENABLE_DYNAMIC_LINKING=true])

if ! $STATIC_LIBS && ! $SHARED_LIBS; then
  AC_MSG_ERROR([You can't disable building static AND shared libraries!])
fi

CPICFLAG=-fPIC
CXXPICFLAG=-fPIC
FPICFLAG=-fPIC
SHLEXT=so
SHLLIB='$(SHLEXT)'
SHLBIN=
SHLEXT_VER='$(SHLEXT).$(version)'
SHLLIB_VER='$(SHLLIB).$(version)'
SHLBIN_VER='$(SHLBIN).$(version)'
SHLLINKEXT=
LIBPRE=lib
SHLPRE=lib
SHLLIBPRE=lib
SHLBINPRE=lib
SH_LD='$(CXX)'
SH_LDFLAGS=-shared
DL_LD='$(SH_LD)'
DL_LDFLAGS='$(SH_LDFLAGS)'
MKOCTFILE_DL_LDFLAGS='$(DL_LDFLAGS)'
SONAME_FLAGS=
NO_OCT_FILE_STRIP=false
TEMPLATE_AR='$(AR)'
TEMPLATE_ARFLAGS="$ARFLAGS"
CRUFT_DLL_DEFS=
OCTAVE_DLL_DEFS=
OCTINTERP_DLL_DEFS=
OCTGUI_DLL_DEFS=
OCTGRAPHICS_DLL_DEFS=
library_path_var=LD_LIBRARY_PATH
ldpreloadsep=" "
BUILD_COMPILED_AUX_PROGRAMS=false
case $canonical_host_type in
  *-*-386bsd* | *-*-netbsd*)
    SH_LD=ld
    SH_LDFLAGS=-Bshareable
  ;;
  *-*-openbsd*)
    SH_LDFLAGS='-shared -fPIC'
  ;;
  *-*-freebsd*)
    SH_LDFLAGS="-shared -Wl,-x"
  ;;
  alpha*-dec-osf*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    SH_LDFLAGS="-shared -Wl,-expect_unresolved -Wl,'*'"
  ;;
  *-*-darwin*)
    DL_LDFLAGS='-bundle -bundle_loader $(top_builddir)/libinterp/octave $(LDFLAGS)'
    MKOCTFILE_DL_LDFLAGS='-bundle -bundle_loader $$BINDIR/octave-$$OCTAVE_VERSION$$EXEEXT'
    SH_LDFLAGS='-dynamiclib -single_module $(LDFLAGS)'
    case $canonical_host_type in
      powerpc-*)
        CXXPICFLAG=
        CPICFLAG=
        FPICFLAG=
      ;;
    esac
    SHLEXT=dylib 
    SHLLIB='$(SHLEXT)'
    SHLEXT_VER='$(version).$(SHLEXT)'
    SHLLIB_VER='$(version).$(SHLLIB)'
    NO_OCT_FILE_STRIP=true
    SONAME_FLAGS='-install_name $(octlibdir)/$@'
    library_path_var=DYLD_LIBRARY_PATH  
  ;;
  *-*-cygwin*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    LIBPRE=lib
    SHLPRE=cyg
    SHLBINPRE=cyg
    SHLEXT=dll
    SHLLIB=dll.a
    SHLBIN=dll    
    DL_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc"
    SH_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
    SONAME_FLAGS='-Wl,--out-implib=$(patsubst $(SHLPRE)%,$(LIBPRE)%,$@).a'
    ldpreloadsep=":"
  ;;
  *-*-mingw*)
    BUILD_COMPILED_AUX_PROGRAMS=true
    if test $have_msvc = yes; then
      DL_LDFLAGS="-shared"
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
      SHLEXT=dll
      SHLLIB=lib
      SHLBIN=dll
      LIBPRE=
      SHLPRE=
      SHLLIBPRE=
      SHLBINPRE=
      SH_LDFLAGS="-shared"
      if test -n "`echo $CFLAGS | grep -e '-g'`" || test -n "`echo $CXXFLAGS | grep -e '-g'`"; then
        DL_LDFLAGS="$DL_LDFLAGS -g"
        SH_LDFLAGS="$SH_LDFLAGS -g"
      fi
      NO_OCT_FILE_STRIP=true
      library_path_var=PATH
      NO_OCT_FILE_STRIP=true
      ## Extra compilation flags.
      CRUFT_DLL_DEFS="-DCRUFT_DLL"
      OCTAVE_DLL_DEFS="-DOCTAVE_DLL"
      OCTINTERP_DLL_DEFS="-DOCTINTERP_DLL"
      OCTGUI_DLL_DEFS="-DOCTGUI_DLL"
      OCTGRAPHICS_DLL_DEFS="-DOCTGRAPHICS_DLL"
    else
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
      SHLEXT=dll
      SHLLIB=dll.a
      SHLBIN=dll
      DL_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc"
      SH_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
      SONAME_FLAGS='-Wl,--out-implib=$@.a'
      library_path_var=PATH
    fi
  ;;

  *-*-msdosmsvc)
    BUILD_COMPILED_AUX_PROGRAMS=true
    DL_LDFLAGS="-shared"
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    SHLEXT=dll
    SHLLIB=lib
    SHLBIN=dll
    LIBPRE=
    SHLPRE=
    SHLLIBPRE=
    SHLBINPRE=
    SH_LDFLAGS="-shared"
    if test -n "`echo $CFLAGS | grep -e '-g'`" || test -n "`echo $CXXFLAGS | grep -e '-g'`"; then
      DL_LDFLAGS="$DL_LDFLAGS -g"
      SH_LDFLAGS="$SH_LDFLAGS -g"
    fi
    NO_OCT_FILE_STRIP=true
    library_path_var=PATH
    NO_OCT_FILE_STRIP=true
    ## Extra compilation flags.
    CRUFT_DLL_DEFS="-DCRUFT_DLL"
    OCTAVE_DLL_DEFS="-DOCTAVE_DLL"
    OCTGUI_DLL_DEFS="-DOCTGUI_DLL"
    OCTGRAPHICS_DLL_DEFS="-DOCTGRAPHICS_DLL"
  ;;
  *-*-linux* | *-*-gnu*)
    MKOCTFILE_DL_LDFLAGS="-shared -Wl,-Bsymbolic"
    SONAME_FLAGS='-Wl,-soname -Wl,$@'
  ;;
  i[[3456]]86-*-sco3.2v5*)
    SONAME_FLAGS='-Wl,-h -Wl,$@'
    SH_LDFLAGS=-G
  ;;
  rs6000-ibm-aix* | powerpc-ibm-aix*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
    library_path_var=LIBPATH
  ;;
  hppa*-hp-hpux*)
    if test $ac_cv_f77_compiler_gnu = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=+Z
    fi
    SHLEXT=sl
    SH_LDFLAGS="-shared -fPIC"
    library_path_var=SHLIB_PATH
  ;;
  ia64*-hp-hpux*)
    if test $ac_cv_f77_compiler_gnu = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=+Z
    fi
    SH_LDFLAGS="-shared -fPIC"
  ;;
  *-sgi-*)
    CPICFLAG=
    CXXPICFLAG=
    FPICFLAG=
  ;;
  sparc-sun-sunos4*)
    if test $ac_cv_f77_compiler_gnu = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=-PIC
    fi
    SH_LD=ld
    SH_LDFLAGS="-assert nodefinitions"
  ;;
  sparc-sun-solaris2* | i386-pc-solaris2*)
    if test $ac_cv_f77_compiler_gnu = yes; then
      FPICFLAG=-fPIC
    else
      FPICFLAG=-KPIC
    fi
    if test "$GCC" = yes; then
      CPICFLAG=-fPIC
    else
      CPICFLAG=-KPIC
    fi
    if test "$GXX" = yes; then
      CXXPICFLAG=-fPIC
      SH_LDFLAGS=-shared
    else
      CXXPICFLAG=-KPIC
      SH_LDFLAGS=-G
    fi
    ## Template closures in archive libraries need a different mechanism.
    if test "$GXX" != yes; then
      TEMPLATE_AR='$(CXX)'
      TEMPLATE_ARFLAGS="-xar -o"
    fi
  ;;
esac

AM_CONDITIONAL([AMCOND_BUILD_COMPILED_AUX_PROGRAMS],
  [test x$BUILD_COMPILED_AUX_PROGRAMS = xtrue])

AC_MSG_NOTICE([defining FPICFLAG to be $FPICFLAG])
AC_MSG_NOTICE([defining CPICFLAG to be $CPICFLAG])
AC_MSG_NOTICE([defining CXXPICFLAG to be $CXXPICFLAG])
AC_MSG_NOTICE([defining SHLEXT to be $SHLEXT])
AC_MSG_NOTICE([defining SHLLIB to be $SHLLIB])
AC_MSG_NOTICE([defining SHLBIN to be $SHLBIN])
AC_MSG_NOTICE([defining SHLEXT_VER to be $SHLEXT_VER])
AC_MSG_NOTICE([defining SHLLIB_VER to be $SHLLIB_VER])
AC_MSG_NOTICE([defining SHLBIN_VER to be $SHLBIN_VER])
AC_MSG_NOTICE([defining SHLLINKEXT to be $SHLLINKEXT])
AC_MSG_NOTICE([defining LIBPRE to be $LIBPRE])
AC_MSG_NOTICE([defining SHLPRE to be $SHLPRE])
AC_MSG_NOTICE([defining SHLLIBPRE to be $SHLLIBPRE])
AC_MSG_NOTICE([defining SHLBINPRE to be $SHLBINPRE])
AC_MSG_NOTICE([defining SH_LD to be $SH_LD])
AC_MSG_NOTICE([defining SH_LDFLAGS to be $SH_LDFLAGS])
AC_MSG_NOTICE([defining DL_LD to be $DL_LD])
AC_MSG_NOTICE([defining DL_LDFLAGS to be $DL_LDFLAGS])
AC_MSG_NOTICE([defining MKOCTFILE_DL_LDFLAGS to be $MKOCTFILE_DL_LDFLAGS])
AC_MSG_NOTICE([defining SONAME_FLAGS to be $SONAME_FLAGS])
AC_MSG_NOTICE([defining NO_OCT_FILE_STRIP to be $NO_OCT_FILE_STRIP])
AC_MSG_NOTICE([defining TEMPLATE_AR to be $TEMPLATE_AR])
AC_MSG_NOTICE([defining TEMPLATE_ARFLAGS to be $TEMPLATE_ARFLAGS])
AC_MSG_NOTICE([defining CRUFT_DLL_DEFS to be $CRUFT_DLL_DEFS])
AC_MSG_NOTICE([defining OCTAVE_DLL_DEFS to be $OCTAVE_DLL_DEFS])
AC_MSG_NOTICE([defining OCTINTERP_DLL_DEFS to be $OCTINTERP_DLL_DEFS])
AC_MSG_NOTICE([defining OCTGUI_DLL_DEFS to be $OCTGUI_DLL_DEFS])
AC_MSG_NOTICE([defining OCTGRAPHICS_DLL_DEFS to be $OCTGRAPHICS_DLL_DEFS])
AC_MSG_NOTICE([defining library_path_var to be $library_path_var])
AC_SUBST(FPICFLAG)
AC_SUBST(CPICFLAG)
AC_SUBST(CXXPICFLAG)
AC_SUBST(SHLEXT)
AC_SUBST(SHLLIB)
AC_SUBST(SHLBIN)
AC_SUBST(SHLEXT_VER)
AC_SUBST(SHLLIB_VER)
AC_SUBST(SHLBIN_VER)
AC_SUBST(SHLLINKEXT)
AC_SUBST(LIBPRE)
AC_SUBST(SHLPRE)
AC_SUBST(SHLLIBPRE)
AC_SUBST(SHLBINPRE)
AC_SUBST(SH_LD)
AC_SUBST(SH_LDFLAGS)
AC_SUBST(DL_LD)
AC_SUBST(DL_LDFLAGS)
AC_SUBST(MKOCTFILE_DL_LDFLAGS)
AC_SUBST(SONAME_FLAGS)
AC_SUBST(NO_OCT_FILE_STRIP)
AC_SUBST(TEMPLATE_AR)
AC_SUBST(TEMPLATE_ARFLAGS)
AC_SUBST(CRUFT_DLL_DEFS)
AC_SUBST(OCTAVE_DLL_DEFS)
AC_SUBST(OCTINTERP_DLL_DEFS)
AC_SUBST(OCTGUI_DLL_DEFS)
AC_SUBST(OCTGRAPHICS_DLL_DEFS)
AC_SUBST(library_path_var)
AC_SUBST(ldpreloadsep)

### More configure argument checking related to linking

AC_ARG_ENABLE([no-undefined],
  [AS_HELP_STRING([--disable-no-undefined],
    [don't pass -no-undefined to libtool when linking Octave and its shared libraries])],
  [case $enableval in
     yes) NO_UNDEFINED_LDFLAG="-no-undefined" ;;
     no)  NO_UNDEFINED_LDFLAG="" ;;
     *) AC_MSG_ERROR([bad value $enableval for --disable-no-undefined]) ;;
   esac],
  [NO_UNDEFINED_LDFLAG="-no-undefined"])
AC_SUBST(NO_UNDEFINED_LDFLAG)

AC_ARG_ENABLE([link-all-dependencies],
  [AS_HELP_STRING([--enable-link-all-dependencies],
    [link Octave and its shared libraries with all dependencies, not just those immediately referenced (should not be needed on most systems)])],
  [case $enableval in
     yes) link_all_deps=true ;;
     no)  link_all_deps=false ;;
     *) AC_MSG_ERROR([bad value $enableval for --enable-link-all-depenencies])
     ;;
   esac],
  [link_all_deps=false])
AM_CONDITIONAL([AMCOND_LINK_ALL_DEPS], [test $link_all_deps = true])

## Dynamic linking is now enabled only if we are building shared
## libs and some API for dynamic linking has been detected.

## FIXME: A lot of the following duplicates the functionality of
## code generated by the dlopen option for LT_INIT.

LD_CXX='$(CXX)'
RDYNAMIC_FLAG=
DL_API_MSG=""
dlopen_api=false
shl_load_api=false
loadlibrary_api=false
dyld_api=false

if $SHARED_LIBS || $ENABLE_DYNAMIC_LINKING; then

  case $lt_cv_dlopen in
    dlopen)
      dlopen_api=true
      DL_API_MSG="(dlopen)"
      AC_DEFINE(HAVE_DLOPEN_API, 1,
        [Define to 1 if your system has dlopen, dlsym, dlerror, and dlclose for dynamic linking.])
      OCTAVE_CXX_FLAG([-rdynamic], [RDYNAMIC_FLAG=-rdynamic])
    ;;
    shl_load)
      shl_load_api=true
      DL_API_MSG="(shl_load)"
      AC_DEFINE(HAVE_SHL_LOAD_API, 1,
        [Define to 1 if your system has shl_load and shl_findsym for dynamic linking.])
    ;;
    LoadLibrary)
      loadlibrary_api=true
      DL_API_MSG="(LoadLibrary)"
      AC_DEFINE(HAVE_LOADLIBRARY_API, 1,
        [Define to 1 if your system has LoadLibrary for dynamic linking.])
    ;;
    dyld)
      dyld_api=true
      DL_API_MSG="(dyld)"
      AC_DEFINE(HAVE_DYLD_API, 1,
        [Define to 1 if your system has dyld for dynamic linking.])
    ;;
  esac

  DL_LIBS="$lt_cv_dlopen_libs"
  AC_SUBST(DL_LIBS)

  ## Disable dynamic linking if capability is not present.
  if $dlopen_api || $shl_load_api || $loadlibrary_api || $dyld_api; then
    :  # some form of dynamic linking present
  else
    ENABLE_DYNAMIC_LINKING=false
  fi
fi

if $ENABLE_DYNAMIC_LINKING; then
  AC_DEFINE(ENABLE_DYNAMIC_LINKING, 1, [Define to 1 if using dynamic linking.])
fi

AM_CONDITIONAL([AMCOND_ENABLE_DYNAMIC_LINKING],
  [test x"$ENABLE_DYNAMIC_LINKING" = x"true"])

if $SHARED_LIBS; then
  LIBOCTINTERP="-loctinterp$SHLLINKEXT"
  LIBOCTAVE="-loctave$SHLLINKEXT"
else
  LIBOCTINTERP='$(top_builddir)/libinterp/liboctinterp.$(LIBEXT)'
  LIBOCTAVE='$(top_builddir)/liboctave/liboctave.$(LIBEXT)'
fi

AC_SUBST(LD_CXX)
AC_SUBST(RDYNAMIC_FLAG)
AC_SUBST(ENABLE_DYNAMIC_LINKING)
AC_SUBST(LIBOCTINTERP)
AC_SUBST(LIBOCTAVE)

### Check for existence of various libraries

## OS-specific test for dirent, opendir.
case $host_os in
  mingw*)
    if test $have_msvc = yes; then
      AC_CHECK_LIB([dirent], [opendir])
      LIBS="$LIBS -ladvapi32 -lgdi32 -lws2_32 -luser32 -lkernel32"
    else
      LIBS="$LIBS -lgdi32 -lws2_32 -luser32 -lkernel32"
    fi
    LIBS="$LIBS -lgdi32 -lws2_32 -luser32 -lkernel32"
  ;;
  msdosmsvc)
    AC_CHECK_LIB([dirent], [opendir])
    LIBS="$LIBS -ladvapi32 -lgdi32 -lws2_32 -luser32 -lkernel32"
  ;;
esac

## Find a termlib to use.
OCTAVE_CHECK_LIB_TERMLIB

### Checks for header files.

AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT

## C headers

dnl Use multiple AC_CHECKs to avoid line continuations '\' in list
AC_CHECK_HEADERS([curses.h direct.h dlfcn.h floatingpoint.h grp.h])
AC_CHECK_HEADERS([ieeefp.h inttypes.h locale.h memory.h ncurses.h])
AC_CHECK_HEADERS([poll.h pthread.h pwd.h sunmath.h sys/ioctl.h])
AC_CHECK_HEADERS([sys/param.h sys/poll.h sys/resource.h ])
AC_CHECK_HEADERS([sys/select.h sys/utsname.h termcap.h])

## C++ headers

AC_LANG_PUSH(C++)

AC_CHECK_HEADERS([sstream])
OCTAVE_UNORDERED_MAP_HEADERS

AC_LANG_POP(C++)

## Find a termio header to include.

AC_CHECK_HEADERS([termios.h], have_termios_h=yes, have_termios_h=no)
AC_CHECK_HEADERS([termio.h], have_termio_h=yes, have_termio_h=no)
AC_CHECK_HEADERS([sgtty.h], have_sgtty_h=yes, have_sgtty_h=no)
AC_CHECK_HEADERS([fnmatch.h], have_fnmatch_h=yes, have_fnmatch_h=no)
AC_CHECK_HEADERS([conio.h], have_conio_h=yes, have_conio_h=no)

if test $have_termios_h != yes \
    && test $have_termio_h != yes \
    && test $have_sgtty_h != yes; then
  AC_MSG_WARN([I couldn't find termios.h, termio.h, or sgtty.h!])
fi

## For MSVC compilers, avoid #define of min/max from windows.h header
if test $have_msvc = yes; then
  AC_DEFINE(NOMINMAX, 1, [Define to 1 if you want to avoid min/max macro definition in Windows headers.])
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
AC_CHECK_TYPES([long long int, unsigned long long int])
AC_CHECK_TYPES([ptrdiff_t])

## How big are ints and how are they oriented?
## These could probably be eliminated in favor of run-time checks.

AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long long])
## Check for long double type (for 64-bit integers)
AC_CHECK_SIZEOF([long double])

### Check structures and existence of necessary members

AC_CHECK_MEMBERS([struct stat.st_blksize, struct stat.st_blocks,
                  struct stat.st_rdev])
AC_CHECK_MEMBERS([struct group.gr_passwd])

AC_STRUCT_TIMEZONE

### Check compiler characteristics.

## Does compiler have support for new friend template declarations?
OCTAVE_CXX_NEW_FRIEND_TEMPLATE_DECL

## Does reinterpret_cast fail for function pointers?
OCTAVE_CXX_BROKEN_REINTERPRET_CAST

## Check if C++ compiler allows placement delete.
OCTAVE_CXX_PLACEMENT_DELETE

## Check if C++ compiler can auto allocate variable sized arrays.
OCTAVE_CXX_DYNAMIC_AUTO_ARRAYS

## Check that C compiler and libraries support IEEE754 data format.
OCTAVE_IEEE754_DATA_FORMAT

## Is C++ runtime library ISO compliant?
OCTAVE_CXX_ISO_COMPLIANT_LIBRARY

## Are bit_and, bit_or, and bit_xor defined as templated operators?
OCTAVE_CXX_BITWISE_OP_TEMPLATES

## Can complex class set components independently?
OCTAVE_CXX_COMPLEX_SETTERS

## Are there functions to access real/imag parts of numbers via references?
OCTAVE_CXX_COMPLEX_REFERENCE_ACCESSORS

## Check if fast integer arithmetics based on bit tricks is available.
OCTAVE_FAST_INT_OPS

## Does the C compiler handle alloca() and const correctly?
AC_FUNC_ALLOCA

## Does the C compiler support Automake subdir-objects option?
AM_PROG_CC_C_O 

### gnulib initialization: part 2
### After all include and path modifications have taken place
### and at the same priority level as function checks.

gl_INIT

### Checks for functions and variables.

dnl These checks define/undefine HAVE_FUNCNAME in config.h.
dnl Code tests HAVE_FUNCNAME and either uses function or provides workaround.
dnl Use multiple AC_CHECKs to avoid line continuations '\' in list
AC_CHECK_FUNCS([canonicalize_file_name dup2])
AC_CHECK_FUNCS([endgrent endpwent execvp expm1 expm1f fork])
AC_CHECK_FUNCS([getegid geteuid getgid getgrent getgrgid getgrnam])
AC_CHECK_FUNCS([getpgrp getpid getppid getpwent getpwuid getuid])
AC_CHECK_FUNCS([isascii kill])
AC_CHECK_FUNCS([lgamma lgammaf lgamma_r lgammaf_r])
AC_CHECK_FUNCS([log1p log1pf pipe])
AC_CHECK_FUNCS([realpath resolvepath roundl])
AC_CHECK_FUNCS([select setgrent setpwent siglongjmp strsignal])
AC_CHECK_FUNCS([tempnam tgammaf toascii])
AC_CHECK_FUNCS([umask uname waitpid])
AC_CHECK_FUNCS([_kbhit])

dnl There are no workarounds in the code for missing these functions.
AC_CHECK_FUNCS([modf pow sqrt sqrtf], [],
               [AC_MSG_ERROR([Missing function required to build Octave])])

## exp2, round, tgamma function checks
AC_LANG_PUSH(C++)
AC_CHECK_DECLS([exp2, round, tgamma], [], [], [[#include <cmath>]])
AC_CHECK_FUNCS([exp2 round tgamma])
AH_VERBATIM([Z_FUNCS_AND_DECLS], [
#if defined (__cplusplus)
extern "C" {
#endif
#if HAVE_EXP2 && ! HAVE_DECL_EXP2
double exp2 (double);
#endif
#if HAVE_ROUND && ! HAVE_DECL_ROUND
double round (double);
#endif
#if HAVE_TGAMMA && ! HAVE_DECL_TGAMMA
double tgamma (double);
#endif
#if defined (__cplusplus)
}
#endif
])
AC_LANG_POP(C++)

## Look in <cmath> for the IEEE functions isnan, isinf, isfinite that we need.

OCTAVE_CHECK_FUNC_CMATH(isnan)
OCTAVE_CHECK_FUNC_CMATH(isinf)
OCTAVE_CHECK_FUNC_CMATH(isfinite)

## Check for Inf and NaN functions

case $canonical_host_type in
  m68k-hp-hpux*)
    ## I am told that Inf and NaN don't work on m68k HP sytems.
  ;;
  *)
    AC_CHECK_FUNCS([finite isnan isinf signbit])
    AC_CHECK_FUNCS([_finite _isnan])
    AC_CHECK_DECLS([signbit], , , [#include <math.h>])
  ;;
esac

## Check for nonstandard, but common math functions, that we need.

dnl Use multiple AC_CHECKs to avoid line continuations '\' in list
AC_CHECK_FUNCS([acosh acoshf asinh asinhf atanh atanhf cbrt cbrtf])
AC_CHECK_FUNCS([erf erff erfc erfcf exp2f hypotf _hypotf log2 log2f])

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

## Windows-specific tests for extra #defines
case $host_os in
  msdosmsvc | mingw*)
    AC_MSG_CHECKING([for required _WIN32_WINNT])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <windows.h>
        #if _WIN32_WINNT < 0x0403
        #error "Wrong version"
        #endif
        ]], [])],
      [AC_MSG_RESULT([none])],
      [AC_DEFINE(_WIN32_WINNT, 0x0403,
        [Define to 0x0403 to access InitializeCriticalSectionAndSpinCount.])
       AC_MSG_RESULT([0x0403])])
  ;;
esac

## Windows-specific use of functions
case $host_os in
  msdosmsvc | mingw*)
    AC_CHECK_FUNCS([setvbuf], [],
                   [AC_MSG_ERROR([Missing function required to build Octave])])
    ;;
esac

## Cygwin kluge for getrusage.
AC_CHECK_FUNCS([getrusage])
case $host_os in
  cygwin*)
    AC_DEFINE(RUSAGE_TIMES_ONLY, 1,
      [Define to 1 if your struct rusage only has time information.])
  ;;
esac

## Check for CGDisplayBitsPerPixel function on Mac OSX systems with Carbon
if test $have_framework_carbon = yes; then
  OCTAVE_CARBON_CGDISPLAYBITSPERPIXEL
fi

AC_CHECK_FUNCS([getpwnam], [], [AC_CHECK_LIB([sun], [getpwnam])])

AC_FUNC_CLOSEDIR_VOID

## Check return type of matherr() 
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
    [Define to 1 if math.h declares struct exception for matherr().])
fi
 
## Signal stuff.

AC_CHECK_DECLS([sys_siglist], [], [],
[[#include <signal.h>
/* NetBSD declares sys_siglist in unistd.h.  */
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
]])

### Need to disable building documentation if gnuplot was not found,
### unless it was already disabled previously.

if test -n "$DOCDIR" && test -n "$warn_gnuplot"; then
  DOCDIR=
  warn_docs="building documentation disabled because gnuplot was not found; make dist will fail"
  OCTAVE_CONFIGURE_WARNING([warn_docs])
fi
AM_CONDITIONAL([AMCOND_BUILD_DOCS], [test -n "$DOCDIR"])

### Maybe add -Wall, -W, and -Wshadow to compiler flags now that we're
### done feature testing. 

GCC_EXTRA_FLAGS="-Wall -W -Wshadow -Wformat -Wpointer-arith -Wmissing-prototypes -Wstrict-prototypes -Wwrite-strings -Wcast-align -Wcast-qual"

GXX_EXTRA_FLAGS="-Wall -W -Wshadow -Wold-style-cast -Wformat -Wpointer-arith -Wwrite-strings -Wcast-align -Wcast-qual"

try_extra_warning_flags=true

AC_ARG_ENABLE([extra-warning-flags],
  [AS_HELP_STRING([--disable-extra-warning-flags],
    [don't add -Wall, -W, -Wshadow, and -Wold-style-cast options to CFLAGS and CXXFLAGS])],
  [if test "$enableval" = no; then
     try_extra_warning_flags=false
   fi],
  [])

if $try_extra_warning_flags; then
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

GCC_STRICT_FLAGS="-Wconversion"

GXX_STRICT_FLAGS="-Wconversion -Weffc++"

try_strict_warning_flags=false

AC_ARG_ENABLE([strict-warning-flags],
  [AS_HELP_STRING([--enable-strict-warning-flags],
    [add extra strict warning options to CFLAGS and CXXFLAGS])],
  [if test "$enableval" = yes; then
     try_strict_warning_flags=true
   fi],
  [])

if $try_strict_warning_flags; then
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

AC_SUBST(WARN_CFLAGS)
AC_SUBST(WARN_CXXFLAGS)

### Check for Java.

build_java=true
AC_ARG_ENABLE([java],
  [AS_HELP_STRING([--disable-java],
    [disable Java interface])],
  [if test "$enableval" = no; then
     build_java=false
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

## Fake loop so that "break" can be used to skip code blocks.
while test $build_java = true
do
  ## Unset build_java.  Variable is set only if all configuration tests pass.
  build_java=false

  ## Warn if JAVA_HOME is unset.  It is *strongly* advised to specify JAVA_HOME.
  if test -z "$JAVA_HOME"; then
    AC_MSG_WARN([JAVA_HOME environment variable not initialized.])
    AC_MSG_WARN([Auto-detection will proceed but is unreliable.])
  fi

  ## Search for a viable Java executable.
  if test -z "$JAVA_HOME"; then
    JAVA_PATH="$PATH"
  else
    JAVA_PATH="${JAVA_HOME}$PATH_SEPARATOR${JAVA_HOME}/jre/bin$PATH_SEPARATOR${JAVA_HOME}/bin$PATH_SEPARATOR${JAVA_HOME}/../bin$PATH_SEPARATOR${PATH}"
  fi
  AC_PATH_PROG(JAVA, java, [], [$JAVA_PATH])

  if test -z "$JAVA"; then
    AC_MSG_WARN([No Java executable found.  Octave will not be able to call Java methods.])   
    break
  fi

  if test -z "$JAVA_HOME"; then
    ## Find JAVA_HOME for JRE by running java and querying properties.
    JAVA_TMP_HOME=`"$JAVA" -classpath ${srcdir}/build-aux OctJavaQry JAVA_HOME`
    ## Strip directory back to top-level installation dir (JAVA_HOME for JDK).
    JAVA_HOME=`echo $JAVA_TMP_HOME | sed -e 's|[[/\\]]bin[[/\\]]\?$||' | sed -e 's|[[/\\]]jre[[/\\]]\?$||'`
  fi

  case $host_os in
    mingw* | msdosmsvc)
      ## Under Win32 platform, we want JAVA_HOME to be in MSYS format, that is
      ## without colon and backslashes, as it is also used as path separator.
      ## Use quoted paths as Java may be installed in a path with whitespaces
      ## (e.g. C:\Program Files\Java\...).
      if test -n "$JAVA_HOME"; then
        JAVA_HOME=`cd "$JAVA_HOME" && pwd`
        ## Maybe this will be useful in the future, as native Java won't
        ## understand MSYS paths.
        JAVA_HOME_NATIVE=`cd "$JAVA_HOME" && pwd -W`
      fi
    ;;
    *)
      JAVA_HOME_NATIVE="$JAVA_HOME"
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
    AC_MSG_WARN([No javac compiler or jar executable found.  Octave will not be able to call Java methods.])
    break
  fi

  ## Check Java version is recent enough.
  AC_MSG_CHECKING([for Java version])
  java_version=[`"$JAVA" -version 2>&1 | sed -n -e 's/^java version[^0-9"]*"\([^"]*\)"/\1/p'`]
  AC_MSG_RESULT([$java_version])
  java_major=[`echo $java_version | sed -e 's/^\([0-9][0-9]*\)\.\([0-9][0-9]*\)\..*$/\1/'`]
  java_minor=[`echo $java_version | sed -e 's/^\([0-9][0-9]*\)\.\([0-9][0-9]*\)\..*$/\2/'`]
  if test $java_major -ge 1 && test $java_minor -ge 5; then
    :  # Version is ok.  Do nothing.
  else
    AC_MSG_WARN([Java version is too old (< 1.5).  Octave will not be able to call Java methods.])
    break
  fi

  ## At this point Win32/MSVC systems have enough configuration data.  We
  ## assume that all appropriate variables (e.g. INCLUDE and LIB) already have
  ## the required paths to compile and link against JDK.
  case $host_os in
    msdosmsvc)
      build_java=true
      JAVA_LIBS=-ladvapi32
      AC_DEFINE(HAVE_JAVA, 1,
        [Define to 1 if Java is available and is at least version 1.5])
      break
    ;;
    mingw*)
      if test $have_msvc = yes; then
        build_java=true
        JAVA_LIBS=-ladvapi32
        AC_DEFINE(HAVE_JAVA, 1,
          [Define to 1 if Java is available and is at least version 1.5])
        break
      fi
    ;;
  esac

  ## Determine which library file name to search for.
  case $host_os in
    darwin*)
      jvmlib=libjvm.dylib
    ;;
    mingw* | cygwin*)
      jvmlib=jvm.dll
    ;;
    *)
      jvmlib=libjvm.so
    ;;
  esac

  AC_MSG_CHECKING([for $jvmlib])

  if test -z "$JAVA_LDPATH"; then
    ## Run Java to try and determine library path to libjvm.so.
    JAVA_TMP_LDPATH=`$JAVA -classpath ${srcdir}/build-aux OctJavaQry JAVA_LDPATH`
    JAVA_TMP_LDPATH=`echo $JAVA_TMP_LDPATH | sed -e "s/${PATH_SEPARATOR}/ /g"`
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
    ## Java failed to find it's own library path.  Guess wildly.
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

  if test -z "$JAVA_LDPATH"; then
    AC_MSG_RESULT([not found])
    AC_MSG_WARN([Library $jvmlib not found.  Octave will not be able to call Java methods.])
    break
  else
    AC_MSG_RESULT([$JAVA_LDPATH])
  fi

  AC_MSG_CHECKING([for include file <jni.h>])

  ## Java and JVM found.  Set up flags.
  case $host_os in
    darwin*)
      ## Sneak the -framework flag into mkoctfile via LFLAGS
      LFLAGS="$LFLAGS -framework JavaVM"
      ## According to: http://developer.apple.com/unix/crossplatform.html
      ## one must explicitly set the include path.
      ## Unfortunately, the include path keeps moving around.
      if test -n "$JAVA_CPPFLAGS"; then
        JAVA_CPPFLAGS="-I${JAVA_CPPFLAGS}"
      else
        JAVA_CPPFLAGS="-I${JAVA_HOME}/include -I/System/Library/Frameworks/JavaVM.framework/Home/include -I/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Headers -I/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers"
      fi
      JAVA_LIBS="-framework JavaVM"
    ;;
    mingw* | cygwin*)
      if test -n "$JAVA_CPPFLAGS"; then
        JAVA_CPPFLAGS="-I${JAVA_CPPFLAGS}/include -I${JAVA_CPPFLAGS}/include/win32"
      else
        JAVA_CPPFLAGS="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/win32"
      fi
      JAVA_LIBS=-ladvapi32
    ;;
    *)
      if test -n "$JAVA_CPPFLAGS"; then
        JAVA_CPPFLAGS="-I${JAVA_CPPFLAGS}/include -I${JAVA_CPPFLAGS}/include/linux"
      else
        JAVA_CPPFLAGS="-I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux"
      fi
    ;;
  esac

  ## Verify jni.h include file exists.
  JNI_PATH=`echo $JAVA_CPPFLAGS | sed -e 's/-I//g'`
  have_jni=false
  for dir in $JNI_PATH; do 
    if test -f "${dir}/jni.h"; then have_jni=true; break; fi
  done
  if test $have_jni = true; then
    AC_MSG_RESULT([$dir])
  else
    AC_MSG_RESULT([not found])
    AC_MSG_WARN([Include file <jni.h> not found.  Octave will not be able to call Java methods.])
    break
  fi

  ## Passed all configuration tests.  A workable Java installation was found.
  build_java=true
  AC_DEFINE(HAVE_JAVA, 1,
    [Define to 1 if Java is available and is at least version 1.5])
  break
done

AM_CONDITIONAL([AMCOND_HAVE_JAVA], [test $build_java = true])
AC_SUBST(JAVA)
AC_SUBST(JAVAC)
AC_SUBST(JAR)
AC_SUBST(JAVA_CPPFLAGS)
AC_SUBST(JAVA_LIBS)
AC_DEFINE_UNQUOTED([JAVA_HOME], ["$JAVA_HOME"], [Java home (top-level installation dir)])
AC_DEFINE_UNQUOTED([JAVA_LDPATH], ["$JAVA_LDPATH"], [Java library path (libjvm)])

### GUI/Qt related tests.

QT_CPPFLAGS=
QT_LDFLAGS=
QT_LIBS=
win32_terminal=no
build_gui=yes
GUIDIR=libgui
AC_ARG_ENABLE([gui],
  [AS_HELP_STRING([--disable-gui], [don't build the GUI])],
  [if test "$enableval" = no; then build_gui=no; fi], [])

if test $build_gui = yes; then
  ## Check for Qt libraries
  PKG_CHECK_MODULES(QT, [QtCore, QtGui, QtNetwork],
    [],
    [AC_MSG_WARN([Qt libraries not found -- disabling GUI])
     build_gui=no])

  if test $build_gui = yes; then
    ## Retrieve Qt compilation and linker flags
    QT_CPPFLAGS="`$PKG_CONFIG --cflags-only-I QtCore QtGui QtNetwork`"
    QT_LDFLAGS="`$PKG_CONFIG --libs-only-L QtCore QtGui QtNetwork`"
    QT_LIBS="`$PKG_CONFIG --libs-only-l QtCore QtGui QtNetwork`"

    ## Check for Qt4
    if ! `$PKG_CONFIG --atleast-version=4.0.0 QtCore`; then
      AC_MSG_WARN([Qt >= 4.0.0 not found -- disabling GUI])
      build_gui=no
    fi
  fi

  if test $build_gui = yes; then
    AC_CHECK_PROGS(MOC, [moc-qt5 moc-qt4 moc])
    AC_CHECK_PROGS(UIC, [uic-qt5 uic-qt4 uic])
    AC_CHECK_PROGS(RCC, [rcc])
    if test -n "$MOC" && test -n "$UIC" && test -n "$RCC"; then
      AC_DEFINE(HAVE_QT, 1, 
        [Define to 1 if Qt is available (libraries, developer header files, utility programs (moc, uic, and rcc))])
    else
      AC_MSG_WARN([Qt utility programs moc, uic, and rcc not found -- disabling GUI])
      build_gui=no
    fi
  fi

  if test $build_gui = yes; then
    OCTAVE_CHECK_FUNC_SETPLACEHOLDERTEXT
  fi

  if test $build_gui = yes; then
    ## Check for Qscintilla library which is used in the GUI editor. 
    AC_CACHE_CHECK([whether Qscintilla library is installed],
      [octave_cv_lib_qscintilla],
      [save_CPPFLAGS="$CPPFLAGS"
      save_LDFLAGS="$LDFLAGS"
      save_LIBS="$LIBS"
      CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
      LDFLAGS="$QT_LDFLAGS $LDFLAGS"
      LIBS="$QT_LIBS -lqscintilla2"
      AC_LANG_PUSH(C++)
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <Qsci/qscilexersql.h>
        ]], [[
        QsciLexerSQL sqlLexer(0);
        ]])],
        octave_cv_lib_qscintilla=yes,
        octave_cv_lib_qscintilla=no)
      CPPFLAGS="$save_CPPFLAGS"
      LDFLAGS="$save_LDFLAGS"
      LIBS="$save_LIBS"
      AC_LANG_POP([C++])
    ])
    if test $octave_cv_lib_qscintilla = no; then
      AC_MSG_WARN([Qscintilla library not found -- disabling built-in GUI editor])
    else
      ## Let's assume Qscintilla library is at the same location as
      ## other regular Qt libraries.
      QT_LIBS="$QT_LIBS -lqscintilla2"
      OCTAVE_CHECK_FUNC_FINDFIRST_MODERN
      AC_DEFINE(HAVE_QSCINTILLA, 1, 
        [Define to 1 if the QScintilla library and header files are available])
    fi

    AC_CHECK_FUNCS([setlocale], [],
      [AC_MSG_WARN([setlocale not found -- disabling GUI])
       build_gui=no])

    if test $build_gui = yes; then
      case $host_os in
        mingw* | msdosmsvc*)
          AC_CHECK_FUNCS([setvbuf], [win32_terminal=yes],
            [AC_MSG_WARN([setvbuf not found -- disabling GUI])
             build_gui=no])
          ;;
        *)
          AC_CHECK_HEADERS([pty.h libutil.h util.h])
          AC_SEARCH_LIBS([openpty], [util],
            [AC_DEFINE(HAVE_OPENPTY, [], [Define whether openpty exists])])
          AC_CHECK_FUNCS([chmod chown ftruncate mmap munmap], [],
            [AC_MSG_ERROR([At least one of chmod, chown, ftruncate, mmap, and munmap not found -- disabling GUI])
             build_gui=no])
          ;;
      esac
    fi
  fi
fi

if test $build_gui = no; then
  ## GUI disabled.  Eliminate building GUIDIR directory
  GUIDIR=
fi
AM_CONDITIONAL([AMCOND_BUILD_GUI], [test $build_gui = yes])
AM_CONDITIONAL([WIN32_TERMINAL], [test $win32_terminal = yes])
AC_SUBST(QT_CPPFLAGS)
AC_SUBST(QT_LDFLAGS)
AC_SUBST(QT_LIBS)
AC_SUBST(GUIDIR)

### Run configure in subdirectories.

export CC
export CXX
export F77

### Some things to add to the bottom of config.h.

dnl ------------------------------------------------------------

AH_BOTTOM([
#if !defined (GNULIB_NAMESPACE)
#define GNULIB_NAMESPACE gnulib
#endif

#if defined (__GNUC__)
#define GCC_ATTR_DEPRECATED __attribute__ ((__deprecated__))
#define HAVE_ATTR_DEPRECATED

#define GCC_ATTR_NORETURN __attribute__ ((__noreturn__))
#define HAVE_ATTR_NORETURN

#define GCC_ATTR_UNUSED __attribute__ ((__unused__))
#define HAVE_ATTR_UNUSED
#else
#define GCC_ATTR_DEPRECATED
#define GCC_ATTR_NORETURN
#define GCC_ATTR_UNUSED
#endif

#define X_CAST(T, E) (T) (E)

#if defined (CXX_BROKEN_REINTERPRET_CAST)
#define FCN_PTR_CAST(T, E) (T) (E)
#else
#define FCN_PTR_CAST(T, E) reinterpret_cast<T> (E)
#endif

#if ! defined (HAVE_DEV_T)
typedef short dev_t;
#endif

#if ! defined (HAVE_INO_T)
typedef unsigned long ino_t;
#endif

#if defined (_MSC_VER)
#define __WIN32__
#define WIN32
/* missing parameters in macros */
#pragma warning (disable: 4003)
/* missing implementations in template instantiation */
#pragma warning (disable: 4996)
/* deprecated function names (FIXME?) */
#pragma warning (disable: 4661)
#endif

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#define OCTAVE_HAVE_WINDOWS_FILESYSTEM 1
#elif defined (__CYGWIN__)
#define OCTAVE_HAVE_WINDOWS_FILESYSTEM 1
#define OCTAVE_HAVE_POSIX_FILESYSTEM 1
#else
#define OCTAVE_HAVE_POSIX_FILESYSTEM 1
#endif

/* Define to 1 if we expect to have <windows.h>, Sleep, etc. */
#if defined (__WIN32__) && ! defined (__CYGWIN__)
#define OCTAVE_USE_WINDOWS_API 1
#endif

#if defined (__APPLE__) && defined (__MACH__)
#define OCTAVE_USE_OS_X_API 1
#endif

/* sigsetjmp is a macro, not a function. */
#if defined (sigsetjmp) && defined (HAVE_SIGLONGJMP)
#define OCTAVE_HAVE_SIG_JUMP
#endif

#if defined (_UNICOS)
#define F77_USES_CRAY_CALLING_CONVENTION
#endif

#if 0
#define F77_USES_VISUAL_FORTRAN_CALLING_CONVENTION
#endif

#ifdef USE_64_BIT_IDX_T
#define SIZEOF_OCTAVE_IDX_TYPE 8
#else
#define SIZEOF_OCTAVE_IDX_TYPE SIZEOF_INT
#endif

/* To be able to use long doubles for 64-bit mixed arithmetics, we need
   them at least 80 bits wide and we need roundl declared in math.h.
   FIXME: Maybe substitute this by a more precise check in the future?  */
#if (SIZEOF_LONG_DOUBLE >= 10) && defined (HAVE_ROUNDL)
#define OCTAVE_INT_USE_LONG_DOUBLE
#endif

#define OCTAVE_EMPTY_CPP_ARG

/* Octave is currently unable to use FFTW unless both float
   and double versions are available.  */
#if defined (HAVE_FFTW3) && defined (HAVE_FFTW3F)
#define HAVE_FFTW
#endif

/* Backward compatibility.  */
#if defined (HAVE_Z)
#define HAVE_ZLIB
#endif

/* oct-dlldefs.h */

#if defined (_MSC_VER)
#define OCTAVE_EXPORT __declspec(dllexport)
#define OCTAVE_IMPORT __declspec(dllimport)
#else
/* All other compilers, at least for now. */
#define OCTAVE_EXPORT
#define OCTAVE_IMPORT
#endif

/* API macro for libcruft */
#ifdef CRUFT_DLL
#define CRUFT_API OCTAVE_EXPORT
#else
#define CRUFT_API OCTAVE_IMPORT
#endif

/* API macro for liboctave */
#ifdef OCTAVE_DLL
#define OCTAVE_API OCTAVE_EXPORT
#else
#define OCTAVE_API OCTAVE_IMPORT
#endif

/* API macro for libinterp */
#ifdef OCTINTERP_DLL
#define OCTINTERP_API OCTAVE_EXPORT
#else
#define OCTINTERP_API OCTAVE_IMPORT
#endif

/* API macro for libinterp/graphics */
#ifdef OCTGRAPHICS_DLL
#define OCTGRAPHICS_API OCTAVE_EXPORT
#else
#define OCTGRAPHICS_API OCTAVE_IMPORT
#endif

/* API macro for libgui */
#ifdef OCTGUI_DLL
#define OCTGUI_API OCTAVE_EXPORT
#else
#define OCTGUI_API OCTAVE_IMPORT
#endif

/* oct-types.h */

typedef OCTAVE_IDX_TYPE octave_idx_type;

#include <stdint.h>

/* Tag indicating Octave config.h has been included */
#define OCTAVE_CONFIG_INCLUDED 1
])

dnl end of AH_BOTTOM
dnl ------------------------------------------------------------

### Make all AC_DEFINES available to testif feature of test.m function.
### This must reside at the bottom of configure.ac after all AC_DEFINES
### have been made.
### Use UGLY_DEFS to hold all the -D options.  These are ultimately placed
### in the DEFS field of the struct returned by octave_config_info, which
### is used by test.m.

AC_OUTPUT_MAKE_DEFS

## We have to insert extra levels of backslash quoting here so that
## the right thing ends up in oct-conf.h.
UGLY_DEFS=`echo $DEFS | $SED 's,\\",\\\\\\\\\\\\\\\\\\",g'`
AC_MSG_NOTICE([defining UGLY_DEFS to be $UGLY_DEFS])
AC_SUBST(UGLY_DEFS)

### Do the substitutions in all the Makefiles.

AC_SUBST(ac_config_files)
AC_SUBST(ac_config_headers)

AC_CONFIG_FILES([
  Makefile 
  doc/Makefile
  doc/icons/Makefile
  doc/interpreter/Makefile
  doc/liboctave/Makefile
  doc/refcard/Makefile
  examples/Makefile
  libgnu/Makefile
  libgui/Makefile
  libinterp/Makefile
  liboctave/Makefile
  liboctave/cruft/Makefile
  liboctave/cruft/mkf77def
  scripts/Makefile
  src/Makefile
  test/Makefile])

AC_OUTPUT

### Print a summary so that important information isn't missed.

AC_MSG_NOTICE([

Octave is now configured for $canonical_host_type

  Source directory:            $srcdir
  Installation prefix:         $prefix
  C compiler:                  $CC $XTRA_CFLAGS $WARN_CFLAGS $CFLAGS
  C++ compiler:                $CXX $XTRA_CXXFLAGS $WARN_CXXFLAGS $CXXFLAGS
  Fortran compiler:            $F77 $FFLAGS
  Fortran libraries:           $FLIBS
  Lex libraries:               $LEXLIB
  LIBS:                        $LIBS

  AMD CPPFLAGS:                $AMD_CPPFLAGS
  AMD LDFLAGS:                 $AMD_LDFLAGS
  AMD libraries:               $AMD_LIBS
  ARPACK CPPFLAGS:             $ARPACK_CPPFLAGS
  ARPACK LDFLAGS:              $ARPACK_LDFLAGS
  ARPACK libraries:            $ARPACK_LIBS
  BLAS libraries:              $BLAS_LIBS
  CAMD CPPFLAGS:               $CAMD_CPPFLAGS
  CAMD LDFLAGS:                $CAMD_LDFLAGS
  CAMD libraries:              $CAMD_LIBS
  CARBON libraries:            $CARBON_LIBS
  CCOLAMD CPPFLAGS:            $CCOLAMD_CPPFLAGS
  CCOLAMD LDFLAGS:             $CCOLAMD_LDFLAGS
  CCOLAMD libraries:           $CCOLAMD_LIBS
  CHOLMOD CPPFLAGS:            $CHOLMOD_CPPFLAGS
  CHOLMOD LDFLAGS:             $CHOLMOD_LDFLAGS
  CHOLMOD libraries:           $CHOLMOD_LIBS
  COLAMD CPPFLAGS:             $COLAMD_CPPFLAGS
  COLAMD LDFLAGS:              $COLAMD_LDFLAGS
  COLAMD libraries:            $COLAMD_LIBS
  CURL CPPFLAGS:               $CURL_CPPFLAGS
  CURL LDFLAGS:                $CURL_LDFLAGS
  CURL libraries:              $CURL_LIBS
  CXSPARSE CPPFLAGS:           $CXSPARSE_CPPFLAGS
  CXSPARSE LDFLAGS:            $CXSPARSE_LDFLAGS
  CXSPARSE libraries:          $CXSPARSE_LIBS
  DL libraries:                $DL_LIBS
  FFTW3 CPPFLAGS:              $FFTW3_CPPFLAGS
  FFTW3 LDFLAGS:               $FFTW3_LDFLAGS
  FFTW3 libraries:             $FFTW3_LIBS
  FFTW3F CPPFLAGS:             $FFTW3F_CPPFLAGS
  FFTW3F LDFLAGS:              $FFTW3F_LDFLAGS
  FFTW3F libraries:            $FFTW3F_LIBS
  fontconfig CFLAGS:           $FONTCONFIG_CFLAGS
  fontconfig libraries:        $FONTCONFIG_LIBS
  FreeType2 CFLAGS:            $FT2_CFLAGS
  FreeType2 libraries:         $FT2_LIBS
  GLPK CPPFLAGS:               $GLPK_CPPFLAGS
  GLPK LDFLAGS:                $GLPK_LDFLAGS
  GLPK libraries:              $GLPK_LIBS
  graphics CFLAGS:             $GRAPHICS_CFLAGS
  graphics libraries:          $GRAPHICS_LIBS
  HDF5 CPPFLAGS:               $HDF5_CPPFLAGS
  HDF5 LDFLAGS:                $HDF5_LDFLAGS
  HDF5 libraries:              $HDF5_LIBS
  Java home:                   $JAVA_HOME
  Java JVM path:               $JAVA_LDPATH
  Java CPPFLAGS:               $JAVA_CPPFLAGS
  Java libraries:              $JAVA_LIBS
  LAPACK libraries:            $LAPACK_LIBS
  LLVM CPPFLAGS:               $LLVM_CPPFLAGS
  LLVM LDFLAGS:                $LLVM_LDFLAGS
  LLVM libraries:              $LLVM_LIBS
  Magick++ CPPFLAGS:           $MAGICK_CPPFLAGS
  Magick++ LDFLAGS:            $MAGICK_LDFLAGS
  Magick++ libraries:          $MAGICK_LIBS
  OPENGL libraries:            $OPENGL_LIBS
  PTHREAD flags:               $PTHREAD_CFLAGS
  PTHREAD libraries:           $PTHREAD_LIBS
  QHULL CPPFLAGS:              $QHULL_CPPFLAGS
  QHULL LDFLAGS:               $QHULL_LDFLAGS
  QHULL libraries:             $QHULL_LIBS
  QRUPDATE CPPFLAGS:           $QRUPDATE_CPPFLAGS
  QRUPDATE LDFLAGS:            $QRUPDATE_LDFLAGS
  QRUPDATE libraries:          $QRUPDATE_LIBS
  Qt CPPFLAGS:                 $QT_CPPFLAGS
  Qt LDFLAGS:                  $QT_LDFLAGS
  Qt libraries:                $QT_LIBS
  READLINE libraries:          $READLINE_LIBS
  REGEX libraries:             $REGEX_LIBS
  TERM libraries:              $TERM_LIBS
  UMFPACK CPPFLAGS:            $UMFPACK_CPPFLAGS
  UMFPACK LDFLAGS:             $UMFPACK_LDFLAGS
  UMFPACK libraries:           $UMFPACK_LIBS
  X11 include flags:           $X11_INCFLAGS
  X11 libraries:               $X11_LIBS
  Z CPPFLAGS:                  $Z_CPPFLAGS
  Z LDFLAGS:                   $Z_LDFLAGS
  Z libraries:                 $Z_LIBS

  Default pager:               $DEFAULT_PAGER
  gnuplot:                     $GNUPLOT

  Build Octave GUI:                   $build_gui
  JIT compiler for loops:             $build_jit
  Build Java interface:               $build_java
  Do internal array bounds checking:  $BOUNDS_CHECKING
  Use octave_allocator:               $USE_OCTAVE_ALLOCATOR
  Build static libraries:             $STATIC_LIBS
  Build shared libraries:             $SHARED_LIBS
  Dynamic Linking:                    $ENABLE_DYNAMIC_LINKING $DL_API_MSG
  Include support for GNU readline:   $USE_READLINE
  64-bit array dims and indexing:     $USE_64_BIT_IDX_T
])

warn_msg_printed=false

OCTAVE_CONFIGURE_WARNING_SUMMARY

if $ENABLE_DYNAMIC_LINKING; then
  if test $SHARED_LIBS = false; then
    AC_MSG_WARN([You used --enable-dl but not --enable-shared.])
    AC_MSG_WARN([Are you sure that is what you want to do?])
    warn_msg_printed=true
  fi
fi

if $USE_64_BIT_IDX_T; then
  AC_MSG_WARN([])
  AC_MSG_WARN([You used the EXPERIMENTAL --enable-64 option.])
  AC_MSG_WARN([Are you sure that is what you want to do?])
  AC_MSG_WARN([])
  AC_MSG_WARN([Your Fortran compiler must have an option to generate])
  AC_MSG_WARN([code with 8 byte signed INTEGER values.  This option])
  AC_MSG_WARN([should be specified in the F77_INTEGER_8_FLAG variable])
  AC_MSG_WARN([Make.  This should work automatically for gfortran.  If])
  AC_MSG_WARN([you use another compiler, you will need to set this])
  AC_MSG_WARN([variable on the configure command line.  You must also])
  AC_MSG_WARN([compile the ARPACK, BLAS, LAPACK, QRUPDATE, and SuiteSparse])
  AC_MSG_WARN([libraries to use 8 byte signed integers for array indexing.])
  AC_MSG_WARN([])
  warn_msg_printed=true
fi

if $USE_OPENMP; then
  AC_MSG_WARN([])
  AC_MSG_WARN([You used the EXPERIMENTAL --enable-openmp option.])
  AC_MSG_WARN([Are you sure that is what you want to do?])
  AC_MSG_WARN([])
  AC_MSG_WARN([This option enables experimental SMP multithreding])
  AC_MSG_WARN([code that has had very little testing.  There is no])
  AC_MSG_WARN([certainity that the results returned by Octave with])
  AC_MSG_WARN([this option enabled will be correct.])
  AC_MSG_WARN([])
  warn_msg_printed=true
fi

if test $native_graphics = false; then
  AC_MSG_WARN([])
  AC_MSG_WARN([I didn't find the necessary libraries to compile native])
  AC_MSG_WARN([graphics.  It isn't necessary to have native graphics,])
  AC_MSG_WARN([but you will need to have gnuplot installed or you won't])
  AC_MSG_WARN([be able to use any of Octave's plotting commands])
  AC_MSG_WARN([])
  warn_msg_printed=true
fi

if test -n "$warn_gnuplot"; then
  if $native_graphics; then
    AC_MSG_WARN([])
    AC_MSG_WARN([I didn't find gnuplot.  Plotting commands will use the])
    AC_MSG_WARN([native graphics toolkit.])
  else
    AC_MSG_WARN([I didn't find gnuplot.  It isn't necessary to have gnuplot])
    AC_MSG_WARN([installed, but you won't be able to use any of Octave's])
    AC_MSG_WARN([plotting commands without it.])
  fi
  AC_MSG_WARN([])
  AC_MSG_WARN([If gnuplot is installed, but isn't in your path, you can])
  AC_MSG_WARN([tell Octave where to find it by using the gnuplot_binary])
  AC_MSG_WARN([function.  For example,])
  AC_MSG_WARN([])
  AC_MSG_WARN([gnuplot_binary ("/full/path/and/name/of/gnuplot/binary")])
  AC_MSG_WARN([])
  AC_MSG_WARN([at the Octave prompt.])
  AC_MSG_WARN([])
  AC_MSG_WARN([Setting default value to $GNUPLOT])
  AC_MSG_WARN([])

  warn_msg_printed=true
fi

if $USE_ATOMIC_REFCOUNT; then
  AC_MSG_WARN([])
  AC_MSG_WARN([Using atomic reference counting.])
  AC_MSG_WARN([This feature allows access to Octave data safely from])
  AC_MSG_WARN([another thread, for instance from a GUI.  However this])
  AC_MSG_WARN([results in a small performance penalty in the Octave])
  AC_MSG_WARN([interpreter.])
  AC_MSG_WARN([])
  if $USE_OCTAVE_ALLOCATOR; then
    AC_MSG_WARN([Thread-safe behavior is not guaranteed unless you also])
    AC_MSG_WARN([disable the use of the octave_allocator class.])
    AC_MSG_WARN([])
  fi
  warn_msg_printed=true
fi

if $warn_msg_printed; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([NOTE: Libraries or auxiliary programs may be skipped if they are])
  AC_MSG_NOTICE([NOTE: not found OR if they are missing required features on your])
  AC_MSG_NOTICE([NOTE: system. ])
fi

### End of configure.
