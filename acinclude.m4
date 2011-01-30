dnl aclocal.m4 -- extra macros for configuring Octave
dnl
dnl Copyright (C) 1995-2011 John W. Eaton
dnl 
dnl This file is part of Octave.
dnl 
dnl Octave is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at
dnl your option) any later version.
dnl 
dnl Octave is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with Octave; see the file COPYING.  If not, see
dnl <http://www.gnu.org/licenses/>.
dnl
dnl
dnl Copyright (C) 2008 - 2009 Free Software Foundation, Inc.
dnl
dnl If needed, define the m4_ifblank and m4_ifnblank macros from autoconf 2.64
dnl This allows us to run with earlier Autoconfs as well.
dnl FIXME: these should go away once Autoconf 2.64 is required or ubiquitous.
dnl
ifdef([m4_ifblank],[],[
m4_define([m4_ifblank],
[m4_if(m4_translit([[$1]],  [ ][	][
]), [], [$2], [$3])])])
dnl
ifdef([m4_ifnblank],[],[
m4_define([m4_ifnblank],
[m4_if(m4_translit([[$1]],  [ ][	][
]), [], [$3], [$2])])])
dnl
dnl ----------------------------------------------------------------------
dnl
dnl Figure out the hardware-vendor-os info.
dnl
dnl OCTAVE_HOST_TYPE
AC_DEFUN([OCTAVE_HOST_TYPE],
[AC_CANONICAL_HOST
if test -z "$host"; then
  host=unknown
fi
canonical_host_type=$host
if test "$host" = unknown; then
  AC_MSG_WARN([configuring Octave for unknown system type
])
fi
AC_SUBST(canonical_host_type)])
dnl
dnl Set default value for a variable and substitute it.
dnl
dnl OCTAVE_SET_DEFAULT
AC_DEFUN([OCTAVE_SET_DEFAULT],
[ifelse($#, 2, [: ${$1=$2}
])dnl
AC_MSG_RESULT([defining $1 to be $$1])
AC_SUBST($1)])
dnl
dnl Check for ar.
dnl
AC_DEFUN([OCTAVE_PROG_AR],
[if test -z "$AR"; then
  AR=ar
fi
AC_SUBST(AR)

if test -z "$ARFLAGS"; then
  ARFLAGS="rc"
fi
AC_SUBST(ARFLAGS)
])
dnl
dnl See if the compiler supports placement delete
dnl
AC_DEFUN([OCTAVE_PLACEMENT_DELETE],
[AC_CACHE_CHECK([whether <new> defines placement delete operator],
octave_cv_placement_delete,
[AC_LANG_PUSH(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <new>]],
[[operator delete((void *)0, (void *)0);]])],
octave_cv_placement_delete=yes, octave_cv_placement_delete=no)])
if test $octave_cv_placement_delete = yes; then
AC_DEFINE(HAVE_PLACEMENT_DELETE,1,[Define if C++ supports operator delete(void *, void *)])
fi
AC_LANG_POP(C++)
])
dnl
dnl See if the compiler dynamic auto arrays
dnl
AC_DEFUN([OCTAVE_DYNAMIC_AUTO_ARRAYS],
[AC_CACHE_CHECK([whether C++ supports dynamic auto arrays],
octave_cv_dynamic_auto_arrays,
[AC_LANG_PUSH(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
[[void test(char *); int length(); char x[length()]; test(x);]])],
octave_cv_dynamic_auto_arrays=yes, octave_cv_dynamic_auto_arrays=no)])
if test $octave_cv_dynamic_auto_arrays = yes; then
AC_DEFINE(HAVE_DYNAMIC_AUTO_ARRAYS,1,[Define if C++ supports dynamic auto arrays])
fi
AC_LANG_POP(C++)
])
dnl
dnl See if the C++ library has functions to set real and imaginary
dnl parts of complex numbers independently.
dnl
AC_DEFUN([OCTAVE_CXX_COMPLEX_SETTERS],
[AC_CACHE_CHECK([whether complex class can set components independently],
octave_cv_cxx_complex_setters,
[AC_LANG_PUSH(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <complex>]],
[[std::complex<double> x; x.real (1.0); x.imag (2.0);]])],
octave_cv_cxx_complex_setters=yes, octave_cv_cxx_complex_setters=no)])
if test $octave_cv_cxx_complex_setters = yes; then
AC_DEFINE(HAVE_CXX_COMPLEX_SETTERS,1,[Define if C++ complex class has void real (T) and void imag (T) methods])
fi
AC_LANG_POP(C++)
])
dnl
dnl See if the C++ library has functions to access real and imaginary
dnl parts of complex numbers independently via references.
dnl
AC_DEFUN([OCTAVE_CXX_COMPLEX_REFERENCE_ACCESSORS],
[AC_CACHE_CHECK([whether complex class can reference components independently],
octave_cv_cxx_complex_reference_accessors,
[AC_LANG_PUSH(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <complex>]],
[[std::complex<double> x; x.real () = 1.0; x.imag () = 1.0;]])],
octave_cv_cxx_complex_reference_accessors=yes, octave_cv_cxx_complex_reference_accessors=no)])
if test $octave_cv_cxx_complex_reference_accessors = yes; then
AC_DEFINE(HAVE_CXX_COMPLEX_REFERENCE_ACCESSORS,1,[Define if C++ complex class has T& real (void) and T& imag (void) methods])
fi
AC_LANG_POP(C++)
])
dnl
dnl The following test is from Karl Berry's Kpathseach library.  I'm
dnl including it here in case we someday want to make the use of
dnl kpathsea optional.
dnl
dnl Some BSD putenv's, e.g., FreeBSD, do malloc/free's on the environment.
dnl This test program is due to Mike Hibler <mike@cs.utah.edu>.
dnl We don't actually need to run this if we don't have putenv, but it
dnl doesn't hurt.
AC_DEFUN([OCTAVE_SMART_PUTENV],
[AC_MSG_CHECKING([whether putenv uses malloc])
AC_CACHE_VAL(octave_cv_func_putenv_malloc,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#define VAR	"YOW_VAR"
#define STRING1 "GabbaGabbaHey"
#define STRING2 "Yow!!"		/* should be shorter than STRING1 */
extern char *getenv (); /* in case char* and int don't mix gracefully */
main ()
{
  char *str1, *rstr1, *str2, *rstr2;
  str1 = getenv (VAR);
  if (str1)
    exit (1);
  str1 = malloc (strlen (VAR) + 1 + strlen (STRING1) + 1);
  if (str1 == 0)
    exit (2);
  strcpy (str1, VAR);
  strcat (str1, "=");
  strcat (str1, STRING1);
  if (putenv (str1) < 0)
    exit (3);
  rstr1 = getenv (VAR);
  if (rstr1 == 0)
    exit (4);
  rstr1 -= strlen (VAR) + 1;
  if (strncmp (rstr1, VAR, strlen (VAR)))
    exit (5);
  str2 = malloc (strlen (VAR) + 1 + strlen (STRING2) + 1);
  if (str2 == 0 || str1 == str2)
    exit (6);
  strcpy (str2, VAR);
  strcat (str2, "=");
  strcat (str2, STRING2);
  if (putenv (str2) < 0)
    exit (7);
  rstr2 = getenv (VAR);
  if (rstr2 == 0)
    exit (8);
  rstr2 -= strlen (VAR) + 1;
#if 0
  printf ("rstr1=0x%x, rstr2=0x%x\n", rstr1, rstr2);
  /*
   * If string from first call was reused for the second call,
   * you had better not do a free on the first string!
   */
  if (rstr1 == rstr2)
          printf ("#define SMART_PUTENV\n");
  else
          printf ("#undef SMART_PUTENV\n");
#endif
  exit (rstr1 == rstr2 ? 0 : 1);
}]])], octave_cv_func_putenv_malloc=yes, octave_cv_func_putenv_malloc=no,
    octave_cv_func_putenv_malloc=no)])dnl
AC_MSG_RESULT([$octave_cv_func_putenv_malloc])
if test $octave_cv_func_putenv_malloc = yes; then
  AC_DEFINE(SMART_PUTENV,1,[To quiet autoheader.])
fi])
dnl
dnl Check to see if C++ compiler needs the new friend template declaration 
dnl syntax. 
dnl
dnl OCTAVE_CXX_NEW_FRIEND_TEMPLATE_DECL
AC_DEFUN([OCTAVE_CXX_NEW_FRIEND_TEMPLATE_DECL], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING([for C++ support for new friend template declaration])
  AC_CACHE_VAL(octave_cv_cxx_new_friend_template_decl, [
    AC_LANG_PUSH(C++)
    rm -f conftest.h
    cat > conftest.h <<EOB
       struct A {
	 friend int operator== (const A&, const A&);
	 A (int) { }
       };

       template <class T> int
       operator== (const T&, const T&)
       {
	 return 0;
       }
EOB
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include "conftest.h"]],
      [[A a (1);
        return a == A(1);]])],
      [octave_cv_cxx_new_friend_template_decl=no],
      [octave_cv_cxx_new_friend_template_decl=yes])
    AC_LANG_POP(C++)
  ])
  AC_MSG_RESULT([$octave_cv_cxx_new_friend_template_decl])
  if test $octave_cv_cxx_new_friend_template_decl = yes; then
    AC_DEFINE(CXX_NEW_FRIEND_TEMPLATE_DECL,1,[Define if your compiler supports `<>' stuff for template friends.])
  fi
])
dnl
dnl Check to see if C compiler handles FLAG command line option.  If
dnl two arguments are specified, execute the second arg as shell
dnl commands.  Otherwise, add FLAG to CFLAGS if the compiler accepts
dnl the flag.
dnl
dnl OCTAVE_CC_FLAG
AC_DEFUN([OCTAVE_CC_FLAG], [
  ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${CC-cc} accepts $1])
  AC_CACHE_VAL(octave_cv_cc_flag_$ac_safe, [
    AC_LANG_PUSH(C)
    XCFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_cc_flag_$ac_safe=yes",
      eval "octave_cv_cc_flag_$ac_safe=no")
    CFLAGS="$XCFLAGS"
    AC_LANG_POP(C)
  ])
  if eval "test \"`echo '$octave_cv_cc_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , [
      CFLAGS="$CFLAGS $1"
      AC_MSG_RESULT([adding $1 to CFLAGS])], [$2])
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check to see if C++ compiler handles FLAG command line option.  If
dnl two arguments are specified, execute the second arg as shell
dnl commands.  Otherwise, add FLAG to CXXFLAGS if the compiler accepts
dnl the flag.
dnl
dnl OCTAVE_CXX_FLAG
AC_DEFUN([OCTAVE_CXX_FLAG], [
  ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${CXX-g++} accepts $1])
  AC_CACHE_VAL(octave_cv_cxx_flag_$ac_safe, [
    AC_LANG_PUSH(C++)
    XCXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_cxx_flag_$ac_safe=yes",
      eval "octave_cv_cxx_flag_$ac_safe=no")
    CXXFLAGS="$XCXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if eval "test \"`echo '$octave_cv_cxx_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , [
      CXXFLAGS="$CXXFLAGS $1"
      AC_MSG_RESULT([adding $1 to CXXFLAGS])], [$2])
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check to see if Fortran compiler handles FLAG command line option.  If
dnl two arguments are specified, execute the second arg as shell
dnl commands.  Otherwise, add FLAG to FFLAGS if the compiler accepts
dnl the flag.
dnl
dnl OCTAVE_F77_FLAG
AC_DEFUN([OCTAVE_F77_FLAG], [
  ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${F77-g77} accepts $1])
  AC_CACHE_VAL(octave_cv_f77_flag_$ac_safe, [
    AC_LANG_PUSH(Fortran 77)
    XFFLAGS="$FFLAGS"
    FFLAGS="$FFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_f77_flag_$ac_safe=yes",
      eval "octave_cv_f77_flag_$ac_safe=no")
    FFLAGS="$XFFLAGS"
    AC_LANG_POP(Fortran 77)
  ])
  if eval "test \"`echo '$octave_cv_f77_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , [
      FFLAGS="$FFLAGS $1"
      AC_MSG_RESULT([adding $1 to FFLAGS])], [$2])
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check to see whether the default Fortran INTEGER is 64 bits wide.
dnl
AC_DEFUN([OCTAVE_CHECK_FORTRAN_INTEGER_SIZE], [
  octave_fintsize_save_FFLAGS="$FFLAGS"
  FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"
  AC_LANG_PUSH(Fortran 77)
  AC_CACHE_CHECK([whether $F77 generates correct size integers],
                 [octave_cv_fortran_integer_size],
[AC_COMPILE_IFELSE(
[      subroutine foo(n, in, out)
      integer n, in(n), out(n)
      integer i
      do 10 i = 1, n
        out(i) = in(i)
   10 continue
      return
      end],
[mv conftest.$ac_objext fintsize.$ac_objext

  octave_fintsize_save_LIBS="$LIBS"
  LIBS="fintsize.$ac_objext $[]_AC_LANG_PREFIX[]LIBS"
  AC_LANG_PUSH(C)dnl
  AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <assert.h>]], [[
#ifdef USE_64_BIT_IDX_T
#if IDX_TYPE_LONG
  typedef long octave_idx_type;
#else
  typedef int octave_idx_type;
#endif
#else
  typedef int octave_idx_type;
#endif
  octave_idx_type n = 2;
  octave_idx_type in[2];
  octave_idx_type out[2];
  in[0] = 13;
  in[0] = 42;
  F77_FUNC(foo,FOO) (&n, &in, &out);
  assert (in[0] == out[0] && in[1] == out[1]);
]])],
  [octave_cv_fortran_integer_size=yes],
  [octave_cv_fortran_integer_size=no])
  AC_LANG_POP(C)dnl
  LIBS="$octave_fintsize_save_LIBS"
rm -f conftest.$ac_objext fintsize.$ac_objext
], [
  rm -f conftest.$ac_objext
  AC_MSG_FAILURE([cannot compile a simple Fortran program])
  octave_cv_fortran_integer_size=no])])
  AC_LANG_POP(Fortran 77)
  FFLAGS="$octave_fintsize_save_FFLAGS"
])
dnl
dnl
dnl
dnl OCTAVE_CHECK_LIBRARY(LIBRARY, DOC-NAME, WARN-MSG, HEADER, FUNC,
dnl                      LANG, DOC-STRING, EXTRA-CHECK)
AC_DEFUN([OCTAVE_CHECK_LIBRARY], [
  AC_ARG_WITH([$1-includedir],
    [AS_HELP_STRING([--with-$1-includedir=DIR],
      [look for $2 include files in DIR])],
    [m4_toupper([$1])_CPPFLAGS="-I$withval"])
  AC_SUBST(m4_toupper([$1])_CPPFLAGS)

  AC_ARG_WITH([$1-libdir],
    [AS_HELP_STRING([--with-$1-libdir=DIR],
      [look for $2 libraries in DIR])],
    [m4_toupper([$1])_LDFLAGS="-L$withval"])
  AC_SUBST(m4_toupper([$1])_LDFLAGS)

  AC_ARG_WITH([$1],
    [m4_ifblank([$7],
      [AS_HELP_STRING([--without-$1], [don't use $2 library])],
      [AS_HELP_STRING([--without-$1], [$7])])],
    with_$1=$withval, with_$1=yes)

  m4_toupper([$1])_LIBS=
  case $with_$1 in
    no)
      m4_toupper([$1])_LIBS=
    ;;
    yes | "")
      m4_toupper([$1])_LIBS="-l$1"
    ;;
    -* | */* | *.a | *.so | *.so.* | *.o)
      m4_toupper([$1])_LIBS="$with_$1"
    ;;
    *)
      m4_toupper([$1])_LIBS="-l$with_$1"
    ;;
  esac

  [TEXINFO_]m4_toupper([$1])=
  warn_$1="$3"
  if test -n "$m4_toupper([$1])_LIBS"; then
    octave_check_library_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$m4_toupper([$1])_CPPFLAGS $CPPFLAGS"
    m4_ifnblank([$6], [AC_LANG_PUSH($6)])
    octave_$1_check_for_lib=false
    m4_ifblank([$4], [octave_$1_check_for_lib=true],
               [AC_CHECK_HEADERS($4, [octave_$1_check_for_lib=true; break])])
    if $octave_$1_check_for_lib; then
      octave_check_library_save_LDFLAGS="$LDFLAGS"
      LDFLAGS="$m4_toupper([$1])_LDFLAGS $LDFLAGS"
      octave_check_library_save_LIBS="$LIBS"
      LIBS="$m4_toupper([$1])_LIBS $LIBS"
      octave_$1_ok=no
      AC_MSG_CHECKING([for $5 in $m4_toupper([$1])_LIBS])
      AC_LINK_IFELSE([AC_LANG_CALL([], [$5])],
	[octave_$1_ok=yes])
      AC_MSG_RESULT([$octave_$1_ok])
      if test $octave_$1_ok = yes; then
	m4_ifblank([$8], [
	  warn_$1=
	  AC_DEFINE([HAVE_]m4_toupper([$1]), 1,
            [Define if $2 is available.])
	  [TEXINFO_]m4_toupper([$1])="@set [HAVE_]m4_toupper([$1])"], [$8])
      fi
      LIBS="$octave_check_library_save_LIBS"
      LDFLAGS="$octave_check_library_save_LDFLAGS"
    fi
    m4_ifnblank([$6], [AC_LANG_POP($6)])
    CPPFLAGS="$octave_check_library_save_CPPFLAGS"
  fi
  AC_SUBST(m4_toupper([$1])_LIBS)
  AC_SUBST([TEXINFO_]m4_toupper([$1]))
  if test -n "$warn_$1"; then
    AC_MSG_WARN([$warn_$1])
    m4_toupper([$1])_LIBS=
  fi
])
dnl
dnl Check for flex
dnl
AC_DEFUN([OCTAVE_PROG_FLEX], [
### For now, don't define LEXLIB to be -lfl -- we don't use anything in
### it, and it might not be installed.
###
### Also make sure that we generate an interactive scanner if we are
### using flex.
  AC_PROG_LEX
  case "$LEX" in
    flex*)
      LFLAGS="-I"
      AC_MSG_RESULT([defining LFLAGS to be $LFLAGS])
      LEXLIB=
    ;;
    *)
      LEX='$(top_srcdir)/missing flex'
      warn_flex="I didn't find flex, but it's only a problem if you need to reconstruct lex.cc"
      AC_MSG_WARN([$warn_flex])
    ;;
  esac
  AC_SUBST(LFLAGS)
])
dnl
dnl Check for bison
dnl
AC_DEFUN([OCTAVE_PROG_BISON], [
  AC_PROG_YACC
  case "$YACC" in
    bison*)
    ;;
    *)
      YACC='$(top_srcdir)/missing bison'
      warn_bison="I didn't find bison, but it's only a problem if you need to reconstruct parse.cc"
      AC_MSG_WARN([$warn_bison])
    ;;
  esac
])
dnl
dnl What pager should we use?
dnl
AC_DEFUN([OCTAVE_PROG_PAGER],
[if test "$cross_compiling" = yes; then
  DEFAULT_PAGER=less
  AC_MSG_RESULT([assuming $DEFAULT_PAGER exists on $canonical_host_type host])
  AC_SUBST(DEFAULT_PAGER)
else
  octave_possible_pagers="less more page pg"
  case "$canonical_host_type" in
    *-*-cygwin* | *-*-mingw32* | *-*-msdosmsvc)
      octave_possible_pagers="$octave_possible_pagers more.com"
    ;;
  esac

  AC_CHECK_PROGS(DEFAULT_PAGER, $octave_possible_pagers, [])
  if test -z "$DEFAULT_PAGER"; then
    warn_less="I couldn't find \`less', \`more', \`page', or \`pg'"
    AC_MSG_WARN([$warn_less])
  fi
fi
])
dnl
dnl Does gnuplot exist?
dnl
AC_DEFUN([OCTAVE_PROG_GNUPLOT], [
gp_names="gnuplot"
gp_default="gnuplot"
if test "$cross_compiling" = yes; then
  GNUPLOT="$gp_default"
  AC_MSG_RESULT([assuming $GNUPLOT exists on $canonical_host_type host])
else
  AC_CHECK_PROGS(GNUPLOT, [$gp_names])
  if test -z "$GNUPLOT"; then
    warn_gnuplot=yes

    GNUPLOT="$gp_default"

    AC_MSG_WARN([gnuplot not found.  It isn't necessary to have gnuplot])
    AC_MSG_WARN([installed, but without native graphics or gnuplot])
	 AC_MSG_WARN([you won't be able to use any of Octave's plotting commands.])
  fi
fi
AC_SUBST(GNUPLOT)
])
dnl
dnl Is gperf installed?
dnl
dnl OCTAVE_PROG_GPERF
AC_DEFUN([OCTAVE_PROG_GPERF], [
  AC_CHECK_PROG(GPERF, gperf, gperf, [])
  if test -z "$GPERF"; then
    GPERF='$(top_srcdir)/missing gperf'
    warn_gperf="I didn't find gperf, but it's only a problem if you need to reconstruct oct-gperf.h"
    AC_MSG_WARN([$warn_gperf])
  fi
  AC_SUBST(GPERF)
])
dnl
dnl Is ghostscript installed?
dnl
dnl OCTAVE_PROG_GHOSTSCRIPT
AC_DEFUN([OCTAVE_PROG_GHOSTSCRIPT], [
  case "$canonical_host_type" in
    *-*-mingw* | *-*-msdosmsvc)
      gs_names="gswin32c gs"
    ;;
    *)
      gs_names="gs"
    ;;
  esac
  AC_CHECK_PROGS(GHOSTSCRIPT, [$gs_names])
  if test -z "$GHOSTSCRIPT"; then
    GHOSTSCRIPT='$(top_srcdir)/missing gs'
    warn_ghostscript="I didn't find ghostscript, so reconstructing figures for the manual will fail, and saving graphics in some output formats will fail when using Octave"
    AC_MSG_WARN([$warn_ghostscript])
  fi
  AC_SUBST(GHOSTSCRIPT)
])
dnl
dnl Is texi2dvi installed?
dnl
dnl OCTAVE_PROG_TEXI2DVI
AC_DEFUN([OCTAVE_PROG_TEXI2DVI], [
  AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi, [])
  if test -z "$TEXI2DVI"; then
    TEXI2DVI='$(top_srcdir)/missing texi2dvi'
    warn_texi2dvi="I didn't find texi2dvi, but it's only a problem if you need to reconstruct the DVI version of the manual"
    AC_MSG_WARN([$warn_texi2dvi])
  fi
  AC_SUBST(TEXI2DVI)
])
dnl
dnl Is texi2pdf installed?
dnl
dnl OCTAVE_PROG_TEXI2PDF
AC_DEFUN([OCTAVE_PROG_TEXI2PDF], [
  AC_REQUIRE([OCTAVE_PROG_TEXI2DVI])
  AC_CHECK_PROG(TEXI2PDF, texi2pdf, texi2pdf, [])
  if test -z "$TEXI2PDF"; then
    missing=true;
    if test -n "$TEXI2DVI"; then
      TEXI2PDF="$TEXI2DVI --pdf"
      missing=false;
    fi
  else
    missing=false;
  fi
  if $missing; then
    TEXI2PDF='$(top_srcdir)/missing texi2pdf'
    warn_texi2pdf="I didn't find texi2pdf, but it's only a problem if you need to reconstruct the PDF version of the manual"
    AC_MSG_WARN([$warn_texi2pdf])
  fi
  AC_SUBST(TEXI2PDF)
])
dnl
dnl See if the C++ library is ISO compliant.
dnl FIXME: This is obviously very simplistic, and trivially fooled.
dnl
dnl OCTAVE_CXX_ISO_COMPLIANT_LIBRARY
AC_DEFUN([OCTAVE_CXX_ISO_COMPLIANT_LIBRARY], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING([if C++ library is ISO compliant])
  AC_CACHE_VAL(octave_cv_cxx_iso_compliant_library, [
    AC_LANG_PUSH(C++)
    rm -f conftest.h
### Omitting cwctype for now, since it is broken with gcc-3.0.x and
### possibly other versions...
    for inc in algorithm bitset cassert cctype cerrno cfloat ciso646 \
	climits clocale cmath complex csetjmp csignal cstdarg cstddef \
	cstdio cstdlib cstring ctime cwchar deque exception \
	fstream functional iomanip ios iosfwd iostream istream iterator \
	limits list locale map memory new numeric ostream queue set \
	sstream stack stdexcept streambuf string strstream typeinfo \
	utility valarray vector; do
      echo "#include <$inc>" >> conftest.h
    done
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include "conftest.h"]],
      [[std::bitset<50> flags;
        flags.set();
        int digits = std::numeric_limits<unsigned long>::digits;
        digits = 0;]])],
      [octave_cv_cxx_iso_compliant_library=yes],
      [octave_cv_cxx_iso_compliant_library=no])
    AC_LANG_POP(C++)
  ])
  AC_MSG_RESULT([$octave_cv_cxx_iso_compliant_library])
  if test $octave_cv_cxx_iso_compliant_library = yes; then
    AC_DEFINE(CXX_ISO_COMPLIANT_LIBRARY, 1, [Define if your C++ runtime library is ISO compliant.])
  fi
])
dnl
dnl Allow the user disable support for command line editing using GNU
dnl readline.
dnl
dnl OCTAVE_ENABLE_READLINE
AC_DEFUN([OCTAVE_ENABLE_READLINE], [
  USE_READLINE=true
  READLINE_LIBS=
  AC_ARG_ENABLE(readline,
    [  --enable-readline       use readline library (default is yes)],
    [if test "$enableval" = no; then
       USE_READLINE=false
       warn_readline="command editing and history features require GNU Readline"
     fi])
  if $USE_READLINE; then
    save_LIBS="$LIBS"
    LIBS="$TERM_LIBS"
    AC_CHECK_LIB(readline, rl_set_keyboard_input_timeout, [
      READLINE_LIBS="-lreadline"
      AC_DEFINE(USE_READLINE, 1, [Define to use the readline library.])
    ], [
      AC_MSG_WARN([I need GNU Readline 4.2 or later])
      AC_MSG_ERROR([this is fatal unless you specify --disable-readline])
    ])
    LIBS="$save_LIBS"
  fi
  AC_SUBST(READLINE_LIBS)
])
dnl
dnl Check to see if C++ reintrepret cast works for function pointers.
dnl
dnl OCTAVE_CXX_BROKEN_REINTERPRET_CAST
dnl
AC_DEFUN([OCTAVE_CXX_BROKEN_REINTERPRET_CAST], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_LANG_PUSH(C++)
  AC_CACHE_CHECK([for broken C++ reinterpret_cast],
    octave_cv_cxx_broken_reinterpret_cast, [
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <cmath>]], [[
      typedef double (*fptr) (double);
      fptr psin = sin;
      void *vptr = reinterpret_cast<void *> (psin);
      psin = reinterpret_cast<fptr> (vptr);]])],
      octave_cv_cxx_broken_reinterpret_cast=no,
      octave_cv_cxx_broken_reinterpret_cast=yes)])
  if test $octave_cv_cxx_broken_reinterpret_cast = yes ; then
    AC_DEFINE(CXX_BROKEN_REINTERPRET_CAST, 1, [Define if C++ reinterpret_cast fails for function pointers.])
fi
  AC_LANG_POP(C++)])
dnl
dnl Find find.
dnl
# Prefer GNU find if found.
AN_MAKEVAR([FIND],  [OCTAVE_PROG_FIND])
AN_PROGRAM([gfind], [OCTAVE_PROG_FIND])
AN_PROGRAM([find],  [OCTAVE_PROG_FIND])
AC_DEFUN([OCTAVE_PROG_FIND],
[AC_CHECK_PROGS(FIND, gfind find, )])
dnl
dnl Find sed.
dnl
# Check for a fully-functional sed program, that truncates
# as few characters as possible and that supports "\(X\|Y\)"
# style regular expression alternation.  Prefer GNU sed if found.
AC_DEFUN([OCTAVE_PROG_SED],
[AC_MSG_CHECKING([for a usable sed])
if test -z "$SED"; then
  AC_CACHE_VAL(ac_cv_path_sed, [
  # Loop through the user's path and test for sed and gsed.
  # Then use that list of sed's as ones to test for truncation.
  _AS_PATH_WALK([$PATH],
    [for ac_prog in sed gsed; do
      for ac_exec_ext in '' $ac_executable_extensions; do
	if AS_EXECUTABLE_P(["$as_dir/$ac_prog$ac_exec_ext"]); then
	  _sed_list="$_sed_list $as_dir/$ac_prog$ac_exec_ext"
	fi
      done
    done
    ])
    AS_TMPDIR(sed)
    _max=0
    _count=0
    # Add /usr/xpg4/bin/sed as it is typically found on Solaris
    # along with /bin/sed that truncates output.
    for _sed in $_sed_list /usr/xpg4/bin/sed; do
      test ! -f ${_sed} && break
      cat /dev/null > "$tmp/sed.in"
      _count=0
      echo $ECHO_N "0123456789$ECHO_C" >"$tmp/sed.in"
      # Check for GNU sed and select it if it is found.
      if "${_sed}" --version 2>&1 < /dev/null | egrep '(GNU)' > /dev/null; then
	octave_cv_path_sed=${_sed}
	break;
      fi
      # Reject if RE alternation is not handled.
      if test "`echo 'this and that' | ${_sed} -n 's/\(this\|that\).*$/\1/p'`" != "this"; then
        continue;
      fi
      while true; do
	cat "$tmp/sed.in" "$tmp/sed.in" >"$tmp/sed.tmp"
	mv "$tmp/sed.tmp" "$tmp/sed.in"
	cp "$tmp/sed.in" "$tmp/sed.nl"
	echo >>"$tmp/sed.nl"
	${_sed} -e 's/a$//' < "$tmp/sed.nl" >"$tmp/sed.out" || break
	cmp -s "$tmp/sed.out" "$tmp/sed.nl" || break
	# 10000 chars as input seems more than enough
	test $_count -gt 10 && break
	_count=`expr $_count + 1`
	if test $_count -gt $_max; then
	  _max=$_count
	  octave_cv_path_sed=$_sed
	fi
      done
    done
    rm -rf "$tmp"
  ])
  SED=$octave_cv_path_sed
  if test -z "$SED"; then
    AC_MSG_ERROR([no usable version of sed found])
  fi
fi
AC_SUBST(SED)
AC_MSG_RESULT([$SED])
])
dnl
dnl Find Perl.
dnl
dnl OCTAVE_PROG_PERL
AC_DEFUN([OCTAVE_PROG_PERL],
[AC_CHECK_PROG(PERL, perl, perl, [])
  AC_SUBST(PERL)
])
dnl
dnl Find Python.
dnl
dnl OCTAVE_PROG_PYTHON
AC_DEFUN([OCTAVE_PROG_PYTHON],
[AC_CHECK_PROG(PYTHON, python, python, [])
  AC_SUBST(PYTHON)
])
dnl
dnl Find desktop-file-install.
dnl
dnl OCTAVE_PROG_DESKTOP_FILE_INSTALL
AC_DEFUN([OCTAVE_PROG_DESKTOP_FILE_INSTALL],
[AC_CHECK_PROG(DESKTOP_FILE_INSTALL, desktop-file-install, desktop-file-install, [])
  AC_SUBST(DESKTOP_FILE_INSTALL)
])
dnl
dnl Check for IEEE 754 data format.
dnl
AC_DEFUN([OCTAVE_IEEE754_DATA_FORMAT],
[AC_MSG_CHECKING([for IEEE 754 data format])
AC_CACHE_VAL(octave_cv_ieee754_data_format,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
int
main (void) 
{
  typedef union { unsigned char c[8]; double d; } ieeebytes;
   
  ieeebytes l = {0x1c, 0xbc, 0x6e, 0xf2, 0x54, 0x8b, 0x11, 0x43};
  ieeebytes b = {0x43, 0x11, 0x8b, 0x54, 0xf2, 0x6e, 0xbc, 0x1c};

  return l.d != 1234567891234567.0 && b.d != 1234567891234567.0;
}]])],
  octave_cv_ieee754_data_format=yes,
  octave_cv_ieee754_data_format=no,
  octave_cv_ieee754_data_format=no)])
if test "$cross_compiling" = yes; then
  AC_MSG_RESULT([$octave_cv_ieee754_data_format assumed for cross compilation])
else
  AC_MSG_RESULT([$octave_cv_ieee754_data_format])
fi
if test "$octave_cv_ieee754_data_format" = yes; then
  AC_DEFINE(HAVE_IEEE754_DATA_FORMAT, 1, [Define if your system uses IEEE 754 data format.])
else
  ## If the format is unknown, then you will probably not have a
  ## useful system, so we will abort here.  Anyone wishing to
  ## experiment with building Octave on a system without IEEE
  ## floating point should be capable of removing this check and
  ## the one in the octave_ieee_init function in liboctave/lo-ieee.cc.
  AC_MSG_ERROR([IEEE 754 data format required for building Octave])
fi
])
dnl
dnl Check for UMFPACK seperately split complex matrix and RHS. Note
dnl that as umfpack.h can be in three different places, rather than
dnl include it, just declare the functions needed.
dnl
dnl Assumes that the check for umfpack has already been performed.
dnl
AC_DEFUN([OCTAVE_UMFPACK_SEPERATE_SPLIT],
[AC_MSG_CHECKING([for UMFPACK seperate complex matrix and rhs split])
AC_CACHE_VAL(octave_cv_umfpack_seperate_split,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#if defined (HAVE_UFSPARSE_UMFPACK_h)
#include <ufsparse/umfpack.h>
#elif defined (HAVE_UMFPACK_UMFPACK_H)
#include <umfpack/umfpack.h>
#elif defined (HAVE_UMFPACK_H)
#include <umfpack.h>
#endif
int n = 5;
int Ap[] = {0, 2, 5, 9, 10, 12};
int Ai[]  = {0, 1, 0, 2, 4, 1, 2, 3, 4, 2, 1, 4};
double Ax[] = {2., 0., 3., 0., 3., 0., -1., 0., 4., 0., 4., 0., 
	      -3., 0., 1., 0., 2., 0., 2., 0., 6., 0., 1., 0.};
double br[] = {8., 45., -3., 3., 19.};
double bi[] = {0., 0., 0., 0., 0.};
int main (void)
{
  double *null = (double *) NULL ;
  double *x = (double *)malloc (2 * n * sizeof(double));
  int i ;
  void *Symbolic, *Numeric ;
  (void) umfpack_zi_symbolic (n, n, Ap, Ai, Ax, null, &Symbolic, null, null) ;
  (void) umfpack_zi_numeric (Ap, Ai, Ax, null, Symbolic, &Numeric, null, null) ;
  umfpack_zi_free_symbolic (&Symbolic) ;
  (void) umfpack_zi_solve (0, Ap, Ai, Ax, null, x, null, br, bi, 
		Numeric, null, null) ;
  umfpack_zi_free_numeric (&Numeric) ;
  for (i = 0; i < n; i++, x+=2) 
    if (fabs(*x - i - 1.) > 1.e-13)
      return (1);
  return (0) ;
}
]])],
  octave_cv_umfpack_seperate_split=yes,
  octave_cv_umfpack_seperate_split=no,
  octave_cv_umfpack_seperate_split=no)])
if test "$cross_compiling" = yes; then
  AC_MSG_RESULT([$octave_cv_umfpack_seperate_split assumed for cross compilation])
else
  AC_MSG_RESULT([$octave_cv_umfpack_seperate_split])
fi
if test "$octave_cv_umfpack_seperate_split" = yes; then
  AC_DEFINE(UMFPACK_SEPARATE_SPLIT, 1, [Define if the UMFPACK Complex solver allow matrix and RHS to be split independently])
fi
])
dnl
dnl Check whether using HDF5 DLL under Windows. This is done by
dnl testing for a data symbol in the HDF5 library, which would
dnl requires the definition of _HDF5USEDL_ under MSVC compiler.
dnl
AC_DEFUN([OCTAVE_HDF5_DLL], [
  AC_CACHE_CHECK([if _HDF5USEDLL_ needs to be defined],octave_cv_hdf5_dll, [
    AC_TRY_LINK([#include <hdf5.h>], [hid_t x = H5T_NATIVE_DOUBLE; return x],
      octave_cv_hdf5_dll=no, [
      save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -DWIN32 -D_HDF5USEDLL_"
      save_LIBS="$LIBS"
      LIBS="$HDF5_LIBS $LIBS"
      AC_TRY_LINK([#include <hdf5.h>], [hid_t x = H5T_NATIVE_DOUBLE; return x],
        octave_cv_hdf5_dll=yes,
	octave_cv_hdf5_dll=no)
      CFLAGS="$save_CFLAGS"
      LIBS="$save_LIBS"])])
  if test "$octave_cv_hdf5_dll" = yes; then
    AC_DEFINE(_HDF5USEDLL_, 1, [Define if using HDF5 dll (Win32)])
  fi])
dnl
dnl Check whether HDF5 library has version 1.6 API functions.
dnl
AC_DEFUN([OCTAVE_HDF5_HAS_ENFORCED_16_API], [
  AC_CACHE_CHECK([whether HDF5 library has enforced version 1.6 API],
    octave_cv_hdf5_has_enforced_16_api, [
    AC_TRY_LINK([
#include <hdf5.h>
], [
  H5Eset_auto (0, 0);], [
      octave_cv_hdf5_has_enforced_16_api=yes], [
      octave_cv_hdf5_has_enforced_16_api=no])])
  if test "$octave_cv_hdf5_has_enforced_16_api" != "yes"; then
    AC_DEFINE(HAVE_HDF5_18, 1, [Define if >=HDF5-1.8 is available.])
  fi
])
dnl
dnl Check for the QHull version.
dnl
AC_DEFUN([OCTAVE_CHECK_QHULL_VERSION],
  [AC_CACHE_CHECK([for qh_version in $QHULL_LIBS],
    octave_cv_lib_qhull_version,  [
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <qhull/qhull_a.h>
]], [[
const char *tmp = qh_version;
]])], [octave_cv_lib_qhull_version=yes], [octave_cv_lib_qhull_version=no])])
  if test "$octave_cv_lib_qhull_version" = no; then
    AC_DEFINE(NEED_QHULL_VERSION, 1,
      [Define if the QHull library needs a qh_version variable defined.])
  fi
])
dnl
dnl Check whether QHull works (does not crash)
dnl
AC_DEFUN([OCTAVE_CHECK_QHULL_OK],
  [AC_CACHE_CHECK([whether the qhull library works],
    octave_cv_lib_qhull_ok, [
      AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <qhull/qhull.h>
#ifdef NEED_QHULL_VERSION
char *qh_version = "version";
#endif
]], [[
int dim = 2;
int n = 4;
coordT points[8] = { -0.5, -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, 0.5 };
boolT ismalloc = 0;
return qh_new_qhull (dim, n, points, ismalloc, "qhull ", 0, stderr); 
]])], [octave_cv_lib_qhull_ok=yes], [octave_cv_lib_qhull_ok=no])])
  if test "$octave_cv_lib_qhull_ok" = "yes"; then
    $1
  else
    $2
  fi
])
dnl
dnl Check for OpenGL.  If found, define OPENGL_LIBS
dnl
dnl FIXME -- the following tests should probably check for the
dnl libraries separately.
dnl
dnl FIXME -- should we allow a way to specify a directory for OpenGL
dnl libraries and header files?
dnl
AC_DEFUN([OCTAVE_OPENGL], [
OPENGL_LIBS=

### On MacOSX systems the OpenGL framework can be used
OCTAVE_HAVE_FRAMEWORK(OpenGL, [
#include <OpenGL/gl.h>
#include <OpenGL/glu.h> ], [GLint par; glGetIntegerv (GL_VIEWPORT, &par);],
  [have_framework_opengl="yes"], [have_framework_opengl="no"])

if test $have_framework_opengl = "yes"; then
  AC_DEFINE(HAVE_FRAMEWORK_OPENGL, 1, [Define if framework OPENGL is available.])
  OPENGL_LIBS="-Wl,-framework -Wl,OpenGL"
  AC_MSG_NOTICE([adding -Wl,-framework -Wl,OpenGL to OPENGL_LIBS])
  OCTAVE_GLUTESSCALLBACK_THREEDOTS
else
  case $canonical_host_type in
    *-*-mingw32* | *-*-msdosmsvc)
      AC_CHECK_HEADERS(windows.h)
    ;;
  esac
  have_opengl_incs=no
  AC_CHECK_HEADERS([GL/gl.h OpenGL/gl.h], [
    AC_CHECK_HEADERS([GL/glu.h OpenGL/glu.h], [
      have_opengl_incs=yes; break], [], [
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif
    ])
    break
    ], [], [
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif
    ])

  if test "$have_opengl_incs" = "yes"; then
    case $canonical_host_type in
      *-*-mingw32* | *-*-msdosmsvc)
        save_LIBS="$LIBS"
        LIBS="$LIBS -lopengl32"
        AC_MSG_CHECKING([for glEnable in -lopengl32])
        AC_TRY_LINK([
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if defined (HAVE_GL_GL_H)
#include <GL/gl.h>
#elif defined (HAVE_OPENGL_GL_H)
#include <OpenGL/gl.h>
#endif
], [glEnable(GL_SMOOTH);], OPENGL_LIBS="-lopengl32 -lglu32")
        LIBS="$save_LIBS"
        if test "x$OPENGL_LIBS" != "x"; then
          AC_MSG_RESULT(yes)
        else
          AC_MSG_RESULT(no)
        fi
        ;;
      *)
        AC_CHECK_LIB(GL, glEnable, OPENGL_LIBS="-lGL -lGLU")
        ;;
    esac
  fi
fi
AC_SUBST(OPENGL_LIBS)
])
dnl
dnl See if function gluTessCallback is called with "(...)"
dnl
dnl OCTAVE_GLUTESSCALLBACK_THREEDOTS
AC_DEFUN([OCTAVE_GLUTESSCALLBACK_THREEDOTS],
[AC_CACHE_CHECK([whether gluTessCallback is called with "(...)"],
octave_cv_glutesscallback_threedots,
[AC_LANG_PUSH(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#ifdef HAVE_GL_GLU_H
#include <GL/glu.h>
#elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
#include <OpenGL/glu.h>
#endif]],
[[GLvoid (*func)(...); gluTessCallback(0, 0, func);]])],
octave_cv_glutesscallback_threedots="yes", octave_cv_glutesscallback_threedots="no")])
AC_LANG_POP(C++)
if test $octave_cv_glutesscallback_threedots = "yes"; then
  AC_DEFINE(HAVE_GLUTESSCALLBACK_THREEDOTS, 1, 
    [Define if gluTessCallback is called with (...)])
fi
])
dnl
dnl Check for support of OpenMP with a given compiler flag. If
dnl found define HAVE_OPENMP and add the compile flag to CFLAGS
dnl and CXXFLAGS.
dnl
AC_DEFUN([OCTAVE_CHECK_OPENMP],
[AC_MSG_CHECKING([for support of OpenMP])
XCFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $1"
AC_CACHE_VAL(octave_cv_check_openmp,[
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <omp.h>
#include <stdio.h>
]], [[
int main(int argc, char* argv[])
{
  _Pragma("omp parallel")  
  printf("Hello, world.\n");
  return 0;
}
]])],octave_cv_openmp=yes, octave_cv_openmmp=no, octave_cv_openmp=no)])
AC_MSG_RESULT($octave_cv_openmp)
if test "$octave_cv_openmp" = yes; then
  AC_DEFINE(HAVE_OPENMP,1,[Define if compiler supports OpenMP])
  CXXFLAGS="$CXXFLAGS $1"
else
  CFLAGS="$XCFLAGS"
fi
])
dnl
dnl Configure paths for FreeType2
dnl Marcelo Magallon 2001-10-26, based on gtk.m4 by Owen Taylor
dnl
dnl Copyright 2001, 2003 by
dnl David Turner, Robert Wilhelm, and Werner Lemberg.
dnl
dnl This file is part of the FreeType project, and may only be used, modified,
dnl and distributed under the terms of the FreeType project license,
dnl LICENSE.TXT.  By continuing to use, modify, or distribute this file you
dnl indicate that you have read the license and understand and accept it
dnl fully.
dnl
dnl As a special exception to the FreeType project license, this file may be
dnl distributed as part of a program that contains a configuration script
dnl generated by Autoconf, under the same distribution terms as the rest of
dnl that program.
dnl
dnl serial 2
dnl
dnl AC_CHECK_FT2([MINIMUM-VERSION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for FreeType 2, and define FT2_CFLAGS and FT2_LIBS.
dnl MINIMUM-VERSION is what libtool reports; the default is `7.0.1' (this is
dnl FreeType 2.0.4).
dnl
AC_DEFUN([AC_CHECK_FT2],
  [dnl Get the cflags and libraries from the freetype-config script
   dnl
   AC_ARG_WITH([ft-prefix],
     dnl don't quote AS_HELP_STRING!
     AS_HELP_STRING([--with-ft-prefix=PREFIX],
                    [Prefix where FreeType is installed (optional)]),
     [ft_config_prefix="$withval"],
     [ft_config_prefix=""])
  
   AC_ARG_WITH([ft-exec-prefix],
     dnl don't quote AS_HELP_STRING!
     AS_HELP_STRING([--with-ft-exec-prefix=PREFIX],
                    [Exec prefix where FreeType is installed (optional)]),
     [ft_config_exec_prefix="$withval"],
     [ft_config_exec_prefix=""])

   AC_ARG_ENABLE([freetypetest],
     dnl don't quote AS_HELP_STRING!
     AS_HELP_STRING([--disable-freetypetest],
                    [Do not try to compile and run a test FreeType program]),
     [],
     [enable_fttest=yes])

   if test x$ft_config_exec_prefix != x ; then
     ft_config_args="$ft_config_args --exec-prefix=$ft_config_exec_prefix"
     if test x${FT2_CONFIG+set} != xset ; then
       FT2_CONFIG=$ft_config_exec_prefix/bin/freetype-config
     fi
   fi

   if test x$ft_config_prefix != x ; then
     ft_config_args="$ft_config_args --prefix=$ft_config_prefix"
     if test x${FT2_CONFIG+set} != xset ; then
       FT2_CONFIG=$ft_config_prefix/bin/freetype-config
     fi
   fi

   AC_PATH_PROG([FT2_CONFIG], [freetype-config], [no])

   min_ft_version=m4_if([$1], [], [7.0.1], [$1])
   AC_MSG_CHECKING([for FreeType -- version >= $min_ft_version])
   no_ft=""
   if test "$FT2_CONFIG" = "no" ; then
     no_ft=yes
   else
     FT2_CFLAGS=`$FT2_CONFIG $ft_config_args --cflags`
     FT2_LIBS=`$FT2_CONFIG $ft_config_args --libs`
     ft_config_major_version=`$FT2_CONFIG $ft_config_args --version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
     ft_config_minor_version=`$FT2_CONFIG $ft_config_args --version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
     ft_config_micro_version=`$FT2_CONFIG $ft_config_args --version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
     ft_min_major_version=`echo $min_ft_version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
     ft_min_minor_version=`echo $min_ft_version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
     ft_min_micro_version=`echo $min_ft_version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
     if test x$enable_fttest = xyes ; then
       ft_config_is_lt=""
       if test $ft_config_major_version -lt $ft_min_major_version ; then
         ft_config_is_lt=yes
       else
         if test $ft_config_major_version -eq $ft_min_major_version ; then
           if test $ft_config_minor_version -lt $ft_min_minor_version ; then
             ft_config_is_lt=yes
           else
            if test $ft_config_minor_version -eq $ft_min_minor_version ; then
               if test $ft_config_micro_version -lt $ft_min_micro_version ; then
                 ft_config_is_lt=yes
               fi
             fi
           fi
         fi
       fi
       if test x$ft_config_is_lt = xyes ; then
         no_ft=yes
       else
         ac_save_CFLAGS="$CFLAGS"
         ac_save_LIBS="$LIBS"
         CFLAGS="$CFLAGS $FT2_CFLAGS"
         LIBS="$FT2_LIBS $LIBS"

         dnl
         dnl Sanity checks for the results of freetype-config to some extent.
         dnl
         AC_RUN_IFELSE([
             AC_LANG_SOURCE([[

#include <ft2build.h>
#include FT_FREETYPE_H
#include <stdio.h>
#include <stdlib.h>

int
main()
{
  FT_Library library;
  FT_Error  error;

  error = FT_Init_FreeType(&library);

  if (error)
    return 1;
  else
  {
    FT_Done_FreeType(library);
    return 0;
  }
}

             ]])
           ],
           [],
           [no_ft=yes],
           [echo $ECHO_N "cross compiling; assuming OK... $ECHO_C"])

         CFLAGS="$ac_save_CFLAGS"
         LIBS="$ac_save_LIBS"
       fi             dnl test $ft_config_version -lt $ft_min_version
     fi               dnl test x$enable_fttest = xyes
   fi                 dnl test "$FT2_CONFIG" = "no"

   if test x$no_ft = x ; then
     AC_MSG_RESULT([yes])
     m4_if([$2], [], [:], [$2])
   else
     AC_MSG_RESULT([no])
     if test "$FT2_CONFIG" = "no" ; then
       AC_MSG_WARN([

  The freetype-config script installed by FreeType 2 could not be found.
  If FreeType 2 was installed in PREFIX, make sure PREFIX/bin is in
  your path, or set the FT2_CONFIG environment variable to the
  full path to freetype-config.
       ])
     else
       if test x$ft_config_is_lt = xyes ; then
         AC_MSG_WARN([

  Your installed version of the FreeType 2 library is too old.
  If you have different versions of FreeType 2, make sure that
  correct values for --with-ft-prefix or --with-ft-exec-prefix
  are used, or set the FT2_CONFIG environment variable to the
  full path to freetype-config.
         ])
       else
         AC_MSG_WARN([

  The FreeType test program failed to run.  If your system uses
  shared libraries and they are installed outside the normal
  system library path, make sure the variable LD_LIBRARY_PATH
  (or whatever is appropiate for your system) is correctly set.
         ])
       fi
     fi

     FT2_CFLAGS=""
     FT2_LIBS=""
     m4_if([$3], [], [:], [$3])
   fi

   AC_SUBST([FT2_CFLAGS])
   AC_SUBST([FT2_LIBS])])
dnl end of freetype2.m4

dnl Check whether a math mapper function is available in <cmath>.
dnl Will define HAVE_CMATH_FUNC if there is a double variant and
dnl HAVE_CMATH_FUNCF if there is a float variant.
dnl Currently capable of checking for functions with single 
dnl argument and returning bool/int/real.
AC_DEFUN([OCTAVE_CMATH_FUNC],[
AC_MSG_CHECKING([for std::$1 in <cmath>])
AC_LANG_PUSH(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <cmath>
void take_func (bool (*func) (double x));
void take_func (int (*func) (double x));
void take_func (double (*func) (double x));
]],
[[
take_func(std::$1);
]])],
[AC_MSG_RESULT([yes])
 AC_DEFINE(HAVE_CMATH_[]AS_TR_CPP($1),1,[Define if <cmath> provides $1])],
[AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for std::$1 (float variant) in <cmath>])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <cmath>
void take_func (bool (*func) (float x));
void take_func (int (*func) (float x));
void take_func (float (*func) (float x));
]],
[[
take_func(std::$1);
]])],
[AC_MSG_RESULT([yes])
 AC_DEFINE(HAVE_CMATH_[]AS_TR_CPP($1)F,1,[Define if <cmath> provides float variant of $1])],
[AC_MSG_RESULT([no])])
AC_LANG_POP(C++)
])

dnl Check whether fast signed integer arithmetics using bit tricks
dnl can be used in oct-inttypes.h. Defines HAVE_FAST_INT_OPS if
dnl the following conditions hold:
dnl 1. Signed numbers are represented by twos complement
dnl    (see <http://en.wikipedia.org/wiki/Two%27s_complement>)
dnl 2. static_cast to unsigned int counterpart works like interpreting
dnl    the signed bit pattern as unsigned (and is thus zero-cost).
dnl 3. Signed addition and subtraction yield the same bit results as unsigned.
dnl    (We use casts to prevent optimization interference, so there is no
dnl     need for things like -ftrapv).
dnl 4. Bit operations on signed integers work like on unsigned integers,
dnl    except for the shifts. Shifts are arithmetic.
dnl
AC_DEFUN([OCTAVE_FAST_INT_OPS],[
AC_MSG_CHECKING([whether fast integer arithmetics is usable])
AC_LANG_PUSH(C++)
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <limits>
template<class UT, class ST>
static bool 
do_test (UT, ST)
{
  volatile ST s = std::numeric_limits<ST>::min () / 3;
  volatile UT u = static_cast<UT> (s);
  if (*(reinterpret_cast<volatile ST *> (&u)) != s) return true;
  
  u = 0; u = ~u;
  if (*(reinterpret_cast<volatile ST *> (&u)) != -1) return true;
  
  ST sx, sy;
  sx = std::numeric_limits<ST>::max () / 2 + 1;
  sy = std::numeric_limits<ST>::max () / 2 + 2;
  if (static_cast<ST> (static_cast<UT> (sx) + static_cast<UT> (sy))
      != std::numeric_limits<ST>::min () + 1) return true;
  if (static_cast<ST> (static_cast<UT> (sx) - static_cast<UT> (sy))
      != -1) return true;
  
  if ((sx & sy) != (static_cast<UT> (sx) & static_cast<UT> (sy)))
    return true;
  if ((sx | sy) != (static_cast<UT> (sx) | static_cast<UT> (sy)))
    return true;
  if ((sx ^ sy) != (static_cast<UT> (sx) ^ static_cast<UT> (sy)))
    return true;
  if ((-1 >> 1) != -1) return true;
  return false;
}

#define DO_TEST(T) \
if (do_test (static_cast<unsigned T> (0), static_cast<signed T> (0))) \
  return sizeof (T);
]],[[
  DO_TEST(char)
  DO_TEST(short)
  DO_TEST(int)
  DO_TEST(long)
#if (defined(HAVE_LONG_LONG_INT) && defined(HAVE_UNSIGNED_LONG_LONG_INT))
  DO_TEST(long long)
#endif
]])],
[AC_MSG_RESULT([yes])
 AC_DEFINE(HAVE_FAST_INT_OPS,1,[Define if signed integers use two's complement])],
[AC_MSG_RESULT([no])])
AC_LANG_POP(C++)])
dnl
dnl Check to see if the compiler and the linker can handle the flags
dnl "-framework $1" for the given prologue $2 and the given body $3 of
dnl a source file.  Arguments 2 and 3 optionally can also be empty.
dnl Add options (lower case letters $1) "--with-framework-$1" and
dnl "--without-framework-$1". If this test is successful then perform
dnl $4, otherwise do $5.
dnl
dnl OCTAVE_HAVE_FRAMEWORK
AC_DEFUN([OCTAVE_HAVE_FRAMEWORK], [
  AC_MSG_CHECKING([whether ${LD-ld} accepts -framework $1])
  AC_CACHE_VAL(octave_cv_framework_$1, [
    XLDFLAGS="$LDFLAGS"
    LDFLAGS="$LDFLAGS -framework $1"
    AC_LANG_PUSH(C++)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
      eval "octave_cv_framework_$1=yes",
      eval "octave_cv_framework_$1=no")
    AC_LANG_POP(C++)
    LDFLAGS="$XLDFLAGS"
  ])
  if test "$octave_cv_framework_$1" = "yes"; then
    AC_MSG_RESULT(yes)
    AC_ARG_WITH(framework-m4_tolower($1),
      [AS_HELP_STRING([--without-framework-m4_tolower($1)], 
        [don't use framework $1])],
         with_have_framework=$withval, with_have_framework="yes")
    if test "$with_have_framework" = "yes"; then
      [$4]
    else
      AC_MSG_NOTICE([framework rejected by --without-framework-m4_tolower($1)])
      [$5]
    fi
  else
    AC_MSG_RESULT(no)
    [$5]
  fi
])

##############################################################################
##############################################################################

# pkg.m4 - Macros to locate and utilise pkg-config.            -*- Autoconf -*-
# 
# Copyright © 2004 Scott James Remnant <scott@netsplit.com>.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# As a special exception to the GNU General Public License, if you
# distribute this file as part of a program that contains a
# configuration script generated by Autoconf, you may include it under
# the same distribution terms that you use for the rest of that program.

# PKG_PROG_PKG_CONFIG([MIN-VERSION])
# ----------------------------------
AC_DEFUN([PKG_PROG_PKG_CONFIG],
[m4_pattern_forbid([^_?PKG_[A-Z_]+$])
m4_pattern_allow([^PKG_CONFIG(_PATH)?$])
AC_ARG_VAR([PKG_CONFIG], [path to pkg-config utility])dnl
if test "x$ac_cv_env_PKG_CONFIG_set" != "xset"; then
	AC_PATH_TOOL([PKG_CONFIG], [pkg-config])
fi
if test -n "$PKG_CONFIG"; then
	_pkg_min_version=m4_default([$1], [0.9.0])
	AC_MSG_CHECKING([pkg-config is at least version $_pkg_min_version])
	if $PKG_CONFIG --atleast-pkgconfig-version $_pkg_min_version; then
		AC_MSG_RESULT([yes])
	else
		AC_MSG_RESULT([no])
		PKG_CONFIG=""
	fi
		
fi[]dnl
])# PKG_PROG_PKG_CONFIG

# PKG_CHECK_EXISTS(MODULES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
#
# Check to see whether a particular set of modules exists.  Similar
# to PKG_CHECK_MODULES(), but does not set variables or print errors.
#
#
# Similar to PKG_CHECK_MODULES, make sure that the first instance of
# this or PKG_CHECK_MODULES is called, or make sure to call
# PKG_CHECK_EXISTS manually
# --------------------------------------------------------------
AC_DEFUN([PKG_CHECK_EXISTS],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
if test -n "$PKG_CONFIG" && \
    AC_RUN_LOG([$PKG_CONFIG --exists --print-errors "$1"]); then
  m4_ifval([$2], [$2], [:])
m4_ifvaln([$3], [else
  $3])dnl
fi])


# _PKG_CONFIG([VARIABLE], [COMMAND], [MODULES])
# ---------------------------------------------
m4_define([_PKG_CONFIG],
[if test -n "$PKG_CONFIG"; then
    if test -n "$$1"; then
        pkg_cv_[]$1="$$1"
    else
        PKG_CHECK_EXISTS([$3],
                         [pkg_cv_[]$1=`$PKG_CONFIG --[]$2 "$3" 2>/dev/null`],
			 [pkg_failed=yes])
    fi
else
	pkg_failed=untried
fi[]dnl
])# _PKG_CONFIG

# _PKG_SHORT_ERRORS_SUPPORTED
# -----------------------------
AC_DEFUN([_PKG_SHORT_ERRORS_SUPPORTED],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])
if $PKG_CONFIG --atleast-pkgconfig-version 0.20; then
        _pkg_short_errors_supported=yes
else
        _pkg_short_errors_supported=no
fi[]dnl
])# _PKG_SHORT_ERRORS_SUPPORTED


# PKG_CHECK_MODULES(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
# [ACTION-IF-NOT-FOUND])
#
#
# Note that if there is a possibility the first call to
# PKG_CHECK_MODULES might not happen, you should be sure to include an
# explicit call to PKG_PROG_PKG_CONFIG in your configure.ac
#
#
# --------------------------------------------------------------
AC_DEFUN([PKG_CHECK_MODULES],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1][_CFLAGS], [C compiler flags for $1, overriding pkg-config])dnl
AC_ARG_VAR([$1][_LIBS], [linker flags for $1, overriding pkg-config])dnl

pkg_failed=no
AC_MSG_CHECKING([for $1])

_PKG_CONFIG([$1][_CFLAGS], [cflags], [$2])
_PKG_CONFIG([$1][_LIBS], [libs], [$2])

m4_define([_PKG_TEXT], [Alternatively, you may set the environment variables $1[]_CFLAGS
and $1[]_LIBS to avoid the need to call pkg-config.
See the pkg-config man page for more details.])

if test $pkg_failed = yes; then
        _PKG_SHORT_ERRORS_SUPPORTED
        if test $_pkg_short_errors_supported = yes; then
	        $1[]_PKG_ERRORS=`$PKG_CONFIG --short-errors --errors-to-stdout --print-errors "$2"`
        else 
	        $1[]_PKG_ERRORS=`$PKG_CONFIG --errors-to-stdout --print-errors "$2"`
        fi
	# Put the nasty error message in config.log where it belongs
	echo "$$1[]_PKG_ERRORS" >&AS_MESSAGE_LOG_FD

	ifelse([$4], , [AC_MSG_ERROR(dnl
[Package requirements ($2) were not met:

$$1_PKG_ERRORS

Consider adjusting the PKG_CONFIG_PATH environment variable if you
installed software in a non-standard prefix.

_PKG_TEXT
])],
		[AC_MSG_RESULT([no])
                $4])
elif test $pkg_failed = untried; then
	ifelse([$4], , [AC_MSG_FAILURE(dnl
[The pkg-config script could not be found or is too old.  Make sure it
is in your PATH or set the PKG_CONFIG environment variable to the full
path to pkg-config.

_PKG_TEXT

To get pkg-config, see <http://pkg-config.freedesktop.org/>.])],
		[$4])
else
	$1[]_CFLAGS=$pkg_cv_[]$1[]_CFLAGS
	$1[]_LIBS=$pkg_cv_[]$1[]_LIBS
        AC_MSG_RESULT([yes])
	ifelse([$3], , :, [$3])
fi[]dnl
])# PKG_CHECK_MODULES

dnl
dnl External macros.
dnl

m4_include([m4/ax_pthread.m4])
m4_include([m4/ax_blas.m4])
m4_include([m4/ax_blas_f77_func.m4])
m4_include([m4/ax_lapack.m4])
