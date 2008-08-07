dnl aclocal.m4 -- extra macros for configuring Octave
dnl
dnl Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
dnl               2003, 2004, 2005, 2006, 2007 John W. Eaton
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
dnl ----------------------------------------------------------------------
dnl
dnl Figure out the hardware-vendor-os info.
dnl
dnl OCTAVE_HOST_TYPE
AC_DEFUN(OCTAVE_HOST_TYPE,
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
AC_DEFUN(OCTAVE_SET_DEFAULT,
[ifelse($#, 2, [: ${$1=$2}
])dnl
AC_MSG_RESULT([defining $1 to be $$1])
AC_SUBST($1)])
dnl
dnl Check for ar.
dnl
AC_DEFUN(OCTAVE_PROG_AR,
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
AC_DEFUN(OCTAVE_PLACEMENT_DELETE,
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
AC_DEFUN(OCTAVE_DYNAMIC_AUTO_ARRAYS,
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
dnl Check for broken strptime
dnl
AC_DEFUN(OCTAVE_STRPTIME_BROKEN,
[AC_CACHE_CHECK([whether strptime is broken],
octave_cv_strptime_broken,
[AC_LANG_PUSH(C)
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#define _XOPEN_SOURCE
#if defined (HAVE_SYS_TYPES_H)
#include <sys/types.h>
#if defined (HAVE_UNISTD_H)
#include <unistd.h>
#endif
#endif
#include <stdio.h>
#include <time.h>
]], [[
struct tm t;
char *q = strptime ("09/13", "%m/%d/%y", &t);
return q ? 1 : 0;
]])], [octave_cv_strptime_broken=no], [octave_cv_strptime_broken=yes])])
if test $octave_cv_strptime_broken = yes; then
AC_DEFINE(OCTAVE_HAVE_BROKEN_STRPTIME, 1, [Define if strptime is broken on your system])
fi
AC_LANG_POP(C)
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
AC_DEFUN(OCTAVE_SMART_PUTENV,
[AC_MSG_CHECKING(whether putenv uses malloc)
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
AC_MSG_RESULT($octave_cv_func_putenv_malloc)
if test $octave_cv_func_putenv_malloc = yes; then
  AC_DEFINE(SMART_PUTENV,1,[To quiet autoheader.])
fi])
dnl
dnl These two checks for signal functions were originally part of the
dnl aclocal.m4 file distributed with bash 2.0.
dnl
dnl Check type of signal routines (posix, 4.2bsd, 4.1bsd or v7)
AC_DEFUN(OCTAVE_SIGNAL_CHECK,
[AC_REQUIRE([AC_TYPE_SIGNAL])
AC_MSG_CHECKING(for type of signal functions)
AC_CACHE_VAL(octave_cv_signal_vintage,
[
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]],
    [[sigset_t ss;
      struct sigaction sa;
      sigemptyset (&ss);
      sigsuspend (&ss);
      sigaction (SIGINT, &sa, (struct sigaction *) 0);
      sigprocmask (SIG_BLOCK, &ss, (sigset_t *) 0);]])],
    [octave_cv_signal_vintage=posix],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]],
       [[int mask = sigmask (SIGINT);
	 sigsetmask (mask);
         sigblock (mask);
         sigpause (mask);]])],
       [octave_cv_signal_vintage=4.2bsd],
       [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>
          RETSIGTYPE foo() { }]],
          [[int mask = sigmask (SIGINT);
	    sigset (SIGINT, foo);
            sigrelse (SIGINT);
	    sighold (SIGINT);
            sigpause (SIGINT);]])],
          [octave_cv_signal_vintage=svr3],
          [octave_cv_signal_vintage=v7])])])])
AC_MSG_RESULT($octave_cv_signal_vintage)
if test "$octave_cv_signal_vintage" = posix; then
AC_DEFINE(HAVE_POSIX_SIGNALS, 1, [Define if you have POSIX style signals.])
elif test "$octave_cv_signal_vintage" = "4.2bsd"; then
AC_DEFINE(HAVE_BSD_SIGNALS, 1, [Define if you have BSD style signals.])
elif test "$octave_cv_signal_vintage" = svr3; then
AC_DEFINE(HAVE_USG_SIGHOLD, 1, [Define if you have System V Release 3 signals.])
fi
])
dnl
AC_DEFUN(OCTAVE_REINSTALL_SIGHANDLERS,
[AC_REQUIRE([AC_TYPE_SIGNAL])
AC_REQUIRE([OCTAVE_SIGNAL_CHECK])
AC_MSG_CHECKING([if signal handlers must be reinstalled when invoked])
AC_CACHE_VAL(octave_cv_must_reinstall_sighandlers,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
typedef RETSIGTYPE sigfunc();
int nsigint;
#ifdef HAVE_POSIX_SIGNALS
sigfunc *
set_signal_handler(sig, handler)
     int sig;
     sigfunc *handler;
{
  struct sigaction act, oact;
  act.sa_handler = handler;
  act.sa_flags = 0;
  sigemptyset (&act.sa_mask);
  sigemptyset (&oact.sa_mask);
  sigaction (sig, &act, &oact);
  return (oact.sa_handler);
}
#else
#define set_signal_handler(s, h) signal(s, h)
#endif
RETSIGTYPE
sigint(s)
    int s;
{
  nsigint++;
}
main()
{
  nsigint = 0;
  set_signal_handler(SIGINT, sigint);
  kill((int)getpid(), SIGINT);
  kill((int)getpid(), SIGINT);
  exit(nsigint != 2);
}
]])],
  octave_cv_must_reinstall_sighandlers=no,
  octave_cv_must_reinstall_sighandlers=yes,
if test "$octave_cv_signal_vintage" = svr3; then
  octave_cv_must_reinstall_sighandlers=yes
else
  octave_cv_must_reinstall_sighandlers=no
fi)])
if test "$cross_compiling" = yes; then
  AC_MSG_RESULT([$octave_cv_must_reinstall_sighandlers assumed for cross compilation])
else
  AC_MSG_RESULT($octave_cv_must_reinstall_sighandlers)
fi
if test "$octave_cv_must_reinstall_sighandlers" = yes; then
  AC_DEFINE(MUST_REINSTALL_SIGHANDLERS,1,[Define if signal handlers must be reinstalled after they are called.])
fi
])
dnl
dnl Check to see if C++ compiler needs the new friend template declaration 
dnl syntax. 
dnl
dnl OCTAVE_CXX_NEW_FRIEND_TEMPLATE_DECL
AC_DEFUN(OCTAVE_CXX_NEW_FRIEND_TEMPLATE_DECL, [
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
  AC_MSG_RESULT($octave_cv_cxx_new_friend_template_decl)
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
AC_DEFUN(OCTAVE_CC_FLAG, [
  ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING(whether ${CC-cc} accepts $1)
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
AC_DEFUN(OCTAVE_CXX_FLAG, [
  ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING(whether ${CXX-g++} accepts $1)
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
AC_DEFUN(OCTAVE_F77_FLAG, [
  ac_safe=`echo "$1" | sed 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING(whether ${F77-g77} accepts $1)
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
dnl Check for flex
dnl
AC_DEFUN(OCTAVE_PROG_FLEX, [
### For now, don't define LEXLIB to be -lfl -- we don't use anything in
### it, and it might not be installed.
###
### Also make sure that we generate an interactive scanner if we are
### using flex.
  AC_PROG_LEX
  case "$LEX" in
    flex*)
      LFLAGS="-t -I"
      AC_MSG_RESULT([defining LFLAGS to be $LFLAGS])
      LEXLIB=
    ;;
    *)
      LEX='$(top_srcdir)/missing flex'
      warn_flex="I didn't find flex, but it's only a problem if you need to reconstruct lex.cc"
      AC_MSG_WARN($warn_flex)
    ;;
  esac
  AC_SUBST(LFLAGS)
])
dnl
dnl Check for bison
dnl
AC_DEFUN(OCTAVE_PROG_BISON, [
  AC_PROG_YACC
  case "$YACC" in
    bison*)
    ;;
    *)
      YACC='$(top_srcdir)/missing bison'
      warn_bison="I didn't find bison, but it's only a problem if you need to reconstruct parse.cc"
      AC_MSG_WARN($warn_bison)
    ;;
  esac
])
dnl
dnl What pager should we use?
dnl
AC_DEFUN(OCTAVE_PROG_PAGER,
[if test "$cross_compiling" = yes; then
  DEFAULT_PAGER=less
  AC_MSG_RESULT(assuming $DEFAULT_PAGER exists on $canonical_host_type host)
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
    AC_MSG_WARN($warn_less)
  fi
fi
])
dnl
dnl Does gnuplot exist?
dnl
AC_DEFUN(OCTAVE_PROG_GNUPLOT, [
case "$canonical_host_type" in
  *-*-cygwin* | *-*-mingw32* | *-*-msdosmsvc)
    gp_names="pgnuplot pipe-gnuplot gnuplot"
    gp_default=pgnuplot
  ;;
  *)
    gp_names=gnuplot
    gp_default=gnuplot
  ;;
esac
if test "$cross_compiling" = yes; then
  GNUPLOT="$gp_default"
  AC_MSG_RESULT(assuming $GNUPLOT exists on $canonical_host_type host)
else
  AC_CHECK_PROGS(GNUPLOT, $gp_names)
  if test -z "$GNUPLOT"; then
    warn_gnuplot=yes

    GNUPLOT="$gp_default"

    ## If you change this text, be sure to also copy it to the set of
    ## warnings at the end of the script

    AC_MSG_WARN([I didn't find gnuplot.  It isn't necessary to have gnuplot])
    AC_MSG_WARN([installed, but you won't be able to use any of Octave's])
    AC_MSG_WARN([plotting commands without it.])
    AC_MSG_WARN([])
    AC_MSG_WARN([If gnuplot is installed but it isn't in your path, you can])
    AC_MSG_WARN([tell Octave where to find it by typing the command])
    AC_MSG_WARN([])
    AC_MSG_WARN([gnuplot_binary = "/full/path/to/gnuplot/binary"])
    AC_MSG_WARN([])
    AC_MSG_WARN([at the Octave prompt.])
    AC_MSG_WARN([])
    AC_MSG_WARN([Setting default value to $GNUPLOT])
  fi
fi
AC_SUBST(GNUPLOT)
])
dnl
dnl Is gperf installed?
dnl
dnl OCTAVE_PROG_GPERF
AC_DEFUN(OCTAVE_PROG_GPERF, [
  AC_CHECK_PROG(GPERF, gperf, gperf, [])
  if test -n "$GPERF"; then
    if echo "%{
enum octave_kw_id { a_kw };
%}
struct octave_kw { const char *name; int tok; octave_kw_id kw_id; };
%%
foo" | $GPERF -t -C -D -E -G -L C++ -H octave_kw_hash -N octave_kw_lookup > /dev/null 2>&1; then
      true
    else
      GPERF=""
      warn_gperf="I found gperf, but it does not support all of the following options: -t -C -D -E -G -L C++ -H -N; you need gperf 3.0.1 or a more recent version"
      AC_MSG_WARN($warn_gperf)
    fi
  else
    GPERF='$(top_srcdir)/missing gperf'
    warn_gperf="I didn't find gperf, but it's only a problem if you need to reconstruct oct-gperf.h"
    AC_MSG_WARN($warn_gperf)
  fi
  AC_SUBST(GPERF)
])
dnl
dnl Is ghostscript installed?
dnl
dnl OCTAVE_PROG_GHOSTSCRIPT
AC_DEFUN(OCTAVE_PROG_GHOSTSCRIPT, [
  case "$canonical_host_type" in
    *-*-cygwin* | *-*-mingw32* | *-*-msdosmsvc)
      gs_names="gs gswin32"
    ;;
    *)
      gs_names=gs
    ;;
  esac
  AC_CHECK_PROGS(GHOSTSCRIPT, $gs_names)
  if test -z "$GHOSTSCRIPT"; then
    GHOSTSCRIPT='$(top_srcdir)/missing gs'
    warn_ghostscript="I didn't find ghostscript, but it's only a problem if you need to reconstruct figures for the manual"
    AC_MSG_WARN($warn_ghostscript)
  fi
  AC_SUBST(GHOSTSCRIPT)
])
dnl
dnl Is makeinfo installed?
dnl
dnl OCTAVE_PROG_MAKEINFO
AC_DEFUN(OCTAVE_PROG_MAKEINFO, [
  AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo, [])
  if test -z "$MAKEINFO"; then
    MAKEINFO='$(top_srcdir)/missing makeinfo'
    warn_makeinfo="I didn't find makeinfo, but it's only a problem if you need to reconstruct the Info version of the manual"
    AC_MSG_WARN($warn_makeinfo)
  fi
  AC_SUBST(MAKEINFO)
])
dnl
dnl Is texi2dvi installed?
dnl
dnl OCTAVE_PROG_TEXI2DVI
AC_DEFUN(OCTAVE_PROG_TEXI2DVI, [
  AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi, [])
  if test -z "$TEXI2DVI"; then
    TEXI2DVI='$(top_srcdir)/missing texi2dvi'
    warn_texi2dvi="I didn't find texi2dvi, but it's only a problem if you need to reconstruct the DVI version of the manual"
    AC_MSG_WARN($warn_texi2dvi)
  fi
  AC_SUBST(TEXI2DVI)
])
dnl
dnl Is texi2pdf installed?
dnl
dnl OCTAVE_PROG_TEXI2PDF
AC_DEFUN(OCTAVE_PROG_TEXI2PDF, [
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
    AC_MSG_WARN($warn_texi2pdf)
  fi
  AC_SUBST(TEXI2PDF)
])
dnl
dnl See if the C++ library is ISO compliant.
dnl FIXME: This is obviously very simplistic, and trivially fooled.
dnl
dnl OCTAVE_CXX_ISO_COMPLIANT_LIBRARY
AC_DEFUN(OCTAVE_CXX_ISO_COMPLIANT_LIBRARY, [
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
  AC_MSG_RESULT($octave_cv_cxx_iso_compliant_library)
  if test $octave_cv_cxx_iso_compliant_library = yes; then
    AC_DEFINE(CXX_ISO_COMPLIANT_LIBRARY, 1, [Define if your C++ runtime library is ISO compliant.])
  fi
])
dnl
dnl Allow the user disable support for command line editing using GNU
dnl readline.
dnl
dnl OCTAVE_ENABLE_READLINE
AC_DEFUN(OCTAVE_ENABLE_READLINE, [
  USE_READLINE=true
  LIBREADLINE=
  AC_ARG_ENABLE(readline,
    [  --enable-readline       use readline library (default is yes)],
    [if test "$enableval" = no; then
       USE_READLINE=false
       warn_readline="command editing and history features require GNU Readline"
     fi])
  if $USE_READLINE; then
    AC_CHECK_LIB(readline, rl_set_keyboard_input_timeout, [
      LIBREADLINE="-lreadline"
      LIBS="$LIBREADLINE $LIBS"
      AC_DEFINE(USE_READLINE, 1, [Define to use the readline library.])
    ], [
      AC_MSG_WARN([I need GNU Readline 4.2 or later])
      AC_MSG_ERROR([this is fatal unless you specify --disable-readline])
    ])
  fi
  AC_SUBST(LIBREADLINE)
])
dnl
dnl Check to see if C++ reintrepret cast works for function pointers.
dnl
dnl OCTAVE_CXX_BROKEN_REINTERPRET_CAST
dnl
AC_DEFUN(OCTAVE_CXX_BROKEN_REINTERPRET_CAST, [
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
dnl Determine if mkdir accepts only one argument instead dnl of the usual 2.
dnl
AC_DEFUN(OCTAVE_MKDIR_TAKES_ONE_ARG, [
AC_LANG_PUSH(C++)
AC_CACHE_CHECK([if mkdir takes one argument], octave_cv_mkdir_takes_one_arg,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>
#endif]], [[mkdir ("foo", 0);]])],
        octave_cv_mkdir_takes_one_arg=no, octave_cv_mkdir_takes_one_arg=yes)])
AC_LANG_POP(C++)
if test $octave_cv_mkdir_takes_one_arg = yes ; then
  AC_DEFINE(MKDIR_TAKES_ONE_ARG, 1, [Define if host mkdir takes a single argument.])
fi
])
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
AC_DEFUN(OCTAVE_PROG_PERL,
[AC_CHECK_PROG(PERL, perl, perl, [])
  AC_SUBST(PERL)
])
dnl
dnl Find Python.
dnl
dnl OCTAVE_PROG_PYTHON
AC_DEFUN(OCTAVE_PROG_PYTHON,
[AC_CHECK_PROG(PYTHON, python, python, [])
  AC_SUBST(PYTHON)
])
dnl
dnl Find desktop-file-install.
dnl
dnl OCTAVE_PROG_DESKTOP_FILE_INSTALL
AC_DEFUN(OCTAVE_PROG_DESKTOP_FILE_INSTALL,
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
  AC_MSG_RESULT($octave_cv_ieee754_data_format)
fi
if test "$octave_cv_ieee754_data_format" = yes; then
  AC_DEFINE(HAVE_IEEE754_DATA_FORMAT, 1, [Define if your system uses IEEE 754 data format.])
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
  AC_MSG_RESULT($octave_cv_umfpack_seperate_split)
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
      CFLAGS_old=$CFLAGS
      CFLAGS="$CFLAGS -DWIN32 -D_HDF5USEDLL_"
      AC_TRY_LINK([#include <hdf5.h>], [hid_t x = H5T_NATIVE_DOUBLE; return x],
        octave_cv_hdf5_dll=yes,
	octave_cv_hdf5_dll=no)
      CFLAGS=$CFLAGS_old])])
  if test "$octave_cv_hdf5_dll" = yes; then
    AC_DEFINE(_HDF5USEDLL_, 1, [Define if using HDF5 dll (Win32)])
  fi])
dnl
dnl Check for the QHull version.
dnl
AC_DEFUN(AC_CHECK_QHULL_VERSION,
[AC_MSG_CHECKING([for qh_qhull in -lqhull with qh_version])
AC_CACHE_VAL(octave_cv_lib_qhull_version,  [
cat > conftest.c <<EOF
#include <stdio.h>
char *qh_version = "version";
char qh_qhull();
int
main(argc, argv)
  int argc;
  char **argv;
{
  qh_qhull();
  return 0;
}
EOF

octave_qhull_try="${CC-cc} $CFLAGS $CPPFLAGS $LDFLAGS conftest.c -o conftest -lqhull $LIBS"
if (eval "$octave_qhull_try") 2>&AS_MESSAGE_LOG_FD && test -s conftest ; then
    octave_cv_lib_qhull_version=yes
else
    octave_cv_lib_qhull_version=no
fi
rm -f conftest.c conftest.o conftest
])dnl
if test "$octave_cv_lib_qhull_version" = "yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$1], , , [$1])
else
  AC_MSG_RESULT(no)
  ifelse([$2], , , [$2])
fi
])
dnl
dnl Check for OpenGL. If found, define OPENGL_LIBS
dnl
dnl FIXME -- add tests for apple
dnl
AC_DEFUN([OCTAVE_OPENGL], [
OPENGL_LIBS=
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
], [
glEnable(GL_SMOOTH);], OPENGL_LIBS="-lopengl32 -lglu32")
      LIBS="$save_LIBS"
      if test "x$OPENGL_LIBS" != "x"; then
        AC_MSG_RESULT(yes)
      else
        AC_MSG_RESULT(no)
      fi
      ;;
    *)
      save_LDFLAGS="$LDFLAGS"
      LDFLAGS="$LDFLAGS -L/usr/X11R6/lib"
      AC_CHECK_LIB(GL, glEnable, OPENGL_LIBS="-L/usr/X11R6/lib -lGL -lGLU")
      LDFLAGS="$save_LDFLAGS"
      ;;
  esac
fi
AC_SUBST(OPENGL_LIBS)
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
