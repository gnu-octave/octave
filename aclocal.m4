dnl aclocal.m4 -- extra macros for configuring Octave
dnl
dnl Copyright (C) 1996, 1997 John W. Eaton
dnl 
dnl This file is part of Octave.
dnl 
dnl Octave is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2, or (at your option) any
dnl later version.
dnl 
dnl Octave is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with Octave; see the file COPYING.  If not, write to the Free
dnl Software Foundation, 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA. 
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
dnl 
dnl OCTAVE_CHECK_EXCLUSIVE_WITH_OPTIONS
AC_DEFUN(OCTAVE_CHECK_EXCLUSIVE_WITH_OPTIONS,
[if test "${with_$1+set}" = set; then
  if test "${with_$2+set}" = set; then
    if test "$with_$2" = no; then
      true
    else
      $3
    fi
  fi
fi])
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
dnl See if $F77 is the GNU Fortran compiler
dnl
AC_DEFUN(OCTAVE_PROG_G77,
[AC_CACHE_CHECK([whether ${F77-f77} is the GNU Fortran compiler],
octave_cv_f77_is_g77,
[if $use_g77; then
  octave_cv_f77_is_g77=yes
else
  echo "      END" > conftest.f
  foutput=`${F77-f77} -v conftest.f 2>&1 | egrep "GNU F77|FSF-g77"`
  if test -n "$foutput"; then
    octave_cv_f77_is_g77=yes
  else
    octave_cv_f77_is_g77=no
  fi
fi])
])
dnl
dnl See what libraries are used by the Fortran compiler.
dnl
dnl Write a minimal program and compile it with -v.  I don't know what
dnl to do if your compiler doesn't have -v...
dnl
dnl OCTAVE_FLIBS()
AC_DEFUN(OCTAVE_FLIBS,
[AC_MSG_CHECKING([for Fortran libraries])
AC_REQUIRE([OCTAVE_HOST_TYPE])
AC_CACHE_VAL(octave_cv_flibs,
[changequote(, )dnl
echo "      END" > conftest.f
foutput=`${F77-f77} -v -o conftest conftest.f 2>&1`
dnl
dnl The easiest thing to do for xlf output is to replace all the commas
dnl with spaces.  Try to only do that if the output is really from xlf,
dnl since doing that causes problems on other systems.
dnl
xlf_p=`echo $foutput | grep xlfentry`
if test -n "$xlf_p"; then
  foutput=`echo $foutput | sed 's/,/ /g'`
fi
dnl
ld_run_path=`echo $foutput | \
  sed -n -e 's/^.*LD_RUN_PATH *= *\([^ ]*\).*/\1/p'`
dnl
dnl We are only supposed to find this on Solaris systems...
dnl Uh, the run path should be absolute, shouldn't it?
dnl
case "$ld_run_path" in
  /*)
    if test "$ac_cv_prog_gcc" = yes; then
      ld_run_path="-Xlinker -R -Xlinker $ld_run_path"
    else
      ld_run_path="-R $ld_run_path"
    fi
  ;;
  *)
    ld_run_path=
  ;;
esac
dnl
flibs=
lflags=
dnl
dnl If want_arg is set, we know we want the arg to be added to the list,
dnl so we don't have to examine it.
dnl
want_arg=
dnl
for arg in $foutput; do
  old_want_arg=$want_arg
  want_arg=
dnl
dnl None of the options that take arguments expect the argument to
dnl start with a -, so pretend we didn't see anything special.
dnl
  if test -n "$old_want_arg"; then
    case "$arg" in
      -*)
	old_want_arg=
      ;;
    esac
  fi
  case "$old_want_arg" in
    '')
      case $arg in
	/*.a)
	  exists=false
	  for f in $lflags; do
	    if test x$arg = x$f; then
	      exists=true
	    fi
	  done
	  if $exists; then
	    arg=
	  else
	    lflags="$lflags $arg"
	  fi
	;;
	-bI:*)
	  exists=false
	  for f in $lflags; do
	    if test x$arg = x$f; then
	      exists=true
	    fi
	  done
	  if $exists; then
	    arg=
	  else
	    if test "$ac_cv_prog_gcc" = yes; then
	      lflags="$lflags -Xlinker $arg"
	    else
	      lflags="$lflags $arg"
	    fi
	  fi
	;;
	-lang* | -lcrt0.o | -lc | -lgcc)
	  arg=
	;;
	-[lLR])
	  want_arg=$arg
	  arg=
	;;
	-[lLR]*)
	  exists=false
	  for f in $lflags; do
	    if test x$arg = x$f; then
	      exists=true
	    fi
	  done
	  if $exists; then
	    arg=
	  else
	    case "$arg" in
	      -lkernel32)
		case "$canonical_host_type" in
		  *-*-cygwin32)
		    arg=
		  ;;
		  *)
		    lflags="$lflags $arg"
		  ;;
		esac
	      ;;
	      -lm)
	      ;;
	      *)
		lflags="$lflags $arg"
	      ;;
	    esac
	  fi
	;;
	-u)
	  want_arg=$arg
	  arg=
	;;
	-Y)
	  want_arg=$arg
	  arg=
	;;
	*)
	  arg=
	;;
      esac
    ;;
    -[lLR])
      arg="$old_want_arg $arg"
    ;;
    -u)
      arg="-u $arg"
    ;;
    -Y)
dnl
dnl Should probably try to ensure unique directory options here too.
dnl This probably only applies to Solaris systems, and then will only
dnl work with gcc...
dnl
      arg=`echo $arg | sed -e 's%^P,%%'`
      SAVE_IFS=$IFS
      IFS=:
      list=
      for elt in $arg; do
	list="$list -L$elt"
      done
      IFS=$SAVE_IFS
      arg="$list"
    ;;
  esac
dnl
  if test -n "$arg"; then
    flibs="$flibs $arg"
  fi
done
if test -n "$ld_run_path"; then
  flibs_result="$ld_run_path $flibs"
else
  flibs_result="$flibs"
fi
changequote([, ])dnl
octave_cv_flibs="$flibs_result"])
FLIBS="$octave_cv_flibs"
AC_MSG_RESULT([$FLIBS])])
dnl
dnl This is apparently needed on some Linux systems.
dnl
AC_DEFUN(OCTAVE_F77_MAIN_FLAG,
[FORTRAN_MAIN_FLAG=
case "$canonical_host_type" in
  *-linux-*)
    FORTRAN_MAIN_FLAG="-u MAIN__"
  ;;
esac
if test -n "$FORTRAN_MAIN_FLAG"; then
  AC_MSG_RESULT([defining FORTRAN_MAIN_FLAG to be $FORTRAN_MAIN_FLAG])
fi
AC_SUBST(FORTRAN_MAIN_FLAG)
])
dnl
dnl See if the Fortran compiler uses uppercase external names.
dnl
dnl OCTAVE_F77_UPPERCASE_NAMES()
AC_DEFUN(OCTAVE_F77_UPPERCASE_NAMES,
[AC_MSG_CHECKING([whether ${F77-f77} uses uppercase external names])
AC_CACHE_VAL(octave_cv_f77_uppercase_names,
[octave_cv_f77_uppercase_names=no
cat > conftest.f <<EOF
      subroutine xxyyzz ()
      return
      end
EOF
if ${F77-f77} -c conftest.f 1>&AC_FD_CC 2>&AC_FD_CC; then
  if test "`${NM-nm} conftest.o | grep XXYYZZ`" != ""; then
    octave_cv_f77_uppercase_names=yes
  fi
fi])
AC_MSG_RESULT([$octave_cv_f77_uppercase_names])
if test "$octave_cv_f77_uppercase_names" = yes; then
  AC_DEFINE(F77_UPPERCASE_NAMES, 1)
fi])
dnl
dnl See if the Fortran compiler appends underscores to external names.
dnl
dnl OCTAVE_F77_APPEND_UNDERSCORE()
AC_DEFUN(OCTAVE_F77_APPEND_UNDERSCORE,
[AC_MSG_CHECKING([whether ${F77-f77} appends underscores to external names])
AC_REQUIRE([OCTAVE_F77_UPPERCASE_NAMES])
AC_CACHE_VAL(octave_cv_f77_append_underscore,
[octave_cv_f77_append_underscore=no
cat > conftest.f <<EOF
      subroutine xxyyzz ()
      return
      end
EOF
if ${F77-f77} -c conftest.f 1>&AC_FD_CC 2>&AC_FD_CC; then
  if test "$octave_cv_f77_uppercase_names" = yes; then
    if test "`${NM-nm} conftest.o | grep XXYYZZ_`" != ""; then
      octave_cv_f77_append_underscore=yes
    fi
  else
    if test "`${NM-nm} conftest.o | grep xxyyzz_`" != ""; then
      octave_cv_f77_append_underscore=yes
    fi
  fi
fi])
AC_MSG_RESULT([$octave_cv_f77_append_underscore])
if test "$octave_cv_f77_append_underscore" = yes; then
  AC_DEFINE(F77_APPEND_UNDERSCORE, 1)
fi])
dnl
dnl See if the Fortran compiler is compatible with f2c.
dnl
dnl Write a minimal program, compile it, and see if it works as
dnl expected.
dnl
dnl OCTAVE_F2C_F77_COMPAT()
AC_DEFUN(OCTAVE_F2C_F77_COMPAT,
[AC_REQUIRE([OCTAVE_PROG_G77])
AC_REQUIRE([OCTAVE_FLIBS])
AC_REQUIRE([OCTAVE_F77_APPEND_UNDERSCORE])
if test "$cross_compiling" = yes; then
  octave_cv_f2c_f77_compat=yes
  if test "$octave_cv_f77_is_g77" = yes; then
    AC_MSG_RESULT([assuming ${F77-f77} cross compiler is f2c compatible])
  else
    AC_MSG_WARN([assuming ${F77-f77} cross compiler is f2c compatible])
  fi
else
  AC_CACHE_CHECK([${F77-f77}/f2c compatibility], octave_cv_f2c_f77_compat,
  [trap 'rm -f ftest* ctest* core; exit 1' 1 3 15
  octave_cv_f2c_f77_compat=no
  cat > ftest.f <<EOF
      SUBROUTINE FORSUB (C, I, D)
      CHARACTER *(*) C
      INTEGER L, I
      DOUBLE PRECISION D
      L = LEN (C)
      WRITE (*, '(A,1X,I2)') C(1:L), INT (D)
      I = 0
      RETURN
      END
EOF
  ${F77-f77} -c ftest.f 1>&AC_FD_CC 2>&AC_FD_CC
  changequote(, )
  cat > ctest.c <<EOF
#include "confdefs.h"
#ifdef F77_APPEND_UNDERSCORE
extern int forsub_ (const char*, int*, double*, long int);
#else
extern int forsub (const char*, int*, double*, long int);
#endif
static char s[14];
int main ()
{
  double d = 10.0;
  int len, i = 1;
  strcpy (s, "FOO-I-HITHERE");
  len = strlen (s);
#ifdef F77_APPEND_UNDERSCORE
  forsub_ (s, &i, &d, len);
#else
  forsub (s, &i, &d, len);
#endif
  return i;
}
#if defined (sun)
int MAIN_ () { return 0; }
#elif defined (linux) && defined (__ELF__)
int MAIN__ () { return 0; }
#endif
EOF
  changequote([, ])
  if ${CC-cc} -c ctest.c 1>&AC_FD_CC 2>&AC_FD_CC; then
    if ${CC-cc} -o ctest ctest.o ftest.o $FLIBS -lm 1>&AC_FD_CC 2>&AC_FD_CC; then
      ctest_output=`./ctest 2>&1`
      status=$?
      if test $status -eq 0 && test "$ctest_output" = "FOO-I-HITHERE 10"; then
	octave_cv_f2c_f77_compat=yes
      fi
    fi
  fi])
fi
rm -f ftest* ctest* core
])
dnl
dnl See if struct group has a gr_passwd field.
dnl
AC_DEFUN(OCTAVE_STRUCT_GR_PASSWD,
[AC_CACHE_CHECK([for gr_passwd in struct group], octave_cv_struct_gr_passwd,
[AC_TRY_COMPILE([#include <sys/types.h>
#include <grp.h>], [struct group s; s.gr_passwd;],
octave_cv_struct_gr_passwd=yes, octave_cv_struct_gr_passwd=no)])
if test $octave_cv_struct_gr_passwd = yes; then
  AC_DEFINE(HAVE_GR_PASSWD)
fi
])
dnl
dnl See if the standard string class has npos as a member.
dnl
AC_DEFUN(OCTAVE_STRING_NPOS,
[AC_CACHE_CHECK([whether including <string> defines NPOS],
octave_cv_string_npos,
[AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_COMPILE([#include <string>],
[size_t foo = NPOS],
octave_cv_string_npos=yes, octave_cv_string_npos=no)])
if test $octave_cv_string_npos = no; then
  AC_DEFINE(NPOS, std::string::npos)
fi
AC_LANG_RESTORE
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
[AC_TRY_RUN([
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
}], octave_cv_func_putenv_malloc=yes, octave_cv_func_putenv_malloc=no,
    octave_cv_func_putenv_malloc=no)])dnl
AC_MSG_RESULT($octave_cv_func_putenv_malloc)
if test $octave_cv_func_putenv_malloc = yes; then
  AC_DEFINE(SMART_PUTENV)
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
  AC_TRY_LINK([#include <signal.h>],[
    sigset_t ss;
    struct sigaction sa;
    sigemptyset(&ss); sigsuspend(&ss);
    sigaction(SIGINT, &sa, (struct sigaction *) 0);
    sigprocmask(SIG_BLOCK, &ss, (sigset_t *) 0);
  ], octave_cv_signal_vintage=posix,
  [
    AC_TRY_LINK([#include <signal.h>], [
	int mask = sigmask(SIGINT);
	sigsetmask(mask); sigblock(mask); sigpause(mask);
    ], octave_cv_signal_vintage=4.2bsd,
    [
      AC_TRY_LINK([
	#include <signal.h>
	RETSIGTYPE foo() { }], [
		int mask = sigmask(SIGINT);
		sigset(SIGINT, foo); sigrelse(SIGINT);
		sighold(SIGINT); sigpause(SIGINT);
        ], octave_cv_signal_vintage=svr3, octave_cv_signal_vintage=v7
    )]
  )]
)
])
AC_MSG_RESULT($octave_cv_signal_vintage)
if test "$octave_cv_signal_vintage" = posix; then
AC_DEFINE(HAVE_POSIX_SIGNALS)
elif test "$octave_cv_signal_vintage" = "4.2bsd"; then
AC_DEFINE(HAVE_BSD_SIGNALS)
elif test "$octave_cv_signal_vintage" = svr3; then
AC_DEFINE(HAVE_USG_SIGHOLD)
fi
])
dnl
AC_DEFUN(OCTAVE_REINSTALL_SIGHANDLERS,
[AC_REQUIRE([AC_TYPE_SIGNAL])
AC_REQUIRE([OCTAVE_SIGNAL_CHECK])
AC_MSG_CHECKING([if signal handlers must be reinstalled when invoked])
AC_CACHE_VAL(octave_cv_must_reinstall_sighandlers,
[AC_TRY_RUN([
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
], octave_cv_must_reinstall_sighandlers=no, octave_cv_must_reinstall_sighandlers=yes,
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
  AC_DEFINE(MUST_REINSTALL_SIGHANDLERS)
fi
])
dnl
dnl This check originally from bash 2.0.
dnl
dnl Check for typedef'd symbols in header files, but allow the caller to
dnl specify the include files to be checked in addition to the default.
dnl 
dnl OCTAVE_CHECK_TYPE(TYPE, HEADERS, DEFAULT[, VALUE-IF-FOUND])
AC_DEFUN(OCTAVE_CHECK_TYPE,
[AC_REQUIRE([AC_HEADER_STDC])dnl
AC_MSG_CHECKING(for $1)
AC_CACHE_VAL(octave_cv_type_$1,
[AC_EGREP_CPP($1, [#include <sys/types.h>
#if STDC_HEADERS
#include <stdlib.h>
#endif
$2
], octave_cv_type_$1=yes, octave_cv_type_$1=no)])
AC_MSG_RESULT($octave_cv_type_$1)
ifelse($#, 4, [if test $octave_cv_type_$1 = yes; then
	AC_DEFINE($4)
	fi])
if test $octave_cv_type_$1 = no; then
  AC_DEFINE($1, $3)
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
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
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
    AC_TRY_LINK([#include "conftest.h"], [
        A a (1);
        return a == A(1);
      ], 
      octave_cv_cxx_new_friend_template_decl=no,
      octave_cv_cxx_new_friend_template_decl=yes
    )
    AC_LANG_RESTORE
  ])
  AC_MSG_RESULT($octave_cv_cxx_new_friend_template_decl)
  if test $octave_cv_cxx_new_friend_template_decl = yes; then
    AC_DEFINE(CXX_NEW_FRIEND_TEMPLATE_DECL)
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
  ac_safe=`echo "$1" | sed 'y%./+-%__p_%'`
  AC_MSG_CHECKING(whether ${CC-cc} accepts $1)
  AC_CACHE_VAL(octave_cv_cc_flag_$ac_safe, [
    AC_LANG_SAVE
    AC_LANG_C
    XCFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $1"
    AC_TRY_LINK([], [],
      eval "octave_cv_cc_flag_$ac_safe=yes",
      eval "octave_cv_cc_flag_$ac_safe=no")
    CFLAGS="$XCFLAGS"
    AC_LANG_RESTORE
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
  ac_safe=`echo "$1" | sed 'y%./+-%__p_%'`
  AC_MSG_CHECKING(whether ${CXX-g++} accepts $1)
  AC_CACHE_VAL(octave_cv_cxx_flag_$ac_safe, [
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    XCXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS $1"
    AC_TRY_LINK([], [],
      eval "octave_cv_cxx_flag_$ac_safe=yes",
      eval "octave_cv_cxx_flag_$ac_safe=no")
    CXXFLAGS="$XCXXFLAGS"
    AC_LANG_RESTORE
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
    *-*-cygwin32)
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
dnl Does gnuplot exist?  Is it a recent version?
dnl
AC_DEFUN(OCTAVE_PROG_GNUPLOT,
[if test "$cross_compiling" = yes; then
  GNUPLOT_BINARY=gnuplot
  AC_MSG_RESULT(assuming $GNUPLOT_BINARY exists on $canonical_host_type host)
  AC_SUBST(DEFAULT_PAGER)
  AC_MSG_RESULT(assuming $GNUPLOT_BINARY supports multiplot mode)
  AC_DEFINE(GNUPLOT_HAS_MULTIPLOT, 1)
  AC_MSG_RESULT(assuming $GNUPLOT_BINARY supports multiple frams)
  AC_DEFINE(GNUPLOT_HAS_FRAMES, 1)
else
  AC_CHECK_PROG(GNUPLOT_BINARY, gnuplot, gnuplot, [])
  if test -n "$GNUPLOT_BINARY"; then
    AC_MSG_CHECKING([to see if your gnuplot supports multiplot])
    if test -z "`echo 'set term unknown; set multiplot' | \
      $GNUPLOT_BINARY 2>&1`"; then
      AC_MSG_RESULT([yes])
      AC_DEFINE(GNUPLOT_HAS_MULTIPLOT, 1)
    else
      AC_MSG_RESULT([no])
    fi
    AC_MSG_CHECKING([to see if your gnuplot supports multiple plot windows])
    if test -z "`echo 'set term x11 2' | $GNUPLOT_BINARY 2>&1`"; then
      AC_MSG_RESULT([yes])
      AC_DEFINE(GNUPLOT_HAS_FRAMES, 1)
    else
      AC_MSG_RESULT([no])
    fi
  else
    warn_gnuplot="yes"

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
  fi
fi
])
dnl
dnl Is DejaGNU installed?
dnl
dnl OCTAVE_PROG_RUNTEST
AC_DEFUN(OCTAVE_PROG_RUNTEST,
[if test "$cross_compiling" = yes; then
  RUNTEST=runtest
  AC_MSG_RESULT(assuming $RUNTEST exists on $canonical_host_type host)
  AC_SUBST(RUNTEST)
else
  AC_CHECK_PROG(RUNTEST, runtest, runtest, [])
  if test -z "$RUNTEST"; then
    warn_runtest="I didn't find runtest -- install DejaGNU if you want to run \`make check'"
    AC_MSG_WARN($warn_runtest)
  fi
  AC_SUBST(RUNTEST)
fi
])
dnl
dnl Is gperf installed?
dnl
dnl OCTAVE_PROG_GPERF
AC_DEFUN(OCTAVE_PROG_GPERF,
[AC_CHECK_PROG(GPERF, gperf, gperf, [])
if test -z "$GPERF"; then
  warn_gperf="I didn't find gperf, but it's only a problem if you need to reconstruct oct-gperf.h"
  AC_MSG_WARN($warn_gperf)
fi
AC_SUBST(GPERF)
])
dnl
dnl Find nm.
dnl
dnl OCTAVE_PROG_NM
AC_DEFUN(OCTAVE_PROG_NM,
[if test "$cross_compiling" = yes; then
  NM=nm
  AC_MSG_RESULT(assuming $NM exists on $canonical_host_type host)
  AC_SUBST(NM)
else
  AC_CHECK_PROG(NM, nm, nm, [])
  AC_SUBST(NM)
fi
])
dnl
dnl See if the C++ compiler prepends an underscore to external names.
dnl
dnl OCTAVE_CXX_PREPENDS_UNDERSCORE
AC_DEFUN(OCTAVE_CXX_PREPENDS_UNDERSCORE,
[AC_MSG_CHECKING([whether ${CXX-g++} prepends an underscore to external names])
  AC_CACHE_VAL(octave_cv_cxx_prepends_underscore,
    [octave_cv_cxx_prepends_underscore=no
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    cat > conftest.$ac_ext <<EOF
bool FSmy_dld_fcn (void) { }
EOF
    if AC_TRY_EVAL(ac_compile); then
      if test "`${NM-nm} conftest.o | grep _FSmy_dld_fcn`" != ""; then
        octave_cv_cxx_prepends_underscore=yes
      fi
    else
      echo "configure: failed program was:" >&AC_FD_CC
      cat conftest.$ac_ext >&AC_FD_CC
    fi
    AC_LANG_RESTORE
  ])
  AC_MSG_RESULT($octave_cv_cxx_prepends_underscore)
  if test $octave_cv_cxx_prepends_underscore = yes; then
    AC_DEFINE(CXX_PREPENDS_UNDERSCORE)
  fi
])
