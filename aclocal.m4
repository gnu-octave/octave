dnl aclocal.m4 -- extra macros for configuring Octave
dnl
dnl Copyright (C) 1995 John W. Eaton
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
dnl Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

dnl See what libraries are used by the C++ compiler.  Need this for
dnl dynamic linking.
dnl
dnl Write a minimal program and compile it with -v.  I don't know what
dnl to do if your compiler doesn't have -v...
dnl
dnl OCTAVE_CXXLIBS()
AC_DEFUN(OCTAVE_CXXLIBS,
[AC_REQUIRE([AC_PROG_CXXCPP])
AC_REQUIRE([AC_PROG_CXX])
AC_MSG_CHECKING([for C++ libraries])
AC_CACHE_VAL(octave_cv_cxxlibs,
[AC_LANG_SAVE
AC_LANG_CPLUSPLUS
XCXXFLAGS="$CXXFLAGS"
CXXFLAGS="$XCXXFLAGS -v"
export CXXFLAGS
dnl
dnl This may be too tricky and break with future autoconf releases,
dnl but it works with version 2.3, even with the Ultrix /bin/sh.
dnl
dnl I don't think that stripping commas out of this will ever hurt.
dnl
coutput=`( AC_TRY_LINK([], [], []) ) AC_FD_CC>&1 | sed 's/,/ /g'`
CXXFLAGS="$XCXXFLAGS"
AC_LANG_RESTORE
changequote(, )dnl
dnl
octave_cv_cxxlibs=
lflags=
want_arg=
dnl
for arg in $coutput; do
  if test x$want_arg = x; then
    want_arg=
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
      -[LR]*)
        exists=false
        for f in $lflags; do
          if test x$arg = x$f; then
            exists=true
          fi
        done
      ;;
      -l*)
	if test x$arg = x-lang-c++; then
	  arg=
        else
          lflags="$lflags $arg"
	fi
      ;;
      -u)
        want_arg=$arg
      ;;
      *)
        arg=
      ;;
    esac
  else
    want_arg=
  fi
  if test x$arg != x; then
    octave_cv_cxxlibs="$octave_cv_cxxlibs $arg"
  fi
done
dnl
changequote([, ])])
AC_MSG_RESULT([$octave_cv_cxxlibs])
CXXLIBS="$octave_cv_cxxlibs"
AC_SUBST(CXXLIBS)])

dnl See what libraries are used by the Fortran compiler.
dnl
dnl Write a minimal program and compile it with -v.  I don't know what
dnl to do if your compiler doesn't have -v...
dnl
dnl OCTAVE_FLIBS()
AC_DEFUN(OCTAVE_FLIBS,
[AC_MSG_CHECKING([for Fortran librarires])
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
  sed -n -e 's/.*\(LD_RUN_PATH *= *[^ ]*\).*/\1/p' | \
  sed -e 's/LD_RUN_PATH *= *//'`
dnl
dnl We are only supposed to find this on Solaris systems, and this
dnl substitution is probably only going to work with gcc on those
dnl systems...
dnl
if test -n "$ld_run_path"; then
  ld_run_path="-Xlinker -R -Xlinker $ld_run_path"
fi
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
  case "$old_want_arg" in
    '')
      case $arg in
	/*.a | /*values-X*.o)
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
	-lang*)
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
	  if $exists || test x$arg = x-lm -o x$arg = x-lc; then
	    arg=
	  else
	    lflags="$lflags $arg"
	  fi
	;;
	-u)
	  want_arg=$arg
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
	list="$list -L $elt"
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
changequote([, ])dnl
octave_cv_flibs="$ld_run_path $flibs"])
FLIBS="$octave_cv_flibs"
AC_MSG_RESULT([$FLIBS])])

dnl See if the Fortran compiler uses uppercase external names.
dnl
dnl OCTAVE_F77_UPPERCASE_NAMES()
AC_DEFUN(OCTAVE_F77_UPPERCASE_NAMES,
[AC_MSG_CHECKING([whether $F77 uses uppercase external names])
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

dnl See if the Fortran compiler appends underscores to external names.
dnl
dnl OCTAVE_F77_APPEND_UNDERSCORE()
AC_DEFUN(OCTAVE_F77_APPEND_UNDERSCORE,
[AC_MSG_CHECKING([whether $F77 appends underscores to external names])
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

dnl See if the Fortran compiler is compatible with f2c.
dnl
dnl Write a minimal program, compile it, and see if it works as
dnl expected.
dnl
dnl OCTAVE_F2C_F77_COMPAT()
AC_DEFUN(OCTAVE_F2C_F77_COMPAT,
[AC_REQUIRE([OCTAVE_FLIBS])
AC_REQUIRE([OCTAVE_F77_APPEND_UNDERSCORE])
AC_MSG_CHECKING([$F77/f2c compatibility])
AC_CACHE_VAL(octave_cv_f2c_f77_compat,
[trap 'rm -f ftest* ctest* core; exit 1' 1 3 15
octave_cv_f2c_f77_compat=no
cat > ftest.f <<EOF
      INTEGER FUNCTION FORSUB (C, D)
      CHARACTER *(*) C
      INTEGER L
      DOUBLE PRECISION D
      L = LEN (C)
      WRITE (*, '(A,1X,I2)') C(1:L), INT (D)
      FORSUB = 1
      RETURN
      END
EOF
${F77-f77} -c ftest.f 1>&AC_FD_CC 2>&AC_FD_CC
dnl
changequote(, )
cat > ctest.c <<EOF
#include "confdefs.h"
static char s[14];
int main ()
{
  double d = 10.0;
  int len;
  strcpy (s, "FOO-I-HITHERE");
  len = strlen (s);
#ifdef F77_APPEND_UNDERSCORE
  return (! forsub_ (s, &d, len));
#else
  return (! forsub (s, &d, len));
#endif
}
#ifdef sun
int MAIN_ () { return 0; }
#endif
EOF
changequote([, ])
dnl
if ${CC-cc} -c ctest.c 1>&AC_FD_CC 2>&AC_FD_CC; then
  if ${CC-cc} -o ctest ctest.o ftest.o $FLIBS -lm 1>&AC_FD_CC 2>&AC_FD_CC; then
    ctest_output=`./ctest 2>&1`
    status=$?
    if test $status -eq 0 && test "$ctest_output" = "FOO-I-HITHERE 10"; then
      octave_cv_f2c_f77_compat=yes
    fi
  fi
fi])
rm -f ftest* ctest* core
AC_MSG_RESULT([$octave_cv_f2c_f77_compat])])

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
dnl This is a GNU libc invention, and this check is also from Karl
dnl Berry's kpathsea library.
dnl
AC_DEFUN(OCTAVE_PROGRAM_INVOCATION_NAME,
[AC_MSG_CHECKING(whether program_invocation_name is predefined)
AC_CACHE_VAL(octave_cv_var_program_inv_name,
[AC_TRY_LINK(, [main() { program_invocation_name = "love"; }],
  octave_cv_var_program_inv_name=yes, octave_cv_var_program_inv_name=no)])dnl
AC_MSG_RESULT($octave_cv_var_program_inv_name)
if test $octave_cv_var_program_inv_name = yes; then
  AC_DEFINE(HAVE_PROGRAM_INVOCATION_NAME)
fi])
