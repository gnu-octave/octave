dnl Modifications for the latest version of Autoconf for kpathsea.
dnl These changes have all been sent back to the Autoconf maintainer.

dnl This is a subroutine of AC_OUTPUT.  It is called inside an unquoted
dnl here document whose contents are going into config.status.
dnl AC_OUTPUT_FILES(FILE...)
dnl I've added ac_include support --karl@cs.umb.edu.
undefine([AC_OUTPUT_FILES])
define(AC_OUTPUT_FILES,
[# Protect against being on the right side of a sed subst in config.status. 
changequote(, )dnl
sed 's/%@/@@/; s/@%/@@/; s/%g$/@g/; /@g$/s/[\\\\&%]/\\\\&/g; 
 s/@@/%@/; s/@@/@%/; s/@g$/%g/' > conftest.subs <<\CEOF
changequote([, ])dnl
dnl These here document variables are unquoted when configure runs
dnl but quoted when config.status runs, so variables are expanded once.
$ac_vpsub
dnl Shell code in configure.in might set extrasub.
$extrasub
dnl Insert the sed substitutions of variables.
undivert(AC_DIVERSION_SED)
CEOF
EOF
cat >> $CONFIG_STATUS <<EOF

CONFIG_FILES=\${CONFIG_FILES-"$1"}
EOF
cat >> $CONFIG_STATUS <<\EOF
for ac_file in .. $CONFIG_FILES; do if test "x$ac_file" != x..; then
dnl Specifying an input file breaks the trap to clean up on interrupt,
dnl but that's not a huge problem.
  # Support "outfile[:infile]", defaulting infile="outfile.in".
  case "$ac_file" in
  *:*) ac_file_in=`echo "$ac_file"|sed 's%.*:%%'`
       ac_file=`echo "$ac_file"|sed 's%:.*%%'` ;;
  *) ac_file_in="${ac_file}.in" ;;
  esac

  # Adjust relative srcdir, etc. for subdirectories.

  # Remove last slash and all that follows it.  Not all systems have dirname.
changequote(, )dnl
  ac_dir=`echo $ac_file|sed 's%/[^/][^/]*$%%'`
changequote([, ])dnl
  if test "$ac_dir" != "$ac_file" && test "$ac_dir" != .; then
    # The file is in a subdirectory.
    test ! -d "$ac_dir" && mkdir "$ac_dir"
    ac_dir_suffix="/$ac_dir"
    # A "../" for each directory in $ac_dir_suffix.
changequote(, )dnl
    ac_dots=`echo $ac_dir_suffix|sed 's%/[^/]*%../%g'`
changequote([, ])dnl
  else
    ac_dir_suffix= ac_dots=
  fi

  case "$ac_given_srcdir" in
  .)  srcdir=.
      if test -z "$ac_dots"; then top_srcdir=.
      else top_srcdir=`echo $ac_dots|sed 's%/$%%'`; fi ;;
  /*) srcdir="$ac_given_srcdir$ac_dir_suffix"; top_srcdir="$ac_given_srcdir" ;;
  *) # Relative path.
    srcdir="$ac_dots$ac_given_srcdir$ac_dir_suffix"
    top_srcdir="$ac_dots$ac_given_srcdir" ;;
  esac

ifdef([AC_PROVIDE_AC_PROG_INSTALL],
[  case "$ac_given_INSTALL" in
changequote(, )dnl
  [/$]*) INSTALL="$ac_given_INSTALL" ;;
changequote([, ])dnl
  *) INSTALL="$ac_dots$ac_given_INSTALL" ;;
  esac
])dnl
  echo creating "$ac_file"
  rm -f "$ac_file"
  configure_input="Generated automatically from `echo $ac_file_in|sed 's%.*/%%'` by configure."
  case "$ac_file" in
  *Makefile*) ac_comsub="1i\\
# $configure_input" ;;
  *) ac_comsub= ;;
  esac
  # Replace lines of the form ac_include foo with the contents of foo:
  # first, from the ac_include lines construct a list of sed
  # commands to remove them, and include the files; then run sed.
  # Have to use sed because old (Ultrix, SunOS) awk does not support
  # getline or system. (Anyway, configure scripts aren't supposed to use awk.)
  # Use \@...@<cmd> form of sed address because the filename may contain /.
  # Can't use only one -e and commands {dr foo} because foo has to be last.
  # Use @e initially instead of -e because GNU echo has a -e option.
  # USe `X' to mean a bunch of backslashes; this is for FreeBSD.
  file_substs=`sed -n \
changequote(,)dnl
"/^ac_include/s%ac_include[ 	]*\(.*\)%@e 'X@^&@r \1' -e 'X@^&@d'%p" \
changequote([,])dnl
               $ac_given_srcdir/${ac_file}.in`
  if test -n "$file_substs"; then
    # Change @e back to -e and X@^ to \@^.
    file_subst_cmd="sed `echo $file_substs \
                         | sed -e 's/@e/-e/g' -e 's/X@^/\\\\\@^/g'`"
  else
    # If no substitutions and hence no sed commands, don't choke.
    file_subst_cmd=cat
  fi
  # cd into the srcdir because the files being included more or less
  # must be part of the distribution. I can't find any way to do
  # variable substitution in the sed commands (so the user could have,
  # e.g., $top_srcdir in their ac_include line).
  (cd $ac_given_srcdir && eval $file_subst_cmd ${ac_file}.in) \
  | sed -e "$ac_comsub
s%@configure_input@%$configure_input%g
s%@srcdir@%$srcdir%g
s%@top_srcdir@%$top_srcdir%g
ifdef([AC_PROVIDE_AC_PROG_INSTALL], [s%@INSTALL@%$INSTALL%g
])dnl
" -f conftest.subs > $ac_file
fi; done
rm -f conftest.subs
])


dnl 
dnl Only change from Autoconf 2.1 is to check IceConnectionNumber, not
dnl ...Numbers.
dnl
dnl Find additional X libraries, magic flags, etc.
undefine([AC_PATH_XTRA])
AC_DEFUN(AC_PATH_XTRA,
[AC_REQUIRE([AC_ISC_POSIX])dnl
AC_REQUIRE([AC_PATH_X])dnl
if test "$no_x" = yes; then 
  # Not all programs may use this symbol, but it does not hurt to define it.
  X_CFLAGS="$X_CFLAGS -DX_DISPLAY_MISSING"
else
  if test -n "$x_includes"; then
    X_CFLAGS="$X_CFLAGS -I$x_includes"
  fi

  # It would be nice to have a more robust check for the -R ld option than
  # just checking for Solaris.
  # It would also be nice to do this for all -L options, not just this one.
  if test -n "$x_libraries"; then
    X_LIBS="$X_LIBS -L$x_libraries"
    if test "`(uname) 2>/dev/null`" = SunOS &&
      uname -r | grep '^5' >/dev/null; then
      X_LIBS="$X_LIBS -R$x_libraries"
    fi
  fi

  # Check for libraries that X11R6 Xt/Xaw programs need.

  ac_save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS -L$x_libraries"
  # SM needs ICE to (dynamically) link under SunOS 4.x (so we have to
  # check for ICE first), but we must link in the order -lSM -lICE or
  # we get undefined symbols.  So assume we have SM if we have ICE.
  # These have to be linked with before -lX11, unlike the other
  # libraries we check for below, so use a different variable.
  #  --interran@uluru.Stanford.EDU, kb@cs.umb.edu.
  AC_CHECK_LIB(ICE, IceConnectionNumber,
    [X_PRE_LIBS="$X_PRE_LIBS -lSM -lICE"])
  LDFLAGS="$ac_save_LDFLAGS"

  # Check for system-dependent libraries X programs must link with.

  if test "$ISC" = yes; then
    X_EXTRA_LIBS="$X_EXTRA_LIBS -lnsl_s -linet"
  else
    # Martyn.Johnson@cl.cam.ac.uk says this is needed for Ultrix, if the X
    # libraries were built with DECnet support.  And karl@cs.umb.edu says
    # the Alpha needs dnet_stub (dnet does not exist).
    AC_CHECK_LIB(dnet, dnet_ntoa, [X_EXTRA_LIBS="$X_EXTRA_LIBS -ldnet"])
    if test $ac_cv_lib_dnet = no; then
      AC_CHECK_LIB(dnet_stub, dnet_ntoa,
        [X_EXTRA_LIBS="$X_EXTRA_LIBS -ldnet_stub"])
    fi

    # msh@cis.ufl.edu says -lnsl (and -lsocket) are needed for his 386/AT,
    # to get the SysV transport functions.
    # Not sure which flavor of 386 UNIX this is, but it seems harmless to
    # check for it.
    AC_CHECK_LIB(nsl, t_accept, [X_EXTRA_LIBS="$X_EXTRA_LIBS -lnsl"])

    # lieder@skyler.mavd.honeywell.com says without -lsocket,
    # socket/setsockopt and other routines are undefined under SCO ODT 2.0.
    # But -lsocket is broken on IRIX, according to simon@lia.di.epfl.ch.
    if test "`(uname) 2>/dev/null`" != IRIX; then
      AC_CHECK_LIB(socket, socket, [X_EXTRA_LIBS="$X_EXTRA_LIBS -lsocket"])
    fi
  fi
fi
AC_SUBST(X_CFLAGS)dnl
AC_SUBST(X_PRE_LIBS)dnl
AC_SUBST(X_LIBS)dnl
AC_SUBST(X_EXTRA_LIBS)dnl
])


dnl 
dnl Definition was buggy in Autoconf 2.1; parameters were incorrectly
dnl passed to AC_MSG_ERROR and AC_TRY_LINK.
dnl
dnl Check if lex declares yytext as a char * by default, not a char[].
undefine([AC_DECL_YYTEXT])
AC_DEFUN(AC_DECL_YYTEXT,
[AC_REQUIRE_CPP()dnl
AC_REQUIRE([AC_PROG_LEX])dnl
AC_CACHE_VAL(ac_cv_prog_lex_output_root,
[# The minimal lex program is just a single line: %%.  But some broken lexes
# (Solaris, I think it was) want two %% lines, so accommodate them.
echo '%%
%%' | $LEX
if test -f lex.yy.c; then
  ac_cv_prog_lex_output_root=lex.yy
elif test -f lexyy.c; then
  ac_cv_prog_lex_output_root=lexyy
else
  AC_MSG_ERROR([cannot find output from $LEX, giving up])
fi])dnl
LEX_OUTPUT_ROOT=$ac_cv_prog_lex_output_root
AC_SUBST(LEX_OUTPUT_ROOT)dnl
AC_MSG_CHECKING(for yytext declaration)
AC_CACHE_VAL(ac_cv_prog_lex_yytext_pointer,
[# POSIX says lex can declare yytext either as a pointer or an array; the
# default is implementation-dependent. Figure out which it is, since
# not all implementations provide the %pointer and %array declarations.
ac_cv_prog_lex_yytext_pointer=no
echo 'extern char *yytext;' >>$LEX_OUTPUT_ROOT.c
ac_save_LIBS="$LIBS"
LIBS="$LIBS $LEXLIB"
AC_TRY_LINK(`cat $LEX_OUTPUT_ROOT.c`,, ac_cv_prog_lex_yytext_pointer=yes)
LIBS="$ac_save_LIBS"
rm -f "${LEX_OUTPUT_ROOT}.c"])dnl
AC_MSG_RESULT($ac_cv_prog_lex_yytext_pointer)
if test $ac_cv_prog_lex_yytext_pointer = yes; then
  AC_DEFINE(YYTEXT_POINTER)
fi
])

dnl 
dnl install-sh needs .. magic.
dnl
dnl AC_OUTPUT_SUBDIRS(DIRECTORY...)
undefine([AC_OUTPUT_SUBDIRS])
define(AC_OUTPUT_SUBDIRS,
[
if test "$no_recursion" != yes; then

  # Remove --cache-file and --srcdir arguments so they do not pile up.
  ac_sub_configure_args=
  ac_prev=
  for ac_arg in $ac_configure_args; do
    if test -n "$ac_prev"; then
      ac_prev=
      continue
    fi
    case "$ac_arg" in
    -cache-file | --cache-file | --cache-fil | --cache-fi \
    | --cache-f | --cache- | --cache | --cach | --cac | --ca | --c)
      ac_prev=cache_file ;;
    -cache-file=* | --cache-file=* | --cache-fil=* | --cache-fi=* \
    | --cache-f=* | --cache-=* | --cache=* | --cach=* | --cac=* | --ca=* | --c=*)
      ;;
    -srcdir | --srcdir | --srcdi | --srcd | --src | --sr)
      ac_prev=srcdir ;;
    -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=*)
      ;;
    *) ac_sub_configure_args="$ac_sub_configure_args $ac_arg" ;;
    esac
  done

  for ac_config_dir in $1; do

    # Do not complain, so a configure script can configure whichever
    # parts of a large source tree are present.
    if test ! -d $srcdir/$ac_config_dir; then
      continue
    fi

    echo configuring in $ac_config_dir

    case "$srcdir" in
    .) ;;
    *)
      if test -d ./$ac_config_dir || mkdir ./$ac_config_dir; then :;
      else
        AC_MSG_ERROR(can not create `pwd`/$ac_config_dir)
      fi
      ;;
    esac

    ac_popdir=`pwd`
    cd $ac_config_dir

    case "$srcdir" in
    .) # No --srcdir option.  We are building in place.
      ac_sub_srcdir=$srcdir ;;
    /*) # Absolute path.
      ac_sub_srcdir=$srcdir/$ac_config_dir ;;
    *) # Relative path.
      ac_sub_srcdir=../$srcdir/$ac_config_dir ;;
    esac

    # Check for guested configure; otherwise get Cygnus style configure.
    if test -f $ac_sub_srcdir/configure; then
      ac_sub_configure=$ac_sub_srcdir/configure
    elif test -f $ac_sub_srcdir/configure.in; then
      ac_sub_configure=$ac_configure
    else
      AC_MSG_WARN(no configuration information is in $ac_config_dir)
      ac_sub_configure=
    fi

    # The recursion is here.
    if test -n "$ac_sub_configure"; then

      # Make the cache file name correct relative to the subdirectory.
changequote(, )dnl
      # A "../" for each directory in /$ac_config_dir.
      ac_dots=`echo /$ac_config_dir|sed 's%/[^/]*%../%g'`
changequote([, ])dnl
      case "$cache_file" in
      /*) ac_sub_cache_file=$cache_file ;;
      *) # Relative path.
        ac_sub_cache_file="$ac_dots$cache_file" ;;
      esac

ifdef([AC_PROVIDE_AC_PROG_INSTALL],
      [  case "$ac_given_INSTALL" in
changequote(, )dnl
        [/$]*) INSTALL="$ac_given_INSTALL" ;;
changequote([, ])dnl
        *) INSTALL="$ac_dots$ac_given_INSTALL" ;;
        esac
])dnl

      echo "[running ${CONFIG_SHELL-/bin/sh} $ac_sub_configure $ac_sub_configure_args --cache-file=$ac_sub_cache_file] --srcdir=$ac_sub_srcdir"
      # The eval makes quoting arguments work.
      if eval ${CONFIG_SHELL-/bin/sh} $ac_sub_configure $ac_sub_configure_args --cache-file=$ac_sub_cache_file --srcdir=$ac_sub_srcdir
      then :
      else
        AC_MSG_ERROR($ac_sub_configure failed for $ac_config_dir)
      fi
    fi

    cd $ac_popdir
  done
fi
])
