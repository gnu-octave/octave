#!/bin/sh
#
# flibs -- try to get the Fortran compiler to tell us what libraries
# it expects to link to, and echo the result to the standard output.
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

trap 'rm -f conftest* core; exit 1' 1 3 15

# Write a minimal program and compile it with -v.  I don't know what
# to do if your compiler doesn't have -v...

echo "      END" > conftest.f

if test $# -eq 1
then
  foutput=`cat $1`
else
  foutput=`${F77-f77} -v -o conftest conftest.f 2>&1`
fi

# The easiest thing to do for xlf output is to replace all the commas
# with spaces.  Try to only do that if the output is really from xlf,
# since doing that causes problems on other systems.

xlf_p=`echo $foutput | grep xlfentry`
if test -n "$xlf_p"
then
  foutput=`echo $foutput | sed 's/,/ /g'`
fi

ld_run_path=`echo $foutput | \
  sed -n -e 's/.*\(LD_RUN_PATH *= *[^ ]*\).*/\1/p' | \
  sed -e 's/LD_RUN_PATH *= *//'`

if test -n "$ld_run_path"
then
  ld_run_path="-Xlinker -R -Xlinker $ld_run_path"
fi

flibs=
lflags=

# If want arg is set, we know we want the arg to be added to the list,
# so we don't have to examine it.
want_arg=

for arg in $foutput
do
  if test -z "$want_arg"
  then
    case $arg in
      /*.a)
        exists=false
        for f in $lflags
        do
          if test x$arg = x$f
          then
            exists=true
          fi
        done
	if $exists
	then
	  arg=
        else
          lflags="$lflags $arg"
	fi
      ;;
      -[lL]*)
        exists=false
        for f in $lflags
        do
          if test x$arg = x$f
          then
            exists=true
          fi
        done
	if $exists || test x$arg = x-lm -o x$arg = x-lc
	then
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
  else
    if test x$want_arg = x-Y
    then
      arg="-Xlinker -Y -Xlinker $arg"
    fi
    want_arg=
  fi

  if test -n "$arg"
  then
    flibs="$flibs $arg"
  fi
done

echo "$ld_run_path $flibs"

rm -f conftest* core

# Bye-bye.

exit 0
