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

# I don't think that stripping commas out of this will ever hurt, and
# we have to do it for the code that follows to understand the output
# from `xlf -v'.  

foutput=`${F77-f77} -v -o conftest conftest.f 2>&1 | sed 's/,/ /g'`

flibs=
lflags=
want_arg=

for arg in $foutput
do
  if test x$want_arg = x
  then
    want_arg=
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
      *)
        arg=
      ;;
    esac
  else
    want_arg=
  fi
  if test x$arg != x
  then
    flibs="$flibs $arg"
  fi
done

echo "$flibs"

rm -f conftest* core

# Bye-bye.

exit 0
