#!/bin/sh
#
# cxxlibs -- try to get the C++ compiler to tell us what libraries
# it expects to link to, and echo the result to the standard output.
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

trap 'rm -f conftest* core; exit 1' 1 3 15

# Write a minimal program and compile it with -v.  I don't know what
# to do if your compiler doesn't have -v...

echo "int main (void) { return 0; }" > conftest.c

# I don't think that stripping commas out of this will ever hurt.

coutput=`${CXX-c++} -v -o conftest conftest.c 2>&1 | sed 's/,/ /g'`

clibs=
lflags=
want_arg=

for arg in $coutput
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
      -[LR]*)
        exists=false
        for f in $lflags
        do
          if test x$arg = x$f
          then
            exists=true
          fi
        done
      ;;
      -l*)
	if test x$arg = x-lang-c++
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
    clibs="$clibs $arg"
  fi
done

echo "$clibs"

rm -f conftest* core

# Bye-bye.

exit 0
