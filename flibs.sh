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

# The easiest thing to do for xlf output is to replace all the commas
# with spaces.  That causes problems for the output from f77 on
# Solaris systems that looks something like
#
#  -Y P,colon:separated:path:of:lib:directories
#
# and that this sed command transforms to
#
#  -Y P colon:separated:path:of:lib:directories
#
# This is handled as a somewhat special case below.

foutput=`${F77-f77} -v -o conftest conftest.f 2>&1 | sed 's/,/ /g'`

flibs=
lflags=

# If want arg is set, we know we want the arg to be added to the list,
# so we don't have to examine it.
want_arg=

for arg in $foutput
do
  if test x$want_arg = x
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
      if test x$arg = xP
      then
        want_arg=$arg
      else
        want_arg=
      fi
      arg=
    else
      if test x$want_arg = xP
      then
	dir_list=`echo $arg | sed 's/^[ \t]*P,//'`
	arg=
	save_IFS=$IFS
	IFS=":"
	for dir in $dir_list
	do
	  exists=false
	  for f in $lflags
	  do
	    if test x$dir = x$f
	    then
	      exists=true
	    fi
	  done
	  if $exists || test x$dir = x-lm -o x$dir = x-lc
	  then
	    true
	  else
	    lflags="$lflags $dir"
	    arg="$arg -L $dir"
	  fi
	done
	IFS="$save_IFS"
      fi
      want_arg=
    fi
  fi

  if test "x$arg" = x
  then
    true
  else
    flibs="$flibs $arg"
  fi
done

echo "$flibs"

rm -f conftest* core

# Bye-bye.

exit 0
