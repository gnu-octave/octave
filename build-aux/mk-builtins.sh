#! /bin/sh
#
# Copyright (C) 1996-2016 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

set -e

SED=${SED:-sed}

defun_dld_are_built_in=false
make_header=false
make_source=false

for arg
do
  case "$arg" in
    --header)
      if $make_source; then
        echo "mk-builtins.sh: only one of --header or --source may be specified" 1>&2
        exit 1
      fi
      make_header=true
      shift
    ;;
    --source)
      if $make_header; then
        echo "mk-builtins.sh: only one of --header or --source may be specified" 1>&2
        exit 1
      fi
      make_source=true
      shift
    ;;
    --disable-dl)
      ## If DLD functions are disabled, then DEFUN_DLD functions are
      ## built-in instead of being dynamically loaded so they will also
      ## need to be installed.
      defun_dld_are_built_in=true
      shift
    ;;
    *)
      srcdir="$arg"
      shift
      if [ "x$1" = "x--" ]; then
        shift
        break
      else
        echo "mk-builtins.sh: '--' must separate SRCDIR from other file names" 1>&2
        exit 1
      fi
    ;;
  esac
done

if [ $# -eq 0 ]; then
  echo "usage: mk-builtins.sh --header|--source [--disable-dl] SRCDIR -- f1 f2 ..." 1>&2
  exit 1
fi

if ! $make_header && ! $make_source; then
  echo "mk-builtins.sh: one of --header or --source must be specified" 1>&2
  exit 1
fi

if $make_header; then

  cat << \EOF
// DO NOT EDIT!  Generated automatically by mk-builtins.sh.

#if ! defined (octave_builtin_defun_decls_h)
#define octave_builtin_defun_decls_h 1

#include "octave-config.h"

#include "ovl.h"

EOF

  pattern='s/^[ \t]*DEF\(CONSTFUN\|UN\)[ \t]*( *\([^ ,]*\).*$/F\2/p; s/^[ \t]*DEFUNX[ \t]*( *"[^"]*" *, *\([^ ,]*\).*$/\1/p'

  dld_pattern='s/^[ \t]*DEFUN_DLD[ \t]*( *\([^ ,]*\).*$/F\1/p; s/^[ \t]*DEFUNX_DLD[ \t]*( *"[^"]*" *, *\([^ ,]*\).*$/\1/p'

  if $defun_dld_are_built_in; then
    pattern="$pattern; $dld_pattern"
  fi

  for arg
  do
    if [ -f $arg ]; then
      file="$arg";
    else
      file="$srcdir/$arg";
    fi

    ## Generate a list of function names to declare.  We could do
    ## this in one step, but this way keeps the sed patterns a
    ## bit smaller.

    names=`$SED -n -e "$pattern" "$file"`

    for name in $names; do
      echo "extern OCTINTERP_API octave_value_list"
      echo "$name (const octave_value_list& = octave_value_list (), int = 0);"
      echo ""
    done
  done

  cat << \EOF

#endif

EOF

elif $make_source; then

  cat << \EOF
// DO NOT EDIT!  Generated automatically by mk-builtins.sh.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "help.h"
#include "ovl.h"
#include "variables.h"
#include "builtin-defun-decls.h"
#include "builtins.h"

#if defined (quad)
#  undef quad
#endif

EOF

  pattern='s/^ *DEFUN *( *\([^ ,]*\) *,.*$/type=fun;fname=F\1;name=\1/p; s/^ *DEFUNX *( *"\([^"]*\)" *, *\([^ ,]*\) *,.*$/type=fun;fname=\2;name=\1/p; s/^ *DEFCONSTFUN *( *\([^ ,]*\) *,.*$/type=fun;fname=F\1;name=\1;const=true/p; s/^ *DEFALIAS *( *\([^ ,]*\) *, *\([^ )]*\) *).*$/type=alias;alias=\1;name=\2/p'

  dld_pattern='s/^ *DEFUN_DLD *( *\([^ ,]*\) *,.*$/type=fun;fname=F\1;name=\1/p; s/^ *DEFUNX_DLD *( *"\([^"]*\)" *, *\([^ ,]*\) *,.*$/type=fun;fname=\2;name=\1/p'

  if $defun_dld_are_built_in; then
    pattern="$pattern; $dld_pattern"
  fi

  for arg
  do
    if [ -f $arg ]; then
      file="$arg";
    else
      file="$srcdir/$arg";
    fi

    fcn=`echo "$arg" | $SED 's,.*/,,; s/\.\(cc\|cpp\|in\.yy\|ll\)$//; s/-/_/g;'`
    echo "static void"
    echo "install_${fcn}_fcns (void)"
    echo "{"
    echo "  std::string file = \"$arg\";"
    echo ""

    ## Generate a list of shell variable assignment expressions
    ## then evaluate them to generate the function calls that
    ## install the built-in functions or function aliases.

    info_list=`$SED -n -e "$pattern" "$file"`

    for info in $info_list; do
      eval "$info"

      const=false
      case $type in
        fun)
          if $const; then
            echo "  install_builtin_function ($fname, \"$name\", file, \"external-doc\", true);"
          else
            echo "  install_builtin_function ($fname, \"$name\", file, \"external-doc\");"
          fi
          unset type fname name const
        ;;
        alias)
          echo "  alias_builtin (\"$alias\", \"$name\");"
          unset type alias name
        ;;
      esac
    done

    echo "}"
    echo ""
  done

  cat << \EOF

void
install_builtins (void)
{
EOF

  for arg
  do
    fcn=`echo "$arg" | $SED 's,.*/,,; s/\.\(cc\|cpp\|in\.yy\|ll\)$//; s/-/_/g;'`
    echo "  install_${fcn}_fcns ();"
  done

  cat << \EOF
}
EOF

fi

exit 0
