#! /bin/sh

########################################################################
##
## Copyright (C) 1997-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

SED=${SED:-sed}

cat << \EOF
// DO NOT EDIT!  Generated automatically by mk-ops.sh.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "ov-typeinfo.h"

OCTAVE_BEGIN_NAMESPACE(octave)

extern void install_base_type_conversions (octave::type_info&);

EOF

for file in "$@"; do
  f=`echo $file | $SED 's,^\./,,; s%^libinterp/operators/op-%%; s%\.cc%%; s%-%_%g'`
  echo "extern void install_${f}_ops (octave::type_info&);"
done

cat << \EOF

void
install_ops (octave::type_info& ti)
{
  install_base_type_conversions (ti);

EOF

for file in "$@"; do
  f=`echo $file | $SED 's,^\./,,; s%^libinterp/operators/op-%%; s%\.cc%%; s%-%_%g'`
  echo "  install_${f}_ops (ti);"
done

cat << \EOF
}

OCTAVE_END_NAMESPACE(octave)

EOF

exit 0
