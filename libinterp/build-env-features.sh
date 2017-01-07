#! /bin/sh

## Copyright (C) 2013-2016 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Attempt to get traditional sort behavior based on byte values.
LC_ALL=C
export LC_ALL

set -e

AWK=${AWK:-awk}

cat << EOF
// DO NOT EDIT!  Generated automatically from $conffile by Make."

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "build-env.h"
#include "ov.h"

namespace octave
{
  namespace build_env
  {
    octave_scalar_map
    features (void)
    {
      static bool initialized = false;

      static octave_scalar_map m;

      if (! initialized)
        {
          static octave_value ov_true = (true);
          static octave_value ov_false = (false);

EOF

for conffile in "$@"; do
  $AWK \
    '/# *define *(OCTAVE_HAVE|HAVE)_/ {
       sub (/# *define */, "", $0);
       sub (/(OCTAVE_HAVE|HAVE)_/, "", $1)
       printf ("          m.assign (\"%s\", ov_true);\n", $1);
     }
     /\/\* #undef (OCTAVE_HAVE|HAVE)_/ {
       sub (/(OCTAVE_HAVE|HAVE)_/, "", $3);
       printf ("          m.assign (\"%s\", ov_false);\n", $3);
     } {
     }' $conffile
done | sort

cat << EOF

          initialized = true;
        }

      return m;
    }
  };
};
EOF
