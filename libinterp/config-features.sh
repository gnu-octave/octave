#! /bin/sh

## Attempt to get traditional sort behavior based on byte values.
LC_ALL="C"
export LC_ALL

set -e
AWK=${AWK:-awk}

conffile=$1

cat << EOF
// DO NOT EDIT!  Generated automatically from $conffile by Make."

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-conf-features.h"
#include "ov.h"

namespace octave
{
  namespace config
  {
    octave_scalar_map
    features (void)
    {
      static bool initialized = false;

      static octave_scalar_map m;

      if (! initialized)
        {
EOF

$AWK \
  '/#define (HAVE|ENABLE)_/ {
     sub (/HAVE_/, "", $2);
     printf ("          m.assign (\"%s\", octave_value (true));\n", $2);
   }
   /\/\* #undef (HAVE|ENABLE)_/ {
     sub (/HAVE_/, "", $3);
     printf ("          m.assign (\"%s\", octave_value (false));\n", $3);
   } {
   }' $conffile | sort

cat << EOF

          initialized = true;
        }

      return m;
    }
  };
};
EOF
