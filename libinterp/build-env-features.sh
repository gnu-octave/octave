#! /bin/sh

## Attempt to get traditional sort behavior based on byte values.
LC_ALL=C
export LC_ALL

set -e

AWK=${AWK:-awk}

conffile=$1

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

$AWK \
  '/#define (OCTAVE_|)HAVE_/ {
     sub (/(OCTAVE_|)HAVE_/, "", $2);
     printf ("          m.assign (\"%s\", ov_true);\n", $2);
   }
   /\/\* #undef (OCTAVE_|)HAVE_/ {
     sub (/(OCTAVE_|)HAVE_/, "", $3);
     printf ("          m.assign (\"%s\", ov_false);\n", $3);
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
