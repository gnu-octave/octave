#! /bin/sh

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

: ${AWK=@AWK@}
: ${SED=@SED@}

if test $# -ne 2; then
  echo "usage: check-subst-vars.sh make-vars-file config-vars-file" 2>&1
  exit 1
fi

awk_script="check-subst-vars-$$.awk"

trap "rm -f $awk_script; exit 1" 1 2 15

make_vars="$1"
config_vars="$2"

## Generate awk script to check variable consistency.

cat << EOF > $awk_script
BEGIN {
  status = 0;
EOF

while read var val; do
  val=`echo "$val" | $SED 's/"/\\\\"/g'`
  echo "make_vars[\"$var\"] = \"$val\";" >> $awk_script
done < $make_vars

cat << EOF >> $awk_script
} {
  line = \$0;
  idx = index (line, " ");
  var = substr (line, 1, idx-1);
  val = substr (line, idx+1);
  if (val != make_vars[var])
    {
      printf ("error: mismatch for configuration variable '%s'\n", var);
      printf ("  value set in configuration files: %s\n", val);
      printf ("  value set in Make: %s\n", make_vars[var]);
      status = 1;
    }
} END {
  exit status;
}
EOF

## Execute it.

$AWK -f $awk_script $config_vars 1>&2

rm -f $awk_script
