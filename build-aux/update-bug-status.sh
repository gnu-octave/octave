#! /bin/sh

########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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

bug_numbers=$(for file in "$@"; do
  sed -n "s/.*<\([0-9][0-9][0-9][0-9]*\)>.*/\1/p" "$file"
done | sort -u)

fixed_bug_numbers=$(for num in $bug_numbers; do
  status=$(wget -q -O - https://octave.org/testfailure/?$num | tr -d '\n' | sed -n 's/.*>Status:<\/span><\/span>&nbsp;<\/td> *<td valign="middle" width="35%">\([^<]*\)<.*/\1/p');
  if [ "$status" = "Fixed" ]; then echo "$num"; fi
done)

if [ -z "$fixed_bug_numbers" ]; then
  echo "no change in bug status"
  exit 0;
fi

fixed_bug_pattern=`echo $fixed_bug_numbers | sed 's/ /\\\\|/g; s/^/<\\\\(/; s/$/\\\\)>/'`

for file in "$@"; do
   sed -i "s/$fixed_bug_pattern/<*\1>/" "$file"
done
