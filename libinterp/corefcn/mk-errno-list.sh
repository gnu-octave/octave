#! /bin/sh

########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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

if [ $# -ne 2 ]; then
  echo "usage: get-errno-list [--perl PERL|--python PYTHON|--sed SED]" 1>&2
  exit 1
fi

if [ $1 = "--perl" ]; then
  PERL="$2"
  $PERL -e 'foreach $key (sort (keys (%!))) {
    $x .= "#if defined ($key)\n    { \"$key\", $key },\n#endif\n";
  }
  while (<>) {
    s/^ *\@SYSDEP_ERRNO_LIST\@/$x/;
    s/\@NO_EDIT_WARNING\@/DO NOT EDIT!  Generated automatically from oct-errno.in.cc by mk-errno-list.sh/;
    print;
  }'

elif [ $1 = "--python" ]; then
  PYTHON="$2"
  $PYTHON -c '
from errno import errorcode
from sys import stdin, stdout

t = "#if defined (%s)\n    { \"%s\", %s },\n#endif\n"
errstr = ""
for v in sorted (errorcode.values ()):
    errstr += t % tuple (3 * [v])

noedit = "DO NOT EDIT!  Generated automatically from oct-errno.in.cc by mk-errno-list.sh"

repls = ("@SYSDEP_ERRNO_LIST@", errstr), ("@NO_EDIT_WARNING@", noedit)

for l in stdin:
  stdout.write (reduce (lambda a, kv: a.replace (*kv), repls, l))
'
elif [ $1 = "--sed" ]; then
  SED="$2"
  $SED -e '/@SYSDEP_ERRNO_LIST@/D' \
       -e 's/@NO_EDIT_WARNING@/DO NOT EDIT!  Generated automatically from oct-errno.in.cc by mk-errno-list.sh/'
fi

exit $?
