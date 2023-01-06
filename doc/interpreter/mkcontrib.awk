########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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

{ x[NR] = $0; } END {
  print "@multitable @columnfractions .33 .33 .33";
  rem = NR % 3;
  n = NR - rem;
  for (i = 1; i <= n; i += 3)
    printf ("@item %s @tab %s @tab %s\n", x[i], x[i+1], x[i+2]);
  if (rem == 1)
    printf ("@item %s\n", x[NR]);
  else if (rem == 2)
    printf ("@item %s @tab %s\n", x[NR-1], x[NR]);
  print "@end multitable";
}
