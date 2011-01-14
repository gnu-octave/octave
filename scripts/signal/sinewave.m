## Copyright (C) 1995-2011 Andreas Weingessel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} sinewave (@var{m}, @var{n}, @var{d})
## Return an @var{m}-element vector with @var{i}-th element given by
## @code{sin (2 * pi * (@var{i}+@var{d}-1) / @var{n})}.
##
## The default value for @var{d} is 0 and the default value for @var{n}
## is @var{m}.
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description: Compute a sine wave

function x = sinewave (m, n, d)

  if (nargin > 0 && nargin < 4)
    if (nargin < 3)
      d = 0;
    endif
    if (nargin < 2)
      n = m;
    endif
    x = sin (((1 : m) + d - 1) * 2 * pi / n);
  else
    print_usage ();
  endif

endfunction
