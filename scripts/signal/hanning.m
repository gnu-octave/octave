## Copyright (C) 1995-2012 Andreas Weingessel
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
## @deftypefn {Function File} {} hanning (@var{m})
## Return the filter coefficients of a Hanning window of length @var{m}.
##
## For a definition of this window type, see e.g., A. V. Oppenheim &
## R. W. Schafer, @cite{Discrete-Time Signal Processing}.
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description: Coefficients of the Hanning window

function c = hanning (m)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isscalar (m) && (m == fix (m)) && (m > 0)))
    error ("hanning: M has to be an integer > 0");
  endif

  if (m == 1)
    c = 1;
  else
    m = m - 1;
    c = 0.5 - 0.5 * cos (2 * pi * (0 : m)' / m);
  endif

endfunction

%!assert (hanning (1), 1);
%!assert (hanning (2), zeros(2,1));
%!assert (hanning (16), fliplr (hanning (16)));
%!assert (hanning (15), fliplr (hanning (15)));
%!test
%! N = 15;
%! A = hanning (N);
%! assert (A (ceil (N/2)), 1);

%!error hanning ();
%!error hanning (0.5);
%!error hanning (-1);
%!error hanning (ones(1,4));
