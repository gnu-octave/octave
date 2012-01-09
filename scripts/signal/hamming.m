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
## @deftypefn {Function File} {} hamming (@var{m})
## Return the filter coefficients of a Hamming window of length @var{m}.
##
## For a definition of the Hamming window, see e.g., A. V. Oppenheim &
## R. W. Schafer, @cite{Discrete-Time Signal Processing}.
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description: Coefficients of the Hamming window

function c = hamming (m)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isscalar (m) && (m == fix (m)) && (m > 0)))
    error ("hamming: M has to be an integer > 0");
  endif

  if (m == 1)
    c = 1;
  else
    m = m - 1;
    c = 0.54 - 0.46 * cos (2 * pi * (0:m)' / m);
  endif

endfunction

%!assert (hamming (1), 1);
%!assert (hamming (2), (0.54 - 0.46)*ones(2,1));
%!assert (hamming (16), fliplr (hamming (16)));
%!assert (hamming (15), fliplr (hamming (15)));
%!test
%! N = 15;
%! A = hamming (N);
%! assert (A (ceil (N/2)), 1);

%!error hamming ();
%!error hamming (0.5);
%!error hamming (-1);
%!error hamming (ones(1,4));
