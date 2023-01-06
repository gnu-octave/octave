########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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

## -*- texinfo -*-
## @deftypefn {} {@var{y} =} sinewave (@var{m}, @var{n}, @var{d})
## Return an @var{m}-element vector with @var{i}-th element given by
## @code{sin (2 * pi * (@var{i}+@var{d}-1) / @var{n})}.
##
## The default value for @var{d} is 0 and the default value for @var{n} is
## @var{m}.
## @seealso{sinetone}
## @end deftypefn

function y = sinewave (m, n, d = 0)

  if (nargin < 1)
    print_usage ();
  endif

  ## FIXME: No input validation of M, N, or D
  if (nargin < 2)
    n = m;
  endif
  if (nargin < 3)
    d = 0;
  endif

  y = sin (((1 : m) + d - 1) * 2 * pi / n);

endfunction


%!assert (sinewave (1), 0)
%!assert (sinewave (1, 4, 1), 1)
%!assert (sinewave (1, 12, 1), 1/2, 1e-6)
%!assert (sinewave (1, 12, 2), sqrt (3)/2, 1e-6)
%!assert (sinewave (1, 20, 1), (sqrt (5)-1)/4, 1e-6)
%!assert (sinewave (1), sinewave (1, 1,0))
%!assert (sinewave (3, 4), sinewave (3, 4, 0))

%!error <Invalid call> sinewave ()
