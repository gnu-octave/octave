########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} sech (@var{x})
## Compute the hyperbolic secant of each element of @var{x}.
## @seealso{asech}
## @end deftypefn

function y = sech (x)

if (nargin < 1)
    print_usage ();
  endif

  y = 1 ./ cosh (x);

endfunction


%!test
%! x = [0, pi*i];
%! v = [1, -1];
%! assert (sech (x), v, sqrt (eps));

%!error <Invalid call> sech ()
