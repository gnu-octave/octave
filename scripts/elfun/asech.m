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
## @deftypefn {} {@var{y} =} asech (@var{x})
## Compute the inverse hyperbolic secant of each element of @var{x}.
## @seealso{sech}
## @end deftypefn

function y = asech (x)

  if (nargin < 1)
    print_usage ();
  endif

  y = acosh (1 ./ x);

endfunction


%!testif ; ! ismac ()
%! v = [0, pi*i];
%! x = [1, -1];
%! assert (asech (x), v, sqrt (eps));

%!test <*52627>
%! ## Same test code as above, but intended only for test statistics on Mac.
%! ## Mac trig/hyperbolic functions have huge tolerances.
%! if (! ismac ()), return; endif
%! v = [0, pi*i];
%! x = [1, -1];
%! assert (asech (x), v, sqrt (eps));

%!error <Invalid call> asech ()
