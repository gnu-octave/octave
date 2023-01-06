########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{G}, @var{y}] =} planerot (@var{x})
## Compute the Givens rotation matrix for the two-element column vector
## @var{x}.
##
## @tex
## The Givens matrix is a $2\times 2$ orthogonal matrix
## $$
##  G = \left[\matrix{c & s\cr -s'& c\cr}\right]
## $$
## such that
## $$
##  G \left[\matrix{x(1)\cr x(2)}\right] = \left[\matrix{\ast\cr 0}\right]
## $$
## @end tex
## @ifnottex
## The Givens matrix is a 2-by-2 orthogonal matrix
##
## @example
## @group
## @var{G} = [ @var{c} , @var{s}
##      -@var{s}', @var{c}]
## @end group
## @end example
##
## @noindent
## such that
##
## @example
## @var{y} = @var{G} * [@var{x}(1); @var{x}(2)] @equiv{} [*; 0]
## @end example
##
## @end ifnottex
##
## Note: The Givens matrix represents a counterclockwise rotation of a 2-D
## plane and can be used to introduce zeros into a matrix prior to complete
## factorization.
## @seealso{givens, qr}
## @end deftypefn

function [G, y] = planerot (x)

  if (nargin < 1)
    print_usage ();
  elseif (! (isvector (x) && numel (x) == 2))
    error ("planerot: X must be a 2-element vector");
  endif

  G = givens (x(1), x(2));
  y = G * x(:);

endfunction


%!test
%! x = [3 4];
%! [g y] = planerot (x);
%! assert (g, [x(1) x(2); -x(2) x(1)] / sqrt (x(1)^2 + x(2)^2), 2e-8);
%! assert (y(2), 0, 2e-8);

## Test input validation
%!error <Invalid call> planerot ()
%!error <X must be a 2-element vector> planerot (ones (2,2))
%!error <X must be a 2-element vector> planerot ([0 0 0])
