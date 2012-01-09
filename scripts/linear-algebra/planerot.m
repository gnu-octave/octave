## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn {Function File} {[@var{g}, @var{y}] =} planerot (@var{x})
## Given a two-element column vector, returns the
## @tex
## $2 \times 2$ orthogonal matrix
## @end tex
## @ifnottex
## 2 by 2 orthogonal matrix
## @end ifnottex
## @var{G} such that
## @code{@var{y} = @var{g} * @var{x}} and @code{@var{y}(2) = 0}.
## @seealso{givens}
## @end deftypefn

function [G, y] = planerot (x)
  G = givens (x(1), x(2));
  y = G * x(:);
endfunction

%!test
%! x = [3 4];
%! [g y] = planerot(x);
%! assert(g - [x(1) x(2); -x(2) x(1)] / sqrt(x(1)^2 + x(2)^2), zeros(2), 2e-8);
%! assert(y(2), 0, 2e-8);

%!error planerot([0]);
%!error planerot([0 0 0]);
%!error planerot();

