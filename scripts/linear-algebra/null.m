########################################################################
##
## Copyright (C) 1994-2020 The Octave Project Developers
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
## @deftypefn  {} {} null (@var{A})
## @deftypefnx {} {} null (@var{A}, @var{tol})
## Return an orthonormal basis of the null space of @var{A}.
##
## The dimension of the null space is taken as the number of singular values of
## @var{A} not greater than @var{tol}.  If the argument @var{tol} is missing,
## it is computed as
##
## @example
## max (size (@var{A})) * max (svd (@var{A})) * eps
## @end example
## @seealso{orth}
## @end deftypefn

function retval = null (A, tol)

  if (nargin < 1)
    print_usage ();
  elseif (nargin == 2 && strcmp (tol, "r"))
    error ("null: option for rational not yet implemented");
  endif

  if (isempty (A))
    if (isa (A, "single"))
      retval = single ([]);
    else
      retval = [];
    endif
  else
    [U, S, V] = svd (A);
    out_cls = class (V);

    s = diag (S);
    if (nargin == 1)
      tol = max (size (A)) * s (1) * eps (out_cls);
    endif
    rank = sum (s > tol);

    cols = columns (A);
    if (rank < cols)
      retval = V(:, rank+1:cols);
      retval(abs (retval) < eps (out_cls)) = 0;
    else
      retval = zeros (cols, 0, out_cls);
    endif
  endif

endfunction


## FIXME: Need some tests for 'single' variables as well

%!test
%! A = 0;
%! assert (null (A), 1);
%! assert (null (single (A)), single (1));

%!test
%! A = 1;
%! assert (null (A), zeros (1,0));

%!test
%! A = [1 0; 0 1];
%! assert (null (A), zeros (2,0));
%! assert (null (single (A)), zeros (2, 0, "single"))

%!test
%! A = [1 0; 1 0];
%! assert (null (A), [0 1]');

%!test
%! A = [1 1; 0 0];
%! assert (null (A), [-1/sqrt(2) 1/sqrt(2)]', eps)
%! assert (null (single (A)), single ([-1/sqrt(2) 1/sqrt(2)]'), eps)

%!test
%! tol = 1e-4;
%! A = [1 0; 0 tol-eps];
%! assert (null (A,tol), [0; 1]);

%!test
%! tol = 1e-4;
%! A = [1 0; 0 tol+eps];
%! assert (null (A,tol), zeros (2,0));

%!assert (null ([]), [])
%!assert (null (single ([])), single ([]))
%!assert (null (uint8 ([])), [])
