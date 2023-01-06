########################################################################
##
## Copyright (C) 2003-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tf} =} isdefinite (@var{A})
## @deftypefnx {} {@var{tf} =} isdefinite (@var{A}, @var{tol})
## Return true if @var{A} is symmetric positive definite matrix within the
## tolerance specified by @var{tol}.
##
## If @var{tol} is omitted, use a tolerance of
## @code{100 * eps * norm (@var{A}, "fro")}.
##
## Background: A positive definite matrix has eigenvalues which are all
## greater than zero.  A positive semi-definite matrix has eigenvalues which
## are all greater than or equal to zero.  The matrix @var{A} is very likely to
## be positive semi-definite if the following two conditions hold for a
## suitably small tolerance @var{tol}.
##
## @example
## @group
## isdefinite (@var{A}) @result{} 0
## isdefinite (@var{A} + 5*@var{tol}, @var{tol}) @result{} 1
## @end group
## @end example
## @seealso{issymmetric, ishermitian}
## @end deftypefn

function tf = isdefinite (A, tol)

  if (nargin < 1)
    print_usage ();
  endif

  ## Validate inputs
  tf = false;
  if (! isnumeric (A))
    return;
  endif

  if (! isfloat (A))
    A = double (A);
  endif

  if (nargin == 1)
    tol = 100 * eps (class (A)) * norm (A, "fro");
  elseif (! (isnumeric (tol) && isscalar (tol) && tol >= 0))
    error ("isdefinite: TOL must be a scalar >= 0");
  endif

  if (! ishermitian (A, tol))
    return;
  endif

  e = tol * eye (rows (A));
  [~, p] = chol (A - e);
  if (p == 0)
    tf = true;
  endif

endfunction


%!test
%! A = [-1, 0; 0, -1];
%! assert (isdefinite (A), false);

%!test
%! A = [1, 0; 0, 1];
%! assert (isdefinite (A), true);

%!test
%! A = [2, -1,  0; -1, 2, -1; 0, -1, 2];
%! assert (isdefinite (A), true);

## Test for positive semi-definite matrix
%!test
%! A = [1, 0; 0, 0];
%! assert (isdefinite (A), false);
%! tol = 100*eps;
%! assert (isdefinite (A+5*tol, tol), true);

%!assert (! isdefinite (magic (3)))

%!error <Invalid call> isdefinite ()
%!error <TOL must be a scalar .= 0> isdefinite (1, {1})
%!error <TOL must be a scalar .= 0> isdefinite (1, [1 1])
%!error <TOL must be a scalar .= 0> isdefinite (1, -1)
