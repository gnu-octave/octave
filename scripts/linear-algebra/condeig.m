########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} condeig (@var{a})
## @deftypefnx {} {[@var{v}, @var{lambda}, @var{c}] =} condeig (@var{a})
## Compute condition numbers of a matrix with respect to eigenvalues.
##
## The condition numbers are the reciprocals of the cosines of the angles
## between the left and right eigenvectors; Large values indicate that the
## matrix has multiple distinct eigenvalues.
##
## The input @var{a} must be a square numeric matrix.
##
## The outputs are:
##
## @itemize @bullet
## @item
## @var{c} is a vector of condition numbers for the eigenvalues of
## @var{a}.
##
## @item
## @var{v} is the matrix of right eigenvectors of @var{a}.  The result is
## equivalent to calling @code{[@var{v}, @var{lambda}] = eig (@var{a})}.
##
## @item
## @var{lambda} is the diagonal matrix of eigenvalues of @var{a}.  The
## result is equivalent to calling
## @code{[@var{v}, @var{lambda}] = eig (@var{a})}.
## @end itemize
##
## Example
##
## @example
## @group
## a = [1, 2; 3, 4];
## c = condeig (a)
##   @result{} c =
##        1.0150
##        1.0150
## @end group
## @end example
## @seealso{eig, cond, balance}
## @end deftypefn

function [v, lambda, c] = condeig (a)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (a) && issquare (a)))
    error ("condeig: A must be a square numeric matrix");
  endif

  if (issparse (a) && nargout <= 1)
    ## Try to use svds to calculate the condition number as it will typically
    ## be much faster than calling eig as only the smallest and largest
    ## eigenvalue are calculated.

    ## FIXME: This calculates one condition number for the entire matrix.
    ## In the full case, separate condition numbers are calculated for every
    ## eigenvalue.
    try
      s0 = svds (a, 1, 0);    # min eigenvalue
      v = svds (a, 1) / s0;   # max eigenvalue
    catch
      ## Caught an error as there is a singular value exactly at zero!!
      v = Inf;
    end_try_catch
    return;
  endif

  ## Right eigenvectors
  [v, lambda] = eig (a);

  if (isempty (a))
    c = [];
  else
    ## Corresponding left eigenvectors
    ## Use 2-argument form to suppress possible singular matrix warning.
    [vl, ~] = inv (v);
    vl = vl';
    ## Normalize vectors
    vl ./= repmat (sqrt (sum (abs (vl .^ 2))), rows (vl), 1);

    ## Condition numbers
    ## Definition: cos (angle) = (norm (v1) * norm (v2)) / dot (v1, v2)
    ## Eigenvectors have been normalized so 'norm (v1) * norm (v2)' = 1
    c = abs (1 ./ dot (vl, v)');
  endif

  if (nargout <= 1)
    v = c;
  endif

endfunction


%!test
%! a = [1, 2; 3, 4];
%! c = condeig (a);
%! expected_c = [1.0150; 1.0150];
%! assert (c, expected_c, 0.001);

%!test
%! a = [1, 3; 5, 8];
%! [v, lambda, c] = condeig (a);
%! [expected_v, expected_lambda] = eig (a);
%! expected_c = [1.0182; 1.0182];
%! assert (v, expected_v, 0.001);
%! assert (lambda, expected_lambda, 0.001);
%! assert (c, expected_c, 0.001);

## Test empty input
%!assert (condeig ([]), [])

## Test input validation
%!error <Invalid call> condeig ()
%!error <A must be a square numeric matrix> condeig ({1})
%!error <A must be a square numeric matrix> condeig (ones (3,2))
%!error <A must be a square numeric matrix> condeig (ones (2,2,2))
