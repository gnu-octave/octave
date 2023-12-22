########################################################################
##
## Copyright (C) 1996-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{r} =} corr (@var{x})
## @deftypefnx {} {@var{r} =} corr (@var{x}, @var{y})
## Compute matrix of correlation coefficients.
##
## If each row of @var{x} and @var{y} is an observation and each column is
## a variable, then the @w{(@var{i}, @var{j})-th} entry of
## @code{corr (@var{x}, @var{y})} is the correlation between the
## @var{i}-th variable in @var{x} and the @var{j}-th variable in @var{y}.
## @var{x} and @var{y} must have the same number of rows (observations).
## @tex
## $$
## {\rm corr}(x,y) = {{\rm cov}(x,y) \over {\rm std}(x) \, {\rm std}(y)}
## $$
## @end tex
## @ifnottex
##
## @example
## corr (@var{x},@var{y}) = cov (@var{x},@var{y}) / (std (@var{x}) * std (@var{y}))
## @end example
##
## @end ifnottex
## If called with one argument, compute @code{corr (@var{x}, @var{x})},
## the correlation between the columns of @var{x}.
## @seealso{cov}
## @end deftypefn

function r = corr (x, y = [])

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("corr: X must be a numeric vector or matrix");
  endif

  ## No check for division by zero error, which happens only when
  ## there is a constant vector and should be rare.

  if (nargin == 2)
    ## Adjust for Octave 9.1.0 compatibility behavior change in two-input cov.
    ## cov now treats cov(x,y) as cov(x(:),y(:)), returning a 2x2 covariance
    ## of the two univariate distributions x and y.  corr will now pass [x,y]
    ## as cov([x,y]), which for m x n inputs will return 2n x 2n outputs, with
    ## the off-diagonal matrix quarters containing what was previously
    ## returned by cov(x,y).

    ## FIXME: Returning a larger than needed array and discarding 3/4 of the
    ##        information is non-ideal.  Consider implementing a more
    ##        efficient cov here as a subfunction to corr.  At that point,
    ##        input validation will need to be coded back into this function.

    if (! (isnumeric (y) || islogical (y)))
      error ("corr: Y must be a numeric vector or matrix");
    endif

    ## Check for equal number of rows before concatenating inputs for cov.
    ## This will also catch mixed orientation 2-D vectors which cov allows but
    ## corr should not.
    if (rows (x) != rows (y))
      error ("corr: X and Y must have the same number of rows");
    endif

    rowx = isrow (x);
    rowy = isrow (y);

    if ((! rowy && ndims (x) > 2) || (! rowx && ndims (y) > 2))
      ## For compatibility 3D is permitted only if other input is row vector
      ## which results in NaNs.
        error (["corr: X and Y must be two dimensional unless the other ", ...
                "input is a scalar or row vector"]);
    endif

    ## Special handling for row vectors.  std=0 along dim 1 and division by 0
    ## will return NaN, but cov will process along vector dimension.  Keep
    ## special handling after call to cov so it handles all other input
    ## validation and avoid duplicating validation overhead for all other
    ## cases.
    ncx = columns (x);
    ncy = columns (y);
    if (rowx || rowy)
      if (isa (x, "single") || isa (y, "single"))
        r = NaN (ncx, ncy, "single");
      else
        r = NaN (ncx, ncy);
      endif
      return;
    endif

    c = cov ([x, y]);  # Also performs input validation of x and y.

    c = c(1:ncx, ncx+1:end);
    s = std (x, [], 1)' * std (y, [], 1);
    r = c ./ s;

  else
    c = cov (x);    # Also performs input validation of x.

    if (isrow (x))  # Special handling for row vector.
      nc = columns (x);
      if (isa (x, "single"))
        r = NaN (nc, "single");
      else
        r = NaN (nc);
      endif
      return;
    endif

    s = sqrt (diag (c));
    r = c ./ (s * s');
  endif

endfunction


%!test
%! x = rand (10);
%! cc1 = corr (x);
%! cc2 = corr (x, x);
%! assert (size (cc1) == [10, 10] && size (cc2) == [10, 10]);
%! assert (cc1, cc2, sqrt (eps));

%!test
%! x = [1:3]';
%! y = [3:-1:1]';
%! assert (corr (x, y), -1, 5*eps);
%! assert (corr (x, flipud (y)), 1, 5*eps);
%! assert (corr ([x, y]), [1 -1; -1 1], 5*eps);

%!test
%! x = single ([1:3]');
%! y = single ([3:-1:1]');
%! assert (corr (x, y), single (-1), 5*eps);
%! assert (corr (x, flipud (y)), single (1), 5*eps);
%! assert (corr ([x, y]), single ([1 -1; -1 1]), 5*eps);

## Special case: scalar
%!assert (corr (5), NaN)
%!assert (corr (single (5)), single (NaN))

## Special case: constant vectors
%!assert (corr ([5; 5; 5], [1; 2; 3]), NaN)
%!assert (corr ([1; 2; 3], [5;5;5]), NaN)

%!test <*64555>
%! x = [1 2; 3 4; 5 6];
%! y = [1 2 3]';
%! assert (corr (x, y), [1; 1]);
%! assert (corr (y, x), [1, 1]);
%! assert (corr (x, [y, y]), [1 1; 1 1])

%!test <*64395>
%! x = [1, 2, 3];
%! assert (corr (x), NaN (3));
%! assert (corr (x'), 1, eps);
%! assert (corr (x, x), NaN (3));
%! assert (corr (x', x'), 1, eps);

%!test <*64395>
%! x = single ([1, 2, 3]);
%! assert (corr (x), single (NaN (3)));
%! assert (corr (x'), 1, single (eps));
%! assert (corr (x, x), single (NaN (3)));
%! assert (corr (x', x'), 1, single (eps));

%!assert <*64555> (corr (1, rand (1, 10)), NaN (1, 10));
%!assert <*64555> (corr (rand (1, 10), 1), NaN (10, 1));
%!assert <*64555> (corr (rand (1, 10), rand (1, 10)), NaN (10, 10));
%!assert <*64555> (corr (rand (1, 5), rand (1, 10)), NaN (5, 10));
%!assert <*64555> (corr (5, rand (1, 10, 5)), NaN (1, 10));
%!assert <*64555> (corr (rand (1, 5, 5), rand (1, 10)), NaN (5, 10));
%!assert <*64555> (corr (rand (1, 5, 5, 99), rand (1, 10)), NaN (5, 10));

## Test input validation
%!error <Invalid call> corr ()
%!error corr (1, 2, 3)
%!error <X must be a> corr ("foo")
%!error <X must be a> corr ({123})
%!error <X must be a> corr (struct())
%!error <Y must be a> corr (1, "foo")
%!error <Y must be a> corr (1, {123})
%!error <Y must be a> corr (1, struct())
%!error <Y must be a> corr ([1; 2], ["A"; "B"])
%!error <X and Y must have the same number of rows> corr (ones (2,2), ones (3,2))
%!error <X and Y must have the same number of rows> corr ([1,2,3], [1,2,3]')
%!error <X and Y must have the same number of rows> corr ([1,2,3]', [1,2,3])
%!error <X and Y must have the same number of rows> corr (ones (2,2), ones (1,2,2))
%!error <X and Y must be two dimensional unless> corr (ones (2,2), ones (2,2,2))
%!error corr (ones (2,2,2)) # Single input validation handled by corr
