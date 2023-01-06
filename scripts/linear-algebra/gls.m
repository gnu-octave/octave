########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{beta}, @var{v}, @var{r}] =} gls (@var{y}, @var{x}, @var{o})
## Generalized least squares (GLS) model.
##
## Perform a generalized least squares estimation for the multivariate model
## @tex
## $@var{y} = @var{x}\,@var{b} + @var{e}$
## @end tex
## @ifnottex
## @w{@math{@var{y} = @var{x}*@var{B} + @var{E}}}
## @end ifnottex
## where
## @tex
## $@var{y}$ is a $t \times p$ matrix, $@var{x}$ is a $t \times k$ matrix,
## $@var{b}$ is a $k \times p$ matrix and $@var{e}$ is a $t \times p$ matrix.
## @end tex
## @ifnottex
## @var{y} is a @math{t}-by-@math{p} matrix, @var{x} is a
## @math{t}-by-@math{k} matrix, @var{b} is a @math{k}-by-@math{p} matrix
## and @var{e} is a @math{t}-by-@math{p} matrix.
## @end ifnottex
##
## @noindent
## Each row of @var{y} is a @math{p}-variate observation in which each column
## represents a variable.  Likewise, the rows of @var{x} represent
## @math{k}-variate observations or possibly designed values.  Furthermore,
## the collection of observations @var{x} must be of adequate rank, @math{k},
## otherwise @var{b} cannot be uniquely estimated.
##
## The observation errors, @var{e}, are assumed to originate from an
## underlying @math{p}-variate distribution with zero mean but possibly
## heteroscedastic observations.  That is, in general,
## @tex
## $\bar{@var{e}} = 0$ and cov(vec(@var{e})) = $s^2@var{o}$
## @end tex
## @ifnottex
## @code{@math{mean (@var{e}) = 0}} and
## @code{@math{cov (vec (@var{e})) = (@math{s}^2)*@var{o}}}
## @end ifnottex
## in which @math{s} is a scalar and @var{o} is a
## @tex
## @math{t \, p \times t \, p}
## @end tex
## @ifnottex
## @math{t*p}-by-@math{t*p}
## @end ifnottex
## matrix.
##
## The return values @var{beta}, @var{v}, and @var{r} are
## defined as follows.
##
## @table @var
## @item beta
## The GLS estimator for matrix @var{b}.
##
## @item v
## The GLS estimator for scalar @math{s^2}.
##
## @item r
## The matrix of GLS residuals, @math{@var{r} = @var{y} - @var{x}*@var{beta}}.
## @end table
## @seealso{ols}
## @end deftypefn

function [beta, v, r] = gls (y, x, o)

  if (nargin != 3)
    print_usage ();
  endif

  if (! (isnumeric (x) && isnumeric (y) && isnumeric (o)))
    error ("gls: X, Y, and O must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2 || ndims (o) != 2)
    error ("gls: X, Y and O must be 2-D matrices or vectors");
  endif

  [rx, cx] = size (x);
  [ry, cy] = size (y);
  [ro, co] = size (o);
  if (rx != ry)
    error ("gls: number of rows of X and Y must be equal");
  endif
  if (! issquare (o) || ro != ry*cy)
    error ("gls: matrix O must be square matrix with rows = rows (Y) * cols (Y)");
  endif

  if (isinteger (x))
    x = double (x);
  endif
  if (isinteger (y))
    y = double (y);
  endif
  if (isinteger (o))
    o = double (o);
  endif

  ## Start of algorithm
  o ^= -1/2;
  z = kron (eye (cy), x);
  z = o * z;
  y1 = o * reshape (y, ry*cy, 1);
  u = z' * z;
  r = rank (u);

  if (r == cx*cy)
    b = inv (u) * z' * y1;
  else
    b = pinv (z) * y1;
  endif

  beta = reshape (b, cx, cy);

  if (isargout (2) || isargout (3))
    r = y - x * beta;
    if (isargout (2))
      v = (reshape (r, ry*cy, 1))' * (o^2) * reshape (r, ry*cy, 1) / (rx*cy - r);
    endif
  endif

endfunction


%!test
%! x = [1:5]';
%! y = 3*x + 2;
%! x = [x, ones(5,1)];
%! o = diag (ones (5,1));
%! assert (gls (y,x,o), [3; 2], 50*eps);

## Test input validation
%!error <Invalid call> gls ()
%!error <Invalid call> gls (1)
%!error <Invalid call> gls (1, 2)
%!error gls ([true, true], [1, 2], ones (2))
%!error gls ([1, 2], [true, true], ones (2))
%!error gls ([1, 2], [1, 2], true (2))
%!error gls (ones (2,2,2), ones (2,2), ones (4,4))
%!error gls (ones (2,2), ones (2,2,2), ones (4,4))
%!error gls (ones (2,2), ones (2,2), ones (4,4,4))
%!error gls (ones (1,2), ones (2,2), ones (2,2))
%!error gls (ones (2,2), ones (2,2), ones (2,2))
