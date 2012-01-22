## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1995-2012 Kurt Hornik
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
## @deftypefn {Function File} {} tcdf (@var{x}, @var{n})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of the t (Student) distribution with
## @var{n} degrees of freedom, i.e., PROB (t(@var{n}) @leq{} @var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the t distribution

function cdf = tcdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("tcdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("tcdf: X and N must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = !isinf (x) & (n > 0);
  if (isscalar (n))
    cdf(k) = betainc (1 ./ (1 + x(k) .^ 2 / n), n/2, 1/2) / 2;
  else
    cdf(k) = betainc (1 ./ (1 + x(k) .^ 2 ./ n(k)), n(k)/2, 1/2) / 2;
  endif
  k &= (x > 0);
  if (any (k(:)))
    cdf(k) = 1 - cdf(k);
  endif

  k = isnan (x) | !(n > 0);
  cdf(k) = NaN;

  k = (x == Inf) & (n > 0);
  cdf(k) = 1;

endfunction


%!shared x,y
%! x = [-Inf 0 1 Inf];
%! y = [0 1/2 3/4 1];
%!assert(tcdf (x, ones(1,4)), y, eps);
%!assert(tcdf (x, 1), y, eps);
%!assert(tcdf (x, [0 1 NaN 1]), [NaN 1/2 NaN 1], eps);
%!assert(tcdf ([x(1:2) NaN x(4)], 1), [y(1:2) NaN y(4)], eps);

%% Test class of input preserved
%!assert(tcdf ([x, NaN], 1), [y, NaN], eps);
%!assert(tcdf (single([x, NaN]), 1), single([y, NaN]), eps("single"));
%!assert(tcdf ([x, NaN], single(1)), single([y, NaN]), eps("single"));

%% Test input validation
%!error tcdf ()
%!error tcdf (1)
%!error tcdf (1,2,3)
%!error tcdf (ones(3),ones(2))
%!error tcdf (ones(2),ones(3))
%!error tcdf (i, 2)
%!error tcdf (2, i)

