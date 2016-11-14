## Copyright (C) 1995-2016 Kurt Hornik
## Copyright (C) 2016 Lachlan Andrew
## Copyright (C) 2014 Mike Giles
## Copyright (C) 2012 Rik Wehbring
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {} poissinv (@var{x}, @var{lambda})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the Poisson distribution with parameter @var{lambda}.
## @end deftypefn

## Author: Lachlan <lachlanbis@gmail.com>
## based on code by
##      KH <Kurt.Hornik@wu-wien.ac.at>
##      Mike Giles <mike.giles@maths.ox.ac.uk>
## Description: Quantile function of the Poisson distribution

function inv = poissinv (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("poissinv: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("poissinv: X and LAMBDA must not be complex");
  endif

  if (isa (x, "single") || isa (lambda, "single"))
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (x < 0) | (x > 1) | isnan (x) | !(lambda > 0);
  inv(k) = NaN;

  k = (x == 1) & (lambda > 0);
  inv(k) = Inf;

  k = (x > 0) & (x < 1) & (lambda > 0);
  if (any (k(:)))
    limit = 20;                         # After 'limit' iterations, use approx
    if (isscalar (lambda))
      cdf = [(cumsum (poisspdf (0:limit-1,lambda))), 2];
      y = x(:);                         # force to column
      r = bsxfun (@le, y(k), cdf);
      [~, inv(k)] = max (r, [], 2);     # find first instance of x <= cdf
      inv(k) -= 1;
    else
      kk = find (k);
      cdf = exp (-lambda(kk));
      for i = 1:limit
        m = find (cdf < x(kk));
        if (isempty (m))
          break;
        else
          inv(kk(m)) += 1;
          cdf(m) += poisspdf (i, lambda(kk(m)));
        endif
      endfor
    endif

    ## Use Mike Giles's magic when inv isn't < limit
    k &= (inv == limit);
    if (any (k(:)))
      if (isscalar (lambda))
        lam = repmat (lambda, size (x));
      else
        lam = lambda;
      endif
      inv(k) = analytic_approx (x(k), lam(k));
    endif
  endif

endfunction


## The following is based on Mike Giles's CUDA implementation,
## [http://people.maths.ox.ac.uk/gilesm/codes/poissinv/poissinv_cuda.h]
## which is copyright by the University of Oxford
## and is provided under the terms of the GNU GPLv3 license:
## http://www.gnu.org/licenses/gpl.html

function inv = analytic_approx (x, lambda)

  s = norminv (x, 0, 1) ./ sqrt (lambda);
  k = (s > -0.6833501) & (s < 1.777993);
  ## use polynomial approximations in central region
  if (any (k))
    lam = lambda(k);
    if (isscalar (s))
      sk = s;
    else
      sk = s(k);
    endif

    ## polynomial approximation to f^{-1}(s) - 1
    rm =  2.82298751e-07;
    rm = -2.58136133e-06 + rm.*sk;
    rm =  1.02118025e-05 + rm.*sk;
    rm = -2.37996199e-05 + rm.*sk;
    rm =  4.05347462e-05 + rm.*sk;
    rm = -6.63730967e-05 + rm.*sk;
    rm =  0.000124762566 + rm.*sk;
    rm = -0.000256970731 + rm.*sk;
    rm =  0.000558953132 + rm.*sk;
    rm =  -0.00133129194 + rm.*sk;
    rm =   0.00370367937 + rm.*sk;
    rm =   -0.0138888706 + rm.*sk;
    rm =     0.166666667 + rm.*sk;
    rm =         sk + sk.*(rm.*sk);

    ## polynomial approximation to correction c0(r)

    t  =   1.86386867e-05;
    t  =  -0.000207319499 + t.*rm;
    t  =     0.0009689451 + t.*rm;
    t  =   -0.00247340054 + t.*rm;
    t  =    0.00379952985 + t.*rm;
    t  =   -0.00386717047 + t.*rm;
    t  =    0.00346960934 + t.*rm;
    t  =   -0.00414125511 + t.*rm;
    t  =    0.00586752093 + t.*rm;
    t  =   -0.00838583787 + t.*rm;
    t  =     0.0132793933 + t.*rm;
    t  =     -0.027775536 + t.*rm;
    t  =      0.333333333 + t.*rm;

    ##  O(1/lam) correction

    y  =   -0.00014585224;
    y  =    0.00146121529 + y.*rm;
    y  =   -0.00610328845 + y.*rm;
    y  =     0.0138117964 + y.*rm;
    y  =    -0.0186988746 + y.*rm;
    y  =     0.0168155118 + y.*rm;
    y  =     -0.013394797 + y.*rm;
    y  =     0.0135698573 + y.*rm;
    y  =    -0.0155377333 + y.*rm;
    y  =     0.0174065334 + y.*rm;
    y  =    -0.0198011178 + y.*rm;
    y ./= lam;

    inv(k) = floor (lam + (y+t)+lam.*rm);
  endif

  k = ! k & (s > -sqrt (2));
  if (any (k))
    ## Newton iteration
    r = 1 + s(k);
    r2 = r + 1;
    while (any (abs (r - r2) > 1e-5))
      t = log (r);
      r2 = r;
      s2 = sqrt (2 * ((1-r) + r.*t));
      s2(r<1) *= -1;
      r = r2 - (s2 - s(k)) .* s2 ./ t;
      if (r < 0.1 * r2)
        r = 0.1 * r2;
      endif
    endwhile
    t = log (r);
    y = lambda(k) .* r + log (sqrt (2*r.*((1-r) + r.*t)) ./ abs (r-1)) ./ t;
    inv(k) = floor (y - 0.0218 ./ (y + 0.065 * lambda(k)));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (poissinv (x, ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (poissinv (x, 1), [NaN 0 1 Inf NaN])
%!assert (poissinv (x, [1 0 NaN 1 1]), [NaN NaN NaN Inf NaN])
%!assert (poissinv ([x(1:2) NaN x(4:5)], 1), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (poissinv ([x, NaN], 1), [NaN 0 1 Inf NaN NaN])
%!assert (poissinv (single ([x, NaN]), 1), single ([NaN 0 1 Inf NaN NaN]))
%!assert (poissinv ([x, NaN], single (1)), single ([NaN 0 1 Inf NaN NaN]))

## Test input validation
%!error poissinv ()
%!error poissinv (1)
%!error poissinv (1,2,3)
%!error poissinv (ones (3), ones (2))
%!error poissinv (ones (2), ones (3))
%!error poissinv (i, 2)
%!error poissinv (2, i)

