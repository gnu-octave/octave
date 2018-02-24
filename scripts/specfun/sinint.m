## Copyright (C) 2017 Michele Ginesi
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

## Author: Michele Ginesi <michele.ginesi@gmail.com>

## -*- texinfo -*-
## @deftypefn {} {} sinint (@var{x})
## Compute the sine integral function:
## @tex
## $$
## {\rm Si} (x) = \int_0^x {\sin (t) \over t} dt
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##            x
##           /
## Si (x) =  | sin (t) / t dt
##           /
##          0
## @end group
## @end example
## @end ifnottex
##
## Reference:
##
## @nospell{M. Abramowitz and I.A. Stegun},
## @cite{Handbook of Mathematical Functions}
## 1964.
##
## @seealso{cosint, expint, sin}
##
## @end deftypefn

function [y] = sinint (x)

  if (nargin != 1)
    print_usage ();
  endif

  sz = size (x);
  x = x(:);
  y = zeros (size (x), class (x));
  tol = eps (class (x));

  i_miss = true (length (x), 1);

  ## Trivial values
  y(x == 0) = x(x == 0);  # correctly signed zero
  y(x == Inf) = pi / 2;
  y(x == - Inf) = - pi / 2;

  i_miss = ((i_miss) & (x != 0) & (x != Inf) & (x != - Inf));

  ## For values large in modulus we use the relation with expint

  flag_large = abs (x) > 2;
  xx = x(flag_large & i_miss);
  ii_neg = (real (xx) < 0);
  xx(ii_neg) *= -1;
  ii_conj = ((real (xx) == 0) & (imag (xx) < 0));
  xx(ii_conj) = conj (xx(ii_conj));
  yy = -0.5i * (expint (1i * xx) - expint (-1i * xx)) + pi / 2;
  yy(ii_neg) *= -1;
  yy(ii_conj) = conj (yy(ii_conj));
  y(i_miss & flag_large) = yy;

  ## For values small in modulus we use the series expansion

  i_miss = ((i_miss) & (! flag_large));
  xx = x(i_miss);
  ssum = xx; # First term of the series expansion
  yy = ssum;
  flag_sum = true (nnz (i_miss), 1);
  it = 0;
  maxit = 300;
  while ((any (flag_sum)) && (it < maxit));
    ssum .*= - xx .^ 2 * (2 * it + 1) / ((2 * it + 3) ^ 2 * (2 * it + 2));
    yy(flag_sum) += ssum (flag_sum);
    flag_sum = (abs (ssum) >= tol);
    it++;
  endwhile

  y(i_miss) = yy;

  y = reshape (y, sz);

endfunction


%!test
%! x = 1.1;
%! %y = sym(11)/10;
%! A = sinint (x);
%! %B = double (sinint (y));
%! B = 1.02868521867373;
%! assert (A, B, -5e-15);

%!test
%! %y = [2 3 sym(pi); exp(sym(1)) 5 6];
%! x = [2, 3, pi; exp(1), 5, 6];
%! A = sinint (x);
%! %B = double (sinint (y));
%! B = [1.60541297680269, 1.84865252799947, 1.85193705198247e+00; ...
%!      1.82104026914757, 1.54993124494467, 1.42468755128051e+00];
%! assert (A, B, -5e-15);

%!assert (sinint (0), 0)
%!assert (signbit (sinint (-0)))
%!assert (sinint (inf), pi/2)
%!assert (sinint (-inf), -pi/2)
%!assert (isnan (sinint (nan)))

%!assert (class (sinint (single (1))), "single")

##tests against maple
%!assert (sinint (1), 0.9460830703671830149414, -2*eps)
%!assert (sinint (-1), -0.9460830703671830149414, -2*eps)
%!assert (sinint (pi), 1.851937051982466170361, -2*eps)
%!assert (sinint (-pi), -1.851937051982466170361, -2*eps)
%!assert (sinint (300), 1.5708810882137495193, -2*eps)
%!assert (sinint (1e4), 1.5708915453859619157, -2*eps)
%!assert (sinint (20i), 1.2807826332028294459e7*1i, -2*eps)

%!test
%! x = (0:4)';
%! y_ex = [0
%!         0.946083070367183015
%!         1.60541297680269485
%!         1.84865252799946826
%!         1.75820313894905306];
%! assert (sinint (x), y_ex, -4*eps);

%!test
%! x = -(0:4)';
%! y_ex = - [0
%!           0.946083070367183015
%!           1.60541297680269485
%!           1.84865252799946826
%!           1.75820313894905306];
%! assert (sinint (x), y_ex, -4*eps);

%!test
%! x = 1i * (0:4).';
%! y_ex = [0
%!         1.05725087537572851*I
%!         2.50156743335497564*I
%!         4.97344047585980680*I
%!         9.81732691123303446*I];
%! assert (sinint (x), y_ex, -4*eps);

%!test
%! x = - 1i * (0:4).';
%! y_ex = - [0
%!           1.05725087537572851*I
%!           2.50156743335497564*I
%!           4.97344047585980680*I
%!           9.81732691123303446*I];
%! assert (sinint (x), y_ex, -4*eps);

%!test
%! % maple:
%! % > A := [1+2*I, -2 + 5*I, 100, 10*I, -1e-4 + 1e-6*I, -20 + I];
%! % > for a in A do evalf(Si(a)) end do;
%! x = [1+2i; -2+5i; 100; 10i; -1e-4 + 1e-6*1i; -20-1i];
%! A = [ 1.6782404878293681180 + 2.0396845546022061045*1i
%!      -18.154174221650281533 + 1.6146414539230479060*1i
%!       1.5622254668890562934
%!       1246.1144901994233444*1i
%!      -0.000099999999944461111128 + 0.99999999833338888972e-6*1i
%!      -1.5386156269726011209 - 0.053969388020443786229*1i ];
%! B = sinint (x);
%! assert (A, B, -3*eps)
%! B = sinint (single (x));
%! assert (A, B, -3*eps ("single"))
