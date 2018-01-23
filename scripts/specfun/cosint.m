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
## @deftypefn {} {} cosint (@var{x})
## Compute the cosine integral function:
## @tex
## $$
## {\rm Ci} (x) = - \int_x^\infty {{\cos (t)} \over t} dt
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##             +oo
##            /
## Ci (x) = - | (cos (t)) / t dt
##            /
##           x
##
## @end group
## @end example
## @end ifnottex
## An equivalent definition is
## @tex
## $$
## {\rm Ci} (x) = \gamma + \log (x) + \int_0^x {{\cos (t) - 1} \over t} dt
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##
##                              x
##                             /
##                             |   cos (t) - 1
## Ci (x) = gamma + log (x) +  | ---------------  dt
##                             |         t
##                             /
##                            0
## @end group
## @end example
## @end ifnottex
## Reference:
##
## @nospell{M. Abramowitz and I.A. Stegun},
## @cite{Handbook of Mathematical Functions}
## 1964.
##
## @seealso{expint, cos, sinint}
##
## @end deftypefn

function [y] = cosint (x)

  if (nargin != 1)
    print_usage ();
  endif

  sz = size (x);
  x = x(:);
  y = zeros (size (x));

  i_miss = true (length (x), 1);

  ## special values
  y(x == 0) = - Inf;
  y(x == Inf) = 0;
  y(x == - Inf) = 1i * pi;

  i_miss = ((i_miss) & (x != 0) & (x != Inf) & (x != - Inf));

  ## For values large in modulus and not in (-oo,0), we use the relation
  ## with expint

  flag_large = (abs (x) > 2 & ((abs (imag (x)) > 1e-15) | real (x) > 0));
  xx = x(flag_large);

  ## Abramowitz, relation 5.2.20
  ii_sw = (real (xx) <= 0 & imag (xx) <= 0);
  xx(ii_sw) = conj (xx(ii_sw));
  ii_nw = (real (xx) < 0);
  xx(ii_nw) *= -1;
  yy = -0.5 * (expint (1i * xx) + expint (-1i * xx));
  yy(ii_nw) += 1i * pi;
  yy(ii_sw) = conj (yy(ii_sw));
  y(i_miss & flag_large) = yy;

  ## For values small in modulus, use the series expansion (also near (-oo, 0])
  i_miss = ((i_miss) & (!flag_large));
  xx = x(i_miss);
  ssum = - xx .^ 2 / 4; # First term of the series expansion
  gma = 0.57721566490153286060651209008; # Euler gamma constant
  yy = gma + log (xx) + ssum;
  flag_sum = true (nnz (i_miss), 1);
  it = 1;
  maxit = 300;
  while ((any (flag_sum)) && (it < maxit));
    ssum .*= - xx .^ 2 * (2 * it) / ((2 * it + 2) ^ 2 * (2 * it + 1));
    yy(flag_sum) += ssum (flag_sum);
    flag_sum = (abs (ssum) >= eps);
    it++;
  endwhile

  y(i_miss) = yy;

  y = reshape (y, sz);

endfunction


%!assert (cosint (1.1), 0.38487337742465081550, 2 * eps);

%!test
%! x = [2, 3, pi; exp(1), 5, 6];
%! A = cosint (x);
%! B = [0.422980828774864996, 0.119629786008000328, 0.0736679120464254860; ...
%!      0.213958001340379779, -0.190029749656643879, -0.0680572438932471262];
%! assert (A, B, -5e-15);

%!assert (cosint (0), - Inf)
%!assert (cosint (inf), 0)
%!assert (cosint (-inf), 1i * pi)

##tests against maple
%!assert (cosint (1), 0.337403922900968135, -2*eps)
%!assert (cosint (-1), 0.337403922900968135 + 3.14159265358979324*I, -2*eps)
%!assert (cosint (pi), 0.0736679120464254860, -2*eps)
%!assert (cosint (-pi), 0.0736679120464254860 + 3.14159265358979324*I, -2*eps)
%!assert (cosint (300), -0.00333219991859211178, -2*eps)
%!assert (cosint (1e4), -0.0000305519167244852127, -2*eps)
%!assert (cosint (20i), 1.28078263320282944e7 + 1.57079632679489662*I, -2*eps)

%!test
%! x = (0:4).';
%! y_ex = [-Inf
%!         0.337403922900968135
%!         0.422980828774864996
%!         0.119629786008000328
%!         -0.140981697886930412];
%! assert (cosint(x), y_ex, -3e-15);

%!test
%! x = -(0:4) ';
%! y_ex = [-Inf
%!         0.337403922900968135 + 3.14159265358979324*I
%!         0.422980828774864996 + 3.14159265358979324*I
%!         0.119629786008000328 + 3.14159265358979324*I
%!         -0.140981697886930412 + 3.14159265358979324*I];
%! assert (cosint (x), y_ex, -4*eps);

%!test
%! x = 1i * (0:4).';
%! y_ex = [-Inf
%!         0.837866940980208241 + 1.57079632679489662*I
%!         2.45266692264691452 + 1.57079632679489662*I
%!         4.96039209476560976 + 1.57079632679489662*I
%!         9.81354755882318556 + 1.57079632679489662*I];
%! assert (cosint (x), y_ex, -4*eps);

%!test
%! x = - 1i * (0:4).';
%! y_ex = [- Inf
%!         0.837866940980208241 - 1.57079632679489662*I
%!         2.45266692264691452 - 1.57079632679489662*I
%!         4.96039209476560976 - 1.57079632679489662*I
%!         9.81354755882318556 - 1.57079632679489662*I];
%! assert (cosint (x), y_ex, -4*eps);

%!test
%! x = [1+2i; -2+5i; 2-5i; 100; 10i; -1e-4 + 1e-6*1i; -20-1i];
%! A = [ 2.03029639329172164 - 0.151907155175856884*I
%!      1.61538963829107749 + 19.7257540553382650*I
%!      1.61538963829107749 + 16.5841614017484717*I
%!      -0.00514882514261049214
%!      1246.11448604245441 + 1.57079632679489662*I
%!      -8.63307471207423322 + 3.13159298695312800*I
%!      0.0698222284673061493 - 3.11847446254772946*I ];
%! B = cosint (x);
%! assert (A, B, -6e-16)
