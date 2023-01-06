########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} cosint (@var{x})
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
## @end group
## @end example
##
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
##                              x
##                             /
##                             |  cos (t) - 1
## Ci (x) = gamma + log (x) +  | -------------  dt
##                             |        t
##                             /
##                            0
## @end group
## @end example
##
## @end ifnottex
## Reference:
##
## @nospell{M. Abramowitz and I.A. Stegun},
## @cite{Handbook of Mathematical Functions}, 1964.
##
## @seealso{sinint, expint, cos}
##
## @end deftypefn

function y = cosint (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("cosint: X must be numeric");
  endif

  ## Convert to floating point if necessary
  if (isinteger (x))
    x = double (x);
  endif

  ## Convert to column vector
  orig_sz = size (x);
  if (iscomplex (x))
    ## Work around reshape which narrows to real (bug #52953)
    x = complex (real (x)(:), imag (x)(:));
  else
    x = x(:);
  endif

  ## Initialize the result
  y = zeros (size (x), class (x));
  tol = eps (class (x));

  todo = true (size (x));

  ## Special values
  y(x == Inf) = 0;
  y((x == -Inf) & ! signbit (imag (x))) = 1i * pi;
  y((x == -Inf) &  signbit (imag (x))) = -1i * pi;

  todo(isinf (x)) = false;

  ## For values large in modulus, but not in the range (-oo,0), we use the
  ## relation with expint.

  flag_large = (abs (x) > 2);
  xx = x(flag_large);

  ## Abramowitz, relation 5.2.20
  ii_sw = (real (xx) <= 0 & imag (xx) < 0);
  xx(ii_sw) = conj (xx(ii_sw));
  ii_nw = (real (xx) < 0);
  xx(ii_nw) *= -1;
  yy = -0.5 * (expint (1i * xx) + expint (-1i * xx));
  yy(ii_nw) += 1i * pi;
  yy(ii_sw) = conj (yy(ii_sw));
  y(todo & flag_large) = yy;

  todo(flag_large) = false;

  ## For values small in modulus, use the series expansion (also near (-oo, 0])
  if (iscomplex (x))
    ## indexing can lose imag part: if it was -0, we could end up on the
    ## wrong right side of the branch cut along the negative real axis.
    xx = complex (real (x)(todo), imag (x)(todo));
  else
    xx = x(todo);
  endif
  ssum = - xx .^ 2 / 4; # First term of the series expansion
  ## FIXME: This is way more precision than a double value can hold.
  gma = 0.57721566490153286060651209008; # Euler gamma constant
  yy = gma + log (complex (xx)) + ssum;  # log (complex (Z)) handles signed zero
  flag_sum = true (nnz (todo), 1);
  it = 0;
  maxit = 300;
  while (any (flag_sum) && (++it < maxit))
    ssum .*= - xx .^ 2 * (2 * it) / ((2 * it + 2) ^ 2 * (2 * it + 1));
    yy(flag_sum) += ssum (flag_sum);
    flag_sum = (abs (ssum) >= tol);
  endwhile
  y(todo) = yy;

  ## Clean up values which are purely real
  flag_neg_zero_imag = (real (x) < 0) & (imag (x) == 0) & signbit (imag (x));
  y(flag_neg_zero_imag) = complex (real (y(flag_neg_zero_imag)), -pi);

  ## Restore original shape
  y = reshape (y, orig_sz);

endfunction


%!assert (cosint (1.1), 0.38487337742465081550, 2 * eps)

%!test
%! x = [2, 3, pi; exp(1), 5, 6];
%! A = cosint (x);
%! B = [0.422980828774864996, 0.119629786008000328, 0.0736679120464254860; ...
%!      0.213958001340379779, -0.190029749656643879, -0.0680572438932471262];
%! assert (A, B, -5e-15);

%!assert (cosint (0), - Inf)
%!assert (cosint (-0), -inf + 1i*pi)
%!assert (cosint (complex (-0, 0)), -inf + 1i*pi)
%!assert (cosint (complex (-0, -0)), -inf - 1i*pi)
%!assert (cosint (inf), 0)
%!assert (cosint (-inf), 1i * pi)
%!assert (cosint (complex (-inf, -0)), -1i * pi)
%!assert (isnan (cosint (nan)))

%!assert (class (cosint (single (1))), "single")

## tests against maple
%!assert (cosint (1), 0.337403922900968135, -2*eps)
%!assert (cosint (-1), 0.337403922900968135 + 3.14159265358979324*I, -2*eps)
%!assert (cosint (pi), 0.0736679120464254860, -4e-15)
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
%! assert (cosint (x), y_ex, -3e-15);

%!test
%! x = -(1:4).';
%! y_ex = [0.337403922900968135 + pi*1i
%!         0.422980828774864996 + pi*1i
%!         0.119629786008000328 + pi*1i
%!         -0.140981697886930412 + pi*1i];
%! assert (cosint (x), y_ex, -4*eps);

%!test
%! x = complex (-(1:4).', 0);
%! y_ex = [0.337403922900968135 + pi*1i
%!         0.422980828774864996 + pi*1i
%!         0.119629786008000328 + pi*1i
%!         -0.140981697886930412 + pi*1i];
%! assert (cosint (x), y_ex, -4*eps);

%!test
%! x = complex (-(1:4).', -0);
%! y_ex = [0.337403922900968135 - pi*1i
%!         0.422980828774864996 - pi*1i
%!         0.119629786008000328 - pi*1i
%!         -0.140981697886930412 - pi*1i];
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
%! x = -1i * (1:4).';
%! y_ex = [0.837866940980208241 - 1.57079632679489662*I
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
%! assert (A, B, -3*eps);
%! B = cosint (single (x));
%! assert (A, B, -3*eps ("single"));

## Fails along negative real axis
%!test
%! x = [-25; -100; -1000];
%! yex = [-0.0068485971797025909189 + pi*1i
%!        -0.0051488251426104921444 + pi*1i
%!        0.000826315511090682282 + pi*1i];
%! y = cosint (x);
%! assert (y, yex, -5*eps);

## FIXME: Need a test for bug #52953
%#!test <*52953>

## Test input validation
%!error <Invalid call> cosint ()
%!error <X must be numeric> cosint ("1")
