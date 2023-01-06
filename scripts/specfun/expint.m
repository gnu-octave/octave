########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} expint (@var{x})
## Compute the exponential integral.
##
## The exponential integral is defined as:
##
## @tex
## $$
## {\rm E_1} (x) = \int_x^\infty {e^{-t} \over t} dt
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##            +oo
##           /
##           | exp (-t)
## E_1 (x) = | -------- dt
##           |    t
##           /
##          x
## @end group
## @end example
##
## @end ifnottex
##
## Note: For compatibility, this function uses the @sc{matlab} definition
## of the exponential integral.  Most other sources refer to this particular
## value as @math{E_1 (x)}, and the exponential integral as
## @tex
## $$
## {\rm Ei} (x) = - \int_{-x}^\infty {e^{-t} \over t} dt.
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##             +oo
##            /
##            | exp (-t)
## Ei (x) = - | -------- dt
##            |    t
##            /
##          -x
## @end group
## @end example
##
## @end ifnottex
## The two definitions are related, for positive real values of @var{x}, by
## @tex
## $
## E_1 (-x) = -{\rm Ei} (x) - i\pi.
## $
## @end tex
## @ifnottex
## @w{@code{E_1 (-x) = -Ei (x) - i*pi}}.
## @end ifnottex
##
## References:
##
## @nospell{M. Abramowitz and I.A. Stegun},
## @cite{Handbook of Mathematical Functions}, 1964.
##
## @nospell{N. Bleistein and R.A. Handelsman},
## @cite{Asymptotic expansions of integrals}, 1986.
##
## @seealso{cosint, sinint, exp}
## @end deftypefn

function E1 = expint (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("expint: X must be numeric");
  endif

  ## Convert to floating point if necessary
  if (isinteger (x))
    x = double (x);
  endif

  orig_sparse = issparse (x);
  orig_sz = size (x);
  x = x(:);  # convert to column vector

  ## Initialize the result
  if (isreal (x) && x >= 0)
    E1 = zeros (size (x), class (x));
  else
    E1 = complex (zeros (size (x), class (x)));
  endif
  tol = eps (class (x));

  ## Divide the input into 3 regions and apply a different algorithm for each.
  ## s = series expansion, cf = continued fraction, a = asymptotic series
  s_idx = (((real (x) + 19.5).^ 2 ./ (20.5^2) + ...
            imag (x).^2 ./ (10^2)) <= 1) ...
          | (real (x) < 0 & abs (imag (x)) <= 1e-8);
  cf_idx = ((((real (x) + 1).^2 ./ (38^2) + ...
              imag (x).^2 ./ (40^2)) <= 1) ...
            & (! s_idx)) & (real (x) <= 35);
  a_idx = (! s_idx) & (! cf_idx);
  x_s  = x(s_idx);
  x_cf = x(cf_idx);
  x_a  = x(a_idx);

  ## Series expansion
  ## Abramowitz, Stegun, "Handbook of Mathematical Functions",
  ## formula 5.1.11, p 229.
  ## FIXME: Why so long?  IEEE double doesn't have this much precision.
  gm = 0.577215664901532860606512090082402431042159335;
  e1_s = -gm - log (x_s);
  res = -x_s;
  ssum = res;
  k = 1;
  todo = true (size (res));
  while (k < 1e3 && any (todo))
    res(todo) .*= (k * (- x_s(todo)) / ((k + 1) ^ 2));
    ssum(todo) += res(todo);
    k += 1;
    todo = (abs (res) > (tol * abs (ssum)));
  endwhile
  e1_s -= ssum;

  ## Continued fraction expansion,
  ## Abramowitz, Stegun, "Handbook of Mathematical Functions",
  ## formula 5.1.22, p 229.
  ## Modified Lentz's algorithm, from "Numerical recipes in Fortran 77" p.165.

  e1_cf = exp (-x_cf) .* __expint__ (x_cf);

  ## Remove spurious imaginary part if needed (__expint__ works automatically
  ## with complex values)

  if (isreal (x_cf) && x_cf >= 0)
    e1_cf = real (e1_cf);
  endif

  ## Asymptotic series, from N. Bleistein and R.A. Handelsman
  ## "Asymptotic expansion of integrals", pages 1-4.
  e1_a = exp (-x_a) ./ x_a;
  ssum = res = ones (size (x_a), class (x_a));
  k = 0;
  todo = true (size (x_a));
  while (k < 1e3 && any (todo))
    res(todo) ./= (- x_a(todo) / (k + 1));
    ssum(todo) += res(todo);
    k += 1;
    todo = abs (x_a) > k;
  endwhile
  e1_a .*= ssum;

  ## Combine results from each region into final output
  E1(s_idx)  = e1_s;
  E1(cf_idx) = e1_cf;
  E1(a_idx)  = e1_a;

  ## Restore shape and sparsity of input
  E1 = reshape (E1, orig_sz);
  if (orig_sparse)
    E1 = sparse (E1);
  endif

endfunction


## The following values were computed with the Octave symbolic package
%!test
%! X = [-50 - 50i  -30 - 50i  -10 - 50i    5 - 50i   15 - 50i   25 - 50i
%!      -50 - 30i  -30 - 30i  -10 - 30i    5 - 30i   15 - 30i   25 - 30i
%!      -50 - 10i  -30 - 10i  -10 - 10i    5 - 10i   15 - 10i   25 - 10i
%!      -50 +  5i  -30 +  5i  -10 +  5i    5 +  5i   15 +  5i   25 +  5i
%!      -50 + 15i  -30 + 15i  -10 + 15i    5 + 15i   15 + 15i   25 + 15i
%!      -50 + 25i  -30 + 25i  -10 + 25i    5 + 25i   15 + 25i   25 + 25i];
%! y_exp = [ -3.61285286166493e+19 + 6.46488018613387e+19i, ...
%!           -4.74939752018180e+10 + 1.78647798300364e+11i, ...
%!            3.78788822381261e+01 + 4.31742823558278e+02i, ...
%!            5.02062497548626e-05 + 1.23967883532795e-04i, ...
%!            3.16785290137650e-09 + 4.88866651583182e-09i, ...
%!            1.66999261039533e-13 + 1.81161508735941e-13i;
%!            3.47121527628275e+19 + 8.33104448629260e+19i, ...
%!            1.54596484273693e+11 + 2.04179357837414e+11i, ...
%!            6.33946547999647e+02 + 3.02965459323125e+02i, ...
%!            2.19834747595065e-04 - 9.25266900230165e-06i, ...
%!            8.49515487435091e-09 - 2.95133588338825e-09i, ...
%!            2.96635342439717e-13 - 1.85401806861382e-13i;
%!            9.65535916388246e+19 + 3.78654062133933e+19i, ...
%!            3.38477774418380e+11 + 8.37063899960569e+10i, ...
%!            1.57615042657685e+03 - 4.33777639047543e+02i, ...
%!            2.36176542789578e-05 - 5.75861972980636e-04i, ...
%!           -6.83624588479039e-09 - 1.47230889442175e-08i, ...
%!           -2.93020801760942e-13 - 4.03912221595793e-13i;
%!           -1.94572937469407e+19 - 1.03494929263031e+20i, ...
%!           -4.22385087573180e+10 - 3.61103191095041e+11i, ...
%!            4.89771220858552e+02 - 2.09175729060712e+03i, ...
%!            7.26650666035639e-04 + 4.71027801635222e-04i, ...
%!            1.02146578536128e-08 + 1.51813977370467e-08i, ...
%!            2.41628751621686e-13 + 4.66309048729523e-13i;
%!            5.42351559144068e+19 + 8.54503231614651e+19i, ...
%!            1.22886461074544e+11 + 3.03555953589323e+11i, ...
%!           -2.13050339387819e+02 + 1.23853666784218e+03i, ...
%!           -3.68087391884738e-04 + 1.94003994408861e-04i, ...
%!           -1.39355838231763e-08 + 6.57189276453356e-10i, ...
%!           -4.55133112151501e-13 - 8.46035902535333e-14i;
%!           -7.75482228205081e+19 - 5.36017490438329e+19i, ...
%!           -1.85284579257329e+11 - 2.08761110392897e+11i, ...
%!           -1.74210199269860e+02 - 8.09467914953486e+02i, ...
%!            9.40470496160143e-05 - 2.44265223110736e-04i, ...
%!            6.64487526601190e-09 - 7.87242868014498e-09i, ...
%!            3.10273337426175e-13 - 2.28030229776792e-13i];
%! assert (expint (X), y_exp, -1e-14);

## Exceptional values (-Inf, Inf, NaN, 0, 0.37250741078)
%!test
%! x = [-Inf; Inf; NaN; 0; -0.3725074107813668];
%! y_exp = [-Inf - i*pi; 0; NaN; Inf; 0 - i*pi];
%! y = expint (x);
%! assert (y, y_exp, 5*eps);

%!test <*53351>
%! assert (expint (32.5 + 1i),
%!         1.181108930758065e-16 - 1.966348533426658e-16i, -4*eps);
%! assert (expint (44 + 1i),
%!         9.018757389858152e-22 - 1.475771020004195e-21i, -4*eps);

%!test <*47738>
%! assert (expint (10i), 0.0454564330044554 + 0.0875512674239774i, -5*eps);

## Test preservation or conversion of the class
%!assert (class (expint (single (1))), "single")
%!assert (class (expint (int8 (1))), "double")
%!assert (class (expint (int16 (1))), "double")
%!assert (class (expint (int32 (1))), "double")
%!assert (class (expint (int64 (1))), "double")
%!assert (class (expint (uint8 (1))), "double")
%!assert (class (expint (uint16 (1))), "double")
%!assert (class (expint (uint32 (1))), "double")
%!assert (class (expint (uint64 (1))), "double")
%!assert (issparse (expint (sparse (1))))

## Test on the correct Image set
%!assert (isreal (expint (linspace (0, 100))))
%!assert (! isreal (expint (-1)))

## Test input validation
%!error <Invalid call> expint ()
%!error <X must be numeric> expint ("1")
