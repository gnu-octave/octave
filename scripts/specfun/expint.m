## Copyright (C) 2006, 2013 Sylvain Pelissier
## Copyright (C) 2017 Michele Ginesi
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

## Authors: Sylvain Pelissier <sylvain.pelissier@gmail.com>
##          Michele Ginesi <michele.ginesi@gmail.com>

## -*- texinfo -*-
## @deftypefn {} {} expint (@var{x})
## Compute the exponential integral:
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
## Note: For compatibility, this functions uses the @sc{matlab} definition
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
## @end deftypefn

function E1 = expint (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("expint: X must be numeric");
  elseif (isinteger (x))
    x = double (x);
  endif

  sparse_x = issparse (x);
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
  s_idx = (((real (x) + 19.5).^2 ./ (20.5^2) + imag (x).^2 ./ (10^2)) <= 1) ...
          | (real (x) < 0 & abs (imag (x)) <= 1e-8);
  cf_idx = ((((real (x) + 1).^2 ./ (38^2) + imag (x).^2 ./ (40^2)) <= 1) ...
            & (! s_idx)) & (real (x) <= 35);
  a_idx = ! s_idx & ! cf_idx;
  x_s  = x(s_idx);
  x_cf = x(cf_idx);
  x_a  = x(a_idx);

  ## FIXME: The performance of these routines need improvement.
  ## There are numerous temporary variables created, some of which could
  ## probably be eliminated. 

  ## Series expansion
  ## Abramowitz, Stegun, "Handbook of Mathematical Functions",
  ## formula 5.1.11, p 229.
  ## FIXME: Why so long?  IEEE double doesn't have this much precision.
  gm = 0.577215664901532860606512090082402431042159335;
  e1_s = -gm - log (x_s);
  res = -x_s;
  ssum = res;
  k = 1;
  fflag = true (size (res));
  while (k < 1e3 && any (fflag))
    res_tmp = res(fflag);
    x_s_tmp = x_s(fflag);
    ssum_tmp = ssum(fflag);
    res_tmp .*= (k * -x_s_tmp/((k + 1)^2));
    ssum_tmp += res_tmp;
    k += 1;
    res(fflag) = res_tmp;
    ssum(fflag) = ssum_tmp;
    x_s(fflag) = x_s_tmp;
    fflag = abs (res) > tol*abs (ssum);
  endwhile
  e1_s -= ssum;

  ## Continued fraction,
  ## Abramowitz, Stegun, "Handbook of Mathematical Functions",
  ## formula 5.1.22, p 229.
  ## modified Lentz's algorithm, from "Numerical recipes in Fortran 77" p.165.
  f_new = 2^-100 * ones (size (x_cf), class (x_cf));
  C_new = f_new;
  D_new = zeros (size (x_cf), class (x_cf));
  k = 0;
  fflag = true (size (x_cf));
  Delta = C_old = D_old = f_old = ones (size (x_cf), class (x_cf));
  while (k < 1e3 && any (fflag))
    x_cf_tmp = x_cf(fflag);
    C_new_tmp = C_new(fflag);
    D_new_tmp = D_new(fflag);
    f_old = f_new(fflag);
    C_old = C_new_tmp;
    D_old = D_new_tmp;
    b = x_cf_tmp*(mod (k,2) == 0) + (mod (k,2) == 1);
    a = exp (-x_cf_tmp)*(k == 0) + ceil (k/2)*(k >= 1);
    D_new_tmp = b + a.*D_old;
    D_new_tmp = D_new_tmp.*(D_new_tmp != 0) + 2^-100*(D_new_tmp == 0);
    C_new_tmp = b + a./C_old;
    C_new_tmp = C_new_tmp.*(C_new_tmp != 0) + 2^-100*(C_new_tmp == 0);
    D_new_tmp = 1 ./ D_new_tmp;
    Delta(fflag) = C_new_tmp.*D_new_tmp;
    x_cf(fflag) = x_cf_tmp;
    f_new(fflag) = f_old.*Delta(fflag);
    C_new(fflag) = C_new_tmp;
    D_new(fflag) = D_new_tmp;
    fflag = abs (Delta-1) > tol;
    k += 1;
  endwhile

  e1_cf = f_new;
  ## Asymptotic series, from Fikioris, Tastsoglou, Bakas,
  ## "Selected Asymptotic Methods with Application to Magnetics and Antennas"
  ## formula A.10 p 161.
  e1_a = exp (-x_a) ./ x_a;
  oldres = ssum = res = ones (size (x_a), class (x_a));
  k = 0;
  fflag = true (size (x_a));
  while (k < 1e3 && any (fflag))
    res_tmp = res(fflag);
    oldres_tmp = res_tmp;
    x_a_tmp = x_a(fflag);
    res_tmp ./= (-x_a_tmp / (k+1));
    ssum(fflag) += res_tmp;
    k += 1;
    res(fflag) = res_tmp;
    oldres(fflag) = oldres_tmp;
    fflag = abs (oldres) > abs (res);
  endwhile
  e1_a .*= ssum;

  ## Combine results from each region into final output
  E1(s_idx)  = e1_s;
  E1(cf_idx) = e1_cf;
  E1(a_idx)  = e1_a;
  E1 = reshape (E1, orig_sz);
  if (sparse_x)
    E1 = sparse (E1);  # if input was sparse format, output should be too.
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
%! assert (expint (X), y_exp, -1e-12);

## Exceptional values (-Inf, Inf, NaN, 0, 0.37250741078)
%!test
%! x = [-Inf; Inf; NaN; 0; -0.3725074107813668];
%! y_exp = [-Inf - i*pi; 0; NaN; Inf; 0 - i*pi];
%! y = expint (x);
%! assert (y, y_exp, 5*eps);

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

## Test input validation
%!error expint ()
%!error expint (1,2)
%!error <X must be numeric> expint ("1")

