## Copyright (C) 2006 Sylvain Pelissier
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

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>

## -*- texinfo -*-
## @deftypefn {Function File} {} expint (@var{x})
## Compute the exponential integral,
## @tex
## $$
##  E_1 (x) = \int_x^\infty {e^{-t} \over t} dt.
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##               infinity
##              /
## expint (x) = | exp (-t)/t dt
##              /
##             x
## @end group
## @end example
##
## @end ifnottex
## @end deftypefn

function y = expint (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = expint_E1 (x);

endfunction

## -*- texinfo -*-
## @deftypefn {Function File} {@var{y} =} expint_E1 (@var{x})
## Compute the exponential integral,
## @verbatim
##                    infinity
##                   /
##       expint(x) = | exp(-t)/t dt
##                   /
##                  x
## @end verbatim
## @end deftypefn

function y = expint_E1 (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = x;

  idx = (imag (x) > 0 & imag (x) != 0);
  y(idx) = -expint_Ei (-y(idx)) - i.*pi;

  idx = (imag (x) < 0 & imag (x) != 0);
  y(idx) = -expint_Ei (-y(idx)) + i.*pi;

  idx = (real (x) >= 0 & imag (x) == 0);
  y(idx) = -expint_Ei (-y(idx));

  idx = (real (x) < 0 & imag (x) == 0);
  y(idx) = -expint_Ei (-y(idx)) - i.*pi;

endfunction

## -*- texinfo -*-
## @deftypefn {Function File} {@var{y} =} expint_Ei (@var{x})
## Compute the exponential integral,
## @verbatim
##                      infinity
##                     /
##    expint_Ei(x) = - | exp(t)/t dt
##                     /
##                     -x
## @end verbatim
## @end deftypefn

function y = expint_Ei (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = zeros (size (x));
  F = @(x) exp (-x)./x;
  s = prod (size (x));

  for t = 1:s;
    if (x(t) < 0 && imag (x(t)) == 0)
      y(t) = -quad (F, -x(t), Inf);
    else
      if (abs (x(t)) > 2 && imag (x(t)) == 0)
        y(t) = expint_Ei (2) - quad (F, -x(t), -2);
      else
        if (abs (x(t)) >= 10)
          if (imag (x(t)) <= 0)
            a1 = 4.03640;
            a2 = 1.15198;
            b1 = 5.03637;
            b2 = 4.19160;
            y(t) = -(x(t).^2 - a1.*x(t) + a2) ...
                   ./ ((x(t).^2 - b1.*x(t) + b2) .* (-x(t)) .* exp (-x(t))) ...
                   - i.*pi;
          else
            y(t) = conj (expint_Ei (conj (x(t))));
          endif;
        ## Serie Expansion
        else
          for k = 1:100;
            y(t) = y(t) + x(t).^k ./ (k.*factorial (k));
          endfor
          y(t) = 0.577215664901532860606512090082402431 + log (x(t)) + y(t);
        endif
      endif
    endif
  endfor
endfunction

%% Test against A&S Table 5.1
%!test
%! x = [5:5:50]'/100;
%! gamma = 0.5772156649;
%! y_exp = [0.9876375971;
%!          0.9755453033;
%!          0.9637156702;
%!          0.9521414833;
%!          0.9408157528;
%!          0.9297317075;
%!          0.9188827858;
%!          0.9082626297;
%!          0.8978650778;
%!          0.8876841584 ];
%! y = (expint (x) + log(x) + gamma) ./ x;
%! assert (y, y_exp, 1e-9);
%!test
%! x = [50:5:95]'/100;
%! y_exp = [0.559773595;
%!          0.503364081;
%!          0.454379503;
%!          0.411516976;
%!          0.373768843;
%!          0.340340813;
%!          0.310596579;
%!          0.284019269;
%!          0.260183939;
%!          0.238737524 ];
%! y = expint (x);
%! assert (y, y_exp, 1e-9);
%!test
%! x = [100:5:145]'/100;
%! y_exp = [0.219383934;
%!          0.201872813;
%!          0.185990905;
%!          0.171555354;
%!          0.158408437;
%!          0.146413373;
%!          0.135450958;
%!          0.125416844;
%!          0.116219313;
%!          0.107777440 ];
%! y = expint (x);
%! assert (y, y_exp, 1e-9);
%!test
%! x = [150:5:200]'/100;
%! y_exp = [0.100019582;
%!          0.092882108;
%!          0.086308334;
%!          0.080247627;
%!          0.074654644;
%!          0.069488685;
%!          0.064713129;
%!          0.060294967;
%!          0.056204378;
%!          0.052414380;
%!          0.048900511 ];
%! y = expint (x);
%! assert (y, y_exp, 1e-9);

%% Test input validation
%!error expint ()
%!error expint (1,2)
