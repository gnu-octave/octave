## Copyright (C) 2009  Tony Richardson, Jaroslav Hajek
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} polyaffine (@var{f}, @var{mu})
## Return the coefficients of the polynomial whose coefficients are given by
## vector @var{f} after an affine tranformation. If @var{f} is the vector
## representing the polynomial f(x), then @var{g} = polytrans (@var{f},
## @var{a}) is the vector representing g(x) = f((x-@var{mu}(1))/@var{mu}(2)).
## 
## Here is a simple example that will plot both the original and
## transformed polynomials.  f is a third order polynomial.
## g is a polynomial obtained after shifting f one unit to the right
## and stretching the x axis by 1.2:
##
## f = [1/5 4/5 -7/5 -2];
##
## g = polyaffine(f, [1, 1.2]);
##
## x = linspace(-4,4,100);
##
## plot(x,polyval(f,x),x,polyval(g,x));
##
## axis([-4 4 -3 5]);
##
## grid("on");
## @seealso{polyval}
## @end deftypefn


function g = polyaffine (f, mu)

   if (nargin != 2)
      print_usage ();
   endif

   if (! isvector (f))
      error ("polyaffine: first argument must be a vector.");
   endif

   if (! isvector (mu) || length (mu) != 2)
      error ("polyaffine: second argument must be a two-element vector.");
   endif

   lf = length (f);

   ## Ensure that f is a row vector
   if (rows (f) > 1)
      f = f.';
   endif

   ## Translate.
   if (mu(1) != 0)
     w = (-mu(1)) .^ (0:lf-1);
     ii = lf:-1:1;
     g = f(ii) * (toeplitz (w) .* pascal (lf, -1));
     g = g(ii);
   else
     g = f;
   endif

   ## Scale.
   if (mu(2) != 1)
     g = g ./ (mu(2) .^ (lf-1:-1:0));
   endif

endfunction
