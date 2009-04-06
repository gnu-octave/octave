## Copyright (C) 2009  Tony Richardson
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
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
## @deftypefn {Function File} {} polyscale (@var{f}, @var{a})
## Return the coefficients of the scaled polynomial whose
## coefficients are given by vector @var{f}.  If @var{f} is the
## vector representing the polynomial f(x), then 
## @var{g} = polyscale (@var{f}, @var{a}) is the vector
## representing g(x) = f(x*a).
##
## Here is a simple example that will plot both the original and
## translated (shifted) polynomials.  f is a third order polynomial.
## g is a polynomial obtained after scaling f by 2 (compression):
##
## f = [1/5 4/5 -7/5 -2];
##
## g = polyscale(f, 2);
##
## x = linspace(-4,4,100);
##
## plot(x,polyval(f,x),x,polyval(g,x));
##
## axis([-4 4 -3 5]);
##
## grid("on");
## @seealso{polytranslate}
## @end deftypefn

## Author: Tony Richardson <richardson@evansville.edu)
## Created: April 2009
## adapted by: jh

function g = polyscale (f, a)

   if (nargin != 2)
      print_usage ();
   endif

   if (! isvector (f))
      error ("polyscale: first argument must be a vector.");
   endif

   if (! isscalar (a))
      error ("polyscale: second argument must be a scalar.");
   endif

   lf = length(f);

   # Ensure that f is a row vector
   if (rows (f) > 1)
      f = reshape (a, 1, lf);
   endif

   g = f .* (a .^ linspace (lf-1, 0, lf));

endfunction
