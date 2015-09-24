## Copyright (C) 2014, Jacopo Corno <jacopo.corno@gmail.com>
## OdePkg - A package for solving ordinary differential equations and more
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
## @deftypefn {Command} {[@var{x_out}] =}
## hermite_quartic_interpolation (@var{t}, @var{x}, @var{der}, @var{t_out})
##
## This function file can be called by a ODE solver function in order to
## interpolate the solution at the time @var{t_out} using 4th order
## hermite interpolation.
##
## This function must be called with one output arguments: @var{x_out}
## which contains the evaluation at @var{t_out} of the hermite interpolant.
##
## The first input argument is the vector with two given times.
##
## The second input argument is the vector with the values of the function
## to interpolate at the times specified in @var{t} and at the middle point.
##
## The third input argument is the value of the derivatives of the function
## evaluated at the two extreme points.
##
## @end deftypefn
##
## @seealso{linear_interpolation, quadratic_interpolation,
## hermite_cubic_interpolation, hermite_quintic_interpolation,
## dorpri_interpolation}

function x_out = hermite_quartic_interpolation (t, x, der, t_out)

  # Rescale time on [0,1]
  s = (t_out - t(1)) / (t(2) - t(1));

  # Hermite basis functions
  # H0 = 1   - 11*s.^2 + 18*s.^3 -  8*s.^4;
  # H1 =   s -  4*s.^2 +  5*s.^3 -  2*s.^4;
  # H2 =       16*s.^2 - 32*s.^3 + 16*s.^4;
  # H3 =     -  5*s.^2 + 14*s.^3 -  8*s.^4;
  # H4 =          s.^2 -  3*s.^3 +  2*s.^4;

  x_out = zeros (size (x, 1), length (t_out));
  for ii = 1:size (x, 1)
    x_out(ii,:) = (1   - 11*s.^2 + 18*s.^3 -  8*s.^4)*x(ii,1) ...
                + (  s -  4*s.^2 +  5*s.^3 -  2*s.^4)*(t(2)-t(1))*der(ii,1) ...
                + (      16*s.^2 - 32*s.^3 + 16*s.^4)*x(ii,2) ...
                + (    -  5*s.^2 + 14*s.^3 -  8*s.^4)*x(ii,3) ...
                + (         s.^2 -  3*s.^3 +  2*s.^4)*(t(2)-t(1))*der(ii,2);
  endfor

endfunction

## Local Variables: ***
## mode: octave ***
## End: ***
