########################################################################
##
## Copyright (C) 2015-2023 The Octave Project Developers
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
## @deftypefn {} {@var{u_interp} =} odemergeopts (@var{order}, @var{z}, @var{t}, @var{k_vals})
## Undocumented internal function.
## @end deftypefn

function u_interp = runge_kutta_interpolate (order, z, u, t, k_vals)

  switch (order)

    case 1
      ## Unused
      u_interp = interp1 (z, u.', t, "linear");

    case 2
      ## ode23s with Rosenbrock scheme:
      der = k_vals(:,1);
      u_interp = quadratic_interpolation (z, u, der, t);

    case 3
      ## ode23 with Bogacki-Shampine scheme:
      u_interp = hermite_cubic_interpolation (z, u, k_vals, t);

    case 5
      ## ode45 with Dormand-Prince scheme:
      u_interp = hermite_quartic_interpolation (z, u, k_vals, t);

    otherwise
      ## This should never happen
      warning (["Invalid/unimplemented interpolation order: ", ...
                "using linear interpolation instead"]);
      u_interp = interp1 (z, u.', t, "linear");

  endswitch

endfunction

## The function below can be used in an ODE solver to interpolate the solution
## at the time t_out using 2nd order Hermite interpolation.
function x_out = quadratic_interpolation (t, x, der, t_out)

  ## coefficients of the parabola
  a = -(x(:,1) - x(:,2) - der(:).*(t(1)-t(2))) / (t(1) - t(2))^2;
  b = der(:) - 2*t(1).*a;
  c = x(:,1) - a*t(1)^2 - b*t(1);

  ## evaluate in t_out
  x_out = a*t_out.^2 + b*t_out + c;

endfunction

## The function below can be used in an ODE solver to interpolate the
## solution at the time t_out using 3rd order Hermite interpolation.
function x_out = hermite_cubic_interpolation (t, x, der, t_out)

  dt = (t(2) - t(1));
  s = (t_out - t(1)) / dt;
  x_out = ((1 + 2*s) .* (1-s).^2) .* x(:,1) + ...
          (s .* (1-s).^2 * dt   ) .* der(:,1) + ...
          ((3-2*s) .* s.^2      ) .* x(:,end) + ...
          ((s-1) .* s.^2   * dt ) .* der(:,end);

endfunction

## The function below can be used in an ODE solver to interpolate the
## solution at the time t_out using 4th order Hermite interpolation.
function x_out = hermite_quartic_interpolation (t, x, der, t_out)

  persistent coefs_u_half = ...
    [6025192743/30085553152; 0; 51252292925/65400821598;
     -2691868925/45128329728; 187940372067/1594534317056;
     -1776094331/19743644256; 11237099/235043384];

  ## 4th order approximation of y in t+dt/2 as proposed by Shampine in
  ## Lawrence, Shampine, "Some Practical Runge-Kutta Formulas", 1986.
  dt = t(2) - t(1);
  u_half = x(:,1) + (1/2) * dt * (der(:,1:7) * coefs_u_half);

  ## Rescale time on [0,1]
  s = (t_out - t(1)) / dt;

  ## Hermite basis functions
  ## H0 = 1   - 11*s.^2 + 18*s.^3 -  8*s.^4;
  ## H1 =   s -  4*s.^2 +  5*s.^3 -  2*s.^4;
  ## H2 =       16*s.^2 - 32*s.^3 + 16*s.^4;
  ## H3 =     -  5*s.^2 + 14*s.^3 -  8*s.^4;
  ## H4 =          s.^2 -  3*s.^3 +  2*s.^4;

  x_out = (1   - 11*s.^2 + 18*s.^3 -  8*s.^4) .* x(:,1) + ...
          (  s -  4*s.^2 +  5*s.^3 -  2*s.^4) .* (dt * der(:,1)) + ...
          (      16*s.^2 - 32*s.^3 + 16*s.^4) .* u_half + ...
          (    -  5*s.^2 + 14*s.^3 -  8*s.^4) .* x(:,2) + ...
          (         s.^2 -  3*s.^3 +  2*s.^4) .* (dt * der(:,end));

endfunction
