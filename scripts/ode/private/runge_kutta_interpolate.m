## Copyright (C) 2015-2016 Carlo de Falco
## Copyright (C) 2015-2016 Jacopo Corno <jacopo.corno@gmail.com>
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

function u_interp = runge_kutta_interpolate (order, z, u, t, k_vals, dt, func, args)

  switch (order)

    ## FIXME: Can interpolations for orders 1-4 simply be deleted? 2015-10-14.

    case 1
      u_interp = interp1 (z, u', t, 'linear');

    case 2
      if (! isempty (k_vals))
        der = k_vals(:,1);
      else
        der = feval (func, z(1) , u(:,1), args);
      endif
      u_interp = quadratic_interpolation (z, u, der, t);
    case 3
      u_interp = ...
      hermite_cubic_interpolation (z, u, k_vals, t);
    #{
    case 4
      ## if ode45 is used without local extrapolation this function
      ## doesn't require a new function evaluation.
      u_interp = dorpri_interpolation ([z(i-1) z(i)],
                                       [u(:,i-1) u(:,i)],
                                       k_vals, tspan(counter));
    #}
    case 5
      ## ode45 with Dormand-Prince scheme:
      u_interp = hermite_quartic_interpolation (z, u, k_vals, t);

      ## it is also possible to do a new function evaluation and use
      ## the quintic hermite interpolator
      ## f_half = feval (func, t+1/2*dt, u_half,
      ##                 options.funarguments{:});
      ## u_interp =
      ##   hermite_quintic_interpolation ([z(i-1) z(i)],
      ##                                  [u(:,i-1) u_half u(:,i)],
      ##                                  [k_vals(:,1) f_half ...
      ##                                   k_vals(:,end)],
      ##                                  tspan(counter));

    otherwise
      warning (["High order interpolation not yet implemented: ", ...
                "using cubic interpolation instead"]);
      der(:,1) = feval (func, z(1), u(:,1), args);
      der(:,2) = feval (func, z(2), u(:,2), args);
      u_interp = hermite_cubic_interpolation (z, u, der, t);

  endswitch

endfunction

## The function below can be used in an ODE solver to interpolate the solution
## at the time t_out using 2th order hermite interpolation.
function x_out = quadratic_interpolation (t, x, der, t_out)

  # coefficients of the parabola
  a = -(x(:,1) - x(:,2) - der(:).*(t(1)-t(2))) / (t(1) - t(2))^2;
  b = der(:) - 2*t(1).*a;
  c = x(:,1) - a*t(1)^2 - b*t(1);

  # evauate in t_out
  x_out = a*t_out.^2 + b*t_out + c;

endfunction

## The function below can be used in an ODE solver to interpolate the
## solution at the time t_out using 4th order hermite interpolation.
function x_out = hermite_quartic_interpolation (t, x, der, t_out)

  persistent coefs_u_half = ...
  [(6025192743/30085553152), 0, (51252292925/65400821598), ...
   (-2691868925/45128329728), (187940372067/1594534317056), ...
   (-1776094331/19743644256), (11237099/235043384)].';

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

## The function below can be used in an ODE solver to interpolate the
## solution at the time t_out using 3rd order hermite interpolation.
function x_out = hermite_cubic_interpolation (t, x, der, t_out)

  dt = (t(2) - t(1));
  s = (t_out - t(1)) / dt;
  x_out = ((1 + 2*s) .* (1-s).^2) .* x(:,1) + ...
          (s .* (1-s).^2 * dt   ) .* der(:,1) + ...
          ((3-2*s) .* s.^2      ) .* x(:,2) + ...
          ((s-1) .* s.^2   * dt ) .* der(:,2);

endfunction

