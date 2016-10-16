## Copyright (C) 2013-2016 Roberto Porcu' <roberto.porcu@polimi.it>
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

## -*- texinfo -*-
## @deftypefn {} {@var{h} =} starting_stepsize (@var{order}, @var{func}, @var{t0}, @var{x0}, @var{AbsTol}, @var{RelTol}, @var{normcontrol})
##
## Determine a good initial timestep for an ODE solver of order @var{order}
## using the algorithm described in reference [1].
##
## The input argument @var{func}, is the function describing the differential
## equations, @var{t0} is the initial time, and @var{x0} is the initial
## condition.  @var{AbsTol} and @var{RelTol} are the absolute and relative
## tolerance on the ODE integration taken from an ode options structure.
##
## References:
## [1] E. Hairer, S.P. Norsett and G. Wanner,
## @cite{Solving Ordinary Differential Equations I: Nonstiff Problems},
## Springer.
## @end deftypefn
##
## @seealso{odepkg}

function h = starting_stepsize (order, func, t0, x0,
                                AbsTol, RelTol, normcontrol,
                                args = {})

  ## compute norm of initial conditions
  d0 = AbsRel_norm (x0, x0, AbsTol, RelTol, normcontrol);

  ## compute norm of the function evaluated at initial conditions
  y = func (t0, x0, args{:});
  if (iscell (y))
    y = y{1};
  endif
  d1 = AbsRel_norm (y, y, AbsTol, RelTol, normcontrol);

  if (d0 < 1e-5 || d1 < 1e-5)
    h0 = 1e-6;
  else
    h0 = .01 * (d0 / d1);
  endif

  ## compute one step of Explicit-Euler
  x1 = x0 + h0 * y;

  ## approximate the derivative norm
  yh = func (t0+h0, x1, args{:});
  if (iscell (yh))
    yh = yh{1};
  endif
  d2 = (1 / h0) * ...
       AbsRel_norm (yh - y, yh - y, AbsTol, RelTol, normcontrol);

  if (max (d1, d2) <= 1e-15)
    h1 = max (1e-6, h0 * 1e-3);
  else
    h1 = (1e-2 / max (d1, d2)) ^(1 / (order+1));
  endif

  h = min (100 * h0, h1);

endfunction

