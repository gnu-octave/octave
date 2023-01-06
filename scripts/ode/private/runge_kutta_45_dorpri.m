########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{t_next}, @var{x_next}] =} runge_kutta_45_dorpri (@var{@@fcn}, @var{t}, @var{x}, @var{dt})
## @deftypefnx {} {[@var{t_next}, @var{x_next}] =} runge_kutta_45_dorpri (@var{@@fcn}, @var{t}, @var{x}, @var{dt}, @var{options})
## @deftypefnx {} {[@var{t_next}, @var{x_next}] =} runge_kutta_45_dorpri (@var{@@fcn}, @var{t}, @var{x}, @var{dt}, @var{options}, @var{k_vals})
## @deftypefnx {} {[@var{t_next}, @var{x_next}] =} runge_kutta_45_dorpri (@var{@@fcn}, @var{t}, @var{x}, @var{dt}, @var{options}, @var{k_vals}, @var{t_next})
## @deftypefnx {} {[@var{t_next}, @var{x_next}, @var{x_est}] =} runge_kutta_45_dorpri (@dots{})
## @deftypefnx {} {[@var{t_next}, @var{x_next}, @var{x_est}, @var{k_vals_out}] =} runge_kutta_45_dorpri (@dots{})
##
## This function can be used to integrate a system of ODEs with a given initial
## condition @var{x} from @var{t} to @var{t+dt} with the Dormand-Prince method.
## For the definition of this method see
## @url{http://en.wikipedia.org/wiki/Dormand%E2%80%93Prince_method}.
##
## First input argument is the function describing the system of ODEs to be
## integrated.
##
## Second input parameter is the first extreme of integration interval.
##
## Third input argument is the initial condition of the system.
##
## Fourth input argument is the timestep, that is the length of the
## integration interval.
##
## Fifth input parameter is optional and describes a set of options useful to
## adapt the computation to what is needed.
##
## Sixth input parameter is optional and describes the Runge-Kutta evaluations
## of the previous step to use in an FSAL scheme.
##
## Seventh input parameter is optional and is the time (@var{t_next}) to
## integrate to.  The default is @code{@var{t} + @var{dt}}.
##
## First output argument is the final integration time value.
##
## Second output parameter is the higher order computed solution at time
## @var{t_next} (local extrapolation).
##
## Third output parameter is a lower order solution for the estimation of the
## error.
##
## Fourth output parameter is matrix containing the Runge-Kutta evaluations
## to use in an FSAL scheme or for dense output.
## @end deftypefn

function [t_next, x_next, x_est, k] = runge_kutta_45_dorpri (fcn, t, x, dt,
                                                             options = [],
                                                             k_vals = [],
                                                             t_next = t + dt)

  ## Reference: Hairer, Ernst; Nørsett, Syvert Paul; Wanner, Gerhard (2008),
  ## Solving ordinary differential equations I: Nonstiff problems,
  ## Berlin, New York: Springer-Verlag, ISBN 978-3-540-56670-0
  persistent a = [0           0          0           0        0          0;
                  1/5         0          0           0        0          0;
                  3/40        9/40       0           0        0          0;
                  44/45      -56/15      32/9        0        0          0;
                  19372/6561 -25360/2187 64448/6561 -212/729  0          0;
                  9017/3168  -355/33     46732/5247  49/176  -5103/18656 0];
  persistent b = [0, 1/5, 3/10, 4/5, 8/9, 1, 1];
  persistent c = [35/384, 0, 500/1113, 125/192, -2187/6784, 11/84];
  persistent c_prime = [5179/57600, 0, 7571/16695, 393/640, ...
                        -92097/339200, 187/2100, 1/40];

  s = t + dt * b;
  cc = dt * c;
  aa = dt * a;
  k = zeros (rows (x), 7);

  if (! isempty (options))   # extra arguments for function evaluator
    args = options.funarguments;
  else
    args = {};
  endif

  if (! isempty (k_vals))    # k values from previous step are passed
    k(:,1) = k_vals(:,end);  # FSAL property
  else
    k(:,1) = feval (fcn, t, x, args{:});
  endif

  k(:,2) = feval (fcn, s(2), x + k(:,1)   * aa(2, 1).'  , args{:});
  k(:,3) = feval (fcn, s(3), x + k(:,1:2) * aa(3, 1:2).', args{:});
  k(:,4) = feval (fcn, s(4), x + k(:,1:3) * aa(4, 1:3).', args{:});
  k(:,5) = feval (fcn, s(5), x + k(:,1:4) * aa(5, 1:4).', args{:});
  k(:,6) = feval (fcn, s(6), x + k(:,1:5) * aa(6, 1:5).', args{:});

  ## compute new time and new values for the unknowns
  ## t_next = t + dt;
  x_next = x + k(:,1:6) * cc(:);  # 5th order approximation

  ## if the estimation of the error is required
  if (nargout >= 3)
    ## new solution to be compared with the previous one
    k(:,7) = feval (fcn, t_next, x_next, args{:});
    cc_prime = dt * c_prime;
    x_est = x + k * cc_prime(:);
  endif

endfunction
