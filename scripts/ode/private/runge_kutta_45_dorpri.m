## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
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
## @deftypefn {Command} {[@var{t_next}, @var{x_next}] =}
## runge_kutta_45_dorpri (@var{@@fun}, @var{t}, @var{x}, @var{dt},
## [@var{options}, @var{k_vals_in}])
## @deftypefnx {Command} {[@var{t_next}, @var{x_next}, @var{x_est}] =}
## runge_kutta_45_dorpri (@var{@@fun}, @var{t}, @var{x}, @var{dt},
## [@var{options}, @var{k_vals_in}])
## @deftypefnx {Command} {[@var{t_next}, @var{x_next}, @var{x_est},
## @var{k_vals_out}] =} runge_kutta_45_dorpri (@var{@@fun}, @var{t}, @var{x},
## @var{dt}, [@var{options}, @var{k_vals_in}])
##
## This function can be used to integrate a system of ODEs with a given initial
## condition @var{x} from @var{t} to @var{t+dt}, with the Dormand-Prince method.
## For the definition of this method see
## @url{http://en.wikipedia.org/wiki/Dormand%E2%80%93Prince_method}.
##
## First output argument is the final integration time value.
##
## Second output parameter is the higher order computed solution at time
## @var{t_next} (local extrapolation).
##
## Third output parameter is a lower order solution for the estimation
## of the error.
##
## Fourth output parameter is matrix containing the Runge-Kutta evaluations
## to use in a FSAL scheme or for dense output.
##
## First input argument is the function describing the system of ODEs
## to be integrated.
##
## Second input parameter is the first extreme of integration interval.
##
## Third input argument is the initial condition of the system.
##
## Fourth input argument is the timestep, that is the length of the
## integration interval.
##
## Fifth input parameter is optional and describes a set of options useful
## to adapt the computation to what is needed.
##
## Sixth input parameter is optional and describes the Runge-Kutta evaluations
## of the previous step to use in a FSAL scheme.
## @end deftypefn
##
## @seealso{odepkg}

function varargout = runge_kutta_45_dorpri (f, t, x, dt, varargin)

  options = varargin{1};
  k = zeros (size (x, 1), 4);

  if (nargin == 5) # only the options are passed
    k(:,1) = feval (f, t , x, options.vfunarguments{:});
  elseif (nargin == 6) # both the options and the k values are passed
    k(:,1) = varargin{2}(:,end); # FSAL property
  endif
  k(:,1) = feval (f, t, x, options.vfunarguments{:});
  k(:,2) = feval (f, t + (1/5)*dt, ...
                  x + dt * (1/5)*k(:,1), ...
                  options.vfunarguments{:});
  k(:,3) = feval (f, t + (3/10)*dt, ...
                  x + dt * ((3/40)*k(:,1) + (9/40)*k(:,2)), ...
                  options.vfunarguments{:});
  k(:,4) = feval (f, t + (4/5)*dt, ...
                  x + dt * ((44/45)*k(:,1) - (56/15)*k(:,2) + (32/9)*k(:,3)), ...
                  options.vfunarguments{:});
  k(:,5) = feval (f, t + (8/9)*dt, ...
                  x + dt * ((19372/6561)*k(:,1) - (25360/2187)*k(:,2) ...
                            + (64448/6561)*k(:,3) - (212/729)*k(:,4)), ...
                  options.vfunarguments{:});
  k(:,6) = feval (f, t + dt, ...
                  x + dt * ((9017/3168)*k(:,1) - (355/33)*k(:,2) ...
                            + (46732/5247)*k(:,3) + (49/176)*k(:,4) ...
                            - (5103/18656)*k(:,5)), ...
                  options.vfunarguments{:});

  ## compute new time and new values for the unkwnowns
  varargout{1} = t + dt;
  varargout{2} = x + dt * ((35/384)*k(:,1) + (500/1113)*k(:,3)
                           + (125/192)*k(:,4) - (2187/6784)*k(:,5)
                           + (11/84)*k(:,6)); # 5th order approximation

  ## if the estimation of the error is required
  if (nargout >= 3)
    ## new solution to be compared with the previous one
    k(:,7) = feval (f, t + dt, varargout{2}, options.vfunarguments{:});
    ## x_est according to Shampine 1986:
    ## varargout{3} = x + dt * ((1951/21600)*k(:,1) + (22642/50085)*k(:,3)
    ##                          + (451/720)*k(:,4) - (12231/42400)*k(:,5)
    ##                          + (649/6300)*k(:,6) + (1/60)*k(:,7));
    varargout{3} = x + dt * ((5179/57600)*k(:,1) + (7571/16695)*k(:,3)
                             + (393/640)*k(:,4) - (92097/339200)*k(:,5)
                             + (187/2100)*k(:,6) + (1/40)*k(:,7)); # x_est
    varargout{4} = k;
  endif

endfunction

## Local Variables: ***
## mode: octave ***
## End: ***
