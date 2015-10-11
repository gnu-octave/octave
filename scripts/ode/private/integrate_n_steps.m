## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
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
## @deftypefn {Function File} {[@var{t}, @var{y}] =} integrate_n_steps (@var{@@stepper}, @var{@@fun}, @var{t0}, @var{x0}, @var{dt}, @var{n}, @var{options})
##
## This function file can be called by an ODE solver function in order to
## integrate the set of ODEs on the interval @var{[t0,t0 + n*dt]} with a
## constant timestep dt and on a fixed number of steps.
##
## This function must be called with two output arguments: @var{t} and @var{y}.
## Variable @var{t} is a column vector and contains the time stamps, instead
## @var{y} is a matrix in which each column refers to a different unknown of
## the problem and the rows number is the same of @var{t} rows number so that
## each row of @var{y} contains the values of all unknowns at the time
## value contained in the corresponding row in @var{t}.
##
## The first input argument must be a function_handle or an inline function
## representing the stepper, that is the function responsible for step-by-step
## integration.  This function discriminates one method from the others.
##
## The second input argument is the order of the stepper.  It is needed to
## compute the adaptive timesteps.
##
## The third input argument is a function_handle or an inline function that
## defines the set of ODE:
##
## @ifhtml
## @example
## @math{y' = f(t,y)}
## @end example
## @end ifhtml
## @ifnothtml
## @math{y' = f(t,y)}.
## @end ifnothtml
##
## The third input argument is the starting point for the integration.
##
## The fourth argument contains the initial conditions for the ODEs.
##
## The fifth input argument represents the fixed timestep while the sixth
## contains the number of integration steps.
##
## The last argument is a struct with the options that may be
## needed by the stepper.
## @end deftypefn
##
## @seealso{integrate_adaptive, integrate_const}

function solution = integrate_n_steps (stepper, func, t0, x0, dt, n, options)

  solution = struct ();

  ## first values for time and solution
  x = x0(:); 
  t = t0;

  vdirection = odeget (options, "vdirection", [], "fast");
  if (sign (dt) != vdirection)
    error ("OdePkg:InvalidArgument", "option 'InitialStep' has a wrong sign");
  endif

  comp = 0.0;
  tk = t0;
  options.comp = comp;
  
  ## Initialize the OutputFcn
  if (options.vhaveoutputfunction)
    if (options.vhaveoutputselection)
      solution.vretout = x(options.OutputSel,end);
    else 
      solution.vretout = x;
    endif
    feval (options.OutputFcn, tspan, solution.vretout, "init",
           options.vfunarguments{:});
  endif

  ## Initialize the EventFcn
  if (options.vhaveeventfunction)
    odepkg_event_handle (options.Events, t(end), x, "init",
                         options.vfunarguments{:});
  endif
  
  solution.vcntloop = 2;
  solution.vcntcycles = 1;
  #vu = vinit;
  #vk = vu.' * zeros(1,6);
  vcntiter = 0;
  solution.vunhandledtermination = true;
  solution.vcntsave = 2;
  
  z = t;
  u = x;
  k_vals = feval (func, t , x, options.vfunarguments{:});

  for i = 1:n
    ## Compute the integration step from t to t+dt
    [s, y, ~, k_vals] = stepper (func, z(end), u(:,end), dt, options, k_vals);
    
    [tk, comp] = kahan (tk, comp, dt);
    options.comp = comp;
    s(end) = tk;
    
    if (options.vhavenonnegative)
      x(options.NonNegative,end) = abs (x(options.NonNegative,end));
      y(options.NonNegative,end) = abs (y(options.NonNegative,end));
    endif
    
    if (options.vhaveoutputfunction && options.vhaverefine)
      vSaveVUForRefine = u(:,end);
    endif

    ## values on this interval for time and solution
    z = [t(end);s];
    u = [x(:,end),y];

    x = [x,u(:,2:end)];
    t = [t;z(2:end)];
    solution.vcntsave = solution.vcntsave + 1;    
    solution.vcntloop = solution.vcntloop + 1;
    vcntiter = 0;
      
    ## Call plot only if a valid result has been found, therefore this code
    ## fragment has moved here.  Stop integration if plot function returns false
    if (options.vhaveoutputfunction)
      for vcnt = 0:options.Refine  # Approximation between told and t
        if (options.vhaverefine)  # Do interpolation
          vapproxtime = (vcnt + 1) / (options.Refine + 2);
          vapproxvals = (1 - vapproxtime) * vSaveVUForRefine ...
                        + (vapproxtime) * y(:,end);
          vapproxtime = s(end) + vapproxtime*dt;
        else
          vapproxvals = x(:,end);
          vapproxtime = t(end);
        endif
        if (options.vhaveoutputselection)
          vapproxvals = vapproxvals(options.OutputSel);
        endif
        vpltret = feval (options.OutputFcn, vapproxtime, vapproxvals, [],
                         options.vfunarguments{:});
        if (vpltret)  # Leave refinement loop
          break;
        endif
      endfor
      if (vpltret)  # Leave main loop
        solution.vunhandledtermination = false;
        break;
      endif
    endif
      
    ## Call event only if a valid result has been found, therefore this
    ## code fragment has moved here.  Stop integration if veventbreak is
    ## true
    if (options.vhaveeventfunction)
      solution.vevent = odepkg_event_handle (options.Events, t(end), x(:,end),
                                             [], options.vfunarguments{:});
      if (! isempty (solution.vevent{1})
          && solution.vevent{1} == 1)
        t(solution.vcntloop-1,:) = solution.vevent{3}(end,:);
        x(:,solution.vcntloop-1) = solution.vevent{4}(end,:)';
        solution.vunhandledtermination = false; 
        break;
      endif
    endif
    
    ## Update counters that count the number of iteration cycles
    solution.vcntcycles = solution.vcntcycles + 1;  # Needed for cost statistics
    vcntiter = vcntiter + 1;  # Needed to find iteration problems

    ## Stop solving because the last 1000 steps no successful valid
    ## value has been found
    if (vcntiter >= 5000)
      error (["Solving has not been successful.  The iterative",
              " integration loop exited at time t = %f before endpoint at",
              " tend = %f was reached.  This happened because the iterative",
              " integration loop does not find a valid solution at this time",
              " stamp.  Try to reduce the value of 'InitialStep' and/or",
              " 'MaxStep' with the command 'odeset'.\n"],
             s(end), tspan(end));
    endif
  endfor

  ## Check if integration of the ode has been successful
  #if (vdirection * z(end) < vdirection * tspan(end))
  #  if (solution.vunhandledtermination == true)
  #    error ("OdePkg:InvalidArgument",
  #           ["Solving has not been successful.  The iterative",
  #            " integration loop exited at time t = %f",
  #            " before endpoint at tend = %f was reached.  This may",
  #            " happen if the stepsize grows smaller than defined in",
  #            " vminstepsize.  Try to reduce the value of 'InitialStep'",
  #            " and/or 'MaxStep' with the command 'odeset'.\n"],
  #           z(end), tspan(end));
  #  else
  #    warning ("OdePkg:InvalidArgument",
  #             ["Solver has been stopped by a call of 'break' in the main",
  #              " iteration loop at time t = %f before endpoint at tend = %f",
  #              " was reached.  This may happen because the @odeplot function",
  #              " returned 'true' or the @event function returned",
  #              " 'true'.\n"],
  #             z(end), tspan(end));
  #  endif
  #endif

  solution.t = t;
  solution.x = x';
  
endfunction

