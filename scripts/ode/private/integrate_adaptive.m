## Copyright (C) 2015, Carlo de Falco
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
## @deftypefn {Function File} {[@var{t}, @var{y}] =} integrate_adaptive (@var{@@stepper}, @var{order}, @var{@@fun}, @var{tspan}, @var{x0}, @var{options})
##
## This function file can be called by an ODE solver function in order to
## integrate the set of ODEs on the interval @var{[t0,t1]} with an
## adaptive timestep.
##
## This function must be called with two output arguments: @var{t} and @var{y}.
## Variable @var{t} is a column vector and contains the time stamps, instead
## @var{y} is a matrix in which each column refers to a different unknown
## of the problem and the rows number is the same of @var{t} rows number so
## that each row of @var{y} contains the values of all unknowns at the time
## value contained in the corresponding row in @var{t}.
##
## The first input argument must be a function_handle or an inline function
## representing the stepper, that is the function responsible for step-by-step
## integration.  This function discriminates one method from the others.
##
## The second input argument is the order of the stepper.  It is needed
## to compute the adaptive timesteps.
##
## The third input argument is a function_handle or an inline function that
## defines the set of ODE:
## @ifhtml
## @example
## @math{y' = f(t,y)}
## @end example
## @end ifhtml
## @ifnothtml
## @math{y' = f(t,y)}.
## @end ifnothtml
##
## The fourth input argument is the time vector which defines integration
## interval, that is @var{[tspan(1),tspan(end)]} and all the intermediate
## elements are taken as times at which the solution is required.
##
## The fifth argument represents the initial conditions for the ODEs and the
## last input argument contains some options that may be needed for the stepper.
##
## @end deftypefn
##
## @seealso{integrate_const, integrate_n_steps}

function solution = integrate_adaptive (stepper, order, func, tspan, x0, options)

  fixed_times = numel (tspan) > 2;

  t_new = t_old = t = tspan(1);
  x_new = x_old = x = x0(:);
  
  ## get first initial timestep
  dt = starting_stepsize (order, func, t, x, options.AbsTol,
                          options.RelTol, options.vnormcontrol);
  dt = odeget (options, "InitialStep", dt, "fast_not_empty");
  
  dir = odeget (options, "vdirection", [], "fast");
  dt = dir * min (abs (dt), options.MaxStep);

  options.comp = 0.0;
  
  ## Factor multiplying the stepsize guess
  facmin = 0.8;
  facmax = 1.5;
  fac = 0.38^(1/(order+1));  # formula taken from Hairer

  ## Initialize the OutputFcn
  if (options.vhaveoutputfunction)
    if (options.vhaveoutputselection)
      solution.vretout = x(options.OutputSel,end);
    else 
      solution.vretout = x;
    endif
    feval (options.OutputFcn, tspan, solution.vretout,
           "init", options.vfunarguments{:});
  endif

  ## Initialize the EventFcn
  if (options.vhaveeventfunction)
    odepkg_event_handle (options.Events, tspan(end), x,
                         "init", options.vfunarguments{:});
  endif

  if (options.vhavenonnegative)
    nn = options.NonNegative;
  endif
  
  solution.vcntloop = 2;
  solution.vcntcycles = 1;
  solution.vcntsave = 2;
  solution.vunhandledtermination = true;
  ireject = 0;

  k_vals = []; 
  iout = istep = 1;
  while (dir * t_old < dir * tspan(end))

    ## Compute integration step from t_old to t_new = t_old + dt
    [t_new, options.comp] = kahan (t_old, options.comp, dt);
    [t_new, x_new, x_est, new_k_vals] = ...
    stepper (func, t_old, x_old, dt, options, k_vals, t_new);

    solution.vcntcycles++;

    if (options.vhavenonnegative)
      x(nn,end) = abs (x(nn,end));
      y(nn,end) = abs (y(nn,end));
      y_est(nn,end) = abs (y_est(nn,end));
    endif

    err = AbsRel_Norm (x_new, x_old, options.AbsTol, options.RelTol,
                       options.vnormcontrol, x_est);
    
    ## Accepted solution only if err <= 1.0
    if (err <= 1)

      solution.vcntloop++;
      ireject = 0;
            
      ## if output time steps are fixed
      if (fixed_times)

        t_caught = find ((tspan(iout:end) > t_old)
                         & (tspan(iout:end) <= t_new));
        if (! isempty (t_caught))
          t(t_caught) = tspan(t_caught);
          iout = max (t_caught);
          x(:, t_caught) = interpolate ([t_old, t_new], [x_old, x_new],
                                        t(t_caught));

          istep++;

          if (options.vhaveeventfunction)
            ## Call event on each dense output timestep.
            ##  Stop integration if veventbreak is true
            break_loop = false;
            for idenseout = 1:numel (t_caught)
              id = t_caught(idenseout);
              td = t(id);
              solution.vevent = ...
              odepkg_event_handle (options.Events, t(id), x(:, id), [],
                                   options.vfunarguments{:});
              if (! isempty (solution.vevent{1})
                  && solution.vevent{1} == 1)
                t(id) = solution.vevent{3}(end);
                t = t(1:id);
                x(:, id) = solution.vevent{4}(end, :).';
                x = x(:,1:id);
                solution.vunhandledtermination = false; 
                break_loop = true;
                break;
              endif
            endfor
            if (break_loop)
              break;
            endif
          endif

          ## Call plot.  Stop integration if plot function
          ## returns true.
          if (options.vhaveoutputfunction)
            vcnt = options.Refine + 1;
            vapproxtime = linspace (t_old, t_new, vcnt);
            vapproxvals = interp1 ([t_old, t(t_caught), t_new],
                                   [x_old, x(:, t_caught), x_new],
                                   vapproxtime, 'linear');
            if (options.vhaveoutputselection)
              vapproxvals = vapproxvals(options.OutputSel);
            endif
            vpltret = feval (options.OutputFcn, vapproxtime,
                             vapproxvals, [], options.vfunarguments{:});
            if (vpltret)  # Leave main loop
              solution.vunhandledtermination = false;
              break;
            endif
          endif

        endif
        
      else

        t(++istep)  = t_new;
        x(:, istep) = x_new;
        iout = istep;

        ## Call event handler on new timestep.
        ##  Stop integration if veventbreak is true
        if (options.vhaveeventfunction)
          solution.vevent = ...
          odepkg_event_handle (options.Events, t(istep), x(:, istep), [],
                                   options.vfunarguments{:});
              if (! isempty (solution.vevent{1})
                  && solution.vevent{1} == 1)
                t(istep) = solution.vevent{3}(end);
                x(:, istep) = solution.vevent{4}(end, :).';
                solution.vunhandledtermination = false; 
                break;
              endif
        endif

        ## Call plot.  Stop integration if plot function
        ## returns true.
        if (options.vhaveoutputfunction)
          vcnt = options.Refine + 1;
          vapproxtime = linspace (t_old, t_new, vcnt);
          vapproxvals = interp1 ([t_old, t_new],
                                 [x_old, x_new],
                                 vapproxtime, 'linear');
          if (options.vhaveoutputselection)
            vapproxvals = vapproxvals(options.OutputSel);
          endif
          vpltret = feval (options.OutputFcn, vapproxtime,
                           vapproxvals, [], options.vfunarguments{:});
          if (vpltret)  # Leave main loop
            solution.vunhandledtermination = false;
            break;
          endif
        endif

      endif

      ## move to next time-step
      t_old = t_new;
      x_old = x_new;
      k_vals = new_k_vals;

      solution.vcntloop = solution.vcntloop + 1;
      vcntiter = 0;
            
    else

      ireject++;

      ## Stop solving because in the last 5000 steps no successful valid
      ## value has been found
      if (ireject >= 5000)
        error (["integrate_adaptive: Solving has not been successful. ",
                " The iterative integration loop exited at time",
                " t = %f before endpoint at tend = %f was reached. ",
                " This happened because the iterative integration loop",
                " does not find a valid solution at this time",
                " stamp.  Try to reduce the value of 'InitialStep' and/or",
                " 'MaxStep' with the command 'odeset'.\n"],
               t_old, tspan(end));
      endif

    endif
    
    ## Compute next timestep, formula taken from Hairer
    err += eps;    # avoid divisions by zero
    dt *= min (facmax, max (facmin, fac * (1 / err)^(1 / (order + 1))));
    dt = dir * min (abs (dt), options.MaxStep);    

  endwhile
  
  ## Check if integration of the ode has been successful
  if (dir * t(end) < dir * tspan(end))
    if (solution.vunhandledtermination == true)
      error ("integrate_adaptive: InvalidArgument",
             ["Solving has not been successful.  The iterative",
              " integration loop exited at time t = %f",
              " before endpoint at tend = %f was reached.  This may",
              " happen if the stepsize grows too small. ",
              " Try to reduce the value of 'InitialStep'",
              " and/or 'MaxStep' with the command 'odeset'.\n"],
             t(end), tspan(end));
    else
      warning ("integrate_adaptive: InvalidArgument",
               ["Solver has been stopped by a call of 'break' in the main",
                " iteration loop at time t = %f before endpoint at tend = %f ",
                " was reached.  This may happen because the @odeplot function",
                " returned 'true' or the @event function returned",
                " 'true'.\n"],
               t(end), tspan(end));
    endif
  endif

  ## Remove not-requested values of time and solution
  solution.t = t;
  solution.x = x.';
  
endfunction

