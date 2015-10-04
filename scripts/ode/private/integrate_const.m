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
## @deftypefn {Function File} {[@var{t}, @var{y}] =} integrate_const (@var{@@stepper}, @var{@@fun}, @var{tspan}, @var{x0}, @var{dt}, @var{options})
##
## This function file can be called by an ODE solver function in order to
## integrate the set of ODEs on the interval @var{[t0,t1]} with a constant
## timestep @var{dt}.
##
## This function must be called with two output arguments: @var{t} and @var{y}.
## Variable @var{t} is a column vector and contains the time stamps, instead
## @var{y} is a matrix in which each column refers to a different unknown of
## the problem and the rows number is the same of @var{t} rows number so that
## each row of @var{y} contains the values of all unknowns at the time value
## contained in the corresponding row in @var{t}.
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
## The third input argument is the time vector which defines integration
## interval, that is @var{[tspan(1),tspan(end)]} and all the intermediate
## elements are taken as times at which the solution is required.
##
## The fourth argument contains the initial conditions for the ODEs.
##
## The fifth input argument represents the fixed timestep and the last input
## argument contains some options that may be needed for the stepper.
## @end deftypefn
##
## @seealso{integrate_adaptive, integrate_n_steps}

function solution = integrate_const (stepper, func, tspan, x0, dt, options)

  solution = struct;

  ## first values for time and solution
  t = tspan(1);
  x = x0(:);

  vdirection = odeget (options, "vdirection", [], "fast");
  if (sign (dt) != vdirection)
    error ("OdePkg:InvalidArgument",
           "option ''InitialStep'' has a wrong sign");
  endif

  ## setting parameters
  k = length (tspan);
  counter = 2;
  comp = 0.0;
  tk = tspan(1);
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

  while (counter <= k)
    ## computing the integration step from t to t+dt
    [s, y, ~, k_vals] = stepper (func, z(end), u(:,end), dt, options, k_vals);

    [tk, comp] = kahan (tk,comp, dt);
    options.comp = comp;
    s(end) = tk;
    
    if (options.vhavenonnegative)
      x(options.NonNegative,end) = abs (x(options.NonNegative,end));
      y(options.NonNegative,end) = abs (y(options.NonNegative,end));
      y_est(options.NonNegative,end) = abs (y_est(options.NonNegative,end));
    endif
    
    if (options.vhaveoutputfunction && options.vhaverefine)
      vSaveVUForRefine = u(:,end);
    endif

    ## values on this interval for time and solution
    z = [t(end);s];
    u = [x(:,end),y];

    ## if next tspan value is caught, update counter
    if ((z(end) == tspan(counter))
        || (abs (z(end) - tspan(counter)) /
            (max(abs (z(end)), abs(tspan(counter)))) < 8*eps) )
      counter++;

    ## if there is an element in time vector at which the solution is required
    ## the program must compute this solution before going on with next steps
    elseif (vdirection * z(end) > vdirection * tspan(counter) )
      ## initializing counter for the following cycle
      i = 2;
      while (i <= length (z))

        ## if next tspan value is caught, update counter
        if ((counter <= k)
            && (((z(i) == tspan(counter))
                 || (abs (z(i) - tspan(counter)) /
                     (max(abs (z(i)), abs (tspan(counter)))) < 8*eps))) )
          counter++;
        endif
        ## else, loop until there are requested values inside this subinterval
        while ((counter <= k)
               && vdirection * z(i) > vdirection * tspan(counter) )
          ## add the interpolated value of the solution
          u = [u(:,1:i-1),u(:,i-1) + (tspan(counter)-z(i-1))/(z(i)-z(i-1))* ...
              (u(:,i)-u(:,i-1)),u(:,i:end)];
          ## add the time requested
          z = [z(1:i-1);tspan(counter);z(i:end)];

          ## update counters
          counter++;
          i++;
        endwhile

        ## if new time requested is not out of this interval
        if ((counter <= k)
            && vdirection * z(end) > vdirection * tspan(counter))
          ## update the counter
          i++;
        else
          ## else, stop the cycle and go on with the next iteration
          i = length (z)+1;
        endif

      endwhile
    endif

    if (mod (solution.vcntloop-1, options.OutputSave) == 0)
      x = [x,u(:,2:end)];
      t = [t;z(2:end)];
      solution.vcntsave = solution.vcntsave + 1;    
    endif
    solution.vcntloop = solution.vcntloop + 1;
    vcntiter = 0;
      
    ## Call plot only if a valid result has been found, therefore this
    ## code fragment has moved here. Stop integration if plot function
    ## returns false
    if (options.vhaveoutputfunction)
      for vcnt = 0:options.Refine # Approximation between told and t
        if (options.vhaverefine) # Do interpolation
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
        if (vpltret) # Leave refinement loop
          break
        endif
      endfor
      if (vpltret) # Leave main loop
        solution.vunhandledtermination = false;
        break
      endif
    endif
      
    ## Call event only if a valid result has been found, therefore this
    ## code fragment has moved here. Stop integration if veventbreak is true
    if (options.vhaveeventfunction)
      solution.vevent = odepkg_event_handle (options.Events, t(end), x(:,end),
                                             [], options.vfunarguments{:});
      if (! isempty (solution.vevent{1})
          && solution.vevent{1} == 1)
        t(solution.vcntloop-1,:) = solution.vevent{3}(end,:);
        x(:,solution.vcntloop-1) = solution.vevent{4}(end,:)';
        solution.vunhandledtermination = false; 
        break
      endif
    endif
    
    ## Update counters that count the number of iteration cycles
    solution.vcntcycles = solution.vcntcycles + 1; # Needed for cost statistics
    vcntiter = vcntiter + 1;     # Needed to find iteration problems

    ## Stop solving because the last 1000 steps no successful valid
    ## value has been found
    if (vcntiter >= 5000)
      error (["Solving has not been successful. The iterative",
              " integration loop exited at time t = %f before endpoint at",
              " tend = %f was reached. This happened because the iterative",
              " integration loop does not find a valid solution at this time",
              " stamp. Try to reduce the value of ''InitialStep'' and/or",
              " ''MaxStep'' with the command ''odeset''.\n"],
             s(end), tspan(end));
    endif

    ## if this is the last iteration, save the length of last interval
    if (counter > k)
      j = length (z);
    endif
  endwhile
  
  ## Check if integration of the ode has been successful
  if (vdirection * z(end) < vdirection * tspan(end))
    if (solution.vunhandledtermination == true)
      error ("OdePkg:InvalidArgument",
             ["Solving has not been successful. The iterative integration loop",
              " exited at time t = %f before endpoint at tend = %f was",
              " reached. This may happen if the stepsize grows smaller than",
              " defined in vminstepsize. Try to reduce the value of",
              " ''InitialStep'' and/or ''MaxStep'' with the command",
              " ''odeset''.\n"], z(end), tspan(end));
    else
      warning ("OdePkg:InvalidArgument",
               ["Solver has been stopped by a call of ''break'' in the main",
                " iteration loop at time t = %f before endpoint at tend = %f",
                " was reached. This may happen because the @odeplot function",
                " returned ''true'' or the @event function returned",
                " ''true''.\n"],
               z(end), tspan(end));
    endif
  endif

  ## compute how many values are out of time inerval
  d = vdirection * t((end-(j-1)):end) > vdirection * tspan(end) * ones (j, 1);
  f = sum (d);

  ## remove not-requested values of time and solution
  solution.t = t(1:end-f);
  solution.x = x(:,1:end-f)';
  
endfunction

## Local Variables: ***
## mode: octave ***
## End: ***
