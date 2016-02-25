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
## @deftypefn {} {@var{solution} =} integrate_n_steps (@var{@@stepper}, @var{@@func}, @var{t0}, @var{x0}, @var{dt}, @var{n}, @var{options})
##
## This function file can be called by an ODE solver function in order to
## integrate the set of ODEs on the interval @var{[t0,t0 + n*dt]} with a
## constant timestep dt and on a fixed number of steps.
##
## The function returns a structure @var{solution} with two fieldss: @var{t}
## and @var{y}.  @var{t} is a column vector and contains the time stamps.
## @var{y} is a matrix in which each column refers to a different unknown
## of the problem and the row number is the same as the @var{t} row number.
## Thus, each row of the matrix @var{y} contains the values of all unknowns at
## the time value contained in the corresponding row in @var{t}.
##
## The first input argument must be a function handle or inline function
## representing the stepper, i.e., the function responsible for step-by-step
## integration.  This function discriminates one method from the others.
##
## The second input argument is the order of the stepper.  It is needed to
## compute the adaptive timesteps.
##
## The third input argument is a function handle or inline function that
## defines the ODE:
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
## The last argument is a struct with the options that may be needed by the
## stepper.
## @end deftypefn
##
## @seealso{integrate_adaptive, integrate_const}

function solution = integrate_n_steps (stepper, func, t0, x0, dt, n, options)

  solution = struct ();

  ## first values for time and solution
  x = x0(:);
  t = t0;

  direction = odeget (options, "direction", [], "fast");
  if (sign (dt) != direction)
    error ("Octave:invalid-input-arg", "option 'InitialStep' has a wrong sign");
  endif

  comp = 0.0;
  tk = t0;
  options.comp = comp;

  ## Initialize the OutputFcn
  if (options.haveoutputfunction)
    if (options.haveoutputselection)
      solution.retout = x(options.OutputSel,end);
    else
      solution.retout = x;
    endif
    feval (options.OutputFcn, tspan, solution.retout, "init",
           options.funarguments{:});
  endif

  ## Initialize the EventFcn
  if (options.haveeventfunction)
    ode_event_handler (options.Events, t(end), x, "init",
                         options.funarguments{:});
  endif

  solution.cntloop = 2;
  solution.cntcycles = 1;
  cntiter = 0;
  solution.unhandledtermination = true;
  solution.cntsave = 2;

  z = t;
  u = x;
  k_vals = feval (func, t , x, options.funarguments{:});

  for i = 1:n
    ## Compute the integration step from t to t+dt
    [s, y, ~, k_vals] = stepper (func, z(end), u(:,end), dt, options, k_vals);

    [tk, comp] = kahan (tk, comp, dt);
    options.comp = comp;
    s(end) = tk;

    if (options.havenonnegative)
      x(options.NonNegative,end) = abs (x(options.NonNegative,end));
      y(options.NonNegative,end) = abs (y(options.NonNegative,end));
    endif

    if (options.haveoutputfunction && options.haverefine)
      SaveVUForRefine = u(:,end);
    endif

    ## values on this interval for time and solution
    z = [t(end);s];
    u = [x(:,end),y];

    x = [x,u(:,2:end)];
    t = [t;z(2:end)];
    solution.cntsave += 1;
    solution.cntloop += 1;
    cntiter = 0;

    ## Call OutputFcn only if a valid result has been found.
    ## Stop integration if function returns false.
    if (options.haveoutputfunction)
      for cnt = 0:options.Refine  # Approximation between told and t
        if (options.haverefine)   # Do interpolation
          approxtime = (cnt + 1) / (options.Refine + 2);
          approxvals = (1 - approxtime) * SaveVUForRefine ...
                        + (approxtime) * y(:,end);
          approxtime = s(end) + approxtime*dt;
        else
          approxvals = x(:,end);
          approxtime = t(end);
        endif
        if (options.haveoutputselection)
          approxvals = approxvals(options.OutputSel);
        endif
        pltret = feval (options.OutputFcn, approxtime, approxvals, [],
                        options.funarguments{:});
        if (pltret)  # Leave refinement loop
          break;
        endif
      endfor
      if (pltret)  # Leave main loop
        solution.unhandledtermination = false;
        break;
      endif
    endif

    ## Call Events function only if a valid result has been found.
    ## Stop integration if eventbreak is true.
    if (options.haveeventfunction)
      solution.event = ode_event_handler (options.Events, t(end), x(:,end),
                                             [], options.funarguments{:});
      if (! isempty (solution.event{1}) && solution.event{1} == 1)
        t(solution.cntloop-1,:) = solution.event{3}(end,:);
        x(:,solution.cntloop-1) = solution.event{4}(end,:)';
        solution.unhandledtermination = false;
        break;
      endif
    endif

    ## Update counters that count the number of iteration cycles
    solution.cntcycles += 1;  # Needed for cost statistics
    cntiter += 1;             # Needed to find iteration problems

    ## Stop solving because, in the last 5,000 steps, no successful valid
    ## value has been found
    if (cntiter >= 5_000)
      error (["integrate_n_steps: Solving was not successful. ", ...
              " The iterative integration loop exited at time", ...
              " t = %f before the endpoint at tend = %f was reached. ", ...
              " This happened because the iterative integration loop", ...
              " did not find a valid solution at this time stamp. ", ...
              " Try to reduce the value of 'InitialStep' and/or", ...
              " 'MaxStep' with the command 'odeset'."],
             s(end), tspan(end));
    endif
  endfor

  ## Check if integration of the ode has been successful
  #if (direction * z(end) < direction * tspan(end))
  #  if (solution.unhandledtermination == true)
  #   error ("integrate_n_steps:unexpected_termination",
  #          [" Solving was not successful. ", ...
  #           " The iterative integration loop exited at time", ...
  #           " t = %f before the endpoint at tend = %f was reached. ", ...
  #           " This may happen if the stepsize becomes too small. ", ...
  #           " Try to reduce the value of 'InitialStep'", ...
  #           " and/or 'MaxStep' with the command 'odeset'."],
  #           z(end), tspan(end));
  #  else
  #   warning ("integrate_n_steps:unexpected_termination",
  #            ["Solver was stopped by a call of 'break'", ...
  #             " in the main iteration loop at time", ...
  #             " t = %f before the endpoint at tend = %f was reached. ", ...
  #             " This may happen because the @odeplot function", ...
  #             " returned 'true' or the @event function returned 'true'."],
  #             z(end), tspan(end));
  #  endif
  #endif

  solution.t = t;
  solution.x = x';

endfunction

