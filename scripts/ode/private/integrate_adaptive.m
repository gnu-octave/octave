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
## @deftypefn {} {@var{solution} =} integrate_adaptive (@var{@@stepper}, @var{order}, @var{@@fcn}, @var{tspan}, @var{x0}, @var{options})
##
## This function file can be called by an ODE solver function in order to
## integrate the set of ODEs on the interval @var{[t0, t1]} with an adaptive
## timestep.
##
## The function returns a structure @var{solution} with two fields: @var{t}
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
## The second input argument is the order of the stepper.  It is needed
## to compute the adaptive timesteps.
##
## The third input argument is a function handle or inline function that
## defines the ODE:
##
## @ifhtml
##
## @example
## @math{y' = f(t,y)}
## @end example
##
## @end ifhtml
## @ifnothtml
## @math{y' = f(t,y)}.
## @end ifnothtml
##
## The fourth input argument is the time vector which defines the integration
## interval, i.e., @var{[tspan(1), tspan(end)]} and all intermediate elements
## are taken as times at which the solution is required.
##
## The fifth argument represents the initial conditions for the ODEs and the
## last input argument contains some options that may be needed for the
## stepper.
##
## @end deftypefn

function solution = integrate_adaptive (stepper, order, fcn, tspan, x0,
                                        options)

  fixed_times = numel (tspan) > 2;

  t_new = t_old = ode_t = output_t = tspan(1);
  x_new = x_old = ode_x = output_x = x0(:);

  ## Get first initial timestep
  dt = options.InitialStep;
  if (isempty (dt))
    dt = starting_stepsize (order, fcn, ode_t, ode_x,
                            options.AbsTol, options.RelTol,
                            strcmp (options.NormControl, "on"),
                            options.funarguments);
  endif

  dir = options.direction;
  dt = dir * min (abs (dt), options.MaxStep);

  options.comp = 0.0;

  ## Factor multiplying the stepsize guess
  facmin = 0.8;
  facmax = 1.5;
  fac = 0.38^(1/(order+1));  # formula taken from Hairer

  ## Initialize Refine value
  refine = options.Refine;
  if isempty (refine)
    refine = 1;
  elseif ((refine != round (refine)) || (refine < 1))
    refine = 1;
    warning ("integrate_adaptive:invalid_refine",
               ["Invalid value of Refine. Refine must be a positive " ...
                "integer. Setting Refine = 1."] );
  endif

  ## Initialize the OutputFcn
  if (options.haveoutputfunction)
    if (! isempty (options.OutputSel))
      solution.retout = output_x(options.OutputSel, end);
    else
      solution.retout = output_x;
    endif
    feval (options.OutputFcn, tspan, solution.retout, "init",
           options.funarguments{:});
  endif

  ## Initialize the EventFcn
  have_EventFcn = false;
  if (! isempty (options.Events))
    have_EventFcn  = true;
    options.Events = @(t,y) options.Events (t, y, options.funarguments{:});
    ode_event_handler (options.Events, tspan(1), ode_x, ...
                       [], order, "init");
  endif

  if (options.havenonnegative)
    nn = options.NonNegative;
  endif

  solution.cntloop = 0;
  solution.cntcycles = 0;
  solution.cntsave = 2;
  solution.unhandledtermination = true;
  ireject = 0;

  NormControl = strcmp (options.NormControl, "on");
  k_vals = [];
  iout = istep = 1;

  while (dir * t_old < dir * tspan(end))

    ## Compute integration step from t_old to t_new = t_old + dt
    [t_new, options.comp] = kahan (t_old, options.comp, dt);
    [t_new, x_new, x_est, new_k_vals] = ...
      stepper (fcn, t_old, x_old, dt, options, k_vals, t_new);

    solution.cntcycles += 1;

    if (options.havenonnegative)
      x_new(nn, end) = abs (x_new(nn, end));
      x_est(nn, end) = abs (x_est(nn, end));
    endif

    err = AbsRel_norm (x_new, x_old, options.AbsTol, options.RelTol,
                       NormControl, x_est);

    ## Accept solution only if err <= 1.0
    if (err <= 1)

      solution.cntloop += 1;
      ireject = 0;              # Clear reject counter
      terminal_event = false;
      terminal_output = false;

      istep++;
      ode_t(istep) = t_new;
      ode_x(:, istep) = x_new;
      iadd = 0;                 # Number of output points added this iteration

      ## Check for Events
      if (have_EventFcn)
        solution.event = ode_event_handler ([], t_new, x_new, ...
                                            new_k_vals, [], []);
        ## Check for terminal Event
        if (! isempty (solution.event{1}) && solution.event{1} == 1)
          ode_t(istep) = solution.event{3}(end);
          ode_x(:, istep) = solution.event{4}(end, :).';
          solution.unhandledtermination = false;
          terminal_event = true;
        endif
      endif

      ## Interpolate to specified or Refined points
      if (fixed_times)
        t_caught = find ((dir * tspan(iout:end) > dir * t_old) ...
                         & (dir * tspan(iout:end) <= dir * ode_t(istep)));
        t_caught = t_caught + iout - 1;
        iadd = length (t_caught);
        if (! isempty (t_caught))
          output_t(t_caught) = tspan(t_caught);
          iout = max (t_caught);
          output_x(:, t_caught) = ...
            runge_kutta_interpolate (order, [t_old t_new], [x_old x_new], ...
                                     output_t(t_caught), new_k_vals);
        endif
        ## Add a possible additional output value if we found a terminal Event
        if ((terminal_event == true) && ...
            (dir * ode_t(istep) > dir * output_t(iout)))
          iadd += 1;
          iout += 1;
          output_x(:, iout) = ode_x(:, istep);
          output_t(iout) = ode_t(istep);
        endif
      elseif (refine > 1)
        iadd = refine;
        tadd = linspace (t_old, ode_t(istep), refine + 1);
        tadd = tadd(2:end);
        output_x(:, iout + (1:iadd)) = ...
          runge_kutta_interpolate (order, [t_old t_new], [x_old x_new], ...
                                   tadd, new_k_vals);
        output_t(iout + (1:iadd)) = tadd;
        iout = length (output_t);
      else                         # refine = 1
        iadd = 1;
        iout += iadd;
        output_x(:, iout) = ode_x(:, istep);
        output_t(iout) = ode_t(istep);
      endif

      ## Call OutputFcn
      if ((options.haveoutputfunction) && (iadd > 0))
        xadd = output_x(:, (iout-iadd+1):end);
        tadd = output_t((iout-iadd+1):end);
        if (! isempty (options.OutputSel))
          xadd = xadd(options.OutputSel, :);
        endif
        stop_solve = feval (options.OutputFcn, tadd, xadd, ...
                            [], options.funarguments{:});
        if (stop_solve)
          solution.unhandledtermination = false;
          terminal_output = true;
        endif
      endif
      if (terminal_event || terminal_output)
        break;                     # break from main loop
      endif


      ## move to next time-step
      t_old = t_new;
      x_old = x_new;
      k_vals = new_k_vals;

    else  # error condition

      ireject += 1;

      ## Stop solving if, in the last 5,000 steps, no successful valid
      ## value has been found.
      if (ireject >= 5_000)
        error (["integrate_adaptive: Solving was not successful. ", ...
                " The iterative integration loop exited at time", ...
                " t = %f before the endpoint at tend = %f was reached. ", ...
                " This happened because the iterative integration loop", ...
                " did not find a valid solution at this time stamp. ", ...
                " Try to reduce the value of 'InitialStep' and/or", ...
                " 'MaxStep' with the command 'odeset'.\n"],
               t_old, tspan(end));
      endif

    endif

    ## Compute next timestep, formula taken from Hairer
    err += eps;  # avoid divisions by zero
    dt *= min (facmax, max (facmin, fac * (1 / err)^(1 / (order + 1))));
    dt = dir * min (abs (dt), options.MaxStep);
    if (! (abs (dt) > eps (ode_t(end))))
      break;
    endif

    ## Make sure we don't go past tpan(end)
    dt = dir * min (abs (dt), abs (tspan(end) - t_old));

  endwhile

  ## Check if integration of the ode has been successful
  if (dir * ode_t(end) < dir * tspan(end))
    if (solution.unhandledtermination == true)
      warning ("integrate_adaptive:unexpected_termination",
               [" Solving was not successful. ", ...
                " The iterative integration loop exited at time", ...
                " t = %f before the endpoint at tend = %f was reached. ", ...
                " This may happen if the stepsize becomes too small. ", ...
                " Try to reduce the value of 'InitialStep'", ...
                " and/or 'MaxStep' with the command 'odeset'."],
               ode_t(end), tspan(end));
    endif
  endif

  ## Set up return structure
  solution.ode_t = ode_t(:);
  solution.ode_x = ode_x.';
  solution.output_t = output_t(:);
  solution.output_x = output_x.';

endfunction
