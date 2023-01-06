########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn {} {@var{stop_solve} =} odeplot (@var{t}, @var{y}, @var{flag})
##
## Open a new figure window and plot the solution of an ode problem at each
## time step during the integration.
##
## The types and values of the input parameters @var{t} and @var{y} depend on
## the input @var{flag} that is of type string.  Valid values of @var{flag}
## are:
##
## @table @option
## @item @qcode{"init"}
## The input @var{t} must be a column vector of length 2 with the first and
## last time step (@code{[@var{tfirst} @var{tlast}]}.  The input @var{y}
## contains the initial conditions for the ode problem (@var{y0}).
##
## @item @qcode{""}
## The input @var{t} must be a scalar double or vector specifying the time(s)
## for which the solution in input @var{y} was calculated.
##
## @item @qcode{"done"}
## The inputs should be empty, but are ignored if they are present.
## @end table
##
## @code{odeplot} always returns false, i.e., don't stop the ode solver.
##
## Example: solve an anonymous implementation of the
## @nospell{@qcode{"Van der Pol"}} equation and display the results while
## solving.
##
## @example
## @group
## fvdp = @@(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
##
## opt = odeset ("OutputFcn", @@odeplot, "RelTol", 1e-6);
## sol = ode45 (fvdp, [0 20], [2 0], opt);
## @end group
## @end example
##
## Background Information:
## This function is called by an ode solver function if it was specified in
## the @qcode{"OutputFcn"} property of an options structure created with
## @code{odeset}.  The ode solver will initially call the function with the
## syntax @code{odeplot ([@var{tfirst}, @var{tlast}], @var{y0}, "init")}.  The
## function initializes internal variables, creates a new figure window, and
## sets the x limits of the plot.  Subsequently, at each time step during the
## integration the ode solver calls @code{odeplot (@var{t}, @var{y}, [])}.
## At the end of the solution the ode solver calls
## @code{odeplot ([], [], "done")} so that odeplot can perform any clean-up
## actions required.
## @seealso{odeset, odeget, ode23, ode45}
## @end deftypefn

function stop_solve = odeplot (t, y, flag)

  ## No input argument checking is done for better performance
  persistent hlines num_lines told yold;

  ## odeplot never stops the integration
  stop_solve = false;

  if (isempty (flag))
    ## Default case, plot and return a value
    told = [told; t(:)];
    yold = [yold, y];
    for i = 1:num_lines
      set (hlines(i), "xdata", told, "ydata", yold(i,:));
    endfor
    drawnow ();

    retval = false;

  elseif (strcmp (flag, "init"))
    ## t is either the time slot [tstart tstop] or [t0, t1, ..., tn]
    ## y is the initial value vector for the ode solution
    told = t(1);
    yold = y(:);
    figure ();
    hlines = plot (told, yold, "o-");
    xlim ([t(1), t(end)]);  # Fix limits which also speeds up plotting
    num_lines = numel (hlines);

  elseif (strcmp (flag, "done"))
    ## Cleanup after ode solver has finished.
    hlines = num_lines = told = yold = [];

  endif

endfunction


%!demo
%! ## Solve an anonymous implementation of the Van der Pol equation
%! ## and display the results while solving
%! fvdp = @(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
%! opt = odeset ("RelTol", 1e-6);
%! ode45 (fvdp, [0 20], [2 0], opt);

%!demo
%! ## Demonstrate simple multi-curve plot from t = 0 to 2 using initial,
%! ## intermediate, and terminating odeplot calling syntaxes.
%! t = linspace(0,2,10);
%! y = [0.2*t; -0.1*t.^2-1; sin(t)];
%!
%! disp("Plot initial points\n");
%! odeplot ([0 2], y(:,1), "init");
%! title("Plot first time step");
%! pause(1.5);
%!
%! disp("Append single time step\n");
%! odeplot (t(2), y(:,2), []);
%! title("Append second time step");
%! pause(1.5);
%!
%! disp("Append remaining time steps\n");
%! odeplot (t(3:end), y(:, 3:end), []);
%! title("Plot all time steps");
%! pause(1.5);
%!
%! disp("Terminate odeplot\n");
%! odeplot ([], [], "done");
%! title("Plot complete");

