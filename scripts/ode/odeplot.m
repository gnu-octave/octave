## Copyright (C) 2006-2016 Thomas Treichl <treichl@users.sourceforge.net>
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

## Author: Thomas Treichl <treichl@users.sourceforge.net>

## -*- texinfo -*-
## @deftypefn {} {@var{retval} =} odeplot (@var{t}, @var{y}, @var{flag})
##
## Open a new figure window and plot input @var{y} over time during the
## solving of an ode problem.
##
## The input @var{y} is a column vector.  The types and values of the input
## parameter @var{t} and output parameter @var{ret} depend on the input
## @var{flag} that is of type string.  If @var{flag} is
##
## @table @option
## @item @qcode{"init"}
## then @var{t} must be a column vector of length 2 with the first and
## the last time step;  Nothing is returned from this function.
##
## @item @qcode{""}
## then @var{t} must be a scalar double specifying the actual time step;
## The return value is false (resp. value 0) for @qcode{"not stop solving"}.
##
## @item @qcode{"done"}
## then @var{t} must be a scalar double specifying the last time step;
## Nothing is returned from this function.
## @end table
##
## This function is called by an ode solver function if it was specified in
## an options structure with @code{odeset}.  This function is an internal
## helper function; It should never be necessary for this function to be
## directly called by a user.  There is only minimal error detection
## implemented in order to to achieve the highest performance.
##
## For example, solve an anonymous implementation of the
## @nospell{@qcode{"Van der Pol"}} equation and display the results while
## solving
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
## @seealso{odeset, odeget}
## @end deftypefn

function retval = odeplot (t, y, flag)

  ## No input argument checking is done for better performance
  persistent hlines num_lines told yold;
  persistent idx = 1;   # Don't remove.  Required for Octave parser.

  if (isempty (flag))
    ## Default case, plot and return a value
    ## FALSE for "not stopping the integration"
    ## TRUE  for "stopping the integration"
    idx += 1;
    told(idx,1) = t(1,1);
    yold(:,idx) = y(:,1);
    for i = 1:num_lines
      set (hlines(i), "xdata", told, "ydata", yold(i,:));
    endfor
    drawnow;

    retval = false;

  elseif (strcmp (flag, "init"))
    ## Nothing to return
    ## t is either the time slot [tstart tstop] or [t0, t1, ..., tn]
    ## y is the initial value vector "init"
    idx = 1;
    told = t(1,1);
    yold = y(:,1);
    figure ();
    hlines = plot (told, yold, "-", "marker", ".", "markersize", 9);
    num_lines = numel (hlines);

  elseif (strcmp (flag, "done"))
    ## Cleanup after ode solver has finished.
    hlines = num_lines = told = yold = idx = [];

  endif

endfunction


%!demo
%! ## Solve an anonymous implementation of the Van der Pol equation
%! ## and display the results while solving
%! fvdp = @(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
%! opt = odeset ("OutputFcn", @odeplot, "RelTol", 1e-6);
%! sol = ode45 (fvdp, [0 20], [2 0], opt);

