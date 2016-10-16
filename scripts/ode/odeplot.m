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
## @deftypefn {} {@var{ret} =} odeplot (@var{t}, @var{y}, @var{flag})
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
## fvdb = @@(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
##
## opt = odeset ("OutputFcn", @@odeplot, "RelTol", 1e-6);
## sol = ode45 (fvdb, [0 20], [2 0], opt);
## @end group
## @end example
##
## @seealso{odeset, odeget}
## @end deftypefn

function ret = odeplot (t, y, flag, varargin)

  ## No input argument check is done for a higher processing speed
  persistent fig told yold counter;

  if (strcmp (flag, "init"))
    ## Nothing to return, t is either the time slot [tstart tstop]
    ## or [t0, t1, ..., tn], y is the initial value vector "init"
    counter = 1;
    fig = figure ();
    told = t(1,1);
    yold = y(:,1);

  elseif (isempty (flag))
    ## Return something, either false for "not stopping
    ## the integration" or true for "stopping the integration"
    counter += 1;
    figure (fig);
    told(counter,1) = t(1,1);
    yold(:,counter) = y(:,1);
    ## FIXME: Why not use '.' rather than 'o' and skip the markersize?
    ## FIXME: Why not just update the xdata, ydata properties?
    ##        Calling plot involves a lot of overhead.
    plot (told, yold, "-o", "markersize", 1); drawnow;
    ret = false;

  elseif (strcmp (flag, "done"))
    ## Cleanup after ode solver has finished.
    clear ("figure", "told", "yold", "counter");

  endif

endfunction


%!demo
%! # Solve an anonymous implementation of the Van der Pol equation
%! # and display the results while solving
%! fvdb = @(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
%! opt = odeset ("OutputFcn", @odeplot, "RelTol", 1e-6);
%! sol = ode45 (fvdb, [0 20], [2 0], opt);

