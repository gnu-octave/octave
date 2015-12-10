## Copyright (C) 2006-2015 Thomas Treichl <treichl@users.sourceforge.net>
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

%# -*- texinfo -*-
%# @deftypefn {Function File} {[@var{ret}] =} odeplot (@var{t}, @var{y}, @var{flag})
%#
%# Open a new figure window and plot the results from the variable @var{y} of type column vector over time while solving. The types and the values of the input parameter @var{t} and the output parameter @var{ret} depend on the input value @var{flag} that is of type string. If @var{flag} is
%# @table @option
%# @item  @code{"init"}
%# then @var{t} must be a double column vector of length 2 with the first and the last time step and nothing is returned from this function,
%# @item  @code{""}
%# then @var{t} must be a double scalar specifying the actual time step and the return value is false (resp. value 0) for 'not stop solving',
%# @item  @code{"done"}
%# then @var{t} must be a double scalar specifying the last time step and nothing is returned from this function.
%# @end table
%#
%# This function is called by a ode solver function if it was specified in an options structure with the @command{odeset}. This function is an internal helper function therefore it should never be necessary that this function is called directly by a user. There is only little error detection implemented in this function file to achieve the highest performance.
%#
%# For example, solve an anonymous implementation of the "Van der Pol" equation and display the results while solving
%# @example
%# fvdb = @@(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
%# 
%# opt = odeset ("OutputFcn", @@odeplot, "RelTol", 1e-6);
%# sol = ode45 (fvdb, [0 20], [2 0], opt);
%# @end example
%# @end deftypefn
%#
%# @seealso{odepkg}

function [varargout] = odeplot (t, y, flag, varargin)

  ## No input argument check is done for a higher processing speed
  persistent fig; persistent told; 
  persistent yold; persistent counter;

  if (strcmp (flag, "init")) 
    ## Nothing to return, t is either the time slot [tstart tstop]
    ## or [t0, t1, ..., tn], y is the inital value vector "init"
    fig = figure; told = t(1,1); yold = y(:,1);
    counter = 1;

  elseif (isempty (flag))
    ## Return something in varargout{1}, either false for "not stopping
    ## the integration" or true for "stopping the integration"
    counter = counter + 1; figure (fig);
    told(counter,1) = t(1,1);
    yold(:,counter) = y(:,1);
    plot (told, yold, "-o", "markersize", 1); drawnow;
    varargout{1} = false;

  elseif (strcmp (flag, "done")) 
    ## Cleanup has to be done, clear the persistent variables because
    ## we don't need them anymore
    clear ("figure", "told", "yold", "counter");

  endif

endfunction


%!demo
%! % solve an anonymous implementation of the "Van der Pol" equation
%! % and display the results while solving
%! fvdb = @(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
%! opt = odeset ("OutputFcn", @odeplot, "RelTol", 1e-6);
%! sol = ode45 (fvdb, [0 20], [2 0], opt);
