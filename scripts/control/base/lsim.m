## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{y}, @var{x}] =} lsim (@var{sys}, @var{u}, @var{t}, @var{x0})
## Produce output for a linear simulation of a system; produces 
## a plot for the output of the system, @var{sys}.
##
## @var{u} is an array that contains the system's inputs.  Each row in @var{u}
## corresponds to a different time step.  Each column in @var{u} corresponds to a
## different input.  @var{t} is an array that contains the time index of the
## system; @var{t} should be regularly spaced.  If initial conditions are required
## on the system, the @var{x0} vector should be added to the argument list.
##
## When the lsim function is invoked a plot is not displayed; 
## however, the data is returned in @var{y} (system output)
## and @var{x} (system states).
## @end deftypefn

## Author: David Clem
## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1995
## modified by John Ingram for system format August 1996

function [y, x] = lsim (sys, u, t, x0)

  if((nargin < 3)||(nargin > 4))
    print_usage ();
  endif

  if(!isstruct(sys))
    error("sys must be in system data structure");
  endif

  sys = sysupdate(sys,"ss");

  [ncstates, ndstates, nin, nout] = sysdimensions(sys);
  [a,b,c,d] = sys2ss(sys);

  if (nargin == 3)     x0 = zeros(columns(a),1);        endif

  if(rows(u) ~= length(t))
    error("lsim: There should be an input value (row) for each time instant");
  endif
  if(columns(u) ~= columns(d))
    error("lsim: U and d should have the same number of inputs");
  endif
  if(columns(x0) > 1)
    error("lsim: Initial condition vector should have only one column");
  endif
  if(rows(x0) > rows(a))
    error("lsim: Initial condition vector is too large");
  endif

  Ts = 0;
  t(2)-t(1);
  u=u';
  n = max(size(t));

  for ii = 1:(n-1)

    ## check if step size changed
    ## FIXME -- this is probably not the best test, but it is
    ## better than a test for exact equality.
    if (abs (t(ii+1) - t(ii) - Ts) > 10 * eps)
      Ts = t(ii+1) - t(ii);
      ## [F,G] = c2d(a,b,Ts);
      dsys = c2d(sys, Ts);
      [F,G] = sys2ss(dsys);
    endif

    x(:,ii) = x0;
    x0 = F*x0 + G*u(:,ii);
  endfor

  ## pick up last point
  x(:,n) = x0;

  y = c*x + d*u;
  if(nargout == 0)
   plot(t,y);
   y=[];
   x=[];
  endif
endfunction
