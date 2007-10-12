## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{y}, @var{t}] =} __stepimp__ (@var{sitype}, @var{sys} [, @var{inp}, @var{tstop}, @var{n}])
## Impulse or step response for a linear system.
## The system can be discrete or multivariable (or both).
## This m-file contains the ``common code'' of step and impulse.
##
## Produces a plot or the response data for system @var{sys}.
##
## Limited argument checking; ``do not attempt to do this at home''.
## Used internally in @command{impulse}, @command{step}. Use @command{step}
## or @command{impulse} instead.
## @seealso{step, impulse}
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: October 2, 1997
## based on lsim.m of Scottedward Hodel

function [y, t] = __stepimp__ (sitype, sys, inp, tstop, n)

  if (sitype == 1)
    IMPULSE = 0;
  elseif (sitype == 2)
    IMPULSE = 1;
  else
    error ("__stepimp__: invalid sitype argument");
  endif
  sys = sysupdate (sys, "ss");

  USE_DEF = 0;   # default tstop and n if we have to give up
  N_MIN = 50;    # minimum number of points
  N_MAX = 2000;  # maximum number of points
  T_DEF = 10.0;  # default simulation time

  ## collect useful information about the system
  [ncstates, ndstates, NIN, NOUT] = sysdimensions (sys);
  TSAMPLE = sysgettsam (sys);

  if (nargin < 3)
    inp = 1;
  elseif (inp < 1 || inp > NIN)
    error ("__stepimp__: argument inp out of range");
  endif

  DIGITAL = is_digital (sys);
  if (DIGITAL)
    NSTATES = ndstates;
    if (TSAMPLE < eps)
      error ("__stepimp__: sampling time of discrete system too small")
    endif
  else
    NSTATES = ncstates;
  endif
  if (NSTATES < 1)
    error ("__stepimp__: pure gain block (n_states < 1), step response is trivial");
  endif
  if (nargin < 5)
    ## we have to compute the time when the system reaches steady state
    ## and the step size
    ev = eig (sys2ss (sys));
    if (DIGITAL)
      ## perform bilinear transformation on poles in z
      for i = 1:NSTATES
        pole = ev(i);
        if (abs(pole + 1) < 1.0e-10)
          ev(i) = 0;
        else
          ev(i) = 2 / TSAMPLE * (pole - 1) / (pole + 1);
        endif
      endfor
    endif
    ## remove poles near zero from eigenvalue array ev
    nk = NSTATES;
    for i = 1:NSTATES
      if (abs (real (ev(i))) < 1.0e-10)
        ev(i) = 0;
        nk = nk - 1;
      endif
    endfor
    if (nk == 0)
      USE_DEF = 1;
      ## printf("##STEPIMP-DEBUG: using defaults.\n");
    else
      ev = ev(find (ev));
      x = max (abs (ev));
      t_step = 0.2 * pi / x;
      x = min (abs (real (ev)));
      t_sim = 5.0 / x;
      ## round up
      yy = 10^(ceil (log10 (t_sim)) - 1);
      t_sim = yy * ceil (t_sim / yy);
      ## printf("##STEPIMP-DEBUG: nk=%d   t_step=%f  t_sim=%f\n",
      ##   nk, t_step, t_sim);
    endif
  endif

  if (DIGITAL)
    ## ---- sampled system
    if (nargin == 5)
      n = round (n);
      if (n < 2)
        error ("__stepimp__: n must not be less than 2.")
      endif
    else
      if (nargin == 4)
        ## n is unknown
      elseif (nargin >= 1)
        ## tstop and n are unknown
        if (USE_DEF)
          tstop = (N_MIN - 1) * TSAMPLE;
        else
          tstop = t_sim;
        endif
      endif
      n = floor (tstop / TSAMPLE) + 1;
      if (n < 2)
	n = 2;
      endif
      if (n > N_MAX)
        n = N_MAX;
        printf ("Hint: number of samples limited to %d by default.\n", \
		N_MAX);
        printf ("  ==> increase \"n\" parameter for longer simulations.\n");
      endif
    endif
    tstop = (n - 1) * TSAMPLE;
    t_step = TSAMPLE;
  else
    ## ---- continuous system
    if (nargin == 5)
      n = round (n);
      if (n < 2)
        error("step: n must not be less than 2.")
      endif
      t_step = tstop / (n - 1);
    else
      if (nargin == 4)
        ## only n in unknown
        if (USE_DEF)
          n = N_MIN;
          t_step = tstop / (n - 1);
        else
          n = floor (tstop / t_step) + 1;
        endif
      else
        ## tstop and n are unknown
        if (USE_DEF)
          tstop = T_DEF;
          n = N_MIN;
          t_step = tstop / (n - 1);
        else
          tstop = t_sim;
          n = floor (tstop / t_step) + 1;
        endif
      endif
      if (n < N_MIN)
        n = N_MIN;
        t_step = tstop / (n - 1);
      endif
      if (n > N_MAX)
        tstop = (n - 1) * t_step;
        t_step = tstop / (N_MAX - 1);
        n = N_MAX;
      endif
    endif
    tstop = (n - 1) * t_step;
    [jnk,B] = sys2ss (sys);
    B = B(:,inp);
    sys = c2d (sys, t_step);
  endif
  ## printf("##STEPIMP-DEBUG: t_step=%f n=%d  tstop=%f\n", t_step, n, tstop);

  F = sys.a;
  G = sys.b(:,inp);
  C = sys.c;
  D = sys.d(:,inp);
  y = zeros (NOUT, n);
  t = linspace (0, tstop, n);

  if (IMPULSE)
    if (! DIGITAL && D'*D > 0)
      error ("impulse: D matrix is nonzero, impulse response infinite.")
    endif
    if (DIGITAL)
      y(:,1) = D / t_step;
      x = G / t_step;
    else
      x = B;
      y(:,1) = C * x;
      x = F * x;
    endif
    for i = 2:n
      y(:,i) = C * x;
      x = F * x;
    endfor
    if (DIGITAL)
      y *= t_step; 
    endif 
  else
    x = zeros (NSTATES, 1);
    for i = 1:n
      y(:,i) = C * x + D;
      x = F * x + G;
    endfor
  endif
  
  if (nargout == 0)
    if (IMPULSE)
      gm = zeros (NOUT, 1);
      tt = "impulse";
    else
      ssys = ss (F, G, C, D, t_step);
      gm = dcgain (ssys);
      tt = "step";
    endif
    ncols = floor (sqrt (NOUT));
    nrows = ceil (NOUT / ncols);
    for i = 1:NOUT
      subplot (nrows, ncols, i);
      if (DIGITAL)
	[ts, ys] = stairs (t, y(i,:));
	ts = ts(1:2*n-2)';
	ys = ys(1:2*n-2)';
	if (length (gm) > 0)
	  yy = [ys; gm(i)*ones(size(ts))];
	else
	  yy = ys;
	endif
	plot (ts, yy);
	grid ("on");
	xlabel ("time [s]");
	ylabel ("y(t)");
      else
	if (length (gm) > 0)
	  yy = [y(i,:); gm(i)*ones(size(t))];
	else
	  yy = y(i,:);
	endif
	plot (t, yy);
	grid ("on");
	xlabel ("time [s]");
	ylabel ("y(t)");
      endif
      title (sprintf ("%s: | %s -> %s", tt,
		      sysgetsignals (sys, "in", inp, 1),
		      sysgetsignals (sys, "out", i, 1)));
    endfor
    y = [];
    t = [];
  endif
  ## printf("##STEPIMP-DEBUG: gratulations, successfull completion.\n");
endfunction  
