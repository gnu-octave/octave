## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{f}, @var{w}, @var{rsys}] =} __bodquist__ (@var{sys}, @var{w}, @var{out_idx}, @var{in_idx})
## Used internally by @command{bode}, @command{nyquist}; compute system frequency response.
##
## @strong{Inputs}
## @table @var
## @item sys
## input system structure
## @item w
## range of frequencies; empty if user wants default
## @item out_idx
## @itemx in_idx
## names or indices of output/input signal names; empty if user wants all
## @item rname
## name of routine that called __bodquist__ ("bode", "nyquist", or "nichols")
## @end table
## @strong{Outputs}
## @table @var
## @item w
## list of frequencies
## @item f
## frequency response of sys; @math{f(ii) = f(omega(ii))}
## @item rsys
## system with selected inputs and outputs
## @end table
##
## @code{bode}, @code{nichols}, and @code{nyquist} share the same 
## introduction, so the common parts are
## in __bodquist__.  It contains the part that finds the number of arguments,
## determines whether or not the system is @acronym{SISO}, and computes the frequency
## response.  Only the way the response is plotted is different between the
## these functions.
## @end deftypefn

function [f, w, rsys] = __bodquist__ (sys, w, outputs, inputs, rname)

  ## check number of input arguments given
  if (nargin != 5)
    usage ("[f, w] = __bodquist__ (sys, w, outputs, inputs, rname)");
  endif

  ## check each argument to see if it's in the correct form
  if (!isstruct(sys))
    error("sys must be a system data structure");
  endif

  ## let __freqresp__ determine w if it's not already given
  USEW = freqchkw(w);

  ## get initial dimensions (revised below if sysprune is called)
  [nn,nz,mm,pp ] = sysdimensions(sys);

  ## check for an output vector and to see whether it`s correct
  if (!isempty(outputs))
    if (isempty(inputs))
      inputs = 1:mm;                    # use all inputs
      warning([rname,": outputs specified but not inputs"]);
    elseif(is_signal_list(inputs) | ischar(inputs))
      inputs = sysidx(sys,"in",inputs);
    endif
    if(is_signal_list(outputs) | ischar(outputs))
      outputs = sysidx(sys,"out",outputs);
    end
    sys = sysprune(sys,outputs,inputs);
    [nn,nz,mm,pp ] = sysdimensions(sys);
  endif

  ## for speed in computation, convert local copy of
  ## SISO state space systems to zero-pole  form
  if( is_siso(sys) & strcmp( sysgettype(sys), "ss") )
    [zer,pol,k,tsam,inname,outname] = sys2zp(sys);
    sys = zp(zer,pol,k,tsam,inname,outname);
  endif

  ## get system frequency response
  [f, w] = __freqresp__ (sys, USEW, w);

  phase = arg(f)*180.0/pi;

  if(!USEW)
    ## smooth plots
    pcnt = 5;           # max number of refinement steps
    dphase = 5;         # desired max change in phase
    dmag = 0.2;         # desired max change in magnitude
    while(pcnt)
      pd = abs(diff(phase));                    # phase variation
      pdbig = find(pd > dphase);

      lp = length(f);  lp1 = lp-1;              # relative variation
      fd = abs(diff(f));
      fm = max(abs([f(1:lp1); f(2:lp)]));
      fdbig = find(fd > fm/10);

      bigpts = union(fdbig, pdbig);

      if(isempty(bigpts) )
        pcnt = 0;
      else
        pcnt = pcnt - 1;
        wnew = [];
        crossover_points = find ( phase(1:lp1).*phase(2:lp) < 0);
        pd(crossover_points) = abs(359.99+dphase - pd(crossover_points));
        np_pts = max(3,ceil(pd/dphase)+2);              # phase points
        nm_pts = max(3,ceil(log(fd./fm)/log(dmag))+2);  # magnitude points
        npts = min(5,max(np_pts, nm_pts));

        w1 = log10(w(1:lp1));
        w2 = log10(w(2:lp));
        for ii=bigpts
          if(npts(ii))
            wtmp = logspace(w1(ii),w2(ii),npts(ii));
            wseg(ii,1:(npts(ii)-2)) = wtmp(2:(npts(ii)-1));
          endif
        endfor
        wnew = vec(wseg)'; # make a row vector
        wnew = wnew(find(wnew != 0));
        wnew = sort(wnew);
        wnew = create_set(wnew);
        if(isempty(wnew))   # all small crossovers
          pcnt = 0;
        else
	  ## get new freq resp points, combine with old, and sort.
          [fnew, wnew] = __freqresp__ (sys, 1, wnew);
          w = [w, wnew];
          f = [f, fnew];
          [w, idx] = sort (w);
          f = f (idx);
          phase = arg(f)*180.0/pi;
        endif
      endif
    endwhile
  endif

  ## ensure unique frequency values
  [w,idx] = sort(w);
  f = f(idx);

  w_diff = diff(w);
  w_dup = find(w_diff == 0);
  w_idx = complement(w_dup,1:length(w));
  w = w(w_idx);
  f = f(w_idx);

  ## set rsys to pruned system
  rsys = sys;

endfunction
