# Copyright (C) 1996,1998 A. Scottedward Hodel 
#
# This file is part of Octave. 
#
# Octave is free software; you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation; either version 2, or (at your option) any 
# later version. 
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for more details.
# 
# You should have received a copy of the GNU General Public License 
# along with Octave; see the file COPYING.  If not, write to the Free 
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 
 
function [f,w] = bodquist(sys,w,outputs,inputs,rname)
# [f,w] = bodquist(sys,w,outputs,inputs)
# used by bode, nyquist
# inputs:
#   sys: input system structure
#   w: range of frequencies; empty if user wants default
#   outputs:list of outputs; empty if user wants all
#   inputs: list of inputs; empty if user wants all
#   rname: name of routine that called bodquist ("bode" or "nyquist")
# outputs:
#   w: list of frequencies 
#   f: frequency response of sys; f(ii) = f(omega(ii))
#
# Both bode and nyquist share the same introduction, so the common parts are 
# in this file bodquist.m.  It contains the part that finds the 
# number of arguments, determines whether or not the system is SISO, and 
# computes the frequency response.  Only the way the response is plotted is
# different between the two functions.

  # check number of input arguments given
  if (nargin != 5)
    usage("[f,w] = bodquist(sys,w,outputs,inputs,rname)");
  endif

  # check each argument to see if it's in the correct form
  if (!is_struct(sys))
    error("sys must be a system data structure");
  endif
	
  # let freqresp determine w if it's not already given
  USEW = freqchkw(w);

  # get initial dimensions (revised below if sysprune is called)
  [nn,nz,mm,pp ] = sysdimensions(sys);

  # check for an output vector and to see whether it`s correct
  if (!isempty(outputs))
    if (isempty(inputs))
      inputs = 1:mm;			# use all inputs
      warning([rname,": outputs specified but not inputs"]);
    endif
    sys = sysprune(sys,outputs,inputs);
    [nn,nz,mm,pp ] = sysdimensions(sys);
  endif

  # for speed in computation, convert local copy of 
  # SISO state space systems to zero-pole  form
  if( is_siso(sys) & strcmp( sysgettype(sys), "ss") )
    [zer,pol,k,tsam,inname,outname] = sys2zp(sys);
    sys = zp2sys(zer,pol,k,tsam,inname,outname);
  endif

  # get system frequency response
  [f,w] = freqresp(sys,USEW,w);   

  phase = arg(f)*180.0/pi;

  if(!USEW)
    # smooth plots
    pcnt = 5;		# max number of refinement steps
    dphase = 5;		# desired max change in phase
    dmag = 0.2;		# desired max change in magnitude
    while(pcnt)
      pd = abs(diff(phase));			# phase variation
      pdbig = vec(find(pd > dphase));

      lp = length(f);  lp1 = lp-1;		# relative variation
      fd = abs(diff(f));
      fm = max(abs([f(1:lp1); f(2:lp)]));
      fdbig = vec(find(fd > fm/10));

      bigpts = union(fdbig, pdbig);

      if(isempty(bigpts) )
        pcnt = 0;
      else
        pcnt = pcnt - 1;
        wnew = [];
        crossover_points = find ( phase(1:lp1).*phase(2:lp) < 0);
        pd(crossover_points) = abs(359.99+dphase - pd(crossover_points));
        np_pts = ceil(pd/dphase)+2;		# phase points
        nm_pts = ceil(log(fd./fm)/log(dmag))+2; 	# magnitude points
        npts = min(5,max(np_pts, nm_pts));
        w1 = log10(w(1:lp1));
        w2 = log10(w(2:lp));
        for ii=bigpts
          if(npts(ii))
            wseg(ii,1:npts(ii)) = logspace(w1(ii),w2(ii),npts(ii));
          endif
        endfor
        wnew = reshape(wseg,1,rows(wseg)*columns(wseg)); # make a row vector
        wnew = wnew(find(wnew != 0));
        wnew = sort(wnew);
        wnew = create_set(wnew);
        if(isempty(wnew))   # all small crossovers
          pcnt = 0;
        else
          [fnew,wnew] = freqresp(sys,1,wnew);    # get new freq resp points
          w = [w,wnew];			# combine with old freq resp
          f = [f,fnew];
          [w,idx] = sort(w);		# sort into order
          f = f(idx);
          phase = arg(f)*180.0/pi;
        endif
      endif
    endwhile
  endif
    
endfunction
