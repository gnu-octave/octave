## Copyright (C) 1998 Auburn University.  All Rights Reserved
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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
## [mag,phase,w] = nichols(sys[,w,outputs,inputs])
## Produce Nichols plot of a system
##
## Compute the frequency response of a system.
## inputs:
##   sys: system data structure (must be either purely continuous or discrete;
##	 see is_digital)
##   w: frequency values for evaluation.
##      if sys is continuous, then nichols evaluates G(jw)
##      if sys is discrete, then nichols evaluates G(exp(jwT)), where T=sys.tsam
##         (the system sampling time)
##      default: the default frequency range is selected as follows: (These
##        steps are NOT performed if w is specified)
##          (1) via routine bodquist, isolate all poles and zeros away from
##              w=0 (jw=0 or exp(jwT)=1) and select the frequency
##             range based on the breakpoint locations of the frequencies.
##          (2) if sys is discrete time, the frequency range is limited
##              to jwT in [0,2p*pi]
##          (3) A "smoothing" routine is used to ensure that the plot phase does
##              not change excessively from point to point and that singular
##              points (e.g., crossovers from +/- 180) are accurately shown.
##   outputs, inputs: the indices of the output(s) and input(s) to be used in
##     the frequency response; see sysprune.
## outputs:
##    mag, phase: the magnitude and phase of the frequency response
##       G(jw) or G(exp(jwT)) at the selected frequency values.
##    w: the vector of frequency values used
## If no output arguments are given, nichols plots the results to the screen.
## Descriptive labels are automatically placed.  See xlabel, ylable, title,
## and replot.
##
## Note: if the requested plot is for an MIMO system, mag is set to
## ||G(jw)|| or ||G(exp(jwT))|| and phase information is not computed.

function [mag,phase,w] = nichols(sys,w,outputs,inputs)

  ## check number of input arguments given
  if (nargin < 1 | nargin > 4)
    usage("[mag,phase,w] = nichols(sys[,w,outputs,inputs])");
  endif
  if(nargin < 2)
    w = [];
  endif
  if(nargin < 3)
    outputs = [];
  endif
  if(nargin < 4)
    inputs = [];
  endif

  [f, w] = bodquist(sys,w,outputs,inputs,"nichols");

  [stname,inname,outname] = sysgetsignals(sys);
  systsam = sysgettsam(sys);

  ## Get the magnitude and phase of f.
  mag = abs(f);
  phase = arg(f)*180.0/pi;

  if (nargout < 1),
    ## Plot the information
    if(gnuplot_has_multiplot)
      oneplot();
    endif
    gset autoscale;
    if(gnuplot_has_multiplot)
      gset nokey;
    endif
    clearplot();
    grid("on");
    gset data style lines;
    if(is_digital(sys))
      tistr = "(exp(jwT)) ";
    else
      tistr = "(jw)";
    endif
    xlabel("Phase (deg)");
    if(is_siso(sys))
      title(["Nichols plot of |[Y/U]",tistr,"|, u=", ...
	sysgetsignals(sys,"in",1,1), ", y=",sysgetsignals(sys,"out",1,1)]);
    else
      title([ "||Y(", tistr, ")/U(", tistr, ")||"]);
      printf("MIMO plot from\n%s\nto\n%s\n",outlist(inname,"	"), ...
        outlist(outname,"	"));
    endif
    if(max(mag) > 0)
      ylabel("Gain in dB");
      md = 20*log10(mag);
    else
      ylabel("Gain |Y/U|")
      md = mag;
    endif

    axvec = axis2dlim([vec(phase),vec(md)]);
    axis(axvec);
    plot(phase,md);
    mag = phase = w = [];
  endif
endfunction
