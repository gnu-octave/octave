# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved
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
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
function [mag,phase,w] = bode(sys,w,outputs,inputs,plot_style)
# [mag,phase,w] = bode(sys[,w,outputs,inputs,plot_style])
# Produce Bode plots of a system
#
# Compute the frequency response of a system.
# inputs:
#   sys: system data structure (must be either purely continuous or discrete;
#	 see is_digital)
#   w: frequency values for evaluation.
#      if sys is continuous, then bode evaluates G(jw)
#      if sys is discrete, then bode evaluates G(exp(jwT)), where T=sys.tsam
#         (the system sampling time)
#      default: the default frequency range is selected as follows: (These
#        steps are NOT performed if w is specified)
#          (1) via routine bodquist, isolate all poles and zeros away from
#              w=0 (jw=0 or exp(jwT)=1) and select the frequency
#             range based on the breakpoint locations of the frequencies.
#          (2) if sys is discrete time, the frequency range is limited
#              to jwT in [0,2p*pi]
#          (3) A "smoothing" routine is used to ensure that the plot phase does
#              not change excessively from point to point and that singular
#              points (e.g., crossovers from +/- 180) are accurately shown.
#   outputs, inputs: the indices of the output(s) and input(s) to be used in
#     the frequency response; see sysprune.
#   plot_style: An optional argument specifying the type of plot to
#               produce (if plotting is being done).  Valid values are
#               "dB" or "mag".  If omitted, "dB" is assumed.
#
# outputs:
#    mag, phase: the magnitude and phase of the frequency response
#       G(jw) or G(exp(jwT)) at the selected frequency values.
#    w: the vector of frequency values used
# If no output arguments are given, bode plots the results to the screen.
# Descriptive labels are automatically placed.  See xlabel, ylable, title,
# and replot.
#
# Note: if the requested plot is for an MIMO system, mag is set to
# ||G(jw)|| or ||G(exp(jwT))|| and phase information is not computed.

# Written by John Ingram  July 10th, 1996
# Based on previous code
# By R. Bruce Tenison, July 13, 1994
# Modified by David Clem November 13, 1994
# again by A. S. Hodel July 1995 (smart plot range, etc.)
# Modified by Kai P. Mueller September 28, 1997 (multiplot mode)

  # check number of input arguments given
  if (nargin < 1 | nargin > 5)
    usage("[mag,phase,w] = bode(sys[,w,outputs,inputs,plot_style])");
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
  if(nargin < 5)
    plot_style = "dB";
  endif

  if (strcmp (plot_style, "dB"))
    do_db_plot = 1;
  elseif (strcmp (plot_style, "mag"))
    do_db_plot = 0;
  else
    error ("bode: invalid value of plot_style specified");
  endif

  [f, w] = bodquist(sys,w,outputs,inputs,"bode");

  [stname,inname,outname] = sysgetsignals(sys);
  systsam = sysgettsam(sys);

  # Get the magnitude and phase of f.
  mag = abs(f);
  phase = arg(f)*180.0/pi;

  if (nargout < 1),
    # Plot the information
    if(gnuplot_has_multiplot)
      oneplot();
    endif
    gset autoscale;
    if(gnuplot_has_multiplot)
      gset nokey;
    endif
    clearplot();
    gset data style lines;
    if(is_digital(sys))
      xlstr = ["Digital frequency w=rad/sec.  pi/T=",num2str(pi/systsam)];
      tistr = "(exp(jwT)) ";
    else
      xlstr = "Frequency in rad/sec";
      tistr = "(jw)";
    endif
    xlabel(xlstr);
    if(is_siso(sys))
      if (gnuplot_has_multiplot)
        subplot(2,1,1);
      endif
      title(["|[Y/U]",tistr,"|, u=", nth(inname,1),", y=",nth(outname,1)]);
    else
      title([ "||Y(", tistr, ")/U(", tistr, ")||"]);
      disp("MIMO plot from")
      disp(outlist(inname,"	"));
      disp("to")
      disp(outlist(outname,"	"));
    endif
    wv = [min(w), max(w)];
    if(do_db_plot && max(mag) > 0)
      ylabel("Gain in dB");
      md = 20*log10(mag);
    else
      ylabel("Gain |Y/U|")
      md = mag;
    endif

    axvec = axis2dlim([vec(w),vec(md)]);
    axvec(1:2) = wv;
    axis(axvec);
    grid("on");
    if (do_db_plot)
      semilogx(w,md);
    else
      loglog(w,md);
    endif
    if (is_siso(sys))
      if (gnuplot_has_multiplot)
        subplot(2,1,2);
      else
        prompt('Press any key for phase plot');
      endif
      axvec = axis2dlim([vec(w),vec(phase)]);
      axvec(1:2) = wv;
      axis(axvec);
      xlabel(xlstr);
      ylabel("Phase in deg");
      title([ "phase([Y/U]", tistr, ...
	 "), u=", nth(inname,1),", y=",nth(outname,1)]);
      grid("on");
      semilogx(w,phase);
      # This should be the default for subsequent plot commands.
      if(gnuplot_has_multiplot)
        oneplot();
      endif
    endif
    mag = phase = w = [];
  endif
endfunction
