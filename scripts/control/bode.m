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
 
function [mag,phase,w] = bode(sys,w,outputs,inputs)
# [mag,phase,w] = bode(sys[,w,outputs,inputs])
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
# $Revision: 2.0.0.2 $
# $Log: bode.m,v $
# Revision 2.0.0.2  1998/12/08  23:29:23  hodel
# Octave-Marsyas Interface updated for signals-as-lists
#
# Revision 2.0.0.1  1998/12/08  21:40:45  hodel
# Dummy version to match ftp.eng.auburn.edu version number
#
# Revision 2.0.0.0  1998/12/08  21:36:52  hodel
# Branch for beta release patches
#
# Revision 2.0  1998/12/08  21:34:57  hodel
# Initial beta release of signals-as-lists rewrite;
# sysdimensions now takes opt as an argument
#
# Revision 2.0.0.1  1998/12/08  20:54:19  hodel
# sysdimensions takes opt parameter now
#
# Revision 2.0.0.0  1998/12/08  20:30:09  hodel
# beta release revision
#
# Revision 2.0  1998/12/08  20:27:56  hodel
# Initial list rewrite of OCST
#
# Revision 1.7  1998/10/21 12:46:59  hodelas
# moved grid command so that grid appears in plots
#
# Revision 1.6  1998/09/04 20:57:18  hodelas
# fixed bodquist bug (use reshape instead of  transpose); removed extraneous
# output from bode.
#
# Bodquist is now much faster
#
# Revision 1.4  1998/08/24 15:50:03  hodelas
# updated documentation
#
# Revision 1.3  1998/08/13 20:11:27  hodelas
# Added calls to axis2dlim for flat-plots
#
# Revision 1.2  1998/07/24 18:16:51  hodelas
# rewrote bodquist as a function call.  nyquist interactive plot now optional
#
# Revision 1.1.1.1  1998/05/19 20:24:05  jwe
#
# Revision 1.7  1998/02/09 13:04:11  scotte
# fixed oneplot/gset nokey to function only if gnuplot_has_multiplot
#
# Revision 1.6  1997/12/01 16:51:50  scotte
# updated by Mueller 27 Nov 97
#
# Revision 1.2  1997/11/24  18:53:01  mueller
# gset autoscale prevents the following error message:
#    line 0: x range must be greater than 0 for log scale!
# gset nokey and call to oneplot() added
#
# Revision 1.1  1997/11/11  17:31:27  mueller
# Initial revision
#

  # check number of input arguments given
  if (nargin < 1 | nargin > 4)
    usage("[mag,phase,w] = bode(sys[,w,outputs,inputs])");
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
    ylabel("Gain in dB");
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
    md = 20*log10(mag);

    axvec = axis2dlim([vec(w),vec(md)]);
    axvec(1:2) = wv;
    axis(axvec);
    grid("on");
    semilogx(w,md);
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
