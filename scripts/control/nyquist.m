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
 
function [realp,imagp,w] = nyquist(sys,w,outputs,inputs,atol)
# [realp,imagp,w] = nyquist(sys[,w,outputs,inputs,atol])
# Produce Nyquist plots of a system
#
# Compute the frequency response of a system.
# inputs: (pass as empty to get default values
#   sys: system data structure (must be either purely continuous or discrete;
#        see is_digital)
#   w: frequency values for evaluation.
#      if sys is continuous, then bode evaluates G(jw)
#      if sys is discrete, then bode evaluates G(exp(jwT)), where 
#         T=sysgettsam(sys) (the system sampling time)
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
#   atol: for interactive nyquist plots: atol is a change-in-angle tolerance 
#     (in degrees) for the of asymptotes (default = 0; 1e-2 is a good choice).
#     Consecutive points along the asymptotes whose angle is within atol of
#     the angle between the largest two points are omitted for "zooming in"
#
# outputs:
#    realp, imagp: the real and imaginary parts of the frequency response
#       G(jw) or G(exp(jwT)) at the selected frequency values.
#    w: the vector of frequency values used
#
# If no output arguments are given, nyquist plots the results to the screen.
# If atol != 0 and asymptotes are detected then the user is asked 
#    interactively if they wish to zoom in (remove asymptotes)
# Descriptive labels are automatically placed.  See xlabel, ylable, title,
# and replot.
#
# Note: if the requested plot is for an MIMO system, a warning message is
# presented; the returned information is of the magnitude 
# ||G(jw)|| or ||G(exp(jwT))|| only; phase information is not computed.
   
  # By R. Bruce Tenison, July 13, 1994
  # A. S. Hodel July 1995 (adaptive frequency spacing, 
  #     remove acura parameter, etc.)
  # Revised by John Ingram July 1996 for system format
  #

  # Both bode and nyquist share the same introduction, so the common parts are 
  # in a file called bodquist.m.  It contains the part that finds the 
  # number of arguments, determines whether or not the system is SISO, and 
  # computes the frequency response.  Only the way the response is plotted is
  # different between the two functions.

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  # check number of input arguments given
  if (nargin < 1 | nargin > 5)
    usage("[realp,imagp,w] = nyquist(sys[,w,outputs,inputs,atol])");
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
    atol = 0;
  elseif(!(is_sample(atol) | atol == 0))
    error("atol must be a nonnegative scalar.")
  endif

  # signal to bodquist who's calling
   
  [f,w] = bodquist(sys,w,outputs,inputs,"nyquist");

  # Get the real and imaginary part of f.
  realp = real(f);
  imagp = imag(f);

  # No output arguments, then display plot, otherwise return data.
  if (nargout == 0)
    dnplot = 0;
    while(!dnplot)
      if(gnuplot_has_multiplot)
        oneplot();
        gset key;
      endif
      clearplot();
      grid ("on");
      gset data style lines;
  
      if(is_digital(sys))
        tstr = " G(e^{jw}) ";
      else
        tstr = " G(jw) ";
      endif
      xlabel(["Re(",tstr,")"]);
      ylabel(["Im(",tstr,")"]);
  
      [stn, inn, outn] = sysgetsignals(sys);
      if(is_siso(sys))
        title(sprintf("Nyquist plot from %s to %s, w (rad/s) in [%e, %e]", ...
	  nth(inn,1), nth(outn,1), w(1), w(length(w))) )
      endif
  
      gset nologscale xy;

      axis(axis2dlim([[vec(realp),vec(imagp)];[vec(realp),-vec(imagp)]]));
      plot(realp,imagp,"- ;+w;",realp,-imagp,"-@ ;-w;");

      # check for interactive plots
      dnplot = 1; # assume done; will change later if atol is satisfied
      if(atol > 0 & length(f) > 2)

        # check for asymptotes
        fmax = max(abs(f));
        fi = max(find(abs(f) == fmax));
        
        # compute angles from point to point
        df = diff(f);
        th = atan2(real(df),imag(df))*180/pi;

        # get angle at fmax
        if(fi == length(f)) fi = fi-1; endif
        thm = th(fi);
    
        # now locate consecutive angles within atol of thm
        ith_same = find(abs(th - thm) < atol);
        ichk = union(fi,find(diff(ith_same) == 1));

        #locate max, min consecutive indices in ichk
        loval = max(complement(ichk,1:fi));
        if(isempty(loval)) loval = fi;
        else               loval = loval + 1;   endif

        hival = min(complement(ichk,fi:length(th)));
        if(isempty(hival))  hival = fi+1;      endif

        keep_idx = complement(loval:hival,1:length(w));

        if(length(keep_idx))
          resp = input("Remove asymptotes and zoom in (y or n): ",1);
          if(resp(1) == "y")
            dnplot = 0;                 # plot again
            w = w(keep_idx);
            f = f(keep_idx);
            realp = real(f);
            imagp = imag(f);
          endif
        endif

     endif
    endwhile
    w = [];
    realp=[];
    imagp=[];
  endif

  implicit_str_to_num_ok = save_val;	# restore value

endfunction
