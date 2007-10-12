## Copyright (C) 1996, 1998, 2000 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{mag}, @var{phase}, @var{w}] =} bode (@var{sys}, @var{w}, @var{out_idx}, @var{in_idx})
## If no output arguments are given: produce Bode plots of a system; otherwise,
## compute the frequency response of a system data structure
##
## @strong{Inputs}
## @table @var
## @item   sys
## a system data structure (must be either purely continuous or discrete;
## see is_digital)
## @item   w
## frequency values for evaluation.
##
## if @var{sys} is continuous, then bode evaluates @math{G(jw)} where
## @math{G(s)} is the system transfer function.
##
## if @var{sys} is discrete, then bode evaluates G(@code{exp}(jwT)), where
## @itemize @bullet
## @item @math{T} is the system sampling time
## @item @math{G(z)} is the system transfer function.
## @end itemize
##
## @strong{Default} the default frequency range is selected as follows: (These
## steps are @strong{not} performed if @var{w} is specified)
## @enumerate
## @item via routine __bodquist__, isolate all poles and zeros away from
## @var{w}=0 (@var{jw}=0 or @math{@code{exp}(jwT)}=1) and select the frequency
## range based on the breakpoint locations of the frequencies.
## @item if @var{sys} is discrete time, the frequency range is limited
##               to @math{jwT} in
## @ifinfo
## [0,2 pi /T]
## @end ifinfo
## @iftex
## @tex
## $[0,2\pi/T]$
## @end tex
## @end iftex
## @item A "smoothing" routine is used to ensure that the plot phase does
## not change excessively from point to point and that singular
## points (e.g., crossovers from +/- 180) are accurately shown.
##
## @end enumerate
## @item out_idx
## @itemx in_idx
##
## The names or indices of outputs and inputs to be used in the frequency
## response.  See @code{sysprune}.
##
## @strong{Example}
## @example
## bode(sys,[],"y_3", @{"u_1","u_4"@});
## @end example
## @end table
## @strong{Outputs}
## @table @var
## @item mag
## @itemx phase
## the magnitude and phase of the frequency response @math{G(jw)} or
## @math{G(@code{exp}(jwT))} at the selected frequency values.
## @item w
## the vector of frequency values used
## @end table
##
## @enumerate
## @item If no output arguments are given, e.g.,
## @example
## bode(sys);
## @end example
## bode plots the results to the screen.  Descriptive labels are
## automatically placed.
##
## Failure to include a concluding semicolon will yield some garbage
## being printed to the screen (@code{ans = []}).
##
## @item If the requested plot is for an @acronym{MIMO} system, mag is set to
## @math{||G(jw)||} or @math{||G(@code{exp}(jwT))||}
## and phase information is not computed.
## @end enumerate
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: July 10, 1996
## Based on previous code by R. Bruce Tenison, July 13, 1994
## Modified by David Clem November 13, 1994
## again by A. S. Hodel July 1995 (smart plot range, etc.)
## Modified by Kai P. Mueller September 28, 1997 (multiplot mode)

function [mag_r, phase_r, w_r] = bode (sys, w, outputs, inputs, plot_style)

  ## check number of input arguments given
  if (nargin < 1 || nargin > 5)
    print_usage ();
  endif
  if (nargin < 2)
    w = [];
  endif
  if (nargin < 3)
    outputs = [];
  endif
  if (nargin < 4)
    inputs = [];
  endif
  if (nargin < 5)
    plot_style = "dB";
  endif

  if (strcmp (plot_style, "dB"))
    do_db_plot = 1;
  elseif (strcmp (plot_style, "mag"))
    do_db_plot = 0;
  else
    error ("bode: invalid value of plot_style specified");
  endif

  [f, w, sys] = __bodquist__ (sys, w, outputs, inputs, "bode");
  bode_nin = sysdimensions (sys, "in");
  bode_nout = sysdimensions (sys, "out");

  [stname, inname, outname] = sysgetsignals (sys);
  systsam = sysgettsam (sys);

  ## Get the magnitude and phase of f.
  mag = abs (f);
  phase = unwrap (arg (f)) * 180.0 / pi;

  if (nargout < 1),
    ## Plot the information
    if (is_digital (sys))
      xlstr = sprintf ("Digital frequency w=rad/sec.  pi/T=%g", pi/systsam);
      tistr = "(exp(jwT)) ";
    else
      xlstr = "Frequency in rad/sec";
      tistr = "(jw)";
    endif

    wv = [min(w), max(w)];

    is_siso_sys = is_siso (sys);
    max_mag_positive = max (mag) > 0;

    if (is_siso_sys)
      subplot (2, 1, 1);
    endif

    if (do_db_plot)
      md = 20 * log10 (mag);
      semilogx (w, md);
      if (max_mag_positive)
	ylabel ("Gain in dB");
	axvec = axis2dlim ([w(:), md(:)]);
	axvec(1:2) = wv;
	axis (axvec);
      endif
    else
      loglog (w, mag);
      ylabel ("Gain |Y/U|")
    endif
    xlabel (xlstr);
    grid ("on");

    if (is_siso_sys)
      title (sprintf ("|[Y/U]%s|, u=%s, y=%s", tistr, inname{1}, outname{1}));
    else
      title (sprintf ("||Y(%s)/U(%s)||", tistr, tistr));
      disp ("MIMO plot from")
      disp (__outlist__(inname,"     "));
      disp ("to")
      disp (__outlist__(outname,"    "));
    endif

    if (is_siso_sys)
      subplot (2, 1, 2);
      axvec = axis2dlim ([w(:), phase(:)]);
      axvec(1:2) = wv;
      semilogx (w, phase);
      axis (axvec);
      xlabel (xlstr);
      ylabel ("Phase in deg");
      title (sprintf ("phase([Y/U]%s), u=%s, y=%s",
		      tistr, inname{1}, outname{1}));
      grid ("on");
    endif
  else
    mag_r = mag;
    phase_r = phase;
    w_r = w;
  endif

endfunction
