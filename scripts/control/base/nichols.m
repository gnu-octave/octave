## Copyright (C) 1998, 2000, 2003, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{mag}, @var{phase}, @var{w}] =} nichols (@var{sys}, @var{w}, @var{outputs}, @var{inputs})
## Produce Nichols plot of a system.
##
## @strong{Inputs}
## @table @var
## @item sys
## System data structure (must be either purely continuous or discrete; 
## see @command{is_digital}).
## @item w
## Frequency values for evaluation.
## @itemize
## @item if sys is continuous, then nichols evaluates @math{G(jw)}.
## @item if sys is discrete, then nichols evaluates @math{G(exp(jwT))}, 
## where @var{T}=@var{sys}. @var{tsam} is the system sampling time.
## @item the default frequency range is selected as follows (These
##        steps are @strong{not} performed if @var{w} is specified):
## @enumerate
## @item via routine @command{__bodquist__}, isolate all poles and zeros away from
## @var{w}=0 (@math{jw=0} or @math{exp(jwT)=1}) and select the frequency range 
## based on the breakpoint locations of the frequencies.
## @item if sys is discrete time, the frequency range is limited to jwT in 
## @iftex
## @tex
## $ [0, 2p\pi] $.
## @end tex
## @end iftex
## @ifinfo
## [0,2p*pi].
## @end ifinfo
## @item A ``smoothing'' routine is used to ensure that the plot phase does
## not change excessively from point to point and that singular points 
## (e.g., crossovers from +/- 180) are accurately shown.
## @end enumerate
## @end itemize
## @item outputs
## @itemx inputs
## the names or indices of the output(s) and input(s) to be used in the 
## frequency response; see @command{sysprune}.
## @end table
## @strong{Outputs}
## @table @var
## @item mag
## @itemx phase
## The magnitude and phase of the frequency response @math{G(jw)} or 
## @math{G(exp(jwT))} at the selected frequency values.
## @item w
## The vector of frequency values used.
## @end table
## If no output arguments are given, @command{nichols} plots the results to the screen.
## Descriptive labels are automatically placed. See @command{xlabel}, 
## @command{ylabel}, and @command{title}.
##
## Note: if the requested plot is for an @acronym{MIMO} system, @var{mag} is set to
## @iftex
## @tex
## $ \Vert G(jw) \Vert $ or $ \Vert G( {\rm exp}(jwT) \Vert $
## @end tex
## @end iftex
## @ifinfo
## ||G(jw)|| or ||G(exp(jwT))||
## @end ifinfo
## and phase information is not computed.
## @end deftypefn

function [mag2, phase2, w2] = nichols (sys, w, outputs, inputs)

  ## check number of input arguments given
  if (nargin < 1 || nargin > 4)
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

  [f, w, sys] = __bodquist__ (sys, w, outputs, inputs, "nichols");

  [stname, inname, outname] = sysgetsignals (sys);
  systsam = sysgettsam (sys);

  ## Get the magnitude and phase of f.
  mag = abs (f);
  phase = arg (f)*180.0/pi;

  if (nargout < 1),
    ## Plot the information

    if (max (mag) > 0)
      md = 20 * log10 (mag);
      plot (phase, md);
      ylabel ("Gain in dB");
    else
      md = mag;
      plot (phase, md);
      ylabel ("Gain |Y/U|")
    endif

    grid ("on");

    if (is_digital (sys))
      tistr = "(exp(jwT)) ";
    else
      tistr = "(jw)";
    endif

    xlabel ("Phase (deg)");

    if (is_siso (sys))
      title (sprintf ("Nichols plot of |[Y/U]%s|, u=%s, y=%s", tistr,
		      sysgetsignals (sys, "in", 1, 1),
		      sysgetsignals (sys, "out", 1, 1)));
    else
      title ([ "||Y(", tistr, ")/U(", tistr, ")||"]);
      printf ("MIMO plot from\n%s\nto\n%s\n", __outlist__ (inname, "    "),
              __outlist__ (outname, "       "));
    endif

    axis (axis2dlim ([phase(:), md(:)]));
  else
    mag2 = mag;
    phase2 = phase;
    w2 = w;
  endif

endfunction
