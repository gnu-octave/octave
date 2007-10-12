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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{realp}, @var{imagp}, @var{w}] =} nyquist (@var{sys}, @var{w}, @var{out_idx}, @var{in_idx}, @var{atol})
## @deftypefnx {Function File} {} nyquist (@var{sys}, @var{w}, @var{out_idx}, @var{in_idx}, @var{atol})
## Produce Nyquist plots of a system; if no output arguments are given, Nyquist
## plot is printed to the screen.
##
## Compute the frequency response of a system.
##
## @strong{Inputs} (pass as empty to get default values)
## @table @var
## @item sys
## system data structure (must be either purely continuous or discrete;
## see @code{is_digital})
## @item w
## frequency values for evaluation.
## If sys is continuous, then bode evaluates @math{G(@var{jw})}; 
## if sys is discrete, then bode evaluates @math{G(exp(@var{jwT}))},
## where @var{T} is the system sampling time.
## @item default
## the default frequency range is selected as follows: (These
## steps are @strong{not} performed if @var{w} is specified)
## @enumerate
## @item via routine @command{__bodquist__}, isolate all poles and zeros away from
## @var{w}=0 (@var{jw}=0 or @math{exp(@var{jwT})=1}) and select the frequency
## range based on the breakpoint locations of the frequencies.
## @item if @var{sys} is discrete time, the frequency range is limited
## to @var{jwT} in
## @ifinfo
## [0,2p*pi]
## @end ifinfo
## @iftex
## @tex
## $ [ 0,2  p \pi ] $
## @end tex
## @end iftex
## @item A ``smoothing'' routine is used to ensure that the plot phase does
## not change excessively from point to point and that singular
## points (e.g., crossovers from +/- 180) are accurately shown.
## @end enumerate
## @item   atol
## for interactive nyquist plots: atol is a change-in-slope tolerance
## for the of asymptotes (default = 0; 1e-2 is a good choice).  This allows
## the user to ``zoom in'' on portions of the Nyquist plot too small to be
## seen with large asymptotes.
## @end table
## @strong{Outputs}
## @table @var
## @item    realp
## @itemx   imagp
## the real and imaginary parts of the frequency response
## @math{G(jw)} or @math{G(exp(jwT))} at the selected frequency values.
## @item w
## the vector of frequency values used
## @end table
##
## If no output arguments are given, nyquist plots the results to the screen.
## If @var{atol} != 0 and asymptotes are detected then the user is asked
## interactively if they wish to zoom in (remove asymptotes)
## Descriptive labels are automatically placed.
##
## Note: if the requested plot is for an @acronym{MIMO} system, a warning message is
## presented; the returned information is of the magnitude
## @iftex
## @tex
## $ \Vert G(jw) \Vert $ or $ \Vert G( {\rm exp}(jwT) \Vert $
## @end tex
## @end iftex
## @ifinfo
## ||G(jw)|| or ||G(exp(jwT))||
## @end ifinfo
## only; phase information is not computed.
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: July 13, 1994
## A. S. Hodel July 1995 (adaptive frequency spacing,
##     remove acura parameter, etc.)
## Revised by John Ingram July 1996 for system format

function [realp, imagp, w] = nyquist (sys, w, outputs, inputs, atol)

  ## Both bode and nyquist share the same introduction, so the common
  ## parts are in a file called __bodquist__.m.  It contains the part that
  ## finds the number of arguments, determines whether or not the system
  ## is SISO, andd computes the frequency response.  Only the way the
  ## response is plotted is different between the two functions.

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
    atol = 0;
  elseif (! (is_sample (atol) || atol == 0))
    error ("nyquist: atol must be a nonnegative scalar")
  endif

  ## signal to __bodquist__ who's calling

  [f, w, sys] = __bodquist__ (sys, w, outputs, inputs, "nyquist");

  ## Get the real and imaginary part of f.
  realp = real (f);
  imagp = imag (f);

  ## No output arguments, then display plot, otherwise return data.
  if (nargout == 0)
    dnplot = 0;
    while (! dnplot)
      plot (realp, imagp, "- ;+w;", realp, -imagp, "-@ ;-w;");

      grid ("on");

      if (is_digital (sys))
        tstr = " G(e^{jw}) ";
      else
        tstr = " G(jw) ";
      endif
      xlabel (sprintf ("Re(%s)", tstr));
      ylabel (sprintf ("Im(%s)", tstr));

      [stn, inn, outn] = sysgetsignals (sys);
      if (is_siso (sys))
        title (sprintf ("Nyquist plot from %s to %s, w (rad/s) in [%e, %e]",
			inn{1}, outn{1}, w(1), w(end)));
      endif

      axis (axis2dlim ([[realp(:), imagp(:)]; [realp(:), -imagp(:)]]));

      ## check for interactive plots
      dnplot = 1; # assume done; will change later if atol is satisfied
      if (atol > 0 && length (f) > 2)

        ## check for asymptotes
        fmax = max (abs (f));
        fi = find (abs (f) == fmax, 1, "last");

        ## compute angles from point to point
        df = diff (f);
        th = atan2 (real (df), imag (df)) * 180 / pi;

        ## get angle at fmax
        if (fi == length(f))
	  fi = fi-1;
	endif
        thm = th(fi);

        ## now locate consecutive angles within atol of thm
        ith_same = find (abs (th - thm) < atol);
        ichk = union (fi, find (diff (ith_same) == 1));

        ## locate max, min consecutive indices in ichk
        loval = max (complement (ichk, 1:fi));
        if (isempty (loval))
	  loval = fi;
        else
          loval = loval + 1;
	endif

        hival = min (complement (ichk, fi:length(th)));
        if (isempty (hival))
	  hival = fi+1;
	endif

        keep_idx = complement (loval:hival, 1:length(w));

        if (length (keep_idx))
          resp = input ("Remove asymptotes and zoom in (y or n): ", 1);
          if (resp(1) == "y")
            dnplot = 0;                 # plot again
            w = w(keep_idx);
            f = f(keep_idx);
            realp = real (f);
            imagp = imag (f);
          endif
        endif

     endif
   endwhile
   w = realp = imagp = [];
 endif

endfunction
