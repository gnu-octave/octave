## Copyright (C) 2002 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} freqz_plot (@var{w}, @var{h})
## Plot the pass band, stop band and phase response of @var{h}.
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function freqz_plot(w,h)

    n = length (w);

    ## ## exclude zero-frequency
    ## h = h (2 : length (h));
    ## w = w (2 : length (w));
    ## n = n-1;

    mag = 20 * log10 (abs (h));
    phase = unwrap (arg (h));
    maxmag = max (mag);

    unwind_protect

      ## Protect graph state.

      replot_state = automatic_replot;
      automatic_replot = 0;

      subplot (311);
      __gnuplot_set__ lmargin 10;
      axis ("labely");
      xlabel ("");
      grid ("on");
      axis ([ w(1), w(n), maxmag-3, maxmag ]);
      plot (w, mag, ";Pass band (dB);");

      subplot (312);
      axis ("labely");
      title ("");
      xlabel ("");
      __gnuplot_set__ tmargin 0;
      grid ("on");
      if (maxmag - min (mag) > 100)
      	axis ([ w(1), w(n), maxmag-100, maxmag ]);
      else
      	axis ("autoy");
      endif
      plot (w, mag, ";Stop band (dB);");
      
      subplot (313);
      axis ("label");
      title ("");
      grid ("on");
      axis ("autoy");
      xlabel ("Frequency");
      axis ([ w(1), w(n) ]);
      plot (w, phase*360/(2*pi), ";Phase (degrees);");
      
    unwind_protect_cleanup

      ## Restore graph state.

      ## XXX FIXME XXX -- if automatic_replot is non-zero, this will
      ## mess up the graph, however if we don't do it here then the user
      ## will have to do it themselves.

      grid ("off");
      axis ("auto", "label");
      __gnuplot_set__ lmargin;
      __gnuplot_set__ tmargin;
      oneplot ();

      automatic_replot = replot_state;

    end_unwind_protect

endfunction
