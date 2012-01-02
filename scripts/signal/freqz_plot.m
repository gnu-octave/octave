## Copyright (C) 2002-2012 John W. Eaton
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
## @deftypefn {Function File} {} freqz_plot (@var{w}, @var{h})
## Plot the pass band, stop band and phase response of @var{h}.
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function freqz_plot (w, h)

  if (nargin != 2)
    print_usage ();
  endif

  n = length (w);

  ## ## exclude zero-frequency
  ## h = h (2 : length (h));
  ## w = w (2 : length (w));
  ## n = n-1;

  mag = 20 * log10 (abs (h));
  phase = unwrap (arg (h));
  maxmag = max (mag);

  subplot (3, 1, 1);
  plot (w, mag);
  grid ("on");
  legend ("Pass band (dB)");
  axis ([w(1), w(n), maxmag-3, maxmag], "labely");

  subplot (3, 1, 2);
  plot (w, mag);
  grid ("on");
  legend ("Stop band (dB)");
  if (maxmag - min (mag) > 100)
    axis ([w(1), w(n), maxmag-100, maxmag], "labely");
  else
    axis ("autoy", "labely");
  endif

  subplot (3, 1, 3);
  plot (w, phase*360/(2*pi));
  grid ("on");
  legend ("Phase (degrees)");
  xlabel ("Frequency");
  axis ([w(1), w(n)], "autoy", "label");

endfunction
