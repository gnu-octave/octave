### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

## Image format conversion.

## Author: Tony Richardson <amr@mpl.ucsd.edu>
## Created: July 1994
## Adapted-By: jwe

function rgb = ntsc2rgb (yiq)

  if (nargin != 1)
    usage ("ntsc2rgb (yiq)");
  endif

  trans = [ 1.0,      1.0,      1.0;
            0.95617, -0.27269, -1.10374;
            0.62143, -0.64681, 1.70062 ];

  rgb = yiq * trans;

endfunction
