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

function [b, r] = deconv (y, a)

  ## usage: deconv (y, a)
  ##
  ## Deconvolve two vectors.
  ##
  ## [b, r] = deconv (y, a) solves for b and r such that 
  ##    y = conv(a,b) + r
  ##
  ## If y and a are polynomial coefficient vectors, b will contain the
  ## coefficients of the polynomial quotient and r will be a remander
  ## polynomial of lowest order.
  ##
  ## SEE ALSO: conv, poly, roots, residue, polyval, polyderiv,
  ## polyinteg 

  ## Written by Tony Richardson (amr@mpl.ucsd.edu) June 1994.

  if (nargin != 2)
    usage ("deconv (y, a)");
  endif

  if (is_matrix (y) || is_matrix (a))
    error("conv: both arguments must be vectors");
  endif

  la = length (a);
  ly = length (y);

  lb = ly - la + 1;

  if (ly > la)
    b = filter (y, a, [1, (zeros (1, ly - la))]);
  elseif (ly == la)
    b = filter (y, a, 1);
  else
    b = 0;
  endif

  b = polyreduce (b);

  lc = la + length (b) - 1;
  if (ly == lc)
    r = y - conv (a, b);
  else
    r = [(zeros (1, lc - ly)), y] - conv (a, b);
  endif

  r = polyreduce (r);

endfunction
