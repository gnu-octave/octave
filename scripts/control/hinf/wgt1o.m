## Copyright (C) 1998 Kai P. Mueller
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{W} =} wgt1o (@var{vl}, @var{vh}, @var{fc})
## State space description of a first order weighting function.
##
## Weighting function are needed by the 
## @iftex
## @tex
## $ { \cal H }_2 / { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-2/H-infinity
## @end ifinfo
## design procedure.
## These function are part of the augmented plant @var{P}
## (see @command{hinfdemo} for an application example).
##
## @strong{Inputs}
## @table @var
## @item vl
## Gain at low frequencies.
## @item vh
## Gain at high frequencies.
## @item fc
## Corner frequency (in Hz, @strong{not} in rad/sec)
## @end table
##
## @strong{Output}
## @table @var
## @item W
## Weighting function, given in form of a system data structure.
## @end table
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: September 30, 1997

function wsys = wgt1o (vl, vh, fc)

  if (nargin != 3)
    usage("wsys = wgt1o(vl, vh, fc)");
  endif

  if(nargout > 1)
    usage("wsys = wgt1o(vl, vh, fc)");
  endif

  if (vl == vh)
      a = [];
      b = [];
      c = [];
  else
      a = [-2*pi*fc];
      b = [-2*pi*fc];
      c = [vh-vl];
  endif
  d=[vh];

  wsys = ss(a,b,c,d);
endfunction
