## Copyright (C) 1997 Kai P. Mueller
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
## @deftypefn {Function File} {} ord2 (@var{nfreq}, @var{damp}, @var{gain})
## Creates a continuous 2nd order system with parameters:
##
## @strong{Inputs}
## @table @var
## @item nfreq
## natural frequency [Hz]. (not in rad/s)
## @item damp
## damping coefficient
## @item gain
## dc-gain
## This is steady state value only for damp > 0.
## gain is assumed to be 1.0 if ommitted.
## @end table
##
## @strong{Output}
## @table @var
## @item outsys
## system data structure has representation with 
## @ifinfo
## @math{w = 2 * pi * nfreq}:
## @end ifinfo
## @iftex
## @tex
## $ w = 2  \pi  f $:
## @end tex
## @end iftex
## @example
## @group
##     /                                        \
##     | / -2w*damp -w \  / w \                 |
## G = | |             |, |   |, [ 0  gain ], 0 |
##     | \   w       0 /  \ 0 /                 |
##     \                                        /
## @end group
## @end example
## @end table
## @strong{See also} @command{jet707} (@acronym{MIMO} example, Boeing 707-321
## aircraft model)
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: September 28, 1997

function outsys = ord2 (nfreq, damp, gain)

  ## Updates

  if(nargin != 2 & nargin != 3)
    usage("outsys = ord2(nfreq, damp[, gain])")
  endif
  if (nargout > 1)
    usage("outsys = ord2(nfreq, damp[, gain])")
  endif
  if (nargin == 2)
    gain = 1.0;
  endif

  w = 2.0 * pi * nfreq;
  outsys = ss ([-2.0*w*damp, -w; w, 0], [w; 0], [0, gain]);
endfunction
