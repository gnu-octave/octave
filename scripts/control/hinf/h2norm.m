## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} h2norm (@var{sys})
## Computes the 
## @iftex
## @tex
## $ { \cal H }_2 $
## @end tex
## @end iftex
## @ifinfo
## H-2
## @end ifinfo
## norm of a system data structure (continuous time only).
##
## Reference:
## Doyle, Glover, Khargonekar, Francis, @cite{State-Space Solutions to Standard} 
## @iftex
## @tex
## $ { \cal H }_2 $ @cite{and} $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## @cite{H-2 and H-infinity}
## @end ifinfo
## @cite{Control Problems}, @acronym{IEEE} @acronym{TAC} August 1989.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## updated for system data structure by John Ingram November 1996

function h2gain = h2norm (sys)

  if((nargin != 1))
    usage("h2gain = h2norm(sys)");
  elseif(!isstruct(sys))
    error("Sys must be in system data structure");
  end
  dflg = is_digital(sys);

  if(!is_stable(sys))
    warning("h2norm: unstable input system; returning Inf");
    h2gain = Inf;
  else
    ## compute gain
    [a,b,c,d] = sys2ss(sys);
    if(dflg)
      M = dlyap(a,b*b');
    else
      M = lyap (a,b*b');
    endif
    if( min(real(eig(M))) < 0)
      error("h2norm: gramian not >= 0 (lightly damped modes?)")
    endif

    h2gain = sqrt(trace(d'*d + c*M*c'));
  endif
endfunction
