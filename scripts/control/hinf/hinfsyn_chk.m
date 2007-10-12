## Copyright (C) 1996, 2000, 2004, 2005, 2007
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
## @deftypefn {Function File} {[@var{retval}, @var{pc}, @var{pf}] =} hinfsyn_chk (@var{a}, @var{b1}, @var{b2}, @var{c1}, @var{c2}, @var{d12}, @var{d21}, @var{g}, @var{ptol})
## Called by @code{hinfsyn} to see if gain @var{g} satisfies conditions in
## Theorem 3 of
## Doyle, Glover, Khargonekar, Francis, @cite{State Space Solutions to Standard}
## @iftex
## @tex
## $ { \cal H }_2 $ @cite{and} $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## @cite{H-2 and H-infinity}
## @end ifinfo
## @cite{Control Problems}, @acronym{IEEE} @acronym{TAC} August 1989.
##
## @strong{Warning:} do not attempt to use this at home; no argument
## checking performed.
##
## @strong{Inputs}
##
## As returned by @code{is_dgkf}, except for:
## @table @var
## @item g
## candidate gain level
## @item ptol
##  as in @code{hinfsyn}
## @end table
##
## @strong{Outputs}
## @table @var
## @item retval
##  1 if g exceeds optimal Hinf closed loop gain, else 0
## @item pc
## solution of ``regulator'' 
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## @acronym{ARE}
## @item pf
## solution of ``filter''
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## @acronym{ARE}
## @end table
## Do not attempt to use this at home; no argument checking performed.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995

function [retval, Pc, Pf] = hinfsyn_chk (A, B1, B2, C1, C2, D12, D21, g, ptol)

  Pc = Pf = [];

  ## Construct the two Hamiltonians
  g2 = 1/(g*g);
  Hc = [ A ,  g2*B1*B1' - B2*B2'; -C1'*C1 , -A'];
  Hf = [ A' , g2*C1'*C1 - C2'*C2; -B1*B1' , -A];

  ## check if Hc, Hf are in dom(Ric)
  Hcminval = min(abs(real(eig(Hc))));
  Hfminval = min(abs(real(eig(Hf))));
  if(Hcminval < ptol);
    disp("hinfsyn_chk: Hc is not in dom(Ric)");
    retval = 0;
    return
  endif
  if(Hfminval < ptol)
    disp("hinfsyn_chk: Hf is not in dom(Ric)");
    retval = 0;
    return
  endif

  ## Solve ARE's
  Pc = are(A, B2*B2'-g2*B1*B1',C1'*C1);
  Pf = are(A',C2'*C2-g2*C1'*C1,B1*B1');

  Pceig = eig(Pc);
  Pfeig = eig(Pf);
  Pcfeig = eig(Pc*Pf);

  if(min(Pceig) < -ptol)
    disp("hinfsyn_chk: Pc is not >= 0");
    retval = 0;
    return
  endif
  if(min(Pfeig) < -ptol)
    disp("hinfsyn_chk: Pf is not >= 0");
    retval = 0;
    return
  endif
  if(max(abs(Pcfeig)) >= g*g)
    disp("hinfsyn_chk: rho(Pf*Pc) is not < g^2");
    retval = 0;
    return
  endif

  ## all conditions met.
  retval = 1;

endfunction
