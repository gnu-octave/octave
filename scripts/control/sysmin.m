## Copyright (C) 1996 Auburn University.  All rights reserved.
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

## [retsys,nc,no] = sysmin(sys{,flg});
## return a minimal (or reduced order) system
## inputs:
##   sys: system data structure
##   flg: 0 [default] return minimal system; state names lost
##      : 1           return system with physical states removed that
##                    are either uncontrollable or unobservable
##                    (cannot reduce further without discarding physical
##                    meaning of states)
## outputs:
##   retsys: returned system
##   nc: number of controllable states in the returned system
##   no: number of observable states in the returned system
##   cflg: is_controllable(retsys)
##   oflg: is_observable(retsys)

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>

function [retsys, nc, no, cflg, oflg] = sysmin (sys, flg);
  
  switch(nargin)
  case(1), flg = 0;
  case(2), jnk = flg;    # dummy operation
  otherwise,
    usage("[retsys,nc,no] = sysmin(sys{,flg})");
  endswitch
  dflg = is_digital(sys,flg);
  Ts = sysgettsam(sys);
  switch(flg)
  case(0),
    ## reduce to a minimal system
    [aa,bb,cc,dd] = sys2ss(sys);
    [cflg,Uc] = is_controllable(aa,bb); 
    if(!cflg)
      ## reduce to controllable states
      if(!isempty(Uc))
        aa = Uc'*aa*Uc;
        bb = Uc'*bb;
        cc = cc*Uc;
      else
        aa = bb = cc = [];
      endif
    endif
    if(!isempty(aa))
      [oflg,Uo] = is_observable(aa,cc);
      if(!oflg)
        if(!isempty(Uo))
          aa = Uo'*aa*Uo;
          bb = Uo'*bb;
          cc = cc*Uo;
        else
          aa = bb = cc = [];
        endif
      endif
    endif
    switch(dflg)
    case(0),
      nc = no = nn = columns(aa);
      nz = 0;
    case(1),
      nc = no = nz = columns(aa);
      nn = 0;
    endswitch
    inname = sysgetsignals(sys,"in");
    outname= sysgetsignals(sys,"out");
    retsys = ss2sys(aa,bb,cc,dd,Ts,nn,nz,[],inname,outname);
  case(1),
    ## reduced model with physical states
    [cflg,Uc] = is_controllable(sys); xc = find(max(abs(Uc')) != 0);
    [oflg,Uo] = is_observable(sys);   xo = find(max(abs(Uo')) != 0);
    xx = intersection(xc,xo);
    if(isempty(xx)) xx = 0;  endif    # signal no states in reduced model
    retsys = sysprune(sys,[],[],xx);
  otherwise,
    error("illegal value of flg=%d",flg);
  endswitch
  if(sysdimensions(retsys,"st") > 0)
    [cflg,Uc] = is_controllable(retsys); nc = columns(Uc);
    [oflg,Uo] = is_observable(retsys);   no = columns(Uo);
  else
    nc = no = 0;
  endif
endfunction
