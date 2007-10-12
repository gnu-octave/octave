## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{retsys}, @var{nc}, @var{no}] =} sysmin (@var{sys}, @var{flg})
## Returns a minimal (or reduced order) system
##
## @strong{Inputs}
## @table @var
## @item sys
## System data structure
## @item flg
## When equal to 0 (default value), returns minimal system,
## in which state names are lost; when equal to 1, returns system 
## with physical states removed that are either uncontrollable or 
## unobservable (cannot reduce further without discarding physical
## meaning of states).
## @end table
## @strong{Outputs}
## @table @var
## @item retsys
## Returned system.
## @item nc
## Number of controllable states in the returned system.
## @item no
## Number of observable states in the returned system.
## @item cflg
## @code{is_controllable(retsys)}.
## @item oflg
## @code{is_observable(retsys)}.
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>

function [retsys, nc, no, cflg, oflg] = sysmin (sys, flg)

  switch(nargin)
  case(1), flg = 0;
  case(2), jnk = flg;    # dummy operation
  otherwise,
    print_usage ();
  endswitch
  dflg = is_digital(sys,2);
  [n,nz,m,p] = sysdimensions(sys);
  if(n*nz > 0)
    # both continuous and discrete states
    [aa,bb,cc,dd,tsam,n,nz,stnam,innam,outnam,yd] = sys2ss(sys);
    crng = 1:n;
    drng = n+(1:nz);

    # get minimal realization of continuous part
    Ac  = aa(crng,crng);
    Acd = aa(crng,drng);
    Adc = aa(drng,crng);
    Ad  = aa(drng,drng);
    Bc  = bb(crng,:);
    Bd  = bb(drng,:);
    Cc  = cc(:,crng);
    Cd  = cc(:,drng);

    cstnam = stnam(crng);
    dstnam = stnam(drng);
    cinnam = __sysconcat__(innam,stnam(drng));
    coutnam = __sysconcat__(outnam,stnam(drng));
    csys = ss(Ac,[Bc,Acd],[Cc;Adc]);
    csys = syssetsignals(csys,"st",cstnam);
    csys = syssetsignals(csys,"in",cinnam);
    csys = syssetsignals(csys,"out",coutnam);

    # reduce continuous system, recombine with discrete part
    csys = sysmin(csys,flg);
    cn = sysdimensions(csys);

    if(cn == 0)
      # continuous states are removed; just reduce the discrete part
      sys = sysprune(sys,1:p,1:m,drng);
      retsys = sysmin(sys,flg);
    else
      # extract updated parameters from reduced continuous system
      [caa,cbb,ccc,cdd,ctsam,cn,cnz,cstnam,cinnam,coutnam] = sys2ss(csys);
      crng = 1:cn;
      Ac  = caa;
      Bc  = cbb(:,1:m);
      Acd = cbb(:,m+(1:nz));
      Cc  = ccc(1:p,:);
      Adc = ccc(p + (1:nz),:);

      # recombine to reduce discrete part of the system
      dinnam = __sysconcat__(innam,cstnam);
      doutnam = __sysconcat__(outnam,cstnam);
      dsys = ss(Ad,[Bd,Adc],[Cd;Acd],[],tsam);
      dsys = syssetsignals(dsys,"st",dstnam);
      dsys = syssetsignals(dsys,"in",dinnam);
      dsys = syssetsignals(dsys,"out",doutnam);

      # reduce discrete subsystem
      dsys = sysmin(dsys);
      [n1,nz] = sysdimensions(dsys);
      if(nz == 0)
        # discrete subsystem is not needed
        retsys = sysprune(csys,1:p,1:m);
      else
        # combine discrete, continuous subsystems
        [Ad,dbb,dcc] = sys2ss(dsys);
        dstnam = sysgetsignals(dsys,"st");
        Bd  = dbb(:,1:m);
        Adc = dbb(:,m+(1:cn));
        Cd  = dcc(1:p,:);
        Acd = dcc(p+(1:cn),:);
        stnam = __sysconcat__(cstnam,dstnam);
        aa = [Ac, Acd; Adc, Ad];
        bb = [Bc; Bd];
        cc = [Cc, Cd];
        retsys = ss([Ac, Acd; Adc, Ad], [Bc ; Bd], [Cc, Cd], dd, tsam, ...
          cn, nz, stnam, innam, outnam, find(yd == 1));
      end
    endif
  else
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
      innam = sysgetsignals(sys,"in");
      outnam= sysgetsignals(sys,"out");
      retsys = ss(aa,bb,cc,dd,Ts,nn,nz,[],innam,outnam);
    case(1),
      ## reduced model with physical states
      [cflg,Uc] = is_controllable(sys); xc = find(max(abs(Uc')) != 0);
      [oflg,Uo] = is_observable(sys);   xo = find(max(abs(Uo')) != 0);
      xx = intersection(xc,xo);
      if(isempty(xx)) xx = 0;  endif    # signal no states in reduced model
      retsys = sysprune(sys,[],[],xx);
    otherwise,
      error ("invalid value of flg = %d", flg);
    endswitch
    if(sysdimensions(retsys,"st") > 0)
      [cflg,Uc] = is_controllable(retsys); nc = columns(Uc);
      [oflg,Uo] = is_observable(retsys);   no = columns(Uo);
    else
      nc = no = 0;
    endif
  endif
endfunction
