## Copyright (C) 1996, 1998, 1999 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {@var{sys} =} sysgroup (@var{asys}, @var{bsys})
## Combines two systems into a single system.
##
## @strong{Inputs}
## @table @var
## @item asys
## @itemx bsys
## System data structures.
## @end table
##
## @strong{Output}
## @table @var
## @item sys
## @math{sys = @r{block diag}(asys,bsys)}
## @end table
## @example
## @group
##          __________________
##          |    ________    |
## u1 ----->|--> | asys |--->|----> y1
##          |    --------    |
##          |    ________    |
## u2 ----->|--> | bsys |--->|----> y2
##          |    --------    |
##          ------------------
##               Ksys
## @end group
## @end example
## The function also rearranges the internal state-space realization of @var{sys}
## so that the continuous states come first and the discrete states come last.
## If there are duplicate names, the second name has a unique suffix appended
## on to the end of the name.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## modified by John Ingram July 1996
## A. S. Hodel: modified for variable number of arguments 1999

function sys = sysgroup (varargin)

  save_warn_empty_list_elements = warn_empty_list_elements;
  unwind_protect
    warn_empty_list_elements = 0;

    if(nargin < 1)
      usage("sys = sysgroup(Asys{,Bsys,...})");
    endif

    ## collect all arguments
    arglist = {};
    for kk=1:nargin
      arglist(kk) = varargin{kk};
      if(!isstruct(arglist{kk}))
	error("sysgroup: argument %d is not a data structure",kk);
      endif
    endfor

    if(nargin == 2)
      ## the usual case; group the two systems together
      Asys = arglist{1};
      Bsys = arglist{2};

      ## extract information from Asys, Bsys to consruct sys
      Asys = sysupdate(Asys,"ss");
      Bsys = sysupdate(Bsys,"ss");
      [n1,nz1,m1,p1] = sysdimensions(Asys);
      [n2,nz2,m2,p2] = sysdimensions(Bsys);
      [Aa,Ab,Ac,Ad,Atsam,An,Anz,Ast,Ain,Aout,Ayd] = sys2ss(Asys);
      [Ba,Bb,Bc,Bd,Btsam,Bn,Bnz,Bst,Bin,Bout,Byd] = sys2ss(Bsys);
      nA = An + Anz;
      nB = Bn + Bnz;

      if(p1*m1*p2*m2 == 0)
	error("sysgroup: argument lacks inputs and/or outputs");

      elseif((Atsam + Btsam > 0) & (Atsam * Btsam == 0) )
	warning("sysgroup: creating combination of continuous and discrete systems")

      elseif(Atsam != Btsam)
	error("sysgroup: Asys.tsam=%e, Bsys.tsam =%e", Atsam, Btsam);
      endif

      if(nA*nB > 0)
        A12 = zeros(nA,nB);
      else
        A12 = [];
      endif
      A = [Aa,A12; A12', Ba];
 
      if(nA*m2 > 0)
        B12 = zeros(nA,m2);
      else
        B12 = [];
      endif
      if(nB*m1 > 0)
        B21 = zeros(nB,m1);
      else
        B21 = [];
      endif
      if(isempty(Ab))
        Ab = [];
      endif
      if(isempty(Bb))
        Bb = [];
      endif
      B = [Ab, B12; B21, Bb];
 
      if(p1*nB > 0)
        C12 = zeros(p1,nB);
      else
        C12 = [];
      endif
      if(p2*nA > 0)
        C21 = zeros(p2,nA);
      else
        C21 = [];
      endif
      C = [Ac, C12; C21,Bc];
 
      if(p1*m2 > 0)
        D12 = zeros(p1,m2);
      else
        D12 = [];
      endif
      if(p2*m1 > 0)
        D21 = zeros(p2,m1);
      else
        D21 = [];
      endif
      D = [Ad, D12; D21, Bd];
      tsam = max(Atsam,Btsam);

      ## construct combined signal names; stnames must check for pure gain blocks
      if(isempty(Ast))
	stname = Bst;
      elseif(isempty(Bst))
	stname = Ast;
      else
        stname= __sysconcat__(Ast,Bst);
      endif
      inname = __sysconcat__(Ain,Bin);
      outname = __sysconcat__(Aout,Bout);

      ## Sort states into continous first, then discrete
      dstates = ones(1,(nA+nB));
      if(An)
	dstates(1:(An)) = zeros(1,An);
      endif
      if(Bn)
	dstates((nA+1):(nA+Bn)) = zeros(1,Bn);
      endif
      [tmp,pv] = sort(dstates);
      A = A(pv,pv);
      B = B(pv,:);
      C = C(:,pv);
      stname = stname(pv);

      ## check for duplicate signal names
      inname = __sysgroupn__ (inname, "input");
      stname = __sysgroupn__ (stname, "state");
      outname = __sysgroupn__ (outname, "output");

      ## mark discrete outputs
      outlist = find([Ayd, Byd]);

      ## build new system
      sys = ss(A,B,C,D,tsam,An+Bn,Anz+Bnz,stname,inname,outname);

    else
      ## multiple systems (or a single system); combine together one by one
      sys = arglist{1};
      for kk=2:length(arglist)
	printf("sysgroup: kk=%d\n",kk);
	sys = sysgroup(sys,arglist{kk});
      endfor
    endif

  unwind_protect_cleanup
    warn_empty_list_elements = save_warn_empty_list_elements;
  end_unwind_protect

endfunction
