## Copyright (C) 1996, 1999 Auburn University.  All rights reserved.
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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{sys} =} sysmult (@var{Asys}, @var{Bsys})
## Compute @math{sys = Asys*Bsys} (series connection):
## @example
## @group
## u   ----------     ----------
## --->|  Bsys  |---->|  Asys  |--->
##     ----------     ----------
## @end group
## @end example
## A warning occurs if there is direct feed-through from an input 
## or a continuous state of @var{Bsys}, through a discrete output 
## of @var{Bsys}, to a continuous state or output in @var{Asys}
## (system data structure does not recognize discrete inputs).
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: July 1996
## updated for variable number of arguments by A. S. Hodel July 1999

function sys = sysmult (varargin)

  if(nargin < 1)
    print_usage ();
  endif

  ## collect all arguments
  arglist = {};
  for kk=1:nargin
    arglist{kk} = varargin{kk};
    if(!isstruct(arglist{kk}))
      error("sysadd: argument %d is not a data structure",kk);
    endif
  endfor

  ## check system dimensions
  [n,nz,mg,pg,Gyd] = sysdimensions(arglist{1});
  for kk=2:nargin
    [n,nz,mh,ph,Hyd] = sysdimensions(arglist{kk});
    if(ph != mg)
      error("arg %d has %d outputs; arg %d has %d inputs",kk,ph,kk-1,mg);
    endif
    [n,nz,mg,pg,Gyd] = sysdimensions(arglist{kk});   # for next iteration
  endfor

  ## perform the multiply
  if(nargin == 2)
    Asys = arglist{1};
    Bsys = arglist{2};

    [An,Anz,Am,Ap] = sysdimensions(Asys);
    [Bn,Bnz,Bm,Bp] = sysdimensions(Bsys);

    [Aa,Ab,Ac,Ad,Atsam,An,Anz,Astname,Ainname,Aoutname,Ayd] = sys2ss(Asys);
    [Ba,Bb,Bc,Bd,Btsam,Bn,Bnz,Bstname,Binname,Boutname,Byd] = sys2ss(Bsys);

    if(Byd)
      ## check direct feed-through of inputs through discrete outputs
      alist = find(Byd);
      if(An)
        bd = Ab(1:An)* Bd(alist,:);
        if(norm(bd,1))
          warning("sysmult: inputs -> Bsys discrete outputs -> continuous states of Asys");
        endif
      endif
      ## check direct feed-through of continuous state through discrete outputs
      if(Bn)
        bc = Ab(1:An)* Bc(alist,1:(Bn));
        if( norm(bc,1) )
          warning("sysmult: Bsys states -> Bsys discrete outputs -> continuous states of Asys");
        endif
      endif
    endif

    ## change signal names to avoid spurious warnings from sysgroup
    Asys = syssetsignals(Asys,"in",__sysdefioname__(Am,"A_sysmult_tmp_name"));
    Bsys = syssetsignals(Bsys,"out",__sysdefioname__(Bp,"B_sysmult_tmp_name"));

    sys = sysgroup(Asys,Bsys);

    ## connect outputs of B to inputs of A
    sys = sysconnect(sys,Ap+(1:Bp),1:Am);

    ## now keep only  outputs of A and inputs of B
    sys = sysprune(sys,1:Ap,Am+(1:Bm));

  else
    ## multiple systems (or a single system); combine together one by one
    sys = arglist{1};
    for kk=2:length(arglist)
      sys = sysmult(sys,arglist{kk});
    endfor
  endif

endfunction

