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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{sys} =} syssub (@var{Gsys}, @var{Hsys})
## Return @math{sys = Gsys - Hsys}.
##
## @strong{Method}
##
## @var{Gsys} and @var{Hsys} are connected in parallel.
## The input vector is connected to both systems; the outputs are
## subtracted.  Returned system names are those of @var{Gsys}.
## @example
## @group
##          +--------+
##     +--->|  Gsys  |---+
##     |    +--------+   |
##     |                +|
## u --+                (_)--> y
##     |                -|
##     |    +--------+   |
##     +--->|  Hsys  |---+
##          +--------+
## @end group
## @end example
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: July 1996
## updated for variable numbers of input arguments by July 1999 A. S. Hodel

function sys = syssub (varargin)

  if(nargin < 1)
    usage("syssub: sys = syssub(Gsys{,Hsys,...})");
  endif

  ## collect all arguments
  arglist = {};
  for kk=1:nargin
    arglist{kk} = varargin{kk};
    if(!isstruct(arglist{kk}))
      error("syssub: argument %d is not a data structure",kk);
    endif
  endfor

  ## check system dimensions
  [n,nz,mg,pg,Gyd] = sysdimensions(arglist{1});
  for kk=2:nargin
    [n,nz,mh,ph,Hyd] = sysdimensions(arglist{kk});
    if(mg != mh)
      error("arg 1 has %d inputs; arg %d has vs %d inputs",mg,kk,mh);
    elseif(pg != ph)
      error("arg 1 has %d outputs; arg %d has vs %d outputs",pg,kk,ph);
    elseif(norm(Gyd - Hyd))
      warning("cannot add a discrete output to a continuous output");
      error("Output type mismatch: arguments 1 and %d\n",kk);
    endif
  endfor

  ## perform the subtract
  if(nargin == 2)
    Gsys = arglist{1};
    Hsys = arglist{2};
    if( strcmp(sysgettype(Gsys),"tf") | strcmp(sysgettype(Hsys),"tf") )
      ## see if subtracting  transfer functions with identical denominators
      [Gnum,Gden,GT,Gin,Gout] = sys2tf(Gsys);
      [Hnum,Hden,HT,Hin,Hout] = sys2tf(Hsys);
      if(length(Hden) == length(Gden) )
        if( (Hden == Gden) & (HT == GT) )
          sys = tf(Gnum-Hnum,Gden,GT,Gin,Gout);
          return
        endif
        ## if not, we go on and do the usual thing...
      endif
    endif

    ## make sure in ss form
    Gsys = sysupdate(Gsys,"ss");
    Hsys = sysupdate(Hsys,"ss");

    ## change signal names to avoid warning messages from sysgroup
    Gsys = syssetsignals(Gsys,"in",__sysdefioname__(length(Gin),"Gin_u"));
    Gsys = syssetsignals(Gsys,"out",__sysdefioname__(length(Gout),"Gout_u"));
    Hsys = syssetsignals(Hsys,"in",__sysdefioname__(length(Hin),"Hin_u"));
    Hsys = syssetsignals(Hsys,"out",__sysdefioname__(length(Hout),"Hout_u"));

    sys = sysgroup(Gsys,Hsys);

    eyin = eye(mg);
    eyout = eye(pg);

    sys = sysscale (sys, [eyout, -eyout], [eyin; eyin], Gout, Gin);

  else
    ## multiple systems (or a single system); combine together one by one
    sys = arglist{1};
    for kk=2:length(arglist)
      sys = syssub(sys,arglist{kk});
    endfor
  endif

endfunction
