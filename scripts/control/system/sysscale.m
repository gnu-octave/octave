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
## @deftypefn {Function File} {@var{retsys} =} sysscale (@var{sys}, @var{outscale}, @var{inscale}, @var{outname}, @var{inname})
## scale inputs/outputs of a system.
##
## @strong{Inputs}
## @table @var
## @item sys
## Structured system.
## @item outscale
## @itemx inscale
## Constant matrices of appropriate dimension.
## @item outname
## @itemx inname
## Lists of strings with the names of respectively outputs and inputs.
## @end table
##
## @strong{Output}
## @table @var
## @item retsys
## resulting open loop system:
## @example
##       -----------    -------    -----------
## u --->| inscale |--->| sys |--->| outscale |---> y
##       -----------    -------    -----------
## @end example
## @end table
## If the input names and output names (each a list of strings)
## are not given and the scaling matrices
## are not square, then default names will be given to the inputs and/or
## outputs.
##
## A warning message is printed if outscale attempts to add continuous
## system outputs to discrete system outputs; otherwise @var{yd} is
## set appropriately in the returned value of @var{sys}.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## modified by John Ingram 7-15-96

function sys = sysscale (sys, outscale, inscale, outname, inname)

  if( (nargin < 3) || (nargin > 5)  )
    usage("retsys = sysscale(Asys,output_list,input_list{,inname,outname})");
  elseif (!isstruct(sys))
    error("sys must be a structured system");
  endif

  [nn,nz,mm,pp] = sysdimensions(sys);

  ## check for omitted scales
  if(isempty(outscale))    outscale = eye(pp);     endif
  if(isempty(inscale))     inscale = eye(mm);      endif

  ## check dimensions of scaling matrices
  if(mm!=rows(inscale))
    error("inscale(%dx%d) should have %d rows(# system inputs)", ...
      rows(inscale),columns(inscale),mm);
  elseif( pp != columns(outscale) )
    error("outscale(%dx%d) should have %d columns(# system outputs)", ...
      rows(outscale), columns(outscale),pp);
  endif

  sysyd = sysgetsignals(sys,"yd");
  outc = find(sysyd==0);
  outd = find(sysyd==1);

  if(length(outc) & length(outd))
    for ii = 1:rows(outscale)
      nci = norm(outscale(ii,outc));
      ndi = norm(outscale(ii,outd));

      if( nci & ndi)
        warning("sysscale: outscale(%d,:) sums continuous and discrete outputs; setting output to cont",ii)
        sysyd(ii) = 0;
      else
        sysyd(ii) = (ndi != 0);
      endif
    endfor
  else
    sysyd = ones(1,rows(outscale))*( length(outd) > 0);
  endif

  ## check for SISO system type
  if strcmp(sysgettype(sys),"tf")
    [num,den,tsam,innam,outnam] = sys2tf(sys);
    num = num*inscale*outscale;
    sys = tf(num,den,tsam,innam,outnam,find(sysyd));
    return
  elseif strcmp(sysgettype(sys),"zp")
    [zer,pol,kk,tsam,innam,outnam] = sys2zp(sys);
    kk = kk*inscale*outscale;
    sys = zp(zer,pol,k,tsam,innam,outnam,find(sysyd));
    return
  endif

  ## it's a state space system...

  [sysa,sysb,sysc,sysd,systsam, ...
    sysn,sysnz,sysstname,sysinname,sysoutname,oldyd] = sys2ss(sys);

  sysb = sysb*inscale;
  sysc = outscale*sysc;
  sysd = outscale*sysd*inscale;

  if( !issquare(outscale) )
    ## strip extra output names (if any)
    sysoutname = sysoutname(1:min(rows(outscale),columns(outscale)));
    if( nargin < 4)
      warning("sysscale: outscale not square, outname not specified");
      warning("sysscale:  using default output names");
      outname = __sysdefioname__(rows(sysc),"y");
    endif
  else
    outname = sysoutname;
  endif
  if( !issquare(inscale) )
    ## strip extra output names (if any)
    sysinname = sysinname(1:min(rows(inscale),columns(inscale)));
    if(nargin < 5)
      warning("sysscale: inscale not square, inname not specified");
      warning("sysscale:  using default input names");
      inname = __sysdefioname__(columns(sysb),"u");
    endif
  else
    inname = sysgetsignals(sys,"in");
  endif

  sys = ss(sysa,sysb,sysc,sysd,systsam,nn,nz,sysstname, ...
        inname,outname,find(sysyd==1));

endfunction
