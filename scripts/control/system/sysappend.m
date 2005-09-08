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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{sys} =} sysappend (@var{syst}, @var{b}, @var{c}, @var{d}, @var{outname}, @var{inname}, @var{yd})
## appends new inputs and/or outputs to a system
##
## @strong{Inputs}
## @table @var
## @item syst
## system data structure
##
## @item b
## matrix to be appended to sys "B" matrix (empty if none)
##
## @item c
## matrix to be appended to sys "C" matrix (empty if none)
##
## @item d
## revised sys d matrix (can be passed as [] if the revised d is all zeros)
##
## @item outname
## list of names for new outputs
##
## @item inname
## list of names for new inputs
##
## @item yd
## binary vector; @math{yd(ii)=0} indicates a continuous output;
## @math{yd(ii)=1} indicates a discrete output.
## @end table
##
## @strong{Outputs}
## @table @var
## @item sys
## @example
## @group
##    sys.b := [syst.b , b]
##    sys.c := [syst.c  ]
##             [ c     ]
##    sys.d := [syst.d | D12 ]
##             [ D21   | D22 ]
## @end group
## @end example
## where @math{D12}, @math{D21}, and @math{D22} are the appropriate dimensioned
## blocks of the input parameter @var{d}.
## @itemize @bullet
## @item The leading block @math{D11} of @var{d} is ignored.
## @item If @var{inname} and @var{outname} are not given as arguments,
##      the new inputs and outputs are be assigned default names.
## @item @var{yd} is a binary vector of length rows(c) that indicates
##      continuous/sampled outputs.  Default value for @var{yd} is:
## @itemize @minus
## @item @var{sys} is continuous or mixed
## @var{yd} = @code{zeros(1,rows(c))}
##
## @item @var{sys} is discrete
## @var{yd} = @code{ones(1,rows(c))}
## @end itemize
## @end itemize
## @end table
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: August 1996

function retsys = sysappend (sys, b, c, d, outname, inname, yd)

  save_warn_empty_list_elements = warn_empty_list_elements;
  unwind_protect
    warn_empty_list_elements = 0;

    ## check input arguments
    if ( (nargin < 2) | (nargin > 7) | (!isstruct(sys)))
      usage("retsys = sysappend(sys,b,c[,d,outname,inname,yd]) ");
    elseif(!isstruct(sys))
      error("sys must be a system data structure");
    endif

    ## default system type must be state space form
    [Aa,Ab,Ac,Ad,Ats,Ann,Anz,Ast,Ain,Aout,Ayd] = sys2ss(sys);
    [Ann,Anz,Am,Ap] = sysdimensions(sys);

    ## default c
    if(nargin < 3)      c = [];                                endif

    ## default d
    if(nargin < 4)     make_d = 1;
    elseif(isempty(d)) make_d = 1;
    else               make_d = 0;                             endif
    if(make_d)         d = zeros(rows(c)+Ap,columns(b) + Am);  endif

    ## Append new input(s) if any
    Bm = max(columns(d),columns(b)+Am);
    if(Bm != Am)
      ## construct new signal names
      if(nargin >= 6)   # new names were passed
	if(!ischar(inname))
	  error("inname must be a string");
	elseif(rows(inname) != (Bm - Am))
	  error(sprintf("%d new inputs requested; inname(%dx%d)", ...
	    (Bm-Am),rows(inname),columns(inname)));
	endif
      else
	inname = __sysdefioname__(Bm,"u",(Am+1));
      endif

      if(Am)
        Ain = __sysconcat__(Ain,inname);
      else
        Ain = inname;
      endif

      ## default b matrix
      if(isempty(b))     b  = zeros(Ann+Anz,(Bm-Am));
      elseif(rows(b) != Ann+Anz | columns(b) != (Bm-Am))
	  error(sprintf("b(%dx%d); should be (%dx%d)", rows(b), columns(b), ...
	    (Ann+Anz), (Bm-Am)));
      endif

      ## append new b matrix
      Ab = [Ab,b];
    endif

    ## Append new output(s) if any
    Bp = max(rows(d),rows(c)+Ap);
    if(Bp != Ap)

      ## construct new signal names, output classification
      if(nargin >= 5)  # new names were passed
	if(!ischar(outname))
	  error("outname must be a string");
	elseif(rows(outname) != (Bp - Ap))
	  error(sprintf("%d new outputs requested; outname(%dx%d)", ...
	    (Bp-Ap),rows(outname),columns(outname)));
	endif
      else
	outname = __sysdefioname__(Bp,"y",(Ap+1));
      endif
      if(Ap)   Aout = __sysconcat__(Aout,outname);
      else     Aout = outname;                endif

      ## construct new yd entries
      if(nargin == 7)
	if(!isvector(yd))
	  error(sprintf("yd(%dx%d) must be a vector",rows(yd),columns(yd)))
	elseif(rows(c) != length(yd) & rows(d) != length(yd))
	  error(sprintf("length(yd) = %d; c(%dx%d), d(%dx%d); mismatch", ...
	    length(yd), rows(c), columns(c),rows(d),columns(d)));
	endif
      else
	## default yd values
	yd = ones(1,Bp)*( (Ats > 0) & (Ann == 0)  & isempty(find(Ayd == 0)) ) ;
      endif
      Ayd = [vec(Ayd);vec(yd)];

      ## default c matrix
      if(isempty(c))      c = zeros((Bp-Ap),Ann+Anz);
      elseif(columns(c) != Ann+Anz | rows(c) != (Bp-Ap))
	  error(sprintf("c(%dx%d); should be (%dx%d)", rows(c), columns(c), ...
	    (Bp-Ap), (Ann+Anz) ));
      endif

      ## append new c matrix
      Ac = [Ac;c];
    endif

    ## check d matrix
    if(isempty(d)) d = zeros(Bp,Bm);
    elseif(rows(d) != Bp | columns(d) != Bm)
      error(sprintf("d(%dx%d) should be (%dx%d)",rows(d), columns(d), Bp, Bp));
    endif

    ## Splice in original D matrix
    if(Am & Ap)          d(1:Ap, 1:Am) = Ad;       endif
    Ad = d;

    ## construct return system
    retsys = ss(Aa,Ab,Ac,Ad,Ats,Ann,Anz,Ast,Ain,Aout,find(Ayd == 1));

  unwind_protect_cleanup
    warn_empty_list_elements = save_warn_empty_list_elements;
  end_unwind_protect

endfunction
