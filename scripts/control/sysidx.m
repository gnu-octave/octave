## Copyright (C) 1999 Auburn University. All rights reserved.
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
##
## Written by A. S. Hodel, a.s.hodel@eng.auburn.edu


function idxvec = sysidx(sys,sigtype,signamelist)

  ## idxvec = sysidx(sys,sigtype,signamelist)
  ## return indices of signals with specified signal names
  ## inputs:
  ##   sys:         OCST system data structure
  ##   sigtype:     signal type to be selected: "in", "out", "st"
  ##   signamelist: list of desired signal names
  ## outputs:
  ##   idxvec: vector of signal indices (appropriate for use with sysprune)

  if(nargin != 3)         usage("idxvec = sysidx(sys,sigtype,signamelist)");
  elseif(!is_struct(sys)) error("sys must be a system data structure");
  elseif(!isstr(sigtype)) error("sigtype must be a string");
  elseif(rows(sigtype) != 1) 
			  error("sigtype (%d x %d) must be a single string", ...
			    rows(sigtype),columns(sigtype));
  elseif(!is_signal_list(signamelist)) 
			  error("signamelist must be a list of strings");
  endif

  sigtype_list = list("input","output","state");
  sigtnum = 0;
  for idx = 1:length(sigtype_list)
    thistype = nth(sigtype_list,idx);
    if(strcmp(sigtype, thistype(1:length(sigtype)) )) sigtnum = idx; endif
  endfor
  if(sigtnum == 0)  error("Illegal sigtype value = %s\n",sigtype); endif

  syssiglist = sysgetsignals(sys,sigtype);

  for idx = 1:length(signamelist)
    signame = nth(signamelist,idx);
    idxvec(idx) = 0;
    nsigs = sysdimensions(sys,sigtype);
    for jdx = 1:nsigs
      ## printf("idx=%d jdx=%d signame=-%s- thissig=-%s-\n",idx,jdx,signame, ...
      ##   sysgetsignals(sys,sigtype,jdx,1));
      if(strcmp(signame,sysgetsignals(sys,sigtype,jdx,1)))
	if(idxvec(idx) != 0)
	  warning("Duplicate system input %s (%d,%d)\n", ...
	    sysgetsignals(sys,sigtype,jdx,1),jdx,idxvec(idx));
	else
	  idxvec(idx) = jdx;
	endif
      endif
    endfor
    if(idxvec(idx) == 0)
      error("Did not find %s %s",nth(sigtype_list,sigtnum),signame);
    endif
  endfor


endfunction
