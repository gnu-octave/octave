## Copyright (C) 2000 Auburn University.  All rights reserved.
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

## [idxvec, errmsg] = listidx(listvar, strlist)
## return indices of string entries in listvar that match strings in strlist
## Inputs:
##   listvar: list of strings to be searched
##   strlist: list of strings to be located in listvar.
## Note: listvar, strlist may be passed as strings or string matrices; 
##    in this case, each entry is processed by deblank() prior to searching 
##    for the entries of strlist in listvar.
## Outputs:
## idxvec
##    vector of indices in listvar;
##    listvar(idxvec(k)) == strlist(kk).
## errmsg
##    if strlist contains a string not in listvar, then
##    an error message is returned in errmsg.  If only one output
##    argument is requested, e.g., idxvec = listidx(listvar, strlist),
##    then listidx prints errmsg to the screen and exits with 
##    an error.
##

function [idxvec,errmsg]  = listidx(listvar,strlist)

if(nargin != 2)
  usage("idxvec = listidx(listvar,strlist)");
endif

if(isstr(strlist))
  tmp = strlist;
  strlist = list();
  for kk=1:rows(tmp)
    strlist(kk) = deblank(tmp(kk,:));
  endfor
endif

if(isstr(listvar))
  tmp = listvar;
  listvar = list();
  for kk=1:rows(tmp)
    listvar(kk) = deblank(tmp(kk,:));
  endfor
endif

## initialize size of idxvec (for premature return)
idxvec = zeros(length(strlist),1);

errmsg = "";
if(!is_signal_list(listvar))
  errmsg = "listvar must be a list of strings";
elseif(!is_signal_list(strlist))
  errmsg = "strlist must be a list of strings";
endif

if(length(errmsg))
  if(nargout < 2) error(errmsg); 
  else return;
  endif
endif

nsigs = length(listvar);
for idx = 1:length(strlist)
  signame = nth(strlist,idx);
  for jdx = 1:nsigs
    if( strcmp(signame,nth(listvar,jdx)) )
      if(idxvec(idx) != 0)
        warning("Duplicate signal name %s (%d,%d)\n", ...
          nth(listvar,jdx),jdx,idxvec(idx));
      else
        idxvec(idx) = jdx;
      endif
    endif
  endfor
  if(idxvec(idx) == 0)
    errmsg = sprintf("Did not find %s",signame);
    if(nargout == 1)
      error(errmsg);
    else
      break
    end
  endif
endfor

endfunction
