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
## @deftypefn {Function File} {[@var{idxvec}, @var{errmsg}] =} listidx(@var{listvar}, @var{strlist})
## return indices of string entries in listvar that match strings in strlist
## @strong{Inputs}
## @table @var
## @item listvar
##   list of strings to be searched
## @item strlist
##   list of strings to be located in listvar.
## @end table
## @strong{Note} @var{listvar}, @var{strlist} may be passed as strings or 
##    string matrices; in this case, each entry is processed by deblank() 
##    prior to searching for the entries of @var{strlist} in @var{listvar}.
## @strong{Outputs}
## @table @var
## @item idxvec
##    vector of indices in @var{listvar};
##    @var{listvar}(@var{idxvec}(@var{k})) == @var{strlist}(@var{kk}).
## @item errmsg
##    if strlist contains a string not in @var{listvar}, then
##    an error message is returned in @var{errmsg}.  If only one output
##    argument is requested, e.g., @code{idxvec = listidex(listvar, strlist)},
##    then @var{listidx} prints @var{errmsg} to the screen and exits with 
##    an error.
## @end table
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
  strlist
endif

if(isstr(listvar))
  tmp = listvar;
  listvar = list();
  for kk=1:rows(tmp)
    listvar(kk) = deblank(tmp(kk,:));
  endfor
endif

if(!is_signal_list(listvar))     error("listvar must be a list of strings");
elseif(!is_signal_list(strlist)) error("strlist must be a list of strings");
endif

nsigs = length(listvar);
idxvec = zeros(length(strlist),1);
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
