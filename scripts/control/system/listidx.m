## Copyright (C) 2000, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{idxvec}, @var{errmsg}] =} listidx (@var{listvar}, @var{strlist})
## Return indices of string entries in @var{listvar} that match strings
## in @var{strlist}.
##
## Both @var{listvar} and @var{strlist} may be passed as strings or
## string matrices.  If they are passed as string matrices, each entry
## is processed by @code{deblank} prior to searching for the entries.
##
## The first output is the vector of indices in @var{listvar}.
##
## If @var{strlist} contains a string not in @var{listvar}, then
## an error message is returned in @var{errmsg}.  If only one output
## argument is requested, then @var{listidx} prints @var{errmsg} to the
## screen and exits with an error.
## @end deftypefn

function [idxvec,errmsg]  = listidx(listvar,strlist)
  error("listidx: don't use this anymore, ok?\n");

if(nargin != 2)
  print_usage ();
endif

if(ischar(strlist))
  tmp = strlist;
  strlist = list();
  for kk=1:rows(tmp)
    strlist(kk) = deblank(tmp(kk,:));
  endfor
endif

if(ischar(listvar))
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
  signame = strlist{idx};
  for jdx = 1:nsigs
    if( strcmp(signame,listvar{jdx}) )
      if(idxvec(idx) != 0)
        warning("Duplicate signal name %s (%d,%d)\n", ...
          listvar{jdx},jdx,idxvec(idx));
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
