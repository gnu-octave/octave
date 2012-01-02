## Copyright (C) 2000-2012 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{idxvec}, @var{errmsg}] =} cellidx (@var{listvar}, @var{strlist})
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
## argument is requested, then @var{cellidx} prints @var{errmsg} to the
## screen and exits with an error.
## @end deftypefn

## deprecated in version 3.4

function [idxvec,errmsg]  = cellidx (listvar, strlist)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "cellidx is obsolete and will be removed from a future version of Octave; use ismember instead");
  endif

  if (nargin != 2)
    print_usage ();
  endif

  if (ischar (strlist))
    tmp = strlist;
    strlist = {};
    for kk = 1:rows(tmp)
      strlist{kk} = deblank (tmp(kk,:));
    endfor
  endif

  if (ischar (listvar))
    tmp = listvar;
    listvar = {};
    for kk = 1:rows(tmp)
      listvar{kk} = deblank (tmp(kk,:));
    endfor
  endif

  ## initialize size of idxvec (for premature return)
  idxvec = zeros (length(strlist), 1);

  errmsg = "";
  if (! iscellstr (listvar))
    errmsg = "listvar must be a list of strings";
  elseif (! iscellstr (strlist))
    errmsg = "strlist must be a list of strings";
  endif

  if (length (errmsg))
    if (nargout < 2)
      error (errmsg);
    else
      return;
    endif
  endif

  nsigs = length(listvar);
  for idx = 1:length(strlist)
    signame = strlist{idx};
    for jdx = 1:nsigs
      if (strcmp (signame, listvar{jdx}))
        if (idxvec(idx) != 0)
          warning ("Duplicate signal name %s (%d,%d)\n",
                   listvar{jdx}, jdx, idxvec(idx));
        else
          idxvec(idx) = jdx;
        endif
      endif
    endfor
    if (idxvec(idx) == 0)
      errmsg = sprintf ("Did not find %s", signame);
      if (nargout == 1)
        error (errmsg);
      else
        break;
      endif
    endif
  endfor

endfunction
