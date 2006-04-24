## Copyright (C) 2006 John W. Eaton
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} @var{opath} setpath (@var{npath})
## Set @code{LOADPATH} to @var{path} and return the previous value.
## 
## @seealso{LOADPATH, addpath, rmpath, savepath}
## @end deftypefn

## PKGADD: mark_as_command setpath

function opath = setpath (npath)

  if (nargin == 1)
    if (nargout > 0)
      opath = LOADPATH;
    endif
    ## FIXME -- perhaps validate elements of npath to make sure
    ## they are existing directories?
    if (ischar (npath))
      LOADPATH = npath;
    else
      error ("setpath: expecting argument to be a character string");
    endif
  else
    usage ("opath = setpath (npath)");
  endif

endfunction
