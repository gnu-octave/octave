## Copyright (C) 2005 William Poetra Yoga Hadisoeseno
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} isequalwithequalnans (@var{x1}, @var{x2}, @dots{})
## Assuming NaN == NaN, return true if all of @var{x1}, @var{x2}, @dots{}
## are equal.
## @end deftypefn
##
## @seealso{isequal}

function retval = isequalwithequalnans (x, varargin)

  if (nargin > 1)
    retval = __isequal__ (1, x, varargin{:});
  else
    usage ("isequalwithequalnans (x1, x2, ...)");
  endif

endfunction

