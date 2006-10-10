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
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} isequalwithequalnans (@var{x1}, @var{x2}, @dots{})
## Assuming NaN == NaN, return true if all of @var{x1}, @var{x2}, @dots{}
## are equal.
## @seealso{isequal}
## @end deftypefn

function retval = isequalwithequalnans (x, varargin)

  if (nargin > 1)
    retval = __isequal__ (1, x, varargin{:});
  else
    print_usage ();
  endif

endfunction

