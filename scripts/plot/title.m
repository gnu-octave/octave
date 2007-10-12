## Copyright (C) 2005 John W. Eaton
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
## @deftypefn {Function File} {} title (@var{title})
## Create a title object and return a handle to it.
## @end deftypefn

## Author: jwe

function retval = title (varargin)

  if (rem (nargin, 2) == 1)
    if (nargout > 0)
      h = __axis_label__ ("title", varargin{:});
    else
      __axis_label__ ("title", varargin{:});
    endif
  else
    print_usage ();
  endif

endfunction
