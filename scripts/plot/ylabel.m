## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2003,
##               2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} ylabel (@var{string})
## See xlabel.
## @end deftypefn

## Author: jwe

function h = ylabel (varargin)

  if (rem (nargin, 2) == 1)
    if (nargout > 0)
      h = __axis_label__ ("ylabel", varargin{:});
    else
      __axis_label__ ("ylabel", varargin{:});
    endif
  else
    print_usage ();
  endif

endfunction
