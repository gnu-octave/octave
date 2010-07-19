## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2003, 2004,
##               2005, 2006, 2007, 2009 John W. Eaton
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
## @deftypefn  {Function File} {} title (@var{title})
## @deftypefnx {Function File} {} title (@var{title}, @var{p1}, @var{v1}, @dots{})
## Create a title object and return a handle to it.
## @end deftypefn

## Author: jwe

function h = title (s, varargin)

  if (rem (nargin, 2) == 1)
    if (nargout > 0)
      h = __axis_label__ ("title", s, varargin{:});
    else
      __axis_label__ ("title", s, varargin{:});
    endif
  else
    print_usage ();
  endif

endfunction
