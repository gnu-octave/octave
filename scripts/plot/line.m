## Copyright (C) 2005 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} line (@var{x}, @var{y})
## @deftypefnx {Function File} {} line (@var{x}, @var{y}, @var{z})
## Create line object from @var{x} and @var{y} and insert in current
## axes object.  Return handle to line object.
## @end deftypefn

## Author: jwe

function h = line (varargin)

  nargs = nargin;

  if (nargs > 1)
    if (isnumeric (varargin{1}) && isnumeric (varargin{2}))
      ## make a default line object, and make it the current axes for
      ## the current figure.
      ca = gca ();
      s = __uiobject_line_ctor__ (ca);
      s.xdata = varargin{1};
      s.ydata = varargin{2};
      num_data_args = 2;
      if (nargs > 2 && isnumeric (varargin{3}))
	s.zdata = varargin{3};
	num_data_args = 3;
      endif
      tmp = __uiobject_make_handle__ (s);
      if (nargs > num_data_args)
	set (tmp, varargin{num_data_args+1:end});
      endif
      __uiobject_adopt__ (ca, tmp);
      if (nargout > 0)
	h = tmp;
      endif
    else
      error ("expecting numeric arguments for line data");
    endif
  else
    print_usage ();
  endif

endfunction
