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
## @deftypefn {Function File} {} __line__ (@var{p}, @var{x}, @var{y})
## @deftypefnx {Function File} {} line (@var{p}, @var{x}, @var{y}, @var{z})
## Create line object from @var{x} and @var{y} with parent @var{p}.
## Return handle to line object.
## @end deftypefn

## Author: jwe

function h = __line__ (p, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  nvargs = numel (varargin);

  if (nvargs > 1 && isnumeric (varargin{1}) && isnumeric (varargin{2}))
    if (nvargs > 2 && isnumeric (varargin{3}))
      num_data_args = 3;
    else
      num_data_args = 2;
    endif
  else
    num_data_args = 0;
  endif

  if (rem (nvargs - num_data_args, 2) == 0)
  else
    print_usage ("line");
  endif

  h = __go_line__ (p);

  if (num_data_args > 1)
    set (h, "xdata", varargin{1}, "ydata", varargin{2});
    if (num_data_args == 3)
      set (h, "zdata", varargin{3});
    endif
  endif

  if (nvargs > num_data_args)
    set (h, varargin{num_data_args+1:end});
  endif

endfunction
