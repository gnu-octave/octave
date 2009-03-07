## Copyright (C) 2005, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn {Function File} {@var{h} =} __line__ (@var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## __line__ (p, x, y, z)
## Create line object from x, y, and z with parent p.
## Return handle to line object.

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

  if (rem (nvargs - num_data_args, 2) != 0)
    print_usage ("line");
  endif

  data_args = {};
  if (num_data_args > 1)
    data_args(1:4) = { "xdata", varargin{1}, "ydata", varargin{2} };
    if (num_data_args == 3)
      data_args(5:6) = { "zdata", varargin{3} };
    endif
  endif

  other_args = {};
  if (nvargs > num_data_args)
    other_args = varargin(num_data_args+1:end);
  endif

  h = __go_line__ (p, data_args{:}, other_args{:});

endfunction
