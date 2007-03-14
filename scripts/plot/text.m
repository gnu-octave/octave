## Copyright (C) 2007 John W. Eaton
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
## @deftypefn {Function File} {@var{h} =} text (@var{x}, @var{y}, @var{label})
## @deftypefnx {Function File} {@var{h} =} text (@var{x}, @var{y}, @var{z}, @var{label})
## Create a text object with text @var{label} at position @var{x},
## @var{y}, @var{z} on the current axes.  The label may be followed by
## property-value pairs.
## @end deftypefn

## Author: jwe

function h = text (varargin)

  nargs = nargin;
  offset = 0;

  if (nargs > 2 && isnumeric (varargin{1}) && isnumeric (varargin{2}))
    x = varargin{1};
    y = varargin{2};
    offset = 3;

    if (nargin > 3 && isnumeric (varargin{3}))
      z = varargin{3};
      offset = 4;
    else
      z = zeros (size (x));
      offset = 3;
    endif

    label = varargin{offset};
    if (ischar (label) || iscellstr (label))
      varargin(1:offset) = [];
      if (ischar (label))
	label = cellstr (label);
      endif
      n = numel (label);
      nx = numel (x);
      ny = numel (y);
      nz = numel (z);
    else
      error ("text: expecting label to be a character string or cell array of character strings");
    endif
  else
    x = y = z = 0;
    nx = ny = nz = 1;
    label = {""};
    n = 1;
  endif

  if (rem (numel (varargin), 2) == 0)

    if (nx == ny && nx == nz)
      pos = [x(:), y(:), z(:)];
      ca = gca ();
      tmp = zeros (n, 1);
      if (n == 1)
	label = label{1};
	for i = 1:nx
	  tmp(i) = __go_text__ (ca, "string", label,
				"position", pos(i,:),
				varargin{:});
	endfor
      elseif (n == nx)
	for i = 1:nx
	  tmp(i) = __go_text__ (ca, "string", label{i},
				"position", pos(i,:),
				varargin{:});
	endfor
      else
	error ("text: dimension mismatch for coordinates and label");
      endif
    else
      error ("text: dimension mismatch for coordinates");
    endif

    if (nargout > 0)
      h = tmp;
    endif

  else
    print_usage ();
  endif

endfunction
