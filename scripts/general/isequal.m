## Copyright (C) 2000 Paul Kienzle
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## isequal(x1, x2, ...)
##    true if all parts of x1, x2, ... are equal

## Author: Paul Kienzle
## Adapted-by: jwe

function t = isequal (x, varargin)

  if (nargin != 2)
    usage ("isequal (x, y, ...)");
  endif

  for arg = 1:length (varargin)
    y = varargin{arg};
    if (isstruct (x))
      t = isstruct (y);
      for [v, k] = x
        t = (t
	     && struct_contains (y, k)
	     && isequal (getfield (x, k), getfield (y, k)));
      endfor
      for [v, k] = y
        t = t && struct_contains (x, k);
      endfor
    elseif (islist (x))
      t = islist(y) && length(x) == length(y);
      if (! t)
	return;
      endif
      for i = 1:length (x)
	t = isequal (x{i}, y{i});
	if (! t)
	  return;
	endif
      endfor
    elseif (any (size (x) != size (y)))
      t = false;
    elseif (iscell (x) || islist (x))
      t = iscell (y) || islist (y);
      if (! t)
	return;
      endif
      x = x(:);
      y = y(:);
      for i = 1:length (x)
	t = isequal (x{i}, y{i});
	if (! t)
	  return;
	endif
      endfor
    else
      t = all (x(:) == y(:));
    endif
    if (! t)
      return;
    endif
  endfor

endfunction
