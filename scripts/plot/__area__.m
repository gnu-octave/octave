## Copyright (C) 2007 David Bateman
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

## Undocumented internal function.

function retval = __area__ (ax, x, y, bv, varargin)
  colors = [1, 0, 0; 0, 1, 0; 0, 0, 1; 1, 1, 0; 1, 0, 1; 0, 1, 1];
  x = [x(1,:) ; x ; x(end,:)];
  y = cumsum ([[bv, ones(1, size (y, 2) - 1)] ; y ; ...
	       [bv, ones(1, size (y, 2) - 1)]], 2);

  retval = patch (ax, x(:, 1), y (:, 1), colors (1,:), varargin{:});
  for i = 2 : size(y, 2)
    retval = [retval; patch(ax, [x(:,i); flipud(x(:,i))], ...
			    [y(:, i) ; flipud(y(:, i-1))], colors(i,:),
			    varargin{:})];
  endfor
endfunction
