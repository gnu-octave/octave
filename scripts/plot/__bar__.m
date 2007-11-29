## Copyright (C) 1996, 1997, 2007 John W. Eaton
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

## Author: jwe

function varargout = __bar__ (vertical, func, varargin)

  [h, varargin] = __plt_get_axis_arg__ ((nargout > 1), func, varargin{:});

  ## Slightly smaller than 0.8 to avoid clipping issue in gnuplot 4.0
  width = 0.8 - 10 * eps; 
  group = true;

  if (nargin < 3)
    print_usage ();
  endif

  if (nargin > 3 && isnumeric (varargin{2}))
    x = varargin{1};
    if (isvector (x))
      x = x(:);
    endif
    y = varargin{2};
    if (isvector (y))
      y = y(:);
    endif
    if (size (x, 1) != size (y, 1))
      y = varargin{1};
      if (isvector (y))
	y = y(:);
      endif
      x = [1:size(y,1)]';
      idx = 2;
    else
      if (! isvector (x))
	error ("%s: x must be a vector", func);
      endif
      idx = 3;
    endif
  else
    y = varargin{1};
    if (isvector (y))
      y = y(:);
    endif
    x = [1:size(y,1)]';
    idx = 2;
  endif
      
  newargs = {};
  have_line_spec = false;
  while (idx <= nargin - 2)
    if (isstr (varargin{idx}) && strcmp (varargin{idx}, "grouped"))
      group = true;
      idx++;
    elseif (isstr (varargin{idx}) && strcmp (varargin{idx}, "stacked"))
      group = false;
      idx++;
    else
      if ((isstr (varargin{idx}) || iscell (varargin{idx}))
	  && ! have_line_spec)
	[linespec, valid] = __pltopt__ (func, varargin{idx}, false);
	if (valid)
	  have_line_spec = true;
	  newargs = [{linespec.color}, newargs];
	  idx++;
	  continue;
	endif
      endif
      if (isscalar(varargin{idx}))
	width = varargin{idx++};
      elseif (idx == nargin - 2)
	newargs = [newargs,varargin(idx++)];
      else
	newargs = [newargs,varargin(idx:idx+1)];
	idx += 2;
      endif
    endif
  endwhile

  xlen = size (x, 1);
  ylen = size (y, 1);

  if (xlen != ylen)
    error ("%s: length of x and y must be equal", func)
  endif
  if (any (x(2:end) < x(1:end-1)))
    error ("%s: x vector values must be in ascending order", func);
  endif

  ycols = size (y, 2);
  if (group)
    width = width / ycols;
  endif

  cutoff = min (diff (double(x))) / 2;
  delta_p = delta_m = repmat (cutoff * width, size (x));
  x1 = (x - delta_m)(:)';
  x2 = (x + delta_p)(:)';
  xb = repmat ([x1; x1; x2; x2](:), 1, ycols);

  if (group)
    width = width / ycols;
    offset = ((delta_p + delta_m) * [-(ycols - 1) / 2 : (ycols - 1) / 2]);
    xb(1:4:4*ylen,:) += offset;
    xb(2:4:4*ylen,:) += offset;
    xb(3:4:4*ylen,:) += offset;
    xb(4:4:4*ylen,:) += offset;
    y0 = zeros (size (y));
    y1 = y;
  else
    y1 = cumsum(y,2);
    y0 = [zeros(ylen,1), y1(:,1:end-1)];
  endif

  yb = zeros (4*ylen, ycols);
  yb(1:4:4*ylen,:) = y0;
  yb(2:4:4*ylen,:) = y1;
  yb(3:4:4*ylen,:) = y1;
  yb(4:4:4*ylen,:) = y0;

  xb = reshape (xb, [4, numel(xb) / 4 / ycols, ycols]);
  yb = reshape (yb, [4, numel(yb) / 4 / ycols, ycols]);

  if (nargout < 2)
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();

      tmp = __bars__ (h, vertical, x, y, xb, yb, width, group,
		      have_line_spec, newargs{:});
      if (nargout == 1)
	varargout{1} = tmp;
      endif
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    if (vertical)
      varargout{1} = xb;
      varargout{2} = yb;
    else
      varargout{1} = yb;
      varargout{2} = xb;
    endif
  endif

endfunction
