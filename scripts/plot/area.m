## Copyright (C) 2007 Michael Goffioul
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
## @deftypefn {Function File} {} area (@var{x}, @var{y})
## @deftypefnx {Function File} {} area (@var{x}, @var{y}, @var{lvl})
## @deftypefnx {Function File} {} area (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} area (@var{y}, @dots{})
## @deftypefnx {Function File} {} area (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} area (@dots{})
## Area plot of cummulative sum of the columns of @var{y}. This shows the
## contributions of a value to a sum, and is functionally similar to 
## @code{plot (@var{x}, cumsum (@var{y}, 2))}, except that the area under 
## the curve is shaded.
##
## If the @var{x} argument is ommitted it is assumed to be given by
## @code{1 : rows (@var{y})}. A value @var{lvl} can be defined that determines
## where the base level of the shading under the curve should be defined.
##
## Additional arguments to the @code{area} function are passed to the 
## @code{patch}. The optional return value @var{h} provides a handle to 
## the list of patch objects.
## @seealso{plot, patch}
## @end deftypefn

function [ h ] = area (varargin)

  if (nargin > 0)
    idx = 1;
    ax = [];
    x = y = [];
    bv = 0;
    args = {};
    # check for axes parent
    if (ishandle (varargin{idx}) &&
	strcmp (get (varargin{idx}, "type"), "axes"))
      ax = varargin{idx};
      idx++;
    endif
    # check for (X) or (X,Y) arguments and possible base value
    if (nargin >= idx && ismatrix (varargin{idx}))
      y = varargin{idx};
      idx++;
      if (nargin >= idx)
        if (isscalar (varargin{idx}))
          bv = varargin{idx};
          idx++;
        elseif (ismatrix (varargin{idx}))
          x = y;
          y = varargin{idx};
          idx++;
          if (nargin >= idx && isscalar (varargin{idx}))
            bv = varargin{idx};
            idx++;
          endif
        endif
      endif
    else
      print_usage ();
    endif
    # check for additional args
    if (nargin >= idx)
      args = {varargin{idx:end}};
    endif
    newplot ();
    if (isvector (y))
      y = y(:);
    endif
    if (isempty (x))
      x = repmat ([1:size(y, 1)]', 1, size (y, 2));
    elseif (isvector (x))
      x = repmat (x(:),  1, size (y, 2));
    endif

    if (isempty (ax))
      tmp = __area__ (gca (), x, y, bv, args{:});
    else
      tmp = __area__ (ax, x, y, bv, args{:});
    endif

    if (nargout > 0)
      h = tmp;
    endif
  else
    print_usage ();
  endif

endfunction
