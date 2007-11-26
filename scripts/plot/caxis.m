## Copyright (C)  2007 David Bateman
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
## @deftypefn {Function File} {} caxis (@var{limits})
## @deftypefnx {Function File} {} caxis (@var{h}, @dots{})
## Set color axis limits for plots.
##
## The argument @var{limits} should be a 2 element vector specifying the 
## lower and upper limits to assign to the first and last value in the
## colormap. Values outside this range are clamped to the first and last
## colormap entries. 
##
## If @var{limits} is 'auto', then automatic colormap scaling is applied,
## whereas if @var{limits} is 'manual' the colormap scaling is set to manual.
##
## Called without any arguments to current color axis limits are returned.
##
## If an axes handle is passed as the first argument, then operate on
## this axes rather than the current axes.
## @end deftypefn

## PKG_ADD: mark_as_command caxis

function varargout = caxis (varargin)

  if (nargin > 0 && isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin{1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("caxis: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      varargout = cell (max (nargin == 1, nargout), 1);
      if (isempty (varargout))
	__caxis__ (h, varargin{2:end});
      else
        [varargout{:}] = __caxis__ (h, varargin{2:end});
      endif
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    varargout = cell (max (nargin == 0, nargout), 1);
    if (isempty (varargout))
      __caxis__ (gca (), varargin{:});
    else
      [varargout{:}] = __caxis__ (gca (), varargin{:});
    endif
  endif

endfunction

function [cmin, cmax] = __caxis__ (ca, ax, varargin)

  if (nargin == 1)
    cmin = get (ca, "clim");
    if (nargout > 1)
      cmax = cmin(2);
      cmin = cmin(1);
    endif
  elseif (ischar (ax))
    ax = tolower (ax);
    if (strcmp (ax, "auto"))
      set (ca, "climmode", "auto");
    elseif (strcmp (ax, "manual"))
      set (ca, "climmode", "manual");
    endif
  elseif (isvector (ax))
    len = length (ax);

    if (len != 2)
      error ("caxis: expecting vector with 2 elements");
    endif

    set (ca, "clim", [ax(1), ax(2)]);
  else
    error ("caxis: expecting no args, a string or a 2 element vector");
  endif

  if (nargin > 2)
    __caxis__ (ca, varargin{:})'
  endif

endfunction
      
