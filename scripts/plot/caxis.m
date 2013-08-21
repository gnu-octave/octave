## Copyright (C)  2007-2012 David Bateman
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
## @deftypefn  {Function File} {} caxis (@var{limits})
## @deftypefnx {Function File} {} caxis ("auto")
## @deftypefnx {Function File} {} caxis ("manual")
## @deftypefnx {Function File} {} caxis (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{limits} =} caxis ()
## Query or set color axis limits for plots.
##
## The argument @var{limits} should be a 2-element vector specifying the
## lower and upper limits to assign to the first and last value in the
## colormap.  Values outside this range are clamped to the first and last
## colormap entries.
##
## If @var{limits} is @qcode{"auto"}, then automatic colormap scaling is
## applied, whereas if @var{limits} is @qcode{"manual"} the colormap scaling
## is set to manual.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Called without arguments the current color axis limits are returned.
## @seealso{colormap}
## @end deftypefn

function limits = caxis (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("caxis", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    if (isempty (hax))
      hax = gca ();
    endif
    if (nargin == 0)
      limits = __caxis__ (hax);
    else
      __caxis__ (hax, varargin{:});
    endif
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction

function limits = __caxis__ (ca, ax, varargin)

  if (nargin == 1)
    limits = get (ca, "clim");
  elseif (ischar (ax))
    if (strcmpi (ax, "auto"))
      set (ca, "climmode", "auto");
    elseif (strcmpi (ax, "manual"))
      set (ca, "climmode", "manual");
    endif
  elseif (isvector (ax))
    len = length (ax);
    if (len != 2)
      error ("caxis: expecting vector with 2 elements");
    endif

    set (ca, "clim", [ax(1), ax(2)]);
  else
    error ("caxis: expecting no args, a string, or a 2 element vector");
  endif

  ## FIXME: Why should it be possible to call __caxis__ recursively?
  if (nargin > 2)
    __caxis__ (ca, varargin{:})';
  endif

endfunction

