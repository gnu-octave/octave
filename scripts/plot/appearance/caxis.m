## Copyright (C) 2007-2017 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} caxis ([cmin cmax])
## @deftypefnx {} {} caxis ("auto")
## @deftypefnx {} {} caxis ("manual")
## @deftypefnx {} {} caxis (@var{hax}, @dots{})
## @deftypefnx {} {@var{limits} =} caxis ()
## Query or set color axis limits for plots.
##
## The limits argument should be a 2-element vector specifying the lower and
## upper limits to assign to the first and last value in the colormap.  Data
## values outside this range are clamped to the first and last colormap
## entries.
##
## If the @qcode{"auto"} option is given then automatic colormap limits are
## applied.  The automatic algorithm sets @var{cmin} to the minimum data value
## and @var{cmax} to the maximum data value.  If @qcode{"manual"} is specified
## then the @qcode{"climmode"} property is set to @qcode{"manual"} and the
## numeric values in the @qcode{"clim"} property are used for limits.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Called without arguments the current color axis limits are returned.
##
## Programming Note: The color axis affects the display of image, patch, and
## surface graphics objects, but @strong{only} if the @qcode{"cdata"} property
## has indexed data and the @qcode{"cdatamapping"} property is set to
## @qcode{"scaled"}.  Graphic objects with true color @code{cdata}, or
## @qcode{"direct"} @code{cdatamapping} are not affected.
## @seealso{colormap, axis}
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
      error ("caxis: LIMITS must be a 2-element vector");
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
