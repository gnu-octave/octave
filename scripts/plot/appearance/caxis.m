########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

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
## this axes rather than the current axes returned by @code{gca}.
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

  if (nargin > 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    if (isempty (hax))
      hax = gca ();
    endif
    if (nargin == 0)
      limits = get (hax, "clim");
    else
      arg1 = varargin{1};
      if (ischar (arg1))
        if (strcmpi (arg1, "auto"))
          set (hax, "climmode", "auto");
        elseif (strcmpi (arg1, "manual"))
          set (hax, "climmode", "manual");
        else
          error ("caxis: invalid mode '%s'", arg1);
        endif
      elseif (isvector (arg1))
        if (numel (arg1) != 2 || ! isnumeric (arg1) || arg1(1) >= arg1(2))
          error ("caxis: LIMITS must be a numeric 2-element vector where LIM1 < LIM2");
        endif
        set (hax, "clim", arg1);
      else
        print_usage ();
      endif
    endif
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction


%!test
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!   caxis ([e, pi]);
%!   assert (caxis (), [e, pi]);
%!   caxis (hax, [-1, 1]);
%!   assert (caxis (hax), [-1, 1]);
%!   assert (get (hax, "climmode"), "manual");
%!   caxis ("auto");
%!   assert (get (hax, "climmode"), "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error caxis (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   fail ("caxis ('foo')", "invalid mode 'foo'");
%!   fail ("caxis ([1 2 3])", "2-element vector");
%!   fail ("caxis ({1 2 3})", "numeric 2-element vector");
%!   fail ("caxis ([1 0])", "LIM1 < LIM2");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
