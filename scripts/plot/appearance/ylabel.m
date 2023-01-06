########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {} ylabel (@var{string})
## @deftypefnx {} {} ylabel (@var{string}, @var{property}, @var{val}, @dots{})
## @deftypefnx {} {} ylabel (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} ylabel (@dots{})
## Specify the string used to label the y-axis of the current axis.
##
## If @var{hax} is specified then label the axis defined by @var{hax}.
##
## An optional list of @var{property}/@var{value} pairs can be used to change
## the properties of the created text label.
##
## The full list of text object properties is documented at
## @ref{Text Properties}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axes rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created text
## object.
## @seealso{xlabel, zlabel, datetick, title, text}
## @end deftypefn

function h = ylabel (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("ylabel", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  if (rem (nargin, 2) != 1)
    print_usage ();
  endif

  htmp = __axis_label__ (hax, "ylabel", varargin{1},
                         "color", get (hax, "ycolor"),
                         varargin{2:end});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hy = ylabel ("ylabel_string");
%!   assert (get (gca, "ylabel"), hy);
%!   assert (get (hy, "type"), "text");
%!   assert (get (hy, "visible"), "on");
%!   assert (get (hy, "string"), "ylabel_string");
%!   assert (get (hy, "color"), get (0, "defaultaxesycolor"));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "fontsize", 5, "labelfontsizemultiplier", 3);
%!   hy = ylabel ("ylabel_string", "color", "r");
%!   assert (get (hy, "fontsize"), 15);
%!   assert (get (hy, "color"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
