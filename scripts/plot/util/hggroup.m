########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {} hggroup ()
## @deftypefnx {} {} hggroup (@var{hax})
## @deftypefnx {} {} hggroup (@dots{}, @var{property}, @var{value}, @dots{})
## @deftypefnx {} {@var{h} =} hggroup (@dots{})
## Create handle graphics group object with axes parent @var{hax}.
##
## If no parent is specified, the group is created in the current axes.
##
## Multiple property/value pairs may be specified for the hggroup, but they
## must appear in pairs.  The full list of properties is documented at
## @ref{Axes Properties}.
##
## The optional return value @var{h} is a graphics handle to the created
## hggroup object.
##
## Programming Note: An hggroup is a way to group base graphics objects such
## as line objects or patch objects into a single unit which can react
## appropriately.  For example, the individual lines of a contour plot are
## collected into a single hggroup so that they can be made visible/invisible
## with a single command, @code{set (hg_handle, "visible", "off")}.
##
## @seealso{addproperty, addlistener}
## @end deftypefn

function h = hggroup (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("hggroup", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  htmp = __go_hggroup__ (hax, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = hggroup ();
%!   assert (findobj (hf, "type", "hggroup"), h);
%!   assert (get (h, "type"), "hggroup");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
