## Copyright (C) 2008-2012 Michael Goffioul
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
## @deftypefn  {Function File} {} hggroup ()
## @deftypefnx {Function File} {} hggroup (@var{h})
## @deftypefnx {Function File} {} hggroup (@dots{}, @var{property}, @var{value}, @dots{})
## Create group object with parent @var{h}.  If no parent is specified,
## the group is created in the current axes.  Return the handle of the
## group object created.
##
## Multiple property-value pairs may be specified for the group, but they
## must appear in pairs.
## @end deftypefn

## Author: goffioul

function h = hggroup (varargin)

  [ax, varargin] = __plt_get_axis_arg__ ("hggroup", varargin{:});

  tmp = __go_hggroup__ (ax, varargin{:});

  if (nargout > 0)
    h = tmp;
  endif

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = hggroup;
%!   assert (findobj (hf, "type", "hggroup"), h);
%!   assert (get (h, "type"), "hggroup");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
