########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{h} =} uifigure ()
## @deftypefnx {} {@var{h} =} uifigure ("@var{property}", @var{value}, @dots{})
## Create a new figure window for applications.
##
## Multiple property-value pairs may be specified for the figure object, but
## they must occur in pairs.
##
## The return value @var{h} is a graphics handle to the created figure object.
##
## Programming Note: The full list of properties is documented at
## @ref{Figure Properties}.  This function differs from @code{figure} in that
## the created figure is optimized for application development, rather than
## plotting.  This means features such as menubars and toolbars are turned off.
## @seealso{uipanel, uibuttongroup}
## @end deftypefn

## FIXME: This is not a perfect replica of a Matlab uifigure object because
## some ordinary figure object properties are exposed (e.g., the "PaperXXX"
## properties).  In addition, the following properties are missing:
## "AutoResizeChildren", "Icon", "Scrollable".

function h = uifigure (varargin)

  if (mod (nargin, 2) != 0)
    error ("uifigure: PROPERTY/VALUE parameters must occur in pairs");
  endif

  h = __go_figure__ (NaN, "handlevisibility", "off",
                          "numbertitle", "off", "integerhandle", "off",
                          "menubar", "none", "toolbar", "none");

  ## Add uifigure-specific properties on top of regular figure graphics object
  ## FIXME: There is no implementation behind these properties.
  addproperty ("AutoResizeChildren", h, "boolean", "on");
  addproperty ("Scrollable", h, "boolean", "off");

  ## Apply any overrides.
  if (! isempty (varargin))
    set (h, varargin{:});
  endif

endfunction


%!test
%! hf = uifigure ("visible", "off");
%! unwind_protect
%!   assert (isfigure (hf));
%!   assert (get (hf, {"numbertitle", "menubar", "scrollable"}),
%!                    {"off", "none", "off"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <parameters must occur in pairs> uifigure ("PROP1")
