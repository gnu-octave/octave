########################################################################
##
## Copyright (C) 2022-2024 The Octave Project Developers
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
## This is not perfect replacement for the @sc{matlab} uifigure object as the
## properties @qcode{"AutoResizeChildren"}, @qcode{"Icon"}, and
## @qcode{"Scrollable"} are not implemented.
## @seealso{uipanel, uibuttongroup}
## @end deftypefn

## FIXME: This is not a perfect replica of a Matlab uifigure object because
## some ordinary figure object properties are exposed (e.g., the "PaperXXX"
## properties).  In addition, the following properties are missing:
## "AutoResizeChildren", "Icon", "Scrollable".

function h = uifigure (varargin)

  if (rem (nargin, 2) != 0)
    error ("uifigure: PROPERTY/VALUE parameters must occur in pairs");
  endif

  strfcn = @(s) any (strcmpi (s, {'AutoResizeChildren', 'Icon', 'Scrollable'}));
  idx = cellfun (strfcn, varargin (1:2:end));
  if (any (idx))
    idx = repelem (idx, 2); 
    props = varargin(idx);  # save special props for applying later
    varargin(idx) = [];     # remove special props from varargin
  endif

  h = __go_figure__ (NaN, "handlevisibility", "off",
                          "numbertitle", "off", "integerhandle", "off",
                          "menubar", "none", "toolbar", "none",
                          varargin{:});

  ## Add uifigure-specific properties on top of regular figure graphics object
  ## FIXME: There is no implementation behind these properties.
  addproperty ("AutoResizeChildren", h, "boolean", "on");
  addproperty ("Icon", h, "data", []);
  addproperty ("Scrollable", h, "boolean", "off");

  ## Set values for special properties added above
  if (! isempty (props))
    set (h, props{:});
  endif

endfunction


%!test
%! hf = uifigure ("visible", "off", "Icon", magic (3));
%! unwind_protect
%!   assert (isfigure (hf));
%!   assert (get (hf, {"numbertitle", "menubar", "icon", "scrollable"}),
%!                    {"off", "none", magic(3), "off"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <parameters must occur in pairs> uifigure ("PROP1")
