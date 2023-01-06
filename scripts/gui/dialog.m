########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{h} =} dialog ()
## @deftypefnx {} {@var{h} =} dialog ("@var{property}", @var{value}, @dots{})
##
## Create an empty modal dialog window to which other uicontrols can be added.
##
## The dialog box is a figure object with properties as recommended for a
## dialog box.
##
## The default properties differing from a figure are:
##
## @table @asis
## @item buttondownfcn
## @code{if isempty (allchild(gcbf)), close (gcbf), endif}
##
## @item colormap
## []
##
## @item color
## defaultuicontrolbackgroundcolor
##
## @item dockcontrols
## off
##
## @item handlevisibility
## callback
##
## @item integerhandle
## off
##
## @item inverthardcopy
## off
##
## @item menubar
## none
##
## @item numbertitle
## off
##
## @item paperpositionmode
## auto
##
## @item resize
## off
##
## @item windowstyle
## modal
##
## @end table
##
##
## Multiple property-value pairs may be specified for the dialog object, but
## they must appear in pairs.  The full list of properties is documented at
## @ref{Figure Properties}.
##
## The return value @var{h} is a graphics handle to the created figure.
##
## Example:
##
## @example
## @group
## ## create an empty dialog window titled "Dialog Example"
## h = dialog ("name", "Dialog Example");
##
## ## create a button (default style)
## b = uicontrol (h, "string", "OK",
##                   "position", [10 10 150 40],
##                   "callback", "delete (gcf)");
##
## ## wait for dialog to resume or close
## uiwait (h);
## @end group
## @end example
##
## @seealso{errordlg, msgbox, questdlg, warndlg, figure, uiwait}
## @end deftypefn

function h = dialog (varargin)

  h = figure ( ...
    "buttondownfcn", "if isempty (allchild (gcbf)), close (gcbf), endif",
    "color", get (0,"defaultuicontrolbackgroundcolor"),
    "colormap", [],
    "dockcontrols", "off",
    "handlevisibility", "callback",
    "integerhandle", "off",
    "inverthardcopy", "off",
    "menubar", "none",
    "numbertitle", "off",
    "paperpositionmode", "auto",
    "resize", "off",
    "toolbar", "none",
    "windowstyle", "modal",
    varargin{:});

endfunction

## No BIST tests.  This function just dispatches to figure().
%!assert (1)
