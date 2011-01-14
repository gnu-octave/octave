## Copyright (C) 2010-2011 Kai Habel
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
## @deftypefn  {Function File} {} uimenu (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} uimenu (@var{h}, @var{property}, @var{value}, @dots{})
## Create an uimenu object and return a handle to it. If @var{h} is ommited then
## a top level menu entry for the current figure is created. If @var{h} is given then a 
## submenu relative to @var{h} is created.
## 
## Uimenu objects have the following specific properties:
##
## @table @code
## @item "accelerator"
## A string containg the key combination together with CTRL to execute this
## menu entry (e.g. "x" for CTRL+x).
##
## @item "callback"
## Is the function called when this menu entry is executed. It can be either a 
## function string (e.g. "myfun"), a function handle (e.g. @@myfun) or a cell
## array containing the function handle and arguments for the callback
## function (e.g. @{@@myfun, arg1, arg2@}).
##
## @item "checked"
## Can be set "on" or "off". Sets a mark at this menu entry.
##
## @item "enable"
## Can be set "on" or "off". If disabled the menu entry cannot be selected and it is
## grayed out.
## 
## @item "foregroundcolor"
## A color value setting the text color for this menu entry.
##
## @item "label"
## A string containing the label for this menu entry. A "&"-symbol can be used to mark
## the "accelerator" character (e.g. "E&xit")
##
## @item "position"
## An scalar value containing the relative menu position. The entry with the lowest
## value is at the first position starting from left or top.
##
## @item "separator"
## Can be set "on" or "off". If enabled it draws a separator line above the current 
## position. It is ignored for top level entries.
##
##
## @end table
##
## Examples:
##
## @example
## @group
## f = uimenu("label", "&File", "accelerator", "f");
## e = uimenu("label", "&Edit", "accelerator", "e");
## uimenu(f, "label", "Close", "accelerator", "q", "callback", "close (gcf)");
## uimenu(e, "label", "Toggle &Grid", "accelerator", "g", "callback", "grid (gca)");
## @end group
## @end example
## @seealso{figure}
## @end deftypefn

## Author: Kai Habel

function hui = uimenu (varargin)

  args = varargin;

  if (ishandle (args{1}))
    h = args{1};
    args(1) = [];
  else
    h = gcf ();
  endif

  if (rem (length (args), 2))
    error ("uimenu: expecting PROPERTY/VALUE pairs");
  endif

  tmp = __go_uimenu__ (h, args{:});

  if (nargout > 0)
    hui = tmp;
  endif

endfunction

%!demo
%! surfl(peaks);
%! colormap(copper);
%! shading("interp");
%! f = uimenu("label", "&File", "accelerator", "f");
%! e = uimenu("label", "&Edit", "accelerator", "e");
%! uimenu(f, "label", "Close", "accelerator", "q", "callback", "close (gcf)");
%! uimenu(e, "label", "Toggle &Grid", "accelerator", "g", "callback", "grid (gca)");
