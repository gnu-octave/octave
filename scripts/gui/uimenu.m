########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{hui} =} uimenu (@var{property}, @var{value}, @dots{})
## @deftypefnx {} {@var{hui} =} uimenu (@var{h}, @var{property}, @var{value}, @dots{})
## Create a uimenu object and return a handle to it.
##
## If @var{h} is omitted then a top-level menu for the current figure is
## created.  If @var{h} is given then a submenu relative to @var{h} is created.
##
## uimenu objects have the following specific properties:
##
## @table @asis
## @item @qcode{"accelerator"}
## A string containing the key, together with CTRL, to execute this
## menu entry (e.g., @qcode{"x"} for CTRL+x).
##
## @item @qcode{"checked"}
## Can be set @qcode{"on"} or @qcode{"off"}.  Sets a mark at this menu entry.
##
## @item @qcode{"enable"}
## Can be set @qcode{"on"} or @qcode{"off"}.  If disabled then the menu entry
## cannot be selected and is grayed out.
##
## @item @qcode{"foregroundcolor"}
## A color value for the text of the menu entry.
##
## @item @qcode{"menuselectedfcn"}
## The function called when this menu entry is executed.  It can be either a
## function string (e.g., @qcode{"@nospell{myfcn}"}), a function handle (e.g.,
## @@@nospell{myfcn}) or a cell array containing the function handle and
## arguments for the callback function (e.g., @{@@@nospell{myfcn}, arg1,
## arg2@}).
##
## @item @qcode{"position"}
## A scalar value containing the relative menu position.  The first position
## has value 1 and will be either the left or top depending on the orientation
## of the uimenu.
##
## @item @qcode{"separator"}
## Can be set @qcode{"on"} or @qcode{"off"}.  If enabled, a separator
## line is drawn above the current position.  This property is ignored for
## top-level entries.
##
## @item @qcode{"text"}
## A string containing the text for this menu entry.  A @qcode{"&"}-symbol
## can be used to mark the @qcode{"accelerator"} character (e.g.,
## @nospell{@qcode{"E&xit"}}).
##
## @end table
##
## The full list of properties is documented at @ref{Uimenu Properties}.
##
## Examples:
##
## @example
## @group
## f = uimenu ("text", "&File", "accelerator", "f");
## e = uimenu ("text", "&Edit", "accelerator", "e");
## uimenu (f, "text", "Close", "accelerator", "q", ...
##            "menuselectedfcn", "close (gcf)");
## uimenu (e, "text", "Toggle &Grid", "accelerator", "g", ...
##            "menuselectedfcn", "grid (gca)");
## @end group
## @end example
## @seealso{figure}
## @end deftypefn

function hui = uimenu (varargin)

  [h, args] = __uiobject_split_args__ ("uimenu", varargin,
                                       {"figure", "uicontextmenu", "uimenu"});

  htmp = __go_uimenu__ (h, args{:});

  if (nargout > 0)
    hui = htmp;
  endif

endfunction


%!demo
%! clf;
%! surfl (peaks);
%! colormap (copper (64));
%! shading ('interp');
%! f = uimenu ('text', '&File', 'accelerator', 'f');
%! e = uimenu ('text', '&Edit', 'accelerator', 'e');
%! uimenu (f, 'text', 'Close', 'accelerator', 'q', ...
%!            'menuselectedfcn', 'close (gcf)');
%! uimenu (e, 'text', 'Toggle &Grid', 'accelerator', 'g', ...
%!            'menuselectedfcn', 'grid (gca)');

%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! toolkit = graphics_toolkit ("qt");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ui = uimenu ("text", "mytext");
%!   assert (findobj (hf, "type", "uimenu"), ui);
%!   assert (get (ui, "text"), "mytext");
%!   assert (get (ui, "checked"), "off");
%!   assert (get (ui, "separator"), "off");
%!   assert (get (ui, "enable"), "on");
%!   assert (get (ui, "position"), 4);
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## check for top level menus file and edit
%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! toolkit = graphics_toolkit ("qt");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   uif = findall (hf, "text", "&file");
%!   assert (ishghandle (uif));
%!   uie = findall (hf, "text", "&edit");
%!   assert (ishghandle (uie));
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! toolkit = graphics_toolkit ("qt");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   uie = findall (hf, "text", "&edit");
%!   myui = uimenu (uie, "text", "mytext");
%!   assert (ancestor (myui, "uimenu", "toplevel"), uie);
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect
