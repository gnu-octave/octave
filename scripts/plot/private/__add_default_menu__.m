## Copyright (C) 2010-2012 Kai Habel
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
## @deftypefn {Function File} {} __add_default_menu__ (@var{fig})
## Add default menu to figure.  All uimenu handles have
## set their property "handlevisibility" to "off".
## @end deftypefn

## Author: Kai Habel

function __add_default_menu__ (fig)

  if (isfigure (fig))
    obj = findall (fig, "label", "&File", "tag", "__default_menu__");
    if (length (obj) == 0)
      __f = uimenu (fig, "label", "&File", "handlevisibility", "off", "tag", "__default_menu__");
        sa = uimenu (__f, "label", "Save &As", "handlevisibility", "off",
                          "callback", @save_cb);
        sv = uimenu (__f, "label", "&Save", "handlevisibility", "off",
                          "callback", @save_cb);
        cl = uimenu (__f, "label", "&Close", "handlevisibility", "off",
                          "callback", "close(gcf)");

      __e = uimenu (fig, "label", "&Edit", "handlevisibility", "off");
        gr = uimenu (__e, "label", "&Grid", "handlevisibility", "off",
                          "callback", @grid_cb);
        as = uimenu (__e, "label", "Auto&scale", "handlevisibility", "off",
                          "callback", @autoscale_cb);
        gm = uimenu (__e, "label", "GUI &Mode", "handlevisibility", "off");
          gm2 = uimenu (gm, "label", "Pan+Zoom", "handlevisibility", "off",
                            "callback", @guimode_cb);
          gm3 = uimenu (gm, "label", "Rotate+Zoom", "handlevisibility", "off",
                            "callback", @guimode_cb);
          gmn = uimenu (gm, "label", "None", "handlevisibility", "off",
                            "callback", @guimode_cb);
      __h = uimenu (fig, "label", "&Help", "handlevisibility", "off");
        ab = uimenu (__h, "label", "A&bout", "handlevisibility", "off", "enable", "off");
    endif
  else
    error ("expecting figure handle", "handlevisibility", "off");
  endif

endfunction

function grid_cb (h, e)
  grid;
  drawnow; # should not be required
endfunction

function save_cb (h, e)
  lbl = get (gcbo, "label");
  if (strcmp (lbl, "&Save"))
    fname = get (gcbo, "userdata");
    if (isempty (fname))
      __save_as__ (gcbo);
    else
      saveas (gcbo, fname);
    endif
  elseif (strcmp (lbl, "Save &As"))
    __save_as__ (gcbo);
  endif
endfunction

function __save_as__ (caller)

  [filename, filedir] = uiputfile ({"*.pdf;*.ps;*.gif;*.png;*.jpg","Supported Graphic Formats"},
                                  "Save Figure",
                                  pwd);
  if (filename != 0)
    fname = strcat (filedir, filesep, filename);
    obj = findall ("label", "&Save");
    if (length (obj) > 0)
      set (obj(1), "userdata", fname);
    endif
    saveas (caller, fname);
  endif
endfunction

function autoscale_cb (h, e)
  axis ("auto");
  drawnow; #should not be required
endfunction

function guimode_cb (h, e)
  lbl = get(h, "label");
  if (strncmp(lbl, "Pan+Zoom", 8))
    gui_mode("2D");
  elseif (strncmp(lbl, "Rotate+Zoom", 11))
    gui_mode("3D");
  elseif (strncmp(lbl, "None", 4))
    gui_mode("None");
  endif
endfunction
