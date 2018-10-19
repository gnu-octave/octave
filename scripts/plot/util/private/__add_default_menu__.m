## Copyright (C) 2010-2018 Kai Habel
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

## -*- texinfo -*-
## @deftypefn {} {} __add_default_menu__ (@var{hfig})
## @deftypefnx {} {} __add_default_menu__ (@var{hfig}, @var{hmenu})
## Add default menu and listeners to figure.
##
##
## All uimenu handles have their @qcode{"HandleVisibility"} property set to
## @qcode{"off"}.
## @end deftypefn

## Author: Kai Habel

function __add_default_menu__ (hf, hmenu = [])

  ## Create
  if (isempty (hmenu))
    ## File menu
    hui = uimenu (hf, "label", "&File", "handlevisibility", "off", ...
                  "tag", "__default_menu__File");
    uimenu (hui, "label", "&Open", "callback", @open_cb, ...
            "accelerator", "o");
    uimenu (hui, "label", "&Save", "callback", @save_cb, ...
            "accelerator", "s");
    uimenu (hui, "label", "Save &As", "callback", @save_cb, ...
            "accelerator", "S");
    uimenu (hui, "label", "&Close", "callback", @close_cb, ...
            "accelerator", "w", "separator", "on");
    hmenu(1) = hui;

    ## Edit menu
    hui = uimenu (hf, "label", "&Edit", "handlevisibility", "off", ...
                  "tag", "__default_menu__Edit");
    uimenu (hui, "label", "&New Figure", "callback", "figure ();", ...
            "accelerator", "n");
    uimenu (hui, "label", "&Duplicate Figure",
            "callback", "copyobj (gcbf (), groot ());", ...
            "accelerator", "d");
    uimenu (hui, "label", "Clea&r Figure",
            "callback", "clf (gcbf ());");
    uimenu (hui, "label", "Reset Figure",
            "callback", "reset (gcbf ());");
    hmenu(2) = hui;

    ## Tools menu
    hui = uimenu (hf, "label", "&Tools", "handlevisibility", "off", ...
                  "tag", "__default_menu__Tools");
    uimenu (hui, "label", "Toggle &grid on all axes", "tag", "toggle", ...
            "callback", @grid_cb);
    uimenu (hui, "label", "Show grid on all axes", "tag", "on", ...
            "callback", @grid_cb);
    uimenu (hui, "label", "Hide grid on all axes", "tag", "off", ...
            "callback", @grid_cb);
    uimenu (hui, "label", "Auto&scale all axes", "callback", @autoscale_cb);

    hui2 = uimenu (hui, "label", "GUI &Mode (on all axes)");
    uimenu (hui2, "label", "Pan x and y", "tag", "pan_on", ...
            "callback", @guimode_cb);
    uimenu (hui2, "label", "Pan x only", "tag", "pan_xon", ...
            "callback", @guimode_cb);
    uimenu (hui2, "label", "Pan y only", "tag", "pan_yon", ...
            "callback", @guimode_cb);
    uimenu (hui2, "label", "Disable pan and rotate", "tag", ...
            "no_pan_rotate", "callback", @guimode_cb);
    uimenu (hui2, "label", "Rotate on", "tag", "rotate3d", ...
            "callback", @guimode_cb);
    uimenu (hui2, "label", "Enable mousezoom", "tag", "zoom_on", ...
            "callback", @guimode_cb);
    uimenu (hui2, "label", "Disable mousezoom", "tag", "zoom_off", ...
            "callback", @guimode_cb);
    hmenu(3) = hui;
  endif

  ## Figure listeners
  toggle_visibility_cb (hf, [], hmenu);
  addlistener (hf, "menubar", {@toggle_visibility_cb, hmenu});

endfunction

function toggle_visibility_cb (hf, ~, hmenu)
  if (strcmp (get (hf, "menubar"), "none"))
    set (hmenu, "visible", "off")
  else
    set (hmenu, "visible", "on")
  endif
endfunction

function open_cb (h, e)
  [filename, filedir] = uigetfile ({"*.ofig", "Octave Figure File"}, ...
                                   "Open Figure");
  if (filename != 0)
    fname = fullfile (filedir, filename);
    tmphf = hgload (fname);
    set (tmphf, "filename", fname);
  endif
endfunction

function save_cb (h, e)
  [hcbo, hfig] = gcbo ();
  lbl = get (hcbo, "label");
  if (strcmp (lbl, "&Save"))
    fname = get (hfig, "filename");
    if (isempty (fname))
      __save_as__ (hfig);
    else
      saveas (hfig, fname);
    endif
  elseif (strcmp (lbl, "Save &As"))
    __save_as__ (hfig);
  endif
endfunction


function __save_as__ (hf)
  [filename, filedir] = uiputfile ...
    ({"*.ofig", "Octave Figure File";
      "*.eps;*.epsc;*.pdf;*.svg;*.ps;*.tikz", "Vector Image Formats";
      "*.gif;*.jpg;*.png;*.tiff", "Bitmap Image Formats"},
     "Save Figure", fullfile (pwd, "untitled.ofig"));

  if (filename != 0)
    fname = fullfile (filedir, filename);
    set (gcbf, "filename", fname);
    flen = numel (fname);
    if (flen > 5 && strcmp (fname(flen-4:end), ".ofig"))
      hgsave (hf, fname);
    else
      saveas (hf, fname);
    endif
  endif
endfunction


function close_cb (h, e)
  close (gcbf);
endfunction


function [hax, fig] = __get_axes__ (h)
  ## Get parent figure
  fig = ancestor (h, "figure");

  ## Find all axes which aren't legends
  hax = findobj (fig, "type", "axes", "-not", "tag", "legend");
endfunction


function grid_cb (h, e)
  hax = __get_axes__ (h);
  id = get (h, "tag");
  switch (id)
    case "toggle"
      arrayfun (@grid, hax);
    otherwise
      arrayfun (@(h) grid(h, id), hax);
  endswitch
  drawnow ();
endfunction


function autoscale_cb (h, e)
  hax = __get_axes__ (h);
  arrayfun (@(h) axis (h, "auto"), hax);
  drawnow ();
endfunction


function guimode_cb (h, e)
  [hax, fig] = __get_axes__ (h);
  id = get (h, "tag");
  switch (id)
    case "pan_on"
      pan (fig, "on");
    case "pan_xon"
      pan (fig, "xon");
    case "pan_yon"
      pan (fig, "yon");
    case "rotate3d"
      rotate3d (fig, "on");
    case "no_pan_rotate"
      pan (fig, "off");
      rotate3d (fig, "off");
    case "zoom_on"
      arrayfun (@(h) set (h, "mousewheelzoom", 0.05), hax);
    case "zoom_off"
      arrayfun (@(h) set (h, "mousewheelzoom", 0.0), hax);
  endswitch
endfunction
