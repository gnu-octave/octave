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
## @deftypefn  {} {} __add_default_menu__ (@var{hf})
## @deftypefnx {} {} __add_default_menu__ (@var{hf}, @var{hmenu})
## @deftypefnx {} {} __add_default_menu__ (@var{hf}, @var{hmenu}, @var{htb})
## Add default menu and listeners to figure.
##
## @var{hf} is a figure handle.
## @var{hmenu} is a uimenu handle.
## @var{htb} is a uitoolbar handle.
##
## All uimenu handles have their @qcode{"HandleVisibility"} property set to
## @qcode{"off"}.
## @end deftypefn

function __add_default_menu__ (hf, hmenu = [], htb = [])

  ## Gnuplot doesn't handle uimenu and uitoolbar objects
  if (strcmp (graphics_toolkit (), "gnuplot"))
    return;
  endif

  ## Create
  if (isempty (hmenu))
    ## File menu
    hui = uimenu (hf, "label", "&File", "tag", "__default_menu__File", ...
                      "handlevisibility", "off");
    uimenu (hui, "label", "&Open...", "menuselectedfcn", @open_cb, ...
                 "accelerator", "o");
    uimenu (hui, "label", "&Save", "menuselectedfcn", {@save_cb, "save"}, ...
                 "accelerator", "s");
    uimenu (hui, "label", "Save &As...", ...
                 "menuselectedfcn", {@save_cb, "saveas"}, ...
                 "accelerator", "S");
    uimenu (hui, "label", "&Close", "menuselectedfcn", @close_cb, ...
                 "accelerator", "w", "separator", "on");
    hmenu(1) = hui;

    ## Edit menu
    hui = uimenu (hf, "label", "&Edit", "handlevisibility", "off", ...
                      "tag", "__default_menu__Edit");
    uimenu (hui, "label", "&New Figure", "menuselectedfcn", "figure ();", ...
                 "accelerator", "n");
    uimenu (hui, "label", "&Duplicate Figure",
                 "menuselectedfcn", "copyobj (gcbf (), groot ());", ...
                 "accelerator", "d");
    uimenu (hui, "label", "Clea&r Figure",
                 "menuselectedfcn", "clf (gcbf ());");
    uimenu (hui, "label", "Reset Figure",
                 "menuselectedfcn", "reset (gcbf ());");
    uimenu (hui, "label", "Copy Figure to Clipboard", ...
                 "menuselectedfcn", @clipboard_cb, "separator", "on", ...
                 "accelerator", "c");
    hmenu(2) = hui;

    ## Tools menu
    hui = uimenu (hf, "label", "&Tools", "handlevisibility", "off", ...
                      "tag", "__default_menu__Tools");
    uimenu (hui, "label", "Toggle &grid on all axes", "tag", "toggle", ...
                 "menuselectedfcn", @grid_cb);
    uimenu (hui, "label", "Show grid on all axes", "tag", "on", ...
                 "menuselectedfcn", @grid_cb);
    uimenu (hui, "label", "Hide grid on all axes", "tag", "off", ...
                 "menuselectedfcn", @grid_cb);
    uimenu (hui, "label", "Auto&scale all axes",
                 "menuselectedfcn", @autoscale_cb);

    hui2 = uimenu (hui, "label", "GUI &Mode (on all axes)");
    uimenu (hui2, "label", "Pan x and y", "tag", "pan_on", ...
                  "menuselectedfcn", @guimode_cb);
    uimenu (hui2, "label", "Pan x only", "tag", "pan_xon", ...
                  "menuselectedfcn", @guimode_cb);
    uimenu (hui2, "label", "Pan y only", "tag", "pan_yon", ...
                  "menuselectedfcn", @guimode_cb);
    uimenu (hui2, "label", "Disable pan and rotate", "tag", ...
                  "no_pan_rotate", "menuselectedfcn", @guimode_cb);
    uimenu (hui2, "label", "Rotate on", "tag", "rotate3d", ...
                  "menuselectedfcn", @guimode_cb);
    uimenu (hui2, "label", "Enable mousezoom", "tag", "zoom_on", ...
                  "menuselectedfcn", @guimode_cb);
    uimenu (hui2, "label", "Disable mousezoom", "tag", "zoom_off", ...
                  "menuselectedfcn", @guimode_cb);
    hmenu(3) = hui;

    ## Default toolbar
    init_mouse_tools (hf);
    htb = uitoolbar (hf, "tag", "__default_toolbar__", ...
                         "handlevisibility", "off", "visible", "off");

    ht(1) = uitoggletool (htb, "tooltipstring", "Pan", ...
                               "tag", "__default_button_pan__", ...
                               "__named_icon__", "figure-pan");
    ht(2) = uitoggletool (htb, "tooltipstring", "Rotate", ...
                               "tag", "__default_button_rotate__", ...
                               "__named_icon__", "figure-rotate");

    ht(3) = uitoggletool (htb, "tooltipstring", "Zoom In", ...
                               "tag", "__default_button_zoomin__", ...
                               "__named_icon__", "figure-zoom-in", ...
                               "separator", "on");
    ht(4) = uitoggletool (htb, "tooltipstring", "Zoom Out", ...
                               "tag", "__default_button_zoomout__", ...
                               "__named_icon__", "figure-zoom-out");
    uipushtool (htb, "tooltipstring", "Automatic limits for current axes", ...
                     "clickedcallback", @auto_cb, ...
                     "__named_icon__", "figure-zoom-original");

    ht(5) = uitoggletool (htb, "tooltipstring", "Insert Text", ...
                               "tag", "__default_button_text__", ...
                               "separator", "on", ...
                               "__named_icon__", "figure-text");

    uipushtool (htb, "tooltipstring", "Toggle current axes visibility", ...
                     "clickedcallback", @axes_cb, "separator", "on", ...
                     "__named_icon__", "figure-axes");
    uipushtool (htb, "tooltipstring", "Toggle current axes grid visibility", ...
                     "clickedcallback", @grid_cb, ...
                     "__named_icon__", "figure-grid");

    set (ht(1), "oncallback", {@mouse_tools_cb, ht, "pan"}, ...
                "offcallback", {@mouse_tools_cb, ht, "pan"});
    set (ht(2), "oncallback", {@mouse_tools_cb, ht, "rotate"}, ...
                "offcallback", {@mouse_tools_cb, ht, "rotate"});
    set (ht(3), "oncallback", {@mouse_tools_cb, ht, "zoomin"}, ...
                "offcallback", {@mouse_tools_cb, ht, "zoomin"});
    set (ht(4), "oncallback", {@mouse_tools_cb, ht, "zoomout"}, ...
                "offcallback", {@mouse_tools_cb, ht, "zoomout"});
    set (ht(5), "oncallback", {@mouse_tools_cb, ht, "text"}, ...
                "offcallback", {@mouse_tools_cb, ht, "text"});
  endif

  if (! exist ("ht", "var"))
    ht = get (htb, "children")(end:-1:1);
    istoggletool = strcmp (get (ht, "type"), "uitoggletool");
    ht(! istoggletool) = [];
  endif

  ## Add/Restore figure listeners
  toggle_visibility_cb (hf, [], hmenu, htb);
  addlistener (hf, "menubar", {@toggle_visibility_cb, hmenu, htb});
  addlistener (hf, "toolbar", {@toggle_visibility_cb, hmenu, htb});
  addlistener (hf, "__mouse_mode__", {@mouse_tools_cb, ht, "mode"});
  addlistener (hf, "__zoom_mode__", {@mouse_tools_cb, ht, "mode"});

endfunction

function toggle_visibility_cb (hf, ~, hmenu, htb)

  menu_state = ifelse (strcmp (get (hf, "menubar"), "figure"), "on", "off");
  toolbar_state = "on";
  if (strcmp (get (hf, "toolbar"), "auto"))
    toolbar_state = menu_state;
  elseif (strcmp (get (hf, "toolbar"), "none"))
    toolbar_state = "off";
  endif

  set (hmenu, "visible", menu_state);
  set (htb, "visible", toolbar_state);

endfunction

function open_cb (~, ~)
  [filename, filedir] = uigetfile ({"*.ofig;*.fig", "Figure Files"}, ...
                                   "Open Figure");
  if (filename != 0)
    fname = fullfile (filedir, filename);
    openfig (fname);
  endif
endfunction

function save_cb (h, ~, action)

  hfig = gcbf ();
  fname = get (hfig, "filename");

  if (strcmp (action, "save"))
    if (isempty (fname))
      __save_as__ (hfig);
    else
      saveas (hfig, fname);
    endif
  elseif (strcmp (action, "saveas"))
    __save_as__ (hfig, fname);
  endif

endfunction

function __save_as__ (hf, fname = "")

  if (! isempty (fname))
    def = fname;
  else
    def = fullfile (pwd (), "untitled.ofig");
  endif
  filter = {"*.ofig", "Octave Figure";
            "*.eps",  "Encapsulated PostScript";
            "*.pdf",  "Portable Document Format";
            "*.ps",   "PostScript";
            "*.svg",  "Scalable Vector Graphics";
            "*.gif",  "GIF Image";
            "*.jpg",  "JPEG Image";
            "*.png",  "Portable Network Graphics Image";
            "*.tiff", "TIFF Image"};
  ## Reorder filters to have current first
  [~, ~, ext] = fileparts (def);
  idx = strcmp (filter(:,1), ["*" tolower(ext)]);
  filter = [filter(idx,:); filter(! idx,:)];

  [filename, filedir, filteridx] = uiputfile (filter, "Save Figure", def);

  if (filename != 0)
    fname = fullfile (filedir, filename);
    [~, ~, ext] = fileparts (fname);
    if (filteridx > rows (filter))
      ## "All Files" option
      if (isempty (ext))
        fmt = "";
      else
        fmt = ext(2:end);
      endif
    else
      fmt = filter{filteridx,1}(3:end);
      if (isempty (ext))
        fname = [fname "." fmt];
      endif
    endif
    set (hf, "filename", fname);
    saveas (hf, fname, fmt);
  endif

endfunction

function close_cb (~, ~)
  close (gcbf ());
endfunction

function [hax, fig] = __get_axes__ (h)
  ## Get parent figure
  fig = ancestor (h, "figure");

  ## Find all axes which aren't legends
  hax = findobj (fig, "type", "axes", "-not", "tag", "legend");
endfunction

function autoscale_cb (h, ~)
  hax = __get_axes__ (h);
  arrayfun (@(h) axis (h, "auto"), hax);
  drawnow ();
endfunction

function init_mouse_tools (hf)

  set (hf, "__pan_mode__", struct ("Enable", "off",
                                   "Motion", "both",
                                   "FigureHandle", hf),
           "__rotate_mode__", struct ("Enable", "off",
                                      "RotateStyle", "box",
                                      "FigureHandle", hf),
           "__zoom_mode__", struct ("Enable", "off",
                                    "Motion", "both",
                                    "Direction", "in",
                                    "FigureHandle", hf));

endfunction

function guimode_cb (h, ~)

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

function mouse_tools_cb (h, ~, htools, typ = "")

  persistent recursion = false;

  if (! recursion)
    recursion = true;

    hf = gcbf ();

    if (strcmp (typ, "mode"))
      ## The mouse mode has been changed from outside this callback,
      ## change the buttons state accordingly
      mode = get (hf, "__mouse_mode__");
      state = "on";

      switch (mode)
        case "zoom"
          zm = get (hf, "__zoom_mode__");
          if (strcmp (zm.Direction, "in"))
            htool = htools(3);
          else
            htool = htools(4);
          endif
        case "pan"
          htool = htools(1);
        case "rotate"
          htool = htools(2);
        case "text"
          htool = htools(5);
        case "none"
          state = "off";
          htool = htools;
      endswitch

      set (htool, "state", state);
      if (strcmp (state, "on"))
        set (htools(htools != htool), "state", "off");
      endif

    else
      ## Update the mouse mode according to the button state
      state = get (h, "state");

      switch (typ)
        case {"zoomin", "zoomout"}
          prop = "__zoom_mode__";
          val = get (hf, prop);

          if (strcmp (state, "on"))
            if (strcmp (typ, "zoomin"))
              val.Direction = "in";
            else
              val.Direction = "out";
            endif
            set (hf, "__mouse_mode__", "zoom");
          endif
          val.Enable = state;
          set (hf, prop, val);

        case {"pan", "rotate"}
          prop = ["__", typ, "_mode__"];
          val = get (hf, prop);
          if (strcmp (state, "on"))
            set (hf, "__mouse_mode__", typ);
          endif
          val.Enable = state;
          set (hf, prop, val);

        case {"text", "select"}
          if (strcmp (state, "on"))
            set (hf, "__mouse_mode__", typ);
          endif
      endswitch

      if (strcmp (state, "on"))
        set (htools(htools != h), "state", "off");
      elseif (! any (strcmp (get (htools, "state"), "on")))
        set (hf, "__mouse_mode__", "none");
      endif
    endif

    recursion = false;
  endif

endfunction

function axes_cb (h, ~)

  hax = get (gcbf (), "currentaxes");
  if (! isempty (hax))
    if (strcmp (get (hax, "visible"), "on"))
      set (hax, "visible", "off");
    else
      set (hax, "visible", "on");
    endif
  endif

endfunction

function grid_cb (h, ~)

  hax = get (gcbf (), "currentaxes");
  if (! isempty (hax))
    if (strcmp (get (hax, "xgrid"), "on") && strcmp (get (hax, "ygrid"), "on"))
      grid (hax, "off");
    else
      grid (hax, "on");
    endif
  endif

endfunction

function auto_cb (h, ~)
  hax = get (gcbf (), "currentaxes");
  if (! isempty (hax))
    axis (hax, "auto");
  endif
endfunction

function clipboard_cb (h, ~)

  hf = gcbf ();
  fname = tempname ();
  props = {"inverthardcopy", "paperposition", "paperpositionmode"};
  values = get (hf, props);
  set (hf, "inverthardcopy", "off", "paperpositionmode", "auto");
  unwind_protect
    print ("-r0", "-dpng", "-svgconvert", fname);
    __event_manager_copy_image_to_clipboard__ (fname);
  unwind_protect_cleanup
    set (hf, props, values);
  end_unwind_protect

endfunction
