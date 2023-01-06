########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn  {} { } uisetfont ()
## @deftypefnx {} { } uisetfont (@var{h})
## @deftypefnx {} { } uisetfont (@var{fontstruct})
## @deftypefnx {} { } uisetfont (@dots{}, @var{title})
## @deftypefnx {} {@var{fontstruct} =} uisetfont (@dots{})
## Open a font selection dialog.
##
## If the first argument is a handle to a text, axes, or uicontrol object,
## pressing the OK button will change the font properties of the object.
##
## The first argument may also be a structure with fields @code{FontName},
## @code{FontWeight}, @code{FontAngle}, @code{FontUnits}, and @code{FontSize},
## indicating the initially selected font.
##
## The title of the dialog window can be specified by using the last argument
## @var{title}.
##
## If an output argument @var{fontstruct} is requested, the selected font
## structure is returned.  Otherwise, the font information is displayed
## onscreen.
##
## Programming Note: On systems that don't use FontConfig natively (all but
## Linux), the font cache is built when Octave is installed.  You will need to
## run @code{system ("fc-cache -fv")} manually after installing new fonts.
##
## @seealso{listfonts, text, axes, uicontrol}
## @end deftypefn

function varargout = uisetfont (varargin)

  persistent sysfonts = build_fontstruct ();
  persistent fontfields = {"FontName", "FontWeight", "FontAngle", ...
                           "FontUnits", "FontSize"};

  do_display = true;
  h = [];
  fontstruct = [];
  ttl = "Font";
  nargin = numel (varargin);

  ## Input checking
  if (nargin > 2)
    print_usage ();
  elseif (nargin == 0)
    ## Do nothing
  elseif (ishghandle (varargin{1}))

    h = varargin{1};
    typ = get (h, "type");
    if (! any (strcmp (typ, {"axes", "text", "uicontrol"})))
      error ("Octave:uisetfont:bad-object",
             "uisetfont: H must be a handle to an axes, text, or uicontrol object");
    endif
    nargin--;
    varargin(1) = [];
    do_display = false;

  elseif (isstruct (varargin{1}))

    fontstruct = varargin{1};
    fields = fieldnames (fontstruct);
    if (isempty (fields)
        || ! all (cellfun (@(s) any (strcmp (s, fontfields)), fields)))
      error ("Octave:uisetfont:bad-fontstruct",
             "uisetfont: FONTSTRUCT structure must have fields %s",
             strjoin (fontfields, ", "));
    endif
    nargin--;
    varargin(1) = [];

  endif

  ## Trailing TITLE argument
  if (nargin == 1)
    ttl = varargin{1};
    if (! (ischar (ttl) && isrow (ttl)))
      error ("Octave:uisetfont:bad-title",
             "uisetfont: TITLE must be a character vector");
    endif
  elseif (nargin == 2)
    print_usage ();
  endif

  ## Populate fontstruct
  persistent defstruct = [];
  if (isempty (defstruct))
    factory_fields = strcat ("factorytext", tolower (fontfields));
    values = get (0, factory_fields);
    defstruct = struct ([fontfields; values]{:});
  endif

  if (isempty (fontstruct))
    if (isempty (h))
      fontstruct = defstruct;
    else
      values = get (h, fontfields);
      fontstruct = struct ([fontfields; values]{:});
      names = {sysfonts.name};
      if (! any (strcmpi (fontstruct.FontName, {sysfonts.name})))
        warning ("Octave:uisefont:unknown-font",
                 "uisetfont: unknown font %s", fontstruct.FontName);
        fontstruct = defstruct;
      endif
    endif
  endif

  ## Sample string
  persistent str = {"Portez ce vieux whisky";
                    "au juge blond qui fume";
                    "0123456789";
                 ['\alpha, \beta, \gamma, \delta, \epsilon, \zeta, \eta, ' ...
                  '\theta, \vartheta, \iota, \kappa, \lambda, \mu, \nu, '];
                 ['\xi, \o, \pi, \varpi, \rho, \sigma, \varsigma, \tau, ' ...
                  '\upsilon, \phi, \chi, \psi, \omega']};

  ## Run the dialog
  warning ("off", "Octave:missing-glyph", "local");
  warning ("off", "Octave:substituted-glyph", "local");
  hf = run_fontdialog (sysfonts, h, fontstruct, ttl, str);

  ## Now wait for a button to be pressed or the figure to be closed
  uiwait (hf);

  fontstruct = [];
  if (ishghandle (hf))
    fontstruct = getappdata (hf, "__uisetfont_struct__");
    if (! isempty (h) && ! isempty (fontstruct))
      set (h, fontstruct);
    endif
    close (hf);
  endif

  if (nargout > 0)
    varargout{1} = fontstruct;
  elseif (do_display && ! isempty (fontstruct))
    disp (fontstruct);
  endif

endfunction

function fonts = build_fontstruct ()

  fontfiles = __get_system_fonts__ ();
  families = unique ({fontfiles.family});

  fonts(numel (families)+1) = struct ("name", "",
                                      "has_regular", false,
                                      "has_bold", false,
                                      "has_italic", false,
                                      "has_bold_italic", false);

  fonts(1) = struct ("name", "*",
                     "has_regular", true,
                     "has_bold", true,
                     "has_italic", true,
                     "has_bold_italic", true);

  for i = 1:numel (families)
    ii = i + 1;
    fonts(ii).name = families{i};
    idx = strcmp ({fontfiles.family}, families{i});

    isbold = strcmp ({fontfiles(idx).weight}, "bold");
    isitalic = strcmp ({fontfiles(idx).angle}, "italic");

    fonts(ii).has_regular = any (! isbold & ! isitalic);
    fonts(ii).has_bold = any (isbold & ! isitalic);
    fonts(ii).has_italic = any (isitalic & ! isbold);
    fonts(ii).has_bold_italic = any (isbold & isitalic);
  endfor

endfunction

function hf = run_fontdialog (sysfonts, hobj, fontstruct, ttl, str)

  [hf, hok, hcancel, hp] = __ok_cancel_dlg__ (ttl,
                                              "position", [200 200 400 400],
                                              "windowstyle", "modal",
                                              "resize", "on");

  ## List controls
  htmp = uipanel (hp, "title", "Font Name",
                      "units", "normalized", "position", [0.04 0.35 0.5 0.6]);
  hnames = uicontrol (htmp, "style", "listbox", "string", {sysfonts.name},
                            "units", "normalized",
                            "position", [0.02 0.01 0.96 .95]);

  htmp = uipanel (hp, "title", "Style",
                      "units", "normalized", "position", [0.56 0.35 0.25 0.6]);
  hstyle = uicontrol (htmp, "style", "listbox",
                      "units", "normalized",
                      "position", [0.02 0.01 0.96 .95]);

  htmp = uipanel (hp, "title", "Size",
                      "units", "normalized", "position", [0.83 0.35 0.13 0.6]);
  hsize = uicontrol (htmp, "style", "listbox",
                           "string", arrayfun (@num2str, (8:30), "uni", false),
                           "units", "normalized",
                           "position", [0.02 0.01 0.96 .95]);

  fcn = @(h) set (hstyle, "string",
                          getstylestring (sysfonts(get (h, "value"))));
  set (hnames, "callback", fcn);

  ## Axes to display samples
  htmp = uipanel (hp, "title", "Sample",
                      "units", "normalized", "position", [0.04 0 0.92 0.33]);
  hax = axes ("parent", htmp, "visible", "off", "units", "normalized",
              "position", [0 0 1 0.95], "xlim", [0 1], "ylim", [0 1]);
  ht = text (hax, 0.5, 0.5, str, "horizontalalignment", "center");

  hlists = [hnames, hstyle, hsize];

  ## Update text and uicontrol objects according to the input fontstruct
  struct_to_lists (fontstruct, sysfonts, hlists);
  set (ht, fontstruct);

  ## Setup callbacks
  set (hlists, "callback", {@cb_list_value_changed, hlists, ht, sysfonts});

  set (hok, "callback", {@cb_button, hlists, "ok"});
  set (hcancel, "callback", {@cb_button, hlists, "cancel"});

  ## Give focus to the OK button
  uicontrol (hok);

endfunction

function str = getstylestring (fontitem)

  styles = {"Plain", "Bold", "Italic", "Bold Italic"};
  if (fontitem.has_bold_italic)
    str = styles;
  elseif (fontitem.has_bold && fontitem.has_italic)
    str = styles(1:3);
  elseif (fontitem.has_bold)
    str = styles(1:2);
  elseif (fontitem.has_italic)
    str = styles(1:2:3);
  else
    str = styles{1};
  endif

endfunction

function fontstruct = struct_from_lists (hlists)

  name = get (hlists(1), "string");
  if (iscell (name))
    name = name{get(hlists(1), "value")};
  endif

  szstr = get (hlists(3), "string");
  sz = str2num (szstr{get(hlists(3), "value")});

  fontstruct = struct ("FontName", name, "FontWeight", "normal",
                       "FontAngle", "normal", "FontUnits", "points",
                       "FontSize", sz);

  style = get (hlists(2), "string");
  if (iscell (style))
    style = style{get(hlists(2), "value")};
  endif

  if (strcmp (style, "Bold"))
    fontstruct.FontWeight = "bold";
  elseif (strcmp (style, "Bold Italic"))
    fontstruct.FontWeight = "bold";
    fontstruct.FontAngle = "italic";
  elseif (strcmp (style, "Italic"))
    fontstruct.FontAngle = "italic";
  endif

endfunction

function struct_to_lists (fontstruct, sysfonts, hlists)

  ## Match font name
  names = get (hlists(1), "string");
  idx = find (strcmpi (fontstruct.FontName, names));
  if (isempty (idx))
    idx = 1;
  endif
  set (hlists(1), "value", idx);
  styles = getstylestring (sysfonts(idx));
  set (hlists(2), "string", styles);

  ## Match style
  style = "Plain";
  if (strcmp (fontstruct.FontWeight, "bold")
      && strcmp (fontstruct.FontAngle, "italic"))
    style = "Bold Italic";
  elseif (strcmp (fontstruct.FontWeight, "bold"))
    style = "Bold";
  elseif (strcmp (fontstruct.FontAngle, "italic"))
    style = "Italic";
  endif

  idx = find (strcmpi (style, styles));
  if (isempty (idx))
    idx = 1;
  endif
  set (hlists(2), "value", idx);

  ## Match size
  szs = (8:30);
  idx = find (round (fontstruct.FontSize) == szs);
  if (isempty (idx))
    idx = 1;
  endif
  set (hlists(3), "value", idx);

endfunction

function cb_button (h, ~, hlists, role)

  fontstruct = [];
  if (strcmp (role, "ok"))
    fontstruct = struct_from_lists (hlists);
  endif

  setappdata (gcbf (), "__uisetfont_struct__", fontstruct);
  uiresume (gcbf ());

endfunction

function cb_list_value_changed (h, ~, hlists, htext, sysfonts)

  if (h == hlists(1))
    set (hlists(2), "string", getstylestring (sysfonts(get (h, "value"))),
                    "value", 1);
  endif
  fontstruct = struct_from_lists (hlists);
  set (htext, fontstruct);

endfunction


## Test input validation
%!testif HAVE_FONTCONFIG
%! fail ("uisetfont (1, 2, 3)", "Invalid call");
%!testif HAVE_FONTCONFIG
%! fail ("uisetfont (110, struct ())", "Invalid call");
%!testif HAVE_FONTCONFIG
%! fail ("uisetfont (groot ())", "H must be a handle to an axes");
%!testif HAVE_FONTCONFIG
%! fail ("uisetfont (struct ())", "FONTSTRUCT .* must have fields FontName,.*");
%!testif HAVE_FONTCONFIG
%! fail ("uisetfont ({'Title'})", "TITLE must be a character vector");
