## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage: fmt = __pltopt__ (caller, opt)
##
## Decode plot option strings.
##
## If OPT is a valid option string, return a string of the form "w l 2"
## ("with lines 2").  Uses abbreviations for the options to avoid
## overrunning gnuplot's command line buffer unnecessarily.
##
## OPT can currently be some combination of the following:
##
##   "-"   for lines plot style (default).
##   "."   for dots plot style.
##   "@"   for points plot style.
##   "-@"  for linespoints plot style.
##   "^"   for impulses plot style.
##   "L"   for steps plot style.
##   "#"   for boxes plot style.
##   "~"   for errorbars plot style.
##   "#~"  for boxerrorbars plot style.
##   "n"   with n in 1-6 (wraps at 8), plot color
##   "nm"  with m in 1-6 (wraps at 6), point style (only valid for "@" or "-@")
##   "c"   where c is one of ["r", "g", "b", "m", "c", "w"] colors.
##   ";title;" where "title" is the label for the key.
##
##   Special points formats:
##
##      "+", "*", "o", "x" will display points in that style for term x11.
##
##   The legend may be fixed to include the name of the variable
##   plotted in some future version of Octave.
##
##   The colors, line styles, and point styles have the following
##   meanings for X11 and Postscript terminals under Gnuplot 3.6.
##
##   Number ------ Color -------  Line Style      ---- Points Style ----   
##          x11       postscript  postscript      x11         postscript   
##   =====================================================================
##     1    red       green       solid           "o"         "+"         
##     2    green     blue        long dash       "+"         "x"         
##     3    blue      red         short dash     square       "*"         
##     4    magenta   magenta     dotted          "x"        open square  
##     5    cyan      cyan        dot long dash  triangle    filled square
##     6    brown     yellow      dot short dash  "*"         "o"         

## Author: Rick Niles <niles@axp745.gsfc.nasa.gov>
## Adapted-By: jwe
## Maintainer: jwe

function fmt = __pltopt__ (caller, opt)

  set_color = 0;
  set_symbol = 0;
  set_lines = 0;
  set_dots = 0;
  set_points = 0;
  set_impulses = 0;
  set_steps = 0;
  set_boxes = 0;
  set_errbars = 0;
  set_key = 0;
  more_opts = 1;

  WITH = "w";
  LINES = "l";
  LINESPOINTS = "linesp";
  BOXERRORBARS = "boxer";
  BOXES = "boxes";
  POINTS = "p";
  DOTS = "d";
  IMPULSES = "i";
  STEPS = "s";
  ERRORBARS = "e";
  TITLE = "title";

  if (nargin != 2)
    usage ("__pltopt__ (opt)");
  endif

  if (! isstr (opt))
    error ("__pltopt__: argument must be a string");
  endif

  while (more_opts)

    ## First get next char.

    if (max (size (opt)) > 1)
#      [char, opt] = sscanf (opt, "%c %s", "C");
       char = opt(1);
       opt = opt(2:length(opt));
    else
      char = opt;
      more_opts = 0;
    endif

    ## Now set flags based on char.

    if (strcmp (char, "-"))
      set_lines = 1;
    elseif (strcmp (char, "."))
      set_dots  = 1;
    elseif (strcmp (char, "@"))
      set_points = 1;
    elseif (strcmp (char, "^"))
      set_impulses = 1;
    elseif (strcmp (char, "L"))
      set_steps = 1;
    elseif (strcmp (char, "~"))
      set_errbars = 1;
    elseif (strcmp (char, "#"))
      set_boxes = 1;
    elseif (strcmp (char, "0") || strcmp (char, "1") ...
            || strcmp (char, "2") || strcmp (char, "3") ...
            || strcmp (char, "4") || strcmp (char, "5") ...
            || strcmp (char, "6") || strcmp (char, "7") ...
            || strcmp (char, "8") || strcmp (char, "9"))
      if (set_color)
	set_points = 1;
	symbol = char;
	set_symbol = 1;
      else
	color = char;
	set_color = 1;
      endif
    elseif (strcmp (char, "r"))
      set_color = 1;
      color = "1";
    elseif (strcmp (char, "g"))
      set_color = 1;
      color = "2";
    elseif (strcmp (char, "b"))
      set_color = 1;
      color = "3";
    elseif (strcmp (char, "m"))
      set_color = 1;
      color = "4";
    elseif (strcmp (char, "c"))
      set_color = 1;
      color = "5";
    elseif (strcmp (char, "w"))
      set_color = 1;
      color = "6";
    elseif (strcmp (char, "*"))
      set_points = 1;
      set_symbol = 1;
      symbol = "6";
    elseif (strcmp (char, "+"))
      set_points = 1;
      set_symbol = 1;
      symbol = "2";
    elseif (strcmp (char, "o"))
      set_points = 1;
      set_symbol = 1;
      symbol = "1";
    elseif (strcmp (char, "x"))
      set_points = 1;
      set_symbol = 1;
      symbol = "4";
    elseif (strcmp (char, ";"))  # title mode.
      set_key = 1;
      working = 1;
      key_title = ""; 
      while (working)
        if (max (size (opt)) > 1)
	  char = opt(1);
	  opt = opt(2:length(opt));
        else
	  char = opt;
	  if (! strcmp (char, ";"))
            error ("%s: unfinished key label", caller);
          end
          more_opts = 0;
          working = 0;
        endif
        if strcmp (char, ";")
          working = 0;
        else
	  if (isempty (key_title))  # needs this to avoid empty matrix warning.
            key_title = char;
	  else
            key_title = strcat (key_title, char);
	  endif
        endif
      endwhile
    elseif (strcmp (char, " ")) 
      ## whitespace -- do nothing.
    else
      error ("%s: unrecognized format character: '%s'", caller, char);
    endif
  endwhile

  ## Now create format string.

  fmt = WITH;

  if (set_lines)
    if (set_points)
      fmt = strcat (fmt, " ", LINESPOINTS);
    else
      fmt = strcat (fmt, " ", LINES);
    endif
  elseif (set_boxes)
    if (set_errbars)
      fmt = strcat (fmt, " ", BOXERRORBARS);
    else
      fmt = strcat (fmt, " ", BOXES);
    endif
  elseif (set_points)
    fmt = strcat (fmt, " ", POINTS);
  elseif (set_dots)
    fmt = strcat (fmt, " ", DOTS);
  elseif (set_impulses)
    fmt = strcat (fmt, " ", IMPULSES);
  elseif (set_steps)
    fmt = strcat (fmt, " ", STEPS);
  elseif (set_errbars)
    fmt = strcat (fmt, " ", ERRORBARS);
  endif

  if (strcmp (fmt, WITH))
    fmt = strcat (fmt, " ", LINES);
  endif

  if (set_color)
    fmt = strcat (fmt, " ", color);
    if (set_symbol)
      fmt = strcat (fmt, " ", symbol);
    endif
  elseif (set_symbol)
    fmt = strcat (fmt, " 1 ", symbol);
  endif

  if (set_key)
    fmt = sprintf ("%s %s \"%s\" ", fmt, TITLE, key_title);
  endif
endfunction
