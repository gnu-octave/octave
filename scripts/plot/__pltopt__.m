# Copyright (C) 1994, 1995 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

# Originally written by Rick Niles <niles@axp745.gsfc.nasa.gov>.

function fmt = plot_opt (caller, opt)

# usage: fmt = plot_opt (caller, opt)
#
# Decode plot option strings.
#
# If OPT is a valid option string, return a string of the form "w l 2"
# ("with lines 2").  Uses abbreviations for the options to avoid
# overrunning gnuplot's command line buffer unnecessarily.
#
# OPT can currently be some combination of the following:
#
#   "-"   for lines plot style (default).
#   "."   for dots plot style.
#   "@"   for points plot style.
#   "-@"  for linespoints plot style.
#   "^"   for impulses plot style.
#   "L"   for steps plot style.
#   "#"   for boxes plot style.
#   "~"   for errorbars plot style.
#   "#~"  for boxerrorbars plot style.
#   "n"   with n in 1-6 (wraps at 8), plot color
#   "nm"  with m in 1-6 (wraps at 6), point style (only valid with "@" or "-@")
#   "c"   where c is one of ["r", "g", "b", "m", "c", "w"] colors.
#
#   Special points formats:
#
#      "+", "*", "o", "x" will display points in that style.
#
#   The legend may be fixed to include the name of the variable
#   plotted in some future version of Octave.
#
#   The color line styles have the following meanings on terminals
#   that support color.
#
#     Number  Gnuplot colors     (lines)points style
#       1       red                 "*"
#       2       green               "+"
#       3       blue                "o"
#       4       magenta             "x"
#       5       cyan                house
#       6       brown               there exists

  set_color = 0;
  set_symbol = 0;
  set_lines = 0;
  set_dots = 0;
  set_points = 0;
  set_impulses = 0;
  set_steps = 0;
  set_boxes = 0;
  set_errbars = 0;
  more = 1;

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

  if (nargin != 2)
    usage ("plot_opt (opt)");
  endif

  if (! isstr (opt))
    error ("plot_opt: argument must be a string");
  endif

  while (more)

# First get next char.

    if (max (size (opt)) > 1)
      [char, opt] = sscanf (opt, "%c %s");
    else
      char = opt;
      more = 0;
    endif

# Now set flags based on char.

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
      symbol = "1";
    elseif (strcmp (char, "+"))
      set_points = 1;
      set_symbol = 1;
      symbol = "2";
    elseif (strcmp (char, "o"))
      set_points = 1;
      set_symbol = 1;
      symbol = "3";
    elseif (strcmp (char, "x"))
      set_points = 1;
      set_symbol = 1;
      symbol = "4";
    else
      error (sprintf ("%s: unrecognized format character %s", caller, char));
    endif
  endwhile

# Now create format string.

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

endfunction
