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
##
## See also: __pltopt1__

## Author: jwe
## Adapted-By: jwe
## Maintainer: jwe

function fmt = __pltopt__ (caller, opt)

  if (! isstr (opt))
    usage ("__pltopt__ (caller, opt)");
  endif

  nr = rows (opt);
  fmt = "";
  for i = 1:nr
    t = __pltopt1__ (caller, deblank (opt(i,:)));
    fmt(i,1:length(t)) = t;
  endfor

endfunction
