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

## -*- texinfo -*-
## @deftypefn {Function File} {} figure (@var{n})
## Set the current plot window to plot window @var{n}.  This function
## currently requires X11 and a version of gnuplot that supports multiple
## frames.  If @var{n} is not specified, the next available window
## number is chosen.
## @end deftypefn

## Author: jwe

function f = figure (n)

  static figure_list = create_set (0);
  static figure_called = 0;

  if (nargin == 0)
    f = max (figure_list) + 1;
  else
    f = n;
  endif

  if (nargin < 2)
    if (gnuplot_has_frames)
      gnuterm = getenv ("GNUTERM");
      if (isempty (gnuterm) && ! isempty ("DISPLAY"))
	gnuterm = "x11";
      endif
      if (! isempty (gnuterm))
        oneplot ();
        figure_list = union (figure_list, f);
        eval (sprintf ("__gset__ term %s %d\n", gnuterm, f));
      else
        error ("figure: requires GNUTERM (Aqua) or DISPLAY (X11)");
      endif
    else
      error ("figure: gnuplot doesn't appear to support this feature");
    endif
  elseif (rem (nargin, 2) == 0)
    if (! figure_called)
      figure_called = 1;
      warning ("figure: setting figure properties is unsupported");
    endif
  else
    usage ("figure (n)");
  endif

endfunction
