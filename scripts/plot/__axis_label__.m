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
## @deftypefn {Function File} {} __axis_label__ (@var{caller}, @var{text})
## Utility function for @code{xlabel}, @code{ylabel}, and @code{zlabel}.
## @end deftypefn

## Author: jwe

function h = __axis_label__ (caller, text)

  if (nargin == 0)
    usage ("__axis_label__ (caller, text)");
  elseif (nargin == 2)
    if (isstr (text))
      eval (sprintf ("__gnuplot_set__ %s \"%s\"", caller,
		     undo_string_escapes (undo_string_escapes (text))));
      if (automatic_replot)
	replot ();
      endif
    else
      error ("%s: text must be a string", caller);
    endif
  else
    usage ("%s (text)", caller);
  endif

  ## XXX FIXME XXX -- eventually, we will return a graphics handle.  For
  ## now, return something, so that calls that expect a handle won't
  ## fail (at least immediately).

  if (nargout > 0)
    h = -1;
  endif

endfunction
