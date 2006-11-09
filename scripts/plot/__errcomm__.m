## Copyright (C) 2001, 2002 Teemu Ikonen
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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __errcomm__ (@var{args})
## Common argument handling code for all error plots (errorbar, loglogerr,
## semilogyerr, semilogxerr).
## @seealso{errorbar, semilogxerr, semilogyerr, loglogerr, __pltopt__}
## @end deftypefn

## Created: 20.02.2001
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function __errcomm__ (caller, varargin)

  if (nargin < 3)
    usage ("%s (x, y, dy, \"fmt\", ...)", caller);
  endif

  nargs = length (varargin);
  save_hold = ishold;
  unwind_protect
    if (! ishold)
      clg
    endif
    hold on;
    k = 1;
    data = cell(6,1);
    while (k <= nargs)
      a = varargin{k++};
      if (isvector (a))
        a = a(:);
      elseif (ismatrix (a))
        ;
      else
        usage ("%s (...)", caller);
      endif
      sz = size (a);
      ndata = 1;
      data{ndata} = a;
      while (k <= nargs)
	a = varargin{k++};
	if (ischar (a) || iscellstr (a))
	  __errplot__ (a, data{1:ndata});
	  break;
	elseif (isvector (a))
	  a = a(:);
	elseif (ismatrix (a))
	  ;
	else
	  error ("wrong argument types");
	endif
	if (size (a) != sz)
	  error ("argument sizes do not match");
	endif
	data{++ndata} = a;
	if (ndata > 6)
	  error ("too many arguments to a plot");
	endif
      endwhile
    endwhile

    if (! (ischar (a) || iscellstr (a)))
      __errplot__ ("~", data{1:ndata});
    endif
  unwind_protect_cleanup
    if (! save_hold)
      hold off;
    endif
  end_unwind_protect

endfunction
