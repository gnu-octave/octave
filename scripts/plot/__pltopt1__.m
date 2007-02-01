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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __pltopt1__ (@var{caller}, @var{opt})
## Really decode plot option strings.
## @seealso{__pltopt__}
## @end deftypefn

## Author: Rick Niles <niles@axp745.gsfc.nasa.gov>
## Adapted-By: jwe
## Maintainer: jwe

function options = __pltopt1__ (caller, opt)

  options = __default_plot_options__ ();

  more_opts = 1;

  if (nargin != 2)
    print_usage ();
  endif

  if (! ischar (opt))
    return;
  endif

  while (! isempty (opt))
    if (strncmp (opt, "--", 2) || strncmp (opt, "-.", 2))
      options.linestyle = opt(1:2);
      n = 2;
    else
      topt = opt(1);
      n = 1;
      if (topt == "-" || topt == ":")
	options.linestyle = topt;
      elseif (topt == "+" || topt == "o" || topt == "*"
	      || topt == "." || topt == "x" || topt == "s"
	      || topt == "d" || topt == "^" || topt == "v"
	      || topt == ">" || topt == "<" || topt == "p"
	      || topt == "h")
	options.marker = topt;
      elseif (topt == "k")
	options.color = [0, 0, 0];
      elseif (topt == "r")
	options.color = [1, 0, 0];
      elseif (topt == "g")
	options.color = [0, 1, 0];
      elseif (topt == "b")
	options.color = [0, 0, 1];
      elseif (topt == "y")
	options.color = [1, 1, 0];
      elseif (topt == "m")
	options.color = [1, 0, 1];
      elseif (topt == "c")
	options.color = [0, 1, 1];
      elseif (topt == "w")
	options.color = [1, 1, 1];
      elseif (isspace (topt))
	## Do nothing.
      elseif (topt == ";")
	t = index (opt(2:end), ";");
	if (t)
	  options.key = undo_string_escapes (opt(2:t));
	  n = t+1;
	else
          error ("%s: unfinished key label", caller);
        endif
      else
	error ("%s: unrecognized format character: `%s'", caller, topt);
      endif
    endif
    opt(1:n) = [];
  endwhile

endfunction
