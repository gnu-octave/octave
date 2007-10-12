## Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
##               2006, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Undocumented internal function.

## Really decode plot option strings.

## Author: Rick Niles <niles@axp745.gsfc.nasa.gov>
## Adapted-By: jwe
## Maintainer: jwe

function [options, valid] = __pltopt1__ (caller, opt, err_on_invalid)

  options = __default_plot_options__ ();
  valid = true;

  more_opts = 1;

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (! ischar (opt))
    return;
  endif

  have_linestyle = false;
  have_marker = false;

  ## If called by __errplot__, extract the linestyle before proceeding.
  if (strcmp (caller,"__errplot__"))
    if (strncmp (opt, "#~>", 3))
      n = 3;
    elseif (strncmp (opt, "#~", 2) || strncmp (opt, "~>", 2))
      n = 2;
    elseif (strncmp (opt, "~", 1) || strncmp (opt, ">", 1) 
	    || strncmp (opt, "#", 1))
      n = 1;
    endif
      options.linestyle = opt(1:n);
      opt(1:n) = [];
      have_linestyle = true;
  endif

  while (! isempty (opt))
    if (strncmp (opt, "--", 2) || strncmp (opt, "-.", 2))
      options.linestyle = opt(1:2);
      have_linestyle = true;
      n = 2;
    else
      topt = opt(1);
      n = 1;
      if (topt == "-" || topt == ":")
	have_linestyle = true;
	options.linestyle = topt;
      elseif (topt == "+" || topt == "o" || topt == "*"
	      || topt == "." || topt == "x" || topt == "s"
	      || topt == "d" || topt == "^" || topt == "v"
	      || topt == ">" || topt == "<" || topt == "p"
	      || topt == "h" || topt == "@")
	have_marker = true;
	## Backward compatibility.  Leave undocumented.
	if (topt == "@")
	  topt = "+";
	endif
	options.marker = topt;
### Numeric color specs for backward compatibility.  Leave undocumented.
      elseif (topt == "k" || topt == "0")
	options.color = [0, 0, 0];
      elseif (topt == "r" || topt == "1")
	options.color = [1, 0, 0];
      elseif (topt == "g" || topt == "2")
	options.color = [0, 1, 0];
      elseif (topt == "b" || topt == "3")
	options.color = [0, 0, 1];
      elseif (topt == "y")
	options.color = [1, 1, 0];
      elseif (topt == "m" || topt == "4")
	options.color = [1, 0, 1];
      elseif (topt == "c" || topt == "5")
	options.color = [0, 1, 1];
      elseif (topt == "w" || topt == "6")
	options.color = [1, 1, 1];
      elseif (isspace (topt))
	## Do nothing.
      elseif (topt == ";")
	t = index (opt(2:end), ";");
	if (t)
	  options.key = undo_string_escapes (opt(2:t));
	  n = t+1;
	else
	  if (err_on_invalid)
            error ("%s: unfinished key label", caller);
	  else
	    valid = false;
	    options = __default_plot_options__ ();
	    return;
	  endif
        endif
      else
	if (err_on_invalid)
	  error ("%s: unrecognized format character: `%s'", caller, topt);
	else
	  valid = false;
	  options = __default_plot_options__ ();
	  return;
	endif
      endif
    endif
    opt(1:n) = [];
  endwhile

  if (have_marker && ! have_linestyle)
    options.linestyle = "none";
  endif

endfunction
