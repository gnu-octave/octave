## Copyright (C) 2004, 2006, 2008 Petr Mikulik
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

## This is ginput.m implementation for gnuplot and X11.
## It requires gnuplot 4.1 and later.

## This file initially bore the copyright statement
## Petr Mikulik
## History: June 2006; August 2005; June 2004; April 2004
## License: public domain

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{buttons}] =} __gnuplot_ginput__ (@var{f}, @var{n})
## Undocumented internal function.
## @end deftypefn

function [x, y, button] = __gnuplot_ginput__ (f, n)

  stream = get (f, "__plot_stream__");

  if (compare_versions (__gnuplot_version__ (), "4.0", "<="))
    error ("ginput: version %s of gnuplot not supported", gnuplot_version ());
  endif

  if (nargin == 0)
    x = zeros (n, 1);
    y = zeros (n, 1);
    button = zeros (n, 1);
  else
    x = zeros (n, 1);
    y = zeros (n, 1);
    button = zeros (n, 1);
  endif

  gpin_name = tmpnam ();

  ## 6*8*8 ==  0600
  [err, msg] = mkfifo (gpin_name, 6*8*8);
  if (err != 0)
    error ("ginput: Can not open fifo (%s)", msg);
  endif    

  unwind_protect

    k = 0;
    while (true)
      k++;
      fprintf (stream, "set print \"%s\";\n", gpin_name);
      fflush (stream);
      gpin = fopen (gpin_name, "r");
      fputs (stream, "pause mouse any;\n\n");

      ## Notes: MOUSE_* can be undefined if user closes gnuplot by "q"
      ## or Alt-F4. Further, this abrupt close also requires the leading
      ## "\n" on the next line.
      fputs (stream, "\nif (exists(\"MOUSE_KEY\") && exists(\"MOUSE_X\")) print MOUSE_X, MOUSE_Y, MOUSE_KEY; else print \"0 0 -1\"\n");

      ## Close output file, otherwise all blocks (why?!).
      fputs (stream, "set print;\n");
      fflush (stream);

      ## Now read from fifo.
      [x(k), y(k), button(k), count] = fscanf (gpin, "%f %f %d", "C");
      fclose (gpin);

      if ([x(k), y(k), button(k)] == [0, 0, -1])
	## Mousing not active (no plot yet).
	break;
      endif

      if (nargin > 0)
	## Input argument n was given => stop when k == n.
	if (k == n) 
	  break; 
	endif
      else
	## Input argument n not given => stop when hitting a return key.
	## if (button(k) == 0x0D || button(k) == 0x0A) 
	##   ## hit Return or Enter
	if (button(k) == 0x0D)
	  ## hit Return
	  x(k:end) = [];
	  y(k:end) = [];
	  button(k:end) = [];
	  break;
	endif
      endif
    endwhile

  unwind_protect_cleanup
    unlink (gpin_name);
  end_unwind_protect

endfunction

