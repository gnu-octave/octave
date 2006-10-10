## Copyright (C) 1996, 1997, 2002 John W. Eaton
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
## @deftypefn {Function File} {} contour (@var{z}, @var{n})
## @deftypefnx {Function File} {} contour (@var{x}, @var{y}, @var{z}, @var{n})
## Make a contour plot of the three-dimensional surface described by
## @var{z}.  Someone needs to improve @code{gnuplot}'s contour routines
## before this will be very useful.
## @seealso{plot, mesh, meshgrid}
## @end deftypefn

## Author: jwe

function contour (x, y, z, n)

  ## FIXME -- these plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  if (nargin == 1 || nargin == 2)
    z = x;
    if (nargin == 1) 
      n = 10;
    else
      n = y; 
    endif
    if (ismatrix (z))
      unwind_protect
	__gnuplot_raw__ ("set nosurface;\n");
	__gnuplot_raw__ ("set contour;\n");
	__gnuplot_raw__ ("set cntrparam bspline;\n");
	if (isscalar (n))
          command = sprintf ("set cntrparam levels %d;\n", n);
	elseif (isvector (n))
          tmp = sprintf ("%f", n(1));
          for i = 2:length (n)
            tmp = sprintf ("%s, %f", tmp, n(i));
          endfor
          command = sprintf ("set cntrparam levels discrete %s;\n", tmp);
	else
	  error ("contour: levels must be a scalar or vector") ;
	endif
	__gnuplot_raw__ (command);
	__gnuplot_set__ parametric;
	__gnuplot_raw__ ("set view 0, 0, 1, 1;\n");
	__plt3__ (z, "", "", [gnuplot_command_with, " l 1"]);
      unwind_protect_cleanup
	__gnuplot_set__ noparametric;
      end_unwind_protect

    else
      error ("contour: z of contour (z, levels) must be a matrix");
    endif
  elseif (nargin == 3 || nargin == 4)
    if (nargin == 3)
      n = 10;
    endif
    if (ismatrix (z))
      size_msg = ["contour: columns(z) must be the same as length(x) and\n" \
		  "rows(z) must be the same as length(y),\n" \
		  "or x, y, and z must be matrices with the same size"];
      if (isvector (x) && isvector (y))
	xlen = length (x);
	ylen = length (y);
	if (xlen == columns (z) && ylen == rows (z))
          if (rows (x) == 1)
            x = x';
          endif
          len = 3 * ylen;
          zz = zeros (xlen, len);
          k = 1;
          for i = 1:3:len
            zz(:,i)   = x;
            zz(:,i+1) = y(k) * ones (xlen, 1);
            zz(:,i+2) = z(k,:)';
            k++;
          endfor
	else
          error (size_msg);
	endif
      else
	z_size = size (z);
	if (z_size == size (x) && z_size == size (y))
	  nc = 3*z_size(1);
	  zz = zeros (z_size(2), nc);
	  zz(:,1:3:nc) = x';
	  zz(:,2:3:nc) = y';
	  zz(:,3:3:nc) = z';
	else
	  error (size_msg);
	endif
      endif
      unwind_protect
	__gnuplot_raw__ ("set nosurface;\n");
	__gnuplot_raw__ ("set contour;\n");
	__gnuplot_raw__ ("set cntrparam bspline;\n");
	if (isscalar (n))
          command = sprintf ("set cntrparam levels %d;\n", n);
	elseif (isvector (n))
          tmp = sprintf ("%f", n(1));
          for i = 2:length (n)
            tmp = sprintf ("%s, %f", tmp, n(i));
          endfor
          command = sprintf ("set cntrparam levels discrete %s;\n", tmp);
	else
	  error ("contour: levels must be a scalar or vector") ;
	endif
	__gnuplot_raw__ (command);
	__gnuplot_set__ parametric;
	__gnuplot_raw__ ("set view 0, 0, 1, 1;\n");
	__plt3__ (zz, "", "", [gnuplot_command_with, " l 1"]);
      unwind_protect_cleanup
	__gnuplot_set__ noparametric;
      end_unwind_protect
    else
      error ("contour: x and y must be vectors and z must be a matrix");
    endif
  else
    print_usage ();
  endif

endfunction
