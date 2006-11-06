## Copyright (C) 2006 Daniel Sebald
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
## @deftypefn {Function File} {} __img__ (@var{x}, @var{y}, @var{A})
## Display an image using @code{gnuplot}, where vectors @var{x} and
## @var{y} define the axes and the matrix @var{A} contains the image
## data.
## @end deftypefn

function __img__ (x, y, A)

  if (nargin != 3)
    print_usage ();
  endif

  if (isempty (A))
    error ("image matrix is empty");
  endif

  __gnuplot_raw__ ("set nokey\n");
  __gnuplot_raw__ ("set size ratio -1;\n");
  __current_color_map__ = colormap ();
  palette_size = size (__current_color_map__, 1);
  __gnuplot_raw__ (sprintf ("set palette positive color model RGB maxcolors %i\n", palette_size));
  if (palette_size <= 128)
    ## Break up command to avoid buffer overflow.
    __gnuplot_raw__ ("set palette file \"-\"\n");
    for i = 1:palette_size
      __gnuplot_raw__ (sprintf ("%g %g %g %g\n", 1e-3 * round (1e3 * [(i-1)/(palette_size-1), __current_color_map__(i,:)])));
    end
    __gnuplot_raw__("e\n");
  else
    ## Let the file be deleted when Octave exits or `purge_tmp_files' is called.
    [fid, binary_file_name, msg] = mkstemp ([P_tmpdir,"/gpimageXXXXXX"], 1);
    fwrite (fid, __current_color_map__', "float32", 0, "ieee-le");
    fclose (fid);
    __gnuplot_raw__ (sprintf ("set palette file \"%s\" binary record=%d using 1:2:3\n", binary_file_name, palette_size));
  endif

  ## Use the newly added mode of "plot" called "with image".
  if (isempty (x))
    x = [1, size(A,2)];
    y = [1, size(A,1)];
  endif

  ## Force rectangular grid by using only end points of
  ## first row (column) if x (y) is a matrix or vector.
  if ((size (x, 2)) > 1)
    x = x(1,:)';
  endif
  if (abs (x(end) - x(1)) < (10*eps))
    error ("end points in x dimension must not be equal");
  else
    x_dim = size (A, 2);
    if (x_dim > 1)
      dx = (x(end)-x(1))/(x_dim-1);
    else
      dx = 1;
    endif
  endif
  if ((size (y, 1)) > 1)
    y = y(:,1)';
  endif
  if (abs(y(end) - y(1)) < 10*eps)
    error ("end points in y dimension must not be equal");
  else
    y_dim = size (A, 1);
    if (y_dim > 1)
      dy = (y(end)-y(1))/(y_dim-1);
    else
      dy = 1;
    endif
  endif

  __gnuplot_raw__ (sprintf ("set xrange [%g:%g]\n", x(1)-dx/2, x(end)+dx/2));
  __gnuplot_raw__ (sprintf ("set yrange [%g:%g]\n", y(1)-dy/2, y(end)+dy/2));
  __gnuplot_raw__ ("set autoscale fix\n"); # "fix" is helpful for "a" hotkey
  __gnuplot_raw__ ("set tics out\n");

  A = reshape(A,size(A,1)*size(A,2),1);
  ## Let the file be deleted when Octave exits or `purge_tmp_files' is called.
  [fid, binary_file_name, msg] = mkstemp ([P_tmpdir, "/gpimageXXXXXX"], 1);
  ## Gnuplot reads binary files very quickly.  However, the 'fwrite' below
  ## is much slower than using the current '__gnuplot_plot__' command.
  fwrite (fid, A', "float", 0, "ieee-le");
  fclose (fid);
  __gnuplot_raw__ (sprintf ("plot \"%s\" binary array=%dx%d scan=yx flipy origin=(%g,%g) dx=%g dy=%g endian=little using 1 with image\n", binary_file_name, x_dim, y_dim, min (x(1), x(end)), min(y(1), y(end)), abs (dx), abs (dy)));
  ## Put back in default data mode.
  __gnuplot_raw__ ("set xrange [*:*];\n");
  __gnuplot_raw__ ("set yrange [*:*];\n");

endfunction
