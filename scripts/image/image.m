# Copyright (C) 1995 John W. Eaton
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

function image (x, zoom)

# Display an octave image matrix.
#
# image (x) displays a matrix as a color image. The elements of x are
# indices into the current colormap and should have values between 1
# and the length of the colormap.
#
# image (x, zoom) changes the zoom factor.  The default value is 4.
#
# SEE ALSO: imshow, imagesc, colormap.

# Written by Tony Richardson (amr@mpl.ucsd.edu) July 1994.

  if (nargin == 0)
# Load Bobbie Jo Richardson (Born 3/16/94)
    x = loadimage ("default.img");
    zoom = 2;
  elseif (nargin == 1)
    zoom = 4;
  elseif (nargin > 2)
    usage ("image (matrix, [zoom])");
  endif

# XXX FIXME XXX -- we should use octave_tmp_file_name.

  rnd_str = num2str (fix (rand * 10000));
  ppm_name = ["image.", rnd_str, ".ppm" ];

  saveimage (ppm_name, x, "ppm");

# Start the viewer.  Try xv, then xloadimage.

  xv = sprintf ("xv -expand %f %s", zoom, ppm_name);
  xloadimage = sprintf ("xloadimage -zoom %f %s", zoom*100, ppm_name);
  rm = sprintf ("rm -f %s", ppm_name);

  command = sprintf ("( %s || %s && %s ) > /dev/null 2>&1 &", ...
                     xv, xloadimage, rm);

  system (command);

endfunction
