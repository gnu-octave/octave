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
## @deftypefn {Function File} {} image (@var{x}, @var{zoom})
## Display a matrix as a color image.  The elements of @var{x} are indices
## into the current colormap and should have values between 1 and the
## length of the colormap.  If @var{zoom} is omitted, a value of 4 is
## assumed.
## @end deftypefn
## @seealso{imshow, imagesc, and colormap}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function image (x, zoom)

  if (nargin == 0)
    ## Load Bobbie Jo Richardson (Born 3/16/94)
    x = loadimage ("default.img");
    zoom = 2;
  elseif (nargin == 1)
    zoom = 4;
  elseif (nargin > 2)
    usage ("image (matrix, [zoom])");
  endif

  ppm_name = tmpnam ();

  saveimage (ppm_name, x, "ppm");

  ## Start the viewer.  Try xv, then xloadimage.

  xv = sprintf ("xv -expand %f %s", zoom, ppm_name);

  xloadimage = sprintf ("xloadimage -zoom %f %s", zoom*100, ppm_name);

  rm = sprintf ("rm -f %s", ppm_name);

  ## Need to let the shell clean up the tmp file because we are putting
  ## the viewer in the background.

  command = sprintf ("( %s || %s && %s ) > /dev/null 2>&1 &",
                     xv, xloadimage, rm);

  system (command);

endfunction
