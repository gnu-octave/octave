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

function saveimage (filename, X, img_form, map)

# Save a matrix to disk in image format.
# 
# saveimage (filename, x) saves matrix x to file filename in octave's
# image format.  The current colormap is saved in the file also.
# 
# saveimage (filename, x, "img") saves the image in the default format
# and is the same as saveimage (filename, x).
# 
# saveimage (filename, x, "ppm") saves the image in ppm format instead
# of the default octave image format.
# 
# saveimage (filename, x, "ps") saves the image in PostScript format
# instead of the default octave image format. (Note: images saved in
# PostScript format can not be read back into octave with loadimage.)
# 
# saveimage (filename, x, format, map) saves the image along with the
# specified colormap in the specified format.
# 
# Note: If the colormap contains only two entries and these entries
# are black and white, the bitmap ppm and PostScript formats are used.
# If the image is a gray scale image (the entries within each row of
# the colormap are equal) the gray scale ppm and PostScript image
# formats are used, otherwise the full color formats are used.
# 
# SEE ALSO: loadimage, save, load, colormap

# Written by Tony Richardson (amr@mpl.ucsd.edu) July 1994.

  if (nargin < 2 || nargin > 4)
    usage ("saveimage (filename, matrix, [format, [colormap]])");
  endif

  if (nargin < 4)
    map = colormap ();
  endif
  if (columns (map) != 3)
    error ("colormap should be an N x 3 matrix");
  endif

  if (nargin < 3)
    img_form = "img";
  elseif (! isstr (img_form))
    error ("image format specification must be a string");
  elseif (! (strcmp (img_form, "img")
             || strcmp (img_form, "ppm")
	     || strcmp (img_form, "ps")))
    error ("unsupported image format specification");     
  endif

  if (! is_matrix (X))
    warning ("image variable is not a matrix");
  endif

  if (! isstr (filename))
    error ("file name must be a string");
  endif

# If we just want Octave image format, save and return.

  if (strcmp (img_form, "img"))
    eval (strcat ("save -ascii ", filename, " map X"));
    return;
  endif

  unwind_protect

    oct_file = octave_tmp_file_name ();

# Save image in Octave image file format

    eval (strcat ("save -ascii ", oct_file, " map X"));

# Convert to another format if requested.

    if (strcmp (img_form, "ppm"))
      shell_cmd (sprintf ("octtopnm %s > %s", oct_file, filename));
    elseif (strcmp (img_form, "ps") == 1)
      shell_cmd (sprintf ("octtopnm %s | pnmtops > %s 2> /dev/null",
                          oct_file, filename));
    endif

  unwind_protect_cleanup

    shell_cmd (sprintf ("rm -f %s", oct_file));

  end_unwind_protect

endfunction
