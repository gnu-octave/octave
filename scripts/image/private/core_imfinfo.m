## Copyright (C) 2008-2012 Soren Hauberg
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

## This function does al the work of imfinfo. It exists here as private
## function so that imfinfo can use other functions if imformats is
## configured to. It is also needed so that imformats can create a
## function handle for it.

## Author: Soren Hauberg <hauberg@gmail.com>

function info = core_imfinfo (filename)

  if (nargin < 1)
    print_usage ("imfinfo");
  endif

  if (! ischar (filename))
    error ("imfinfo: FILENAME must be a string");
  endif
  filename = tilde_expand (filename);

  delete_file = false;
  unwind_protect

    fn = file_in_path (IMAGE_PATH, filename);

    if (isempty (fn))
      ## Couldn't find file. See if it's an URL.
      [fn, status, msg] = urlwrite (filename, tmpnam ());
      if (! status)
        error ("imfinfo: cannot find %s", filename);
      endif

      if (! isempty (fn))
        delete_file = true;
      endif
    endif
    info = __magick_finfo__ (fn);

  unwind_protect_cleanup
    if (delete_file)
      unlink (fn);
    endif
  end_unwind_protect

endfunction
