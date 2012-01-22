## Copyright (C) 2004-2012 John W. Eaton
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

## -*- texinfo -*-
## @deftypefn  {Function File} {} delete (@var{file})
## @deftypefnx {Function File} {} delete (@var{handle})
## Delete the named file or graphics handle.
##
## Deleting graphics objects is the proper way to remove
## features from a plot without clearing the entire figure.
## @seealso{clf, cla, unlink}
## @end deftypefn

## Author: jwe

function delete (arg)

  if (nargin != 1)
    print_usage ();
  endif

  if (ischar (arg))
    files = glob (arg);
    if (isempty (files))
      warning ("delete: no such file: %s", arg);
    endif
    for i = 1:length (files)
      file = files{i};
      [err, msg] = unlink (file);
      if (err)
        warning ("delete: %s: %s", file, msg);
      endif
    endfor
  elseif (all (ishandle (arg(:))))
    ## Delete a graphics object.
    __go_delete__ (arg);
  else
    error ("delete: first argument must be a filename or graphics handle");
  endif

endfunction


%% Test input validation
%!error delete ()
%!error delete (1, 2)
%!error <first argument must be a filename> delete (struct ())

