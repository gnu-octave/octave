## Copyright (C) 2006, 2007, 2009 Sylvain Pelissier
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
## @deftypefn {Function File} {@var{entries} =} zip (@var{zipfile}, @var{files})
## @deftypefnx {Function File} {@var{entries} =} zip (@var{zipfile}, @var{files}, @var{rootdir})
## Compress the list of files and/or directories specified in @var{files} 
## into the archive @var{zipfiles} in the same directory.  If @var{rootdir} 
## is defined the @var{files} is located relative to @var{rootdir} rather 
## than the current directory
## @seealso{unzip,tar}
## @end deftypefn

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>

function entries = zip (zipfile, files, rootdir)

  if (nargin != 3)
    rootdir = "./";
  endif

  if (nargin == 2 || nargin == 3)
    rootdir = tilde_expand (rootdir);

    if (ischar (files))
      files = cellstr (files);
    endif

    if (ischar (zipfile) && iscellstr (files))

      cmd = sprintf ("cd %s; zip -r %s/%s %s", rootdir, pwd (), zipfile,
                     sprintf (" %s", files{:}));

      [status, output] = system (cmd);

      if (status == 0)
        if (nargout > 0)
          cmd = sprintf ("unzip -Z -1 %s", zipfile);
          [status, entries] = system (cmd);
          if (status == 0)
            if (entries(end) == "\n")
              entries(end) = [];
            endif
            entries = strsplit (entries, "\n");
          else
            error ("zip: zipinfo failed with exit status = %d", status);
          endif
        endif
      else
        error ("zip: zip failed with exit status = %d", status);
      endif
    
    else
      error ("zip: expecting all arguments to be character strings");
    endif

  else
    print_usage ();
  endif

endfunction
