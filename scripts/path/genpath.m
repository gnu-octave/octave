## Copyright (C) 2006 John W. Eaton
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
## @deftypefn {Built-in Function} {} genpath (@var{dir})
## Return a path constructed from @var{dir} and all its subdiretories.
## @end deftypefn

function retval = genpath (dirname)

  if (nargin == 1)
    s = stat (dirname);
    if (S_ISDIR (s.mode))
      lst = __genpath__ (dirname);
      lst{2,:} = pathsep ();
      lst{2,end} = "";
      retval = strcat (lst{:});
    else
      retval = "";
    endif
  else
    print_usage ("genpath");
  endif

endfunction

function retval = __genpath__ (dirname)
  
  retval = {dirname};

  s = dir (dirname);
  n = length (s);
  for i = 1:n
    elt = s(i);
    nm = elt.name;
    if (elt.isdir && ! (strcmp (nm, ".") || strcmp (nm, "..")))
      ## FIXME -- Octave bug: recursion fails here if the __genpath__
      ## call is moved inside the [].
      tmp = __genpath__ (fullfile (dirname, nm));
      retval = [retval, tmp];
    endif
  endfor

endfunction
