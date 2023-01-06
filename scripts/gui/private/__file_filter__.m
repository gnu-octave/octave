########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {[retval, defname, defdir] =} __file_filter__ (@var{caller}, @var{file_filter})
## Undocumented internal function.
## @end deftypefn

function [retval, defname, defdir] = __file_filter__ (caller, file_filter)

  retval = {};
  defname = "";
  defdir = "";

  if (nargin == 1 || isempty (file_filter))
    ## Do nothing, and just add default pattern.
  elseif (iscell (file_filter))
    [r, c] = size (file_filter);
    if (c != 1 && c != 2)
      error ("%s: invalid filter specification", caller);
    endif
    if (c == 1)
      retval = cell (r, 2);
      for i = 1:r
        retval{i, 1} = file_filter{i};
        retval{i, 2} = __default_filtername__ (file_filter{i});
      endfor
    else
      retval = file_filter;
      for i = 1:r
        if (isempty (retval{i, 2}))
          retval{i, 2} = __default_filtername__ (retval{i, 1});
        endif
      endfor
    endif
  elseif (ischar (file_filter))
    [defdir, fname, fext] = fileparts (file_filter);
    if (! strcmp (fname, "*"))
      defname = [fname, fext];
    endif
    if (! isempty (fext))
      fext = ["*" fext];
      retval = {fext, __default_filtername__(fext)};
    endif
  endif

  ## Delete any "*.*" pattern, and add "* All Files"
  if (! isempty (retval))
    retval(strcmp (retval(1,:), "*.*"), :) = [];
  endif
  retval(end+1,:) = {"*", __default_filtername__("*")};

endfunction

function name = __default_filtername__ (filterext)

  name = "";

  switch (filterext)
    case "*"
      name = "All Files";
    case "*.m"
      name = "Octave Source Files";
    case "*.c"
      name = "C Source Files";
    case {"*.cc", "*.cp", "*.cpp", "*.CPP", "*.cxx", "*.c++" "*.C"}
      name = "C++ Source Files";
    case "*.oct"
      name = "Octave Compiled Files";
  endswitch

  if (isempty (name))
    extlist = ostrsplit (filterext, ";");
    extlist = strrep (extlist, "*.", "");
    extlist = toupper (extlist);
    extlist(end+1, :) = repmat ({","}, 1, length (extlist));
    extlist = strcat (extlist{:});
    extlist = extlist(1:end-1);
    name = strcat (extlist, "-Files");
  endif

endfunction
