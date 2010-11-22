## Copyright (C) 2010 Kai Habel
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
## @deftypefn  {Function File} {@var{dirname} =} uigetdir (@var{init_path}, @var{dialog_name})
## @deftypefnx {Function File} {@var{dirname} =} uigetdir (@var{init_path})
## @deftypefnx {Function File} {@var{dirname} =} uigetdir ()
## Open a GUI dialog to select a directory. If @var{init_path} is not given
## the working directory is taken. @var{dialog_name} can be used to
## customize the dialog title.
## @end deftypefn

## Author: Kai Habel

function [retdir] = uigetdir (init_path = pwd, name = "Choose directory?")

  if (!ischar(init_path) || !ischar(name))
    error ("Expecting string arguments.");
  endif
  
  if (nargin > 2)
    print_usage ();
  endif

  if (any (cellfun(@(x)strcmp (x, "fltk"), available_backends)))
      if (!isdir (init_path))
        init_path = fileparts (init_path);
      endif
      retdir = __fltk_uigetfile__ ("", name, init_path, [240, 120], "dir");
  else
    error ("uigetdir requires fltk backend.");
  endif

endfunction

%!demo 
%! uigetdir(pwd, "Select Directory")
