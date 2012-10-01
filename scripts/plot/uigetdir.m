## Copyright (C) 2010-2012 Kai Habel
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
## @deftypefn  {Function File} {@var{dirname} =} uigetdir ()
## @deftypefnx {Function File} {@var{dirname} =} uigetdir (@var{init_path})
## @deftypefnx {Function File} {@var{dirname} =} uigetdir (@var{init_path}, @var{dialog_name})
## Open a GUI dialog for selecting a directory.  If @var{init_path} is not
## given the current working directory is used.  @var{dialog_name} may be
## used to customize the dialog title.
## @seealso{uigetfile}
## @end deftypefn

## Author: Kai Habel

function dirname = uigetdir (init_path = pwd, dialog_name = "Select Directory to Open")

  defaulttoolkit = get (0, "defaultfigure__graphics_toolkit__");
  funcname = ["__uigetdir_", defaulttoolkit, "__"];
  functype = exist (funcname);
  if (! __is_function__ (funcname))
    funcname = "__uigetdir_fltk__";
    if (! __is_function__ (funcname))
      error ("uigetdir: fltk graphics toolkit required");
    elseif (! strcmp (defaulttoolkit, "gnuplot"))
      warning ("uigetdir: no implementation for toolkit '%s', using 'fltk' instead",
               defaulttoolkit);
    endif
  endif

  if (nargin > 2)
    print_usage ();
  endif

  if (!ischar (init_path) || !ischar (dialog_name))
    error ("uigetdir: INIT_PATH and DIALOG_NAME must be string arguments");
  endif

  if (!isdir (init_path))
    init_path = fileparts (init_path);
  endif
  dirname = feval (funcname, init_path, dialog_name);

endfunction

%!demo
%! uigetdir(pwd, "Select Directory")

## Remove from test statistics.  No real tests possible.
%!test
%! assert (1);
