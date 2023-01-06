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
## @deftypefn  {} {@var{dirname} =} uigetdir ()
## @deftypefnx {} {@var{dirname} =} uigetdir (@var{init_path})
## @deftypefnx {} {@var{dirname} =} uigetdir (@var{init_path}, @var{dialog_name})
## Open a GUI dialog for selecting a directory.
##
## If @var{init_path} is not given the current working directory is used.
##
## @var{dialog_name} may be used to customize the dialog title.
##
## The output @var{dirname} is a character string with the name of the selected
## directory.  However, if the @samp{Cancel} button is clicked the output is of
## type double with the value @code{0}.
## @seealso{uigetfile, uiputfile}
## @end deftypefn

function dirname = uigetdir (init_path = pwd, dialog_name = "Select Directory to Open")

  if (! ischar (init_path) || ! ischar (dialog_name))
    error ("uigetdir: INIT_PATH and DIALOG_NAME must be string arguments");
  endif

  if (! isfolder (init_path))
    init_path = fileparts (init_path);
  endif

  if (__event_manager_have_dialogs__ ())
    file_filter = cell (0, 2);
    default_file_name = "";
    dialog_mode = "dir";

    [filename, dirname, filterindex] ...
      = __event_manager_file_dialog__ (file_filter, dialog_name,
                                       default_file_name, dialog_mode,
                                       init_path);
  else
    funcname = __get_funcname__ (mfilename ());
    dirname = feval (funcname, init_path, dialog_name);
  endif

endfunction


%!demo
%! uigetdir (pwd, 'Select Directory');

## Remove from test statistics.  No real tests possible.
%!assert (1)
