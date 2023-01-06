########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{choice} =} menu (@var{title}, @var{opt1}, @dots{})
## @deftypefnx {} {@var{choice} =} menu (@var{title}, @{@var{opt1}, @dots{}@})
## Display a menu with heading @var{title} and options @var{opt1}, @dots{},
## and wait for user input.
##
## If the GUI is running, the menu is displayed graphically using
## @code{listdlg}.  Otherwise, the title and menu options are printed on the
## console.
##
## @var{title} is a string and the options may be input as individual strings
## or as a cell array of strings.
##
## The return value @var{choice} is the number of the option selected by the
## user counting from 1.  If the user aborts the dialog or makes an invalid
## selection then 0 is returned.
##
## This function is useful for interactive programs.  There is no limit to the
## number of options that may be passed in, but it may be confusing to present
## more than will fit easily on one screen.
## @seealso{input, listdlg}
## @end deftypefn

function choice = menu (title, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! ischar (title))
    error ("menu: TITLE must be a string");
  elseif (nargin > 2 && ! iscellstr (varargin))
    error ("menu: All OPTIONS must be strings");
  elseif (! ischar (varargin{1}) && ! iscellstr (varargin{1}))
    error ("menu: OPTIONS must be string or cell array of strings");
  endif

  if (__event_manager_enabled__ ())  # GUI menu
    [choice, ok] = listdlg ("Name", "menu", "PromptString", title,
                            "ListString", varargin, "SelectionMode", "Single");
    if (! ok)
      choice = 0;
    endif
  else  # console menu
    ## Force pending output to appear before the menu.
    fflush (stdout);

    ## Don't send the menu through the pager as that can cause major confusion.
    page_screen_output (false, "local");

    if (! isempty (title))
      printf ("%s\n", title);
    endif

    ## Handle case where choices are given as a cell array
    if (iscellstr (varargin{1}))
      varargin = varargin{1};
    endif

    nopt = numel (varargin);
    for i = 1:nopt
      printf ("  [%2d] %s\n", i, varargin{i});
    endfor
    printf ("\n");

    s = input ("Select a number: ", "s");
    choice = sscanf (s, "%d");

    if (! isscalar (choice) || choice < 1 || choice > nopt)
      warning ("menu: input invalid or out of range\n");
      choice = 0;
    endif
  endif

endfunction


## Test input validation
%!error <Invalid call> menu ()
%!error <Invalid call> menu ("title")
%!error <TITLE must be a string> menu (1, "opt1")
%!error <All OPTIONS must be strings> menu ("title", "opt1", 1)
%!error <OPTIONS must be string or cell array of strings> menu ("title", 1)
