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
## @deftypefn  {} {} helpdlg ()
## @deftypefnx {} {} helpdlg (@var{msg})
## @deftypefnx {} {} helpdlg (@var{msg}, @var{title})
## @deftypefnx {} {@var{h} =} helpdlg (@dots{})
## Display a help dialog box with help message @var{msg} and caption
## @var{title}.
##
## The default help message is @qcode{"This is the default help string.@:"}
## and the default caption is @qcode{"Help Dialog"}.
##
## The help message may have multiple lines separated by newline characters
## ("\n"), or it may be a cellstr array with one element for each line.
##
## The return value @var{h} is a handle to the figure object used for
## building the dialog.
##
## Examples:
##
## @example
## @group
## helpdlg ("Some helpful text for the user.");
## helpdlg ("Some helpful text\nwith two lines.");
## helpdlg (@{"Some helpful text", "with two lines."@});
## helpdlg ("Some helpful text for the user.", "Fancy caption");
## @end group
## @end example
##
## @seealso{errordlg, warndlg, msgbox, inputdlg, listdlg, questdlg}
## @end deftypefn

function h = helpdlg (varargin)

  msg = "This is the default help.";
  tit = "Help Dialog";
  opt = "non-modal";

  nargs = numel (varargin);

  if (nargs > 2)
    print_usage ();
  elseif (nargs == 1)
    msg = varargin{1};
  elseif (nargs == 2)
    msg = varargin{1};
    tit = varargin{2};
  endif

  htmp = msgbox (msg, tit, "help", opt);

  if (nargout)
    h = htmp;
  endif

endfunction

## No BIST tests.  This function just dispatches to msgbox().
%!assert (1)
