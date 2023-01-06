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
## @deftypefn  {} {} errordlg ()
## @deftypefnx {} {} errordlg (@var{msg})
## @deftypefnx {} {} errordlg (@var{msg}, @var{title})
## @deftypefnx {} {} errordlg (@var{msg}, @var{title}, @var{opt})
## @deftypefnx {} {@var{h} =} errordlg (@dots{})
## Display an error dialog box with error message @var{msg} and caption
## @var{title}.
##
## The default error message is @qcode{"This is the default error string.@:"}
## and the default caption is @qcode{"Error Dialog"}.
##
## The error message may have multiple lines separated by newline characters
## ("\n"), or it may be a cellstr array with one element for each line.
##
## The third optional argument @var{opt} controls the behavior of the dialog.
## For details, @pxref{XREFmsgbox,,@code{msgbox}}.
##
## The return value @var{h} is a handle to the figure object used for
## building the dialog.
##
## Examples:
##
## @example
## @group
## errordlg ("Some fancy error occurred.");
## errordlg ("Some fancy error\nwith two lines.");
## errordlg (@{"Some fancy error", "with two lines."@});
## errordlg ("Some fancy error occurred.", "Fancy caption");
## @end group
## @end example
##
## @seealso{helpdlg, warndlg, msgbox, inputdlg, listdlg, questdlg}
## @end deftypefn

function h = errordlg (varargin)

  msg = "This is the default error.";
  tit = "Error Dialog";
  opt = "non-modal";

  nargs = numel (varargin);

  if (nargs > 3)
    print_usage ();
  elseif (nargs == 1)
    msg = varargin{1};
  elseif (nargs == 2)
    msg = varargin{1};
    tit = varargin{2};
  elseif (nargs == 3)
    msg = varargin{1};
    tit = varargin{2};
    opt = varargin{3};
  endif

  htmp = msgbox (msg, tit, "error", opt);

  if (nargout)
    h = htmp;
  endif

endfunction

## No BIST tests.  This function just dispatches to msgbox().
%!assert (1)
