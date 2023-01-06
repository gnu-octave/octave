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
## @deftypefn  {} {} warndlg ()
## @deftypefnx {} {} warndlg (@var{msg})
## @deftypefnx {} {} warndlg (@var{msg}, @var{title})
## @deftypefnx {} {} warndlg (@var{msg}, @var{title}, @var{opt})
## @deftypefnx {} {@var{h} =} warndlg (@dots{})
## Display a warning dialog box with warning message @var{msg} and caption
## @var{title}.
##
## The default warning message is
## @qcode{"This is the default warning string.@:"} and the default caption is
## @qcode{"Warning Dialog"}.
##
## The warning message may have multiple lines separated by newline characters
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
## warndlg ("Some warning text for the user.");
## warndlg ("Some warning text\nwith two lines.");
## warndlg (@{"Some warning text", "with two lines."@});
## warndlg ("Some warning text for the user.", "Fancy caption");
## @end group
## @end example
##
## @seealso{errordlg, helpdlg, msgbox, inputdlg, listdlg, questdlg}
## @end deftypefn

function h = warndlg (varargin)

  msg = "This is the default warning.";
  tit = "Warning Dialog";
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

  htmp = msgbox (msg, tit, "warn", opt);

  if (nargout)
    h = htmp;
  endif

endfunction

## No BIST tests.  This function just dispatches to msgbox().
%!assert (1)
