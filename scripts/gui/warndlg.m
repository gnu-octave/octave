## Copyright (C) 2010-2018 Martin Hepperle
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

## -*- texinfo -*-
## @deftypefn  {} {@var{h} =} warndlg ()
## @deftypefnx {} {@var{h} =} warndlg (@var{msg})
## @deftypefnx {} {@var{h} =} warndlg (@var{msg}, @var{title})
## @deftypefnx {} {@var{h} =} warndlg (@var{msg}, @var{title}, @var{createmode})
## Display a warning dialog box with warning message @var{msg} and caption
## @var{title}.
##
## The default warning message is @qcode{"This is the default warning string."}
## and the default caption is @qcode{"Warning Dialog"}.
##
## The warning message may have multiple lines separated by newline characters
## ("\n"), or it may be a cellstr array with one element for each line.
##
## The return value @var{h} is always 1.
##
## Compatibility Note: The optional argument @var{createmode} is accepted for
## @sc{matlab} compatibility, but is not implemented.  See @code{msgbox} for
## details.
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
## @seealso{errordlg, helpdlg, inputdlg, listdlg, msgbox, questdlg}
## @end deftypefn

function retval = warndlg (varargin)

  narginchk (0, 3);

  msg = "This is the default warning string.";
  title = "Warning Dialog";

  if (nargin > 0)
    msg = varargin{1};
  endif
  if (nargin > 1)
    title = varargin{2};
  endif

  if (nargin < 3)
    retval = msgbox (msg, title, "warn");
  else
    retval = msgbox (msg, title, "warn", varargin{3});
  endif

endfunction


%!error warndlg (1, 2, 3, 4)
%!error <MSG must be a character string> warndlg (1)
%!error <TITLE must be a character string> warndlg ("msg", 1)
