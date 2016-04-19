## Copyright (C) 2010, 2013 Martin Hepperle
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
## @deftypefn  {} {@var{h} =} errordlg ()
## @deftypefnx {} {@var{h} =} errordlg (@var{msg})
## @deftypefnx {} {@var{h} =} errordlg (@var{msg}, @var{title})
## @deftypefnx {} {@var{h} =} errordlg (@var{msg}, @var{title}, @var{createmode})
## Display an error message @var{msg} using an error dialog box with caption
## @var{title} (character string).  The default error message is
## @qcode{"This is the default error string."} and the default caption is
## @qcode{"Error Dialog"}.
##
## The error message may have multiple lines separated by newline characters
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
## errordlg ("Some fancy error occured.");
## errordlg ("Some fancy error\nwith two lines.");
## errordlg (@{"Some fancy error", "with two lines."@});
## errordlg ("Some fancy error occured.", "Fancy caption");
## @end group
## @end example
##
## @seealso{helpdlg, inputdlg, listdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function retval = errordlg (varargin)

  narginchk (0, 3);

  box_msg = "This is the default error string.";
  box_title = "Error Dialog";

  if (nargin > 0)
    box_msg = varargin{1};
  endif
  if (nargin > 1)
    box_title = varargin{2};
  endif

  if (nargin < 3)
    retval = msgbox (box_msg, box_title, "error");
  else
    retval = msgbox (box_msg, box_title, "error", varargin{3});
  endif

endfunction

%!error<narginchk> errordlg (1, 2, 3, 4)
%!error<MSG must be a character string> errordlg (1)
%!error<TITLE must be a character string> errordlg ("msg", 1)
