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
## @deftypefn  {} {@var{h} =} helpdlg ()
## @deftypefnx {} {@var{h} =} helpdlg (@var{msg})
## @deftypefnx {} {@var{h} =} helpdlg (@var{msg}, @var{title})
## Display help message @var{msg} using a help dialog box with caption
## @var{title} (character string).  The default help message is
## @qcode{"This is the default help string."} and the default caption is
## @qcode{"Help Dialog"}.
##
## The help message may have multiple lines separated by newline characters
## ("\n"), or it may be a cellstr array with one element for each line.
##
## The return value @var{h} is always 1.
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
## @seealso{errordlg, inputdlg, listdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function retval = helpdlg (varargin)

  narginchk (0, 2);

  box_msg = "This is the default help string.";
  box_title = "Help Dialog";

  if (nargin > 0)
    box_msg = varargin{1};
  endif
  if (nargin > 1)
    box_title = varargin{2};
  endif

  retval = msgbox (box_msg, box_title, "help");

endfunction

%!error<narginchk> helpdlg (1, 2, 3)
%!error<MSG must be a character string> helpdlg (1)
%!error<TITLE must be a character string> helpdlg ("msg", 1)
