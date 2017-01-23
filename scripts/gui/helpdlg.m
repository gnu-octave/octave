## Copyright (C) 2010-2016 Martin Hepperle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{h} =} helpdlg ()
## @deftypefnx {} {@var{h} =} helpdlg (@var{msg})
## @deftypefnx {} {@var{h} =} helpdlg (@var{msg}, @var{title})
## Display a help dialog box with help message @var{msg} and caption
## @var{title}.
##
## The default help message is @qcode{"This is the default help string."} and
## the default caption is @qcode{"Help Dialog"}.
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

  msg = "This is the default help string.";
  title = "Help Dialog";

  if (nargin > 0)
    msg = varargin{1};
  endif
  if (nargin > 1)
    title = varargin{2};
  endif

  retval = msgbox (msg, title, "help");

endfunction


%!error helpdlg (1, 2, 3)
%!error <MSG must be a character string> helpdlg (1)
%!error <TITLE must be a character string> helpdlg ("msg", 1)
