## Copyright (C) 2010 Martin Hepperle
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
## @deftypefn {Function file} {@var{p} =} msgbox (@var{msg}, @var{title}, @var{ICON})
## Display @var{msg} using a message dialog. 
##
## The message may have multiple lines separated by newline characters
## (@code{"\n"}), or it may be a cellstr array with one element for each
## line.  The optional @var{title} (character string) can be used to
## decorate the dialog caption.
##
## The optional argument @var{icon} selects a dialog icon. 
## It can be one of @code{"error"}, @code{"help"} or @code{"warn"}.
##
## The return value is always 1.
## @seealso{helpdlg, questdlg, warndlg}
## @end deftypefn

function retval = msgbox (message, varargin)

  if (! ischar (message))
    if (iscell (message))
      message = cell2mlstr (message);
    else
      error ("msgbox: character string or cellstr array expected for message");
    endif
  endif
  
  switch length (varargin)
    case 0
      title = "";
      dlg = "emptydlg";

    case 1
      title = varargin{1};
      dlg = "emptydlg";

    otherwise
      ## two or more arguments
      title = varargin{1};
      icon =  varargin{2};
      if (strcmp (icon, "error"))
        dlg = "errordlg";
      elseif (strcmp (icon, "help"))
        dlg = "helpdlg";
      elseif (strcmp (icon, "warn"))
        dlg = "warndlg";
      else
        dlg = "emptydlg";
      end
  endswitch

  if (! ischar (title))
    error ("msgbox: character string expected for title");
  endif

  retval = java_invoke ("org.octave.JDialogBox", dlg, message, title );

endfunction
