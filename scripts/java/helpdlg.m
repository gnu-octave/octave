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
## @deftypefn {Function file} {@var{p} =} helpdlg (@var{msg} ,@var{title}])
## Display @var{msg} in a help dialog box.
##
## The message may have multiple lines separated by newline characters
## (@code{"\n"}), or it may be a cellstr array with one element for each
## line.  The optional @var{title} (character string) can be used to
## decorate the dialog caption.
##
## The return value is always 1.
## @seealso{errordlg, inputdlg, listdlg, questdlg, warndlg}
## @end deftypefn

function retval = helpdlg (message, varargin)

  if (! ischar (message))
    if (iscell (message))
      message = cell2mlstr (message);
    else
      error ("helpdlg: character string or cellstr array expected for message");
    endif
  endif
  
  switch length (varargin)
    case 0
      title = "Help Dialog";

    otherwise
      if (ischar (varargin {1}))
        title = varargin{1};
      else
        error ("helpdlg: character string expected for title");
      endif
  endswitch

  if (! ischar (title))
    error ("helpdlg: character string expected for title");
  endif

  retval = java_invoke ("org.octave.JDialogBox", "helpdlg", message, title);

endfunction
