## Copyright (C) 2010 Martin Hepperle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function file} {@var{P} =} warndlg (@var{MESSAGE} [,@var{TITLE}])
## Displays the @var{MESSAGE} using a warning dialog box. 
##
## @var{message} can have multiple lines separated by newline characters
## ("\n"), or it can be a cellstr array (one element for each line).
## The optional @var{TITLE} (character string) can be used to decorate the
## dialog caption.
##
## @end deftypefn
## @seealso{helpdlg, inputdlg, listdlg, questiondlg}

function ret = warndlg (message, varargin)

  if (! ischar (message))
    if (iscell (message))
      message = cell2mlstr (message);
    else
      error ("warndlg: character string or cellstr array expected for message");
    endif
  endif
  
  switch length (varargin)
  case 0
     title = 'Warning Dialog';
  otherwise
    if (ischar (varargin{1}))
      title = varargin{1};
    else
      error ("warndlg: character string expected for title");
    endif
  endswitch

  ret = java_invoke ('org.octave.JDialogBox', 'warndlg', message, title);

endfunction
