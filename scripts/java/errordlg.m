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
## @deftypefn {Function file} {@var{P} =} errordlg (@var{MESSAGE} [,@var{TITLE}])
##
## Displays the @var{MESSAGE} using an error dialog box. 
## The @var{TITLE} can be used optionally to decorate the dialog caption.
## The return value is always 1.
##
## @end deftypefn
## @seealso{helpdlg, inputdlg, listdlg, questdlg, warndlg}

function ret = errordlg(message,varargin)
  
  switch length (varargin)
  case 0
     title = "Error Dialog";
  otherwise
     title = varargin{1};
  endswitch

  ret = java_invoke ("org.octave.JDialogBox", "errordlg", message, title);

endfunction
