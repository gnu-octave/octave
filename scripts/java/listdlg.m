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
## @deftypefn {Function file} {[@var{sel}, @var{ok}] =} listdlg (@var{key}, @var{value}, @dots{})
## Return user inputs from a list dialog box in a vector of 
## selection indices @var{sel} and a flag @var{ok} indicating how the
## user closed the dialog box.  The value of @var{ok} is 1 if the user
## closed the box with the OK button, otherwise it is 0 and @var{sel} is
## empty.
##
## The indices in @var{sel} are 1 based.
##
## The arguments are specified in form of @var{key}, @var{value} pairs. 
## The @code{"ListString"} argument pair must be specified.
##
## Valid @var{key} and @var{value} pairs are:
##
## @table @code
## @item "ListString"
##    a cell array of strings comprising the content of the list.
## @item "SelectionMode"
##    can be either @code{"Single"} or @code{"Multiple"}.
## @item "ListSize"
##    a vector with two elements @var{width} and @var{height} defining
##    the size of the list field in pixels.
## @item "InitialValue"
##    a vector containing 1-based indices of preselected elements.
## @item "Name"
##    a string to be used as the dialog caption.
## @item "PromptString"
##    a cell array of strings to be displayed above the list field.
## @item "OKString"
##    a string used to label the OK button.
## @item "CancelString"
##    a string used to label the Cancel button.
## @end table
##
## Example:
##
## @example
## [sel, ok] = listdlg ("ListString", @{"An item", "another", "yet another"@}, "SelectionMode", "Multiple" );
## if (ok == 1)
##    imax = length(sel);
##    for i=1:1:imax
##     disp(sel(i));
##    end
## end
## @end example
##
## @seealso{errordlg, helpdlg, inputdlg, questdlg, warndlg}
## @end deftypefn

function varargout = listdlg (varargin)

   if nargin < 2
     print_usage ();
     return;
   end
   
   listcell = {""};
   selmode = "single";
   listsize = [300, 160];
   initialvalue = 1;
   name = "";
   prompt = {""};
   okstring = "OK";
   cancelstring = "Cancel";
   
   ## handle key, value pairs
   for i = 1:2:nargin-1
     if strcmp (varargin{i}, "ListString")
       listcell = varargin{i+1};
     elseif strcmp (varargin{i}, "SelectionMode")
       selmode = varargin{i+1};
     elseif strcmp (varargin{i}, "ListSize")
       listsize = varargin{i+1};
     elseif strcmp (varargin{i}, "InitialValue")
       initialvalue = varargin{i+1};
     elseif strcmp (varargin{i}, "Name")
       name = varargin{i+1};
     elseif strcmp (varargin{i}, "PromptString")
       prompt = varargin{i+1};
     elseif strcmp (varargin{i}, "OKString")
       okstring = varargin{i+1};
     elseif strcmp (varargin{i}, "CancelString")
       cancelstring = varargin{i+1};
     endif
   endfor

   ## make sure prompt strings are a cell array
   if (! iscell (prompt))
     prompt = {prompt};
   end

   ## make sure listcell strings are a cell array
   if (! iscell (listcell))
     listcell = {listcell};
   endif
   
   ## transform matrices to cell arrays of strings
   listsize = arrayfun (@num2str, listsize, "UniformOutput", false);
   initialvalue = arrayfun (@num2str, initialvalue, "UniformOutput", false);
   
   ret = java_invoke ("org.octave.JDialogBox", "listdlg", listcell,
                      selmode, listsize, initialvalue, name, prompt,
                      okstring, cancelstring);

   if (length(ret) > 0)
     varargout = {ret, 1};
   else
     varargout = {{}, 0};
   endif

endfunction
