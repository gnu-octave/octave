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
## @deftypefn {Function file} {@var{p} =} inputdlg (@var{prompt}, @var{title}, @var{rowscols}, @var{defaults})
## Return user input from a multi-textfield dialog box in a cell array
## of strings, or an empty cell array if the dialog is closed by the
## Cancel button.
##
## @table @var
## @item prompt
## The first argument @var{prompt} is mandatory.
## It is a cell array with strings labeling each textfield. 
## @item title
## The optional string @var{title} can be used as the caption of the dialog. 
## @item rowscols
## The size of the text fields can be defined by the argument @var{rowscols}, 
## which can have three forms:
## @enumerate
## @item a scalar value which defines the number of rows used for each
## text field.
## @item a vector which defines the individual number of rows
## used for each text field. 
## @item a matrix which defines the individual number of rows and
## columns used for each text field.
## @end enumerate
## @item defaults
## It is possible to place default values into the text fields by supplying
## the a cell array of strings or number for the argument @var{defaults}.
## @end table
## @seealso{errordlg, helpdlg, listdlg, questdlg, warndlg}
## @end deftypefn

function retval = inputdlg (prompt, varargin)

  if (iscell (prompt))
    ## Silently extract only char elements
    prompt = prompt (find (cellfun ("ischar", prompt)));
  elseif (ischar (prompt))
    prompt = {prompt};
  else
    error ("inputdlg: character string or cellstr array expected for prompt");
  endif

  switch length (varargin)
    case 0
      title = "Input Dialog";
      lineNo = 1;
      defaults = cellstr (cell( size (prompt)));

    case 1
      title = varargin{1};
      lineNo = 1;
      defaults = cellstr (cell (size (prompt)));

    case 2
      title = varargin{1};
      lineNo = varargin{2};
      defaults = cellstr (cell (size (prompt)));

    otherwise
      title = varargin{1};
      lineNo = varargin{2};
      defaults = varargin{3};
  endswitch

  if (! ischar (title))
    error ("inputdlg: character string expected for title");
  endif

  ## specification of text field sizes as in Matlab 
  ## Matlab requires a matrix for lineNo, not a cell array...
  ## rc = [1,10; 2,20; 3,30];
  ##     c1  c2
  ## r1  1   10   first  text field is 1x10
  ## r2  2   20   second text field is 2x20
  ## r3  3   30   third  text field is 3x30
  if (isscalar (lineNo))
    ## only scalar value in lineTo, copy from lineNo and add defaults
    rowscols = zeros(size(prompt)(2),2);
    ## cols
    rowscols(:,2) = 25;
    rowscols(:,1) = lineNo;
  elseif (isvector (lineNo))
      ## only one column in lineTo, copy from vector lineNo and add defaults
      rowscols = zeros (columns (prompt), 2);
      ## rows from colum vector lineNo, columns are set to default
      rowscols(:,2) = 25;
      rowscols(:,1) = lineNo(:);         
  elseif (ismatrix (lineNo))
    if (rows (lineNo) == columns (prompt) && columns (lineNo) == 2)
      ## (rows x columns) match, copy array lineNo
      rowscols = lineNo;
    endif
  else
    ## dunno
    error ("inputdlg: unknown form of lineNo argument");
  endif
  
  ## convert numeric values in defaults cell array to strings
  defs = cellfun (@num2str, defaults, "UniformOutput", false);
  rc = arrayfun (@num2str, rowscols, "UniformOutput", false);

  user_inputs = java_invoke ("org.octave.JDialogBox", "inputdlg",
                             prompt, title, rc, defs);
  
   if (isempty (user_inputs))
     retval = {};
   else
     retval = cellstr (user_inputs);
   endif

endfunction
