########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{cstr} =} inputdlg (@var{prompt})
## @deftypefnx {} {@var{cstr} =} inputdlg (@var{prompt}, @var{title})
## @deftypefnx {} {@var{cstr} =} inputdlg (@var{prompt}, @var{title}, @var{rowscols})
## @deftypefnx {} {@var{cstr} =} inputdlg (@var{prompt}, @var{title}, @var{rowscols}, @var{defaults})
## @deftypefnx {} {@var{cstr} =} inputdlg (@var{prompt}, @var{title}, @var{rowscols}, @var{defaults}, @var{options})
## Return user input from a multi-textfield dialog box in a cell array of
## strings, or an empty cell array if the dialog is closed by the Cancel
## button.
##
## Inputs:
##
## @table @var
## @item prompt
## A cell array with strings labeling each text field.  This input is required.
##
## @item title
## String to use for the caption of the dialog.  The default is
## @qcode{"Input Dialog"}.
##
## @item rowscols
## Specifies the size of the text fields and can take three forms:
##
## @enumerate
## @item a scalar value which defines the number of rows used for each text
## field.
##
## @item a vector which defines the individual number of rows used for each
## text field.
##
## @item a matrix which defines the individual number of rows and columns used
## for each text field.  In the matrix each row describes a single text field.
## The first column specifies the number of input rows to use and the second
## column specifies the text field width.
## @end enumerate
##
## @item defaults
## A list of default values to place in each text field.  It must be a cell
## array of strings with the same size as @var{prompt}.
##
## @item options
## Not supported, only for @sc{matlab} compatibility.
## @end table
##
## Example:
##
## @example
## @group
## prompt = @{"Width", "Height", "Depth"@};
## defaults = @{"1.10", "2.20", "3.30"@};
## rowscols = [1,10; 2,20; 3,30];
## dims = inputdlg (prompt, "Enter Box Dimensions", ...
##                  rowscols, defaults);
## @end group
## @end example
##
## @seealso{errordlg, helpdlg, listdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function cstr = inputdlg (prompt, varargin)

  narginchk (1, 5);

  if (iscell (prompt))
    ## Silently extract only char elements
    prompt = prompt(cellfun ("isclass", prompt, "char"));
  elseif (ischar (prompt))
    prompt = {prompt};
  else
    error ("inputdlg: PROMPT must be a character string or cellstr array");
  endif

  title = "Input Dialog";
  if (nargin > 1)
    if (! ischar (varargin{1}))
      error ("inputdlg: TITLE must be a character string");
    endif
    title = varargin{1};
  endif

  linespec = 1;
  if (nargin > 2)
    linespec = varargin{2};
  endif

  defaults = cellstr (cell (size (prompt)));
  if (nargin > 3)
    if (numel (varargin{3}) != numel (prompt))
      error ("inputdlg: number of DEFAULT items must match number of PROMPT items");
    endif
    defaults = varargin{3};
  endif

  if (nargin > 4)
    warning ("inputdlg: 5th 'options' argument ignored");
  endif

  ## specification of text field sizes as in Matlab
  ## Matlab requires a matrix for linespec, not a cell array...
  ## rc = [1,10; 2,20; 3,30];
  ##     c1  c2
  ## r1  1   10   first  text field is 1x10
  ## r2  2   20   second text field is 2x20
  ## r3  3   30   third  text field is 3x30
  if (! isnumeric (linespec))
    error ("inputdlg: ROWSCOLS must be numeric");
  endif

  if (isscalar (linespec))
    ## only scalar value in lineTo, copy from linespec and add defaults
    rowscols = zeros (numel (prompt), 2);
    ## cols
    rowscols(:,2) = 25;
    rowscols(:,1) = linespec;
  elseif (ismatrix (linespec))
    if (rows (linespec) == columns (prompt) && columns (linespec) == 2)
      ## (rows x columns) match, copy array linespec
      rowscols = linespec;
    elseif (isvector (linespec))
      if (numel (linespec) == numel (prompt))
        ## only one column in lineTo, copy from vector linespec and add defaults
        rowscols = zeros (numel (prompt), 2);
        ## rows from column vector linespec, columns are set to default
        rowscols(:,2) = 25;
        rowscols(:,1) = linespec(:);
      else
        error ("inputdlg: ROWSCOLS vector does not match size of PROMPT");
      endif
    else
      error ("inputdlg: ROWSCOLS matrix does not match size of PROMPT");
    endif
  else
    ## dunno
    error ("inputdlg: unknown form of ROWSCOLS argument");
  endif
  rowscols = ceil (rowscols);

  ## convert numeric values in defaults cell array to strings
  defs = cellfun (@num2str, defaults, "UniformOutput", false);

  if (__event_manager_have_dialogs__ ())
    cstr = __event_manager_input_dialog__ (prompt, title, rowscols, defs);
  else
    error ("inputdlg is not available in this version of Octave");
  endif

endfunction


%!demo
%! disp ('- test inputdlg with prompt and caption only.');
%! prompt = {'Width', 'Height', 'Depth'};
%! dims = inputdlg (prompt, 'Enter Box Dimensions');
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!   helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', ...
%!                     volume, surface), 'Box Dimensions');
%! endif

%!demo
%! disp ('- test inputdlg with prescribed scalar (2 lines per text field) and defaults.');
%! prompt = {'Width', 'Height', 'Depth'};
%! default = {'1.1', '2.2', '3.3'};
%! rc = 2;
%! dims = inputdlg (prompt, 'Enter Box Dimensions', rc, default);
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!    helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', ...
%!                      volume, surface), 'Box Dimensions');
%! endif

%!demo
%! disp ('- test inputdlg with prescribed vector [1,2,3] for # of lines per text field and defaults.');
%! prompt = {'Width', 'Height', 'Depth'};
%! default = {'1.10', '2.10', '3.10'};
%! rc = [1,2,3];  # NOTE: must be an array
%! dims = inputdlg (prompt, 'Enter Box Dimensions', rc, default);
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!   helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', ...
%!                     volume, surface), 'Box Dimensions');
%! endif

%!demo
%! disp ('- test inputdlg with prescribed row by column sizes and defaults.');
%! prompt = {'Width', 'Height', 'Depth'};
%! default = {'1.10', '2.20', '3.30'};
%! rc = [1,10; 2,20; 3,30];  # NOTE: must be an array
%! dims = inputdlg (prompt, 'Enter Box Dimensions', rc, default);
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!   helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', ...
%!                     volume, surface), 'Box Dimensions');
%! endif

%!demo
%! disp ('- test inputdlg with vector for a single item.');
%! prompt = {'enter x value'};
%! default = {1};
%! answer = inputdlg (prompt, 'Enter value', [1 10], default);
%! if (isempty (answer))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   helpdlg (sprintf ('answer = %d', str2num (answer{1})), 'answer');
%! endif

%!error inputdlg (1, 2, 3, 4, 5, 6)
%!error <PROMPT must be a character string> inputdlg (1)
%!error <TITLE must be a character string> inputdlg ("msg", 1)
%!error <ROWSCOLS must be numeric> inputdlg ("msg", "title", "1")
%!error <ROWSCOLS vector does not match size>
%! inputdlg ({"a1", "a2"}, "title", [1, 2, 3]);
