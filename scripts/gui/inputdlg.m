########################################################################
##
## Copyright (C) 2010-2025 The Octave Project Developers
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
## Return user input from a multi-textfield dialog box.
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
## Specifies the size of the text fields and can take four forms:
##
## @enumerate
## @item a scalar value which defines the number of rows used for each text
## field.
##
## @item a row or column vector with the number of elements matching the number
## of prompts.  Each element defines the number of rows used for the
## corresponding text field.
##
## @item a 1x2 row vector which defines the number of rows and the number of
## columns used for all text fields.
##
## @item an Mx2 matrix which defines the individual number of rows and columns
## used for each text field.  In the matrix, each row describes a single text
## field.  The first column specifies the number of input rows to use and the
## second column specifies the text field width.
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
## Output:
## The output is a cell array of strings with each element being the text that
## was entered by the user.  An empty cell array is returned if the dialog was
## closed by the Cancel button.
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

  if (nargin < 1 || nargin > 5)
    print_usage ();
  endif

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
    if (! isnumeric (linespec))
      error ("inputdlg: ROWSCOLS must be numeric");
    endif
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

  if (isscalar (linespec))
    ## only scalar value setting Height for all prompts
    rowscols = zeros (numel (prompt), 2);
    rowscols(:,1) = linespec;
    rowscols(:,2) = 25;
  elseif (isrow (linespec) && columns (linespec) == 2)
    ## One HxW specification for all prompts
    rowscols = repmat (linespec, numel (prompt), 1);
  elseif (ismatrix (linespec))
    if (rows (linespec) == columns (prompt) && columns (linespec) == 2)
      ## (rows x columns) match, copy array linespec
      rowscols = linespec;
    elseif (isvector (linespec))
      if (numel (linespec) != numel (prompt))
        error ("inputdlg: ROWSCOLS vector does not match size of PROMPT");
      endif
      ## rows from column vector linespec, columns are set to default
      rowscols = zeros (numel (prompt), 2);
      rowscols(:,1) = linespec(:);
      rowscols(:,2) = 25;
    else
      error ("inputdlg: ROWSCOLS matrix does not match size of PROMPT");
    endif
  else
    error ("inputdlg: invalid form of ROWSCOLS argument");
  endif
  rowscols = ceil (rowscols);

  ## convert numeric values in defaults cell array to strings
  defs = cellfun ('num2str', defaults, "UniformOutput", false);

  if (! __event_manager_have_dialogs__ ())
    error ("inputdlg is not available in this version of Octave");
  endif

  cstr = __event_manager_input_dialog__ (prompt, title, rowscols, defs);

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

%!error <Invalid call> inputdlg ()
%!error <Invalid call> inputdlg (1, 2, 3, 4, 5, 6)
%!error <PROMPT must be a character string> inputdlg (1)
%!error <TITLE must be a character string> inputdlg ("msg", 1)
%!error <ROWSCOLS must be numeric> inputdlg ("msg", "title", "1")
%!error <number of DEFAULT items must match number of PROMPT items>
%!  inputdlg ("prompt", "title", 1, {'1', '2'})
%!error <ROWSCOLS vector does not match size of PROMPT>
%! inputdlg ({"a1", "a2"}, "title", [1, 2, 3]);
%!error <ROWSCOLS matrix does not match size of PROMPT>
%! inputdlg ({"a1", "a2"}, "title", [1, 2, 3; 4, 5, 6]);
%!error <invalid form of ROWSCOLS argument>
%! inputdlg ({"a1", "a2"}, "title", ones (2,2,2));
