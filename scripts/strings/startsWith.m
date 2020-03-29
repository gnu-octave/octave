########################################################################
##
## Copyright (C) 2020 The Octave Project Developers
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
## @deftypefn  {} {@var{retval} =} startsWith (@var{str}, @var{pattern})
## @deftypefnx {} {@var{retval} =} startsWith (@var{str}, @var{pattern}, "IgnoreCase", @var{confirm_ignore})
## Checks that a cell array of strings starts with a pattern
##
## Return an array of logical values that indicates which strings in the cell
## array of strings --- or the string --- @var{str} starts with a string in the
## cell array of strings --- or the string --- @var{pattern}
##
## If the parameter @qcode{"IgnoreCase"} is set to true, then the function
## will ignore the letter case of @var{str} and @var{pattern}.  By default, the
## comparison is case sensitive.
##
## Examples:
##
## @example
## ## one string and one pattern while considering case
## startsWith ("hello", "he")
##       @result{}  1
##
## ## one string and one pattern while ignoring case
## startsWith ("hello", "HE", "IgnoreCase", true)
##       @result{}  1
##
## ## multiple strings and multiple patterns while considering case
## startsWith (@{"lab work.pptx", "data.txt", "foundations.ppt"@},
##             @{"lab", "data"@})
##       @result{}  1  1  0
##
## ## multiple strings and one pattern while considering case
## startsWith (@{"DATASHEET.ods", "data.txt", "foundations.ppt"@},
##             "data", "IgnoreCase", false)
##       @result{}  0  1  0
##
## ## multiple strings and one pattern while ignoring case
## startsWith (@{"DATASHEET.ods", "data.txt", "foundations.ppt"@},
##             "data", "IgnoreCase", true)
##       @result{}  1  1  0
## @end example
##
## @seealso{endsWith, strcmp, strcmpi}
## @end deftypefn

function retval = startsWith (str, pattern, ignore_case, confirm_ignore)
  if (! (nargin == 2 || nargin == 4))
    print_usage ();
  endif

  ## check input str and pattern
  if (! (iscellstr (str) || ischar (str))
      || ! (iscellstr (pattern) || ischar (pattern)))
    error ("startsWith: arguments must be strings or cell arrays of strings");
  else
    str = cellstr (str);
    pattern = cellstr (pattern);
  endif

  retval = zeros (size (str));

  if (nargin == 2)
    IgnoreFlag = false;
  endif

  if (nargin == 4)
    ## check third input argument
    if (! ischar (ignore_case) || isempty (ignore_case))
      error ('startsWith: third input must be "IgnoreCase"');
    endif

    ## to be compatible with MATLAB
    ## (MATLAB accepts the command with "I", "i", "Ig", "IG" ..etc )
    if (! strncmpi (ignore_case, "IgnoreCase", length (ignore_case)))
      error (['startsWith: invalid argument "%s"; ', ...
              'third input must be "IgnoreCase"'], ignore_case);
    endif

    if (! isscalar (confirm_ignore))
      error ("startsWith: 'IgnoreCase' value must be a logical scalar.");
    endif

    ## to be compatible with MATLAB
    ## (MATLAB accepts the command with true, 5, 1 ..etc  )
    try
      IgnoreFlag = logical (confirm_ignore);
    catch
      error ("startsWith: 'IgnoreCase' value must be a logical scalar");
    end_try_catch
  endif

  if (IgnoreFlag)
    for j = 1:numel (pattern)
      retval |= strncmpi (str, pattern{j}, length (pattern{j}));
    endfor
  else
    for j = 1:numel (pattern)
      retval |= strncmp (str, pattern{j}, length (pattern{j}));
    endfor
  endif

endfunction


## Test simple use with one string and one pattern
%!assert (startsWith ("hello", "he"))
%!assert (! startsWith ("hello", "HE"))
%!assert (startsWith ("hello", "HE", "i", 5)) #  check compatibility with MATLAB
%!assert (! startsWith ("hello", "no"))

## Test multiple strings with a single pattern
%!test
%!  str = {"data science", "dataSheet.ods", "myFunc.m"; "foundations.ppt", ...
%!         "results.txt", "myFile.odt"};
%!  pattern = "data";
%!  correct_ans = [true, true, false; false, false, false];
%!  assert (startsWith (str, pattern), correct_ans)

## Test multiple strings with multiple patterns
%!test
%!  str = {"lab work.pptx", "myFile.odt", "data.txt", "foundations.ppt"};
%!  pattern = {"lab", "data"};
%!  correct_ans = [true, false, true, false];
%!  assert (startsWith (str, pattern), correct_ans)

## Test IgnoreCase
%!test
%!  str = {"DATASHEET.ods", "myFile.odt", "data.txt", "foundations.ppt"};
%!  pattern = "data";
%!  correct_ans_with_ignore = [true, false, true, false];
%!  correct_ans_without_ignore = [false, false, true, false];
%!  assert (startsWith (str, pattern, "IgnoreCase", true), ...
%!          correct_ans_with_ignore)
%!  assert (startsWith (str, pattern, "IgnoreCase", false), ...
%!          correct_ans_without_ignore)
%!  assert (startsWith (str, pattern, "I", 500), ...
%!          correct_ans_with_ignore)  #  check compatibility with MATLAB
%!  assert (startsWith (str, pattern, "iG", 0), ...
%!          correct_ans_without_ignore) #  check compatibility with MATLAB

## Test error detection
%!error <must be strings or cell arrays of strings> startsWith (152, "hi")

%!error <must be strings or cell arrays of strings> startsWith ("hello", 152)

%!error <'IgnoreCase' value must be a logical scalar> ...
%!      startsWith ("hello", "hi", "i", "true")

%!error <'IgnoreCase' value must be a logical scalar> ...
%!      startsWith ("hello", "hi", "i", [1, 2])

%!error <invalid argument "ignire"; third input must be> ...
%!      startsWith ("hello", "he", "ignire", 1)

%!error <invalid argument "IgnoreCasefd"; third input must be> ...
%!      startsWith ("hello", "he", "IgnoreCasefd", 1)

%!error <third input must be "IgnoreCase"> startsWith ("hello", "he", "", 1)

%!error <third input must be "IgnoreCase"> startsWith ("hello", "he", {5}, 1)
