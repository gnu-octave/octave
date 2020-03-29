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
## @deftypefn  {} {@var{retval} =} endsWith (@var{str}, @var{pattern})
## @deftypefnx {} {@var{retval} =} endsWith (@var{str}, @var{pattern}, "IgnoreCase", @var{confirm_ignore})
## Checks that a cell array of strings ends with a pattern
##
## Return an array of logical values that indicates which strings in the cell
## array of strings --- or the string --- @var{str} ends with a string in the
## cell array of strings --- or the string --- @var{pattern}.
##
## If the parameter @qcode{"IgnoreCase"} is set to true, then the function
## will ignore the letter case of @var{str} and @var{pattern}.  By default, the
## comparison is case sensitive.
##
## Examples:
##
## @example
## ## one string and one pattern while considering case
## endsWith ("hello", "lo")
##       @result{}  1
##
## ## one string and one pattern while ignoring case
## endsWith ("hello", "LO", "IgnoreCase", true)
##       @result{}  1
##
## ## multiple strings and multiple patterns while considering case
## endsWith (@{"tests.txt", "mydoc.odt", "myFunc.m", "results.pptx"@},
##           @{".docx", ".odt", ".txt"@})
##       @result{}  1  1  0  0
##
## ## multiple strings and one pattern while considering case
## endsWith (@{"TESTS.TXT", "mydoc.odt", "result.txt", "myFunc.m"@},
##           ".txt", "IgnoreCase", false)
##       @result{}  0  0  1  0
##
## ## multiple strings and one pattern while ignoring case
## endsWith (@{"TESTS.TXT", "mydoc.odt", "result.txt", "myFunc.m"@},
##           ".txt", "IgnoreCase", true)
##       @result{}  1  0  1  0
## @end example
##
## @seealso{startsWith, strcmp, strcmpi}
## @end deftypefn

function retval = endsWith (str, pattern, ignore_case, confirm_ignore)
  if (! (nargin == 2 || nargin == 4))
    print_usage ();
  endif

  ## check input str and pattern
  if (! (iscellstr (str) || ischar (str))
      || ! (iscellstr (pattern) || ischar (pattern)))
    error ("endsWith: arguments must be strings or cell arrays of strings");
  else
    str = cellstr (str);
    pattern = cellstr (pattern);
  endif

  retval = zeros (size (str));

  ## reverse str and pattern
  for j = 1:numel (str)
    str{j} = flip (str{j});
  endfor
  for j = 1:numel (pattern)
    pattern{j} = flip (pattern{j});
  endfor

  if (nargin == 2)
    IgnoreFlag = false;
  endif

  if (nargin == 4)
    ## checks third input argument
    if (! ischar (ignore_case) || isempty (ignore_case))
      error ('endsWith: third input must be "IgnoreCase"');
    endif

    ## to be compatible with MATLAB
    ## (MATLAB accepts the command with "I", "i", "Ig", "IG" ..etc )
    if (! strncmpi (ignore_case, "IgnoreCase", length (ignore_case)))
      error (['endsWith: invalid argument "%s"; ', ...
              'third input must be "IgnoreCase"'], ignore_case);
    endif

    if (numel (confirm_ignore) != 1)
      error ("endsWith: 'IgnoreCase' value must be a logical scalar");
    endif

    ## to be compatible with MATLAB
    ## (MATLAB accepts the command with true, 5, 1 ..etc  )
    try
      IgnoreFlag = logical (confirm_ignore);
    catch
      error ("endsWith: 'IgnoreCase' value must be a logical scalar");
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
%!assert (endsWith ("hello", "lo"))
%!assert (! endsWith ("hello", "LO"))
%!assert (endsWith ("hello", "LO", "i", 5)) #  check compatibility with MATLAB
%!assert (! endsWith ("hello", "no"))

## Test multiple strings with a single pattern
%!test
%!  str = {"myFile.odt", "results.ppt", "myCode.m"; ...
%!         "data-analysis.ppt", "foundations.txt", "data.odt"};
%!  pattern = ".odt";
%!  correct_ans = [true, false, false; false, false, true];
%!  assert (endsWith (str, pattern), correct_ans)

## Test multiple strings with multiple patterns
%!test
%!  str = {"tests.txt", "mydoc.odt", "myFunc.m", "results.pptx"};
%!  pattern = {".docx", ".odt", ".txt"};
%!  correct_ans = [true, true, false, false];
%!  assert (endsWith (str, pattern), correct_ans)

## Test IgnoreCase
%!test
%!  str = {"TESTS.TXT", "mydoc.odt", "result.txt", "myFunc.m"};
%!  pattern = ".txt";
%!  correct_ans_with_ignore = [true, false, true, false];
%!  correct_ans_without_ignore = [false, false, true, false];
%!  assert (endsWith (str, pattern, "IgnoreCase", true), ...
%!          correct_ans_with_ignore)
%!  assert (endsWith (str, pattern, "IgnoreCase", false), ...
%!          correct_ans_without_ignore)
%!  assert (endsWith (str, pattern, "I", 500), ...
%!          correct_ans_with_ignore) #  check compatibility with MATLAB
%!  assert (endsWith (str, pattern, "iG", 0), ...
%!          correct_ans_without_ignore) #  check compatibility with MATLAB

## Test error detection
%!error <must be strings or cell arrays of strings> endsWith (152, "hi")

%!error <must be strings or cell arrays of strings> endsWith ("hello", 152)

%!error <'IgnoreCase' value must be a logical scalar> ...
%!      endsWith ("hello", "hi", "i", "true")

%!error <'IgnoreCase' value must be a logical scalar> ...
%!      endsWith ("hello", "hi", "i", [1, 2])

%!error <invalid argument "ignire"; third input must be> ...
%!      endsWith ("hello", "he", "ignire", 1)

%!error <invalid argument "IgnoreCasefd"; third input must be> ...
%!      endsWith ("hello", "he", "IgnoreCasefd", 1)

%!error <third input must be "IgnoreCase"> endsWith ("hello", "he", "", 1)

%!error <third input must be "IgnoreCase"> endsWith ("hello", "he", 5, 1)
