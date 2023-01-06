########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefnx {} {@var{retval} =} endsWith (@var{str}, @var{pattern}, "IgnoreCase", @var{ignore_case})
## Check whether string(s) end with pattern(s).
##
## Return an array of logical values that indicates which string(s) in the
## input @var{str} (a single string or cell array of strings) end with
## the input @var{pattern} (a single string or cell array of strings).
##
## If the value of the parameter @qcode{"IgnoreCase"} is true, then the
## function will ignore the letter case of @var{str} and @var{pattern}.  By
## default, the comparison is case sensitive.
##
## Examples:
##
## @example
## @group
## ## one string and one pattern while considering case
## endsWith ("hello", "lo")
##       @result{}  1
## @end group
##
## @group
## ## one string and one pattern while ignoring case
## endsWith ("hello", "LO", "IgnoreCase", true)
##       @result{}  1
## @end group
##
## @group
## ## multiple strings and multiple patterns while considering case
## endsWith (@{"tests.txt", "mydoc.odt", "myFunc.m", "results.pptx"@},
##           @{".docx", ".odt", ".txt"@})
##       @result{}  1  1  0  0
## @end group
##
## @group
## ## multiple strings and one pattern while considering case
## endsWith (@{"TESTS.TXT", "mydoc.odt", "result.txt", "myFunc.m"@},
##           ".txt", "IgnoreCase", false)
##       @result{}  0  0  1  0
## @end group
##
## @group
## ## multiple strings and one pattern while ignoring case
## endsWith (@{"TESTS.TXT", "mydoc.odt", "result.txt", "myFunc.m"@},
##           ".txt", "IgnoreCase", true)
##       @result{}  1  0  1  0
## @end group
## @end example
##
## @seealso{startsWith, regexp, strncmp, strncmpi}
## @end deftypefn

function retval = endsWith (str, pattern, IgnoreCase, ignore_case)

  if (nargin != 2 && nargin != 4)
    print_usage ();
  endif

  ## Validate input str and pattern
  if (! (iscellstr (str) || ischar (str)))
    error ("endsWith: STR must be a string or cell array of strings");
  endif
  if (! (iscellstr (pattern) || ischar (pattern)))
    error ("endsWith: PATTERN must be a string or cell array of strings");
  endif

  ## reverse str and pattern
  str = cellfun (@flip, cellstr (str), "UniformOutput", false);
  pattern = cellfun (@flip, cellstr (pattern), "UniformOutput", false);

  if (nargin == 2)
    ignore_case = false;
  else
    ## For Matlab compatibility accept any abbreviation of 3rd argument
    if (! ischar (IgnoreCase) || isempty (IgnoreCase)
        || ! strncmpi (IgnoreCase, "IgnoreCase", length (IgnoreCase)))
      error ('endsWith: third input must be "IgnoreCase"');
    endif

    if (! isscalar (ignore_case) || ! isreal (ignore_case))
      error ('endsWith: "IgnoreCase" value must be a logical scalar');
    endif
    ignore_case = logical (ignore_case);
  endif

  retval = false (size (str));
  if (ignore_case)
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
%!assert (endsWith ("hello", "LO", "i", 5))
%!assert (! endsWith ("hello", "no"))

## Test multiple strings with a single pattern
%!test
%! str = {"myFile.odt", "results.ppt", "myCode.m"; ...
%!        "data-analysis.ppt", "foundations.txt", "data.odt"};
%! pattern = ".odt";
%! expected = [true, false, false; false, false, true];
%! assert (endsWith (str, pattern), expected);

## Test multiple strings with multiple patterns
%!test
%! str = {"tests.txt", "mydoc.odt", "myFunc.m", "results.pptx"};
%! pattern = {".docx", ".odt", ".txt"};
%! expected = [true, true, false, false];
%! assert (endsWith (str, pattern), expected);

## Test IgnoreCase
%!test
%! str = {"TESTS.TXT", "mydoc.odt", "result.txt", "myFunc.m"};
%! pattern = ".txt";
%! expected_ignore = [true, false, true, false];
%! expected_wo_ignore = [false, false, true, false];
%! assert (endsWith (str, pattern, "IgnoreCase", true), expected_ignore);
%! assert (endsWith (str, pattern, "IgnoreCase", false), expected_wo_ignore);
%! assert (endsWith (str, pattern, "I", 500), expected_ignore);
%! assert (endsWith (str, pattern, "iG", 0), expected_wo_ignore);

## Test input validation
%!error <Invalid call> endsWith ()
%!error endsWith ("A")
%!error endsWith ("A", "B", "C")
%!error endsWith ("A", "B", "C", "D", "E")
%!error <STR must be a string> endsWith (152, "hi")
%!error <STR must be a .* cell array of strings> endsWith ({152}, "hi")
%!error <PATTERN must be a string> endsWith ("hi", 152)
%!error <PATTERN must be a .* cell array of strings> endsWith ("hi", {152})
%!error <third input must be "IgnoreCase"> endsWith ("hello", "lo", 1, 1)
%!error <third input must be "IgnoreCase"> endsWith ("hello", "lo", "", 1)
%!error <third input must be "IgnoreCase"> endsWith ("hello", "lo", "foo", 1)
%!error <"IgnoreCase" value must be a logical scalar>
%! endsWith ("hello", "hi", "i", "true");
%!error <"IgnoreCase" value must be a logical scalar>
%! endsWith ("hello", "hi", "i", {true});
