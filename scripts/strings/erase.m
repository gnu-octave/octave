########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn {} {@var{newstr} =} erase (@var{str}, @var{ptn})
## Delete all occurrences of @var{ptn} within @var{str}.
##
## @var{str} and @var{ptn} can be ordinary strings, cell array of strings, or
## character arrays.
##
## Examples
##
## @example
## @group
## ## string, single pattern
## erase ("Hello World!", " World")
##     @result{} "Hello!"
##
## ## cellstr, single pattern
## erase (@{"Hello", "World!"@}, "World")
##     @result{} @{"Hello", "!"@}
##
## ## string, multiple patterns
## erase ("The Octave interpreter is fabulous", ...
##        @{"interpreter ", "The "@})
##     @result{} "Octave is fabulous"
##
## ## cellstr, multiple patterns
## erase (@{"The ", "Octave interpreter ", "is fabulous"@}, ...
##        @{"interpreter ", "The "@})
##     @result{} @{"", "Octave ", "is fabulous"@}
## @end group
## @end example
##
## Programming Note: @code{erase} deletes the first instance of a pattern in a
## string when there are overlapping occurrences.  For example:
##
## @example
## @group
## erase ("abababa", "aba")
##     @result{} "b"
## @end group
## @end example
##
## For processing overlaps, @pxref{XREFstrrep,,@code{strrep}}.
##
## @seealso{strrep, regexprep}
## @end deftypefn


function newstr = erase (str, ptn)

  if (nargin != 2)
    print_usage ();
  endif

  ischarmatrix = false;
  if (ischar (str))
    if (rows (str) > 1)
      ## Convert to cell.  Can't use cellstr which trims spaces.
      str = mat2cell (str, ones (1, rows (str)));
      ischarmatrix = true;
    endif
  elseif (! iscellstr (str))
    error ("erase: STR must be a string or cell array of strings");
  endif

  if (ischar (ptn))
    if (rows (ptn) > 1)
      warning ("Octave:erase:chararray",
               "erase: using character array for PTN is not recommended, consider cell array of strings instead");
      ## Convert to cell.  Can't use cellstr which trims spaces.
      ptn = mat2cell (ptn, ones (1, rows (ptn)));
    endif
  elseif (! iscellstr (ptn))
    error ("erase: PTN must be a string or cell array of strings");
  endif

  nptn = ifelse (ischar (ptn), 1, numel (ptn));
  if (nptn == 1)
    newstr = strrep (str, ptn, "", "overlaps", false);
  else
    ptn = regexptranslate ("escape", ptn(:).');
    ptn = strjoin (ptn, '|');
    newstr = regexprep (str, ptn, "");
  endif

  if (ischarmatrix)
    newstr = char (newstr);
  endif

endfunction


%!assert (erase ("Hello World!", " World"), "Hello!")
%!assert (erase ({"Hello World!"}, " World"), {"Hello!"})
%!assert (erase (char ("Hello", "World!"), "World"), char ("Hello ", "!"))
%!assert (erase ({"Hello", "World!"}, "World"), {"Hello", "!"})
%!assert (erase ("Hello World!", {"o"; "World"; " "}), "Hell!")

## Test overlaps
## from https://savannah.gnu.org/bugs/?52647#comment5
%!assert (erase ('ababa', 'aba'), 'ba')
%!assert (erase ('abababa', 'aba'), 'b')
%!assert (erase ('ababababa', 'aba'), 'bba')
%!assert (erase ('ababababa', {'aba', 'bba'}), 'bba')
%!assert (erase ('ababababa ', {'aba', 'bba'}), 'bba ')
%!assert (erase ({' ababababa '}, {'aba', 'bba'}), {' bba '})
%!assert (erase (' ababa ', {'aba', 'baba'}), ' ba ')
%!assert (erase (' Hello World t ', {'t';'llo'}), ' He World  ')
%!assert (erase ({' Hello World t '}, [ 'o ']), {' HellWorld t '})
%!assert (erase ( 'Hello World t ', {'ld '; 'o '}), 'HellWort ')
%!assert (erase ('aba', 'aba'), '')
%!assert (erase ({'aba'}, 'aba'), ({""}))
%!assert (erase ('', 'aba'), '')
%!assert (erase ({'abbabbabba'},{'abba'}), {'bb'})
%!assert (erase ({'ababababa';'abbabbabba'}, 'aba'), {'bba';'abbabbabba'})
%!assert (erase ({''}, {''}), {''})
%!assert (erase ({'pppppppp'}, 'p'), {''})
%!assert (erase ('Hello World t ', {'ld '; 'o '}), 'HellWort ')
%!assert (erase ({'Hello World t '}, {'ld '; 'o '}), {'HellWort '})

## Test input validation
%!error <Invalid call> erase ()
%!error erase ("a")
%!error erase ("a", "b", "c")
%!error <STR must be a string> erase ([1], "foo")
%!error <PTN must be a string> erase ("foo", [1])
%!warning <using character array for PTN is not recommended>
%! erase ("a", ["a";"b"]);
