########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tf} =} isstrprop (@var{str}, @var{prop})
## @deftypefnx {} {@var{tf} =} isstrprop (@var{str}, @var{prop}, 'ForceCellOutput', @var{flag})
## Test character string properties.
##
## For example:
##
## @example
## @group
## isstrprop ("abc123", "alpha")
## @result{} [1, 1, 1, 0, 0, 0]
## @end group
## @end example
##
## If @var{str} is a cell array, @code{isstrpop} is applied recursively to
## each element of the cell array.
##
## Numeric arrays are converted to character strings.
##
## The second argument @var{prop} must be one of
##
## @table @asis
## @item @qcode{"alpha"}
## True for characters that are alphabetic (letters).
##
## @item  @nospell{@qcode{"alnum"}}
## @itemx @nospell{@qcode{"alphanum"}}
## True for characters that are alphabetic or digits.
##
## @item @qcode{"lower"}
## True for lowercase letters.
##
## @item @qcode{"upper"}
## True for uppercase letters.
##
## @item @qcode{"digit"}
## True for decimal digits (0-9).
##
## @item @nospell{@qcode{"xdigit"}}
## True for hexadecimal digits (@nospell{a-fA-F0-9}).
##
## @item  @qcode{"space"}
## @itemx @nospell{@qcode{"wspace"}}
## True for whitespace characters (space, formfeed, newline, carriage return,
## tab, vertical tab).
##
## @item @nospell{@qcode{"punct"}}
## True for punctuation characters (printing characters except space or
## letter or digit).
##
## @item @nospell{@qcode{"cntrl"}}
## True for control characters.
##
## @item  @qcode{"graph"}
## @itemx @qcode{"graphic"}
## True for printing characters except space.
##
## @item @qcode{"print"}
## True for printing characters including space.
##
## @item @qcode{"ascii"}
## True for characters that are in the range of ASCII encoding.
##
## @end table
##
## If the option @nospell{@qcode{'ForceCellOutput'}} is given and @var{flag} is
## true then a cell value is returned rather than a logical array.
##
## @seealso{isalpha, isalnum, islower, isupper, isdigit, isxdigit,
## isspace, ispunct, iscntrl, isgraph, isprint, isascii}
## @end deftypefn

function tf = isstrprop (str, prop, opt, flag)

  if (nargin != 2 && nargin != 4)
    print_usage ();
  endif

  force_cell_output = false;
  if (nargin > 2)
    if (! (isrow (opt) && strcmpi (opt, 'ForceCellOutput')))
      error ("isstrprop: only accepted option is 'ForceCellOutput'");
    elseif (! (isscalar (flag) && isreal (flag)))
      error ("isstrprop: FLAG must be a real scalar");
    endif
    force_cell_output = flag;
  endif

  switch (prop)
    case "alpha"
      tf = isalpha (str);
    case {"alnum", "alphanum"}
      tf = isalnum (str);
    case "ascii"
      tf = isascii (str);
    case "cntrl"
      tf = iscntrl (str);
    case "digit"
      tf = isdigit (str);
    case {"graph", "graphic"}
      tf = isgraph (str);
    case "lower"
      tf = islower (str);
    case "print"
      tf = isprint (str);
    case "punct"
      tf = ispunct (str);
    case {"space", "wspace"}
      tf = isspace (str);
    case "upper"
      tf = isupper (str);
    case "xdigit"
      tf = isxdigit (str);
    otherwise
      error ("isstrprop: invalid string property");
  endswitch

  if (force_cell_output)
    tf = {tf};
  endif

endfunction


%!assert (isstrprop ("abc123", "alpha"), logical ([1, 1, 1, 0, 0, 0]))
%!assert (isstrprop ("abc123", "digit"), logical ([0, 0, 0, 1, 1, 1]))
%!assert (isstrprop ("Hello World", "wspace"), isspace ("Hello World"))
%!assert (isstrprop ("Hello World", "graphic"), isgraph ("Hello World"))
%!assert (isstrprop (char ("AbC", "123"), "upper"), logical ([1 0 1; 0 0 0]))
%!assert (isstrprop (char ("AbC", "123"), "upper", 'ForceCellOutput', true),
%!        {logical([1 0 1; 0 0 0])})
%!assert (isstrprop ({"AbC", "123"}, "lower"),
%!        {logical([0 1 0]), logical([0 0 0])})

## Test input validation
%!error <Invalid call> isstrprop ()
%!error <Invalid call> isstrprop ("abc")
%!error <Invalid call> isstrprop ("abc", 'alpha', 'ForceCellOutput')
%!error <only accepted option is 'ForceCellOutput'>
%! isstrprop ('a', 'alpha', ['ForceCellOutput';'ForceCellOutput'], true)
%!error <only accepted option is 'ForceCellOutput'>
%! isstrprop ('a', 'alpha', 'Foobar', true)
%!error <FLAG must be a real scalar>
%! isstrprop ('a', 'alpha', 'ForceCellOutput', [true, true])
%!error <FLAG must be a real scalar>
%! isstrprop ('a', 'alpha', 'ForceCellOutput', {true})
%!error <invalid string property> isstrprop ("abc123", "foo")
