## Copyright (C) 2008, 2009 John W. Eaton
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
## @deftypefn {Function File} {} isstrprop (@var{str}, @var{pred})
## Test character string properties.  For example,
##
## @example
## @group
## isstrprop ("abc123", "alpha")
## @result{} [1, 1, 1, 0, 0, 0]
## @end group
## @end example
## 
## If @var{str} is a cell array, @code{isstrpop} is applied recursively
## to each element of the cell array.
##
## Numeric arrays are converted to character strings.
##
## The second argument @var{pred} may be one of
##
## @table @code
## @item "alpha"
## True for characters that are alphabetic
##
## @item "alnum"
## @itemx "alphanum"
## True for characters that are alphabetic or digits.
## 
## @item "ascii"
## True for characters that are in the range of ASCII encoding.
## 
## @item "cntrl"
## True for control characters.
## 
## @item "digit"
## True for decimal digits.
## 
## @item "graph"
## @itemx "graphic"
## True for printing characters except space.
## 
## @item "lower"
## True for lower-case letters.
## 
## @item "print"
## True for printing characters including space.
## 
## @item "punct"
## True for printing characters except space or letter or digit.
## 
## @item "space"
## @itemx "wspace"
## True for whitespace characters (space, formfeed, newline, carriage
## return, tab, vertical tab).
## 
## @item "upper"
## True for upper-case letters.
## 
## @item "xdigit"
## True for hexadecimal digits.
## @end table
##
## @seealso{isalnum, isalpha, isascii, iscntrl, isdigit, isgraph,
## islower, isprint, ispunct, isspace, isupper, isxdigit}
## @end deftypefn

function retval = isstrprop (str, pred)

  if (nargin == 2)
    switch (pred)
      case "alpha"
        retval = isalpha (str);
      case {"alnum", "alphanum"}
        retval = isalnum (str);
      case "ascii"
        retval = isascii (str);
      case "cntrl"
        retval = iscntrl (str);
      case "digit"
        retval = isdigit (str);
      case {"graph", "graphic"}
        retval = isgraph (str);
      case "lower"
        retval = islower (str);
      case "print"
        retval = isprint (str);
      case "punct"
        retval = ispunct (str);
      case {"space", "wspace"}
        retval = isspace (str);
      case "upper"
        retval = isupper (str);
      case "xdigit"
        retval = isxdigit (str);
      otherwise
        error ("isstrprop: invalid predicate");
    endswitch
  else
    print_usage ();
  endif

endfunction

%!error <invalid predicate> isstrprop ("abc123", "foo");
%!assert (isstrprop ("abc123", "alpha"), logical ([1, 1, 1, 0, 0, 0]));