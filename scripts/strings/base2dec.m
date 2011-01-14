## Copyright (C) 2000-2011 Daniel Calvelo
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
## @deftypefn {Function File} {} base2dec (@var{s}, @var{base})
## Convert @var{s} from a string of digits in base @var{base} to a decimal
## integer (base 10).
##
## @example
## @group
## base2dec ("11120", 3)
##      @result{} 123
## @end group
## @end example
##
## If @var{s} is a matrix, returns a column vector with one value per
## row of @var{s}.  If a row contains invalid symbols then the
## corresponding value will be NaN@.  Rows are right-justified before
## converting so that trailing spaces are ignored.
##
## If @var{base} is a string, the characters of @var{base} are used as the
## symbols for the digits of @var{s}.  Space (' ') may not be used as a
## symbol.
##
## @example
## @group
## base2dec ("yyyzx", "xyz")
##      @result{} 123
## @end group
## @end example
## @seealso{dec2base, bin2dec, hex2dec}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function out = base2dec (s, base)

  if (nargin != 2)
    print_usage ();
  endif

  symbols = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  if (ischar (base))
    symbols = base;
    base = length (symbols);
    if (length (unique (symbols)) != base)
      error ("base2dec: symbols representing digits must be unique");
    endif
    if (any (isspace (symbols)))
      error ("base2dec: whitespace characters are not valid symbols");
    endif
  elseif (! isscalar (base))
    error ("base2dec: cannot convert from several bases at once");
  elseif (base < 2 || base > length (symbols))
    error ("base2dec: BASE must be between 2 and 36, or a string of symbols");
  else
    s = toupper (s);
  endif

  ## Right justify the values before anything else.
  s = strjust (s, "right");

  ## Lookup value of symbols in symbol table, with invalid symbols
  ## evaluating to NaN and space evaluating to 0.
  table = NaN (1, 256);
  table(toascii (symbols(1:base))) = 0 : base-1;
  table(toascii (" ")) = 0;
  s = table(toascii (s));

  ## Multiply the resulting digits by the appropriate power
  ## and sum the rows.
  out = s * (base .^ (columns(s)-1 : -1 : 0)');

endfunction

%!assert(base2dec ("11120", 3), 123);
%!assert(base2dec ("yyyzx", "xyz"), 123);
%!assert(base2dec ("-1", 2), NaN);

%%Test input validation
%!error base2dec ();
%!error base2dec ("11120");
%!error base2dec ("11120", 3, 4);
%!error base2dec ("11120", "1231");
%!error base2dec ("11120", "12 3");
%!error base2dec ("11120", ones(2));
%!error base2dec ("11120", 37);

