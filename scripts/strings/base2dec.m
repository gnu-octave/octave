########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn {} {@var{d} =} base2dec (@var{str}, @var{base})
## Convert @var{str} from a string of digits in base @var{base} to a decimal
## integer (base 10).
##
## @example
## @group
## base2dec ("11120", 3)
##    @result{} 123
## @end group
## @end example
##
## If @var{str} is a string matrix, return a column vector with one value per
## row of @var{str}.  If a row contains invalid symbols then the corresponding
## value will be NaN@.
##
## If @var{str} is a cell array of strings, return a column vector with one
## value per cell element in @var{str}.
##
## If @var{base} is a string, the characters of @var{base} are used as the
## symbols for the digits of @var{str}.  Space (' ') may not be used as a
## symbol.
##
## @example
## @group
## base2dec ("yyyzx", "xyz")
##    @result{} 123
## @end group
## @end example
## @seealso{dec2base, bin2dec, hex2dec}
## @end deftypefn

function d = base2dec (str, base)

  if (nargin != 2)
    print_usage ();
  endif

  if (iscellstr (str))
    str = char (str);
  elseif (! ischar (str))
    error ("base2dec: STR must be a string or cellstring");
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
  elseif (! (base >= 2 && base <= length (symbols)))
    error ("base2dec: BASE must be between 2 and 36, or a string of symbols");
  else
    str = toupper (str);
  endif

  ## Right justify the values and squeeze out any spaces.
  ## This looks complicated, but indexing solution is very fast
  ## compared to alternatives which use cellstr or cellfun or looping.
  [nr, nc] = size (str);
  if (nc > 1)   # Bug #35621
    str = str.';
    nonbl = str != " ";
    num_nonbl = sum (nonbl);
    nc = max (num_nonbl);
    num_blank = nc - num_nonbl;
    R = repmat ([1 2; 0 0], 1, nr);
    R(2, 1:2:2*nr) = num_blank;
    R(2, 2:2:2*nr) = num_nonbl;
    idx = repelems ([false, true], R);
    idx = reshape (idx, nc, nr);

    ## Create a blank matrix and position the nonblank characters.
    s2 = repmat (" ", nc, nr);
    s2(idx) = str(nonbl);
    str = s2.';
  endif

  ## Lookup value of symbols in symbol table, with invalid symbols
  ## evaluating to NaN and space evaluating to 0.
  table = NaN (1, 256);
  table(double (symbols(1:base))) = 0 : base-1;
  table(double (" ")) = 0;
  str = reshape (table(double (str)), size (str));

  ## Multiply the resulting digits by the appropriate power
  ## and sum the rows.
  d = str * (base .^ (columns (str)-1 : -1 : 0)');

endfunction


%!assert (base2dec ("11120", 3), 123)
%!assert (base2dec ("yyyzx", "xyz"), 123)
%!assert (base2dec ("-1", 2), NaN)
%!assert (base2dec ({"A1", "1A"}, 16), [161; 26])

%!assert <*35621> (base2dec (["0"; "1"], 2), [0; 1])

## Test input validation
%!error <Invalid call> base2dec ()
%!error base2dec ("11120")
%!error base2dec ("11120", 3, 4)
%!error <symbols .* must be unique> base2dec ("11120", "1231")
%!error <whitespace characters are not valid> base2dec ("11120", "12 3")
%!error <cannot convert from several bases> base2dec ("11120", ones (2))
%!error <BASE must be between 2 and 36> base2dec ("11120", 1)
%!error <BASE must be between 2 and 36> base2dec ("11120", 37)
%!error <BASE must be between 2 and 36> base2dec ("11120", NaN)
