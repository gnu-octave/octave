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
## @deftypefn  {} {@var{str} =} dec2base (@var{d}, @var{base})
## @deftypefnx {} {@var{str} =} dec2base (@var{d}, @var{base}, @var{len})
## @deftypefnx {} {@var{str} =} dec2base (@var{d}, @var{base}, @var{len}, @var{decimals})
## Return a string of symbols in base @var{base} corresponding to the
## value @var{d}.
##
## @example
## @group
## dec2base (123, 3)
##    @result{} "11120"
## @end group
## @end example
##
## If @var{d} is negative, then the result will represent @var{d} in complement
## notation.  For example, negative binary numbers are in twos-complement, and
## analogously for other bases.
##
## If @var{d} is a matrix or cell array, return a string matrix with one row
## per element in @var{d}, padded with leading zeros to the width of the
## largest value.
##
## If @var{base} is a string then the characters of @var{base} are used as
## the symbols for the digits of @var{d}.  Whitespace (spaces, tabs, newlines,
##, etc.@:) may not be used as a symbol.
##
## @example
## @group
## dec2base (123, "aei")
##    @result{} "eeeia"
## @end group
## @end example
##
## The optional third argument, @var{len}, specifies the minimum number of
## digits in the integer part of the result.  If this is omitted, then
## @code{dec2base} uses enough digits to accommodate the input.
##
## The optional fourth argument, @var{decimals}, specifies the number of
## digits to represent the fractional part of the input.  If this is omitted,
## then it is set to zero, and @code{dec2base} returns an integer output for
## backward compatibility.
##
## @example
## @group
## dec2base (100*pi, 16)
## @result{} "13A"
## dec2base (100*pi, 16, 4)
## @result{} "013A"
## dec2base (100*pi, 16, 4, 6)
## @result{} "013A.28C59D"
## dec2base (-100*pi, 16)
## @result{} "EC6"
## dec2base (-100*pi, 16, 4)
## @result{} "FEC6"
## dec2base (-100*pi, 16, 4, 6)
## @result{} "FEC5.D73A63"
## @end group
## @end example
##
## Programming tip: When passing negative inputs to @code{dec2base}, it is
## best to explicitly specify the length of the output required.
##
## @seealso{base2dec, dec2bin, dec2hex}
## @end deftypefn

function str = dec2base (d, base, len, decimals = 0)

  if (nargin < 2)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif

  ## Create column vector for algorithm
  d = d(:);

  ## Treat logical as numeric for compatibility with ML
  if (islogical (d))
    d = double (d);
  elseif (! isnumeric (d) || iscomplex (d))
    error ("dec2base: input must be real numbers");
  endif

  ## Note which elements are negative for processing later.
  ## This also needs special processing for the corresponding intmax.
  belowlim = false(size(d));
  if (isinteger (d))
    belowlim = (d <= intmin(class(d)));
  endif
  neg = (d < 0);
  d(neg) = -d(neg);

  ## Pull out the fractional part for processing later
  fracpart = d - floor (d);
  d = floor (d);

  symbols = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  if (ischar (base))
    symbols = base(:).';  # force a row vector
    base = numel (symbols);
    if (numel (unique (symbols)) != base)
      error ("dec2base: symbols representing digits must be unique");
    elseif (any (isspace (symbols)))
      error ("dec2base: whitespace characters are not valid symbols");
    endif
  elseif (! isscalar (base) || ! isreal (base) || fix (base) != base
          || base < 2 || base > 36)
    error ("dec2base: BASE must be an integer between 2 and 36, or a string of symbols");
  endif

  ## Determine number of digits required to handle all numbers.
  max_len = round (log (max (max (d), 1)) / log (base)) + 1;

  if (nargin >= 3)
    if (! (isscalar (len) && isreal (len) && len >= 0 && len == fix (len)))
      error ("dec2base: LEN must be a non-negative integer");
    endif
    max_len = max (max_len, len);
  endif

  ## determine digits for each number
  digits = zeros (numel (d), max_len);
  for k = max_len:-1:1
    digits(:,k) = mod (d, base);
    d = round ((d - digits(:,k)) / base);
  endfor

  ## Compute any fractional part and append
  if (nargin == 4 && decimals > 0)
    digits2 = zeros (numel (d), decimals);
    for k = 1:decimals
      fracpart *= base;
      digits2(:,k) = floor (fracpart);
      fracpart -= floor (fracpart);
    endfor
  else
    digits2 = zeros (rows (digits), 0);
  endif

  ## Handle negative inputs now
  for k = find (neg)(:)'
    digits(k, :) = (base-1) - digits(k, :);
    if (! isempty (digits2))
      digits2 (k, :) = (base-1) - digits2 (k, :);
    endif

    if (! isempty (digits2))
      j = columns (digits2);
      digits2 (k, j) += 1;  # this is a generalization of two's complement
      while (digits2(j) >= base && j > 1)
        digits2(k, j) -= base;
        digits2(k, j-1) += 1;
        j -= 1;
      endwhile
      if (digits2(k, 1) >= base)  # carry over to integer part
        digits2(k, 1) -= base;
        digits(k, end) += 1;
      endif
    else  # no fractional part ==> increment integer part
      digits(k, end) += 1;
    endif

    if (belowlim (k))  # we need to handle an extra +1
      digits(k, end) -= 1;
      ## Reason: consider the input intmin("int64"),
      ## which is -(2)^64 of type int64.
      ## The code above takes its negation but that exceeds intmax("int64"),
      ## so it's pegged back to 1 lower than what it needs to be, due to
      ## the inherent limitation of the representation.
      ## We add that 1 back here, but because the original sign was negative,
      ## and we are dealing with complement notation, we subtract it instead.
    endif

    j = columns (digits);
    while (digits(k, j) >= base && j > 1)
      digits(k, j) -= base;
      digits(k, j-1) += 1;
      j -= 1;
    endwhile

    if (digits(k, 1) >= base)  # augment by one place if really needed
      digits(k, 1) -= base;
      digits = [zeros(rows(digits), 1), digits];
      digits(k, 1) += 1;
      ## FIXME Should we left-pad with zeros or with (base-1) in this context?
    endif
  endfor

  ## Convert digits to symbols: integer part
  str = reshape (symbols(digits+1), size (digits));

  ## Convert digits to symbols: fractional part
  ## Append fractional part to str if needed.
  if (! isempty (digits2))
    str2 = reshape (symbols(digits2+1), size (digits2));
    str = [str, repmat('.', rows(str), 1), str2];
  endif

  ## Check if the first element is the zero symbol.  It seems possible
  ## that LEN is provided, and is less than the computed MAX_LEN and
  ## MAX_LEN is computed to be one larger than necessary, so we would
  ## have a leading zero to remove.  But if LEN >= MAX_LEN, we should
  ## not remove any leading zeros.
  if ((nargin == 2 || (nargin >= 3 && max_len > len))
      && columns (str) != 1 && ! any (str(:,1) != symbols(1))
      && (~any(neg)))
    str = str(:,2:end);
  endif

endfunction


%!test
%! s0 = "";
%! for n = 1:13
%!   for b = 2:16
%!     pp = dec2base (b^n+1, b);
%!     assert (dec2base (b^n, b), ['1',s0,'0']);
%!     assert (dec2base (b^n+1, b), ['1',s0,'1']);
%!   endfor
%!   s0 = [s0,'0'];
%! endfor

## Test positive fractional inputs
%!assert (dec2base (pi,  2, 0, 16), "11.0010010000111111")
%!assert (dec2base ( e,  2, 2, 16), "10.1011011111100001")
%!assert (dec2base (pi,  3, 0, 16), "10.0102110122220102")
%!assert (dec2base ( e,  3, 0, 16), "2.2011011212211020")
%!assert (dec2base (pi, 16, 0, 10), "3.243F6A8885")
%!assert (dec2base ( e, 16, 0, 10), "2.B7E151628A")

## Test negative inputs: all correct in complement notation
%!assert (dec2base (-1,   10),        "9")
%!assert (dec2base (-1,   10, 3),     "999")
%!assert (dec2base (-1,   10, 3,  2), "999.00")
%!assert (dec2base (-1.1, 10, 3,  2), "998.90")
%!assert (dec2base (-pi,  2,  8, 16), "11111100.1101101111000001")
%!assert (dec2base (-pi,  3,  8, 16), "22222212.2120112100002121")
%!assert (dec2base (-pi, 16,  8, 10), "FFFFFFFC.DBC095777B")
%!assert (dec2base ( -e,  2,  8, 16), "11111101.0100100000011111")
%!assert (dec2base ( -e,  3,  8, 16), "22222220.0211211010011210")
%!assert (dec2base ( -e, 16,  8, 10), "FFFFFFFD.481EAE9D76")

## Test negative inputs close to powers of bases
%!assert (dec2base (-128, 2), "10000000")
%!assert (dec2base (-129, 2, 9), "101111111")
%!assert (dec2base (-129, 2), "01111111")
## FIXME: should dec2base (-129, 2) return "01111111" or ""101111111"?
## The second is an explicit 9-bit universe. The first is an implied 9-bit
## universe but the user needs to be careful not to mistake it for +127, which
## is true in modular arithmetic anyway (i.e., +127 == -129 in 8-bits).
## Currently we work around this by telling the user in `help dec2base` to
## explicitly set the lengths when working with negative numbers.

## Test intmin values
%!assert (dec2base (intmin ("int8"), 2), "10000000")
%!assert (dec2base (intmin ("int16"), 2), "1000000000000000")
%!assert (dec2base (intmin ("int32"), 2), "10000000000000000000000000000000")
%!assert (dec2base (intmin ("int64"), 2), "1000000000000000000000000000000000000000000000000000000000000000")

%!test
%! digits = "0123456789ABCDEF";
%! for n = 1:13
%!   for b = 2:16
%!     pm = dec2base (b^n-1, b);
%!     assert (numel (pm), n);
%!     assert (all (pm == digits(b)));
%!   endfor
%! endfor

%!test
%! for b = 2:16
%!   assert (dec2base (0, b), '0');
%! endfor

%!assert (dec2base (0, 2, 4), "0000")
%!assert (dec2base (2^51-1, 2), ...
%!        "111111111111111111111111111111111111111111111111111")
%!assert (dec2base (uint64 (2)^63-1, 16), "7FFFFFFFFFFFFFFF")
%!assert (dec2base ([1, 2; 3, 4], 2, 3), ["001"; "011"; "010"; "100"])
%!assert (dec2base ({1, 2; 3, 4}, 2, 3), ["001"; "011"; "010"; "100"])

%!test
%! a = 0:3;
%! assert (dec2base (! a, 2, 1), ["1"; "0"; "0"; "0"]);

%!assert <*56005> (dec2base ([0, 0], 16), ["0"; "0"])

## Test input validation
%!error <Invalid call> dec2base ()
%!error <Invalid call> dec2base (1)
%!error <dec2base: input must be real numbers> dec2base ("A", 10)
%!error <dec2base: input must be real numbers> dec2base (2i, 10)
%!error <symbols representing digits must be unique> dec2base (1, "ABA")
%!error <whitespace characters are not valid symbols> dec2base (1, "A B")
%!error <BASE must be an integer> dec2base (1, ones (2))
%!error <BASE must be an integer> dec2base (1, 2i)
%!error <BASE must be an integer> dec2base (1, 2.5)
%!error <BASE must be .* between 2 and 36> dec2base (1, 1)
%!error <BASE must be .* between 2 and 36> dec2base (1, 37)
%!error <LEN must be a non-negative integer> dec2base (1, 2, ones (2))
%!error <LEN must be a non-negative integer> dec2base (1, 2, 2i)
%!error <LEN must be a non-negative integer> dec2base (1, 2, -1)
%!error <LEN must be a non-negative integer> dec2base (1, 2, 2.5)
