## Copyright (C) 2000, 2001, 2002, 2003, 2005, 2006, 2007, 2009 Daniel Calvelo
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
## @deftypefn {Function File} {} dec2base (@var{n}, @var{b}, @var{len})
## Return a string of symbols in base @var{b} corresponding to
## the non-negative integer @var{n}.
##
## @example
## @group
## dec2base (123, 3)
##      @result{} "11120"
## @end group
## @end example
##
## If @var{n} is a vector, return a string matrix with one row per value,
## padded with leading zeros to the width of the largest value.
##
## If @var{b} is a string then the characters of @var{b} are used as
## the symbols for the digits of @var{n}.  Space (' ') may not be used
## as a symbol.
##
## @example
## @group
## dec2base (123, "aei")
##      @result{} "eeeia"
## @end group
## @end example
##
## The optional third argument, @var{len}, specifies the minimum
## number of digits in the result.
## @seealso{base2dec, dec2bin, bin2dec, hex2dec, dec2hex}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function retval = dec2base (n, base, len)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (numel (n) != length (n))
    n = n(:);
  elseif (any (n < 0 | n != fix (n)))
    error ("dec2base: can only convert non-negative integers");
  endif

  symbols = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  if (ischar (base))
    symbols = base;
    base = length (symbols);
    if any (diff (sort (toascii (symbols))) == 0)
      error ("dec2base: symbols representing digits must be unique");
    endif
  elseif (! isscalar (base))
    error ("dec2base: cannot convert from several bases at once");
  elseif (base < 2 || base > length (symbols))
    error ("dec2base: base must be between 2 and 36 or a string of symbols");
  endif
  
  ## determine number of digits required to handle all numbers, can overflow
  ## by 1 digit
  max_len = round (log (max (max (n), 1)) ./ log (base)) + 1;

  if (nargin == 3)
    max_len = max (max_len, len);
  endif
  
  ## determine digits for each number
  power = ones (length (n), 1) * (base .^ (max_len-1 : -1 : 0));
  n = n(:) * ones (1, max_len);
  digits = floor (double (rem (n, base*power)) ./ power);

  ## convert digits to symbols
  retval = reshape (symbols (digits+1), size (digits));

  ## Check if the first element is the zero symbol. It seems possible
  ## that LEN is provided, and is less than the computed MAX_LEN and
  ## MAX_LEN is computed to be one larger than necessary, so we would
  ## have a leading zero to remove.  But if LEN >= MAX_LEN, we should
  ## not remove any leading zeros.
  if ((nargin == 2 || (nargin == 3 && max_len > len))
      && all (retval(:,1) == symbols(1)) && length (retval) != 1)
    retval = retval(:,2:end);
  endif

endfunction

%!test
%! s0='';
%! for n=1:13
%!   for b=2:16
%!     pp=dec2base(b^n+1,b);
%!     assert(dec2base(b^n,b),['1',s0,'0']);
%!     assert(dec2base(b^n+1,b),['1',s0,'1']);
%!   end
%!   s0=[s0,'0'];
%! end

%!test
%! digits='0123456789ABCDEF';
%! for n=1:13
%!   for b=2:16
%!     pm=dec2base(b^n-1,b);
%!     assert(length(pm),n);
%!     assert(all(pm==digits(b)));
%!   end
%! end

%!test
%! for b=2:16
%!   assert(dec2base(0,b),'0');
%! end

%!test
%!   assert(dec2base(2^51-1,2),
%!          '111111111111111111111111111111111111111111111111111');
