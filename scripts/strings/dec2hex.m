########################################################################
##
## Copyright (C) 1996-2020 The Octave Project Developers
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
## @deftypefn  {} {} dec2hex (@var{d})
## @deftypefnx {} {} dec2hex (@var{d}, @var{len})
## Return a string representing the conversion of the integer @var{d} to a
## hexadecimal (base16) number.
##
## If @var{d} is negative, return the two's complement binary value of @var{d}.
## If @var{d} is a matrix or cell array, return a string matrix with one row
## for each element in @var{d}, padded with leading zeros to the width of the
## largest value.
##
## The optional second argument, @var{len}, specifies the minimum number of
## digits in the result.
##
## Examples:
##
## @example
## @group
## dec2hex (2748)
##      @result{} "ABC"
##
## dec2hex (-2)
##      @result{} "FE"
## @end group
## @end example
##
## @seealso{hex2dec, dec2base, dec2bin}
## @end deftypefn

function h = dec2hex (d, len)

  if (nargin == 0 || nargin > 2)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif
  ## Create column vector for algorithm (output is always col. vector anyways)
  d = d(:);

  lt_zero_idx = (d < 0);
  if (any (lt_zero_idx))
    ## FIXME: Need an algorithm that works with larger values such as int64.
    if (any (d(lt_zero_idx) < -2^52))
      error ("dec2hex: negative inputs cannot be less than -flintmax () / 2");
    elseif (any (d(lt_zero_idx) < intmin ("int32")))
      d(lt_zero_idx) += flintmax ();
    elseif (any (d < intmin ("int16")))
      d(lt_zero_idx) += double (intmax ("uint32")) + 1;
    elseif (any (d < intmin ("int8")))
      d(lt_zero_idx) += double (intmax ("uint16"))+ 1;
    else
      d(lt_zero_idx) += double (intmax ("uint8")) + 1;
    endif
  endif

  if (nargin == 1)
    h = dec2base (d, 16);
  else
    h = dec2base (d, 16, len);
  endif

endfunction


%!assert (dec2hex (2748), "ABC")
%!assert (dec2hex (2748, 5), "00ABC")
%!assert (dec2hex ([2748, 2746]), ["ABC"; "ABA"])
%!assert (dec2hex ({2748, 2746}), ["ABC"; "ABA"])
%!assert (dec2hex ({2748, 2746}, 4), ["0ABC"; "0ABA"])

## Test negative inputs
%!assert (dec2hex (-3), "FD")
%!assert (dec2hex (-3, 1), "FD")
%!assert (dec2hex (-3, 3), "0FD")
%!assert (dec2hex (-2^7 -1), "FF7F")
%!assert (dec2hex (-2^15 -1), "FFFF7FFF")
## FIXME: Matlab returns longer string that begins with 'F'
%!assert (dec2hex (-2^31 -1), "1FFFFF7FFFFFFF")
## FIXME: Matlab returns longer string that begins with 'FFF'
%!assert (dec2hex (-2^52), "10000000000000")
## FIXME: Uncomment when support for int64 is added
%!#assert (dec2hex (-2^63),
%!        "1000000000000000000000000000000000000000000000000000000000000000")
%!#test
%! assert (dec2hex (int64 (-2^63)),
%!        "1000000000000000000000000000000000000000000000000000000000000000");
%!#test
%! assert (dec2hex (int64 (-2^63) -1),
%!        "1000000000000000000000000000000000000000000000000000000000000000");
%!#test
%! assert (dec2hex (int64 (-2^63) +1),
%!        "1000000000000000000000000000000000000000000000000000000000000001");
%!assert (dec2hex ([-1, -2; -3, -4]), ["FF"; "FD"; "FE"; "FC"])
%!assert (dec2hex ([1, 2; 3, -4]), ["01"; "03"; "02"; "FC"])
%!assert (dec2hex ({1, 2; 3, -4}), ["01"; "03"; "02"; "FC"])

## Test input validation
%!error dec2hex ()
%!error dec2hex (1, 2, 3)
%!error <negative inputs cannot be less than> dec2hex (- flintmax ())
