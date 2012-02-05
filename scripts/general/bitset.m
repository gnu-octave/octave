## Copyright (C) 2004-2012 David Bateman
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
## @deftypefn  {Function File} {@var{C} =} bitset (@var{A}, @var{n})
## @deftypefnx {Function File} {@var{C} =} bitset (@var{A}, @var{n}, @var{val})
## Set or reset bit(s) @var{n} of unsigned integers in @var{A}.
## @var{val} = 0 resets and @var{val} = 1 sets the bits.
## The lowest significant bit is: @var{n} = 1
##
## @example
## @group
## dec2bin (bitset (10, 1))
##   @result{} 1011
## @end group
## @end example
## @seealso{bitand, bitor, bitxor, bitget, bitcmp, bitshift, bitmax}
## @end deftypefn

## Liberally based on the version by Kai Habel from octave-forge

function C = bitset (A, n, val)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 2)
    val = 1;
  endif

  if (isa (A, "double"))
    Bmax = bitmax;
    Amax = log2 (Bmax) + 1;
    _conv = @double;
  else
    if (isa (A, "uint8"))
      Amax = 8;
      _conv = @uint8;
    elseif (isa (A, "uint16"))
      Amax = 16;
      _conv = @uint16;
    elseif (isa (A, "uint32"))
      Amax = 32;
      _conv = @uint32;
    elseif (isa (A, "uint64"))
      Amax = 64;
      _conv = @uint64;
    elseif (isa (A, "int8"))
      Amax = 8;
      _conv = @int8;
    elseif (isa (A, "int16"))
      Amax = 16;
      _conv = @int16;
    elseif (isa (A, "int32"))
      Amax = 32;
      _conv = @int32;
    elseif (isa (A, "int64"))
      Amax = 64;
      _conv = @int64;
    else
      error ("bitset: invalid class %s", class (A));
    endif
    Bmax = intmax (class (A));
  endif

  m = double (n(:));
  if (any (m < 1) || any (m > Amax))
    error ("bitset: N must be in the range [1,%d]", Amax);
  endif

  mask = bitshift (_conv (1), uint8 (n) - uint8 (1));
  C = bitxor (A, bitand (A, mask));

  if (val)
    C = bitor (A, mask);
  endif

endfunction

%!error bitset (1);
%!error bitset (1, 2, 3, 4);

%!test
%! assert (bitset ([0, 10], [3, 3]), [4, 14]);
%! pfx = {"", "u"};
%! for i = 1:2
%!   for prec = [8, 16, 32, 64]
%!     fcn = str2func (sprintf ("%sint%d", pfx{i}, prec));
%!     assert (bitset (fcn ([0, 10]), [3, 3]), fcn ([4, 14]));
%!   endfor
%! endfor

%!error bitset (0, 0);
%!error bitset (0, 55);

%!error bitset (int8 (0), 9);
%!error bitset (uint8 (0), 9);

%!error bitset (int16 (0), 17);
%!error bitset (uint16 (0), 17);

%!error bitset (int32 (0), 33);
%!error bitset (uint32 (0), 33);

%!error bitset (int64 (0), 65);
%!error bitset (uint64 (0), 65);
