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
## @deftypefn {Function File} {} bitcmp (@var{A}, @var{k})
## Return the @var{k}-bit complement of integers in @var{A}.  If
## @var{k} is omitted @code{k = log2 (bitmax) + 1} is assumed.
##
## @example
## @group
## bitcmp (7,4)
##   @result{} 8
## dec2bin (11)
##   @result{} 1011
## dec2bin (bitcmp (11, 6))
##   @result{} 110100
## @end group
## @end example
## @seealso{bitand, bitor, bitxor, bitset, bitget, bitcmp, bitshift, bitmax}
## @end deftypefn

## Liberally based on the version by Kai Habel from octave-forge

function C = bitcmp (A, k)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 2 && (! isscalar (k) || (floor (k) != k)))
    error ("bitcmp: K must be a scalar integer");
  endif

  if (isa (A, "double"))
    bmax = bitmax;
    amax = ceil (log2 (bmax));
  else
    if (isa (A, "uint8"))
      amax = 8;
    elseif (isa (A, "uint16"))
      amax = 16;
    elseif (isa (A, "uint32"))
      amax = 32;
    elseif (isa (A, "uint64"))
      amax = 64;
    elseif (isa (A, "int8"))
      amax = 8;
    elseif (isa (A, "int16"))
      amax = 16;
    elseif (isa (A, "int32"))
      amax = 32;
    elseif (isa (A, "int64"))
      amax = 64;
    else
      error ("bitcmp: invalid class %s", class (A));
    endif
    bmax = intmax (class (A));
  endif

  if (nargin == 1 || k == amax)
    C = bitxor (A, bmax);
  else
    m = double (k);
    if (any (m < 1) || any (m > amax))
      error ("bitcmp: K must be in the range [1,%d]", amax);
    endif
    mask = bitshift (bmax, k - amax);
    C = bitxor (bitand (A, mask), mask);
  endif
endfunction


%!test
%! Amax=53;
%! Bmax = bitmax;
%! A = bitshift(Bmax,-2);
%! assert(bitcmp(A,Amax),bitor(bitshift(1,Amax-1),bitshift(1,Amax-2)));
%! assert(bitcmp(A,Amax-1),bitshift(1,Amax-2));
%! assert(bitcmp(A,Amax-2),0);
%!test
%! Amax=8;
%! Bmax = intmax('uint8');
%! A = bitshift(Bmax,-2);
%! assert(bitcmp(A,Amax),bitor(bitshift(uint8(1),Amax-1),bitshift(uint8(1),Amax-2)));
%! assert(bitcmp(A,Amax-1),bitshift(uint8(1),Amax-2));
%! assert(bitcmp(A,Amax-2),uint8(0));
%!test
%! Amax=16;
%! Bmax = intmax('uint16');
%! A = bitshift(Bmax,-2);
%! assert(bitcmp(A,Amax),bitor(bitshift(uint16(1),Amax-1),bitshift(uint16(1),Amax-2)));
%! assert(bitcmp(A,Amax-1),bitshift(uint16(1),Amax-2));
%! assert(bitcmp(A,Amax-2),uint16(0));
%!test
%! Amax=32;
%! Bmax = intmax('uint32');
%! A = bitshift(Bmax,-2);
%! assert(bitcmp(A,Amax),bitor(bitshift(uint32(1),Amax-1),bitshift(uint32(1),Amax-2)));
%! assert(bitcmp(A,Amax-1),bitshift(uint32(1),Amax-2));
%! assert(bitcmp(A,Amax-2),uint32(0));
%!test
%! Amax=64;
%! Bmax = intmax('uint64');
%! A = bitshift(Bmax,-2);
%! assert(bitcmp(A,Amax),bitor(bitshift(uint64(1),Amax-1),bitshift(uint64(1),Amax-2)));
%! assert(bitcmp(A,Amax-1),bitshift(uint64(1),Amax-2));
%! assert(bitcmp(A,Amax-2),uint64(0));

