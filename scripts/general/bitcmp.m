## Copyright (C) 2004, 2005, 2006, 2007, 2009 David Bateman
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
## @deftypefn {Function File} {} bitcmp (@var{a}, @var{k})
## Return the @var{k}-bit complement of integers in @var{a}.  If
## @var{k} is omitted @code{k = log2 (bitmax) + 1} is assumed.
##
## @example
## @group
## bitcmp(7,4)
## @result{} 8
## dec2bin(11)
## @result{} 1011
## dec2bin(bitcmp(11, 6))
## @result{} 110100
## @end group
## @end example
## @seealso{bitand, bitor, bitxor, bitset, bitget, bitcmp, bitshift, bitmax}
## @end deftypefn

## Liberally based of the version by Kai Habel from octave-forge

function x = bitcmp (a, n)
  
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 2 && (! isscalar (n) || (floor (n) != n)))
    error ("bitcmp: k must be a scalar integer");
  endif

  if (isa (a, "double"))
    bmax = bitmax;
    amax = ceil (log2 (bmax));
  else
    if (isa (a, "uint8"))
      amax = 8;
    elseif (isa (a, "uint16"))
      amax = 16;
    elseif (isa (a, "uint32"))
      amax = 32;
    elseif (isa (a, "uint64"))
      amax = 64;
    elseif (isa (a, "int8"))
      amax = 8;
    elseif (isa (a, "int16"))
      amax = 16;
    elseif (isa (a, "int32"))
      amax = 32;
    elseif (isa (a, "int64"))
      amax = 64;
    else
      error ("bitcmp: invalid class %s", class (a));
    endif
    bmax = intmax (class (a));
  endif

  if (nargin == 1 || n == amax)
    x = bitxor (a, bmax);
  else
    m = double (n);
    if (any (m < 1) || any (m > amax))
      error ("bitcmp: n must be in the range [1,%d]", amax);
    endif
    mask = bitshift (bmax, n - amax);
    x = bitxor (bitand (a, mask), mask);
  endif
endfunction

%!shared Amax,Bmax,A
%! Amax=53;
%! Bmax = bitmax;
%! A = bitshift(Bmax,-2);
%!assert(bitcmp(A,Amax),bitor(bitshift(1,Amax-1),bitshift(1,Amax-2)));
%!assert(bitcmp(A,Amax-1),bitshift(1,Amax-2));
%!assert(bitcmp(A,Amax-2),0);
%!shared Amax,Bmax,A
%! Amax=8;
%! Bmax = intmax('uint8');
%! A = bitshift(Bmax,-2);
%!assert(bitcmp(A,Amax),bitor(bitshift(uint8(1),Amax-1),bitshift(uint8(1),Amax-2)));
%!assert(bitcmp(A,Amax-1),bitshift(uint8(1),Amax-2));
%!assert(bitcmp(A,Amax-2),uint8(0));
%!shared Amax,Bmax,A
%! Amax=16;
%! Bmax = intmax('uint16');
%! A = bitshift(Bmax,-2);
%!assert(bitcmp(A,Amax),bitor(bitshift(uint16(1),Amax-1),bitshift(uint16(1),Amax-2)));
%!assert(bitcmp(A,Amax-1),bitshift(uint16(1),Amax-2));
%!assert(bitcmp(A,Amax-2),uint16(0));
%!shared Amax,Bmax,A
%! Amax=32;
%! Bmax = intmax('uint32');
%! A = bitshift(Bmax,-2);
%!assert(bitcmp(A,Amax),bitor(bitshift(uint32(1),Amax-1),bitshift(uint32(1),Amax-2)));
%!assert(bitcmp(A,Amax-1),bitshift(uint32(1),Amax-2));
%!assert(bitcmp(A,Amax-2),uint32(0));
%!shared Amax,Bmax,A
%! Amax=64;
%! Bmax = intmax('uint64');
%! A = bitshift(Bmax,-2);
%!assert(bitcmp(A,Amax),bitor(bitshift(uint64(1),Amax-1),bitshift(uint64(1),Amax-2)));
%!assert(bitcmp(A,Amax-1),bitshift(uint64(1),Amax-2));
%!assert(bitcmp(A,Amax-2),uint64(0));
