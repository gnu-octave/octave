## Copyright (C) 2004 David Bateman
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {@var{X} =} bitcmp (@var{a},@var{k})
## Return the @var{k}-bit complement of integers in @var{a}.  If
## @var{k} is omitted @code{k = log2(bitmax) + 1} is assumed.
##
## @example
## bitcmp(7,4)
## @result{} 8
## dec2bin(11)
## @result{} 1011
## dec2bin(bitcmp(11))
## @result{} 11111111111111111111111111110100
## @end example
## @end deftypefn
##
## @seealso{bitand,bitor,bitxor,bitset,bitget,bitcmp,bitshift,bitmax}

## Liberally based of the version by Kai Habel from octave-forge

function X = bitcmp (A, n)
  
  if (nargin < 1 || nargin > 2)
    usage ("bitcmp (A, n)");
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
      error ("invalid class %s", class (A));
    endif
    Bmax = intmax (class (A));
  endif

  if (nargin == 2)
    m = double (n(:));
    if (any (m < 1) || any (m > Amax))
      error ("n must be in the range [1,%d]", Amax);
    endif
    X = bitxor (A, bitshift (Bmax, -int8(n)));
  else
    X = bitxor (A, Bmax);
  endif

endfunction
