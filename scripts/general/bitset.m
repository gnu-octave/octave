## Copyright (C) 2004 David BAteman
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
## @deftypefn {Function File} {@var{x} =} bitset (@var{a}, @var{n})
## @deftypefnx {Function File} {@var{x} =} bitset (@var{a}, @var{n}, @var{v})
## Set or reset bit(s) @var{n} of unsigned integers in @var{a}.
## @var{v} = 0 resets and @var{v} = 1 sets the bits.
## The lowest significant bit is: @var{n} = 1
##
## @example
## dec2bin (bitset (10, 1))
## @result{} 1011
## @end example
##
## @seealso{bitand, bitor, bitxor, bitget, bitcmp, bitshift, bitmax}
## @end deftypefn

## Liberally based of the version by Kai Habel from octave-forge

function X = bitset (A, n, value)

  if (nargin < 2 || nargin > 3)
    usage ("bitset (A, n, v)");
  endif

  if (nargin == 2)
    value = 1;
  endif
  
  cname = class (A);
  if (strcmp (cname, "double"))
    Bmax = bitmax;
    Amax = log2 (Bmax) + 1;
  elseif strcmp("uint", substr (cname, 1, 4))
    Bmax = intmax (cname);
    Amax = eval ([cname, " (log2 (double (intmax (cname))) + 1);"]);
  else
    Bmax = eval ([cname, " (-1);"]);
    Amax = eval ([cname, " (log2 (double (intmax (cname))) + 2);"]);
  endif

  Aone = eval ([ cname, "(1);"]);
  m = eval ([cname, " (n(:));"]);
  if (any (m < Aone) || any (m > Amax))
    error ("n must be in the range [1,%d]", Amax);
  endif

  ## XXX FIXME XXX Need extra cast to cname due to bad return value of .^
  X = eval (["bitand (A, Bmax - ", cname, " (", cname, " (2) .^ (", cname, " (n) - Aone)));"]);

  if (value)
    X = eval (["bitor (A, ", cname, " (2) .^ (", cname, " (n) - Aone));"]);
  endif

endfunction
