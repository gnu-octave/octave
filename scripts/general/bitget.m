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
## @deftypefn {Function File} {@var{X} =} bitget (@var{a},@var{n})
## returns the status of bit(s) @var{n} of unsigned integers in @var{a}
## the lowest significant bit is @var{n} = 1.
##
## @example
## bitget(100,8:-1:1)
## @result{} 0  1  1  0  0  1  0  0 
## @end example
## @seealso{bitand,bitor,bitxor,bitset,bitcmp,bitshift,bitmax}
## @end deftypefn

## Liberally based of the version by Kai Habel from octave-forge

function X = bitget (A, n)
  if (nargin != 2)
    usage ("bitget(A,n)");
  endif

  cname = class(A);
  if (strcmp (cname, "double"))
    Amax = log2 (bitmax) + 1;
  elseif strcmp("uint",substr(cname,1,4))
    Amax = eval([cname, " (log2 (double (intmax (cname))) + 1);"]);
  else
    Amax = eval([cname, " (log2 (double (intmax (cname))) + 2);"]);
  endif

  Aone = eval([ cname, "(1);"]);
  m = eval([cname, " (n(:));"]);
  if (any(m < Aone) || any( m > Amax))
    error ("n must be in the range [1,%d]", Amax);
  endif

  X = eval (["bitand (A, ", cname, " (2) .^ (", cname, " (n) - Aone)) != ", cname, "(0);"]);

endfunction
