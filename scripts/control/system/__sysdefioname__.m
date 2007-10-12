## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {} __sysdefioname__ (@var{n}, @var{str}, @var{m})
## return default input or output names given @var{n}, @var{str}, @var{m}.
## @var{n} is the final value, @var{str} is the string prefix, and @var{m}
## is start value
##
## used internally, minimal argument checking
##
## @strong{Example} @code{ioname = __sysdefioname__(5,"u",3)}
## returns the cell array:
## @example
## ioname =
## (
##   [1] = u_3
##   [2] = u_4
##   [3] = u_5
## )
## @end example
## @end deftypefn

function ioname = __sysdefioname__ (n, str, m)

  if (nargin < 2 | nargin > 3)
    print_usage ();
  endif

  if (nargin == 2)           m = min(1,n);            endif

  ioname = {};
  jj = 1;
  if(n > 0 & m > 0 & m <= n)
    for ii = m:n
      ioname{ii+1-m} = sprintf("%s_%d",str,ii);
    endfor
  elseif(m > n)
    error("str=%s; start value m=%d > final value n=%d",str,m,n);
  endif

endfunction
