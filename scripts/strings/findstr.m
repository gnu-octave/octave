## Copyright (C) 1996 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} findstr (@var{s}, @var{t}, @var{overlap})
## Return the vector of all positions in the longer of the two strings
## @var{s} and @var{t} where an occurrence of the shorter of the two starts.
## If the optional argument @var{overlap} is nonzero, the returned vector
## can include overlapping positions (this is the default).  For example,
##
## @example
## findstr ("ababab", "a")
##      @result{} [ 1, 3, 5 ]
## findstr ("abababa", "aba", 0)
##      @result{} [ 1, 5 ]
## @end example
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function v = findstr (s, t, overlap)

  if (nargin < 2 || nargin > 3)
    usage ("findstr (s, t [, overlap])");
  endif

  if (nargin == 2)
    overlap = 1;
  endif

  if (isstr (s) && isstr (t))

    ## Make S be the longer string.

    if (length (s) < length (t))
      tmp = s;
      s = t;
      t = tmp;
    endif

    s = toascii (s);
    t = toascii (t);

    l_t = length (t);

    ind = 1 : l_t;
    limit = length (s) - l_t + 1;
    v  = zeros (1, limit);
    i = 0;

    k = 1;
    while (k <= limit)
      if (s (ind + k - 1) == t)
        v (++i) = k;
        if (! overlap)
          k = k + l_t - 1;
        endif
      endif
      k++;
    endwhile

    if (i > 0)
      v = v (1:i);
    else
      v = [];
    endif

  else
    error ("findstr: expecting first two arguments to be strings");
  endif

endfunction
