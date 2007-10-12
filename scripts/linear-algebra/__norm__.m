## Copyright (C) 1996, 1997, 2007 John W. Eaton
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

## Author: jwe

function retval = __norm__ (x, p)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (isempty (x))
    retval = [];
    return;
  endif

  if (ndims (x) > 2)
    error ("norm: only valid on 2-D objects")
  endif

  if (nargin == 1)
    p = 2;
  endif

  ## Do we have a vector or matrix as the first argument?

  if (ndims(x) == 2 && (rows (x) == 1 || columns (x) == 1))
    if (ischar (p))
      if (strcmp (p, "fro"))
	retval = sqrt (sum (abs (x) .^ 2));
      elseif (strcmp (p, "inf"))
        retval = max (abs (x));
      else
        error ("norm: unrecognized norm");
      endif
    else
      if (p == Inf)
        retval = max (abs (x));
      elseif (p == -Inf)
        retval = min (abs (x));
      elseif (p == 1)
	retval = sum (abs (x));
      elseif (p == 2)
        if (iscomplex (x))
          x = abs (x);
        endif
        retval = sqrt (sumsq (x));
      else
        retval = sum (abs (x) .^ p) ^ (1/p);
      endif
    endif
  else
    if (ischar (p))
      if (strcmp (p, "fro"))
	retval = sqrt (sum (sum (abs (x) .^ 2)));
      elseif (strcmp (p, "inf"))
        retval = max (sum (abs (x')));
      else
        error ("norm: unrecognized vector norm");
      endif
    else
      if (p == 1)
        retval = max (sum (abs (x)));
      elseif (p == 2)
        s = svd (x);
        retval = s (1);
      elseif (p == Inf)
        retval = max (sum (abs (x')));
      else
	error ("norm: unrecognized matrix norm");
      endif
    endif
  endif

endfunction
