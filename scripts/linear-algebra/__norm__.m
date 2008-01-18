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
        inf_norm = norm (x, "inf");
        if (inf_norm && finite (inf_norm)) 
          retval = inf_norm .* sqrt (sum (abs (x ./ inf_norm) .^ 2));
        else
          retval = inf_norm;
        endif
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
        inf_norm = norm (x, "inf");
        if (inf_norm && finite (inf_norm))
          retval = inf_norm .* sqrt (sum (sum (abs (x ./ inf_norm) .^ 2)));
        else
          retval = inf_norm;
        endif
      elseif (strcmp (p, "inf"))
        retval = max (sum (abs (x), 2));
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
        retval = max (sum (abs (x), 2));
      else
        error ("norm: unrecognized matrix norm");
      endif
    endif
  endif

endfunction

%!test
%! assert (__norm__ (magic (3)), 15, -2*eps);
%! assert (__norm__ (magic (3) * i), 15, -2*eps);

%!test
%! assert (__norm__ (zeros (5), "fro"), 0);
%! assert (__norm__ (ones (5), "fro"), 5);
%! assert (__norm__ (zeros (5,1), "fro"), 0);
%! assert (__norm__ (2*ones (5,3), "fro"), sqrt (60));

%!test
%! assert (__norm__ (zeros (5), "inf"), 0);
%! assert (__norm__ (ones (5), "inf"), 5);
%! assert (__norm__ (2*ones (5,1), "inf"), 2);
%! assert (__norm__ (2*ones (5,3), "inf"), 6);

%!test
%! assert (__norm__ (zeros (5), 1), 0);
%! assert (__norm__ (ones (5), 1), 5);
%! assert (__norm__ (2*ones (1,5), 1), 10);
%! assert (__norm__ (2*ones (3,5), 1), 6);


%!test
%! assert (__norm__ (1e304 * ones (5, 3), "fro"), 1e304 * sqrt (15));
%! assert (__norm__ (1e-320 * ones (5, 3), "fro"), 1e-320 * sqrt (15));
%! assert (x = __norm__ ([1, 2; 3, Inf], "fro"), Inf);
%! assert (x = __norm__ ([1, 2, 3, Inf], "fro"), Inf);
%! assert (x = __norm__ ([1, -Inf; 3, 4], "fro"), Inf);
%! assert (x = __norm__ ([1, 2; 3, NaN], "fro"), NaN);

%!test
%! assert (__norm__ (1e304 * ones (5, 3), "inf"), 3e304);
%! assert (__norm__ (1e-320 * ones (5, 3), "inf"), 3e-320);
%! assert (x = __norm__ ([1, 2; 3, Inf], "inf"), Inf);
%! assert (x = __norm__ ([1, 2, 3, Inf], "inf"), Inf);
%! assert (x = __norm__ ([1, -Inf; 3, 4], "inf"), Inf);
%! assert (x = __norm__ ([1, 2; 3, NaN], "inf"), 3);


