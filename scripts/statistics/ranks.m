########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{y} =} ranks (@var{x})
## @deftypefnx {} {@var{y} =} ranks (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} ranks (@var{x}, @var{dim}, @var{rtype})
## Return the ranks (in the sense of order statistics) of @var{x} along the
## first non-singleton dimension adjusted for ties.
##
## If the optional @var{dim} argument is given, operate along this dimension.
##
## The optional parameter @var{rtype} determines how ties are handled.  All
## examples below assume an input of @code{[ 1, 2, 2, 4 ]}.
##
## @table @asis
## @item 0 or @qcode{"fractional"} (default) for fractional ranking (1, 2.5,
## 2.5, 4);
##
## @item 1 or @qcode{"competition"} for competition ranking (1, 2, 2, 4);
##
## @item 2 or @qcode{"modified"} for modified competition ranking (1, 3, 3, 4);
##
## @item 3 or @qcode{"ordinal"} for ordinal ranking (1, 2, 3, 4);
##
## @item 4 or @qcode{"dense"} for dense ranking (1, 2, 2, 3).
## @end table
##
## @seealso{spearman, kendall}
## @end deftypefn

function y = ranks (x, dim, rtype = 0)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("ranks: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);

  if (nargin < 2 || isempty (dim))
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (! (isscalar (dim) && dim == fix (dim) && dim > 0))
      error ("ranks: DIM must be an integer and a valid dimension");
    endif
  endif

  if (sz(dim) == 1)
    y = ones (sz);  # dimension DIM is singleton, so all are ranked first.
  else
    ## The algorithm works only on dim = 1, so permute if necessary.
    ## FIXME: Most all functions now accept a dim argument.
    ##        Would it be faster not to permute and use the dim argument
    ##        to sort, find, cumsum, etc.?
    if (dim != 1)
      perm = [1 : nd];
      perm(1) = dim;
      perm(dim) = 1;
      x = permute (x, perm);
      sz = size (x);
    endif

    [sx, ids] = sort (x);  # sx is sorted x.
    lin = repmat ((1:rows (x))', [1, sz(2:end)]);  # linearly increasing array.

    switch (rtype)
      case {0, "fractional"}
        lin = (_competition (lin, sx, sz) + _modified (lin, sx, sz)) / 2;
      case {1, "competition"}
        lin = _competition (lin, sx, sz);
      case {2, "modified"}
        lin = _modified (lin, sx, sz);
      case {3, "ordinal"}
        ## no processing needed here.
      case {4, "dense"}
        lin = _dense (lin, sx, sz);
      otherwise
        if (! ischar (rtype))
          rtype = num2str (rtype);
        endif
        error ("ranks: unknown RTYPE '%s'", rtype);
    endswitch

    y = NaN (size (lin));

    ## Offsets to map indices into each column to indices into the linear array.
    ## FIXME: Would sub2ind be faster here?
    idf = zeros (sz);
    idf(1, :) = 0 : sz(1) : (numel (ids)-1);
    idf(:, :) = repmat (idf(1, :), [sz(1), ones(1,length(sz)-1)]);
    y(ids + idf) = lin;

    if (dim != 1)
      y = permute (y, perm);
    endif
  endif

endfunction

function linnew = _dense (lin, sx, sz)
  infvec = -Inf ([1, sz(2:end)]);
  fnewp = logical (diff ([infvec; sx]));
  linnew = cumsum (fnewp, 1);
endfunction

function linnew = _competition (lin, sx, sz)

  ## Stop increasing lin when sx does not increase.  Otherwise, same as before.
  infvec = -Inf ([1, sz(2:end)]);
  fnewp = find (diff ([infvec; sx]));
  linnew = zeros (size (lin));
  linnew(fnewp) = lin(fnewp);
  linnew = cummax (linnew, 1);

endfunction

function linnew = _modified (lin, sx, sz)

  ## Traverse lin backwards.  Stop decreasing it when sx doesn't decrease.
  infvec = Inf ([1, sz(2:end)]);
  fnewp = find (diff ([sx; infvec]));
  linnew = Inf (size (lin));
  linnew(fnewp) = lin(fnewp);
  linnew = flip (cummin (flip (linnew, 1)), 1);

endfunction


%!assert (ranks (1:2:10), 1:5)
%!assert (ranks (10:-2:1), 5:-1:1)
%!assert (ranks ([2, 1, 2, 4]), [2.5, 1, 2.5, 4])
%!assert (ranks (ones (1, 5)), 3*ones (1, 5))
%!assert (ranks (1e6*ones (1, 5)), 3*ones (1, 5))
%!assert (ranks (rand (1, 5), 1), ones (1, 5))

%!assert (ranks ([1, 2, 2, 4], [], "fractional"), [1, 2.5, 2.5, 4])
%!assert (ranks ([1, 2, 2, 4], [], "competition"), [1, 2, 2, 4])
%!assert (ranks ([1, 2, 2, 4], [], "modified"), [1, 3, 3, 4])
%!assert (ranks ([1, 2, 2, 4], [], "ordinal"), [1, 2, 3, 4])
%!assert (ranks ([1, 2, 2, 4], [], "dense"), [1, 2, 2, 3])

## Test input validation
%!error <Invalid call> ranks ()
%!error <X must be a numeric vector or matrix> ranks ({1, 2})
%!error <X must be a numeric vector or matrix> ranks (['A'; 'B'])
%!error <DIM must be an integer> ranks (1, 1.5)
%!error <DIM must be .* a valid dimension> ranks (1, 0)
%!error <unknown RTYPE 'foobar'> ranks (ones (2), 1, "foobar")
