########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{b} =} unwrap (@var{x})
## @deftypefnx {} {@var{b} =} unwrap (@var{x}, @var{tol})
## @deftypefnx {} {@var{b} =} unwrap (@var{x}, @var{tol}, @var{dim})
## Unwrap radian phases by adding or subtracting multiples of 2*pi as
## appropriate to remove jumps greater than @var{tol}.
##
## @var{tol} defaults to pi.
##
## @code{unwrap} will work along the dimension @var{dim}.  If @var{dim}
## is unspecified it defaults to the first non-singleton dimension.
##
## @code{unwrap} ignores all non-finite input values (Inf, NaN, NA).
##
## @end deftypefn

function retval = unwrap (x, tol, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("unwrap: X must be numeric");
  endif

  if (nargin < 2 || isempty (tol))
    tol = pi;
  endif

  ## Don't let anyone use a negative value for TOL.
  tol = abs (tol);

  nd = ndims (x);
  sz = size (x);
  if (nargin == 3)
    if (!(isnumeric (dim) && isscalar (dim) && ...
            dim == fix (dim)) || !(1 <= dim))
      error ("unwrap: DIM must be an integer and a valid dimension");
    endif
  else
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  endif

  rng = 2*pi;

  ## Handle case where we are trying to unwrap a scalar, or only have
  ## one sample in the specified dimension (a given when dim > nd).
  if ((dim > nd) || (m = sz(dim) == 1))
    retval = x;
    return;
  endif

  if (all (isfinite (x(:))))

    ## Take first order difference so that wraps will show up as large values
    ## and the sign will show direction.
    sz(dim) = 1;
    zero_padding = zeros (sz);
    d = cat (dim, zero_padding, -diff (x, 1, dim));

    ## Find only the peaks and multiply them by the appropriate amount
    ## of ranges so that there are kronecker deltas at each wrap point
    ## multiplied by the appropriate amount of range values.
    p = round (abs (d)./rng) .* rng .* (((d > tol) > 0) - ((d < -tol) > 0));

    ## Integrate this so that the deltas become cumulative steps to shift
    ## the original data.
    retval = cumsum (p, dim) + x;

  else
    ## Unwrap needs to skip over NaN, NA, Inf in wrapping calculations.

    if (isvector (x))
      ## Simlpified path for vector inputs.

      retval = x;
      xfin_idx = isfinite (x);
      xfin = x(xfin_idx);
      d = cat (dim, 0, -diff(xfin, 1, dim));
      p = round (abs (d)./rng) .* rng .* ...
                      (((d > tol) > 0) - ((d < -tol) > 0));
      retval(xfin_idx) = xfin + cumsum (p, dim);

    else
      ## For n-dimensional arrays with a possibly unequal number of non-finite
      ## values, mask entries with values that do not impact calcualation.

            ## Locate nonfinite values.
      nf_idx = ! isfinite (x);

      if (all (nf_idx(:)))
        ## Trivial case, all non-finite values
        retval = x;
        return;
      endif

      ## Permute all operations to occur along dim 1.  Inverse permute at end.
      permuteflag = dim != 1;
      if (permuteflag)
        perm_idx = [1 : nd];
        perm_idx([1, dim]) = [dim, 1];

        x = permute (x, perm_idx);
        nf_idx = permute (nf_idx, perm_idx);
        sz([1, dim]) = sz([dim, 1]);
        dim = 1;
      endif

      ## Substitute next value in dim direction for nonfinite values(ignoring
      ## any at trailing end) to prevent calculation impact.

      x_nf = x(nf_idx); # Store nonfinite values.

      zero_padding = zeros ([1, sz(2:end)]);
      x = __fill_nonfinite_columnwise__ (x, nf_idx, zero_padding, sz, nd);

      d = [zero_padding; -diff(x, 1, 1)];

      p = round (abs (d)./rng) .* rng .* ...
          (((d > tol) > 0) - ((d < -tol) > 0));

      retval = x + cumsum (p, 1);

      ## Restore nonfinite values.
      retval(nf_idx) = x_nf;

      ## Invert permutation.
      if (permuteflag)
        retval = ipermute (retval, perm_idx);
      endif

    endif
  endif

endfunction


function x = __fill_nonfinite_columnwise__ (x, nonfinite_loc, zero_padding, szx, ndx)
  ## Replace non-finite values of x, as indicated by logical index
  ## nonfinite_loc, with next values.

  ## TODO: This is a streamlined version of the fillmissing 'next' method from
  ## the statistics package.  Function calls can be replaced by:
  ## fillmissing (x, 'next', 1, 'missinglocations', nonfinite_loc)
  ## if/when that is added to Octave core if full function overhead is okay.

  ## Build index for faster/simpler inline replacement for flipud
  flip_idx(1:ndx) = {':'};
  flip_idx(1) = {szx(1):-1:1};

  ## Isolate nf values by location:
  nf_front = cumprod (nonfinite_loc, 1);
  nf_back = cumprod (nonfinite_loc(flip_idx{:}), 1)(flip_idx{:});
  nf_middle = nonfinite_loc & ! (nf_back | nf_front);

  ## Process bound/middle elements
  locs_before = [diff(nf_middle, 1, 1); zero_padding] == 1;
  locs_after = diff ([zero_padding; nf_middle], 1, 1) == -1;
  mid_gap_sizes = find (locs_after) - find (locs_before) - 1;
  x(nf_middle) = repelems (x(locs_after), ...
                          [1 : numel(mid_gap_sizes); mid_gap_sizes'])';

  ## Process front edge elements
  nf_front = nf_front & ! all (nonfinite_loc, 1); # Remove all nf columns.
  locs_after = diff ([zero_padding; nf_front], 1, 1) == -1;
  front_gap_sizes = (sum (nf_front, 1))(any (nf_front, 1))(:);
  x(nf_front) = repelems (x(locs_after), ...
                             [1:numel(front_gap_sizes); front_gap_sizes'])';

endfunction


%!shared i, t, r, w, tol
%! i = 0;
%! t = [];
%! r = [0:100];                         ## original vector
%! w = r - 2*pi*floor ((r+pi)/(2*pi));  ## wrapped into [-pi,pi]
%! tol = 1e3*eps;

%!assert (r,  unwrap (w),  tol)
%!assert (r', unwrap (w'), tol)
%!assert ([r',r'], unwrap ([w',w']), tol)
%!assert ([r; r ], unwrap ([w; w ], [], 2), tol)
%!assert (r + 10, unwrap (10 + w), tol)

%!assert (w', unwrap (w', [], 2))
%!assert (w,  unwrap (w,  [], 1))
%!assert ([w; w], unwrap ([w; w]))

## Test that small values of tol have the same effect as tol = pi
%!assert (r, unwrap (w, 0.1), tol)
%!assert (r, unwrap (w, eps), tol)

%!shared # Clear shared variables to avoid echo on subsequent failures.

## Test that phase changes larger than 2*pi unwrap properly
%!assert ([0;  1],        unwrap ([0;  1]))
%!assert ([0;  4 - 2*pi], unwrap ([0;  4]))
%!assert ([0;  7 - 2*pi], unwrap ([0;  7]))
%!assert ([0; 10 - 4*pi], unwrap ([0; 10]))
%!assert ([0; 13 - 4*pi], unwrap ([0; 13]))
%!assert ([0; 16 - 6*pi], unwrap ([0; 16]))
%!assert ([0; 19 - 6*pi], unwrap ([0; 19]))
%!assert (max (abs (diff (unwrap (100*pi * rand (1000, 1))))) < pi)

%!test
%! A = [pi*(-4), pi*(-2+1/6), pi/4, pi*(2+1/3), pi*(4+1/2), pi*(8+2/3), pi*(16+1), pi*(32+3/2), pi*64];
%! assert (unwrap (A), unwrap (A, pi));
%! assert (unwrap (A, pi), unwrap (A, pi, 2));
%! assert (unwrap (A', pi), unwrap (A', pi, 1));

%!test
%! A = [pi*(-4); pi*(2+1/3); pi*(16+1)];
%! B = [pi*(-2+1/6); pi*(4+1/2); pi*(32+3/2)];
%! C = [pi/4; pi*(8+2/3); pi*64];
%! D = [pi*(-2+1/6); pi*(2+1/3); pi*(8+2/3)];
%! E(:, :, 1) = [A, B, C, D];
%! E(:, :, 2) = [A+B, B+C, C+D, D+A];
%! F(:, :, 1) = [unwrap(A), unwrap(B), unwrap(C), unwrap(D)];
%! F(:, :, 2) = [unwrap(A+B), unwrap(B+C), unwrap(C+D), unwrap(D+A)];
%! assert (unwrap (E), F);

%!test
%! A = [0, 2*pi, 4*pi, 8*pi, 16*pi, 65536*pi];
%! B = [pi*(-2+1/6), pi/4, pi*(2+1/3), pi*(4+1/2), pi*(8+2/3), pi*(16+1), pi*(32+3/2), pi*64];
%! assert (unwrap (A), zeros (1, length (A)));
%! assert (diff (unwrap (B), 1) < 2*pi, true (1, length (B)-1));

## Test trivial return for m = 1 and dim > nd
%!assert (unwrap (ones(4,1), [], 1), ones(4,1))
%!assert (unwrap (ones(4,1), [], 2), ones(4,1))
%!assert (unwrap (ones(4,1), [], 3), ones(4,1))
%!assert (unwrap (ones(4,3,2), [], 99), ones(4,3,2))

## Test empty input return
%!assert (unwrap ([]), [])
%!assert (unwrap (ones (1,0)), ones (1,0))
%!assert (unwrap (ones (1,0), [], 1), ones (1,0))
%!assert (unwrap (ones (1,0), [], 2), ones (1,0))
%!assert (unwrap (ones (1,0), [], 3), ones (1,0))

## Test handling of non-finite values
%!assert <*64556> (unwrap (NaN(4,1)), NaN(4,1))
%!assert <*64556> (unwrap (NaN(4)), NaN(4))

%!test <*64556>
%! x = pi * [-Inf, 0.5, -1, NaN, Inf, -0.5, 1];
%! assert (unwrap (x), pi * [-Inf, 0.5, 1, NaN, Inf, 1.5, 1], eps)
%! assert (unwrap (x.'), pi * [-Inf, 0.5, 1, NaN, Inf, 1.5, 1].', eps)

%!test <*64556>
%! x = pi * [-Inf, 0.5, -1, NaN, Inf, -0.5, 1];
%! y = unwrap ([x; fliplr(x); NaN(1, 7)], [], 2);
%! z = pi * [-Inf, 0.5, 1, NaN, Inf, 1.5, 1; 1, 1.5, Inf, NaN, 1, 0.5, -Inf; NaN(1,7)];
%! assert (y, z, eps);


## Test input validation
%!error <Invalid call> unwrap ()
%!error unwrap (1, 2, 3, 4)
%!error <X must be numeric> unwrap ("foo")
%!error <X must be numeric> unwrap ({1})
%!error <X must be numeric> unwrap (struct())
%!error <DIM must be an> unwrap (1, 2, "foo")
%!error <DIM must be an> unwrap (1, 2, -1)
%!error <DIM must be an> unwrap (1, 2, 1.5)
%!error <DIM must be an> unwrap (1, 2, {1})
%!error <DIM must be an> unwrap (1, 2, struct())
