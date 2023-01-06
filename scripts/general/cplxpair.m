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
## @deftypefn  {} {@var{zsort} =} cplxpair (@var{z})
## @deftypefnx {} {@var{zsort} =} cplxpair (@var{z}, @var{tol})
## @deftypefnx {} {@var{zsort} =} cplxpair (@var{z}, @var{tol}, @var{dim})
## Sort the numbers @var{z} into complex conjugate pairs ordered by increasing
## real part.
##
## The negative imaginary complex numbers are placed first within each pair.
## All real numbers (those with
## @code{abs (imag (@var{z})) / abs (@var{z}) < @var{tol}}) are placed after
## the complex pairs.
##
## @var{tol} is a weighting factor in the range [0, 1) which determines the
## tolerance of the matching.  The default value is @code{100 * eps} and the
## resulting tolerance for a given complex pair is
## @code{@var{tol} * abs (@var{z}(i)))}.
##
## By default the complex pairs are sorted along the first non-singleton
## dimension of @var{z}.  If @var{dim} is specified, then the complex pairs are
## sorted along this dimension.
##
## Signal an error if some complex numbers could not be paired.  Signal an
## error if all complex numbers are not exact conjugates (to within @var{tol}).
## Note that there is no defined order for pairs with identical real parts but
## differing imaginary parts.
## @c Set example in small font to prevent overfull line
##
## @smallexample
## cplxpair (exp (2i*pi*[0:4]'/5)) == exp (2i*pi*[3; 2; 4; 1; 0]/5)
## @end smallexample
## @end deftypefn

## 2006-05-12 David Bateman - Modified for NDArrays

function zsort = cplxpair (z, tol, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (z))
    zsort = zeros (size (z));
    return;
  endif

  cls = ifelse (isa (z, "single"), "single", "double");
  if (nargin < 2 || isempty (tol))
    tol = 100*eps (cls);
  elseif (! isscalar (tol) || tol < 0 || tol >= 1)
    error ("cplxpair: TOL must be a scalar number in the range 0 <= TOL < 1");
  endif

  nd = ndims (z);
  if (nargin < 3)
    ## Find the first singleton dimension.
    sz = size (z);
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    dim = floor (dim);
    if (dim < 1 || dim > nd)
      error ("cplxpair: invalid dimension DIM");
    endif
  endif

  ## Move dimension to analyze to first position, and convert to a 2-D matrix.
  perm = [dim:nd, 1:dim-1];
  z = permute (z, perm);
  sz = size (z);
  n = sz(1);
  m = prod (sz) / n;
  z = reshape (z, n, m);

  ## Sort the sequence in terms of increasing real values.
  [~, idx] = sort (real (z), 1);
  z = z(idx + n * ones (n, 1) * [0:m-1]);

  ## Put the purely real values at the end of the returned list.
  [idxi, idxj] = find (abs (imag (z)) ./ (abs (z) + realmin (cls)) <= tol);
  ## Force values detected to be real within tolerance to actually be real.
  z(idxi + n*(idxj-1)) = real (z(idxi + n*(idxj-1)));
  q = sparse (idxi, idxj, 1, n, m);
  nr = sum (q, 1);
  [~, idx] = sort (q, 1);
  midx = idx + rows (idx) * ones (rows (idx), 1) * [0:columns(idx)-1];
  z = z(midx);
  zsort = z;

  ## For each remaining z, place the value and its conjugate at the start of
  ## the returned list, and remove them from further consideration.
  for j = 1:m
    p = n - nr(j);
    for i = 1:2:p
      if (i+1 > p)
        error ("cplxpair: could not pair all complex numbers");
      endif
      [v, idx] = min (abs (z(i+1:p,j) - conj (z(i,j))));
      if (v >= tol * abs (z(i,j)))
        error ("cplxpair: could not pair all complex numbers");
      endif
      ## For pairs, select the one with positive imaginary part and use it and
      ## it's conjugate, but list the negative imaginary pair first.
      if (imag (z(i,j)) > 0)
        zsort([i, i+1],j) = [conj(z(i,j)), z(i,j)];
      else
        zsort([i, i+1],j) = [conj(z(idx+i,j)), z(idx+i,j)];
      endif
      z(idx+i,j) = z(i+1,j);
    endfor
  endfor

  ## Reshape the output matrix.
  zsort = ipermute (reshape (zsort, sz), perm);

endfunction


%!demo
%! [ cplxpair(exp(2i*pi*[0:4]'/5)), exp(2i*pi*[3; 2; 4; 1; 0]/5) ]

%!assert (isempty (cplxpair ([])))
%!assert (cplxpair (1), 1)
%!assert (cplxpair ([1+1i, 1-1i]), [1-1i, 1+1i])
%!assert (cplxpair ([1+1i, 1+1i, 1, 1-1i, 1-1i, 2]), ...
%!                  [1-1i, 1+1i, 1-1i, 1+1i, 1, 2])
%!assert (cplxpair ([1+1i; 1+1i; 1; 1-1i; 1-1i; 2]), ...
%!                  [1-1i; 1+1i; 1-1i; 1+1i; 1; 2])
%!assert (cplxpair ([0, 1, 2]), [0, 1, 2])

%!shared z,y
%! z = exp (2i*pi*[4; 3; 5; 2; 6; 1; 0]/7);
%! z(2) = conj (z(1));
%! z(4) = conj (z(3));
%! z(6) = conj (z(5));
%!assert (cplxpair (z(randperm (7))), z)
%!assert (cplxpair (z(randperm (7))), z)
%!assert (cplxpair (z(randperm (7))), z)
%!assert (cplxpair ([z(randperm (7)), z(randperm (7))]), [z,z])
%!assert (cplxpair ([z(randperm (7)), z(randperm (7))],[],1), [z,z])
%!assert (cplxpair ([z(randperm (7)).'; z(randperm (7)).'],[],2), [z.';z.'])
%! y = [ -1-1i; -1+1i;-3; -2; 1; 2; 3];
%!assert (cplxpair ([z(randperm (7)), y(randperm (7))]), [z,y])
%!assert (cplxpair ([z(randperm (7)), y(randperm (7)),z(randperm (7))]),
%!        [z,y,z])

## Test tolerance
%!assert (cplxpair ([2000 * (1+eps) + 4j; 2000 * (1-eps) - 4j]),
%!        [(2000 - 4j); (2000 + 4j)], 100*eps(200))
%!error <could not pair>
%! cplxpair ([2000 * (1+eps) + 4j; 2000 * (1-eps) - 4j], 0);
%!error <could not pair>
%! cplxpair ([2e6 + j; 2e6 - j; 1e-9 * (1 + j); 1e-9 * (1 - 2j)]);

## Test input validation
%!error <Invalid call> cplxpair ()
%!error <cplxpair: TOL must be .* scalar number> cplxpair (1, ones (2,2))
%!error <cplxpair: TOL must be .* in the range 0 <= TOL < 1> cplxpair (1, -1)
%!error <cplxpair: TOL must be .* in the range 0 <= TOL < 1> cplxpair (1, -1)
%!error <invalid dimension DIM> cplxpair (1, [], 3)
