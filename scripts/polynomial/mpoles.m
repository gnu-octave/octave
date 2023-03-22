########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{multp}, @var{idxp}] =} mpoles (@var{p})
## @deftypefnx {} {[@var{multp}, @var{idxp}] =} mpoles (@var{p}, @var{tol})
## @deftypefnx {} {[@var{multp}, @var{idxp}] =} mpoles (@var{p}, @var{tol}, @var{reorder})
## Identify unique poles in @var{p} and their associated multiplicity.
##
## By default, the output is ordered from the pole with the largest magnitude
## to the smallest magnitude.
##
## Two poles are considered to be multiples if the difference between them
## is less than the relative tolerance @var{tol}.
##
## @example
## abs (@var{p1} - @var{p0}) / abs (@var{p0}) < @var{tol}
## @end example
##
## If the pole is 0 then no scaling is done and @var{tol} is interpreted as an
## absolute tolerance.  The default value for @var{tol} is 0.001.
##
## If the optional parameter @var{reorder} is false/zero, poles are not
## sorted.
##
## The output @var{multp} is a vector specifying the multiplicity of the poles.
## @code{@var{multp}(n)} refers to the multiplicity of the Nth pole
## @code{@var{p}(@var{idxp}(n))}.
##
## For example:
##
## @example
## @group
## p = [2 3 1 1 2];
## [m, n] = mpoles (p)
##    @result{} m = [1; 1; 2; 1; 2]
##    @result{} n = [2; 5; 1; 4; 3]
##    @result{} p(n) = [3, 2, 2, 1, 1]
## @end group
## @end example
##
## @seealso{residue, poly, roots, conv, deconv}
## @end deftypefn

function [multp, idxp] = mpoles (p, tol, reorder)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isfloat (p))
    error ("mpoles: P must be a single or double floating point vector");
  endif

  if (nargin < 2 || isempty (tol))
    tol = 0.001;
  elseif (! (isscalar (tol) && isreal (tol) && tol > 0))
    error ("mpoles: TOL must be a real scalar greater than 0");
  endif

  if (nargin < 3 || isempty (reorder))
    reorder = true;
  elseif (! (isscalar (reorder) && isreal (reorder)))
    error ("mpoles: REORDER must be a numeric or logical scalar");
  endif

  Np = numel (p);
  p = p(:);  # force poles to be a column vector

  if (reorder)
    ## sort with largest magnitude first
    [~, order] = sort (abs (p), "descend");
    p = p(order);
  else
    order = (1:Np).';
  endif

  ## Create vector of tolerances for use in algorithm.
  vtol = zeros (Np, 1, class (p));
  p_nz = (p != 0);     # non-zero poles
  vtol(! p_nz) = tol;  # use absolute tolerance for zero poles

  ## Find pole multiplicity by comparing relative difference of poles.
  multp = zeros (Np, 1, class (p));
  idxp = [];
  n = find (multp == 0, 1);
  while (n)
    dp = abs (p - p(n));
    vtol(p_nz) = tol * abs (p(n));
    k = find (dp < vtol);
    ## Poles can only be members of one multiplicity group.
    if (numel (idxp))
      k = k(! ismember (k, idxp));
    endif
    m = 1:numel (k);
    multp(k) = m;
    idxp = [idxp; k];
    n = find (multp == 0, 1);
  endwhile
  multp = multp(idxp);
  idxp = order(idxp);

endfunction


%!test
%! [mp, ip] = mpoles ([0 0], 0.01);
%! assert (mp, [1; 2]);

%!test
%! [mp, ip] = mpoles ([-1e4, -0.1, 0]);
%! assert (mp, [1; 1; 1]);
%! assert (ip, [1; 2; 3]);

## Test single inputs
%!test
%! [mp, ip] = mpoles (single ([-1e4, -0.1, 0]));
%! assert (mp, single ([1; 1; 1]));
%! assert (ip, [1; 2; 3]);

## Test relative tolerance criteria
%!test
%! [mp, ip] = mpoles ([1, 1.1, 1.3], .1/1.1);
%! assert (mp, [1; 1; 1]);
%! [mp, ip] = mpoles ([1, 1.1, 1.3], .1/1.1 + eps);
%! assert (mp, [1; 1; 2]);

## Test absolute tolerance criteria with a zero pole
%!test
%! [mp, ip] = mpoles ([0, -0.1, 0.3], .1);
%! assert (mp, [1; 1; 1]);
%! [mp, ip] = mpoles ([0, -0.1, 0.3], .1 + eps);
%! assert (mp, [1; 1; 2]);

## Test input validation
%!error <Invalid call> mpoles ()
%!error <P must be a single or double floating point vector> mpoles (uint8 (1))
%!error <TOL must be a real scalar greater than 0> mpoles (1, [1, 2])
%!error <TOL must be a real scalar greater than 0> mpoles (1, 1i)
%!error <TOL must be a real scalar greater than 0> mpoles (1, 0)
%!error <REORDER must be a numeric or logical scalar> mpoles (1, 1, [1, 2])
%!error <REORDER must be a numeric or logical scalar> mpoles (1, 1, {1})
