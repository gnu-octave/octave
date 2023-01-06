########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} pqpnonneg (@var{c}, @var{d})
## @deftypefnx {} {@var{x} =} pqpnonneg (@var{c}, @var{d}, @var{x0})
## @deftypefnx {} {@var{x} =} pqpnonneg (@var{c}, @var{d}, @var{x0}, @var{options})
## @deftypefnx {} {[@var{x}, @var{minval}] =} pqpnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{minval}, @var{exitflag}] =} pqpnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{minval}, @var{exitflag}, @var{output}] =} pqpnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{minval}, @var{exitflag}, @var{output}, @var{lambda}] =} pqpnonneg (@dots{})
##
## Minimize @code{ (1/2 * @var{x}' * @var{c} * @var{x} + @var{d}' * @var{x}) }
## subject to @code{@var{x} >= 0}.
##
## @var{c} and @var{d} must be real matrices, and @var{c} must be symmetric and
## positive definite.
##
## @var{x0} is an optional initial guess for the solution @var{x}.
##
## @var{options} is an options structure to change the behavior of the
## algorithm (@pxref{XREFoptimset,,@code{optimset}}).  @code{pqpnonneg}
## recognizes one option: @qcode{"MaxIter"}.
##
## Outputs:
##
## @table @var
##
## @item x
## The solution matrix
##
## @item minval
## The minimum attained model value,
## @code{1/2*@var{xmin}'*@var{c}*@var{xmin} + @var{d}'*@var{xmin}}
##
## @item exitflag
## An indicator of convergence.  0 indicates that the iteration count was
## exceeded, and therefore convergence was not reached; >0 indicates that the
## algorithm converged.  (The algorithm is stable and will converge given
## enough iterations.)
##
## @item output
## A structure with two fields:
##
## @itemize @bullet
## @item @qcode{"algorithm"}: The algorithm used (@nospell{@qcode{"nnls"}})
##
## @item @qcode{"iterations"}: The number of iterations taken.
## @end itemize
##
## @item lambda
## Lagrange multipliers.  If these are nonzero, the corresponding @var{x}
## values should be zero, indicating the solution is pressed up against a
## coordinate plane.  The magnitude indicates how much the residual would
## improve if the @code{@var{x} >= 0} constraints were relaxed in that
## direction.
##
## @end table
## @seealso{lsqnonneg, qp, optimset}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("pqpnonneg");

## This is analogical to the lsqnonneg implementation, which is implemented
## from Lawson and Hanson's 1973 algorithm on page 161 of Solving Least Squares
## Problems.  It shares the convergence guarantees.

function [x, minval, exitflag, output, lambda] = pqpnonneg (c, d, x0 = [],
                                                            options = struct ())

  ## Special case: called to find default optimization options
  if (nargin == 1 && ischar (c) && strcmp (c, "defaults"))
    x = struct ("MaxIter", 1e5);
    return;
  endif

  if (nargin < 2)
    print_usage ();
  endif

  if (! (isnumeric (c) && ismatrix (c)) || ! (isnumeric (d) && ismatrix (d)))
    error ("pqpnonneg: C and D must be numeric matrices");
  endif
  if (! issquare (c))
    error ("pqpnonneg: C must be a square matrix");
  endif

  if (! isstruct (options))
    error ("pqpnonneg: OPTIONS must be a struct");
  endif

  ## Lawson-Hanson Step 1 (LH1): initialize the variables.
  n = columns (c);
  if (isempty (x0))
    ## Initial guess is all zeros.
    x = zeros (n, 1);
  else
    ## ensure nonnegative guess.
    x = max (x0, 0);
  endif

  max_iter = optimget (options, "MaxIter", 1e5);

  ## Initialize P, according to zero pattern of x.
  p = find (x > 0).';
  ## Initialize the Cholesky factorization.
  r = chol (c(p, p));
  usechol = true;

  iter = 0;

  ## LH3: test for completion.
  while (iter < max_iter)
    while (iter < max_iter)
      iter += 1;

      ## LH6: compute the positive matrix and find the min norm solution
      ## of the positive problem.
      if (usechol)
        xtmp = -(r \ (r' \ d(p)));
      else
        xtmp = -(c(p,p) \ d(p));
      endif
      idx = find (xtmp < 0);

      if (isempty (idx))
        ## LH7: tmp solution found, iterate.
        x(:) = 0;
        x(p) = xtmp;
        break;
      else
        ## LH8, LH9: find the scaling factor.
        pidx = p(idx);
        sf = x(pidx) ./ (x(pidx) - xtmp(idx));
        alpha = min (sf);
        ## LH10: adjust X.
        xx = zeros (n, 1);
        xx(p) = xtmp;
        x += alpha*(xx - x);
        ## LH11: move from P to Z all X == 0.
        ## This corresponds to those indices where minimum of sf is attained.
        idx = idx(sf == alpha);
        p(idx) = [];
        if (usechol)
          ## update the Cholesky factorization.
          r = choldelete (r, idx);
        endif
      endif
    endwhile

    ## compute the gradient.
    w = -(d + c*x);
    w(p) = [];
    if (! any (w > 0))
      if (usechol)
        ## verify the solution achieved using qr updating.
        ## In the best case, this should only take a single step.
        usechol = false;
        continue;
      else
        ## we're finished.
        break;
      endif
    endif

    ## find the maximum gradient.
    idx = find (w == max (w));
    if (numel (idx) > 1)
      warning ("pqpnonneg:nonunique",
               "a non-unique solution may be returned due to equal gradients");
      idx = idx(1);
    endif
    ## move the index from Z to P.  Keep P sorted.
    z = [1:n]; z(p) = [];
    zidx = z(idx);
    jdx = 1 + lookup (p, zidx);
    p = [p(1:jdx-1), zidx, p(jdx:end)];
    if (usechol)
      ## insert the column into the Cholesky factorization.
      [r, bad] = cholinsert (r, jdx, c(p,zidx));
      if (bad)
        ## If the insertion failed, we switch off updates and go on.
        usechol = false;
      endif
    endif

  endwhile
  ## LH12: complete.

  ## Generate the additional output arguments.
  if (isargout (2))
    minval = 1/2*(x'*c*x) + d'*x;
  endif
  if (isargout (3))
    if (iter >= max_iter)
      exitflag = 0;
    else
      exitflag = iter;
    endif
  endif
  if (isargout (4))
    output = struct ("algorithm", "nnls-pqp", "iterations", iter);
  endif
  if (isargout (5))
    lambda = zeros (size (x));
    lambda (setdiff (1:numel(x), p)) = w;
  endif

endfunction


%!test
%! C = [5 2;2 2];
%! d = [3; -1];
%! assert (pqpnonneg (C, d), [0;0.5], 100*eps);

## Test equivalence of lsq and pqp
%!test
%! C = rand (20, 10);
%! d = rand (20, 1);
%! assert (pqpnonneg (C'*C, -C'*d), lsqnonneg (C, d), 100*eps);

## Test Lagrange multiplier duality: lambda .* x == 0
%!test
%! [x, resid, ~, ~, lambda] = pqpnonneg ([3 2; 2 2], [6; 5]);
%! assert (x, [0 0]', eps);
%! assert (resid, 0, eps);
%! assert (lambda, [-6 -5]', eps);
%! assert (x .* lambda, [0 0]');

%!test
%! [x, resid, ~, ~, lambda] = pqpnonneg ([3 2; 2 2], [6; -5]);
%! assert (x, [0 2.5]', eps);
%! assert (resid, -6.25, eps);
%! assert (lambda, [-11 0]', eps);
%! assert (x .* lambda, [0 0]');

%!test
%! [x, resid, ~, ~, lambda] = pqpnonneg ([3 2; 2 2], [-6; 5]);
%! assert (x, [2 0]', eps);
%! assert (resid, -6, eps);
%! assert (lambda, [0 -9]', eps);
%! assert (x .* lambda, [0 0]');

%!test
%! [x, resid, ~, ~, lambda] = pqpnonneg ([3 2; 2 2], [-6; -5]);
%! assert (x, [1 1.5]', 10*eps);
%! assert (resid, -6.75, 10*eps);
%! assert (lambda, [0 0]', eps);
%! assert (x .* lambda, [0 0]');

## Test input validation
%!error <Invalid call> pqpnonneg ()
%!error <Invalid call> pqpnonneg (1)
%!error <C .* must be numeric matrices> pqpnonneg ({1},2)
%!error <C .* must be numeric matrices> pqpnonneg (ones (2,2,2),2)
%!error <D must be numeric matrices> pqpnonneg (1,{2})
%!error <D must be numeric matrices> pqpnonneg (1, ones (2,2,2))
%!error <C must be a square matrix> pqpnonneg ([1 2], 2)
%!error <OPTIONS must be a struct> pqpnonneg (1, 2, [], 3)
