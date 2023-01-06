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
## @deftypefn  {} {@var{x} =} lsqnonneg (@var{c}, @var{d})
## @deftypefnx {} {@var{x} =} lsqnonneg (@var{c}, @var{d}, @var{x0})
## @deftypefnx {} {@var{x} =} lsqnonneg (@var{c}, @var{d}, @var{x0}, @var{options})
## @deftypefnx {} {[@var{x}, @var{resnorm}] =} lsqnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{resnorm}, @var{residual}] =} lsqnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{resnorm}, @var{residual}, @var{exitflag}] =} lsqnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{resnorm}, @var{residual}, @var{exitflag}, @var{output}] =} lsqnonneg (@dots{})
## @deftypefnx {} {[@var{x}, @var{resnorm}, @var{residual}, @var{exitflag}, @var{output}, @var{lambda}] =} lsqnonneg (@dots{})
##
## Minimize @code{norm (@var{c}*@var{x} - @var{d})} subject to
## @code{@var{x} >= 0}.
##
## @var{c} and @var{d} must be real matrices.
##
## @var{x0} is an optional initial guess for the solution @var{x}.
##
## @var{options} is an options structure to change the behavior of the
## algorithm (@pxref{XREFoptimset,,@code{optimset}}).  @code{lsqnonneg}
## recognizes these options: @qcode{"MaxIter"}, @qcode{"TolX"}.
##
## Outputs:
##
## @table @var
## @item resnorm
## The squared 2-norm of the residual: @code{norm (@var{c}*@var{x}-@var{d})^2}
##
## @item residual
## The residual: @code{@var{d}-@var{c}*@var{x}}
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
## @item @qcode{"algorithm"}: The algorithm used (@qcode{"nnls"})
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
## @seealso{pqpnonneg, lscov, optimset}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("lsqnonneg");

## This is implemented from Lawson and Hanson's 1973 algorithm on page 161 of
## Solving Least Squares Problems.

function [x, resnorm, residual, exitflag, output, lambda] = lsqnonneg (c, d, x0 = [], options = struct ())

  ## Special case: called to find default optimization options
  if (nargin == 1 && ischar (c) && strcmp (c, "defaults"))
    x = struct ("MaxIter", 1e5);
    return;
  endif

  if (nargin < 2)
    print_usage ();
  endif

  if (! (isnumeric (c) && ismatrix (c)) || ! (isnumeric (d) && ismatrix (d)))
    error ("lsqnonneg: C and D must be numeric matrices");
  endif

  if (! isstruct (options))
    error ("lsqnonneg: OPTIONS must be a struct");
  endif

  ## Lawson-Hanson Step 1 (LH1): initialize the variables.
  m = rows (c);
  n = columns (c);
  if (isempty (x0))
    ## Initial guess is all zeros.
    x = zeros (n, 1);
  else
    ## ensure nonnegative guess.
    x = max (x0, 0);
  endif

  useqr = (m >= n);
  max_iter = optimget (options, "MaxIter", 1e5);

  ## Initialize P, according to zero pattern of x.
  p = find (x > 0).';
  if (useqr)
    ## Initialize the QR factorization, economized form.
    [q, r] = qr (c(:,p), 0);
  endif

  iter = 0;

  ## LH3: test for completion.
  while (iter < max_iter)
    while (iter < max_iter)
      iter += 1;

      ## LH6: compute the positive matrix and find the min norm solution
      ## of the positive problem.
      if (useqr)
        xtmp = r \ q'*d;
      else
        xtmp = c(:,p) \ d;
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
        if (useqr)
          ## update the QR factorization.
          [q, r] = qrdelete (q, r, idx);
        endif
      endif
    endwhile

    ## compute the gradient.
    w = c'*(d - c*x);
    w(p) = [];
    tolx = optimget (options, "TolX", 10*eps*norm (c, 1)*length (c));
    if (! any (w > tolx))
      if (useqr)
        ## verify the solution achieved using qr updating.
        ## in the best case, this should only take a single step.
        useqr = false;
        continue;
      else
        ## we're finished.
        break;
      endif
    endif

    ## find the maximum gradient.
    idx = find (w == max (w));
    if (numel (idx) > 1)
      warning ("lsqnonneg:nonunique",
               "a non-unique solution may be returned due to equal gradients");
      idx = idx(1);
    endif
    ## move the index from Z to P.  Keep P sorted.
    z = [1:n]; z(p) = [];
    zidx = z(idx);
    jdx = 1 + lookup (p, zidx);
    p = [p(1:jdx-1), zidx, p(jdx:end)];
    if (useqr)
      ## insert the column into the QR factorization.
      [q, r] = qrinsert (q, r, jdx, c(:,zidx));
    endif

  endwhile
  ## LH12: complete.

  ## Generate the additional output arguments.
  if (isargout (2))
    resnorm = norm (c*x - d) ^ 2;
  endif
  if (isargout (3))
    residual = d - c*x;
  endif
  if (isargout (4))
    if (iter >= max_iter)
      exitflag = 0;
    else
      exitflag = iter;
    endif
  endif
  if (isargout (5))
    output = struct ("algorithm", "nnls", "iterations", iter);
  endif
  if (isargout (6))
    lambda = zeros (size (x));
    lambda (setdiff (1:numel(x), p)) = w;
  endif

endfunction


%!test
%! C = [1 0;0 1;2 1];
%! d = [1;3;-2];
%! assert (lsqnonneg (C, d), [0;0.5], 100*eps);

%!test
%! C = [0.0372 0.2869;0.6861 0.7071;0.6233 0.6245;0.6344 0.6170];
%! d = [0.8587;0.1781;0.0747;0.8405];
%! xnew = [0;0.6929];
%! assert (lsqnonneg (C, d), xnew, 0.0001);

## Test Lagrange multiplier duality: x .* lambda == 0

%!test
%! [x, resn, resid, ~, ~, lambda] = lsqnonneg ([1 0; 0 1; 2 1], [1 1 3]');
%! assert (x, [1 1]', 10*eps);
%! assert (resn, 0, 10*eps);
%! assert (resid, [0 0 0]', 10*eps);
%! assert (lambda, [0 0]', 10*eps);
%! assert (x .* lambda, [0 0]');

%!test
%! [x, resn, resid, ~, ~, lambda] = lsqnonneg ([1 0; 0 1; 2 1], [1 -1 1]');
%! assert (x, [0.6 0]', 10*eps);
%! assert (resn, 1.2, 10*eps);
%! assert (resid, [0.4 -1 -0.2]', 10*eps);
%! assert (lambda, [0 -1.2]', 10*eps);
%! assert (x .* lambda, [0 0]');

%!test
%! [x, resn, resid, ~, ~, lambda] = lsqnonneg ([1 0; 0 1; 2 1], [-1 1 -1]');
%! assert (x, [0 0]', 10*eps);
%! assert (resn, 3, 10*eps);
%! assert (resid, [-1 1 -1]', 10*eps);
%! assert (lambda, [-3 0]', 10*eps);
%! assert (x .* lambda, [0 0]');

%!test
%! [x, resn, resid, ~, ~, lambda] = lsqnonneg ([1 0; 0 1; 2 1], [-1 -1 -3]');
%! assert (x, [0 0]', 10*eps);
%! assert (resn, 11, 20*eps);
%! assert (resid, [-1 -1 -3]', 10*eps);
%! assert (lambda, [-7 -4]', 10*eps);
%! assert (x .* lambda, [0 0]');

## Test input validation
%!error <Invalid call> lsqnonneg ()
%!error <Invalid call> lsqnonneg (1)
%!error <C .* must be numeric matrices> lsqnonneg ({1},2)
%!error <C .* must be numeric matrices> lsqnonneg (ones (2,2,2),2)
%!error <D must be numeric matrices> lsqnonneg (1,{2})
%!error <D must be numeric matrices> lsqnonneg (1, ones (2,2,2))
%!error <OPTIONS must be a struct> lsqnonneg (1, 2, [], 3)
