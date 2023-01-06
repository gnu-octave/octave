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
## @deftypefn  {} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H})
## @deftypefnx {} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q})
## @deftypefnx {} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q}, @var{A}, @var{b})
## @deftypefnx {} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q}, @var{A}, @var{b}, @var{lb}, @var{ub})
## @deftypefnx {} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q}, @var{A}, @var{b}, @var{lb}, @var{ub}, @var{A_lb}, @var{A_in}, @var{A_ub})
## @deftypefnx {} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@dots{}, @var{options})
## Solve a quadratic program (QP).
##
## Solve the quadratic program defined by
## @tex
## $$
##  \min_x {1 \over 2} x^T H x + x^T q
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## min 0.5 x'*H*x + x'*q
##  x
## @end group
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##  A x = b \qquad lb \leq x \leq ub \qquad A_{lb} \leq A_{in} x \leq A_{ub}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## A*x = b
## lb <= x <= ub
## A_lb <= A_in*x <= A_ub
## @end group
## @end example
##
## @end ifnottex
## @noindent
## using a null-space active-set method.
##
## Any bound (@var{A}, @var{b}, @var{lb}, @var{ub}, @var{A_in}, @var{A_lb},
## @var{A_ub}) may be set to the empty matrix (@code{[]}) if not present.  The
## constraints @var{A} and @var{A_in} are matrices with each row representing
## a single constraint.  The other bounds are scalars or vectors depending on
## the number of constraints.  The algorithm is faster if the initial guess is
## feasible.
##
## @var{options} is a structure specifying additional parameters which
## control the algorithm.  Currently, @code{qp} recognizes these options:
## @qcode{"MaxIter"}, @qcode{"TolX"}.
##
## @qcode{"MaxIter"} proscribes the maximum number of algorithm iterations
## before optimization is halted.  The default value is 200.
## The value must be a positive integer.
##
## @qcode{"TolX"} specifies the termination tolerance for the unknown variables
## @var{x}.  The default is @code{sqrt (eps)} or approximately 1e-8.
##
## On return, @var{x} is the location of the minimum and @var{fval} contains
## the value of the objective function at @var{x}.
##
## @table @var
## @item info
## Structure containing run-time information about the algorithm.  The
## following fields are defined:
##
## @table @code
## @item solveiter
## The number of iterations required to find the solution.
##
## @item info
## An integer indicating the status of the solution.
##
## @table @asis
## @item 0
## The problem is feasible and convex.  Global solution found.
##
## @item 1
## The problem is not convex.  Local solution found.
##
## @item 2
## The problem is not convex and unbounded.
##
## @item 3
## Maximum number of iterations reached.
##
## @item 6
## The problem is infeasible.
## @end table
## @end table
## @end table
## @seealso{sqp}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("qp");

function [x, obj, INFO, lambda] = qp (x0, H, varargin)

  if (nargin == 1 && ischar (x0) && strcmp (x0, "defaults"))
    x = struct ("MaxIter", 200, "TolX", sqrt (eps));
    return;
  endif

  nargs = nargin;
  if (nargs > 2 && isstruct (varargin{end}))
    options = varargin{end};
    nargs -= 1;
  else
    options = struct ();
  endif

  if (nargs != 2 && nargs != 3 && nargs != 5 && nargs != 7 && nargs != 10)
    print_usage ();
  endif

  if (nargs >= 3)
    q = varargin{1};
  else
    q = [];
  endif

  if (nargs >= 5)
    A = varargin{2};
    b = varargin{3};
  else
    A = [];
    b = [];
  endif

  if (nargs >= 7)
    lb = varargin{4};
    ub = varargin{5};
  else
    lb = [];
    ub = [];
  endif

  if (nargs == 10)
    A_lb = varargin{6};
    A_in = varargin{7};
    A_ub = varargin{8};
  else
    A_lb = [];
    A_in = [];
    A_ub = [];
  endif

  maxit = optimget (options, "MaxIter", 200);
  tol = optimget (options, "TolX", sqrt (eps));

  ## Validate the quadratic penalty.
  if (! issquare (H))
    error ("qp: quadratic penalty matrix must be square");
  elseif (! ishermitian (H))
    ## warning ("qp: quadratic penalty matrix not hermitian");
    H = (H + H')/2;
  endif
  n = rows (H);

  ## Validate the initial guess.
  ## If empty it is resized to the right dimension and filled with 0.
  if (isempty (x0))
    x0 = zeros (n, 1);
  else
    if (! isvector (x0))
      error ("qp: the initial guess X0 must be a vector");
    elseif (numel (x0) != n)
      error ("qp: the initial guess X0 has incorrect length");
    endif
    x0 = x0(:);  # always use column vector.
  endif

  ## Validate linear penalty.
  if (isempty (q))
    q = zeros (n, 1);
  else
    if (! isvector (q))
      error ("qp: Q must be a vector");
    elseif (numel (q) != n)
      error ("qp: Q has incorrect length");
    endif
    q = q(:);   # always use column vector.
  endif

  ## Validate equality constraint matrices.
  if (isempty (A) || isempty (b))
    A = zeros (0, n);
    b = zeros (0, 1);
    n_eq = 0;
  else
    [n_eq, n1] = size (A);
    if (n1 != n)
      error ("qp: equality constraint matrix has incorrect column dimension");
    endif
    if (numel (b) != n_eq)
      error ("qp: equality constraint matrix and vector have inconsistent dimensions");
    endif
  endif

  ## Validate bound constraints.
  Ain = zeros (0, n);
  bin = zeros (0, 1);
  n_in = 0;
  if (nargs > 5)
    if (! isempty (lb))
      if (numel (lb) != n)
        error ("qp: lower bound LB has incorrect length");
      elseif (isempty (ub))
        Ain = [Ain; eye(n)];
        bin = [bin; lb];
      endif
    endif

    if (! isempty (ub))
      if (numel (ub) != n)
        error ("qp: upper bound UB has incorrect length");
      elseif (isempty (lb))
        Ain = [Ain; -eye(n)];
        bin = [bin; -ub];
      endif
    endif

    if (! isempty (lb) && ! isempty (ub))
      rtol = tol;
      for i = 1:n
        if (abs (lb (i) - ub(i)) < rtol*(1 + max (abs (lb(i) + ub(i)))))
          ## These are actually an equality constraint
          tmprow = zeros (1,n);
          tmprow(i) = 1;
          A = [A;tmprow];
          b = [b; 0.5*(lb(i) + ub(i))];
          n_eq += 1;
        else
          tmprow = zeros (1,n);
          tmprow(i) = 1;
          Ain = [Ain; tmprow; -tmprow];
          bin = [bin; lb(i); -ub(i)];
          n_in += 2;
        endif
      endfor
    endif
  endif

  ## Validate inequality constraints.
  if (nargs > 7 && isempty (A_in) && ! (isempty (A_lb) || isempty (A_ub)))
    warning ("qp: empty inequality constraint matrix but non-empty bound vectors");
  endif

  if (nargs > 7 && ! isempty (A_in))
    [dimA_in, n1] = size (A_in);
    if (n1 != n)
      error ("qp: inequality constraint matrix has incorrect column dimension, expected %i", n1);
    else
      if (! isempty (A_lb))
        if (numel (A_lb) != dimA_in)
          error ("qp: inequality constraint matrix and lower bound vector are inconsistent, %i != %i", dimA_in, numel (A_lb));
        elseif (isempty (A_ub))
          Ain = [Ain; A_in];
          bin = [bin; A_lb];
        endif
      endif
      if (! isempty (A_ub))
        if (numel (A_ub) != dimA_in)
          error ("qp: inequality constraint matrix and upper bound vector are inconsistent, %i != %i", dimA_in, numel (A_ub));
        elseif (isempty (A_lb))
          Ain = [Ain; -A_in];
          bin = [bin; -A_ub];
        endif
      endif

      if (! isempty (A_lb) && ! isempty (A_ub))
        rtol = tol;
        for i = 1:dimA_in
          if (abs (A_lb(i) - A_ub(i))
              < rtol*(1 + max (abs (A_lb(i) + A_ub(i)))))
            ## These are actually an equality constraint
            tmprow = A_in(i,:);
            A = [A;tmprow];
            b = [b; 0.5*(A_lb(i) + A_ub(i))];
            n_eq += 1;
          else
            tmprow = A_in(i,:);
            Ain = [Ain; tmprow; -tmprow];
            bin = [bin; A_lb(i); -A_ub(i)];
            n_in += 2;
          endif
        endfor
      endif
    endif
  endif

  ## Now we should have the following QP:
  ##
  ##   min_x  0.5*x'*H*x + x'*q
  ##   s.t.   A*x = b
  ##          Ain*x >= bin

  ## Discard inequality constraints that have -Inf bounds since those
  ## will never be active.
  idx = (bin == -Inf);

  bin(idx) = [];
  Ain(idx,:) = [];

  n_in = numel (bin);

  ## Check if the initial guess is feasible.
  if (isa (x0, "single") || isa (H, "single") || isa (q, "single")
      || isa (A, "single") || isa (b, "single"))
    rtol = sqrt (eps ("single"));
  else
    rtol = tol;
  endif

  eq_infeasible = (n_eq > 0 && norm (A*x0-b) > rtol*(1+abs (b)));
  in_infeasible = (n_in > 0 && any (Ain*x0-bin < -rtol*(1+abs (bin))));

  info = 0;

  if (isdefinite (H) != 1)
    info = 2;
  endif

  if (info == 0 && (eq_infeasible || in_infeasible))
    ## The initial guess is not feasible.
    ## First, define an xbar that is feasible with respect to the
    ## equality constraints.
    if (eq_infeasible)
      if (rank (A) < n_eq)
        error ("qp: equality constraint matrix must be full row rank");
      endif
      xbar = pinv (A) * b;
    else
      xbar = x0;
    endif

    ## Second, check that xbar is also feasible with respect to the
    ## inequality constraints.
    if (n_in > 0)
      res = Ain * xbar - bin;
      if (any (res < -rtol * (1 + abs (bin))))
        ## xbar is not feasible with respect to the inequality constraints.
        ## Compute a step in the null space of the equality constraints,
        ## by solving a QP.  If the slack is small, we have a feasible initial
        ## guess.  Otherwise, the problem is infeasible.
        if (n_eq > 0)
          Z = null (A);
          if (isempty (Z))
            ## The problem is infeasible because A is square and full rank,
            ## but xbar is not feasible.
            info = 6;
          endif
        endif

        if (info != 6)
          ## Solve an LP with additional slack variables
          ## to find a feasible starting point.
          gamma = eye (n_in);
          if (n_eq > 0)
            Atmp = [Ain*Z, gamma];
            btmp = -res;
          else
            Atmp = [Ain, gamma];
            btmp = bin;
          endif
          ctmp = [zeros(n-n_eq, 1); ones(n_in, 1)];
          lb = [-Inf(n-n_eq,1); zeros(n_in,1)];
          ub = [];
          ctype = repmat ("L", n_in, 1);
          [P, FMIN, status] = glpk (ctmp, Atmp, btmp, lb, ub, ctype);
          ## FIXME: Test based only on rtol occasionally fails (Bug #38353).
          ## This seems to be a problem in glpk in which return value XOPT(1)
          ## is the same as FMIN.  Workaround this by explicit test
          if (status != 0)
            info = 6;  # The problem is infeasible
          else
            if (all (abs (P(n-n_eq+2:end)) < rtol * (1 + norm (btmp)))
                && (P(n-n_eq+1) < rtol * (1 + norm (btmp))
                    || P(n-n_eq+1) == FMIN))
              ## We found a feasible starting point
              if (n_eq > 0)
                x0 = xbar + Z*P(1:n-n_eq);
              else
                x0 = P(1:n);
              endif
            else
              info = 6;  # The problem is infeasible
            endif
          endif
        endif
      else
        ## xbar is feasible.  We use it a starting point.
        x0 = xbar;
      endif
    else
      ## xbar is feasible.  We use it a starting point.
      x0 = xbar;
    endif
  endif

  if (info == 0)
    ## The initial (or computed) guess is feasible.  Call the solver.
    [x, lambda, info, iter] = __qp__ (x0, H, q, A, b, Ain, bin, maxit, rtol);
  else
    iter = 0;
    x = x0;
    lambda = [];
  endif
  if (isargout (2))
    obj = 0.5 * x' * H * x + q' * x;
  endif
  if (isargout (3))
    INFO.solveiter = iter;
    INFO.info = info;
  endif

endfunction


## Test infeasible initial guess
%!testif HAVE_GLPK <*40536>
%!
%! H = 1;  q = 0;                # objective: x -> 0.5 x^2
%! A = 1;  lb = 1;  ub = +inf;   # constraint: x >= 1
%! x0 = 0;                       # infeasible initial guess
%!
%! [x, obj_qp, INFO, lambda] = qp (x0, H, q, [], [], [], [], lb, A, ub);
%!
%! assert (isstruct (INFO) && isfield (INFO, "info") && (INFO.info == 0));
%! assert ([x obj_qp], [1.0 0.5], eps);

%!test <*61762>
%! [x, obj, info] = qp ([], [21, 30, 39; 30, 45, 60; 39, 60, 81], [-40; -65; -90]);
%! assert (x, zeros (3, 1));
%! assert (obj, 0);
%! assert (info.info, 2);
