## Copyright (C) 2007-2016 Regents of the University of California
## Copyright (C) 2016 Marco Caliari
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{cest} =} condest (@var{A})
## @deftypefnx {} {@var{cest} =} condest (@var{A}, @var{t})
## @deftypefnx {} {@var{cest} =} condest (@var{A}, @var{solvefun}, @var{t}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {@var{cest} =} condest (@var{Afcn}, @var{solvefun}, @var{t}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {[@var{cest}, @var{v}] =} condest (@dots{})
##
## Estimate the 1-norm condition number of a square matrix @var{A} using
## @var{t} test vectors and a randomized 1-norm estimator.
##
## The optional input @var{t} specifies the number of test vectors (default 5).
##
## If the matrix is not explicit, e.g., when estimating the condition number of
## @var{A} given an LU@tie{}factorization, @code{condest} uses the following
## functions:
##
## @itemize @minus
## @item @var{Afcn} which must return
##
## @itemize @bullet
## @item
## the dimension @var{n} of @var{a}, if @var{flag} is @qcode{"dim"}
##
## @item
## true if @var{a} is a real operator, if @var{flag} is @qcode{"real"}
##
## @item
## the result @code{@var{a} * @var{x}}, if @var{flag} is "notransp"
##
## @item
## the result @code{@var{a}' * @var{x}}, if @var{flag} is "transp"
## @end itemize
##
## @item @var{solvefun} which must return
##
## @itemize @bullet
## @item
## the dimension @var{n} of @var{a}, if @var{flag} is @qcode{"dim"}
##
## @item
## true if @var{a} is a real operator, if @var{flag} is @qcode{"real"}
##
## @item
## the result @code{@var{a} \ @var{x}}, if @var{flag} is "notransp"
##
## @item
## the result @code{@var{a}' \ @var{x}}, if @var{flag} is "transp"
## @end itemize
## @end itemize
##
## The parameters @var{p1}, @var{p2}, @dots{} are arguments of
## @code{@var{Afcn} (@var{flag}, @var{x}, @var{p1}, @var{p2}, @dots{})}
## and @code{@var{solvefcn} (@var{flag}, @var{x}, @var{p1}, @var{p2},
## @dots{})}.
##
## The principal output is the 1-norm condition number estimate @var{cest}.
##
## The optional second output is an approximate null vector when @var{cest} is
## large; it satisfies the equation
## @code{norm (A*v, 1) == norm (A, 1) * norm (@var{v}, 1) / @var{est}}.
##
## Algorithm Note:  @code{condest} uses a randomized algorithm to approximate
## the 1-norms.  Therefore, if consistent results are required, the
## @qcode{"state"} of the random generator should be fixed before invoking
## @code{condest}.
##
## References:
##
## @itemize
## @item
## @nospell{N.J. Higham and F. Tisseur}, @cite{A Block Algorithm
## for Matrix 1-Norm Estimation, with an Application to 1-Norm
## Pseudospectra}. SIMAX vol 21, no 4, pp 1185-1201.
## @url{http://dx.doi.org/10.1137/S0895479899356080}
##
## @item
## @nospell{N.J. Higham and F. Tisseur}, @cite{A Block Algorithm
## for Matrix 1-Norm Estimation, with an Application to 1-Norm
## Pseudospectra}. @url{http://citeseer.ist.psu.edu/223007.html}
## @end itemize
##
## @seealso{cond, norm, normest1, normest}
## @end deftypefn

## Code originally licensed under:
##
## Copyright (c) 2007, Regents of the University of California
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
##    * Redistributions of source code must retain the above copyright
##      notice, this list of conditions and the following disclaimer.
##
##    * Redistributions in binary form must reproduce the above
##      copyright notice, this list of conditions and the following
##      disclaimer in the documentation and/or other materials provided
##      with the distribution.
##
##    * Neither the name of the University of California, Berkeley nor
##      the names of its contributors may be used to endorse or promote
##      products derived from this software without specific prior
##      written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS''
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
## TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
## PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
## USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
## OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
## SUCH DAMAGE.

## Author: Jason Riedy <ejr@cs.berkeley.edu>
## Keywords: linear-algebra norm estimation
## Version: 0.2

function [cest, v] = condest (varargin)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif

  if ((nargin == 3 && is_function_handle (varargin{3}))
      || (nargin == 4 && is_function_handle (varargin{3})
          && isnumeric (varargin{4})))
    ## onenormest syntax, deprecated in 4.2
    [cest, v] = condest_legacy (varargin{:});
    return;
  elseif ((nargin >= 5) && is_function_handle (varargin{4}))
    ## onenormest syntax, deprecated in 4.2
    [cest, v] = condest_legacy (varargin{:});
    return;
  endif

  have_A = false;
  have_t = false;
  have_apply_normest1 = false;
  have_solve_normest1 = false;

  if (isnumeric (varargin{1}))
    A = varargin{1};
    if (! issquare (A))
      error ("condest: A must be square");
    endif
    n = rows (A);
    have_A = true;
    if (nargin > 1)
      if (is_function_handle (varargin{2}))
        solve = varargin{2};
        have_solve_normest1 = true;
        if (nargin > 2)
          t = varargin{3};
          have_t = true;
        endif
      else
        t = varargin{2};
        have_t = true;
        real_op = isreal (A);
      endif
    else
      real_op = isreal (A);
    endif
  else  # varargin{1} is a function handle
    if (nargin == 1)
      error("condest: must provide SOLVEFCN when using AFCN");
    endif
    apply = varargin{1};
    have_apply_normest1 = true;
    solve = varargin{2};
    have_solve_normest1 = true;
    n = apply ("dim", [], varargin{4:end});
    if (nargin > 2)
      t = varargin{3};
      have_t = true;
    endif
  endif

  if (! have_t)
    t = min (n, 5);
  endif

  if (! have_solve_normest1)
     ## prepare solve in normest1 form
    if (issparse (A))
      [L, U, P, Pc] = lu (A);
      solve = @(flag, x) solve_sparse (flag, x, n, real_op, L, U, P, Pc);
    else
      [L, U, P] = lu (A);
      solve = @(flag, x) solve_not_sparse (flag, x, n, real_op, L, U, P);
    endif
  endif

  if (have_A)
    Anorm = norm (A, 1);
  else
    Anorm = normest1 (apply, t, [], varargin{4:end});
  endif
  [Ainv_norm, v, w] = normest1 (solve, t, [], varargin{4:end});

  cest = Anorm * Ainv_norm;
  v = w / norm (w, 1);

endfunction

function value = solve_sparse (flag, x, n, real_op, L , U , P , Pc)
  switch (flag)
    case "dim"
      value = n;
    case "real"
      value = real_op;
    case "notransp"
      value = P' * (L' \ (U' \ (Pc * x)));
    case "transp"
      value = Pc' * (U \ (L \ (P * x)));
  endswitch
endfunction

function value = solve_not_sparse (flag, x, n, real_op, L, U, P)
  switch (flag)
    case "dim"
      value = n;
    case "real"
      value = real_op;
    case "notransp"
      value = P' * (L' \ (U' \ x));
    case "transp"
      value = U \ (L \ (P * x));
  endswitch
endfunction

## FIXME: remove after 4.4
function [cest, v] = condest_legacy (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "condest: this syntax is deprecated, call condest (A, SOLVEFUN, T, P1, P2, ...) instead.");
  endif

  default_t = 5;

  have_A = false;
  have_t = false;
  have_solve = false;
  if (isnumeric (varargin{1}))
    A = varargin{1};
    if (! issquare (A))
      error ("condest: matrix must be square");
    endif
    n = rows (A);
    have_A = true;

    if (nargin > 1)
      if (! is_function_handle (varargin{2}))
        t = varargin{2};
        have_t = true;
      elseif (nargin > 2)
        solve = varargin{2};
        solve_t = varargin{3};
        have_solve = true;
        if (nargin > 3)
          t = varargin{4};
          have_t = true;
        endif
      else
        error ("condest: must supply both SOLVE and SOLVE_T");
      endif
    endif
  elseif (nargin > 4)
    apply = varargin{1};
    apply_t = varargin{2};
    solve = varargin{3};
    solve_t = varargin{4};
    have_solve = true;
    n = varargin{5};
    if (! isscalar (n))
      error ("condest: dimension argument of implicit form must be scalar");
    endif
    if (nargin > 5)
      t = varargin{6};
      have_t = true;
    endif
  else
    error ("condest: implicit form of condest requires at least 5 arguments");
  endif

  if (! have_t)
    t = min (n, default_t);
  endif

  if (! have_solve)
    if (issparse (A))
      [L, U, P, Pc] = lu (A);
      solve = @(x) Pc' * (U \ (L \ (P * x)));
      solve_t = @(x) P' * (L' \ (U' \ (Pc * x)));
    else
      [L, U, P] = lu (A);
      solve = @(x) U \ (L \ (P*x));
      solve_t = @(x) P' * (L' \ (U' \ x));
    endif
  endif

  ## We already warned about this usage being deprecated.
  ## Don't warn again about onenormest.
  warning ("off", "Octave:deprecated-function", "local");

  if (have_A)
    Anorm = norm (A, 1);
  else
    Anorm = onenormest (apply, apply_t, n, t);
  endif

  [Ainv_norm, v, w] = onenormest (solve, solve_t, n, t);

  cest = Anorm * Ainv_norm;
  v = w / norm (w, 1);

endfunction


## Note: These test bounds are very loose.  There is enough randomization to
## trigger odd cases with hilb().

%!function value = apply_fun (flag, x, A, m)
%!  if (nargin == 3)
%!    m = 1;
%!  endif
%!  switch (flag)
%!    case "dim"
%!      value = length (A);
%!    case "real"
%!      value = isreal (A);
%!    case "notransp"
%!      value = x; for i = 1:m, value = A * value;, endfor
%!    case "transp"
%!      value = x; for i = 1:m, value = A' * value;, endfor
%!  endswitch
%!endfunction
%!function value = solve_fun (flag, x, A, m)
%!  if (nargin == 3)
%!    m = 1;
%!  endif
%!  switch (flag)
%!    case "dim"
%!      value = length (A);
%!    case "real"
%!      value = isreal (A);
%!    case "notransp"
%!      value = x; for i = 1:m, value = A \ value;, endfor;
%!    case "transp"
%!      value = x; for i = 1:m, value = A' \ value;, endfor;
%!  endswitch
%!endfunction

%!test
%! N = 6;
%! A = hilb (N);
%! cA = condest (A);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-8);

%!test # to be removed after 4.4
%! warning ("off", "Octave:deprecated-function", "local");
%! N = 6;
%! A = hilb (N);
%! solve = @(x) A\x; solve_t = @(x) A'\x;
%! cA = condest (A, solve, solve_t);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-8);

%!test # to be removed after 4.4
%! warning ("off", "Octave:deprecated-function", "local");
%! N = 6;
%! A = hilb (N);
%! apply = @(x) A*x; apply_t = @(x) A'*x;
%! solve = @(x) A\x; solve_t = @(x) A'\x;
%! cA = condest (apply, apply_t, solve, solve_t, N);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test # to be removed after 4.4
%! warning ("off", "Octave:deprecated-function", "local");
%! N = 6;
%! A = hilb (N);
%! apply = @(x) A*x; apply_t = @(x) A'*x;
%! solve = @(x) A\x; solve_t = @(x) A'\x;
%! cA = condest (apply, apply_t, solve, solve_t, N, 2);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test
%! warning ("off", "Octave:nearly-singular-matrix", "local");
%! N = 12;
%! A = hilb (N);
%! [~, v] = condest (A);
%! x = A*v;
%! assert (norm (x, inf), 0, eps);

%!test
%! N = 6;
%! A = hilb (N);
%! solve = @(flag, x) solve_fun (flag, x, A);
%! cA = condest (A, solve);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test
%! N = 6;
%! A = hilb (N);
%! apply = @(flag, x) apply_fun (flag, x, A);
%! solve = @(flag, x) solve_fun (flag, x, A);
%! cA = condest (apply, solve);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test # parameters for apply and solve functions
%! N = 6;
%! A = hilb (N);
%! m = 2;
%! cA = condest (@apply_fun, @solve_fun, [], A, m);
%! cA_test = norm (inv (A^2), 1) * norm (A^2, 1);
%! assert (cA, cA_test, -2^-6);

## Test input validation
%!error condest ()
%!error condest (1,2,3,4,5,6,7)
%!error <A must be square> condest ([1 2])
%!error <must provide SOLVEFCN when using AFCN> condest (@sin)

