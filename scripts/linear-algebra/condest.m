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
## @deftypefn  {} {@var{cest} =} condest (@var{A})
## @deftypefnx {} {@var{cest} =} condest (@var{A}, @var{t})
## @deftypefnx {} {@var{cest} =} condest (@var{A}, @var{Ainvfcn})
## @deftypefnx {} {@var{cest} =} condest (@var{A}, @var{Ainvfcn}, @var{t})
## @deftypefnx {} {@var{cest} =} condest (@var{A}, @var{Ainvfcn}, @var{t}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {@var{cest} =} condest (@var{Afcn}, @var{Ainvfcn})
## @deftypefnx {} {@var{cest} =} condest (@var{Afcn}, @var{Ainvfcn}, @var{t})
## @deftypefnx {} {@var{cest} =} condest (@var{Afcn}, @var{Ainvfcn}, @var{t}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {[@var{cest}, @var{v}] =} condest (@dots{})
##
## Estimate the 1-norm condition number of a square matrix @var{A} using
## @var{t} test vectors and a randomized 1-norm estimator.
##
## The optional input @var{t} specifies the number of test vectors (default 5).
##
## The input may be a matrix @var{A} (the algorithm is particularly
## appropriate for large, sparse matrices).  Alternatively, the behavior of
## the matrix can be defined implicitly by functions.  When using an implicit
## definition, @code{condest} requires the following functions:
##
## @itemize @minus
## @item @code{@var{Afcn} (@var{flag}, @var{x})} which must return
##
## @itemize @bullet
## @item
## the dimension @var{n} of @var{A}, if @var{flag} is @qcode{"dim"}
##
## @item
## true if @var{A} is a real operator, if @var{flag} is @qcode{"real"}
##
## @item
## the result @code{@var{A} * @var{x}}, if @var{flag} is "notransp"
##
## @item
## the result @code{@var{A}' * @var{x}}, if @var{flag} is "transp"
## @end itemize
##
## @item @code{@var{Ainvfcn} (@var{flag}, @var{x})} which must return
##
## @itemize @bullet
## @item
## the dimension @var{n} of @code{inv (@var{A})}, if @var{flag} is
## @qcode{"dim"}
##
## @item
## true if @code{inv (@var{A})} is a real operator, if @var{flag} is
## @qcode{"real"}
##
## @item
## the result @code{inv (@var{A}) * @var{x}}, if @var{flag} is "notransp"
##
## @item
## the result @code{inv (@var{A})' * @var{x}}, if @var{flag} is "transp"
## @end itemize
## @end itemize
##
## Any parameters @var{p1}, @var{p2}, @dots{} are additional arguments of
## @code{@var{Afcn} (@var{flag}, @var{x}, @var{p1}, @var{p2}, @dots{})}
## and @code{@var{Ainvfcn} (@var{flag}, @var{x}, @var{p1}, @var{p2}, @dots{})}.
##
## The principal output is the 1-norm condition number estimate @var{cest}.
##
## The optional second output @var{v} is an approximate null vector; it
## satisfies the equation @code{norm (@var{A}*@var{v}, 1) ==
## norm (@var{A}, 1) * norm (@var{v}, 1) / @var{cest}}.
##
## Algorithm Note: @code{condest} uses a randomized algorithm to approximate
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
## Pseudospectra}.  SIMAX vol 21, no 4, pp 1185--1201.
## @url{https://dx.doi.org/10.1137/S0895479899356080}
##
## @item
## @nospell{N.J. Higham and F. Tisseur}, @cite{A Block Algorithm
## for Matrix 1-Norm Estimation, with an Application to 1-Norm
## Pseudospectra}.  @url{https://citeseer.ist.psu.edu/223007.html}
## @end itemize
##
## @seealso{cond, rcond, norm, normest1, normest}
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

function [cest, v] = condest (varargin)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif

  have_A = false;
  have_t = false;
  have_Afcn = false;
  have_Ainvfcn = false;

  if (isnumeric (varargin{1}))
    A = varargin{1};
    if (! issquare (A))
      error ("condest: A must be square");
    endif
    have_A = true;
    n = rows (A);
    if (nargin > 1)
      if (is_function_handle (varargin{2}))
        Ainvfcn = varargin{2};
        have_Ainvfcn = true;
        if (nargin > 2)
          t = varargin{3};
          have_t = true;
        endif
      else
        t = varargin{2};
        have_t = true;
      endif
    endif
  elseif (is_function_handle (varargin{1}))
    if (nargin == 1)
      error ("condest: must provide AINVFCN when using AFCN");
    endif
    Afcn = varargin{1};
    have_Afcn = true;
    if (! is_function_handle (varargin{2}))
      error ("condest: AINVFCN must be a function handle");
    endif
    Ainvfcn = varargin{2};
    have_Ainvfcn = true;
    n = Afcn ("dim", [], varargin{4:end});
    if (nargin > 2)
      t = varargin{3};
      have_t = true;
    endif
  else
    error ("condest: first argument must be a square matrix or function handle");
  endif

  if (! have_t)
    t = min (n, 5);
  endif

  ## Disable warnings which may be emitted during calculation process.
  warning ("off", "Octave:nearly-singular-matrix", "local");

  if (! have_Ainvfcn)
    ## Prepare Ainvfcn in normest1 form
    if (issparse (A))
      [L, U, P, Q] = lu (A);
      Ainvfcn = @inv_sparse_fcn;
    else
      [L, U, P] = lu (A);
      Q = [];
      Ainvfcn = @inv_full_fcn;
    endif

    ## Check for singular matrices before continuing (bug #46737)
    if (any (diag (U) == 0))
      cest = Inf;
      v = [];
      return;
    endif

    ## Initialize solver
    Ainvfcn ("init", A, L, U, P, Q);
    clear L U P Q;
  endif

  if (have_A)
    Anorm = norm (A, 1);
  else
    Anorm = normest1 (Afcn, t, [], varargin{4:end});
  endif
  [Ainv_norm, v, w] = normest1 (Ainvfcn, t, [], varargin{4:end});

  cest = Anorm * Ainv_norm;
  if (isargout (2))
    v = w / norm (w, 1);
  endif

  if (! have_Ainvfcn)
    Ainvfcn ("clear");  # clear persistent memory in subfunction
  endif

endfunction

function retval = inv_sparse_fcn (flag, x, varargin)

  ## FIXME: Sparse algorithm is less accurate than full matrix version.
  ##        See BIST test for asymmetric matrix where relative tolerance
  ##        of 1e-12 is used for sparse, but 4e-16 for full matrix.
  ##        BUT, does it really matter for an "estimate"?
  persistent Ainv Ainvt n isreal_op;

  switch (flag)
    case "dim"
      retval = n;
    case "real"
      retval = isreal_op;
    case "notransp"
      retval = Ainv * x;
    case "transp"
      retval = Ainvt * x;
    case "init"
      n = rows (x);
      isreal_op = isreal (x);
      [L, U, P, Q] = deal (varargin{1:4});
      Ainv = Q * (U \ (L \ P));
      Ainvt = P' * (L' \ (U' \ Q'));
    case "clear"  # called to free memory at end of condest function
      clear Ainv Ainvt n isreal_op;
  endswitch

endfunction

function retval = inv_full_fcn (flag, x, varargin)
  persistent Ainv Ainvt n isreal_op;

  switch (flag)
    case "dim"
      retval = n;
    case "real"
      retval = isreal_op;
    case "notransp"
      retval = Ainv * x;
    case "transp"
      retval = Ainvt \ x;
    case "init"
      n = rows (x);
      isreal_op = isreal (x);
      [L, U, P] = deal (varargin{1:3});
      Ainv = U \ (L \ P);
      Ainvt = P' * (L' \ U');
    case "clear"  # called to free memory at end of condest function
      clear Ainv Ainvt n isreal_op;
  endswitch

endfunction


## Note: These test bounds are very loose.  There is enough randomization to
## trigger odd cases with hilb().

%!function retval = __Afcn__ (flag, x, A, m)
%!  if (nargin == 3)
%!    m = 1;
%!  endif
%!  switch (flag)
%!    case "dim"
%!      retval = length (A);
%!    case "real"
%!      retval = isreal (A);
%!    case "notransp"
%!      retval = x; for i = 1:m, retval = A * retval;, endfor
%!    case "transp"
%!      retval = x; for i = 1:m, retval = A' * retval;, endfor
%!  endswitch
%!endfunction
%!function retval = __Ainvfcn__ (flag, x, A, m)
%!  if (nargin == 3)
%!    m = 1;
%!  endif
%!  switch (flag)
%!    case "dim"
%!      retval = length (A);
%!    case "real"
%!      retval = isreal (A);
%!    case "notransp"
%!      retval = x; for i = 1:m, retval = A \ retval;, endfor
%!    case "transp"
%!      retval = x; for i = 1:m, retval = A' \ retval;, endfor
%!  endswitch
%!endfunction

%!test
%! N = 6;
%! A = hilb (N);
%! cA = condest (A);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-8);

%!test
%! N = 12;
%! A = hilb (N);
%! [~, v] = condest (A);
%! x = A*v;
%! assert (norm (x, inf), 0, eps);

%!test
%! N = 6;
%! A = hilb (N);
%! Ainvfcn = @(flag, x) __Ainvfcn__ (flag, x, A);
%! cA = condest (A, Ainvfcn);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test
%! N = 6;
%! A = hilb (N);
%! Afcn = @(flag, x) __Afcn__ (flag, x, A);
%! Ainvfcn = @(flag, x) __Ainvfcn__ (flag, x, A);
%! cA = condest (Afcn, Ainvfcn);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test # parameters for apply and Ainvfcn functions
%! N = 6;
%! A = hilb (N);
%! m = 2;
%! cA = condest (@__Afcn__, @__Ainvfcn__, [], A, m);
%! cA_test = norm (inv (A^2), 1) * norm (A^2, 1);
%! assert (cA, cA_test, -2^-6);

## Test singular matrices
%!test <*46737>
%! A = [ 0         0         0
%!       0   3.33333 0.0833333
%!       0 0.0833333   1.66667];
%! [cest, v] = condest (A);
%! assert (cest, Inf);
%! assert (v, []);

## Test asymmetric matrices
%!test <*57968>
%! A = reshape (sqrt (0:15), 4, 4);
%! cexp = norm (A, 1) * norm (inv (A), 1);
%! cest = condest (A);
%! assert (cest, cexp, -2*eps);

%!testif HAVE_UMFPACK <*57968>
%! As = sparse (reshape (sqrt (0:15), 4, 4));
%! cexp = norm (As, 1) * norm (inv (As), 1);
%! cest = condest (As);
%! assert (cest, cexp, -1e-12);

## Test input validation
%!error <Invalid call> condest ()
%!error <Invalid call> condest (1,2,3,4,5,6,7)
%!error <A must be square> condest ([1, 2])
%!error <must provide AINVFCN when using AFCN> condest (@sin)
%!error <AINVFCN must be a function handle> condest (@sin, 1)
%!error <argument must be a square matrix or function handle> condest ({1})
