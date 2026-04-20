########################################################################
##
## Copyright (C) 2013-2026 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} linsolve (@var{A}, @var{b})
## @deftypefnx {} {@var{x} =} linsolve (@var{A}, @var{b}, @var{opts})
## @deftypefnx {} {[@var{x}, @var{R}] =} linsolve (@dots{})
## Solve the linear system @code{A*x = b}.
##
## With no options, this function is equivalent to the left division operator
## @w{(@code{x = A \ b})}@ or the matrix-left-divide function
## @w{(@code{x = mldivide (A, b)})}.
##
## Octave ordinarily examines the properties of the matrix @var{A} and chooses
## a solver that best matches the matrix.  By passing a structure @var{opts}
## to @code{linsolve} you can inform Octave directly about the matrix @var{A}.
## In this case Octave will skip the matrix examination and proceed directly
## to solving the linear system producing a small speedup.
##
## @strong{Warning:} If the matrix @var{A} does not have the properties listed
## in the @var{opts} structure then the result will not be accurate AND no
## warning will be given.  When in doubt, let Octave examine the matrix and
## choose the appropriate solver as this step takes little time and the result
## is cached so that it is done just once per linear system.  Benchmarking
## with @w{Octave 12} and square matrices of 10,000 rows shows just 300
## milliseconds for this step.
##
## Possible @var{opts} fields (set value to @code{true}/@code{false}):
##
## @table @code
## @item LT
##   @var{A} is lower triangular
##
## @item UT
##   @var{A} is upper triangular
##
## @item UHESS
##   @var{A} is upper Hessenberg (currently makes no difference)
##
## @item SYM
##   @var{A} is symmetric or complex Hermitian (currently makes no difference)
##
## @item POSDEF
##   @var{A} is positive definite
##
## @item RECT
##   @var{A} is general rectangular (currently makes no difference)
##
## @item TRANSA
##   Solve @code{A'*x = b} if true rather than @code{A*x = b}
## @end table
##
## The optional second output @var{R} is the reciprocal condition number of
## @var{A} for square matrices.  For rectangular matrices @var{R} is the rank
## of @var{A} or @code{NaN} if the size of @var{A} is large.  When a second
## output is requested Octave will not emit a warning about an ill-conditioned
## matrix.
## @seealso{mldivide, matrix_type, rcond, rank}
## @end deftypefn

function [x, R] = linsolve (A, b, opts)

  if (nargin < 2)
    print_usage ();
  endif

  if (! (isnumeric (A) && isnumeric (b)))
    error ("linsolve: A and B must be numeric");
  endif

  trans_A = false;

  ## Process any opts
  if (nargin > 2)
    if (! isstruct (opts))
      error ("linsolve: OPTS must be a structure");
    endif
    if (isfield (opts, "TRANSA") && opts.TRANSA)
      trans_A = true;
    endif
    if (isfield (opts, "POSDEF") && opts.POSDEF)
      A = matrix_type (A, "positive definite");
    endif
    if (isfield (opts, "LT") && opts.LT)
      A = matrix_type (A, "lower");
    elseif (isfield (opts, "UT") && opts.UT)
      A = matrix_type (A, "upper");
    endif
  endif

  if (nargout > 1)
    warning ("off", "Octave:singular-matrix", "local");
  endif

  ## This way is faster as the transpose is not calculated in Octave,
  ## but forwarded as a flag option to BLAS.
  if (trans_A)
    x = A' \ b;
  else
    x = A \ b;
  endif

  if (nargout > 1)
    if (issquare (A))
      R = rcond (A);
    else
      if (numel (A) > 1e6)
        warning ('Octave:linsolve:rank-not-calculated',
                 'linsolve: matrix A too large.  Rank of A not calculated');
        R = NaN;
      else
        R = rank (A);
      endif
    endif
  endif

endfunction


%!test
%! n = 10;
%! A = rand (n);
%! x = rand (n, 1);
%! b = A * x;
%! assert (linsolve (A, b), A \ b);
%! assert (linsolve (A, b, struct ()), A \ b);

%!test
%! n = 10;
%! A = triu (gallery ("condex", n));
%! x = rand (n, 1);
%! b = A' * x;
%! opts.UT = true;
%! opts.TRANSA = true;
%! assert (linsolve (A, b, opts), A' \ b);

## Test second output for square and rectangular inputs.
%!test <*68238>
%! A = hilb (4);
%! b = [1:4]';
%! [x, R] = linsolve (A, b);
%! assert (x, [-64; 900; -2520; 1820], -3e-13);
%! assert (R, rcond (A));

%!test <*68238>
%! A = hilb (5);
%! A(5,:) = [];      # Make rectangular 4x5 matrix
%! A(3,:) = A(1,:);  # Reduce rank to 3
%! b = [1:4]';
%! [x, R] = linsolve (A, b);
%! assert (R, 3);

%!test <*68238>
%! A = eye (3);
%! b = [2; 3; 4];
%! [x, R] = linsolve (A, b);
%! assert (x, [2; 3; 4]);
%! assert (R, 1);

%!test <*68238>
%! A = [eye(3); eye(3)];
%! b = [2; 3; 4];
%! [x, R] = linsolve (A, [b; b]);
%! assert (x, b, 10*eps);
%! assert (R, 3);

%!test <*68238>
%! A = [eye(3), eye(3)];
%! b = [2; 3; 4];
%! [x, R] = linsolve (A, b);
%! assert (A*x, b, 10*eps);
%! assert (R, 3);

%!test <*68238>
%! A = [1 2 3 4; 2 3 5 7; 3 5 6 7];
%! b = [2; 3; 4];
%! [x, R] = linsolve (A, b);
%! assert (A*x, b, 10*eps);
%! assert (R, 3);

## Test input validation
%!error <Invalid call> linsolve ()
%!error <Invalid call> linsolve (1)
%!error <A and B must be numeric> linsolve ({1},2)
%!error <A and B must be numeric> linsolve (1,{2})
%!error <OPTS must be a structure> linsolve (1,2,3)
%!warning <matrix singular to machine precision>
%! A = ones (3, 3);
%! b = ones (3, 1);
%! x = linsolve (A, b);
