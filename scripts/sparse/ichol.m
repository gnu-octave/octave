## Copyright (C) 2013 Kai T. Ohlhus <k.ohlhus@gmail.com>
## Copyright (C) 2014 Eduardo Ramos Fernández <eduradical951@gmail.com>
## 
## This file is part of Octave.
## 
## Octave is free software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
## 
## Octave is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
## 
## You should have received a copy of the GNU General Public License along with
## Octave; see the file COPYING. If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} ichol (@var{A}, @var{opts})
## @deftypefnx {Function File} {@var{L} =} ichol (@var{A}, @var{opts})
##
## @code{@var{L} = ichol (@var{A})} performs the incomplete Cholesky
## factorization of A with zero-fill.
##
## @code{@var{L} = ichol (@var{A}, @var{opts})} performs the incomplete Cholesky
## factorization of A with options specified by opts.
##
## By default, ichol references the lower triangle of A and produces lower
## triangular factors.
##
## The factor given by this routine may be useful as a preconditioner for a
## system of linear equations being solved by iterative methods such as
## PCG (Preconditioned conjugate gradient).
##
## ichol works only for sparse square matrices.
##
## The fields of @var{opts} must be named exactly as shown below. You can
## include any number of these fields in the structure and define them in any
## order. Any additional fields are ignored. Names and specifiers are case
## sensitive.
##
## @table @asis
## @item type
## Type of factorization.
## String indicating which flavor of incomplete Cholesky to perform. Valid
## values of this field are @samp{nofill} and @samp{ict}. The
## @samp{nofill} variant performs incomplete Cholesky with zero-fill [IC(0)].
## The @samp{ict} variant performs incomplete Cholesky with threshold dropping
## [ICT]. The default value is @samp{nofill}.
##
## @item droptol
## Drop tolerance when type is @samp{ict}.
## Nonnegative scalar used as a drop tolerance when performing ICT. Elements
## which are smaller in magnitude than a local drop tolerance are dropped from
## the resulting factor except for the diagonal element which is never dropped.
## The local drop tolerance at step j of the factorization is
## @code{norm (@var{A}(j:end, j), 1) * droptol}. @samp{droptol} is ignored if
## @samp{type} is @samp{nofill}. The default value is 0.
##
## @item michol
## Indicates whether to perform modified incomplete Cholesky.
## Indicates whether or not modified incomplete Cholesky [MIC] is performed.
## The field may be @samp{on} or @samp{off}. When performing MIC, the diagonal
## is compensated for dropped elements to enforce the relationship
## @code{@var{A} * @var{e} = @var{L} * @var{L}' * @var{e}} where
## @code{@var{e} = ones (size (@var{A}, 2), 1))}. The default value is
## @samp{off}.
##
## @item diagcomp
## Perform compensated incomplete Cholesky with the specified coefficient.
## Real nonnegative scalar used as a global diagonal shift @code{@var{alpha}}
## in forming the incomplete Cholesky factor. That is, instead of performing
## incomplete Cholesky on @code{@var{A}}, the factorization of
## @code{@var{A} + @var{alpha} * diag (diag (@var{A}))} is formed. The default
## value is 0.
##
## @item shape
## Determines which triangle is referenced and returned.
## Valid values are @samp{upper} and @samp{lower}. If @samp{upper} is specified,
## only the upper triangle of @code{@var{A}} is referenced and @code{@var{R}}
## is constructed such that @code{@var{A}} is approximated by
## @code{@var{R}' * @var{R}}. If @samp{lower} is specified, only the lower
## triangle of @code{@var{A}} is referenced and @code{@var{L}} is constructed
## such that @code{@var{A}} is approximated by @code{@var{L} * @var{L}'}. The
## default value is @samp{lower}.
## @end table
##
## EXAMPLES
##
## The following problem demonstrates how to factorize a sample symmetric
## positive definite matrix with the full Cholesky decomposition and with the
## incomplete one.
##
## @example
## A = [ 0.37, -0.05,  -0.05,  -0.07;
##      -0.05,  0.116,  0.0,   -0.05;
##      -0.05,  0.0,    0.116, -0.05;
##      -0.07, -0.05,  -0.05,   0.202];
## A = sparse(A);
## nnz(tril (A))
## ans =  9
## L = chol(A, "lower");
## nnz (L)
## ans =  10
## norm (A - L * L', 'fro') / norm (A, 'fro')
## ans =  1.1993e-16
## opts.type = 'nofill';
## L = ichol(A,opts);
## nnz (L)
## ans =  9
## norm (A - L * L', 'fro') / norm (A, 'fro')
## ans =  0.019736
## @end example
##
## Another example for decomposition is finite difference matrix to solve a
## boundary value problem on the unit square.
##
## @example
## nx = 400; ny = 200;
## hx = 1 / (nx + 1); hy = 1 / (ny + 1);
## Dxx = spdiags ([ones(nx, 1), -2 * ones(nx, 1), ones(nx, 1)], [-1 0 1 ], nx, nx) / (hx ^ 2);
## Dyy = spdiags ([ones(ny, 1), -2 * ones(ny, 1), ones(ny, 1)], [-1 0 1 ], ny, ny) / (hy ^ 2);
## A = -kron (Dxx, speye (ny)) - kron (speye (nx), Dyy);
## nnz (tril (A))
## ans =  239400
## opts.type = 'nofill';
## L = ichol (A, opts);
## nnz (tril (A))
## ans =  239400
## norm (A - L * L', 'fro') / norm (A, 'fro')
## ans =  0.062327
## @end example
##
## References for the implemented algorithms:
##
## [1] Saad, Yousef. "Preconditioning Techniques." Iterative Methods for Sparse Linear
## Systems. PWS Publishing Company, 1996.
##
## [2] Jones, Mark T. and Plassmann, Paul E.: An Improved Incomplete Cholesky
## Factorization (1992).
## @end deftypefn

function [L] = ichol (A, opts)

  if ((nargin > 2) || (nargin < 1) || (nargout > 1))
    print_usage ();
  endif

  % Check input matrix
  if (isempty (A) || ~issparse(A) || ~issquare (A))
    error ("ichol: Input A must be a non-empty sparse square matrix");
  endif

  % Check input structure, otherwise set default values
  if (nargin == 2)
    if (~isstruct (opts))
      error ("ichol: Input \"opts\" must be a valid structure.");
    endif
  else
    opts = struct ();
  endif

  if (~isfield (opts, "type"))
    opts.type = "nofill"; % set default
  else
    type = tolower (getfield (opts, "type"));
    if ((strcmp (type, "nofill") == 0)
        && (strcmp (type, "ict") == 0))
      error ("ichol: Invalid field \"type\" in input structure.");
    else
      opts.type = type;
    endif
  endif

  if (~isfield (opts, "droptol"))
    opts.droptol = 0; % set default
  else
    if (~isscalar (opts.droptol) || (opts.droptol < 0))
      error ("ichol: Invalid field \"droptol\" in input structure.");
    endif
  endif

  michol = "";
  if (~isfield (opts, "michol"))
    opts.michol = "off"; % set default
  else
    michol = tolower (getfield (opts, "michol"));
    if ((strcmp (michol, "off") == 0) 
        && (strcmp (michol, "on") == 0))
      error ("ichol: Invalid field \"michol\" in input structure.");
    else
      opts.michol = michol;
    endif
  endif

  if (~isfield (opts, "diagcomp"))
    opts.diagcomp = 0; % set default
  else
    if (~isscalar (opts.diagcomp) || (opts.diagcomp < 0))
      error ("ichol: Invalid field \"diagcomp\" in input structure.");
    endif
  endif

  if (~isfield (opts, "shape"))
    opts.shape = "lower"; % set default
  else
    shape = tolower (getfield (opts, "shape"));
    if ((strcmp (shape, "lower") == 0) 
        && (strcmp (shape, "upper") == 0))
      error ("ichol: Invalid field \"shape\" in input structure.");
    else
      opts.shape = shape;
    endif
  endif

  % Prepare input for specialized ICHOL
  A_in = [];
  if (opts.diagcomp > 0)
    A += opts.diagcomp * diag (diag (A));
  endif
  if (strcmp (opts.shape, "upper") == 1)
    disp("entro");
    A_in = triu (A);
    A_in = A_in';

  else
    A_in = tril (A);
  endif

  % Delegate to specialized ICHOL
  switch (opts.type)
    case "nofill"
      L  = ichol0 (A_in,  opts.michol);
    case "ict"
      L = icholt (A_in, opts.droptol, opts.michol);
    otherwise
      printf ("The input structure is invalid.\n");
  endswitch

  if (strcmp (opts.shape, "upper") == 1)
    L = L';
  endif
  

endfunction

%!shared A1, A2
%! A1 = [ 0.37, -0.05,  -0.05,  -0.07;
%!      -0.05,  0.116,  0.0,   -0.05;
%!      -0.05,  0.0,    0.116, -0.05;
%!      -0.07, -0.05,  -0.05,   0.202];
%! A1 = sparse(A1);
%! nx = 400; ny = 200;
%! hx = 1 / (nx + 1); hy = 1 / (ny + 1);
%! Dxx = spdiags ([ones(nx, 1), -2 * ones(nx, 1), ones(nx, 1)], [-1 0 1 ], nx, nx) / (hx ^ 2);
%! Dyy = spdiags ([ones(ny, 1), -2 * ones(ny, 1), ones(ny, 1)], [-1 0 1 ], ny, ny) / (hy ^ 2);
%! A2 = -kron (Dxx, speye (ny)) - kron (speye (nx), Dyy);
%!
%!test
%!error ichol ([]);
%!error ichol (0);
%!error ichol (-0);
%!error ichol (1);
%!error ichol (-1);
%!error ichol (i);
%!error ichol (-i);
%!error ichol (1 + 1i);
%!error ichol (1 - 1i);
%!error ichol (sparse (0));
%!error ichol (sparse (-0));
%!error ichol (sparse (-1));
%!error ichol (sparse (-1));
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! assert (nnz (tril (A1)), nnz (ichol (A1, opts)));
%! assert (nnz (tril (A2)), nnz (ichol (A2, opts)));
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', 'fro') / norm (A1, 'fro'), 0.01, 0.01);
%! L = ichol (A2, opts);
%! assert (norm (A2 - L * L', 'fro') / norm (A2, 'fro'), 0.06, 0.01);
%!
%%!test
%%! opts.type = "nofill";
%%! opts.michol = "off";
%%! opts.shape = "upper";
%%! U = ichol (A1, opts);
%%! assert (norm (A1 - U' * U, 'fro') / norm (A1, 'fro'), 0.01, 0.01);
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! opts.shape = "lower";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', 'fro') / norm (A1, 'fro'), 0.01, 0.01);
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "on";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', 'fro') / norm (A1, 'fro'), 0.02, 0.01);
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "on";
%! opts.diagcomp = 3e-3;
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', 'fro') / norm (A1, 'fro'), 0.02, 0.01);
%!
%!test
%! opts.type = "ict";
%! opts.michol = "off";
%! opts.droptol = 1e-4;
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', 'fro') / norm (A1, 'fro'), eps, eps);
%!
%!test
%! opts.type = "ict";
%! opts.michol = "off";
%! opts.droptol = 1e-4;
%! L = ichol (A2, opts);
%! assert (norm (A2 - L * L', 'fro') / norm (A2, 'fro'), 5e-4, 5e-4);
