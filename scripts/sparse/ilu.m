## Copyright (C) 2013 Kai T. Ohlhus <k.ohlhus@gmail.com>
## Copyright (C) 2014 Eduardo Ramos Fernández <eduradical951@gmail.com>
##
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
## @deftypefn  {Function File} ilu (@var{A}, @var{setup})
## @deftypefnx {Function File} {[@var{L}, @var{U}] =} ilu (@var{A}, @var{setup})
## @deftypefnx {Function File} {[@var{L}, @var{U}, @var{P}] =} ilu (@var{A}, @var{setup})
## ilu produces a unit lower triangular matrix, an upper triangular matrix, and
## a permutation matrix.
##
## These incomplete factorizations may be useful as preconditioners for a system
## of linear equations being solved by iterative methods such as BICG
## (BiConjugate Gradients), GMRES (Generalized Minimum Residual Method).
##
## @code{ilu (@var{A}, @var{setup})} computes the incomplete LU factorization
## of @var{A}. @var{setup} is an input structure with up to five setup options.
## The fields must be named exactly as shown below. You can include any number
## of these fields in the structure and define them in any order. Any
## additional fields are ignored.
##
## @table @asis
## @item type
## Type of factorization. Values for type include:
##
## @table @asis
## @item @samp{nofill}
## Performs ILU factorization with 0 level of fill in, known as ILU(0). With
## type set to @samp{nofill}, only the milu setup option is used; all other
## fields are ignored.
## @item @samp{crout}
## Performs the Crout version of ILU factorization, known as ILUC. With type
## set to @samp{crout}, only the droptol and milu setup options are used; all
## other fields are ignored.
## @item @samp{ilutp}
## (default) Performs ILU factorization with threshold and pivoting.
## @end table
##
## If type is not specified, the ILU factorization with pivoting ILUTP is
## performed. Pivoting is never performed with type set to @samp{nofill} or
## @samp{crout}.
##
## @item droptol
## Drop tolerance of the incomplete LU factorization. droptol is a non-negative
## scalar. The default value is 0, which produces the complete LU factorization.
##
## The nonzero entries of U satisfy
##
## @code{abs (@var{U}(i,j)) >= droptol * norm ((@var{A}:,j))}
##
## with the exception of the diagonal entries, which are retained regardless of
## satisfying the criterion. The entries of @var{L} are tested against the
## local drop tolerance before being scaled by the pivot, so for nonzeros in
## @var{L}
##
## @code{abs(@var{L}(i,j)) >= droptol * norm(@var{A}(:,j))/@var{U}(j,j)}.
##
## @item milu
## Modified incomplete LU factorization. Values for milu
## include:
## @table @asis
## @item @samp{row}
## Produces the row-sum modified incomplete LU factorization. Entries from the
## newly-formed column of the factors are subtracted from the diagonal of the
## upper triangular factor, @var{U}, preserving column sums. That is,
## @code{@var{A} * e = @var{L} * @var{U} * e}, where e is the vector of ones.
## @item @samp{col}
## Produces the column-sum modified incomplete LU factorization. Entries from
## the newly-formed column of the factors are subtracted from the diagonal of
## the upper triangular factor, @var{U}, preserving column sums. That is,
## @code{e'*@var{A} = e'*@var{L}*@var{U}}.
## @item @samp{off}
## (default) No modified incomplete LU factorization is produced.
## @end table
##
## @item udiag
## If udiag is 1, any zeros on the diagonal of the upper
## triangular factor are replaced by the local drop tolerance. The default is 0.
##
## @item thresh
## Pivot threshold between 0 (forces diagonal pivoting) and 1,
## the default, which always chooses the maximum magnitude entry in the column
## to be the pivot.
## @end table
##
## @code{ilu (@var{A},@var{setup})} returns
## @code{@var{L} + @var{U} - speye (size (@var{A}))}, where @var{L} is a unit
## lower triangular matrix and @var{U} is an upper triangular matrix.
##
## @code{[@var{L}, @var{U}] = ilu (@var{A},@var{setup})} returns a unit lower
## triangular matrix in @var{L} and an upper triangular matrix in @var{U}. When
## SETUP.type = 'ilutp', the role of @var{P} is determined by the value of
## SETUP.milu. For SETUP.type == 'ilutp', one of the factors is permuted
## based on the value of SETUP.milu. When SETUP.milu == 'row', U is a column 
## permuted upper triangular factor. Otherwise, L is a row-permuted unit lower 
## triangular factor.
##
## @code{[@var{L}, @var{U}, @var{P}] = ilu (@var{A},@var{setup})} returns a
## unit lower triangular matrix in @var{L}, an upper triangular matrix in
## @var{U}, and a permutation matrix in @var{P}. When SETUP.milu ~= 'row', @var{P} 
## is returned such that @var{L} and @var{U} are incomplete factors of @var{P}*@var{A}.
## When SETUP.milu == 'row', @var{P} is returned such that and @var{U} are 
## incomplete factors of A*P.
##
## @strong{NOTE}: ilu works on sparse square matrices only.
##
## EXAMPLES
##
## @example
## A = gallery('neumann', 1600) + speye(1600);
## setup.type = 'nofill';
## nnz(A)
## ans = 7840
##
## nnz(lu(A))
## ans = 126478
##
## nnz(ilu(A,setup))
## ans = 7840
## @end example
##
## This shows that @var{A} has 7840 nonzeros, the complete LU factorization has
## 126478 nonzeros, and the incomplete LU factorization, with 0 level of
## fill-in, has 7840 nonzeros, the same amount as @var{A}. Taken from:
## http://www.mathworks.com/help/matlab/ref/ilu.html
##
## @example
## A = gallery ('wathen', 10, 10);
## b = sum (A,2); 
## tol = 1e-8; 
## maxit = 50;
## opts.type = 'crout';
## opts.droptol = 1e-4;
## [L, U] = ilu (A, opts);
## x = bicg (A, b, tol, maxit, L, U);
## norm(A * x - b, inf)
## @end example
##
## This example uses ILU as preconditioner for a random FEM-Matrix, which has a
## bad condition. Without @var{L} and @var{U} BICG would not converge.
##
## @end deftypefn

function [L, U, P] = ilu (A, setup)

  if ((nargin > 2) || (nargin < 1) || (nargout > 3))
    print_usage ();
  endif

  % Check input matrix
  if (~issparse(A) || ~issquare (A))
    error ("ilu: Input A must be a sparse square matrix.");
  endif

  % Check input structure, otherwise set default values
  if (nargin == 2)
    if (~isstruct (setup))
      error ("ilu: Input 'setup' must be a valid structure.");
    endif
  else
    setup = struct ();
  endif

  if (~isfield (setup, "type"))
    setup.type = "nofill"; % set default
  else
    type = tolower (getfield (setup, "type"));
    if ((strcmp (type, "nofill") == 0)
        && (strcmp (type, "crout") == 0)
        && (strcmp (type, "ilutp") == 0))
      error ("ilu: Invalid field \"type\" in input structure.");
    else
      setup.type = type;
    endif
  endif

  if (~isfield (setup, "droptol"))
    setup.droptol = 0; % set default
  else
    if (~isscalar (setup.droptol) || (setup.droptol < 0))
      error ("ilu: Invalid field \"droptol\" in input structure.");
    endif
  endif

  if (~isfield (setup, "milu"))
    setup.milu = "off"; % set default
  else
    milu = tolower (getfield (setup, "milu"));
    if ((strcmp (milu, "off") == 0) 
        && (strcmp (milu, "col") == 0)
        && (strcmp (milu, "row") == 0))
      error ("ilu: Invalid field \"milu\" in input structure.");
    else
      setup.milu = milu;
    endif
  endif

  if (~isfield (setup, "udiag"))
    setup.udiag = 0; % set default
  else
    if (~isscalar (setup.udiag) || ((setup.udiag ~= 0) && (setup.udiag ~= 1)))
      error ("ilu: Invalid field \"udiag\" in input structure.");
    endif
  endif

  if (~isfield (setup, "thresh"))
    setup.thresh = 1; % set default
  else
    if (~isscalar (setup.thresh) || (setup.thresh < 0) || (setup.thresh > 1))
      error ("ilu: Invalid field \"thresh\" in input structure.");
    endif
  endif

  n = length (A);

  % Delegate to specialized ILU
  switch (setup.type)
    case "nofill"
        [L, U] = ilu0 (A, setup.milu);
        if (nargout == 3)
          P = speye (length (A));
        endif
    case "crout"
        [L, U] = iluc (A, setup.droptol, setup.milu);
        if (nargout == 3)
          P = speye (length (A));
        endif
    case "ilutp"
        if (nargout == 2)
          [L, U]  = ilutp (A, setup.droptol, setup.thresh, setup.milu, setup.udiag);
        elseif (nargout == 3)
          [L, U, P]  = ilutp (A, setup.droptol, setup.thresh, setup.milu, setup.udiag);
        endif
    otherwise
      printf ("The input structure is invalid.\n");
  endswitch

  if (nargout == 1)
    L = L + U - speye (n);
  endif

endfunction

%!shared n, dtol, A
%! n = 1600;
%! dtol = 0.1;
%! A = gallery ('neumann', n) + speye (n);
%!test
%! setup.type = 'nofill';
%! assert (nnz (ilu (A, setup)), 7840);
%!test
%! # This test is taken from the mathworks and should work for full support.
%! setup.type = 'crout';
%! setup.milu = 'row';
%! setup.droptol = dtol;
%! [L, U] = ilu (A, setup);
%! e = ones (size (A,2),1);
%! assert (norm (A*e - L*U*e), 1e-14, 1e-14);
%!test
%! setup.type = 'crout';
%! setup.droptol = dtol;
%! [L, U] = ilu(A,setup);
%! assert (norm (A - L * U, 'fro') / norm (A, 'fro'), 0.05, 1e-2);
%!test
%! setup.type = 'crout';
%! setup.droptol = dtol;
%! [L, U] = ilu (A, setup);
%! for j = 1:n
%!   cmp_value = dtol * norm (A(:, j)) / 2;
%!   non_zeros = nonzeros (U(:, j));
%!   for i = 1:length (non_zeros);
%!     assert (abs (non_zeros (i)) >= cmp_value, logical (1));
%!   endfor
%! endfor
%!test
%! setup.type = 'crout';
%! setup.droptol = dtol;
%! [L, U] = ilu (A, setup);
%! for j = 1:n
%!   cmp_value = dtol * norm (A(:, j)) / 2;
%!   non_zeros = nonzeros (U(:, j));
%!   for i = 1:length (non_zeros);
%!     assert (abs (non_zeros (i)) >= cmp_value, logical (1));
%!   endfor
%! endfor
%!test
%! setup.type = 'crout';
%! setup.droptol = 0;
%! [L1, U1] = ilu (A, setup);
%! setup.type = 'ilutp';
%! [L2, U2] = ilu (A, setup);
%! assert (norm (L1 - L2, 'fro') / norm (L1, 'fro'), 0, eps);
%! assert (norm (U1 - U2, 'fro') / norm (U1, 'fro'), 0, eps);
