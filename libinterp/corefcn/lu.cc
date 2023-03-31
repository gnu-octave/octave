////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lu.h"
#include "sparse-lu.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename MT>
static octave_value
get_lu_l (const math::lu<MT>& fact)
{
  MT L = fact.L ();
  if (L.issquare ())
    return octave_value (L, MatrixType (MatrixType::Lower));
  else
    return L;
}

template <typename MT>
static octave_value
get_lu_u (const math::lu<MT>& fact)
{
  MT U = fact.U ();
  if (U.issquare () && fact.regular ())
    return octave_value (U, MatrixType (MatrixType::Upper));
  else
    return U;
}

DEFUN (lu, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{L}, @var{U}] =} lu (@var{A})
@deftypefnx {} {[@var{L}, @var{U}, @var{P}] =} lu (@var{A})
@deftypefnx {} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} lu (@var{S})
@deftypefnx {} {[@var{L}, @var{U}, @var{P}, @var{Q}, @var{R}] =} lu (@var{S})
@deftypefnx {} {[@dots{}] =} lu (@var{S}, @var{thresh})
@deftypefnx {} {@var{y} =} lu (@dots{})
@deftypefnx {} {[@dots{}] =} lu (@dots{}, "vector")
@cindex LU decomposition
Compute the LU@tie{}decomposition of @var{A}.

If @var{A} is full then subroutines from @sc{lapack} are used, and if
@var{A} is sparse then @sc{umfpack} is used.

The result is returned in a permuted form, according to the optional return
value @var{P}.  For example, given the matrix @code{@var{A} = [1, 2; 3, 4]},

@example
[@var{L}, @var{U}, @var{P}] = lu (@var{A})
@end example

@noindent
returns

@example
@group
L =

  1.00000  0.00000
  0.33333  1.00000

U =

  3.00000  4.00000
  0.00000  0.66667

P =

  0  1
  1  0
@end group
@end example

The matrix is not required to be square.

When called with two or three output arguments and a sparse input matrix,
@code{lu} does not attempt to perform sparsity preserving column permutations.
Called with a fourth output argument, the sparsity preserving column
transformation @var{Q} is returned, such that
@code{@var{P} * @var{A} * @var{Q} = @var{L} * @var{U}}.  This is the
@strong{preferred} way to call @code{lu} with sparse input matrices.

Called with a fifth output argument and a sparse input matrix, @code{lu}
attempts to use a scaling factor @var{R} on the input matrix such that
@code{@var{P} * (@var{R} \ @var{A}) * @var{Q} = @var{L} * @var{U}}.
This typically leads to a sparser and more stable factorization.

An additional input argument @var{thresh} that defines the pivoting
threshold can be given.  @var{thresh} can be a scalar, in which case
it defines the @sc{umfpack} pivoting tolerance for both symmetric and
unsymmetric cases.  If @var{thresh} is a 2-element vector, then the first
element defines the pivoting tolerance for the unsymmetric @sc{umfpack}
pivoting strategy and the second for the symmetric strategy.  By default,
the values defined by @code{spparms} are used ([0.1, 0.001]).

Given the string argument @qcode{"vector"}, @code{lu} returns the values
of @var{P} and @var{Q} as vector values, such that for full matrix,
@code{@var{A}(@var{P},:) = @var{L} * @var{U}}, and @code{@var{R}(@var{P},:)
* @var{A}(:,@var{Q}) = @var{L} * @var{U}}.

With two output arguments, returns the permuted forms of the upper and
lower triangular matrices, such that @code{@var{A} = @var{L} * @var{U}}.
With one output argument @var{y}, then the matrix returned by the
@sc{lapack} routines is returned.  If the input matrix is sparse then the
matrix @var{L} is embedded into @var{U} to give a return value similar to
the full case.  For both full and sparse matrices, @code{lu} loses the
permutation information.
@seealso{luupdate, ilu, chol, hess, qr, qz, schur, svd}
@end deftypefn */)
{
  int nargin = args.length ();
  bool issparse = (nargin > 0 && args(0).issparse ());

  if (nargin < 1 || (issparse && nargin > 3) || (! issparse && nargin > 2))
    print_usage ();

  bool vecout = false;
  Matrix thresh;
  int n = 1;

  while (n < nargin)
    {
      if (args(n).is_string ())
        {
          std::string tmp = args(n++).string_value ();

          if (tmp == "vector")
            vecout = true;
          else
            error ("lu: unrecognized string argument");
        }
      else
        {
          if (! issparse)
            error ("lu: can not define pivoting threshold THRESH for full matrices");

          Matrix tmp = args(n++).matrix_value ();
          if (tmp.numel () == 1)
            {
              thresh.resize (1, 2);
              thresh(0) = tmp(0);
              thresh(1) = tmp(0);
            }
          else if (tmp.numel () == 2)
            thresh = tmp;
          else
            error ("lu: THRESH must be a 1- or 2-element vector");
        }
    }

  octave_value_list retval;
  bool scale = (nargout == 5);

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (issparse)
    {
      if (arg.isempty ())
        return octave_value_list (5, SparseMatrix ());

      if (arg.isreal ())
        {
          SparseMatrix m = arg.sparse_matrix_value ();

          if (nargout < 4)
            {
              warning_with_id ("Octave:lu:sparse_input",
                               "lu: function may fail when called with less than 4 output arguments and a sparse input");

              ColumnVector Qinit (nc);
              for (octave_idx_type i = 0; i < nc; i++)
                Qinit(i) = i;
              math::sparse_lu<SparseMatrix> fact (m, Qinit, thresh, false,
                                                  true);

              if (nargout < 2)
                retval(0) = fact.Y ();
              else
                {
                  retval.resize (nargout == 3 ? 3 : 2);
                  retval(1)
                    = octave_value (fact.U () * fact.Pc_mat ().transpose (),
                                    MatrixType (MatrixType::Permuted_Upper,
                                                nc, fact.col_perm ()));

                  PermMatrix P = fact.Pr_mat ();
                  SparseMatrix L = fact.L ();

                  if (nargout == 2)
                    retval(0)
                      = octave_value (P.transpose () * L,
                                      MatrixType (MatrixType::Permuted_Lower,
                                                  nr, fact.row_perm ()));
                  else
                    {
                      retval(0) = L;
                      if (vecout)
                        retval(2) = fact.Pr_vec();
                      else
                        retval(2) = P;
                    }
                }
            }
          else
            {
              retval.resize (scale ? 5 : 4);
              math::sparse_lu<SparseMatrix> fact (m, thresh, scale);

              retval(0) = octave_value (fact.L (),
                                        MatrixType (MatrixType::Lower));
              retval(1) = octave_value (fact.U (),
                                        MatrixType (MatrixType::Upper));

              if (vecout)
                {
                  retval(2) = fact.Pr_vec ();
                  retval(3) = fact.Pc_vec ();
                }
              else
                {
                  retval(2) = fact.Pr_mat ();
                  retval(3) = fact.Pc_mat ();
                }

              if (scale)
                retval(4) = fact.R ();
            }
        }
      else if (arg.iscomplex ())
        {
          SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

          if (nargout < 4)
            {
              warning_with_id ("Octave:lu:sparse_input",
                               "lu: function may fail when called with less than 4 output arguments and a sparse input");

              ColumnVector Qinit (nc);
              for (octave_idx_type i = 0; i < nc; i++)
                Qinit(i) = i;
              math::sparse_lu<SparseComplexMatrix> fact (m, Qinit, thresh,
                  false, true);

              if (nargout < 2)
                retval(0) = fact.Y ();
              else
                {
                  retval.resize (nargout == 3 ? 3 : 2);
                  retval(1)
                    = octave_value (fact.U () * fact.Pc_mat ().transpose (),
                                    MatrixType (MatrixType::Permuted_Upper,
                                                nc, fact.col_perm ()));

                  PermMatrix P = fact.Pr_mat ();
                  SparseComplexMatrix L = fact.L ();
                  if (nargout == 2)
                    retval(0)
                      = octave_value (P.transpose () * L,
                                      MatrixType (MatrixType::Permuted_Lower,
                                                  nr, fact.row_perm ()));
                  else
                    {
                      retval(0) = L;
                      if (vecout)
                        retval(2) = fact.Pr_vec();
                      else
                        retval(2) = P;
                    }
                }
            }
          else
            {
              retval.resize (scale ? 5 : 4);
              math::sparse_lu<SparseComplexMatrix> fact (m, thresh, scale);

              retval(0) = octave_value (fact.L (),
                                        MatrixType (MatrixType::Lower));
              retval(1) = octave_value (fact.U (),
                                        MatrixType (MatrixType::Upper));

              if (vecout)
                {
                  retval(2) = fact.Pr_vec ();
                  retval(3) = fact.Pc_vec ();
                }
              else
                {
                  retval(2) = fact.Pr_mat ();
                  retval(3) = fact.Pc_mat ();
                }

              if (scale)
                retval(4) = fact.R ();
            }

        }
      else
        err_wrong_type_arg ("lu", arg);
    }
  else
    {
      if (arg.isempty ())
        return octave_value_list (3, Matrix ());

      if (arg.isreal ())
        {
          if (arg.is_single_type ())
            {
              FloatMatrix m = arg.float_matrix_value ();

              math::lu<FloatMatrix> fact (m);

              switch (nargout)
                {
                case 0:
                case 1:
                  retval = ovl (fact.Y ());
                  break;

                case 2:
                  {
                    PermMatrix P = fact.P ();
                    FloatMatrix L = P.transpose () * fact.L ();
                    retval = ovl (L, get_lu_u (fact));
                  }
                  break;

                case 3:
                default:
                  {
                    if (vecout)
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P_vec ());
                    else
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P ());
                  }
                  break;
                }
            }
          else
            {
              Matrix m = arg.matrix_value ();

              math::lu<Matrix> fact (m);

              switch (nargout)
                {
                case 0:
                case 1:
                  retval = ovl (fact.Y ());
                  break;

                case 2:
                  {
                    PermMatrix P = fact.P ();
                    Matrix L = P.transpose () * fact.L ();
                    retval = ovl (L, get_lu_u (fact));
                  }
                  break;

                case 3:
                default:
                  {
                    if (vecout)
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P_vec ());
                    else
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P ());
                  }
                  break;
                }
            }
        }
      else if (arg.iscomplex ())
        {
          if (arg.is_single_type ())
            {
              FloatComplexMatrix m = arg.float_complex_matrix_value ();

              math::lu<FloatComplexMatrix> fact (m);

              switch (nargout)
                {
                case 0:
                case 1:
                  retval = ovl (fact.Y ());
                  break;

                case 2:
                  {
                    PermMatrix P = fact.P ();
                    FloatComplexMatrix L = P.transpose () * fact.L ();
                    retval = ovl (L, get_lu_u (fact));
                  }
                  break;

                case 3:
                default:
                  {
                    if (vecout)
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P_vec ());
                    else
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P ());
                  }
                  break;
                }
            }
          else
            {
              ComplexMatrix m = arg.complex_matrix_value ();

              math::lu<ComplexMatrix> fact (m);

              switch (nargout)
                {
                case 0:
                case 1:
                  retval = ovl (fact.Y ());
                  break;

                case 2:
                  {
                    PermMatrix P = fact.P ();
                    ComplexMatrix L = P.transpose () * fact.L ();
                    retval = ovl (L, get_lu_u (fact));
                  }
                  break;

                case 3:
                default:
                  {
                    if (vecout)
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P_vec ());
                    else
                      retval = ovl (get_lu_l (fact), get_lu_u (fact),
                                    fact.P ());
                  }
                  break;
                }
            }
        }
      else
        err_wrong_type_arg ("lu", arg);
    }

  return retval;
}

/*
%!assert(lu ([1, 2; 3, 4]), [3, 4; 1/3, 2/3], eps)

%!test
%! [l, u] = lu ([1, 2; 3, 4]);
%! assert (l, [1/3, 1; 1, 0], sqrt (eps));
%! assert (u, [3, 4; 0, 2/3], sqrt (eps));

%!test
%! [l, u, p] = lu ([1, 2; 3, 4]);
%! assert (l, [1, 0; 1/3, 1], sqrt (eps));
%! assert (u, [3, 4; 0, 2/3], sqrt (eps));
%! assert (p(:,:), [0, 1; 1, 0], sqrt (eps));

%!test
%! [l, u, p] = lu ([1, 2; 3, 4], "vector");
%! assert (l, [1, 0; 1/3, 1], sqrt (eps));
%! assert (u, [3, 4; 0, 2/3], sqrt (eps));
%! assert (p, [2;1], sqrt (eps));

%!test
%! [l, u, p] = lu ([1, 2; 3, 4; 5, 6]);
%! assert (l, [1, 0; 1/5, 1; 3/5, 1/2], sqrt (eps));
%! assert (u, [5, 6; 0, 4/5], sqrt (eps));
%! assert (p(:,:), [0, 0, 1; 1, 0, 0; 0 1 0], sqrt (eps));

%!assert (lu (single ([1, 2; 3, 4])), single ([3, 4; 1/3, 2/3]), eps ("single"))

%!test
%! [l, u] = lu (single ([1, 2; 3, 4]));
%! assert (l, single ([1/3, 1; 1, 0]), sqrt (eps ("single")));
%! assert (u, single ([3, 4; 0, 2/3]), sqrt (eps ("single")));

%!test
%! [l, u, p] = lu (single ([1, 2; 3, 4]));
%! assert (l, single ([1, 0; 1/3, 1]), sqrt (eps ("single")));
%! assert (u, single ([3, 4; 0, 2/3]), sqrt (eps ("single")));
%! assert (p(:,:), single ([0, 1; 1, 0]), sqrt (eps ("single")));

%!test
%! [l, u, p] = lu (single ([1, 2; 3, 4]), "vector");
%! assert (l, single ([1, 0; 1/3, 1]), sqrt (eps ("single")));
%! assert (u, single ([3, 4; 0, 2/3]), sqrt (eps ("single")));
%! assert (p, single ([2;1]), sqrt (eps ("single")));

%!test
%! [l u p] = lu (single ([1, 2; 3, 4; 5, 6]));
%! assert (l, single ([1, 0; 1/5, 1; 3/5, 1/2]), sqrt (eps ("single")));
%! assert (u, single ([5, 6; 0, 4/5]), sqrt (eps ("single")));
%! assert (p(:,:), single ([0, 0, 1; 1, 0, 0; 0 1 0]), sqrt (eps ("single")));

# complex matrix input
%!test
%! C = [1, 0, 1, 0;
%!      1i, 1/3, -1i, 1/3;
%!      1, 2i/3, 1, -2i/3;
%!      1i, -1/3, -1i, -1/3];
%! [L, U, P] = lu (C);
%! assert (rcond (C), 1/8, eps);
%! assert (norm (P'*L*U - C, Inf) < eps);

%!testif HAVE_UMFPACK
%! Bi = [1 2 3 4 5 2 3 6 7 8 4 5 7 8 9];
%! Bj = [1 3 4 5 6 7 8 9 11 12 13 14 15 16 17];
%! Bv = [1 1 1 1 1 1 -1 1 1 1 1 -1 1 -1 1];
%! B = sparse (Bi, Bj, Bv);
%! warning ("off", "Octave:lu:sparse_input", "local");
%! [L, U] = lu (B);
%! assert (L*U, B);
%! [L, U, P] = lu(B);
%! assert (P'*L*U, B);
%! [L, U, P, Q] = lu (B);
%! assert (P'*L*U*Q', B);

%!error lu ()
%!testif HAVE_UMFPACK
%! fail ("[l,u] = lu (sparse (magic (3)))", "warning", "function may fail");
%!error <can not define pivoting threshold> lu ([1, 2; 3, 4], 2)

*/

static
bool check_lu_dims (const octave_value& l, const octave_value& u,
                    const octave_value& p)
{
  octave_idx_type m = l.rows ();
  octave_idx_type k = u.rows ();
  octave_idx_type n = u.columns ();

  return ((l.ndims () == 2 && u.ndims () == 2 && k == l.columns ())
          && k == std::min (m, n)
          && (p.is_undefined () || p.rows () == m));
}

DEFUN (luupdate, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{L}, @var{U}] =} luupdate (@var{L}, @var{U}, @var{x}, @var{y})
@deftypefnx {} {[@var{L}, @var{U}, @var{P}] =} luupdate (@var{L}, @var{U}, @var{P}, @var{x}, @var{y})
Given an LU@tie{}factorization of a real or complex matrix
@w{@var{A} = @var{L}*@var{U}}, @var{L}@tie{}lower unit trapezoidal and
@var{U}@tie{}upper trapezoidal, return the LU@tie{}factorization
of @w{@var{A} + @var{x}*@var{y}.'}, where @var{x} and @var{y} are
column vectors (rank-1 update) or matrices with equal number of columns
(rank-k update).

Optionally, row-pivoted updating can be used by supplying a row permutation
(pivoting) matrix @var{P}; in that case, an updated permutation matrix is
returned.  Note that if @var{L}, @var{U}, @var{P} is a pivoted
LU@tie{}factorization as obtained by @code{lu}:

@example
[@var{L}, @var{U}, @var{P}] = lu (@var{A});
@end example

@noindent
then a factorization of @tcode{@var{A}+@var{x}*@var{y}.'} can be obtained
either as

@example
[@var{L1}, @var{U1}] = lu (@var{L}, @var{U}, @var{P}*@var{x}, @var{y})
@end example

@noindent
or

@example
[@var{L1}, @var{U1}, @var{P1}] = lu (@var{L}, @var{U}, @var{P}, @var{x}, @var{y})
@end example

The first form uses the unpivoted algorithm, which is faster, but less
stable.  The second form uses a slower pivoted algorithm, which is more
stable.

The matrix case is done as a sequence of rank-1 updates; thus, for large
enough k, it will be both faster and more accurate to recompute the
factorization from scratch.
@seealso{lu, cholupdate, qrupdate}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 4 && nargin != 5)
    print_usage ();

  bool pivoted = (nargin == 5);

  octave_value argl = args(0);
  octave_value argu = args(1);
  octave_value argp = (pivoted ? args(2) : octave_value ());
  octave_value argx = args(2 + pivoted);
  octave_value argy = args(3 + pivoted);

  if (! (argl.isnumeric () && argu.isnumeric ()
         && argx.isnumeric () && argy.isnumeric ()
         && (! pivoted || argp.is_perm_matrix ())))
    error ("luupdate: L, U, X, and Y must be numeric");

  if (! check_lu_dims (argl, argu, argp))
    error ("luupdate: dimension mismatch");

  PermMatrix P = (pivoted
                  ? argp.perm_matrix_value ()
                  : PermMatrix::eye (argl.rows ()));

  if (argl.isreal () && argu.isreal ()
      && argx.isreal () && argy.isreal ())
    {
      // all real case
      if (argl.is_single_type () || argu.is_single_type ()
          || argx.is_single_type () || argy.is_single_type ())
        {
          FloatMatrix L = argl.float_matrix_value ();
          FloatMatrix U = argu.float_matrix_value ();
          FloatMatrix x = argx.float_matrix_value ();
          FloatMatrix y = argy.float_matrix_value ();

          math::lu<FloatMatrix> fact (L, U, P);
          if (pivoted)
            fact.update_piv (x, y);
          else
            fact.update (x, y);

          if (pivoted)
            return ovl (get_lu_l (fact), get_lu_u (fact), fact.P ());
          else
            return ovl (get_lu_l (fact), get_lu_u (fact));
        }
      else
        {
          Matrix L = argl.matrix_value ();
          Matrix U = argu.matrix_value ();
          Matrix x = argx.matrix_value ();
          Matrix y = argy.matrix_value ();

          math::lu<Matrix> fact (L, U, P);
          if (pivoted)
            fact.update_piv (x, y);
          else
            fact.update (x, y);

          if (pivoted)
            return ovl (get_lu_l (fact), get_lu_u (fact), fact.P ());
          else
            return ovl (get_lu_l (fact), get_lu_u (fact));
        }
    }
  else
    {
      // complex case
      if (argl.is_single_type () || argu.is_single_type ()
          || argx.is_single_type () || argy.is_single_type ())
        {
          FloatComplexMatrix L = argl.float_complex_matrix_value ();
          FloatComplexMatrix U = argu.float_complex_matrix_value ();
          FloatComplexMatrix x = argx.float_complex_matrix_value ();
          FloatComplexMatrix y = argy.float_complex_matrix_value ();

          math::lu<FloatComplexMatrix> fact (L, U, P);
          if (pivoted)
            fact.update_piv (x, y);
          else
            fact.update (x, y);

          if (pivoted)
            return ovl (get_lu_l (fact), get_lu_u (fact), fact.P ());
          else
            return ovl (get_lu_l (fact), get_lu_u (fact));
        }
      else
        {
          ComplexMatrix L = argl.complex_matrix_value ();
          ComplexMatrix U = argu.complex_matrix_value ();
          ComplexMatrix x = argx.complex_matrix_value ();
          ComplexMatrix y = argy.complex_matrix_value ();

          math::lu<ComplexMatrix> fact (L, U, P);
          if (pivoted)
            fact.update_piv (x, y);
          else
            fact.update (x, y);

          if (pivoted)
            return ovl (get_lu_l (fact), get_lu_u (fact), fact.P ());
          else
            return ovl (get_lu_l (fact), get_lu_u (fact));
        }
    }
}

/*
%!shared A, u, v, Ac, uc, vc
%! A = [0.091364  0.613038  0.999083;
%!      0.594638  0.425302  0.603537;
%!      0.383594  0.291238  0.085574;
%!      0.265712  0.268003  0.238409;
%!      0.669966  0.743851  0.445057 ];
%!
%! u = [0.85082;
%!      0.76426;
%!      0.42883;
%!      0.53010;
%!      0.80683 ];
%!
%! v = [0.98810;
%!      0.24295;
%!      0.43167 ];
%!
%! Ac = [0.620405 + 0.956953i  0.480013 + 0.048806i  0.402627 + 0.338171i;
%!       0.589077 + 0.658457i  0.013205 + 0.279323i  0.229284 + 0.721929i;
%!       0.092758 + 0.345687i  0.928679 + 0.241052i  0.764536 + 0.832406i;
%!       0.912098 + 0.721024i  0.049018 + 0.269452i  0.730029 + 0.796517i;
%!       0.112849 + 0.603871i  0.486352 + 0.142337i  0.355646 + 0.151496i ];
%!
%! uc = [0.20351 + 0.05401i;
%!       0.13141 + 0.43708i;
%!       0.29808 + 0.08789i;
%!       0.69821 + 0.38844i;
%!       0.74871 + 0.25821i ];
%!
%! vc = [0.85839 + 0.29468i;
%!       0.20820 + 0.93090i;
%!       0.86184 + 0.34689i ];
%!

%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (A);
%! [L,U] = luupdate (L,U,P*u,v);
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - A - u*v.'), Inf) < norm (A)*1e1*eps);
%!
%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (Ac);
%! [L,U] = luupdate (L,U,P*uc,vc);
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - Ac - uc*vc.'), Inf) < norm (Ac)*1e1*eps);

%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (single (A));
%! [L,U] = luupdate (L,U,P* single (u), single (v));
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - single (A) - single (u)* single (v).'), Inf)
%!         < norm (single (A))*1e1* eps ("single"));
%!
%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (single (Ac));
%! [L,U] = luupdate (L,U,P* single (uc),single (vc));
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - single (Ac) - single (uc)* single (vc).'), Inf)
%!         < norm (single (Ac))*1e1* eps ("single"));

%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (A);
%! [L,U,P] = luupdate (L,U,P,u,v);
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - A - u*v.'), Inf) < norm (A)*1e1*eps);
%!
%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (A);
%! [~,ordcols] = max (P,[],1);
%! [~,ordrows] = max (P,[],2);
%! P1 = eye (size (P))(:,ordcols);
%! P2 = eye (size (P))(ordrows,:);
%! assert (P1 == P);
%! assert (P2 == P);
%! [L,U,P] = luupdate (L,U,P,u,v);
%! [L,U,P1] = luupdate (L,U,P1,u,v);
%! [L,U,P2] = luupdate (L,U,P2,u,v);
%! assert (P1 == P);
%! assert (P2 == P);
%!
%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (Ac);
%! [L,U,P] = luupdate (L,U,P,uc,vc);
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - Ac - uc*vc.'), Inf) < norm (Ac)*1e1*eps);

%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (single (A));
%! [L,U,P] = luupdate (L,U,P,single (u),single (v));
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - single (A) - single (u)* single (v).'), Inf)
%!         < norm (single (A))*1e1* eps ("single"));
%!
%!testif HAVE_QRUPDATE_LUU
%! [L,U,P] = lu (single (Ac));
%! [L,U,P] = luupdate (L,U,P,single (uc),single (vc));
%! assert (norm (vec (tril (L)-L), Inf) == 0);
%! assert (norm (vec (triu (U)-U), Inf) == 0);
%! assert (norm (vec (P'*L*U - single (Ac) - single (uc)* single (vc).'), Inf)
%!         < norm (single (Ac))*1e1* eps ("single"));
*/

OCTAVE_END_NAMESPACE(octave)
