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

#include <string>

#include "Matrix.h"
#include "chol.h"
#include "oct-string.h"
#include "sparse-chol.h"
#include "sparse-util.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename CHOLT>
static octave_value
get_chol (const CHOLT& fact)
{
  return octave_value (fact.chol_matrix());
}

template <typename CHOLT>
static octave_value
get_chol_r (const CHOLT& fact)
{
  return octave_value (fact.chol_matrix (),
                       MatrixType (MatrixType::Upper));
}

template <typename CHOLT>
static octave_value
get_chol_l (const CHOLT& fact)
{
  return octave_value (fact.chol_matrix ().transpose (),
                       MatrixType (MatrixType::Lower));
}

DEFUN (chol, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{R} =} chol (@var{A})
@deftypefnx {} {[@var{R}, @var{p}] =} chol (@var{A})
@deftypefnx {} {[@var{R}, @var{p}, @var{Q}] =} chol (@var{A})
@deftypefnx {} {[@var{R}, @var{p}, @var{Q}] =} chol (@var{A}, "vector")
@deftypefnx {} {[@var{L}, @dots{}] =} chol (@dots{}, "lower")
@deftypefnx {} {[@var{R}, @dots{}] =} chol (@dots{}, "upper")
@cindex Cholesky factorization
Compute the upper Cholesky@tie{}factor, @var{R}, of the real symmetric
or complex Hermitian positive definite matrix @var{A}.

The upper Cholesky@tie{}factor @var{R} is computed by using the upper
triangular part of matrix @var{A} and is defined by
@tex
$ R^T R = A $.
@end tex
@ifnottex

@example
@var{R}' * @var{R} = @var{A}.
@end example

@end ifnottex

Calling @code{chol} using the optional @qcode{"upper"} flag has the
same behavior.  In contrast, using the optional @qcode{"lower"} flag,
@code{chol} returns the lower triangular factorization, computed by using
the lower triangular part of matrix @var{A}, such that
@tex
$ L L^T = A $.
@end tex
@ifnottex

@example
@var{L} * @var{L}' = @var{A}.
@end example

@end ifnottex

Called with one output argument @code{chol} fails if matrix @var{A} is
not positive definite.  Note that if matrix @var{A} is not real symmetric
or complex Hermitian then the lower triangular part is considered to be
the (complex conjugate) transpose of the upper triangular part, or vice
versa, given the @qcode{"lower"} flag.

Called with two or more output arguments @var{p} flags whether the matrix
@var{A} was positive definite and @code{chol} does not fail.  A zero value
of @var{p} indicates that matrix @var{A} is positive definite and @var{R}
gives the factorization.  Otherwise, @var{p} will have a positive value.

If called with three output arguments matrix @var{A} must be sparse and
a sparsity preserving row/column permutation is applied to matrix @var{A}
prior to the factorization.  That is @var{R} is the factorization of
@code{@var{A}(@var{Q},@var{Q})} such that
@tex
$ R^T R = Q^T A Q$.
@end tex
@ifnottex

@example
@var{R}' * @var{R} = @var{Q}' * @var{A} * @var{Q}.
@end example

@end ifnottex

The sparsity preserving permutation is generally returned as a matrix.
However, given the optional flag @qcode{"vector"}, @var{Q} will be
returned as a vector such that
@tex
$ R^T R = A (Q, Q)$.
@end tex
@ifnottex

@example
@var{R}' * @var{R} = @var{A}(@var{Q}, @var{Q}).
@end example

@end ifnottex

In general the lower triangular factorization is significantly faster for
sparse matrices.
@seealso{hess, lu, qr, qz, schur, svd, ichol, cholinv, chol2inv, cholupdate,
cholinsert, choldelete, cholshift}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3 || nargout > 3)
    print_usage ();
  if (nargout > 2 && ! args(0).issparse ())
    error ("chol: using three output arguments, matrix A must be sparse");

  bool LLt = false;
  bool vecout = false;

  int n = 1;
  while (n < nargin)
    {
      std::string tmp = args(n++).xstring_value ("chol: optional arguments must be strings");

      if (string::strcmpi (tmp, "vector"))
        vecout = true;
      else if (string::strcmpi (tmp, "lower"))
        LLt = true;
      else if (string::strcmpi (tmp, "upper"))
        LLt = false;
      else
        error (R"(chol: optional argument must be one of "vector", "lower", or "upper")");
    }

  octave_value_list retval;
  octave_value arg = args(0);

  if (arg.isempty ())
    return ovl (Matrix ());

  if (arg.issparse ())
    {
      octave_idx_type info;
      bool natural = (nargout != 3);
      bool force = nargout > 1;

      if (arg.isreal ())
        {
          SparseMatrix m = arg.sparse_matrix_value ();

          math::sparse_chol<SparseMatrix> fact (m, info, natural, force);

          if (nargout == 3)
            {
              if (vecout)
                retval(2) = fact.perm ();
              else
                retval(2) = fact.Q ();
            }

          if (nargout >= 2 || info == 0)
            {
              retval(1) = info;
              if (LLt)
                retval(0) = fact.L ();
              else
                retval(0) = fact.R ();
            }
          else
            error ("chol: input matrix must be positive definite");
        }
      else if (arg.iscomplex ())
        {
          SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

          math::sparse_chol<SparseComplexMatrix> fact (m, info, natural, force);

          if (nargout == 3)
            {
              if (vecout)
                retval(2) = fact.perm ();
              else
                retval(2) = fact.Q ();
            }

          if (nargout >= 2 || info == 0)
            {
              retval(1) = info;
              if (LLt)
                retval(0) = fact.L ();
              else
                retval(0) = fact.R ();
            }
          else
            error ("chol: input matrix must be positive definite");
        }
      else
        err_wrong_type_arg ("chol", arg);
    }
  else if (arg.is_single_type ())
    {
      if (vecout)
        error (R"(chol: A must be sparse for the "vector" option)");
      if (arg.isreal ())
        {
          FloatMatrix m = arg.float_matrix_value ();

          octave_idx_type info;

          math::chol<FloatMatrix> fact (m, info, LLt != true);

          if (nargout == 2 || info == 0)
            retval = ovl (get_chol (fact), info);
          else
            error ("chol: input matrix must be positive definite");
        }
      else if (arg.iscomplex ())
        {
          FloatComplexMatrix m = arg.float_complex_matrix_value ();

          octave_idx_type info;

          math::chol<FloatComplexMatrix> fact (m, info, LLt != true);

          if (nargout == 2 || info == 0)
            retval = ovl (get_chol (fact), info);
          else
            error ("chol: input matrix must be positive definite");
        }
      else
        err_wrong_type_arg ("chol", arg);
    }
  else
    {
      if (vecout)
        error (R"(chol: A must be sparse for the "vector" option)");
      if (arg.isreal ())
        {
          Matrix m = arg.matrix_value ();

          octave_idx_type info;

          math::chol<Matrix> fact (m, info, LLt != true);

          if (nargout == 2 || info == 0)
            retval = ovl (get_chol (fact), info);
          else
            error ("chol: input matrix must be positive definite");
        }
      else if (arg.iscomplex ())
        {
          ComplexMatrix m = arg.complex_matrix_value ();

          octave_idx_type info;

          math::chol<ComplexMatrix> fact (m, info, LLt != true);

          if (nargout == 2 || info == 0)
            retval = ovl (get_chol (fact), info);
          else
            error ("chol: input matrix must be positive definite");
        }
      else
        err_wrong_type_arg ("chol", arg);
    }

  return retval;
}

/*
%!assert (chol ([2, 1; 1, 1]), [sqrt(2), 1/sqrt(2); 0, 1/sqrt(2)], sqrt (eps))
%!assert (chol (single ([2, 1; 1, 1])),
%!        single ([sqrt(2), 1/sqrt(2); 0, 1/sqrt(2)]), sqrt (eps ("single")))

%!assert (chol ([2, 1; 1, 1], "upper"), [sqrt(2), 1/sqrt(2); 0, 1/sqrt(2)],
%!        sqrt (eps))
%!assert (chol ([2, 1; 1, 1], "lower"), [sqrt(2), 0; 1/sqrt(2), 1/sqrt(2)],
%!        sqrt (eps))

%!assert (chol ([2, 1; 1, 1], "lower"), chol ([2, 1; 1, 1], "LoweR"))
%!assert (chol ([2, 1; 1, 1], "upper"), chol ([2, 1; 1, 1], "Upper"))

## Check the "vector" option which only affects the 3rd argument and
## is only valid for sparse input.
%!testif HAVE_CHOLMOD
%! a = sparse ([2 1; 1 1]);
%! r = sparse ([sqrt(2), 1/sqrt(2); 0, 1/sqrt(2)]);
%! [rd, pd, qd] = chol (a);
%! [rv, pv, qv] = chol (a, "vector");
%! assert (r, rd, eps)
%! assert (r, rv, eps)
%! assert (pd, 0)
%! assert (pd, pv)
%! assert (qd, sparse (eye (2)))
%! assert (qv, [1 2])
%!
%! [rv, pv, qv] = chol (a, "Vector"); # check case sensitivity
%! assert (r, rv, eps)
%! assert (pd, pv)
%! assert (qv, [1 2])

%!testif HAVE_CHOLMOD <*42587>
%! A = sparse ([1 0 8;0 1 8;8 8 1]);
%! [Q, p] = chol (A);
%! assert (p != 0);

%!error chol ()
%!error <matrix must be positive definite> chol ([1, 2; 3, 4])
%!error <requires square matrix> chol ([1, 2; 3, 4; 5, 6])
%!error <optional arguments must be strings> chol (1, 2)
%!error <optional argument must be one of "vector", "lower"> chol (1, "foobar")
%!error <matrix A must be sparse> [L,p,Q] = chol ([1, 2; 3, 4])
%!error <A must be sparse> [L, p] = chol ([1, 2; 3, 4], "vector")
*/

DEFUN (cholinv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{Ainv} =} cholinv (@var{A})
Compute the inverse of the symmetric positive definite matrix @var{A} using
the Cholesky@tie{}factorization.
@seealso{chol, chol2inv, inv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval;
  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr == 0 || nc == 0)
    retval = Matrix ();
  else
    {
      if (arg.issparse ())
        {
          octave_idx_type info;

          if (arg.isreal ())
            {
              SparseMatrix m = arg.sparse_matrix_value ();

              math::sparse_chol<SparseMatrix> chol (m, info);

              if (info == 0)
                retval = chol.inverse ();
              else
                error ("cholinv: A must be positive definite");
            }
          else if (arg.iscomplex ())
            {
              SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

              math::sparse_chol<SparseComplexMatrix> chol (m, info);

              if (info == 0)
                retval = chol.inverse ();
              else
                error ("cholinv: A must be positive definite");
            }
          else
            err_wrong_type_arg ("cholinv", arg);
        }
      else if (arg.is_single_type ())
        {
          if (arg.isreal ())
            {
              FloatMatrix m = arg.float_matrix_value ();

              octave_idx_type info;
              math::chol<FloatMatrix> chol (m, info);
              if (info == 0)
                retval = chol.inverse ();
              else
                error ("cholinv: A must be positive definite");
            }
          else if (arg.iscomplex ())
            {
              FloatComplexMatrix m = arg.float_complex_matrix_value ();

              octave_idx_type info;
              math::chol<FloatComplexMatrix> chol (m, info);
              if (info == 0)
                retval = chol.inverse ();
              else
                error ("cholinv: A must be positive definite");
            }
          else
            err_wrong_type_arg ("chol", arg);
        }
      else
        {
          if (arg.isreal ())
            {
              Matrix m = arg.matrix_value ();

              octave_idx_type info;
              math::chol<Matrix> chol (m, info);
              if (info == 0)
                retval = chol.inverse ();
              else
                error ("cholinv: A must be positive definite");
            }
          else if (arg.iscomplex ())
            {
              ComplexMatrix m = arg.complex_matrix_value ();

              octave_idx_type info;
              math::chol<ComplexMatrix> chol (m, info);
              if (info == 0)
                retval = chol.inverse ();
              else
                error ("cholinv: A must be positive definite");
            }
          else
            err_wrong_type_arg ("chol", arg);
        }
    }

  return retval;
}

/*
%!shared A, Ainv
%! A = [2,0.2;0.2,1];
%! Ainv = inv (A);
%!test
%! Ainv1 = cholinv (A);
%! assert (norm (Ainv-Ainv1), 0, 1e-10);
%!testif HAVE_CHOLMOD
%! Ainv2 = inv (sparse (A));
%! assert (norm (Ainv-Ainv2), 0, 1e-10);
%!testif HAVE_CHOLMOD
%! Ainv3 = cholinv (sparse (A));
%! assert (norm (Ainv-Ainv3), 0, 1e-10);
*/

DEFUN (chol2inv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{Ainv} =} chol2inv (@var{R})
Invert a symmetric, positive definite square matrix from its Cholesky
decomposition, @var{R}.

Note that @var{R} should be an upper-triangular matrix with positive diagonal
elements.  @code{chol2inv (@var{U})} provides @code{inv (@var{R}'*@var{R})} but
is much faster than using @code{inv}.
@seealso{chol, cholinv, inv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval;

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr == 0 || nc == 0)
    retval = Matrix ();
  else
    {
      if (arg.issparse ())
        {
          if (arg.isreal ())
            {
              SparseMatrix r = arg.sparse_matrix_value ();

              retval = math::chol2inv (r);
            }
          else if (arg.iscomplex ())
            {
              SparseComplexMatrix r = arg.sparse_complex_matrix_value ();

              retval = math::chol2inv (r);
            }
          else
            err_wrong_type_arg ("chol2inv", arg);
        }
      else if (arg.is_single_type ())
        {
          if (arg.isreal ())
            {
              FloatMatrix r = arg.float_matrix_value ();

              retval = math::chol2inv (r);
            }
          else if (arg.iscomplex ())
            {
              FloatComplexMatrix r = arg.float_complex_matrix_value ();

              retval = math::chol2inv (r);
            }
          else
            err_wrong_type_arg ("chol2inv", arg);

        }
      else
        {
          if (arg.isreal ())
            {
              Matrix r = arg.matrix_value ();

              retval = math::chol2inv (r);
            }
          else if (arg.iscomplex ())
            {
              ComplexMatrix r = arg.complex_matrix_value ();

              retval = math::chol2inv (r);
            }
          else
            err_wrong_type_arg ("chol2inv", arg);
        }
    }

  return retval;
}

/*

## Test for bug #36437
%!function sparse_chol2inv (T, tol)
%!  iT = inv (T);
%!  ciT = chol2inv (chol (T));
%!  assert (ciT, iT, tol);
%!  assert (chol2inv (chol ( full (T))), ciT, tol*2);
%!endfunction

%!testif HAVE_CHOLMOD
%! A = gallery ("poisson", 3);
%! sparse_chol2inv (A, eps);

%!testif HAVE_CHOLMOD
%! n = 10;
%! B = spdiags (ones (n, 1) * [1 2 1], [-1 0 1], n, n);
%! sparse_chol2inv (B, eps*100);

%!testif HAVE_CHOLMOD
%! C = gallery ("tridiag", 5);
%! sparse_chol2inv (C, eps*10);

%!testif HAVE_CHOLMOD
%! D = gallery ("wathen", 1, 1);
%! sparse_chol2inv (D, eps*10^4);

*/

DEFUN (cholupdate, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{R1}, @var{info}] =} cholupdate (@var{R}, @var{u}, @var{op})
Update or downdate a Cholesky@tie{}factorization.

Given an upper triangular matrix @var{R} and a column vector @var{u},
attempt to determine another upper triangular matrix @var{R1} such that

@itemize @bullet
@item
@var{R1}'*@var{R1} = @var{R}'*@var{R} + @var{u}*@var{u}'
if @var{op} is @qcode{"+"}

@item
@var{R1}'*@var{R1} = @var{R}'*@var{R} - @var{u}*@var{u}'
if @var{op} is @qcode{"-"}
@end itemize

If @var{op} is @qcode{"-"}, @var{info} is set to

@itemize
@item 0 if the downdate was successful,

@item 1 if @var{R}'*@var{R} - @var{u}*@var{u}' is not positive definite,

@item 2 if @var{R} is singular.
@end itemize

If @var{info} is not present, an error message is printed in cases 1 and 2.
@seealso{chol, cholinsert, choldelete, cholshift}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value argr = args(0);
  octave_value argu = args(1);

  if (! argr.isnumeric () || ! argu.isnumeric ()
      || (nargin > 2 && ! args(2).is_string ()))
    print_usage ();

  octave_value_list retval (nargout == 2 ? 2 : 1);

  octave_idx_type n = argr.rows ();

  std::string op = (nargin < 3) ? "+" : args(2).string_value ();

  bool down = (op == "-");

  if (! down && op != "+")
    error (R"(cholupdate: OP must be "+" or "-")");

  if (argr.columns () != n || argu.rows () != n || argu.columns () != 1)
    error ("cholupdate: dimension mismatch between R and U");

  int err = 0;
  if (argr.is_single_type () || argu.is_single_type ())
    {
      if (argr.isreal () && argu.isreal ())
        {
          // real case
          FloatMatrix R = argr.float_matrix_value ();
          FloatColumnVector u = argu.float_column_vector_value ();

          math::chol<FloatMatrix> fact;
          fact.set (R);

          if (down)
            err = fact.downdate (u);
          else
            fact.update (u);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          FloatComplexMatrix R = argr.float_complex_matrix_value ();
          FloatComplexColumnVector u
            = argu.float_complex_column_vector_value ();

          math::chol<FloatComplexMatrix> fact;
          fact.set (R);

          if (down)
            err = fact.downdate (u);
          else
            fact.update (u);

          retval = ovl (get_chol_r (fact));
        }
    }
  else
    {
      if (argr.isreal () && argu.isreal ())
        {
          // real case
          Matrix R = argr.matrix_value ();
          ColumnVector u = argu.column_vector_value ();

          math::chol<Matrix> fact;
          fact.set (R);

          if (down)
            err = fact.downdate (u);
          else
            fact.update (u);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          ComplexMatrix R = argr.complex_matrix_value ();
          ComplexColumnVector u = argu.complex_column_vector_value ();

          math::chol<ComplexMatrix> fact;
          fact.set (R);

          if (down)
            err = fact.downdate (u);
          else
            fact.update (u);

          retval = ovl (get_chol_r (fact));
        }
    }

  if (nargout > 1)
    retval(1) = err;
  else if (err == 1)
    error ("cholupdate: downdate violates positiveness");
  else if (err == 2)
    error ("cholupdate: singular matrix");

  return retval;
}

/*
%!shared A, u, Ac, uc
%! A = [  0.436997  -0.131721   0.124120  -0.061673 ;
%!       -0.131721   0.738529   0.019851  -0.140295 ;
%!        0.124120   0.019851   0.354879  -0.059472 ;
%!       -0.061673  -0.140295  -0.059472   0.600939 ];
%!
%! u = [  0.98950 ;
%!        0.39844 ;
%!        0.63484 ;
%!        0.13351 ];
%! Ac = [  0.5585528 + 0.0000000i  -0.1662088 - 0.0315341i   0.0107873 + 0.0236411i  -0.0276775 - 0.0186073i ;
%!        -0.1662088 + 0.0315341i   0.6760061 + 0.0000000i   0.0011452 - 0.0475528i   0.0145967 + 0.0247641i ;
%!         0.0107873 - 0.0236411i   0.0011452 + 0.0475528i   0.6263149 - 0.0000000i  -0.1585837 - 0.0719763i ;
%!        -0.0276775 + 0.0186073i   0.0145967 - 0.0247641i  -0.1585837 + 0.0719763i   0.6034234 - 0.0000000i ];
%!
%! uc = [ 0.54267 + 0.91519i ;
%!        0.99647 + 0.43141i ;
%!        0.83760 + 0.68977i ;
%!        0.39160 + 0.90378i ];

%!test
%! R = chol (A);
%! R1 = cholupdate (R, u);
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - R'*R - u*u', Inf) < 1e1*eps);
%!
%! R1 = cholupdate (R1, u, "-");
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1 - R, Inf) < 1e1*eps);

%!test
%! R = chol (Ac);
%! R1 = cholupdate (R, uc);
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - R'*R - uc*uc', Inf) < 1e1*eps);
%!
%! R1 = cholupdate (R1, uc, "-");
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1 - R, Inf) < 1e1*eps);

%!test
%! R = chol (single (A));
%! R1 = cholupdate (R, single (u));
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - R'*R - single (u*u'), Inf) < 1e1* eps ("single"));
%!
%! R1 = cholupdate (R1, single (u), "-");
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1 - R, Inf) < 2e1* eps ("single"));

%!test
%! R = chol (single (Ac));
%! R1 = cholupdate (R, single (uc));
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - R'*R - single (uc*uc'), Inf) < 1e1* eps ("single"));
%!
%! R1 = cholupdate (R1, single (uc), "-");
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1 - R, Inf) < 2e1* eps ("single"));
*/

DEFUN (cholinsert, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{R1} =} cholinsert (@var{R}, @var{j}, @var{u})
@deftypefnx {} {[@var{R1}, @var{info}] =} cholinsert (@var{R}, @var{j}, @var{u})
Update a Cholesky factorization given a row or column to insert in the
original factored matrix.

Given a Cholesky@tie{}factorization of a real symmetric or complex Hermitian
positive definite matrix @w{@var{A} = @var{R}'*@var{R}}, @var{R}@tie{}upper
triangular, return the Cholesky@tie{}factorization of
@var{A1}, where @w{A1(p,p) = A}, @w{A1(:,j) = A1(j,:)' = u} and
@w{p = [1:j-1,j+1:n+1]}.  @w{u(j)} should be positive.

On return, @var{info} is set to

@itemize
@item 0 if the insertion was successful,

@item 1 if @var{A1} is not positive definite,

@item 2 if @var{R} is singular.
@end itemize

If @var{info} is not present, an error message is printed in cases 1 and 2.
@seealso{chol, cholupdate, choldelete, cholshift}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  octave_value argr = args(0);
  octave_value argj = args(1);
  octave_value argu = args(2);

  if (! argr.isnumeric () || ! argu.isnumeric ()
      || ! argj.is_real_scalar ())
    print_usage ();

  octave_idx_type n = argr.rows ();
  octave_idx_type j = argj.scalar_value ();

  if (argr.columns () != n || argu.rows () != n+1 || argu.columns () != 1)
    error ("cholinsert: dimension mismatch between R and U");

  if (j < 1 || j > n+1)
    error ("cholinsert: index J out of range");

  octave_value_list retval (nargout == 2 ? 2 : 1);

  int err = 0;
  if (argr.is_single_type () || argu.is_single_type ())
    {
      if (argr.isreal () && argu.isreal ())
        {
          // real case
          FloatMatrix R = argr.float_matrix_value ();
          FloatColumnVector u = argu.float_column_vector_value ();

          math::chol<FloatMatrix> fact;
          fact.set (R);
          err = fact.insert_sym (u, j-1);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          FloatComplexMatrix R = argr.float_complex_matrix_value ();
          FloatComplexColumnVector u
            = argu.float_complex_column_vector_value ();

          math::chol<FloatComplexMatrix> fact;
          fact.set (R);
          err = fact.insert_sym (u, j-1);

          retval = ovl (get_chol_r (fact));
        }
    }
  else
    {
      if (argr.isreal () && argu.isreal ())
        {
          // real case
          Matrix R = argr.matrix_value ();
          ColumnVector u = argu.column_vector_value ();

          math::chol<Matrix> fact;
          fact.set (R);
          err = fact.insert_sym (u, j-1);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          ComplexMatrix R = argr.complex_matrix_value ();
          ComplexColumnVector u = argu.complex_column_vector_value ();

          math::chol<ComplexMatrix> fact;
          fact.set (R);
          err = fact.insert_sym (u, j-1);

          retval = ovl (get_chol_r (fact));
        }
    }

  if (nargout > 1)
    retval(1) = err;
  else if (err == 1)
    error ("cholinsert: insertion violates positiveness");
  else if (err == 2)
    error ("cholinsert: singular matrix");
  else if (err == 3)
    error ("cholinsert: diagonal element must be real");

  return retval;
}

/*
%!test
%! u2 = [  0.35080 ;
%!         0.63930 ;
%!         3.31057 ;
%!        -0.13825 ;
%!         0.45266 ];
%!
%! R = chol (A);
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (A1(p,p) - A, Inf) < 1e1*eps);

%!test
%! u2 = [  0.35080  + 0.04298i;
%!         0.63930  + 0.23778i;
%!         3.31057  + 0.00000i;
%!        -0.13825  + 0.19879i;
%!         0.45266  + 0.50020i];
%!
%! R = chol (Ac);
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (A1(p,p) - Ac, Inf) < 1e1*eps);

%!test
%! u2 = single ([  0.35080 ;
%!                 0.63930 ;
%!                 3.31057 ;
%!                -0.13825 ;
%!                 0.45266 ]);
%!
%! R = chol (single (A));
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (A1(p,p) - A, Inf) < 1e1* eps ("single"));

%!test
%! u2 = single ([  0.35080  + 0.04298i;
%!                 0.63930  + 0.23778i;
%!                 3.31057  + 0.00000i;
%!                -0.13825  + 0.19879i;
%!                 0.45266  + 0.50020i]);
%!
%! R = chol (single (Ac));
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (A1(p,p) - single (Ac), Inf) < 2e1* eps ("single"));

%!test
%! cu = chol (triu (A), "upper");
%! cl = chol (tril (A), "lower");
%! assert (cu, cl', eps);

%!test
%! cca   = chol (Ac);
%!
%! ccal  = chol (Ac, "lower");
%! ccal2 = chol (tril (Ac), "lower");
%!
%! ccau  = chol (Ac, "upper");
%! ccau2 = chol (triu (Ac), "upper");
%!
%! assert (cca'*cca,     Ac, eps);
%! assert (ccau'*ccau,   Ac, eps);
%! assert (ccau2'*ccau2, Ac, eps);
%!
%! assert (cca, ccal',  eps);
%! assert (cca, ccau,   eps);
%! assert (cca, ccal2', eps);
%! assert (cca, ccau2,  eps);

%!test
%! cca   = chol (single (Ac));
%!
%! ccal  = chol (single (Ac), "lower");
%! ccal2 = chol (tril (single (Ac)), "lower");
%!
%! ccau  = chol (single (Ac), "upper");
%! ccau2 = chol (triu (single (Ac)), "upper");
%!
%! assert (cca'*cca,     single (Ac), eps ("single"));
%! assert (ccau'*ccau,   single (Ac), eps ("single"));
%! assert (ccau2'*ccau2, single (Ac), eps ("single"));
%!
%! assert (cca, ccal',  eps ("single"));
%! assert (cca, ccau,   eps ("single"));
%! assert (cca, ccal2', eps ("single"));
%! assert (cca, ccau2,  eps ("single"));

%!test
%! a = [12,  2,  3,  4;
%!       2, 14,  5,  3;
%!       3,  5, 16,  6;
%!       4,  3,  6, 16];
%!
%! b = [0,  1,  2,  3;
%!     -1,  0,  1,  2;
%!     -2, -1,  0,  1;
%!     -3, -2, -1,  0];
%!
%! ca = a + i*b;
%!
%! cca   = chol (ca);
%!
%! ccal  = chol (ca, "lower");
%! ccal2 = chol (tril (ca), "lower");
%!
%! ccau  = chol (ca, "upper");
%! ccau2 = chol (triu (ca), "upper");
%!
%! assert (cca'*cca,     ca, 16*eps);
%! assert (ccau'*ccau,   ca, 16*eps);
%! assert (ccau2'*ccau2, ca, 16*eps);
%!
%! assert (cca, ccal',  16*eps);
%! assert (cca, ccau,   16*eps);
%! assert (cca, ccal2', 16*eps);
%! assert (cca, ccau2,  16*eps);
*/

DEFUN (choldelete, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{R1} =} choldelete (@var{R}, @var{j})
Update a Cholesky factorization given a row or column to delete from the
original factored matrix.

Given a Cholesky@tie{}factorization of a real symmetric or complex Hermitian
positive definite matrix @w{@var{A} = @var{R}'*@var{R}}, @var{R}@tie{}upper
triangular, return the Cholesky@tie{}factorization of @w{A(p,p)}, where
@w{p = [1:j-1,j+1:n+1]}.
@seealso{chol, cholupdate, cholinsert, cholshift}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value argr = args(0);
  octave_value argj = args(1);

  if (! argr.isnumeric () || ! argj.is_real_scalar ())
    print_usage ();

  octave_idx_type n = argr.rows ();
  octave_idx_type j = argj.scalar_value ();

  if (argr.columns () != n)
    err_square_matrix_required ("choldelete", "R");

  if (j < 0 && j > n)
    error ("choldelete: index J out of range");

  octave_value_list retval;

  if (argr.is_single_type ())
    {
      if (argr.isreal ())
        {
          // real case
          FloatMatrix R = argr.float_matrix_value ();

          math::chol<FloatMatrix> fact;
          fact.set (R);
          fact.delete_sym (j-1);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          FloatComplexMatrix R = argr.float_complex_matrix_value ();

          math::chol<FloatComplexMatrix> fact;
          fact.set (R);
          fact.delete_sym (j-1);

          retval = ovl (get_chol_r (fact));
        }
    }
  else
    {
      if (argr.isreal ())
        {
          // real case
          Matrix R = argr.matrix_value ();

          math::chol<Matrix> fact;
          fact.set (R);
          fact.delete_sym (j-1);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          ComplexMatrix R = argr.complex_matrix_value ();

          math::chol<ComplexMatrix> fact;
          fact.set (R);
          fact.delete_sym (j-1);

          retval = ovl (get_chol_r (fact));
        }
    }

  return retval;
}

/*
%!test
%! R = chol (A);
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - A(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (Ac);
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - Ac(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (single (A));
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R, j);
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - single (A(p,p)), Inf) < 1e1* eps ("single"));

%!test
%! R = chol (single (Ac));
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R,j);
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - single (Ac(p,p)), Inf) < 1e1* eps ("single"));
*/

DEFUN (cholshift, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{R1} =} cholshift (@var{R}, @var{i}, @var{j})
Update a Cholesky factorization given a range of columns to shift in the
original factored matrix.

Given a Cholesky@tie{}factorization of a real symmetric or complex Hermitian
positive definite matrix @w{@var{A} = @var{R}'*@var{R}}, @var{R}@tie{}upper
triangular, return the Cholesky@tie{}factorization of
@w{@var{A}(p,p)}, where @w{p} is the permutation @*
@code{p = [1:i-1, shift(i:j, 1), j+1:n]} if @w{@var{i} < @var{j}} @*
 or @*
@code{p = [1:j-1, shift(j:i,-1), i+1:n]} if @w{@var{j} < @var{i}}.  @*

@seealso{chol, cholupdate, cholinsert, choldelete}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  octave_value argr = args(0);
  octave_value argi = args(1);
  octave_value argj = args(2);

  if (! argr.isnumeric () || ! argi.is_real_scalar ()
      || ! argj.is_real_scalar ())
    print_usage ();

  octave_idx_type n = argr.rows ();
  octave_idx_type i = argi.scalar_value ();
  octave_idx_type j = argj.scalar_value ();

  if (argr.columns () != n)
    err_square_matrix_required ("cholshift", "R");

  if (j < 0 || j > n+1 || i < 0 || i > n+1)
    error ("cholshift: index I or J is out of range");

  octave_value_list retval;

  if (argr.is_single_type () && argi.is_single_type ()
      && argj.is_single_type ())
    {
      if (argr.isreal ())
        {
          // real case
          FloatMatrix R = argr.float_matrix_value ();

          math::chol<FloatMatrix> fact;
          fact.set (R);
          fact.shift_sym (i-1, j-1);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          FloatComplexMatrix R = argr.float_complex_matrix_value ();

          math::chol<FloatComplexMatrix> fact;
          fact.set (R);
          fact.shift_sym (i-1, j-1);

          retval = ovl (get_chol_r (fact));
        }
    }
  else
    {
      if (argr.isreal ())
        {
          // real case
          Matrix R = argr.matrix_value ();

          math::chol<Matrix> fact;
          fact.set (R);
          fact.shift_sym (i-1, j-1);

          retval = ovl (get_chol_r (fact));
        }
      else
        {
          // complex case
          ComplexMatrix R = argr.complex_matrix_value ();

          math::chol<ComplexMatrix> fact;
          fact.set (R);
          fact.shift_sym (i-1, j-1);

          retval = ovl (get_chol_r (fact));
        }
    }

  return retval;
}

/*
%!test
%! R = chol (A);
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - A(p,p), Inf) < 1e1*eps);
%!
%! j = 1;  i = 3;  p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1) - R1, Inf), 0);
%! assert (norm (R1'*R1 - A(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (Ac);
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - Ac(p,p), Inf) < 1e1*eps);
%!
%! j = 1;  i = 3;  p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - Ac(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (single (A));
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (A(p,p)), Inf) < 1e1* eps ("single"));
%!
%! j = 1;  i = 3;  p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (A(p,p)), Inf) < 1e1* eps ("single"));

%!test
%! R = chol (single (Ac));
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (Ac(p,p)), Inf) < 1e1* eps ("single"));
%!
%! j = 1; i = 3; p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (Ac(p,p)), Inf) < 1e1* eps ("single"));
*/

OCTAVE_END_NAMESPACE(octave)
