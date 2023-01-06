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

#include "MArray.h"
#include "Matrix.h"
#include "qr.h"
#include "qrp.h"
#include "sparse-qr.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

/*
## Restore all rand* "state" values
%!function restore_rand_states (state)
%!  rand ("state", state.rand);
%!  randn ("state", state.randn);
%!endfunction

%!shared old_state, restore_state
%! ## Save and restore the states of both random number generators that are
%! ## tested by the unit tests in this file.
%! old_state.rand = rand ("state");
%! old_state.randn = randn ("state");
%! restore_state = onCleanup (@() restore_rand_states (old_state));
*/

template <typename MT>
static octave_value
get_qr_r (const math::qr<MT>& fact)
{
  MT R = fact.R ();
  if (R.issquare () && fact.regular ())
    return octave_value (R, MatrixType (MatrixType::Upper));
  else
    return R;
}

template <typename T>
static typename math::qr<T>::type
qr_type (int nargout, bool economy)
{
  if (nargout == 0 || nargout == 1)
    return math::qr<T>::raw;
  else if (economy)
    return math::qr<T>::economy;
  else
    return math::qr<T>::std;
}

// dense X
//
// [Q, R] = qr (X):       form Q unitary and R upper triangular such
//                        that Q * R = X
//
// [Q, R] = qr (X, 0):    form the economy decomposition such that if X is
//                        m by n then only the first n columns of Q are
//                        computed.
//
// [Q, R, P] = qr (X):    form QRP factorization of X where
//                        P is a permutation matrix such that
//                        A * P = Q * R
//
// [Q, R, P] = qr (X, 0): form the economy decomposition with
//                        permutation vector P such that Q * R = X(:, P)
//
// qr (X) alone returns the output of the LAPACK routine dgeqrf, such
// that R = triu (qr (X))
//
// sparse X
//
// X = qr (A, B):         if M < N, X is the minimum 2-norm solution of
//                        A\B. If M >= N, X is the least squares
//                        approximation of A\B. X is calculated by
//                        SPQR-function SuiteSparseQR_min2norm.
//
DEFUN (qr, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{Q}, @var{R}] =} qr (@var{A})
@deftypefnx {} {[@var{Q}, @var{R}, @var{P}] =} qr (@var{A})
@deftypefnx {} {@var{X} =} qr (@var{A})  # non-sparse A
@deftypefnx {} {@var{R} =} qr (@var{A})  # sparse A
@deftypefnx {} {@var{X} =} qr (@var{A}, @var{B}) # sparse A
@deftypefnx {} {[@var{C}, @var{R}] =} qr (@var{A}, @var{B})
@deftypefnx {} {[@dots{}] =} qr (@dots{}, 0)
@deftypefnx {} {[@dots{}] =} qr (@dots{}, "econ")
@deftypefnx {} {[@dots{}] =} qr (@dots{}, "vector")
@deftypefnx {} {[@dots{}] =} qr (@dots{}, "matrix")
@cindex QR factorization
Compute the QR@tie{}factorization of @var{A}, using standard @sc{lapack}
subroutines.

The QR@tie{}factorization is
@tex
$QR = A$ where $Q$ is an orthogonal matrix and $R$ is upper triangular.
@end tex
@ifnottex

@example
@var{Q} * @var{R} = @var{A}
@end example

@noindent
where @var{Q} is an orthogonal matrix and @var{R} is upper triangular.
@end ifnottex

For example, given the matrix @code{@var{A} = [1, 2; 3, 4]},

@example
[@var{Q}, @var{R}] = qr (@var{A})
@end example

@noindent
returns

@example
@group
@var{Q} =

  -0.31623  -0.94868
  -0.94868   0.31623

@var{R} =

  -3.16228  -4.42719
   0.00000  -0.63246
@end group
@end example

@noindent
which multiplied together return the original matrix

@example
@group
@var{Q} * @var{R}
  @result{}
     1.0000   2.0000
     3.0000   4.0000
@end group
@end example

If just a single return value is requested then it is either @var{R}, if
@var{A} is sparse, or @var{X}, such that @code{@var{R} = triu (@var{X})} if
@var{A} is full.  (Note: unlike most commands, the single return value is not
the first return value when multiple values are requested.)

If a third output @var{P} is requested, then @code{qr} calculates the permuted
QR@tie{}factorization
@tex
$QR = AP$ where $Q$ is an orthogonal matrix, $R$ is upper triangular, and $P$
is a permutation matrix.
@end tex
@ifnottex

@example
@var{Q} * @var{R} = @var{A} * @var{P}
@end example

@noindent
where @var{Q} is an orthogonal matrix, @var{R} is upper triangular, and
@var{P} is a permutation matrix.
@end ifnottex

If @var{A} is dense, the permuted QR@tie{}factorization has the additional
property that the diagonal entries of @var{R} are ordered by decreasing
magnitude.  In other words, @code{abs (diag (@var{R}))} will be ordered
from largest to smallest.

If @var{A} is sparse, @var{P} is a fill-reducing ordering of the columns
of @var{A}.  In that case, the diagonal entries of @var{R} are not ordered by
decreasing magnitude.

For example, given the matrix @code{@var{A} = [1, 2; 3, 4]},

@example
[@var{Q}, @var{R}, @var{P}] = qr (@var{A})
@end example

@noindent
returns

@example
@group
@var{Q} =

  -0.44721  -0.89443
  -0.89443   0.44721

@var{R} =

  -4.47214  -3.13050
   0.00000   0.44721

@var{P} =

   0  1
   1  0
@end group
@end example

If the input matrix @var{A} is sparse, the sparse QR@tie{}factorization
is computed by using @sc{SPQR} or @sc{cxsparse} (e.g., if @sc{SPQR} is not
available).  Because the matrix @var{Q} is, in general, a full matrix, it is
recommended to request only one return value @var{R}.  In that case, the
computation avoids the construction of @var{Q} and returns a sparse @var{R}
such that @code{@var{R} = chol (@var{A}' * @var{A})}.

If @var{A} is dense, an additional matrix @var{B} is supplied and two
return values are requested, then @code{qr} returns @var{C}, where
@code{@var{C} = @var{Q}' * @var{B}}.  This allows the least squares
approximation of @code{@var{A} \ @var{B}} to be calculated as

@example
@group
[@var{C}, @var{R}] = qr (@var{A}, @var{B})
@var{X} = @var{R} \ @var{C}
@end group
@end example

If @var{A} is a sparse @nospell{MxN} matrix and an additional matrix @var{B} is
supplied, one or two return values are possible.  If one return value @var{X}
is requested and M < N, then @var{X} is the minimum 2-norm solution of
@w{@code{@var{A} \ @var{B}}}.  If M >= N, @var{X} is the least squares
approximation @w{of @code{@var{A} \ @var{B}}}.  If two return values are
requested, @var{C} and @var{R} have the same meaning as in the dense case
(@var{C} is dense and @var{R} is sparse).  The version with one return
parameter should be preferred because it uses less memory and can handle
rank-deficient matrices better.

If the final argument is the string @qcode{"vector"} then @var{P} is a
permutation vector (of the columns of @var{A}) instead of a permutation
matrix.  In this case, the defining relationship is:

@example
@var{Q} * @var{R} = @var{A}(:, @var{P})
@end example

The default, however, is to return a permutation matrix and this may be
explicitly specified by using a final argument of @qcode{"matrix"}.

If the final argument is the scalar 0 or the string @qcode{"econ"}, an economy
factorization is returned.  If the original matrix @var{A} has size
@nospell{MxN} and M > N, then the economy factorization will calculate just N
rows in @var{R} and N columns in @var{Q} and omit the zeros in @var{R}.  If M
@leq{} N, there is no difference between the economy and standard
factorizations.  When calculating an economy factorization and @var{A} is
dense, the output @var{P} is always a vector rather than a matrix.  If @var{A}
is sparse, output @var{P} is a sparse permutation matrix.

Background: The QR factorization has applications in the solution of least
squares problems
@tex
$$
\min_x \left\Vert A x - b \right\Vert_2
$$
@end tex
@ifnottex

@example
min norm (A*x - b)
@end example

@end ifnottex
for overdetermined systems of equations (i.e.,
@tex
$A$
@end tex
@ifnottex
@var{A}
@end ifnottex
is a tall, thin matrix).

The permuted QR@tie{}factorization
@code{[@var{Q}, @var{R}, @var{P}] = qr (@var{A})} allows the construction of an
orthogonal basis of @code{span (A)}.

@seealso{chol, hess, lu, qz, schur, svd, qrupdate, qrinsert, qrdelete, qrshift}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value_list retval;

  octave_value arg = args(0);

  bool economy = false;
  bool is_cmplx = false;
  bool have_b = false;
  bool vector_p = false;

  if (arg.iscomplex ())
    is_cmplx = true;
  if (nargin > 1)
    {
      have_b = true;
      if (args(nargin-1).is_scalar_type ())
        {
          int val = args(nargin-1).int_value ();
          if (val == 0)
            {
              economy = true;
              have_b = (nargin > 2);
            }
          else if (nargin == 3)   // argument 3 should be 0 or a string
            print_usage ();
        }
      else if (args(nargin-1).is_string ())
        {
          std::string str = args(nargin-1).string_value ();
          if (str == "vector")
            vector_p = true;
          else if (str == "econ")
            {
              economy = true;
              have_b = (nargin > 2);
            }
          else if (str != "matrix")
            error ("qr: option string must be 'econ' or 'matrix' or " \
                   "'vector', not \"%s\"", str.c_str ());
          have_b = (nargin > 2);
        }
      else if (! args(nargin-1).is_matrix_type ())
        err_wrong_type_arg ("qr", args(nargin-1));
      else if (nargin == 3)   // should be caught by is_scalar_type or is_string
        print_usage ();

      if (have_b && args(1).iscomplex ())
        is_cmplx = true;
    }

  if (arg.issparse ())
    {
      if (nargout > 3)
        error ("qr: too many output arguments");

      if (is_cmplx)
        {
          if (have_b && nargout == 1)
            {
              octave_idx_type info;

              if (! args(1).issparse () && args(1).iscomplex ())
                retval = ovl
                         (math::sparse_qr<SparseComplexMatrix>::solve
                          <MArray<Complex>, ComplexMatrix>
                          (arg.sparse_complex_matrix_value (),
                           args(1).complex_matrix_value (), info));
              else if (args(1).issparse () && args(1).iscomplex ())
                retval = ovl
                         (math::sparse_qr<SparseComplexMatrix>::solve
                          <SparseComplexMatrix, SparseComplexMatrix>
                          (arg.sparse_complex_matrix_value (),
                           args(1).sparse_complex_matrix_value (), info));
              else if (! args(1).issparse () && ! args(1).iscomplex ())
                retval = ovl
                         (math::sparse_qr<SparseComplexMatrix>::solve
                          <MArray<double>, ComplexMatrix>
                          (arg.sparse_complex_matrix_value (),
                           args(1).matrix_value (), info));
              else if (args(1).issparse () && ! args(1).iscomplex ())
                retval = ovl
                         (math::sparse_qr<SparseComplexMatrix>::solve
                          <SparseMatrix, SparseComplexMatrix>
                          (arg.sparse_complex_matrix_value (),
                           args(1).sparse_matrix_value (), info));
              else
                error ("qr: b is not valid");
            }
          else if (have_b && nargout == 2)
            {
              math::sparse_qr<SparseComplexMatrix>
              q (arg.sparse_complex_matrix_value (), 0);
              retval = ovl (q.C (args(1).complex_matrix_value (), economy),
                            q.R (economy));
            }
          else if (have_b && nargout == 3)
            {
              math::sparse_qr<SparseComplexMatrix>
              q (arg.sparse_complex_matrix_value ());
              if (vector_p)
                retval = ovl (q.C (args(1).complex_matrix_value (), economy),
                              q.R (economy), q.E ());
              else
                retval = ovl (q.C (args(1).complex_matrix_value (), economy),
                              q.R (economy), q.E_MAT ());
            }
          else
            {
              if (nargout > 2)
                {
                  math::sparse_qr<SparseComplexMatrix>
                  q (arg.sparse_complex_matrix_value ());
                  if (vector_p)
                    retval = ovl (q.Q (economy), q.R (economy), q.E ());
                  else
                    retval = ovl (q.Q (economy), q.R (economy),
                                  q.E_MAT ());
                }
              else if (nargout > 1)
                {
                  math::sparse_qr<SparseComplexMatrix>
                  q (arg.sparse_complex_matrix_value (), 0);
                  retval = ovl (q.Q (economy), q.R (economy));
                }
              else
                {
                  math::sparse_qr<SparseComplexMatrix>
                  q (arg.sparse_complex_matrix_value (), 0);
                  retval = ovl (q.R (economy));
                }
            }
        }
      else
        {
          if (have_b && nargout == 1)
            {
              octave_idx_type info;
              if (args(1).issparse () && ! args(1).iscomplex ())
                retval = ovl (math::sparse_qr<SparseMatrix>::solve
                              <SparseMatrix, SparseMatrix>
                              (arg.sparse_matrix_value (),
                               args (1).sparse_matrix_value (), info));
              else if (! args(1).issparse () && args(1).iscomplex ())
                retval = ovl (math::sparse_qr<SparseMatrix>::solve
                              <MArray<Complex>, ComplexMatrix>
                              (arg.sparse_matrix_value (),
                               args (1).complex_matrix_value (), info));
              else if (! args(1).issparse () && ! args(1).iscomplex ())
                retval = ovl (math::sparse_qr<SparseMatrix>::solve
                              <MArray<double>, Matrix>
                              (arg.sparse_matrix_value (),
                               args (1).matrix_value (), info));
              else if (args(1).issparse () &&  args(1).iscomplex ())
                retval = ovl (math::sparse_qr<SparseMatrix>::solve
                              <SparseComplexMatrix, SparseComplexMatrix>
                              (arg.sparse_matrix_value (),
                               args(1).sparse_complex_matrix_value (),
                               info));
              else
                error ("qr: b is not valid");
            }
          else if (have_b && nargout == 2)
            {
              math::sparse_qr<SparseMatrix>
              q (arg.sparse_matrix_value (), 0);
              retval = ovl (q.C (args(1).matrix_value (), economy),
                            q.R (economy));
            }
          else if (have_b && nargout == 3)
            {
              math::sparse_qr<SparseMatrix>
              q (arg.sparse_matrix_value ());
              if (vector_p)
                retval = ovl (q.C (args(1).matrix_value (), economy),
                              q.R (economy), q.E ());
              else
                retval = ovl (q.C (args(1).matrix_value (), economy),
                              q.R (economy), q.E_MAT ());
            }

          else
            {
              if (nargout > 2)
                {
                  math::sparse_qr<SparseMatrix>
                  q (arg.sparse_matrix_value ());
                  if (vector_p)
                    retval = ovl (q.Q (economy), q.R (economy), q.E ());
                  else
                    retval = ovl (q.Q (economy), q.R (economy),
                                  q.E_MAT ());
                }
              else if (nargout > 1)
                {
                  math::sparse_qr<SparseMatrix>
                  q (arg.sparse_matrix_value (), 0);
                  retval = ovl (q.Q (economy), q.R (economy));
                }
              else
                {
                  math::sparse_qr<SparseMatrix>
                  q (arg.sparse_matrix_value (), 0);
                  retval = ovl (q.R (economy));
                }
            }
        }
    }
  else
    {
      if (have_b && nargout > 2)
        error ("qr: too many output arguments for dense A with B");

      if (arg.is_single_type ())
        {
          if (arg.isreal ())
            {
              math::qr<FloatMatrix>::type type
                = qr_type<FloatMatrix> (nargout, economy);

              FloatMatrix m = arg.float_matrix_value ();

              switch (nargout)
                {
                case 0:
                case 1:
                  {
                    math::qr<FloatMatrix> fact (m, type);
                    retval = ovl (fact.R ());
                  }
                  break;

                case 2:
                  {
                    math::qr<FloatMatrix> fact (m, type);
                    retval = ovl (fact.Q (), get_qr_r (fact));
                    if (have_b)
                      {
                        if (is_cmplx)
                          retval(0) = fact.Q ().transpose ()
                                      * args(1).float_complex_matrix_value ();
                        else
                          retval(0) = fact.Q ().transpose ()
                                      * args(1).float_matrix_value ();
                      }
                  }
                  break;

                default:
                  {
                    math::qrp<FloatMatrix> fact (m, type);

                    if (economy || vector_p)
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.Pvec ());
                    else
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.P ());
                  }
                  break;
                }
            }
          else if (arg.iscomplex ())
            {
              math::qr<FloatComplexMatrix>::type type
                = qr_type<FloatComplexMatrix> (nargout, economy);

              FloatComplexMatrix m = arg.float_complex_matrix_value ();

              switch (nargout)
                {
                case 0:
                case 1:
                  {
                    math::qr<FloatComplexMatrix> fact (m, type);
                    retval = ovl (fact.R ());
                  }
                  break;

                case 2:
                  {
                    math::qr<FloatComplexMatrix> fact (m, type);
                    retval = ovl (fact.Q (), get_qr_r (fact));
                    if (have_b)
                      retval(0) = conj (fact.Q ().transpose ())
                                  * args(1).float_complex_matrix_value ();
                  }
                  break;

                default:
                  {
                    math::qrp<FloatComplexMatrix> fact (m, type);
                    if (economy || vector_p)
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.Pvec ());
                    else
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.P ());
                  }
                  break;
                }
            }
        }
      else
        {
          if (arg.isreal ())
            {
              math::qr<Matrix>::type type
                = qr_type<Matrix> (nargout, economy);

              Matrix m = arg.matrix_value ();

              switch (nargout)
                {
                case 0:
                case 1:
                  {
                    math::qr<Matrix> fact (m, type);
                    retval = ovl (fact.R ());
                  }
                  break;

                case 2:
                  {
                    math::qr<Matrix> fact (m, type);
                    retval = ovl (fact.Q (), get_qr_r (fact));
                    if (have_b)
                      {
                        if (is_cmplx)
                          retval(0) = fact.Q ().transpose ()
                                      * args(1).complex_matrix_value ();
                        else
                          retval(0) = fact.Q ().transpose ()
                                      * args(1).matrix_value ();
                      }
                  }
                  break;

                default:
                  {
                    math::qrp<Matrix> fact (m, type);
                    if (economy || vector_p)
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.Pvec ());
                    else
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.P ());
                  }
                  break;
                }
            }
          else if (arg.iscomplex ())
            {
              math::qr<ComplexMatrix>::type type
                = qr_type<ComplexMatrix> (nargout, economy);

              ComplexMatrix m = arg.complex_matrix_value ();

              switch (nargout)
                {
                case 0:
                case 1:
                  {
                    math::qr<ComplexMatrix> fact (m, type);
                    retval = ovl (fact.R ());
                  }
                  break;

                case 2:
                  {
                    math::qr<ComplexMatrix> fact (m, type);
                    retval = ovl (fact.Q (), get_qr_r (fact));
                    if (have_b)
                      retval(0) = conj (fact.Q ().transpose ())
                                  * args(1).complex_matrix_value ();
                  }
                  break;

                default:
                  {
                    math::qrp<ComplexMatrix> fact (m, type);
                    if (economy || vector_p)
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.Pvec ());
                    else
                      retval = ovl (fact.Q (), get_qr_r (fact), fact.P ());
                  }
                  break;
                }
            }
          else
            err_wrong_type_arg ("qr", arg);
        }
    }

  return retval;
}

/*
%!test
%! a = [0, 2, 1; 2, 1, 2];
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%! [qe2, re2] = qr (a, "econ");
%!
%! assert (q * r, a, sqrt (eps));
%! assert (qe * re, a, sqrt (eps));
%! assert (qe2 * re2, a, sqrt (eps));

%!test
%! a = [0, 2, 1; 2, 1, 2];
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%! [qe2, re2] = qr (a, "econ");
%!
%! assert (q * r, a, sqrt (eps));
%! assert (qe * re, a, sqrt (eps));
%! assert (qe2 * re2, a, sqrt (eps));

%!test
%! a = [0, 2, 1; 2, 1, 2];
%!
%! [q, r, p] = qr (a);  # FIXME: not giving right dimensions.
%! [qe, re, pe] = qr (a, 0);
%! [qe2, re2, pe2] = qr (a, "econ");
%!
%! assert (q * r, a * p, sqrt (eps));
%! assert (qe * re, a(:, pe), sqrt (eps));
%! assert (qe2 * re2, a(:, pe2), sqrt (eps));

%!test
%! a = [0, 2; 2, 1; 1, 2];
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%! [qe2, re2] = qr (a, "econ");
%!
%! assert (q * r, a, sqrt (eps));
%! assert (qe * re, a, sqrt (eps));
%! assert (qe2 * re2, a, sqrt (eps));

%!test
%! a = [0, 2; 2, 1; 1, 2];
%!
%! [q, r, p] = qr (a);
%! [qe, re, pe] = qr (a, 0);
%! [qe2, re2, pe2] = qr (a, "econ");
%!
%! assert (q * r, a * p, sqrt (eps));
%! assert (qe * re, a(:, pe), sqrt (eps));
%! assert (qe2 * re2, a(:, pe2), sqrt (eps));

%!test
%! a = [0, 2, 1; 2, 1, 2; 3, 1, 2];
%! b = [1, 3, 2; 1, 1, 0; 3, 0, 2];
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps));
%! assert (q'*b, c, sqrt (eps));

%!test
%! a = [0, 2, i; 2, 1, 2; 3, 1, 2];
%! b = [1, 3, 2; 1, i, 0; 3, 0, 2];
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps));
%! assert (q'*b, c, sqrt (eps));

%!test
%! a = [0, 2, i; 2, 1, 2; 3, 1, 2];
%! b = [1, 3, 2; 1, 1, 0; 3, 0, 2];
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps));
%! assert (q'*b, c, sqrt (eps));

%!test
%! a = [0, 2, 1; 2, 1, 2; 3, 1, 2];
%! b = [1, 3, 2; 1, i, 0; 3, 0, 2];
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps));
%! assert (q'*b, c, sqrt (eps));

## Empty matrices
%!test
%! assert (qr (zeros (0, 0)), zeros (0, 0))
%! assert (qr (zeros (1, 0)), zeros (1, 0))
%! assert (qr (zeros (0, 1)), zeros (0, 1))

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE") <*63069>
%! assert (qr (sparse (0, 0)), sparse (0, 0))
%! assert (qr (sparse (1, 0)), sparse (1, 0))
%! assert (qr (sparse (0, 1)), sparse (0, 1))

%!error qr ()
%!error qr ([1, 2; 3, 4], 0, 2)
%!error <option string must be .*, not "foo"> qr (magic (3), "foo")
%!error <option string must be .*, not "foo"> qr (magic (3), rand (3, 1), "foo")
%!error <too many output arguments for dense A with B>
%! [q, r, p] = qr (rand (3, 2), rand (3, 1));
%!error <too many output arguments for dense A with B>
%! [q, r, p] = qr (rand (3, 2), rand (3, 1), 0);

%!function retval = __testqr (q, r, a, p)
%!  tol = 100* eps (class (q));
%!  retval = 0;
%!  if (nargin == 3)
%!    n1 = norm (q*r - a);
%!    n2 = norm (q'*q - eye (columns (q)));
%!    retval = (n1 < tol && n2 < tol);
%!  else
%!    n1 = norm (q'*q - eye (columns (q)));
%!    retval = (n1 < tol);
%!    if (isvector (p))
%!      n2 = norm (q*r - a(:,p));
%!      retval = (retval && n2 < tol);
%!    else
%!      n2 = norm (q*r - a*p);
%!      retval = (retval && n2 < tol);
%!    endif
%!  endif
%!endfunction

%!test
%! t = ones (24, 1);
%! j = 1;
%!
%! if (false)  # eliminate big matrix tests
%!   a = rand (5000, 20);
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%!   a = a+1i*eps;
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%! endif
%!
%! a = [ ones(1,15); sqrt(eps)*eye(15) ];
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i*eps;
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = [ ones(1,15); sqrt(eps)*eye(15) ];
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i*eps;
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%! a = [ 611   196  -192   407    -8   -52   -49    29
%!       196   899   113  -192   -71   -43    -8   -44
%!      -192   113   899   196    61    49     8    52
%!       407  -192   196   611     8    44    59   -23
%!        -8   -71    61     8   411  -599   208   208
%!       -52   -43    49    44  -599   411   208   208
%!       -49    -8     8    59   208   208    99  -911
%!        29   -44    52   -23   208   208  -911    99 ];
%! [q,r] = qr (a);
%!
%! assert (all (t) && norm (q*r - a) < 5000*eps);

%!test
%! a = single ([0, 2, 1; 2, 1, 2]);
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%!
%! assert (q * r, a, sqrt (eps ("single")));
%! assert (qe * re, a, sqrt (eps ("single")));

%!test
%! a = single ([0, 2, 1; 2, 1, 2]);
%!
%! [q, r, p] = qr (a);  # FIXME: not giving right dimensions.
%! [qe, re, pe] = qr (a, 0);
%!
%! assert (q * r, a * p, sqrt (eps ("single")));
%! assert (qe * re, a(:, pe), sqrt (eps ("single")));

%!test
%! a = single ([0, 2; 2, 1; 1, 2]);
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%!
%! assert (q * r, a, sqrt (eps ("single")));
%! assert (qe * re, a, sqrt (eps ("single")));

%!test
%! a = single ([0, 2; 2, 1; 1, 2]);
%!
%! [q, r, p] = qr (a);
%! [qe, re, pe] = qr (a, 0);
%!
%! assert (q * r, a * p, sqrt (eps ("single")));
%! assert (qe * re, a(:, pe), sqrt (eps ("single")));

%!test
%! a = single ([0, 2, 1; 2, 1, 2; 3, 1, 2]);
%! b = single ([1, 3, 2; 1, 1, 0; 3, 0, 2]);
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps ("single")));
%! assert (q'*b, c, sqrt (eps ("single")));

%!test
%! a = single ([0, 2, i; 2, 1, 2; 3, 1, 2]);
%! b = single ([1, 3, 2; 1, i, 0; 3, 0, 2]);
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps ("single")));
%! assert (q'*b, c, sqrt (eps ("single")));

%!test
%! a = single ([0, 2, i; 2, 1, 2; 3, 1, 2]);
%! b = single ([1, 3, 2; 1, 1, 0; 3, 0, 2]);
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps));
%! assert (q'*b, c, sqrt (eps));

%!test
%! a = single ([0, 2, 1; 2, 1, 2; 3, 1, 2]);
%! b = single ([1, 3, 2; 1, i, 0; 3, 0, 2]);
%!
%! [q, r] = qr (a);
%! [c, re] = qr (a, b);
%!
%! assert (r, re, sqrt (eps ("single")));
%! assert (q'*b, c, sqrt (eps ("single")));

%!test
%! t = ones (24, 1);
%! j = 1;
%!
%! if (false)  # eliminate big matrix tests
%!   a = rand (5000,20);
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%!   a = a+1i* eps ("single");
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%! endif
%!
%! a = [ ones(1,15); sqrt(eps("single"))*eye(15) ];
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i* eps ("single");
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = [ ones(1,15); sqrt(eps("single"))*eye(15) ];
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i* eps ("single");
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a',p);
%!
%! a = [ 611   196  -192   407    -8   -52   -49    29
%!       196   899   113  -192   -71   -43    -8   -44
%!      -192   113   899   196    61    49     8    52
%!       407  -192   196   611     8    44    59   -23
%!        -8   -71    61     8   411  -599   208   208
%!       -52   -43    49    44  -599   411   208   208
%!       -49    -8     8    59   208   208    99  -911
%!        29   -44    52   -23   208   208  -911    99 ];
%! [q,r] = qr (a);
%!
%! assert (all (t) && norm (q*r-a) < 5000* eps ("single"));

## The deactivated tests below can't be tested till rectangular back-subs is
## implemented for sparse matrices.

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (n,n,d) + speye (n,n);
%! r = qr (a);
%! assert (r'*r, a'*a, 1e-10);

%!testif HAVE_COLAMD; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (n,n,d) + speye (n,n);
%! q = symamd (a);
%! a = a(q,q);
%! r = qr (a);
%! assert (r'*r, a'*a, 1e-10);

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (n,n,d) + speye (n,n);
%! [c,r] = qr (a, ones (n,1));
%! assert (r\c, full (a)\ones (n,1), 10e-10);

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (n,n,d) + speye (n,n);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10);

## Test under-determined systems!!
%!#testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (n,n+1,d) + speye (n,n+1);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10);

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = 1i* sprandn (n,n,d) + speye (n,n);
%! r = qr (a);
%! assert (r'*r,a'*a,1e-10);

%!testif HAVE_COLAMD; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = 1i* sprandn (n,n,d) + speye (n,n);
%! q = symamd (a);
%! a = a(q,q);
%! r = qr (a);
%! assert (r'*r, a'*a, 1e-10);

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = 1i* sprandn (n,n,d) + speye (n,n);
%! [c,r] = qr (a, ones (n,1));
%! assert (r\c, full (a)\ones (n,1), 10e-10);

%!testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = 1i* sprandn (n,n,d) + speye (n,n);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10);

## Test under-determined systems!!
%!#testif ; (__have_feature__ ("SPQR") && __have_feature__ ("CHOLMOD")) || __have_feature__ ("CXSPARSE")
%! n = 20;  d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = 1i* sprandn (n,n+1,d) + speye (n,n+1);
%! b = randn (n, 2);
%! [c, r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = randn (m, 2);
%! [c, r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = sprandn (m, 2, d);
%! [c, r] = qr (a, b, 0);
%! [c2, r2] = qr (full (a), full (b), 0);
%! assert (r\c, r2\c2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = randn (m, 2);
%! [c, r, p] = qr (a, b, "matrix");
%! x = p * (r\c);
%! [c2, r2] = qr (full (a), b);
%! x2 = r2 \ c2;
%! assert (x, x2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! [q, r, p] = qr (a, "matrix");
%! assert (q * r, a * p, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = randn (m, 2);
%! x = qr (a, b);
%! [c2, r2] = qr (full (a), b);
%! assert (x, r2\c2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = i * randn (m, 2);
%! x = qr (a, b);
%! [c2, r2] = qr (full (a), b);
%! assert (x, r2\c2, 10e-10);

%!#testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = i * randn (m, 2);
%! [c, r] = qr (a, b);
%! [c2, r2] = qr (full (a), b);
%! assert (r\c, r2\c2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = sprandn (m, n, d);
%! b = i * randn (m, 2);
%! [c, r, p] = qr (a, b, "matrix");
%! x = p * (r\c);
%! [c2, r2] = qr (full (a), b);
%! x2 = r2 \ c2;
%! assert (x, x2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = i * sprandn (m, n, d);
%! b = sprandn (m, 2, d);
%! [c, r] = qr (a, b, 0);
%! [c2, r2] = qr (full (a), full (b), 0);
%! assert (r\c, r2\c2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = i * sprandn (m, n, d);
%! b = randn (m, 2);
%! [c, r, p] = qr (a, b, "matrix");
%! x = p * (r\c);
%! [c2, r2] = qr (full (a), b);
%! x2 = r2 \ c2;
%! assert(x, x2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = i * sprandn (m, n, d);
%! [q, r, p] = qr (a, "matrix");
%! assert(q * r, a * p, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! n = 12; m = 20; d = 0.2;
%! ## initialize generators to make behavior reproducible
%! rand ("state", 42);
%! randn ("state", 42);
%! a = i * sprandn (m, n, d);
%! b = randn (m, 2);
%! x = qr (a, b);
%! [c2, r2] = qr (full (a), b);
%! assert (x, r2\c2, 10e-10);

%!testif HAVE_SPQR, HAVE_CHOLMOD
%! a = sparse (5, 6);
%! a(3,1) = 0.8;
%! a(2,2) = 1.4;
%! a(1,6) = -0.5;
%! r = qr (a);
%! assert (r'*r, a'*a, 10e-10);
*/

static
bool check_qr_dims (const octave_value& q, const octave_value& r,
                    bool allow_ecf = false)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();
  return ((q.ndims () == 2 && r.ndims () == 2 && k == q.columns ())
          && (m == k || (allow_ecf && k == n && k < m)));
}

static
bool check_index (const octave_value& i, bool vector_allowed = false)
{
  return ((i.isreal () || i.isinteger ())
          && (i.is_scalar_type () || vector_allowed));
}

DEFUN (qrupdate, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{Q1}, @var{R1}] =} qrupdate (@var{Q}, @var{R}, @var{u}, @var{v})
Update a QR factorization given update vectors or matrices.

Given a QR@tie{}factorization of a real or complex matrix
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization of
@w{@var{A} + @var{u}*@var{v}'}, where @var{u} and @var{v} are column vectors
(rank-1 update) or matrices with equal number of columns
(rank-k update).  Notice that the latter case is done as a sequence of
rank-1 updates; thus, for k large enough, it will be both faster and more
accurate to recompute the factorization from scratch.

The QR@tie{}factorization supplied may be either full (Q is square) or
economized (R is square).

@seealso{qr, qrinsert, qrdelete, qrshift}
@end deftypefn */)
{
  octave_value_list retval;

  if (args.length () != 4)
    print_usage ();

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argu = args(2);
  octave_value argv = args(3);

  if (! argq.isnumeric () || ! argr.isnumeric ()
      || ! argu.isnumeric () || ! argv.isnumeric ())
    print_usage ();

  if (! check_qr_dims (argq, argr, true))
    error ("qrupdate: Q and R dimensions don't match");

  if (argq.isreal () && argr.isreal () && argu.isreal ()
      && argv.isreal ())
    {
      // all real case
      if (argq.is_single_type () || argr.is_single_type ()
          || argu.is_single_type () || argv.is_single_type ())
        {
          FloatMatrix Q = argq.float_matrix_value ();
          FloatMatrix R = argr.float_matrix_value ();
          FloatMatrix u = argu.float_matrix_value ();
          FloatMatrix v = argv.float_matrix_value ();

          math::qr<FloatMatrix> fact (Q, R);
          fact.update (u, v);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          Matrix Q = argq.matrix_value ();
          Matrix R = argr.matrix_value ();
          Matrix u = argu.matrix_value ();
          Matrix v = argv.matrix_value ();

          math::qr<Matrix> fact (Q, R);
          fact.update (u, v);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }
  else
    {
      // complex case
      if (argq.is_single_type () || argr.is_single_type ()
          || argu.is_single_type () || argv.is_single_type ())
        {
          FloatComplexMatrix Q = argq.float_complex_matrix_value ();
          FloatComplexMatrix R = argr.float_complex_matrix_value ();
          FloatComplexMatrix u = argu.float_complex_matrix_value ();
          FloatComplexMatrix v = argv.float_complex_matrix_value ();

          math::qr<FloatComplexMatrix> fact (Q, R);
          fact.update (u, v);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          ComplexMatrix Q = argq.complex_matrix_value ();
          ComplexMatrix R = argr.complex_matrix_value ();
          ComplexMatrix u = argu.complex_matrix_value ();
          ComplexMatrix v = argv.complex_matrix_value ();

          math::qr<ComplexMatrix> fact (Q, R);
          fact.update (u, v);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }

  return retval;
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
%!      0.589077 + 0.658457i  0.013205 + 0.279323i  0.229284 + 0.721929i;
%!      0.092758 + 0.345687i  0.928679 + 0.241052i  0.764536 + 0.832406i;
%!      0.912098 + 0.721024i  0.049018 + 0.269452i  0.730029 + 0.796517i;
%!      0.112849 + 0.603871i  0.486352 + 0.142337i  0.355646 + 0.151496i ];
%!
%! uc = [0.20351 + 0.05401i;
%!      0.13141 + 0.43708i;
%!      0.29808 + 0.08789i;
%!      0.69821 + 0.38844i;
%!      0.74871 + 0.25821i ];
%!
%! vc = [0.85839 + 0.29468i;
%!      0.20820 + 0.93090i;
%!      0.86184 + 0.34689i ];
%!

%!test
%! [Q,R] = qr (A);
%! [Q,R] = qrupdate (Q, R, u, v);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - A - u*v'), Inf) < norm (A)*1e1*eps);
%!
%!test
%! [Q,R] = qr (Ac);
%! [Q,R] = qrupdate (Q, R, uc, vc);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - Ac - uc*vc'), Inf) < norm (Ac)*1e1*eps);

%!test
%! [Q,R] = qr (single (A));
%! [Q,R] = qrupdate (Q, R, single (u), single (v));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - single (A) - single (u)* single (v)'), Inf)
%!         < norm (single (A))*1e1* eps ("single"));
%!
%!test
%! [Q,R] = qr (single (Ac));
%! [Q,R] = qrupdate (Q, R, single (uc), single (vc));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - single (Ac) - single (uc)* single (vc)'), Inf)
%!         < norm (single (Ac))*1e1* eps ("single"));
*/

DEFUN (qrinsert, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{Q1}, @var{R1}] =} qrinsert (@var{Q}, @var{R}, @var{j}, @var{x}, @var{orient})
Update a QR factorization given a row or column to insert in the original
factored matrix.


Given a QR@tie{}factorization of a real or complex matrix
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization of
@w{[A(:,1:j-1) x A(:,j:n)]}, where @var{u} is a column vector to be inserted
into @var{A} (if @var{orient} is @qcode{"col"}), or the
QR@tie{}factorization of @w{[A(1:j-1,:);x;A(:,j:n)]}, where @var{x} is a row
vector to be inserted into @var{A} (if @var{orient} is @qcode{"row"}).

The default value of @var{orient} is @qcode{"col"}.  If @var{orient} is
@qcode{"col"}, @var{u} may be a matrix and @var{j} an index vector
resulting in the QR@tie{}factorization of a matrix @var{B} such that
@w{B(:,@var{j})} gives @var{u} and @w{B(:,@var{j}) = []} gives @var{A}.
Notice that the latter case is done as a sequence of k insertions;
thus, for k large enough, it will be both faster and more accurate to
recompute the factorization from scratch.

If @var{orient} is @qcode{"col"}, the QR@tie{}factorization supplied may
be either full (Q is square) or economized (R is square).

If @var{orient} is @qcode{"row"}, full factorization is needed.
@seealso{qr, qrupdate, qrdelete, qrshift}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 4 || nargin > 5)
    print_usage ();

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argj = args(2);
  octave_value argx = args(3);

  if (! argq.isnumeric () || ! argr.isnumeric ()
      || ! argx.isnumeric ()
      || (nargin > 4 && ! args(4).is_string ()))
    print_usage ();

  std::string orient = (nargin < 5) ? "col" : args(4).string_value ();
  bool col = (orient == "col");

  if (! col && orient != "row")
    error (R"(qrinsert: ORIENT must be "col" or "row")");

  if (! check_qr_dims (argq, argr, col) || (! col && argx.rows () != 1))
    error ("qrinsert: dimension mismatch");

  if (! check_index (argj, col))
    error ("qrinsert: invalid index J");

  octave_value_list retval;

  MArray<octave_idx_type> j = argj.octave_idx_type_vector_value ();

  octave_idx_type one = 1;

  if (argq.isreal () && argr.isreal () && argx.isreal ())
    {
      // real case
      if (argq.is_single_type () || argr.is_single_type ()
          || argx.is_single_type ())
        {
          FloatMatrix Q = argq.float_matrix_value ();
          FloatMatrix R = argr.float_matrix_value ();
          FloatMatrix x = argx.float_matrix_value ();

          math::qr<FloatMatrix> fact (Q, R);

          if (col)
            fact.insert_col (x, j-one);
          else
            fact.insert_row (x.row (0), j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          Matrix Q = argq.matrix_value ();
          Matrix R = argr.matrix_value ();
          Matrix x = argx.matrix_value ();

          math::qr<Matrix> fact (Q, R);

          if (col)
            fact.insert_col (x, j-one);
          else
            fact.insert_row (x.row (0), j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }
  else
    {
      // complex case
      if (argq.is_single_type () || argr.is_single_type ()
          || argx.is_single_type ())
        {
          FloatComplexMatrix Q = argq.float_complex_matrix_value ();
          FloatComplexMatrix R = argr.float_complex_matrix_value ();
          FloatComplexMatrix x = argx.float_complex_matrix_value ();

          math::qr<FloatComplexMatrix> fact (Q, R);

          if (col)
            fact.insert_col (x, j-one);
          else
            fact.insert_row (x.row (0), j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          ComplexMatrix Q = argq.complex_matrix_value ();
          ComplexMatrix R = argr.complex_matrix_value ();
          ComplexMatrix x = argx.complex_matrix_value ();

          math::qr<ComplexMatrix> fact (Q, R);

          if (col)
            fact.insert_col (x, j-one);
          else
            fact.insert_row (x.row (0), j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }

  return retval;
}

/*
%!test
%! [Q,R] = qr (A);
%! [Q,R] = qrinsert (Q, R, 3, u);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [A(:,1:2) u A(:,3)]), Inf) < norm (A)*1e1*eps);
%!test
%! [Q,R] = qr (Ac);
%! [Q,R] = qrinsert (Q, R, 3, uc);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [Ac(:,1:2) uc Ac(:,3)]), Inf) < norm (Ac)*1e1*eps);
%!test
%! x = [0.85082  0.76426  0.42883 ];
%!
%! [Q,R] = qr (A);
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [A(1:2,:);x;A(3:5,:)]), Inf) < norm (A)*1e1*eps);
%!test
%! x = [0.20351 + 0.05401i  0.13141 + 0.43708i  0.29808 + 0.08789i ];
%!
%! [Q,R] = qr (Ac);
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [Ac(1:2,:);x;Ac(3:5,:)]), Inf) < norm (Ac)*1e1*eps);

%!test
%! [Q,R] = qr (single (A));
%! [Q,R] = qrinsert (Q, R, 3, single (u));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([A(:,1:2) u A(:,3)])), Inf)
%!         < norm (single (A))*1e1* eps ("single"));
%!test
%! [Q,R] = qr (single (Ac));
%! [Q,R] = qrinsert (Q, R, 3, single (uc));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([Ac(:,1:2) uc Ac(:,3)])), Inf)
%!         < norm (single (Ac))*1e1* eps ("single"));
%!test
%! x = single ([0.85082  0.76426  0.42883 ]);
%!
%! [Q,R] = qr (single (A));
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([A(1:2,:);x;A(3:5,:)])), Inf)
%!         < norm (single (A))*1e1* eps ("single"));
%!test
%! x = single ([0.20351 + 0.05401i  0.13141 + 0.43708i  0.29808 + 0.08789i ]);
%!
%! [Q,R] = qr (single (Ac));
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([Ac(1:2,:);x;Ac(3:5,:)])), Inf)
%!         < norm (single (Ac))*1e1* eps ("single"));
*/

DEFUN (qrdelete, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{Q1}, @var{R1}] =} qrdelete (@var{Q}, @var{R}, @var{j}, @var{orient})
Update a QR factorization given a row or column to delete from the original
factored matrix.

Given a QR@tie{}factorization of a real or complex matrix
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization of
@w{[A(:,1:j-1), U, A(:,j:n)]},
where @var{u} is a column vector to be inserted into @var{A}
(if @var{orient} is @qcode{"col"}),
or the QR@tie{}factorization of @w{[A(1:j-1,:);X;A(:,j:n)]},
where @var{x} is a row @var{orient} is @qcode{"row"}).
The default value of @var{orient} is @qcode{"col"}.

If @var{orient} is @qcode{"col"}, @var{j} may be an index vector
resulting in the QR@tie{}factorization of a matrix @var{B} such that
@w{A(:,@var{j}) = []} gives @var{B}.  Notice that the latter case is done as
a sequence of k deletions; thus, for k large enough, it will be both faster
and more accurate to recompute the factorization from scratch.

If @var{orient} is @qcode{"col"}, the QR@tie{}factorization supplied may
be either full (Q is square) or economized (R is square).

If @var{orient} is @qcode{"row"}, full factorization is needed.
@seealso{qr, qrupdate, qrinsert, qrshift}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 3 || nargin > 4)
    print_usage ();

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argj = args(2);

  if (! argq.isnumeric () || ! argr.isnumeric ()
      || (nargin > 3 && ! args(3).is_string ()))
    print_usage ();

  std::string orient = (nargin < 4) ? "col" : args(3).string_value ();
  bool col = orient == "col";

  if (! col && orient != "row")
    error (R"(qrdelete: ORIENT must be "col" or "row")");

  if (! check_qr_dims (argq, argr, col))
    error ("qrdelete: dimension mismatch");

  MArray<octave_idx_type> j = argj.octave_idx_type_vector_value ();
  if (! check_index (argj, col))
    error ("qrdelete: invalid index J");

  octave_value_list retval;

  octave_idx_type one = 1;

  if (argq.isreal () && argr.isreal ())
    {
      // real case
      if (argq.is_single_type () || argr.is_single_type ())
        {
          FloatMatrix Q = argq.float_matrix_value ();
          FloatMatrix R = argr.float_matrix_value ();

          math::qr<FloatMatrix> fact (Q, R);

          if (col)
            fact.delete_col (j-one);
          else
            fact.delete_row (j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          Matrix Q = argq.matrix_value ();
          Matrix R = argr.matrix_value ();

          math::qr<Matrix> fact (Q, R);

          if (col)
            fact.delete_col (j-one);
          else
            fact.delete_row (j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }
  else
    {
      // complex case
      if (argq.is_single_type () || argr.is_single_type ())
        {
          FloatComplexMatrix Q = argq.float_complex_matrix_value ();
          FloatComplexMatrix R = argr.float_complex_matrix_value ();

          math::qr<FloatComplexMatrix> fact (Q, R);

          if (col)
            fact.delete_col (j-one);
          else
            fact.delete_row (j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          ComplexMatrix Q = argq.complex_matrix_value ();
          ComplexMatrix R = argr.complex_matrix_value ();

          math::qr<ComplexMatrix> fact (Q, R);

          if (col)
            fact.delete_col (j-one);
          else
            fact.delete_row (j(0)-one);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }

  return retval;
}

/*
%!test
%! AA = [0.091364  0.613038  0.027504  0.999083;
%!       0.594638  0.425302  0.562834  0.603537;
%!       0.383594  0.291238  0.742073  0.085574;
%!       0.265712  0.268003  0.783553  0.238409;
%!       0.669966  0.743851  0.457255  0.445057 ];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 16*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = [0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!       0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!       0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!       0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!       0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ] * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 16*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = [0.091364  0.613038  0.027504  0.999083;
%!       0.594638  0.425302  0.562834  0.603537;
%!       0.383594  0.291238  0.742073  0.085574;
%!       0.265712  0.268003  0.783553  0.238409;
%!       0.669966  0.743851  0.457255  0.445057 ];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = [0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!       0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!       0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!       0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!       0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ] * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps);

%!test
%! AA = single ([0.091364  0.613038  0.027504  0.999083;
%!               0.594638  0.425302  0.562834  0.603537;
%!               0.383594  0.291238  0.742073  0.085574;
%!               0.265712  0.268003  0.783553  0.238409;
%!               0.669966  0.743851  0.457255  0.445057 ]);
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf)
%!         < norm (AA)*1e1* eps ("single"));
%!
%!test
%! AA = single ([0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!               0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!               0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!               0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!               0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ]) * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf)
%!         < norm (AA)*1e1* eps ("single"));

%!test
%! AA = single ([0.091364  0.613038  0.027504  0.999083;
%!               0.594638  0.425302  0.562834  0.603537;
%!               0.383594  0.291238  0.742073  0.085574;
%!               0.265712  0.268003  0.783553  0.238409;
%!               0.669966  0.743851  0.457255  0.445057 ]);
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4,"single")), Inf) < 1.5e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf)
%!         < norm (AA)*1e1* eps ("single"));
%!testif HAVE_QRUPDATE
%! ## Same test as above but with more precicision
%! AA = single ([0.091364  0.613038  0.027504  0.999083;
%!               0.594638  0.425302  0.562834  0.603537;
%!               0.383594  0.291238  0.742073  0.085574;
%!               0.265712  0.268003  0.783553  0.238409;
%!               0.669966  0.743851  0.457255  0.445057 ]);
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf)
%!         < norm (AA)*1e1* eps ("single"));
%!
%!test
%! AA = single ([0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!              0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!              0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!              0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!              0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ]) * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf)
%!         < norm (AA)*1e1* eps ("single"));
*/

DEFUN (qrshift, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{Q1}, @var{R1}] =} qrshift (@var{Q}, @var{R}, @var{i}, @var{j})
Update a QR factorization given a range of columns to shift in the original
factored matrix.

Given a QR@tie{}factorization of a real or complex matrix
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization
of @w{@var{A}(:,p)}, where @w{p} is the permutation @*
@code{p = [1:i-1, shift(i:j, 1), j+1:n]} if @w{@var{i} < @var{j}} @*
 or @*
@code{p = [1:j-1, shift(j:i,-1), i+1:n]} if @w{@var{j} < @var{i}}.  @*

@seealso{qr, qrupdate, qrinsert, qrdelete}
@end deftypefn */)
{
  if (args.length () != 4)
    print_usage ();

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argi = args(2);
  octave_value argj = args(3);

  if (! argq.isnumeric () || ! argr.isnumeric ())
    print_usage ();

  if (! check_qr_dims (argq, argr, true))
    error ("qrshift: dimensions mismatch");

  octave_idx_type i = argi.idx_type_value ();
  octave_idx_type j = argj.idx_type_value ();

  if (! check_index (argi) || ! check_index (argj))
    error ("qrshift: invalid index I or J");

  octave_value_list retval;

  if (argq.isreal () && argr.isreal ())
    {
      // all real case
      if (argq.is_single_type ()
          && argr.is_single_type ())
        {
          FloatMatrix Q = argq.float_matrix_value ();
          FloatMatrix R = argr.float_matrix_value ();

          math::qr<FloatMatrix> fact (Q, R);
          fact.shift_cols (i-1, j-1);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          Matrix Q = argq.matrix_value ();
          Matrix R = argr.matrix_value ();

          math::qr<Matrix> fact (Q, R);
          fact.shift_cols (i-1, j-1);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }
  else
    {
      // complex case
      if (argq.is_single_type ()
          && argr.is_single_type ())
        {
          FloatComplexMatrix Q = argq.float_complex_matrix_value ();
          FloatComplexMatrix R = argr.float_complex_matrix_value ();

          math::qr<FloatComplexMatrix> fact (Q, R);
          fact.shift_cols (i-1, j-1);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
      else
        {
          ComplexMatrix Q = argq.complex_matrix_value ();
          ComplexMatrix R = argr.complex_matrix_value ();

          math::qr<ComplexMatrix> fact (Q, R);
          fact.shift_cols (i-1, j-1);

          retval = ovl (fact.Q (), get_qr_r (fact));
        }
    }

  return retval;
}

/*
%!test
%! AA = A.';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = Ac.';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);

%!test
%! AA = single (A).';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1* eps ("single"));
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1* eps ("single"));
%!
%!test
%! AA = single (Ac).';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1* eps ("single"));
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1* eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1* eps ("single"));
*/

OCTAVE_END_NAMESPACE(octave)
