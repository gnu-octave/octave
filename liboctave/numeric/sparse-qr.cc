////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

#include "CMatrix.h"
#include "CSparse.h"
#include "MArray.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dSparse.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"
#include "quit.h"
#include "sparse-qr.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

#if defined (HAVE_CXSPARSE)
template <typename SPARSE_T>
class
cxsparse_types
{ };

template <>
class
cxsparse_types<SparseMatrix>
{
public:
  typedef CXSPARSE_DNAME (s) symbolic_type;
  typedef CXSPARSE_DNAME (n) numeric_type;
};

template <>
class
cxsparse_types<SparseComplexMatrix>
{
public:
  typedef CXSPARSE_ZNAME (s) symbolic_type;
  typedef CXSPARSE_ZNAME (n) numeric_type;
};
#endif

template <typename SPARSE_T>
class sparse_qr<SPARSE_T>::sparse_qr_rep
{
public:

  sparse_qr_rep (const SPARSE_T& a, int order);

  // No copying!

  sparse_qr_rep (const sparse_qr_rep&) = delete;

  sparse_qr_rep& operator = (const sparse_qr_rep&) = delete;

  ~sparse_qr_rep (void);

  bool ok (void) const
  {
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
    return (m_H && m_Htau && m_HPinv && m_R && m_E);
#elif defined (HAVE_CXSPARSE)
    return (N && S);
#else
    return false;
#endif
  }

  SPARSE_T V (void) const;

  ColumnVector Pinv (void) const;

  ColumnVector P (void) const;

  ColumnVector E (void) const;

  SPARSE_T R (bool econ) const;

  typename SPARSE_T::dense_matrix_type
  C (const typename SPARSE_T::dense_matrix_type& b, bool econ = false);

  typename SPARSE_T::dense_matrix_type Q (bool econ = false);

  octave_idx_type nrows;
  octave_idx_type ncols;

#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  template <typename RHS_T, typename RET_T>
  RET_T solve (const RHS_T& b, octave_idx_type& info) const;

#endif

#if defined (HAVE_CXSPARSE)

  typename cxsparse_types<SPARSE_T>::symbolic_type *S;
  typename cxsparse_types<SPARSE_T>::numeric_type *N;

#endif

  template <typename RHS_T, typename RET_T>
  RET_T tall_solve (const RHS_T& b, octave_idx_type& info);

  template <typename RHS_T, typename RET_T>
  RET_T wide_solve (const RHS_T& b, octave_idx_type& info) const;

#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

private:

  cholmod_common m_cc;
  cholmod_sparse *m_R;  // R factor
                        // Column permutation for A. Fill-reducing ordering.
  SuiteSparse_long *m_E;
  cholmod_sparse *m_H;  // Householder vectors
  cholmod_dense *m_Htau;  // beta scalars
  SuiteSparse_long *m_HPinv;

#endif
};

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::sparse_qr_rep::Pinv (void) const
{
#if defined (HAVE_CXSPARSE)

  ColumnVector ret (N->L->m);

  for (octave_idx_type i = 0; i < N->L->m; i++)
    ret.xelem (i) = S->pinv[i];

  return ret;

#else

  return ColumnVector ();

#endif
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::sparse_qr_rep::P (void) const
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  ColumnVector ret (nrows);

  // FIXME: Is ret.xelem (m_HPinv[i]) = i + 1 correct?
  for (octave_idx_type i = 0; i < nrows; i++)
    ret.xelem (from_suitesparse_long (m_HPinv[i])) = i + 1;

  return ret;

#elif defined (HAVE_CXSPARSE)

  ColumnVector ret (N->L->m);

  for (octave_idx_type i = 0; i < N->L->m; i++)
    ret.xelem (S->pinv[i]) = i;

  return ret;

#else

  return ColumnVector ();

#endif
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::sparse_qr_rep::E (void) const
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  ColumnVector ret (ncols);

  if (m_E)
    for (octave_idx_type i = 0; i < ncols; i++)
      ret(i) = from_suitesparse_long (m_E[i]) + 1;
  else
    for (octave_idx_type i = 0; i < ncols; i++)
      ret(i) = i + 1;

  return ret;

#else

  return ColumnVector ();

#endif
}

#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

// Convert real sparse octave matrix to real sparse cholmod matrix.
// Returns a "shallow" copy of a.
static cholmod_sparse
ros2rcs (const SparseMatrix& a)
{
  cholmod_sparse A;

  octave_idx_type ncols = a.cols ();
  octave_idx_type nnz = a.nnz ();

  A.ncol = ncols;
  A.nrow = a.rows ();
  A.itype = CHOLMOD_LONG;
  A.nzmax = nnz;
  A.sorted = 0;
  A.packed = 1;
  A.stype = 0;
  A.xtype = CHOLMOD_REAL;
  A.dtype = CHOLMOD_DOUBLE;
  A.nz = nullptr;
  A.z = nullptr;
  if (sizeof (octave_idx_type) == sizeof (SuiteSparse_long))
    {
      A.p = reinterpret_cast<SuiteSparse_long *> (a.cidx ());
      A.i = reinterpret_cast<SuiteSparse_long *> (a.ridx ());
    }
  else
    {
      SuiteSparse_long *A_p;
      A_p = new SuiteSparse_long[ncols+1];
      for (octave_idx_type i = 0; i < ncols+1; i++)
        A_p[i] = a.cidx (i);
      A.p = A_p;
      SuiteSparse_long *A_i;
      A_i = new SuiteSparse_long[nnz];
      for (octave_idx_type i = 0; i < nnz; i++)
        A_i[i] = a.ridx (i);
      A.i = A_i;
    }
  A.x = const_cast<double *> (a.data ());

  return A;
}

// Convert complex sparse octave matrix to complex sparse cholmod matrix.
// Returns a "shallow" copy of a.
static cholmod_sparse
cos2ccs (const SparseComplexMatrix& a)
{
  cholmod_sparse A;

  octave_idx_type ncols = a.cols ();
  octave_idx_type nnz = a.nnz ();

  A.ncol = ncols;
  A.nrow = a.rows ();
  A.itype = CHOLMOD_LONG;
  A.nzmax = nnz;
  A.sorted = 0;
  A.packed = 1;
  A.stype = 0;
  A.xtype = CHOLMOD_COMPLEX;
  A.dtype = CHOLMOD_DOUBLE;
  A.nz = nullptr;
  A.z = nullptr;
  if (sizeof (octave_idx_type) == sizeof (SuiteSparse_long))
    {
      A.p = reinterpret_cast<SuiteSparse_long *> (a.cidx ());
      A.i = reinterpret_cast<SuiteSparse_long *> (a.ridx ());
    }
  else
    {
      SuiteSparse_long *A_p;
      A_p = new SuiteSparse_long[ncols+1];
      for (octave_idx_type i = 0; i < ncols+1; i++)
        A_p[i] = a.cidx (i);
      A.p = A_p;
      SuiteSparse_long *A_i;
      A_i = new SuiteSparse_long[nnz];
      for (octave_idx_type i = 0; i < nnz; i++)
        A_i[i] = a.ridx (i);
      A.i = A_i;
    }
  A.x = const_cast<Complex *>
        (reinterpret_cast<const Complex *> (a.data ()));

  return A;
}

// Convert real dense octave matrix to complex dense cholmod matrix.
// Returns a "deep" copy of a.
static cholmod_dense *
rod2ccd (const MArray<double>& a, cholmod_common *cc1)
{
  cholmod_dense *A
    = cholmod_l_allocate_dense (a.rows (), a.cols (), a.rows(),
                                CHOLMOD_COMPLEX, cc1);

  const double *a_x = a.data ();

  Complex *A_x = reinterpret_cast<Complex *> (A->x);
  for (octave_idx_type j = 0; j < a.cols() * a.rows() ; j++)
    A_x[j] = Complex (a_x[j], 0.0);

  return A;
}

// Convert real dense octave matrix to real dense cholmod matrix.
// Returns a "shallow" copy of a.
static cholmod_dense
rod2rcd (const MArray<double>& a)
{
  cholmod_dense A;

  A.ncol = a.cols ();
  A.nrow = a.rows ();
  A.nzmax = a.cols () * a.rows ();
  A.xtype = CHOLMOD_REAL;
  A.dtype = CHOLMOD_DOUBLE;
  A.z = nullptr;
  A.d = a.rows ();
  A.x = const_cast<double *> (a.data ());

  return A;
}

// Convert complex dense octave matrix to complex dense cholmod matrix.
// Returns a "shallow" copy of a.
static cholmod_dense
cod2ccd (const ComplexMatrix& a)
{
  cholmod_dense A;

  A.ncol = a.cols ();
  A.nrow = a.rows ();
  A.nzmax = a.cols () * a.rows ();
  A.xtype = CHOLMOD_COMPLEX;
  A.dtype = CHOLMOD_DOUBLE;
  A.z = nullptr;
  A.d = a.rows ();
  A.x = const_cast<Complex *> (reinterpret_cast<const Complex *> (a.data ()));

  return A;
}

// Convert real sparse cholmod matrix to real sparse octave matrix.
// Returns a "shallow" copy of y.
static SparseMatrix
rcs2ros (const cholmod_sparse *y)
{
  octave_idx_type nrow = from_size_t (y->nrow);
  octave_idx_type ncol = from_size_t (y->ncol);
  octave_idx_type nz = from_size_t (y->nzmax);
  SparseMatrix ret (nrow, ncol, nz);

  SuiteSparse_long *y_p = reinterpret_cast<SuiteSparse_long *> (y->p);
  for (octave_idx_type j = 0; j < ncol + 1; j++)
    ret.xcidx (j) = from_suitesparse_long (y_p[j]);

  SuiteSparse_long *y_i = reinterpret_cast<SuiteSparse_long *> (y->i);
  double *y_x = reinterpret_cast<double *> (y->x);
  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = from_suitesparse_long (y_i[j]);
      ret.xdata (j) = y_x[j];
    }

  return ret;
}

// Convert complex sparse cholmod matrix to complex sparse octave matrix.
// Returns a "deep" copy of a.
static SparseComplexMatrix
ccs2cos (const cholmod_sparse *a)
{
  octave_idx_type nrow = from_size_t (a->nrow);
  octave_idx_type ncol = from_size_t (a->ncol);
  octave_idx_type nz = from_size_t (a->nzmax);
  SparseComplexMatrix ret (nrow, ncol, nz);

  SuiteSparse_long *a_p = reinterpret_cast<SuiteSparse_long *> (a->p);
  for (octave_idx_type j = 0; j < ncol + 1; j++)
    ret.xcidx(j) = from_suitesparse_long (a_p[j]);

  SuiteSparse_long *a_i = reinterpret_cast<SuiteSparse_long *> (a->i);
  Complex *a_x = reinterpret_cast<Complex *> (a->x);
  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx(j) = from_suitesparse_long (a_i[j]);
      ret.xdata(j) = a_x[j];
    }

  return ret;
}

// Convert real sparse octave matrix to complex sparse cholmod matrix.
// Returns a "deep" copy of a.
static cholmod_sparse *
ros2ccs (const SparseMatrix& a, cholmod_common *cc)
{
  cholmod_sparse *A
    = cholmod_l_allocate_sparse (a.rows (), a.cols (), a.nnz (), 0, 1, 0,
                                 CHOLMOD_COMPLEX, cc);

  octave_idx_type ncol = a.cols ();
  SuiteSparse_long *A_p = reinterpret_cast<SuiteSparse_long *> (A->p);
  for (octave_idx_type j = 0; j < ncol + 1; j++)
    A_p[j] = a.cidx(j);

  const double *a_x = a.data ();
  Complex *A_x = reinterpret_cast<Complex *> (A->x);
  SuiteSparse_long *A_i = reinterpret_cast<SuiteSparse_long *> (A->i);
  for (octave_idx_type j = 0; j < a.nnz (); j++)
    {
      A_x[j] = Complex (a_x[j], 0.0);
      A_i[j] = a.ridx(j);
    }
  return A;
}

static suitesparse_integer
suitesparse_long_to_suitesparse_integer (SuiteSparse_long x)
{
  if (x < std::numeric_limits<suitesparse_integer>::min ()
      || x > std::numeric_limits<suitesparse_integer>::max ())
    (*current_liboctave_error_handler)
      ("integer dimension or index out of range for SuiteSparse's indexing type");

  return static_cast<suitesparse_integer> (x);
}

static void
spqr_error_handler (const cholmod_common *cc)
{
  if (cc->status >= 0)
    return;

  switch (cc->status)
    {
    case CHOLMOD_OUT_OF_MEMORY:
      (*current_liboctave_error_handler)
        ("sparse_qr: sparse matrix QR factorization failed"
         " - out of memory");
    case CHOLMOD_TOO_LARGE:
      (*current_liboctave_error_handler)
        ("sparse_qr: sparse matrix QR factorization failed"
         " - integer overflow occurred");
    default:
      (*current_liboctave_error_handler)
        ("sparse_qr: sparse matrix QR factorization failed"
         " - error %d", cc->status);
    }

  // FIXME: Free memory?
  // FIXME: Can cc-status > 0 (CHOLMOD_NOT_POSDEF, CHOLMOD_DSMALL) occur?
}
#endif

// Specializations.

// Real-valued matrices.

// Arguments for parameter order (taken from SuiteSparseQR documentation).
// 0: fixed ordering 0 (no permutation of columns)
// 1: natural ordering 1 (only singleton columns are permuted to the left of
//    the matrix)
// 2: colamd
// 3:
// 4: CHOLMOD best-effort (COLAMD, METIS,...)
// 5: AMD(a'*a)
// 6: metis(a'*a)
// 7: SuiteSparseQR default ordering
// 8: try COLAMD, AMD, and METIS; pick best
// 9: try COLAMD and AMD; pick best
//FIXME: What is order = 3?
template <>
sparse_qr<SparseMatrix>::sparse_qr_rep::sparse_qr_rep
(const SparseMatrix& a, int order)
  : nrows (a.rows ()), ncols (a.columns ())
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
  , m_cc (), m_R (nullptr), m_E (nullptr), m_H (nullptr), m_Htau (nullptr),
    m_HPinv (nullptr)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr < 0 || nc < 0)
    (*current_liboctave_error_handler)
      ("matrix dimension with negative size");

  if (order < 0 || order > 9)
    (*current_liboctave_error_handler)
      ("ordering %d is not supported by SPQR", order);

  cholmod_l_start (&m_cc);
  cholmod_sparse A = ros2rcs (a);

  SuiteSparseQR<double> (order, static_cast<double> (SPQR_DEFAULT_TOL),
                         static_cast<SuiteSparse_long> (A.nrow),
                         &A, &m_R, &m_E, &m_H, &m_HPinv, &m_Htau, &m_cc);
  spqr_error_handler (&m_cc);

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
    }
}

#elif defined (HAVE_CXSPARSE)
  , S (nullptr), N (nullptr)
{
  CXSPARSE_DNAME () A;

  A.nzmax = a.nnz ();
  A.m = nrows;
  A.n = ncols;
  // Cast away const on A, with full knowledge that CSparse won't touch it
  // Prevents the methods below making a copy of the data.
  A.p = const_cast<suitesparse_integer *>
        (to_suitesparse_intptr (a.cidx ()));
  A.i = const_cast<suitesparse_integer *>
        (to_suitesparse_intptr (a.ridx ()));
  A.x = const_cast<double *> (a.data ());
  A.nz = -1;

  S = CXSPARSE_DNAME (_sqr) (order, &A, 1);
  N = CXSPARSE_DNAME (_qr) (&A, S);

  if (! N)
    (*current_liboctave_error_handler)
      ("sparse_qr: sparse matrix QR factorization filled");

}

#else

{
  octave_unused_parameter (order);

  (*current_liboctave_error_handler)
    ("sparse_qr: support for SPQR or CXSparse was unavailable or disabled when liboctave was built");
}

#endif

template <>
sparse_qr<SparseMatrix>::sparse_qr_rep::~sparse_qr_rep (void)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  cholmod_l_free_sparse (&m_R, &m_cc);
  cholmod_l_free_sparse (&m_H, &m_cc);
  cholmod_l_free_dense (&m_Htau, &m_cc);
  free (m_E);  // FIXME: use cholmod_l_free
  free (m_HPinv);
  cholmod_l_finish (&m_cc);

#elif defined (HAVE_CXSPARSE)

  CXSPARSE_DNAME (_sfree) (S);
  CXSPARSE_DNAME (_nfree) (N);

#endif
}

template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::V (void) const
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  return rcs2ros (m_H);

#elif defined (HAVE_CXSPARSE)

  // Drop zeros from V and sort
  // FIXME: Is the double transpose to sort necessary?

  CXSPARSE_DNAME (_dropzeros) (N->L);
  CXSPARSE_DNAME () *D = CXSPARSE_DNAME (_transpose) (N->L, 1);
  CXSPARSE_DNAME (_spfree) (N->L);
  N->L = CXSPARSE_DNAME (_transpose) (D, 1);
  CXSPARSE_DNAME (_spfree) (D);

  octave_idx_type nc = N->L->n;
  octave_idx_type nz = N->L->nzmax;
  SparseMatrix ret (N->L->m, nc, nz);

  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->L->p[j];

  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->L->i[j];
      ret.xdata (j) = N->L->x[j];
    }

  return ret;

#else

  return SparseMatrix ();

#endif
}

template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::R (bool econ) const
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  octave_idx_type nr = static_cast<octave_idx_type> (m_R->nrow);
  octave_idx_type nc = static_cast<octave_idx_type> (m_R->ncol);
  octave_idx_type nz = static_cast<octave_idx_type> (m_R->nzmax);

  // FIXME: Does this work if econ = true?
  SparseMatrix ret ((econ ? (nc > nr ? nr : nc) : nr), nc, nz);
  SuiteSparse_long *Rp = reinterpret_cast<SuiteSparse_long *> (m_R->p);
  SuiteSparse_long *Ri = reinterpret_cast<SuiteSparse_long *> (m_R->i);

  for (octave_idx_type j = 0; j < nc + 1; j++)
    ret.xcidx (j) = from_suitesparse_long (Rp[j]);

  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = from_suitesparse_long (Ri[j]);
      ret.xdata (j) = (reinterpret_cast<double *> (m_R->x))[j];
    }

  return ret;

#elif defined (HAVE_CXSPARSE)

  // Drop zeros from R and sort
  // FIXME: Is the double transpose to sort necessary?

  CXSPARSE_DNAME (_dropzeros) (N->U);
  CXSPARSE_DNAME () *D = CXSPARSE_DNAME (_transpose) (N->U, 1);
  CXSPARSE_DNAME (_spfree) (N->U);
  N->U = CXSPARSE_DNAME (_transpose) (D, 1);
  CXSPARSE_DNAME (_spfree) (D);

  octave_idx_type nc = N->U->n;
  octave_idx_type nz = N->U->nzmax;

  SparseMatrix ret ((econ ? (nc > nrows ? nrows : nc) : nrows), nc, nz);

  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->U->p[j];

  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->U->i[j];
      ret.xdata (j) = N->U->x[j];
    }

  return ret;

#else

  octave_unused_parameter (econ);

  return SparseMatrix ();

#endif
}

template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::C (const Matrix& b, bool econ)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
  octave_idx_type nr = (econ
                        ? (ncols > nrows ? nrows : ncols)
                        : nrows);
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  Matrix ret (nr, b_nc);

  if (nrows != b_nr)
    (*current_liboctave_error_handler)
      ("sparse_qr: matrix dimension mismatch");
  else if (b_nc < 0 || b_nr < 0)
    (*current_liboctave_error_handler)
      ("sparse_qr: matrix dimension with negative size");

  cholmod_dense *QTB;  // Q' * B
  cholmod_dense B = rod2rcd (b);

  QTB = SuiteSparseQR_qmult<double> (SPQR_QTX, m_H, m_Htau, m_HPinv, &B,
                                     &m_cc);
  spqr_error_handler (&m_cc);

  // copy QTB into ret
  double *QTB_x = reinterpret_cast<double *> (QTB->x);
  double *ret_vec = reinterpret_cast<double *> (ret.fortran_vec ());
  for (octave_idx_type j = 0; j < b_nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      ret_vec[j * nr + i] = QTB_x[j * b_nr + i];

  cholmod_l_free_dense (&QTB, &m_cc);

  return ret;

#elif defined (HAVE_CXSPARSE)

  if (econ)
    (*current_liboctave_error_handler)
      ("sparse-qr: economy mode with CXSparse not supported");

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;

  const double *bvec = b.data ();

  Matrix ret (b_nr, b_nc);
  double *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0 || b_nc == 0)
    ret = Matrix (nc, b_nc, 0.0);
  else
    {
      OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0;
           j < b_nc;
           j++, idx += b_nr)
        {
          octave_quit ();

          for (octave_idx_type i = nr; i < S->m2; i++)
            buf[i] = 0.;

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          CXSPARSE_DNAME (_ipvec) (S->pinv, bvec + idx, buf, b_nr);

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              CXSPARSE_DNAME (_happly) (N->L, i, N->B[i], buf);
            }

          for (octave_idx_type i = 0; i < b_nr; i++)
            vec[i+idx] = buf[i];
        }
    }

  return ret;

#else

  octave_unused_parameter (b);
  octave_unused_parameter (econ);

  return Matrix ();

#endif
}

template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::Q (bool econ)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  octave_idx_type nc = (econ
                        ? (ncols > nrows ? nrows : ncols)
                        : nrows);
  Matrix ret (nrows, nc);
  cholmod_dense *q;

  // I is nrows x nrows identity matrix
  cholmod_dense *I
    = cholmod_l_allocate_dense (nrows, nrows, nrows, CHOLMOD_REAL, &m_cc);

  for (octave_idx_type i = 0; i < nrows * nrows; i++)
    (reinterpret_cast<double *> (I->x))[i] = 0.0;

  for (octave_idx_type i = 0; i < nrows; i++)
    (reinterpret_cast<double *> (I->x))[i * nrows + i] = 1.0;

  q = SuiteSparseQR_qmult<double> (SPQR_QX, m_H, m_Htau, m_HPinv, I, &m_cc);
  spqr_error_handler (&m_cc);

  double *q_x = reinterpret_cast<double *> (q->x);
  double *ret_vec = const_cast<double *> (ret.fortran_vec ());
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nrows; i++)
      ret_vec[j * nrows + i] = q_x[j * nrows + i];

  cholmod_l_free_dense (&q, &m_cc);
  cholmod_l_free_dense (&I, &m_cc);

  return ret;

#elif defined (HAVE_CXSPARSE)

  if (econ)
    (*current_liboctave_error_handler)
      ("sparse-qr: economy mode with CXSparse not supported");

  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  Matrix ret (nr, nr);
  double *ret_vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0)
    ret = Matrix (nc, nr, 0.0);
  else
    {
      OCTAVE_LOCAL_BUFFER (double, bvec, nr + 1);

      for (octave_idx_type i = 0; i < nr; i++)
        bvec[i] = 0.;

      OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0; j < nr; j++, idx += nr)
        {
          octave_quit ();

          bvec[j] = 1.0;
          for (octave_idx_type i = nr; i < S->m2; i++)
            buf[i] = 0.;

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          CXSPARSE_DNAME (_ipvec) (S->pinv, bvec, buf, nr);

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              CXSPARSE_DNAME (_happly) (N->L, i, N->B[i], buf);
            }

          for (octave_idx_type i = 0; i < nr; i++)
            ret_vec[i+idx] = buf[i];

          bvec[j] = 0.0;
        }
    }

  return ret.transpose ();

#else

  octave_unused_parameter (econ);

  return Matrix ();

#endif
}

template <>
template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<MArray<double>, Matrix>
(const MArray<double>& b, octave_idx_type& info)
{
  info = -1;

#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD) && defined (HAVE_CXSPARSE))

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  Matrix x (ncols, b_nc);  // X = m_E'*(m_R\(Q'*B))

  if (nrows < 0 || ncols < 0 || b_nc < 0 || b_nr < 0)
    (*current_liboctave_error_handler)
      ("matrix dimension with negative size");

  if (nrows < 0 || ncols < 0 || nrows != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  cholmod_dense *QTB;  // Q' * B
  cholmod_dense B = rod2rcd (b);

  // FIXME: Process b column by column as in the CXSPARSE version below.
  // This avoids a large dense matrix Q' * B in memory.
  QTB = SuiteSparseQR_qmult<double> (SPQR_QTX, m_H, m_Htau, m_HPinv, &B,
                                     &m_cc);

  spqr_error_handler (&m_cc);

  // convert m_R into CXSPARSE matrix R2
  CXSPARSE_DNAME (_sparse) R2;
  R2.n = ncols;
  R2.m = ncols;
  R2.nzmax = m_R->nzmax;
  R2.x = reinterpret_cast<double *> (m_R->x);
  suitesparse_integer *R2_p;
  suitesparse_integer *R2_i;
  if (sizeof (suitesparse_integer) == sizeof (SuiteSparse_long))
    {
      R2.p = reinterpret_cast<suitesparse_integer *> (m_R->p);
      R2.i = reinterpret_cast<suitesparse_integer *> (m_R->i);
    }
  else
    {
      R2_p = new suitesparse_integer[ncols+1];
      SuiteSparse_long *R_p = reinterpret_cast<SuiteSparse_long *> (m_R->p);
      for (octave_idx_type i = 0; i < ncols+1; i++)
        R2_p[i] = suitesparse_long_to_suitesparse_integer (R_p[i]);
      R2.p = R2_p;
      octave_idx_type nnz = m_R->nzmax;
      R2_i = new suitesparse_integer[nnz];
      SuiteSparse_long *R_i = reinterpret_cast<SuiteSparse_long *> (m_R->i);
      for (octave_idx_type i = 0; i < nnz; i++)
        R2_i[i] =  suitesparse_long_to_suitesparse_integer (R_i[i]);
      R2.i = R2_i;
    }
  R2.nz = -1;
  double *x_vec = const_cast<double *> (x.fortran_vec ());
  suitesparse_integer *E;
  if (sizeof (suitesparse_integer) != sizeof (SuiteSparse_long))
    {
      E = new suitesparse_integer [ncols];
      for (octave_idx_type i = 0; i < ncols; i++)
        E[i] = suitesparse_long_to_suitesparse_integer (m_E[i]);
    }
  else
    E = reinterpret_cast<suitesparse_integer *> (m_E);
  for (volatile octave_idx_type j = 0; j < b_nc; j++)
    {
      // fill x(:,j)
      // solve (m_R\(Q'*B(:,j)) and store result in QTB(:,j)
      CXSPARSE_DNAME (_usolve)
      (&R2, &(reinterpret_cast<double *> (QTB->x)[j * b_nr]));
      // x(:,j) = m_E' * (m_R\(Q'*B(:,j))
      CXSPARSE_DNAME (_ipvec)
      (E, &(reinterpret_cast<double *> (QTB->x)[j * b_nr]),
       &x_vec[j * ncols], ncols);
    }

  if (sizeof (suitesparse_integer) != sizeof (SuiteSparse_long))
    {
      delete [] R2_p;
      delete [] R2_i;
      delete [] E;
    }
  cholmod_l_free_dense (&QTB, &m_cc);

  info = 0;

  return x;

#elif defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const double *bvec = b.data ();

  Matrix x (nc, b_nc);
  double *vec = x.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, bvec + bidx, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, vec + idx, nc);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return Matrix ();

#endif
}

template <>
template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<MArray<double>, Matrix>
(const MArray<double>& b, octave_idx_type& info) const
{
  info = -1;
#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const double *bvec = b.data ();

  Matrix x (nc, b_nc);
  double *vec = x.fortran_vec ();

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_pvec) (S->q, bvec + bidx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_pvec) (S->pinv, buf, vec + idx, nc);
    }

  info = 0;

  return x;

#else
  octave_unused_parameter (b);

  return Matrix ();

#endif
}

template <>
template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<SparseMatrix, SparseMatrix>
(const SparseMatrix& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, Xx, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xx, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          double tmp = Xx[j];

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return SparseMatrix ();

#endif
}

template <>
template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<SparseMatrix, SparseMatrix>
(const SparseMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_pvec) (S->q, Xx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xx, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          double tmp = Xx[j];

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  octave_unused_parameter (b);

  return SparseMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<MArray<Complex>, ComplexMatrix>
(const MArray<Complex>& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  Complex *vec = x.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j, i);
          Xx[j] = c.real ();
          Xz[j] = c.imag ();
        }

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, Xx, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xx, nc);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, Xz, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xz, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        vec[j+idx] = Complex (Xx[j], Xz[j]);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return ComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<MArray<Complex>, ComplexMatrix>
(const MArray<Complex>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  Complex *vec = x.fortran_vec ();

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j, i);
          Xx[j] = c.real ();
          Xz[j] = c.imag ();
        }

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_pvec) (S->q, Xx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xx, nc);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_pvec) (S->q, Xz, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xz, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        vec[j+idx] = Complex (Xx[j], Xz[j]);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return ComplexMatrix ();

#endif
}

// Complex-valued matrices.

template <>
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::sparse_qr_rep
(const SparseComplexMatrix& a, int order)
  : nrows (a.rows ()), ncols (a.columns ())
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
  , m_cc (), m_R (nullptr), m_E (nullptr), m_H (nullptr),
    m_Htau (nullptr), m_HPinv (nullptr)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr < 0 || nc < 0)
    (*current_liboctave_error_handler)
      ("matrix dimension with negative size");

  if (order < 0 || order > 9)
    (*current_liboctave_error_handler)
      ("ordering %d is not supported by SPQR", order);

  cholmod_l_start (&m_cc);
  cholmod_sparse A = cos2ccs (a);

  SuiteSparseQR<Complex> (order, static_cast<double> (SPQR_DEFAULT_TOL),
                          static_cast<SuiteSparse_long> (A.nrow),
                          &A, &m_R, &m_E, &m_H,
                          &m_HPinv, &m_Htau, &m_cc);
  spqr_error_handler (&m_cc);

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
    }
}

#elif defined (HAVE_CXSPARSE)
  , S (nullptr), N (nullptr)
{
  CXSPARSE_ZNAME () A;

  A.nzmax = a.nnz ();
  A.m = nrows;
  A.n = ncols;
  // Cast away const on A, with full knowledge that CSparse won't touch it
  // Prevents the methods below making a copy of the data.
  A.p = const_cast<suitesparse_integer *>
        (to_suitesparse_intptr (a.cidx ()));
  A.i = const_cast<suitesparse_integer *>
        (to_suitesparse_intptr (a.ridx ()));
  A.x = const_cast<cs_complex_t *>
        (reinterpret_cast<const cs_complex_t *> (a.data ()));
  A.nz = -1;

  S = CXSPARSE_ZNAME (_sqr) (order, &A, 1);
  N = CXSPARSE_ZNAME (_qr) (&A, S);

  if (! N)
    (*current_liboctave_error_handler)
      ("sparse_qr: sparse matrix QR factorization filled");

}

#else

{
  octave_unused_parameter (order);

  (*current_liboctave_error_handler)
    ("sparse_qr: support for CXSparse was unavailable or disabled when liboctave was built");
}

#endif

template <>
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::~sparse_qr_rep (void)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  cholmod_l_free_sparse (&m_R, &m_cc);
  cholmod_l_free_sparse (&m_H, &m_cc);
  cholmod_l_free_dense (&m_Htau, &m_cc);
  free (m_E);  // FIXME: use cholmod_l_free
  free (m_HPinv);
  cholmod_l_finish (&m_cc);

#elif defined (HAVE_CXSPARSE)

  CXSPARSE_ZNAME (_sfree) (S);
  CXSPARSE_ZNAME (_nfree) (N);

#endif
}

template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::V (void) const
{
#if defined (HAVE_CXSPARSE)
  // Drop zeros from V and sort
  // FIXME: Is the double transpose to sort necessary?

  CXSPARSE_ZNAME (_dropzeros) (N->L);
  CXSPARSE_ZNAME () *D = CXSPARSE_ZNAME (_transpose) (N->L, 1);
  CXSPARSE_ZNAME (_spfree) (N->L);
  N->L = CXSPARSE_ZNAME (_transpose) (D, 1);
  CXSPARSE_ZNAME (_spfree) (D);

  octave_idx_type nc = N->L->n;
  octave_idx_type nz = N->L->nzmax;
  SparseComplexMatrix ret (N->L->m, nc, nz);

  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->L->p[j];

  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->L->i[j];
      ret.xdata (j) = reinterpret_cast<Complex *> (N->L->x)[j];
    }

  return ret;

#else

  return SparseComplexMatrix ();

#endif
}

template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::R (bool econ) const
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  octave_idx_type nr = from_size_t (m_R->nrow);
  octave_idx_type nc = from_size_t (m_R->ncol);
  octave_idx_type nz = from_size_t (m_R->nzmax);

  // FIXME: Does this work if econ = true?
  SparseComplexMatrix ret ((econ ? (nc > nr ? nr : nc) : nr), nc, nz);
  SuiteSparse_long *Rp = reinterpret_cast<SuiteSparse_long *> (m_R->p);
  SuiteSparse_long *Ri = reinterpret_cast<SuiteSparse_long *> (m_R->i);

  for (octave_idx_type j = 0; j < nc + 1; j++)
    ret.xcidx (j) = from_suitesparse_long (Rp[j]);

  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = from_suitesparse_long (Ri[j]);
      ret.xdata (j) = (reinterpret_cast<Complex *> (m_R->x))[j];
    }

  return ret;

#elif defined (HAVE_CXSPARSE)

  // Drop zeros from R and sort
  // FIXME: Is the double transpose to sort necessary?

  CXSPARSE_ZNAME (_dropzeros) (N->U);
  CXSPARSE_ZNAME () *D = CXSPARSE_ZNAME (_transpose) (N->U, 1);
  CXSPARSE_ZNAME (_spfree) (N->U);
  N->U = CXSPARSE_ZNAME (_transpose) (D, 1);
  CXSPARSE_ZNAME (_spfree) (D);

  octave_idx_type nc = N->U->n;
  octave_idx_type nz = N->U->nzmax;

  SparseComplexMatrix ret ((econ ? (nc > nrows ? nrows : nc) : nrows),
                           nc, nz);

  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->U->p[j];

  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->U->i[j];
      ret.xdata (j) = reinterpret_cast<Complex *>(N->U->x)[j];
    }

  return ret;

#else

  octave_unused_parameter (econ);

  return SparseComplexMatrix ();

#endif
}

template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::C
(const ComplexMatrix& b, bool econ)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  // FIXME: not tested
  octave_idx_type nr = (econ
                        ? (ncols > nrows ? nrows : ncols)
                        : nrows);
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  ComplexMatrix ret (nr, b_nc);

  if (nrows != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (b_nc < 0 || b_nr < 0)
    (*current_liboctave_error_handler)
      ("matrix dimension with negative size");

  cholmod_dense *QTB;  // Q' * B
  cholmod_dense B = cod2ccd (b);

  QTB = SuiteSparseQR_qmult<Complex> (SPQR_QTX, m_H, m_Htau, m_HPinv, &B,
                                      &m_cc);
  spqr_error_handler (&m_cc);

  // copy QTB into ret
  Complex *QTB_x = reinterpret_cast<Complex *> (QTB->x);
  Complex *ret_vec = reinterpret_cast<Complex *> (ret.fortran_vec ());
  for (octave_idx_type j = 0; j < b_nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      ret_vec[j * nr + i] = QTB_x[j * b_nr + i];

  cholmod_l_free_dense (&QTB, &m_cc);

  return ret;

#elif defined (HAVE_CXSPARSE)

  if (econ)
    (*current_liboctave_error_handler)
      ("sparse-qr: economy mode with CXSparse not supported");

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  const cs_complex_t *bvec
    = reinterpret_cast<const cs_complex_t *> (b.data ());
  ComplexMatrix ret (b_nr, b_nc);
  Complex *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0 || b_nc == 0)
    ret = ComplexMatrix (nc, b_nc, Complex (0.0, 0.0));
  else
    {
      OCTAVE_LOCAL_BUFFER (Complex, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0;
           j < b_nc;
           j++, idx += b_nr)
        {
          octave_quit ();

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          CXSPARSE_ZNAME (_ipvec) (S->pinv, bvec + idx,
                                   reinterpret_cast<cs_complex_t *> (buf),
                                   b_nr);

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              CXSPARSE_ZNAME (_happly) (N->L, i, N->B[i],
                                        reinterpret_cast<cs_complex_t *> (buf));
            }

          for (octave_idx_type i = 0; i < b_nr; i++)
            vec[i+idx] = buf[i];
        }
    }

  return ret;

#else

  octave_unused_parameter (b);
  octave_unused_parameter (econ);

  return ComplexMatrix ();

#endif
}

template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::Q (bool econ)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  octave_idx_type nc = (econ
                        ? (ncols > nrows ? nrows : ncols)
                        : nrows);
  ComplexMatrix ret (nrows, nc);
  cholmod_dense *q;

  // I is nrows x nrows identity matrix
  cholmod_dense *I
    = reinterpret_cast<cholmod_dense *>
      (cholmod_l_allocate_dense (nrows, nrows, nrows, CHOLMOD_COMPLEX, &m_cc));

  for (octave_idx_type i = 0; i < nrows * nrows; i++)
    (reinterpret_cast<Complex *> (I->x))[i] = 0.0;

  for (octave_idx_type i = 0; i < nrows; i++)
    (reinterpret_cast<Complex *> (I->x))[i * nrows + i] = 1.0;

  q = SuiteSparseQR_qmult<Complex> (SPQR_QX, m_H, m_Htau, m_HPinv, I,
                                    &m_cc);
  spqr_error_handler (&m_cc);

  Complex *q_x = reinterpret_cast<Complex *> (q->x);
  Complex *ret_vec = const_cast<Complex *> (ret.fortran_vec ());

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nrows; i++)
      ret_vec[j * nrows + i] = q_x[j * nrows + i];

  cholmod_l_free_dense (&q, &m_cc);
  cholmod_l_free_dense (&I, &m_cc);

  return ret;

#elif defined (HAVE_CXSPARSE)

  if (econ)
    (*current_liboctave_error_handler)
      ("sparse-qr: economy mode with CXSparse not supported");

  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  ComplexMatrix ret (nr, nr);
  Complex *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0)
    ret = ComplexMatrix (nc, nr, Complex (0.0, 0.0));
  else
    {
      OCTAVE_LOCAL_BUFFER (cs_complex_t, bvec, nr);

      for (octave_idx_type i = 0; i < nr; i++)
        bvec[i] = cs_complex_t (0.0, 0.0);

      OCTAVE_LOCAL_BUFFER (Complex, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0; j < nr; j++, idx+=nr)
        {
          octave_quit ();

          bvec[j] = cs_complex_t (1.0, 0.0);

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          CXSPARSE_ZNAME (_ipvec) (S->pinv, bvec,
                                   reinterpret_cast<cs_complex_t *> (buf),
                                   nr);

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              CXSPARSE_ZNAME (_happly) (N->L, i, N->B[i],
                                        reinterpret_cast<cs_complex_t *> (buf));
            }

          for (octave_idx_type i = 0; i < nr; i++)
            vec[i+idx] = buf[i];

          bvec[j] = cs_complex_t (0.0, 0.0);
        }
    }

  return ret.hermitian ();

#else

  octave_unused_parameter (econ);

  return ComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<SparseComplexMatrix,
          SparseComplexMatrix>
          (const SparseComplexMatrix& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j, i);
          Xx[j] = c.real ();
          Xz[j] = c.imag ();
        }

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, Xx, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xx, nc);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, Xz, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xz, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          Complex tmp = Complex (Xx[j], Xz[j]);

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<SparseComplexMatrix,
          SparseComplexMatrix>
          (const SparseComplexMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j, i);
          Xx[j] = c.real ();
          Xz[j] = c.imag ();
        }

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_pvec) (S->q, Xx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xx, nc);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_pvec) (S->q, Xz, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xz, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          Complex tmp = Complex (Xx[j], Xz[j]);

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  octave_unused_parameter (b);

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<MArray<double>,
          ComplexMatrix>
          (const MArray<double>& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *> (x.fortran_vec ());

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);
  OCTAVE_LOCAL_BUFFER (Complex, Xx, b_nr);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_ipvec) (S->pinv,
                               reinterpret_cast<cs_complex_t *>(Xx),
                               buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf, vec + idx, nc);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return ComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<MArray<double>,
          ComplexMatrix>
          (const MArray<double>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *> (x.fortran_vec ());

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (Complex, Xx, b_nr);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_pvec) (S->q, reinterpret_cast<cs_complex_t *> (Xx),
                              buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
        }

      CXSPARSE_ZNAME (_pvec) (S->pinv, buf, vec + idx, nc);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return ComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<SparseMatrix,
          SparseComplexMatrix>
          (const SparseMatrix& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_ipvec) (S->pinv,
                               reinterpret_cast<cs_complex_t *> (Xx),
                               buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf,
                               reinterpret_cast<cs_complex_t *> (Xx),
                               nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          Complex tmp = Xx[j];

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  octave_unused_parameter (b);

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<SparseMatrix,
          SparseComplexMatrix>
          (const SparseMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_pvec) (S->q,
                              reinterpret_cast<cs_complex_t *> (Xx),
                              buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
        }

      CXSPARSE_ZNAME (_pvec) (S->pinv, buf,
                              reinterpret_cast<cs_complex_t *> (Xx),
                              nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          Complex tmp = Xx[j];

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  octave_unused_parameter (b);

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<MArray<Complex>,
          ComplexMatrix>
          (const MArray<Complex>& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const cs_complex_t *bvec = reinterpret_cast<const cs_complex_t *>
                             (b.data ());

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *>
                      (x.fortran_vec ());

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_ipvec) (S->pinv, bvec + bidx, buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf, vec + idx, nc);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return ComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<MArray<Complex>,
          ComplexMatrix>
          (const MArray<Complex>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const cs_complex_t *bvec = reinterpret_cast<const cs_complex_t *>
                             (b.data ());

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *> (x.fortran_vec ());

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_pvec) (S->q, bvec + bidx, buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
        }

      CXSPARSE_ZNAME (_pvec) (S->pinv, buf, vec + idx, nc);
    }

  info = 0;

  return x;

#else

  octave_unused_parameter (b);

  return ComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<SparseComplexMatrix,
          SparseComplexMatrix>
          (const SparseComplexMatrix& b, octave_idx_type& info)
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_ipvec) (S->pinv,
                               reinterpret_cast<cs_complex_t *> (Xx),
                               buf, nr);

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
        }

      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf,
                               reinterpret_cast<cs_complex_t *> (Xx),
                               nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          Complex tmp = Xx[j];

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  octave_unused_parameter (b);

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<SparseComplexMatrix,
          SparseComplexMatrix>
          (const SparseComplexMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j, i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      CXSPARSE_ZNAME (_pvec) (S->q, reinterpret_cast<cs_complex_t *>(Xx),
                              buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
        }

      CXSPARSE_ZNAME (_pvec) (S->pinv, buf,
                              reinterpret_cast<cs_complex_t *>(Xx), nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          Complex tmp = Xx[j];

          if (tmp != 0.0)
            {
              if (ii == x_nz)
                {
                  // Resize the sparse matrix
                  octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
                  sz = (sz > 10 ? sz : 10) + x_nz;
                  x.change_capacity (sz);
                  x_nz = sz;
                }

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  octave_unused_parameter (b);

  return SparseComplexMatrix ();

#endif
}

template <typename SPARSE_T>
sparse_qr<SPARSE_T>::sparse_qr (void)
  : m_rep (new sparse_qr_rep (SPARSE_T (), 0))
{ }

template <typename SPARSE_T>
sparse_qr<SPARSE_T>::sparse_qr (const SPARSE_T& a, int order)
  : m_rep (new sparse_qr_rep (a, order))
{ }

template <typename SPARSE_T>
bool
sparse_qr<SPARSE_T>::ok (void) const
{
  return m_rep->ok ();
}

template <typename SPARSE_T>
SPARSE_T
sparse_qr<SPARSE_T>::V (void) const
{
  return m_rep->V ();
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::Pinv (void) const
{
  return m_rep->P ();
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::P (void) const
{
  return m_rep->P ();
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::E (void) const
{
  return m_rep->E();
}


template <typename SPARSE_T>
SparseMatrix
sparse_qr<SPARSE_T>::E_MAT (void) const
{
  ColumnVector perm = m_rep->E ();
  octave_idx_type nrows = perm.rows ();
  SparseMatrix ret (nrows, nrows, nrows);
  for (octave_idx_type i = 0; i < nrows; i++)
    ret(perm(i) - 1, i) = 1.0;
  return ret;
}


template <typename SPARSE_T>
SPARSE_T
sparse_qr<SPARSE_T>::R (bool econ) const
{
  return m_rep->R (econ);
}

template <typename SPARSE_T>
typename SPARSE_T::dense_matrix_type
sparse_qr<SPARSE_T>::C (const typename SPARSE_T::dense_matrix_type& b,
                        bool econ) const
{
  return m_rep->C (b, econ);
}

template <typename SPARSE_T>
typename SPARSE_T::dense_matrix_type
sparse_qr<SPARSE_T>::Q (bool econ) const
{
  return m_rep->Q (econ);
}

#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
//specializations of function min2norm_solve
template <>
template <>
OCTAVE_API Matrix
sparse_qr<SparseMatrix>::min2norm_solve<MArray<double>, Matrix>
(const SparseMatrix& a, const MArray<double>& b,
 octave_idx_type& info, int order)
{
  info = -1;
  octave_idx_type b_nc = b.cols ();
  octave_idx_type nc = a.cols ();
  Matrix x (nc, b_nc);
  cholmod_common cc;

  cholmod_l_start (&cc);
  cholmod_sparse A = ros2rcs (a);
  cholmod_dense B = rod2rcd (b);
  cholmod_dense *X;

  X = SuiteSparseQR_min2norm<double> (order, SPQR_DEFAULT_TOL, &A, &B, &cc);
  spqr_error_handler (&cc);

  double *vec = x.fortran_vec ();
  for (volatile octave_idx_type i = 0; i < nc * b_nc; i++)
    vec[i] = reinterpret_cast<double *> (X->x)[i];

  info = 0;
  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
    }
  cholmod_l_finish (&cc);

  return x;

}

template <>
template <>
OCTAVE_API SparseMatrix
sparse_qr<SparseMatrix>::min2norm_solve<SparseMatrix, SparseMatrix>
(const SparseMatrix& a, const SparseMatrix& b, octave_idx_type& info,
 int order)
{
  info = -1;
  SparseMatrix x;
  cholmod_common cc;

  cholmod_l_start (&cc);
  cholmod_sparse A = ros2rcs (a);
  cholmod_sparse B = ros2rcs (b);
  cholmod_sparse *X;

  X = SuiteSparseQR_min2norm<double> (order, SPQR_DEFAULT_TOL, &A, &B, &cc);
  spqr_error_handler (&cc);

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
      delete [] reinterpret_cast<SuiteSparse_long *> (B.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (B.i);
    }

  x = rcs2ros (X);
  cholmod_l_finish (&cc);
  info = 0;

  return x;

}

template <>
template <>
OCTAVE_API ComplexMatrix
sparse_qr<SparseMatrix>::min2norm_solve<MArray<Complex>, ComplexMatrix>
(const SparseMatrix& a, const MArray<Complex>& b,
 octave_idx_type& info, int order)
{
  info = -1;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix x (nc, b_nc);

  cholmod_common cc;

  cholmod_l_start (&cc);

  cholmod_sparse *A = ros2ccs (a, &cc);
  cholmod_dense B = cod2ccd (b);
  cholmod_dense *X;

  X = SuiteSparseQR_min2norm<Complex> (order, SPQR_DEFAULT_TOL, A, &B, &cc);
  spqr_error_handler (&cc);

  Complex *vec = x.fortran_vec ();
  for (volatile octave_idx_type i = 0; i < nc * b_nc; i++)
    vec[i] = reinterpret_cast<Complex *> (X->x)[i];

  cholmod_l_free_sparse (&A, &cc);
  cholmod_l_finish (&cc);

  info = 0;

  return x;

}

template <>
template <>
OCTAVE_API SparseComplexMatrix
sparse_qr<SparseMatrix>::min2norm_solve<SparseComplexMatrix,
          SparseComplexMatrix>
          (const SparseMatrix& a, const SparseComplexMatrix& b,
           octave_idx_type& info, int order)
{
  info = -1;

  cholmod_common cc;

  cholmod_l_start (&cc);

  cholmod_sparse *A = ros2ccs (a, &cc);
  cholmod_sparse B = cos2ccs (b);
  cholmod_sparse *X;

  X = SuiteSparseQR_min2norm<Complex> (order, SPQR_DEFAULT_TOL, A, &B, &cc);
  spqr_error_handler (&cc);

  cholmod_l_free_sparse (&A, &cc);
  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (B.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (B.i);
    }
  cholmod_l_finish (&cc);

  SparseComplexMatrix ret = ccs2cos(X);

  info = 0;

  return ret;

}

template <>
template <>
OCTAVE_API ComplexMatrix
sparse_qr<SparseComplexMatrix>::min2norm_solve<MArray<Complex>,
          ComplexMatrix>
          (const SparseComplexMatrix& a, const MArray<Complex>& b,
           octave_idx_type& info, int order)
{
  info = -1;
  octave_idx_type b_nc = b.cols ();
  octave_idx_type nc = a.cols ();
  ComplexMatrix x (nc, b_nc);

  cholmod_common cc;

  cholmod_l_start (&cc);

  cholmod_sparse A = cos2ccs (a);
  cholmod_dense B = cod2ccd (b);
  cholmod_dense *X;

  X = SuiteSparseQR_min2norm<Complex> (order, SPQR_DEFAULT_TOL, &A, &B, &cc);
  spqr_error_handler (&cc);

  Complex *vec = x.fortran_vec ();
  for (volatile octave_idx_type i = 0; i < nc * b_nc; i++)
    vec[i] = reinterpret_cast<Complex *> (X->x)[i];

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
    }
  cholmod_l_finish (&cc);

  info = 0;

  return x;

}

template <>
template <>
OCTAVE_API ComplexMatrix
sparse_qr<SparseComplexMatrix>::min2norm_solve<MArray<double>,
          ComplexMatrix>
          (const SparseComplexMatrix& a, const MArray<double>& b,
           octave_idx_type& info, int order)
{
  info = -1;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type nc = a.cols ();
  ComplexMatrix x (nc, b_nc);

  cholmod_common cc;

  cholmod_l_start (&cc);

  cholmod_sparse A = cos2ccs (a);
  cholmod_dense *B = rod2ccd (b, &cc);
  cholmod_dense *X;

  X = SuiteSparseQR_min2norm<Complex> (order, SPQR_DEFAULT_TOL, &A, B, &cc);
  spqr_error_handler (&cc);

  Complex *vec = x.fortran_vec ();

  for (volatile octave_idx_type i = 0; i < nc * b_nc; i++)
    vec[i] = reinterpret_cast<Complex *> (X->x)[i];

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
    }
  cholmod_l_free_dense (&B, &cc);
  cholmod_l_finish (&cc);

  info = 0;

  return x;

}

template <>
template <>
OCTAVE_API SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::min2norm_solve<SparseComplexMatrix,
          SparseComplexMatrix>
          (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
           octave_idx_type& info, int order)
{
  info = -1;

  cholmod_common cc;

  cholmod_l_start (&cc);

  cholmod_sparse A = cos2ccs (a);
  cholmod_sparse B = cos2ccs (b);
  cholmod_sparse *X;

  X = SuiteSparseQR_min2norm<Complex> (order, SPQR_DEFAULT_TOL, &A, &B, &cc);
  spqr_error_handler (&cc);

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
      delete [] reinterpret_cast<SuiteSparse_long *> (B.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (B.i);
    }
  cholmod_l_finish (&cc);

  info = 0;

  return ccs2cos (X);

}

template <>
template <>
OCTAVE_API SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::min2norm_solve<SparseMatrix,
          SparseComplexMatrix>
          (const SparseComplexMatrix& a, const SparseMatrix& b,
           octave_idx_type& info, int order)
{
  info = -1;

  cholmod_common cc;

  cholmod_l_start (&cc);

  cholmod_sparse A = cos2ccs (a);
  cholmod_sparse *B = ros2ccs (b, &cc);
  cholmod_sparse *X;

  X = SuiteSparseQR_min2norm<Complex> (order, SPQR_DEFAULT_TOL, &A, B, &cc);
  spqr_error_handler (&cc);

  SparseComplexMatrix ret = ccs2cos(X);

  if (sizeof (octave_idx_type) != sizeof (SuiteSparse_long))
    {
      delete [] reinterpret_cast<SuiteSparse_long *> (A.p);
      delete [] reinterpret_cast<SuiteSparse_long *> (A.i);
    }
  cholmod_l_free_sparse (&B, &cc);
  cholmod_l_finish (&cc);

  info = 0;

  return ret;

}
#endif

// FIXME: Why is the "order" of the QR calculation as used in the
// CXSparse function sqr 3 for real matrices and 2 for complex?  These
// values seem to be required but there was no explanation in David
// Bateman's original code.

template <typename SPARSE_T>
class
cxsparse_defaults
{
public:
  enum { order = -1 };
};

template <>
class
cxsparse_defaults<SparseMatrix>
{
public:
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
  enum { order = SPQR_ORDERING_DEFAULT };
#elif defined (HAVE_CXSPARSE)
  enum { order = 3 };
#endif
};

template <>
class
cxsparse_defaults<SparseComplexMatrix>
{
public:
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
  enum { order = SPQR_ORDERING_DEFAULT };
#elif defined (HAVE_CXSPARSE)
  enum { order = 2 };
#endif
};

template <typename SPARSE_T>
template <typename RHS_T, typename RET_T>
RET_T
sparse_qr<SPARSE_T>::solve (const SPARSE_T& a, const RHS_T& b,
                            octave_idx_type& info)
{
#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))

  info = -1;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  int order = cxsparse_defaults<SPARSE_T>::order;

  if (nr < 0 || nc < 0 || b_nc < 0 || b_nr < 0)
    (*current_liboctave_error_handler)
      ("matrix dimension with negative size");

  if ( nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");

  info = 0;

  return min2norm_solve<RHS_T, RET_T> (a, b, info, order);

#elif defined (HAVE_CXSPARSE)

  info = -1;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  int order = cxsparse_defaults<SPARSE_T>::order;

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");

  if (nr == 0 || nc == 0 || b_nc == 0)
    {
      info = 0;

      return RET_T (nc, b_nc, 0.0);
    }
  else if (nr >= nc)
    {
      sparse_qr<SPARSE_T> q (a, order);

      return q.ok () ? q.tall_solve<RHS_T, RET_T> (b, info) : RET_T ();
    }
  else
    {
      sparse_qr<SPARSE_T> q (a.hermitian (), order);

      return q.ok () ? q.wide_solve<RHS_T, RET_T> (b, info) : RET_T ();
    }

#else

  octave_unused_parameter (a);
  octave_unused_parameter (b);
  octave_unused_parameter (info);

  return RET_T ();

#endif
}

//explicit instantiations of static member function solve
template
OCTAVE_API Matrix
sparse_qr<SparseMatrix>::solve<MArray<double>, Matrix>
(const SparseMatrix& a, const MArray<double>& b, octave_idx_type& info);

template
OCTAVE_API SparseMatrix
sparse_qr<SparseMatrix>::solve<SparseMatrix, SparseMatrix>
(const SparseMatrix& a, const SparseMatrix& b, octave_idx_type& info);

template
OCTAVE_API ComplexMatrix
sparse_qr<SparseMatrix>::solve<MArray<Complex>, ComplexMatrix>
(const SparseMatrix& a, const MArray<Complex>& b, octave_idx_type& info);

template
OCTAVE_API SparseComplexMatrix
sparse_qr<SparseMatrix>::solve<SparseComplexMatrix, SparseComplexMatrix>
(const SparseMatrix& a, const SparseComplexMatrix& b,
 octave_idx_type& info);

template
OCTAVE_API ComplexMatrix
sparse_qr<SparseComplexMatrix>::solve<MArray<Complex>, ComplexMatrix>
(const SparseComplexMatrix& a, const MArray<Complex>& b,
 octave_idx_type& info);

template
OCTAVE_API SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::solve<
SparseComplexMatrix, SparseComplexMatrix>
(const SparseComplexMatrix& a, const SparseComplexMatrix& b,
 octave_idx_type& info);

template
OCTAVE_API ComplexMatrix
sparse_qr<SparseComplexMatrix>::solve<MArray<double>, ComplexMatrix>
(const SparseComplexMatrix& a, const MArray<double>& b,
 octave_idx_type& info);

template
OCTAVE_API SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::solve<SparseMatrix, SparseComplexMatrix>
(const SparseComplexMatrix& a, const SparseMatrix& b,
 octave_idx_type& info);

//explicit instantiations of member function E_MAT
template
OCTAVE_API SparseMatrix
sparse_qr<SparseMatrix>::E_MAT (void) const;

template
OCTAVE_API SparseMatrix
sparse_qr<SparseComplexMatrix>::E_MAT (void) const;

template <typename SPARSE_T>
template <typename RHS_T, typename RET_T>
RET_T
sparse_qr<SPARSE_T>::tall_solve (const RHS_T& b, octave_idx_type& info) const
{
  return m_rep->template tall_solve<RHS_T, RET_T> (b, info);
}

template <typename SPARSE_T>
template <typename RHS_T, typename RET_T>
RET_T
sparse_qr<SPARSE_T>::wide_solve (const RHS_T& b, octave_idx_type& info) const
{
  return m_rep->template wide_solve<RHS_T, RET_T> (b, info);
}

// Explicitly instantiate all member functions

template OCTAVE_API sparse_qr<SparseMatrix>::sparse_qr (void);
template OCTAVE_API
sparse_qr<SparseMatrix>::sparse_qr (const SparseMatrix& a, int order);
template OCTAVE_API bool sparse_qr<SparseMatrix>::ok (void) const;
template OCTAVE_API ColumnVector sparse_qr<SparseMatrix>::E (void) const;
template OCTAVE_API SparseMatrix sparse_qr<SparseMatrix>::V (void) const;
template OCTAVE_API ColumnVector sparse_qr<SparseMatrix>::Pinv (void) const;
template OCTAVE_API ColumnVector sparse_qr<SparseMatrix>::P (void) const;
template OCTAVE_API SparseMatrix
sparse_qr<SparseMatrix>::R (bool econ) const;
template OCTAVE_API Matrix
sparse_qr<SparseMatrix>::C (const Matrix& b, bool econ) const;
template OCTAVE_API Matrix sparse_qr<SparseMatrix>::Q (bool econ) const;

template OCTAVE_API sparse_qr<SparseComplexMatrix>::sparse_qr (void);
template OCTAVE_API
sparse_qr<SparseComplexMatrix>::sparse_qr
(const SparseComplexMatrix& a, int order);
template OCTAVE_API bool sparse_qr<SparseComplexMatrix>::ok (void) const;
template OCTAVE_API ColumnVector
sparse_qr<SparseComplexMatrix>::E (void) const;
template OCTAVE_API SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::V (void) const;
template OCTAVE_API ColumnVector
sparse_qr<SparseComplexMatrix>::Pinv (void) const;
template OCTAVE_API ColumnVector
sparse_qr<SparseComplexMatrix>::P (void) const;
template OCTAVE_API SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::R (bool econ) const;
template OCTAVE_API ComplexMatrix
sparse_qr<SparseComplexMatrix>::C (const ComplexMatrix& b, bool econ) const;
template OCTAVE_API ComplexMatrix
sparse_qr<SparseComplexMatrix>::Q (bool econ) const;

Matrix
qrsolve (const SparseMatrix& a, const MArray<double>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<MArray<double>, Matrix> (a, b,
         info);
}

SparseMatrix
qrsolve (const SparseMatrix& a, const SparseMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<SparseMatrix, SparseMatrix> (a, b,
         info);
}

ComplexMatrix
qrsolve (const SparseMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<MArray<Complex>,
         ComplexMatrix> (a, b, info);
}

SparseComplexMatrix
qrsolve (const SparseMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<SparseComplexMatrix,
         SparseComplexMatrix> (a, b, info);
}

ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<double>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<MArray<double>,
         ComplexMatrix> (a, b, info);
}

SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<SparseMatrix,
         SparseComplexMatrix>
         (a, b, info);
}

ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<MArray<Complex>,
         ComplexMatrix> (a, b, info);
}

SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<SparseComplexMatrix,
         SparseComplexMatrix>
         (a, b, info);
}

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
