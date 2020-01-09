////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2020 The Octave Project Developers
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

#include "lo-error.h"
#include "oct-sparse.h"

#if (defined (HAVE_AMD) || defined (HAVE_CCOLAMD)               \
     || defined (HAVE_CHOLMOD) || defined (HAVE_COLAMD)         \
     || defined (HAVE_CXSPARSE) || defined (HAVE_UMFPACK))

namespace octave
{
  static inline void
  check_suitesparse_integer_size (void)
  {
    // FIXME: maybe it would be better to make this a configure check and
    // disable suitesparse if it fails?

    if (sizeof (suitesparse_integer) != sizeof (octave_idx_type))
      (*current_liboctave_error_handler)
        ("size of suitesparse integer does not match octave_idx_type!");
  }

  suitesparse_integer *
  to_suitesparse_intptr (octave_idx_type *i)
  {
    check_suitesparse_integer_size ();

    return reinterpret_cast<suitesparse_integer *> (i);
  }

  const suitesparse_integer *
  to_suitesparse_intptr (const octave_idx_type *i)
  {
    check_suitesparse_integer_size ();

    return reinterpret_cast<const suitesparse_integer *> (i);
  }

  octave_idx_type*
  to_octave_idx_type_ptr (suitesparse_integer *i)
  {
    check_suitesparse_integer_size ();

    return reinterpret_cast<octave_idx_type *> (i);
  }

  const octave_idx_type*
  to_octave_idx_type_ptr (const suitesparse_integer *i)
  {
    check_suitesparse_integer_size ();

    return reinterpret_cast<const octave_idx_type *> (i);
  }

#  if defined (HAVE_CHOLMOD)
  const cholmod_sparse
  ros2rcs (const SparseMatrix &a)
  {
    cholmod_sparse A;

    A.ncol = a.cols ();
    A.nrow = a.rows ();
#    if defined (OCTAVE_ENABLE_64)
    A.itype = CHOLMOD_LONG;
#    else
    A.itype = CHOLMOD_INT;
#    endif
    A.nzmax = a.nnz ();
    A.sorted = 0;
    A.packed = 1;
    A.stype = 0;
    A.xtype = CHOLMOD_REAL;
    A.dtype = CHOLMOD_DOUBLE;
    A.nz = NULL;
    A.z = NULL;
    A.p = const_cast<suitesparse_integer*> (to_suitesparse_intptr (a.cidx ()));
    A.i = const_cast<suitesparse_integer*> (to_suitesparse_intptr (a.ridx ()));
    A.x = const_cast<double*> (a.data ());

    return A;
  }

  const cholmod_sparse
  cos2ccs (const SparseComplexMatrix &a)
  {
    cholmod_sparse A;

    A.ncol = a.cols ();
    A.nrow = a.rows ();
#    if defined (OCTAVE_ENABLE_64)
    A.itype = CHOLMOD_LONG;
#    else
    A.itype = CHOLMOD_INT;
#    endif
    A.nzmax = a.nnz ();
    A.sorted = 0;
    A.packed = 1;
    A.stype = 0;
    A.xtype = CHOLMOD_COMPLEX;
    A.dtype = CHOLMOD_DOUBLE;
    A.nz = NULL;
    A.z = NULL;
    A.p = const_cast<suitesparse_integer*> (to_suitesparse_intptr (a.cidx ()));
    A.i = const_cast<suitesparse_integer*> (to_suitesparse_intptr (a.ridx ()));
    A.x = const_cast<Complex*>
                 (reinterpret_cast<const Complex*> (a.data ()));

    return A;
  }

  cholmod_dense*
  rod2ccd (const MArray<double> &a, cholmod_common* cc1)
  {
    cholmod_dense *A = static_cast<cholmod_dense*>
                         (CHOLMOD_NAME (allocate_dense)
                            (a.rows (), a.cols (), a.rows(), CHOLMOD_COMPLEX,
                             cc1));

    const double *a_x = a.data ();

    for (octave_idx_type j = 0; j < a.cols() * a.rows() ; j++)
      (static_cast<Complex*> (A->x))[j] = Complex (a_x[j], 0.0);

    return A;
  }

  const cholmod_dense
  rod2rcd (const MArray<double> &a)
  {
    cholmod_dense A;

    A.ncol = a.cols ();
    A.nrow = a.rows ();
    A.nzmax = a.cols() * a.rows();
    A.xtype = CHOLMOD_REAL;
    A.dtype = CHOLMOD_DOUBLE;
    A.z = NULL;
    A.d = a.rows();
    A.x = const_cast<double*> (a.data ());

    return A;
  }

  const cholmod_dense
  cod2ccd (const ComplexMatrix &a)
  {
    cholmod_dense A;

    A.ncol = a.cols ();
    A.nrow = a.rows ();
    A.nzmax = a.cols () * a.rows ();
    A.xtype = CHOLMOD_COMPLEX;
    A.dtype = CHOLMOD_DOUBLE;
    A.z = NULL;
    A.d = a.rows();
    A.x = const_cast<Complex*>
            (reinterpret_cast<const Complex*> (a.data ()));

    return A;
  }

  SparseMatrix
  rcs2ros (const cholmod_sparse* y)
  {
    SparseMatrix ret (static_cast<octave_idx_type> (y->nrow),
                      static_cast<octave_idx_type> (y->ncol),
                      static_cast<octave_idx_type> (y->nzmax));

    octave_idx_type nz = static_cast<octave_idx_type> (y->nzmax);

    for (octave_idx_type j = 0; j < static_cast<octave_idx_type> (y->ncol) + 1;
         j++)
      ret.xcidx (j) = (static_cast<octave_idx_type*> (y->p))[j];

    for (octave_idx_type j = 0; j < nz; j++)
      {
        ret.xridx (j) = (static_cast<octave_idx_type*> (y->i))[j];
        ret.xdata (j) = (static_cast<double*> (y->x))[j];
      }

    return ret;
  }

  SparseComplexMatrix
  ccs2cos (const cholmod_sparse* a)
  {
    SparseComplexMatrix ret (static_cast<octave_idx_type> (a->nrow),
                             static_cast<octave_idx_type> (a->ncol),
                             static_cast<octave_idx_type> (a->nzmax));

    octave_idx_type nz = static_cast<octave_idx_type> (a->nzmax);

    for (octave_idx_type j = 0; j < static_cast<octave_idx_type> (a->ncol) + 1;
         j++)
      ret.xcidx (j) = (static_cast<octave_idx_type*> (a->p))[j];

    for (octave_idx_type j = 0; j < nz; j++)
      {
        ret.xridx (j) = (static_cast<octave_idx_type*> (a->i))[j];
        ret.xdata (j) = (static_cast<Complex*> (a->x))[j];
      }
 
    return ret;
  }

  cholmod_sparse*
  ros2ccs (const SparseMatrix& a, cholmod_common* cc1)
  {
    cholmod_sparse *A = static_cast<cholmod_sparse*>
                          (CHOLMOD_NAME(allocate_sparse)
                             (a.rows (), a.cols (), a.nnz (), 0, 1, 0,
                              CHOLMOD_COMPLEX, cc1));

    for (octave_idx_type j = 0;
         j < static_cast<octave_idx_type> (a.cols ()) + 1; j++)
      static_cast<octave_idx_type*> (A->p)[j] = a.cidx(j);

    const double *a_x = a.data ();
    for (octave_idx_type j = 0; j < a.nnz (); j++)
      {
        (static_cast<Complex*> (A->x))[j] = Complex (a_x[j], 0.0);
        (static_cast<octave_idx_type*> (A->i))[j] = a.ridx(j);
      }
    return A;
  }

  void
  spqr_error_handler (const cholmod_common* cc)
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
          ("sparse_qr: sparse matrix QR factorization failed");
      }

    // FIXME: Free memory?
    // FIXME: Can cc-status > 0 (CHOLMOD_NOT_POSDEF, CHOLMOD_DSMALL) occur?
  }
#  endif
}

#endif
