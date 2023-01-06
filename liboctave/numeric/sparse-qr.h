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

#if ! defined (octave_sparse_qr_h)
#define octave_sparse_qr_h 1

#include "octave-config.h"

#include <memory>

#include "oct-cmplx.h"
#include "MArray-fwd.h"
#include "mx-fwd.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

// If the sparse matrix classes become templated on the element type
// (i.e., sparse_matrix<double>), then it might be best to make the
// template parameter of this class also be the element type instead
// of the matrix type.

template <typename SPARSE_T>
class
sparse_qr
{
public:

  OCTAVE_API sparse_qr (void);

#if (defined (HAVE_SPQR) && defined (HAVE_CHOLMOD))
  // order = 7 selects SPQR default ordering
  OCTAVE_API sparse_qr (const SPARSE_T& a, int order = 7);
#else
  OCTAVE_API sparse_qr (const SPARSE_T& a, int order = 0);
#endif

  sparse_qr (const sparse_qr& a) = default;

  ~sparse_qr (void) = default;

  sparse_qr& operator = (const sparse_qr& a) = default;

  OCTAVE_API bool ok (void) const;

  OCTAVE_API ColumnVector E (void) const;

  // constructs permutation matrix from permutation vector rep -> E()
  OCTAVE_API SparseMatrix E_MAT () const;

  OCTAVE_API SPARSE_T V (void) const;

  OCTAVE_API ColumnVector Pinv (void) const;

  OCTAVE_API ColumnVector P (void) const;

  OCTAVE_API SPARSE_T R (bool econ = false) const;

  OCTAVE_API typename SPARSE_T::dense_matrix_type
  C (const typename SPARSE_T::dense_matrix_type& b, bool econ = false) const;

  OCTAVE_API typename SPARSE_T::dense_matrix_type
  Q (bool econ = false) const;

  template <typename RHS_T, typename RET_T>
  static OCTAVE_API RET_T
  solve (const SPARSE_T& a, const RHS_T& b,
         octave_idx_type& info);

private:

  template <typename RHS_T, typename RET_T>
  static OCTAVE_API RET_T
  min2norm_solve (const SPARSE_T& a, const RHS_T& b,
                  octave_idx_type& info, int order);

  template <typename RHS_T, typename RET_T>
  OCTAVE_API RET_T
  tall_solve (const RHS_T& b, octave_idx_type& info) const;

  template <typename RHS_T, typename RET_T>
  OCTAVE_API RET_T
  wide_solve (const RHS_T& b, octave_idx_type& info) const;

  //--------
  class sparse_qr_rep;

  std::shared_ptr<sparse_qr_rep> m_rep;
};

// extern instantiations with set visibility/export/import attribute

extern template class OCTAVE_EXTERN_TEMPLATE_API sparse_qr<SparseMatrix>;

extern template class OCTAVE_EXTERN_TEMPLATE_API sparse_qr<SparseComplexMatrix>;

// Provide qrsolve for backward compatibility.

extern OCTAVE_API Matrix
qrsolve (const SparseMatrix& a, const MArray<double>& b,
         octave_idx_type& info);

extern OCTAVE_API SparseMatrix
qrsolve (const SparseMatrix& a, const SparseMatrix& b,
         octave_idx_type& info);

extern OCTAVE_API ComplexMatrix
qrsolve (const SparseMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info);

extern OCTAVE_API SparseComplexMatrix
qrsolve (const SparseMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info);

extern OCTAVE_API ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<double>& b,
         octave_idx_type& info);

extern OCTAVE_API SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseMatrix& b,
         octave_idx_type& info);

extern OCTAVE_API ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info);

extern OCTAVE_API SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info);

typedef sparse_qr<SparseMatrix> SparseQR;
typedef sparse_qr<SparseComplexMatrix> SparseComplexQR;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
