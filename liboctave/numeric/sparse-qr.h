/*

Copyright (C) 2016 John W. Eaton
Copyright (C) 2005-2015 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_sparse_qr_h)
#define octave_sparse_qr_h 1

#include "octave-config.h"

#include "dMatrix.h"
#include "CMatrix.h"
#include "dSparse.h"
#include "CSparse.h"

// If the sparse matrix classes become templated on the element type
// (i.e., sparse_matrix<double>), then it might be best to make the
// template parameter of this class also be the element type instead
// of the matrix type.

template <typename SPARSE_T>
class
sparse_qr
{
public:

  sparse_qr (void);

  sparse_qr (const SPARSE_T& a, int order = 0);

  sparse_qr (const sparse_qr& a);

  ~sparse_qr (void);

  sparse_qr& operator = (const sparse_qr& a);

  bool ok (void) const;

  SPARSE_T V (void) const;

  ColumnVector Pinv (void) const;

  ColumnVector P (void) const;

  SPARSE_T R (bool econ = false) const;

  typename SPARSE_T::dense_matrix_type
  C (const typename SPARSE_T::dense_matrix_type& b) const;

  typename SPARSE_T::dense_matrix_type
  Q (void) const;

  template <typename RHS_T, typename RET_T>
  static RET_T
  solve (const SPARSE_T& a, const RHS_T& b,
         octave_idx_type& info);

private:

  class sparse_qr_rep;

  sparse_qr_rep *rep;

  template <typename RHS_T, typename RET_T>
  RET_T
  tall_solve (const RHS_T& b, octave_idx_type& info) const;

  template <typename RHS_T, typename RET_T>
  RET_T
  wide_solve (const RHS_T& b, octave_idx_type& info) const;
};

// Provide qrsolve for backward compatibility.

extern Matrix
qrsolve (const SparseMatrix& a, const MArray<double>& b,
         octave_idx_type& info);

extern SparseMatrix
qrsolve (const SparseMatrix& a, const SparseMatrix& b,
         octave_idx_type& info);

extern ComplexMatrix
qrsolve (const SparseMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info);

extern SparseComplexMatrix
qrsolve (const SparseMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info);

extern ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<double>& b,
         octave_idx_type& info);

extern SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseMatrix& b,
         octave_idx_type& info);

extern ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info);

extern SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info);

typedef sparse_qr<SparseMatrix> SparseQR;
typedef sparse_qr<SparseComplexMatrix> SparseComplexQR;

#endif
