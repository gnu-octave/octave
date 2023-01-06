////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#if ! defined (octave_dSparse_h)
#define octave_dSparse_h 1

#include "octave-config.h"

#include "mx-fwd.h"

#include "CColVector.h"
#include "CMatrix.h"
#include "DET.h"
#include "MSparse.h"
#include "MatrixType.h"
#include "Sparse-op-decls.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dNDArray.h"

class
SparseMatrix : public MSparse<double>
{
public:

  // Corresponding dense matrix type for this sparse matrix type.
  typedef Matrix dense_matrix_type;

  typedef void (*solve_singularity_handler) (double rcond);

  SparseMatrix (void) : MSparse<double> () { }

  SparseMatrix (octave_idx_type r, octave_idx_type c)
    : MSparse<double> (r, c) { }

  SparseMatrix (const dim_vector& dv, octave_idx_type nz = 0) :
    MSparse<double> (dv, nz) { }

  explicit SparseMatrix (octave_idx_type r, octave_idx_type c, double val)
    : MSparse<double> (r, c, val) { }

  SparseMatrix (const SparseMatrix& a) : MSparse<double> (a) { }

  SparseMatrix (const SparseMatrix& a, const dim_vector& dv)
    : MSparse<double> (a, dv) { }

  SparseMatrix (const MSparse<double>& a) : MSparse<double> (a) { }

  SparseMatrix (const Sparse<double>& a) : MSparse<double> (a) { }

  explicit OCTAVE_API SparseMatrix (const SparseBoolMatrix& a);

  explicit SparseMatrix (const Matrix& a) : MSparse<double> (a) { }

  explicit SparseMatrix (const NDArray& a) : MSparse<double> (a) { }

  SparseMatrix (const Array<double>& a, const octave::idx_vector& r,
                const octave::idx_vector& c, octave_idx_type nr = -1,
                octave_idx_type nc = -1, bool sum_terms = true,
                octave_idx_type nzm = -1)
    : MSparse<double> (a, r, c, nr, nc, sum_terms, nzm) { }

  explicit OCTAVE_API SparseMatrix (const DiagMatrix& a);

  explicit SparseMatrix (const PermMatrix& a) : MSparse<double>(a) { }

  SparseMatrix (octave_idx_type r, octave_idx_type c,
                octave_idx_type num_nz) : MSparse<double> (r, c, num_nz) { }

  SparseMatrix& operator = (const SparseMatrix& a)
  {
    MSparse<double>::operator = (a);
    return *this;
  }

  OCTAVE_API bool operator == (const SparseMatrix& a) const;
  OCTAVE_API bool operator != (const SparseMatrix& a) const;

  OCTAVE_API bool issymmetric (void) const;

  OCTAVE_API SparseMatrix max (int dim = -1) const;
  OCTAVE_API SparseMatrix max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API SparseMatrix min (int dim = -1) const;
  OCTAVE_API SparseMatrix min (Array<octave_idx_type>& index, int dim = -1) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API SparseMatrix&
  insert (const SparseMatrix& a, octave_idx_type r, octave_idx_type c);

  OCTAVE_API SparseMatrix&
  insert (const SparseMatrix& a, const Array<octave_idx_type>& indx);

  OCTAVE_API SparseMatrix
  concat (const SparseMatrix& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API SparseComplexMatrix
  concat (const SparseComplexMatrix& rb, const Array<octave_idx_type>& ra_idx);

  friend OCTAVE_API SparseMatrix real (const SparseComplexMatrix& a);
  friend OCTAVE_API SparseMatrix imag (const SparseComplexMatrix& a);

  SparseMatrix transpose (void) const
  {
    return MSparse<double>::transpose ();
  }
  SparseMatrix hermitian (void) const { return transpose (); }

  // extract row or column i.

  OCTAVE_API RowVector row (octave_idx_type i) const;

  OCTAVE_API ColumnVector column (octave_idx_type i) const;

private:
  OCTAVE_API SparseMatrix
  dinverse (MatrixType& mattype, octave_idx_type& info, double& rcond,
            const bool force = false, const bool calccond = true) const;

  OCTAVE_API SparseMatrix
  tinverse (MatrixType& mattype, octave_idx_type& info, double& rcond,
            const bool force = false, const bool calccond = true) const;

public:
  OCTAVE_API SparseMatrix inverse (void) const;
  OCTAVE_API SparseMatrix inverse (MatrixType& mattype) const;
  OCTAVE_API SparseMatrix
  inverse (MatrixType& mattype, octave_idx_type& info) const;
  OCTAVE_API SparseMatrix
  inverse (MatrixType& mattype, octave_idx_type& info, double& rcond,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API DET determinant (void) const;
  OCTAVE_API DET determinant (octave_idx_type& info) const;
  OCTAVE_API DET determinant (octave_idx_type& info, double& rcond,
                              bool calc_cond = true) const;

private:
  // Diagonal matrix solvers
  OCTAVE_API Matrix
  dsolve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API ComplexMatrix
  dsolve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API SparseMatrix
  dsolve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API SparseComplexMatrix
  dsolve (MatrixType& typ, const SparseComplexMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  // Upper triangular matrix solvers
  OCTAVE_API Matrix
  utsolve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  OCTAVE_API ComplexMatrix
  utsolve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  OCTAVE_API SparseMatrix
  utsolve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  OCTAVE_API SparseComplexMatrix
  utsolve (MatrixType& typ, const SparseComplexMatrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  // Lower triangular matrix solvers
  OCTAVE_API Matrix
  ltsolve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  OCTAVE_API ComplexMatrix
  ltsolve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  OCTAVE_API SparseMatrix
  ltsolve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  OCTAVE_API SparseComplexMatrix
  ltsolve (MatrixType& typ, const SparseComplexMatrix& b, octave_idx_type& info,
           double& rcond, solve_singularity_handler sing_handler,
           bool calc_cond = false) const;

  // Tridiagonal matrix solvers
  OCTAVE_API Matrix
  trisolve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
            double& rcond, solve_singularity_handler sing_handler,
            bool calc_cond = false) const;

  OCTAVE_API ComplexMatrix
  trisolve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
            double& rcond, solve_singularity_handler sing_handler,
            bool calc_cond = false) const;

  OCTAVE_API SparseMatrix
  trisolve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
            double& rcond, solve_singularity_handler sing_handler,
            bool calc_cond = false) const;

  OCTAVE_API SparseComplexMatrix
  trisolve (MatrixType& typ, const SparseComplexMatrix& b, octave_idx_type& info,
            double& rcond, solve_singularity_handler sing_handler,
            bool calc_cond = false) const;

  // Banded matrix solvers (umfpack/cholesky)
  OCTAVE_API Matrix
  bsolve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API ComplexMatrix
  bsolve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API SparseMatrix
  bsolve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API SparseComplexMatrix
  bsolve (MatrixType& typ, const SparseComplexMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  // Full matrix solvers (umfpack/cholesky)
  OCTAVE_API void *
  factorize (octave_idx_type& err, double& rcond, Matrix& Control,
             Matrix& Info, solve_singularity_handler sing_handler,
             bool calc_cond = false) const;

  OCTAVE_API Matrix
  fsolve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API ComplexMatrix
  fsolve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API SparseMatrix
  fsolve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

  OCTAVE_API SparseComplexMatrix
  fsolve (MatrixType& typ, const SparseComplexMatrix& b, octave_idx_type& info,
          double& rcond, solve_singularity_handler sing_handler,
          bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  OCTAVE_API Matrix solve (MatrixType& typ, const Matrix& b) const;
  OCTAVE_API Matrix
  solve (MatrixType& typ, const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API Matrix
  solve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API Matrix
  solve (MatrixType& typ, const Matrix& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API ComplexMatrix
  solve (MatrixType& typ, const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& typ, const ComplexMatrix& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API SparseMatrix solve (MatrixType& typ, const SparseMatrix& b) const;
  OCTAVE_API SparseMatrix
  solve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info) const;
  OCTAVE_API SparseMatrix
  solve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API SparseMatrix
  solve (MatrixType& typ, const SparseMatrix& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& typ, const SparseComplexMatrix& b) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& typ, const SparseComplexMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& typ, const SparseComplexMatrix& b,
         octave_idx_type& info, double& rcond) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& typ, const SparseComplexMatrix& b,
         octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler,
         bool singular_fallabck = true) const;

  OCTAVE_API ColumnVector solve (MatrixType& typ, const ColumnVector& b) const;
  OCTAVE_API ColumnVector
  solve (MatrixType& typ, const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ColumnVector
  solve (MatrixType& typ, const ColumnVector& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API ColumnVector
  solve (MatrixType& typ, const ColumnVector& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler) const;

  OCTAVE_API ComplexColumnVector
  solve (MatrixType& typ, const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& typ, const ComplexColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& typ, const ComplexColumnVector& b,
         octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& typ, const ComplexColumnVector& b,
         octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  // Generic interface to solver with probing of type
  OCTAVE_API Matrix solve (const Matrix& b) const;
  OCTAVE_API Matrix solve (const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API Matrix
  solve (const Matrix& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API Matrix
  solve (const Matrix& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API ComplexMatrix solve (const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API SparseMatrix solve (const SparseMatrix& b) const;
  OCTAVE_API SparseMatrix
  solve (const SparseMatrix& b, octave_idx_type& info) const;
  OCTAVE_API SparseMatrix
  solve (const SparseMatrix& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API SparseMatrix
  solve (const SparseMatrix& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API SparseComplexMatrix solve (const SparseComplexMatrix& b) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseComplexMatrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseComplexMatrix& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler) const;

  OCTAVE_API ColumnVector solve (const ColumnVector& b) const;
  OCTAVE_API ColumnVector
  solve (const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API ColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API ComplexColumnVector solve (const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler) const;

  // other operations

  OCTAVE_API bool any_element_is_negative (bool = false) const;
  OCTAVE_API bool any_element_is_nan (void) const;
  OCTAVE_API bool any_element_is_inf_or_nan (void) const;
  OCTAVE_API bool any_element_not_one_or_zero (void) const;
  OCTAVE_API bool all_elements_are_zero (void) const;
  OCTAVE_API bool all_elements_are_int_or_inf_or_nan (void) const;
  OCTAVE_API bool all_integers (double& max_val, double& min_val) const;
  OCTAVE_API bool too_large_for_float (void) const;

  OCTAVE_API SparseBoolMatrix operator ! (void) const;

  OCTAVE_API SparseBoolMatrix all (int dim = -1) const;
  OCTAVE_API SparseBoolMatrix any (int dim = -1) const;

  OCTAVE_API SparseMatrix cumprod (int dim = -1) const;
  OCTAVE_API SparseMatrix cumsum (int dim = -1) const;
  OCTAVE_API SparseMatrix prod (int dim = -1) const;
  OCTAVE_API SparseMatrix sum (int dim = -1) const;
  OCTAVE_API SparseMatrix sumsq (int dim = -1) const;
  OCTAVE_API SparseMatrix abs (void) const;

  OCTAVE_API SparseMatrix diag (octave_idx_type k = 0) const;

  OCTAVE_API Matrix matrix_value (void) const;

  OCTAVE_API SparseMatrix squeeze (void) const;

  OCTAVE_API SparseMatrix reshape (const dim_vector& new_dims) const;

  OCTAVE_API SparseMatrix
  permute (const Array<octave_idx_type>& vec, bool inv = false) const;

  OCTAVE_API SparseMatrix ipermute (const Array<octave_idx_type>& vec) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const SparseMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               SparseMatrix& a);

};

// Publish externally used friend functions.

extern OCTAVE_API SparseMatrix real (const SparseComplexMatrix& a);
extern OCTAVE_API SparseMatrix imag (const SparseComplexMatrix& a);

// Other operators.

extern OCTAVE_API SparseMatrix operator * (const SparseMatrix& a,
                                           const SparseMatrix& b);
extern OCTAVE_API Matrix operator * (const Matrix& a,
                                     const SparseMatrix& b);
extern OCTAVE_API Matrix mul_trans (const Matrix& a,
                                    const SparseMatrix& b);
extern OCTAVE_API Matrix operator * (const SparseMatrix& a,
                                     const Matrix& b);
extern OCTAVE_API Matrix trans_mul (const SparseMatrix& a,
                                    const Matrix& b);

extern OCTAVE_API SparseMatrix operator * (const DiagMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator * (const SparseMatrix&,
                                           const DiagMatrix&);

extern OCTAVE_API SparseMatrix operator + (const DiagMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator + (const SparseMatrix&,
                                           const DiagMatrix&);
extern OCTAVE_API SparseMatrix operator - (const DiagMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator - (const SparseMatrix&,
                                           const DiagMatrix&);

extern OCTAVE_API SparseMatrix operator * (const PermMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator * (const SparseMatrix&,
                                           const PermMatrix&);

extern OCTAVE_API SparseMatrix min (double d, const SparseMatrix& m);
extern OCTAVE_API SparseMatrix min (const SparseMatrix& m, double d);
extern OCTAVE_API SparseMatrix min (const SparseMatrix& a,
                                    const SparseMatrix& b);

extern OCTAVE_API SparseMatrix max (double d, const SparseMatrix& m);
extern OCTAVE_API SparseMatrix max (const SparseMatrix& m, double d);
extern OCTAVE_API SparseMatrix max (const SparseMatrix& a,
                                    const SparseMatrix& b);

SPARSE_SMS_CMP_OP_DECLS (SparseMatrix, double, OCTAVE_API)
SPARSE_SMS_BOOL_OP_DECLS (SparseMatrix, double, OCTAVE_API)

SPARSE_SSM_CMP_OP_DECLS (double, SparseMatrix, OCTAVE_API)
SPARSE_SSM_BOOL_OP_DECLS (double, SparseMatrix, OCTAVE_API)

SPARSE_SMSM_CMP_OP_DECLS (SparseMatrix, SparseMatrix, OCTAVE_API)
SPARSE_SMSM_BOOL_OP_DECLS (SparseMatrix, SparseMatrix, OCTAVE_API)

SPARSE_FORWARD_DEFS (MSparse, SparseMatrix, Matrix, double)

#endif
