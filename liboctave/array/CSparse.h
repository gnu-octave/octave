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

#if ! defined (octave_CSparse_h)
#define octave_CSparse_h 1

#include "octave-config.h"

#include "CColVector.h"
#include "CMatrix.h"
#include "CNDArray.h"
#include "DET.h"
#include "MSparse.h"
#include "MatrixType.h"
#include "Sparse-op-decls.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dNDArray.h"
#include "oct-cmplx.h"
#include "mx-fwd.h"

class
OCTAVE_API
SparseComplexMatrix : public MSparse<Complex>
{
public:

  // Corresponding dense matrix type for this sparse matrix type.
  typedef ComplexMatrix dense_matrix_type;

  typedef void (*solve_singularity_handler) (double rcond);

  SparseComplexMatrix (void) : MSparse<Complex> () { }

  SparseComplexMatrix (octave_idx_type r,
                       octave_idx_type c) : MSparse<Complex> (r, c) { }

  SparseComplexMatrix (const dim_vector& dv, octave_idx_type nz = 0)
    : MSparse<Complex> (dv, nz) { }

  explicit SparseComplexMatrix (octave_idx_type r, octave_idx_type c,
                                Complex val)
    : MSparse<Complex> (r, c, val) { }

  SparseComplexMatrix (octave_idx_type r, octave_idx_type c, double val)
    : MSparse<Complex> (r, c, Complex (val)) { }

  SparseComplexMatrix (const SparseComplexMatrix& a)
    : MSparse<Complex> (a) { }

  SparseComplexMatrix (const SparseComplexMatrix& a, const dim_vector& dv)
    : MSparse<Complex> (a, dv) { }

  SparseComplexMatrix (const MSparse<Complex>& a) : MSparse<Complex> (a) { }

  SparseComplexMatrix (const Sparse<Complex>& a) : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const ComplexMatrix& a)
    : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const ComplexNDArray& a)
    : MSparse<Complex> (a) { }

  SparseComplexMatrix (const Array<Complex>& a, const octave::idx_vector& r,
                       const octave::idx_vector& c, octave_idx_type nr = -1,
                       octave_idx_type nc = -1, bool sum_terms = true,
                       octave_idx_type nzm = -1)
    : MSparse<Complex> (a, r, c, nr, nc, sum_terms, nzm) { }

  explicit OCTAVE_API SparseComplexMatrix (const SparseMatrix& a);

  explicit OCTAVE_API SparseComplexMatrix (const SparseBoolMatrix& a);

  explicit OCTAVE_API SparseComplexMatrix (const ComplexDiagMatrix& a);

  SparseComplexMatrix (octave_idx_type r, octave_idx_type c,
                       octave_idx_type num_nz)
    : MSparse<Complex> (r, c, num_nz) { }

  SparseComplexMatrix& operator = (const SparseComplexMatrix& a)
  {
    MSparse<Complex>::operator = (a);
    return *this;
  }

  OCTAVE_API bool operator == (const SparseComplexMatrix& a) const;
  OCTAVE_API bool operator != (const SparseComplexMatrix& a) const;

  OCTAVE_API bool ishermitian (void) const;

  OCTAVE_API SparseComplexMatrix max (int dim = -1) const;
  OCTAVE_API SparseComplexMatrix
  max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API SparseComplexMatrix min (int dim = -1) const;
  OCTAVE_API SparseComplexMatrix
  min (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API SparseComplexMatrix&
  insert (const SparseComplexMatrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API SparseComplexMatrix&
  insert (const SparseMatrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API SparseComplexMatrix&
  insert (const SparseComplexMatrix& a, const Array<octave_idx_type>& indx);
  OCTAVE_API SparseComplexMatrix&
  insert (const SparseMatrix& a, const Array<octave_idx_type>& indx);

  OCTAVE_API SparseComplexMatrix
  concat (const SparseComplexMatrix& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API SparseComplexMatrix
  concat (const SparseMatrix& rb, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API ComplexMatrix matrix_value (void) const;

  OCTAVE_API SparseComplexMatrix hermitian (void) const;  // complex conjugate transpose
  SparseComplexMatrix transpose (void) const
  { return MSparse<Complex>::transpose (); }

  friend OCTAVE_API SparseComplexMatrix conj (const SparseComplexMatrix& a);

  // extract row or column i.

  OCTAVE_API ComplexRowVector row (octave_idx_type i) const;

  OCTAVE_API ComplexColumnVector column (octave_idx_type i) const;

private:
  SparseComplexMatrix dinverse (MatrixType& mattype, octave_idx_type& info,
                                double& rcond, const bool force = false,
                                const bool calccond = true) const;

  SparseComplexMatrix tinverse (MatrixType& mattype, octave_idx_type& info,
                                double& rcond, const bool force = false,
                                const bool calccond = true) const;

public:
  OCTAVE_API SparseComplexMatrix inverse (void) const;
  OCTAVE_API SparseComplexMatrix inverse (MatrixType& mattype) const;
  OCTAVE_API SparseComplexMatrix
  inverse (MatrixType& mattype, octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  inverse (MatrixType& mattype, octave_idx_type& info, double& rcond,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API ComplexDET determinant (void) const;
  OCTAVE_API ComplexDET determinant (octave_idx_type& info) const;
  OCTAVE_API ComplexDET
  determinant (octave_idx_type& info, double& rcond,
               bool calc_cond = true) const;

private:
  // Diagonal matrix solvers
  ComplexMatrix dsolve (MatrixType& mattype, const Matrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  ComplexMatrix dsolve (MatrixType& mattype, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix dsolve (MatrixType& mattype, const SparseMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  SparseComplexMatrix dsolve (MatrixType& mattype, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  // Upper triangular matrix solvers
  ComplexMatrix utsolve (MatrixType& mattype, const Matrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  ComplexMatrix utsolve (MatrixType& mattype, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseComplexMatrix utsolve (MatrixType& mattype, const SparseMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  SparseComplexMatrix utsolve (MatrixType& mattype, const SparseComplexMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  // Lower triangular matrix solvers
  ComplexMatrix ltsolve (MatrixType& mattype, const Matrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  ComplexMatrix ltsolve (MatrixType& mattype, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseComplexMatrix ltsolve (MatrixType& mattype, const SparseMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  SparseComplexMatrix ltsolve (MatrixType& mattype, const SparseComplexMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  // Tridiagonal matrix solvers
  ComplexMatrix trisolve (MatrixType& mattype, const Matrix& b,
                          octave_idx_type& info, double& rcond,
                          solve_singularity_handler sing_handler,
                          bool calc_cond = false) const;

  ComplexMatrix trisolve (MatrixType& mattype, const ComplexMatrix& b,
                          octave_idx_type& info, double& rcond,
                          solve_singularity_handler sing_handler,
                          bool calc_cond = false) const;

  SparseComplexMatrix trisolve (MatrixType& mattype, const SparseMatrix& b,
                                octave_idx_type& info, double& rcond,
                                solve_singularity_handler sing_handler,
                                bool calc_cond = false) const;

  SparseComplexMatrix trisolve (MatrixType& mattype, const SparseComplexMatrix& b,
                                octave_idx_type& info, double& rcond,
                                solve_singularity_handler sing_handler,
                                bool calc_cond = false) const;

  // Banded matrix solvers (umfpack/cholesky)
  ComplexMatrix bsolve (MatrixType& mattype, const Matrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  ComplexMatrix bsolve (MatrixType& mattype, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix bsolve (MatrixType& mattype, const SparseMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  SparseComplexMatrix bsolve (MatrixType& mattype, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  // Full matrix solvers (umfpack/cholesky)
  void * factorize (octave_idx_type& err, double& rcond, Matrix& Control,
                    Matrix& Info, solve_singularity_handler sing_handler,
                    bool calc_cond) const;

  ComplexMatrix fsolve (MatrixType& mattype, const Matrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  ComplexMatrix fsolve (MatrixType& mattype, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix fsolve (MatrixType& mattype, const SparseMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  SparseComplexMatrix fsolve (MatrixType& mattype, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  OCTAVE_API ComplexMatrix solve (MatrixType& mattype, const Matrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b,
         octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b,
         octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseMatrix& b) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseMatrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseMatrix& b, octave_idx_type& info,
         double& rcond, solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseComplexMatrix& b) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseComplexMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseComplexMatrix& b,
         octave_idx_type& info, double& rcond) const;
  OCTAVE_API SparseComplexMatrix
  solve (MatrixType& mattype, const SparseComplexMatrix& b,
         octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler,
         bool singular_fallback = true) const;

  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b,
         octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b,
         octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  // Generic interface to solver with probing of type
  OCTAVE_API ComplexMatrix solve (const Matrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (const Matrix& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexMatrix
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

  OCTAVE_API SparseComplexMatrix solve (const SparseMatrix& b) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseMatrix& b, octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseMatrix& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseMatrix& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API SparseComplexMatrix solve (const SparseComplexMatrix& b) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseComplexMatrix& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API SparseComplexMatrix
  solve (const SparseComplexMatrix& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API ComplexColumnVector solve (const ColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcond) const;
  OCTAVE_API ComplexColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API ComplexColumnVector solve (const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info,
         double& rcond) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info, double& rcond,
         solve_singularity_handler sing_handler) const;

  OCTAVE_API SparseComplexMatrix squeeze (void) const;

  OCTAVE_API SparseComplexMatrix reshape (const dim_vector& new_dims) const;

  OCTAVE_API SparseComplexMatrix
  permute (const Array<octave_idx_type>& vec, bool inv = false) const;

  OCTAVE_API SparseComplexMatrix
  ipermute (const Array<octave_idx_type>& vec) const;

  OCTAVE_API bool any_element_is_nan (void) const;
  OCTAVE_API bool any_element_is_inf_or_nan (void) const;
  OCTAVE_API bool all_elements_are_real (void) const;
  OCTAVE_API bool all_integers (double& max_val, double& min_val) const;
  OCTAVE_API bool too_large_for_float (void) const;

  OCTAVE_API SparseBoolMatrix operator ! (void) const;

  OCTAVE_API SparseBoolMatrix all (int dim = -1) const;
  OCTAVE_API SparseBoolMatrix any (int dim = -1) const;

  OCTAVE_API SparseComplexMatrix cumprod (int dim = -1) const;
  OCTAVE_API SparseComplexMatrix cumsum (int dim = -1) const;
  OCTAVE_API SparseComplexMatrix prod (int dim = -1) const;
  OCTAVE_API SparseComplexMatrix sum (int dim = -1) const;
  OCTAVE_API SparseComplexMatrix sumsq (int dim = -1) const;
  OCTAVE_API SparseMatrix abs (void) const;

  OCTAVE_API SparseComplexMatrix diag (octave_idx_type k = 0) const;

  // i/o
  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const SparseComplexMatrix& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, SparseComplexMatrix& a);
};

extern OCTAVE_API SparseComplexMatrix
operator * (const SparseMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator * (const SparseComplexMatrix&, const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator * (const SparseComplexMatrix&, const SparseComplexMatrix&);

extern OCTAVE_API ComplexMatrix
operator * (const Matrix&, const SparseComplexMatrix&);
extern OCTAVE_API ComplexMatrix
operator * (const ComplexMatrix&, const SparseMatrix&);
extern OCTAVE_API ComplexMatrix
operator * (const ComplexMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API ComplexMatrix
mul_trans (const ComplexMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API ComplexMatrix
mul_herm (const ComplexMatrix&, const SparseComplexMatrix&);

extern OCTAVE_API ComplexMatrix
operator * (const SparseMatrix&, const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix
operator * (const SparseComplexMatrix&, const Matrix&);
extern OCTAVE_API ComplexMatrix
operator * (const SparseComplexMatrix&, const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix
trans_mul (const SparseComplexMatrix&, const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix
herm_mul (const SparseComplexMatrix&, const ComplexMatrix&);

extern OCTAVE_API SparseComplexMatrix
operator * (const DiagMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator * (const SparseComplexMatrix&, const DiagMatrix&);

extern OCTAVE_API SparseComplexMatrix
operator * (const ComplexDiagMatrix&, const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator * (const SparseMatrix&, const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix
operator * (const ComplexDiagMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator * (const SparseComplexMatrix&, const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix
operator + (const ComplexDiagMatrix&, const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator + (const DiagMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator + (const ComplexDiagMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator + (const SparseMatrix&, const ComplexDiagMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator + (const SparseComplexMatrix&, const DiagMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator + (const SparseComplexMatrix&, const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix
operator - (const ComplexDiagMatrix&, const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator - (const DiagMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator - (const ComplexDiagMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator - (const SparseMatrix&, const ComplexDiagMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator - (const SparseComplexMatrix&, const DiagMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator - (const SparseComplexMatrix&, const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix
operator * (const PermMatrix&, const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix
operator * (const SparseComplexMatrix&, const PermMatrix&);

extern OCTAVE_API SparseComplexMatrix
min (const Complex& c, const SparseComplexMatrix& m);
extern OCTAVE_API SparseComplexMatrix
min (const SparseComplexMatrix& m, const Complex& c);
extern OCTAVE_API SparseComplexMatrix
min (const SparseComplexMatrix& a, const SparseComplexMatrix& b);

extern OCTAVE_API SparseComplexMatrix
max (const Complex& c, const SparseComplexMatrix& m);
extern OCTAVE_API SparseComplexMatrix
max (const SparseComplexMatrix& m, const Complex& c);
extern OCTAVE_API SparseComplexMatrix
max (const SparseComplexMatrix& a, const SparseComplexMatrix& b);

SPARSE_SMS_CMP_OP_DECLS (SparseComplexMatrix, Complex, OCTAVE_API)
SPARSE_SMS_BOOL_OP_DECLS (SparseComplexMatrix, Complex, OCTAVE_API)

SPARSE_SSM_CMP_OP_DECLS (Complex, SparseComplexMatrix, OCTAVE_API)
SPARSE_SSM_BOOL_OP_DECLS (Complex, SparseComplexMatrix, OCTAVE_API)

SPARSE_SMSM_CMP_OP_DECLS (SparseComplexMatrix, SparseComplexMatrix, OCTAVE_API)
SPARSE_SMSM_BOOL_OP_DECLS (SparseComplexMatrix, SparseComplexMatrix, OCTAVE_API)

SPARSE_FORWARD_DEFS (MSparse, SparseComplexMatrix, ComplexMatrix, Complex)

#endif
