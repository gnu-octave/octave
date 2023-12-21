////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
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

#if ! defined (octave_MatrixType_h)
#define octave_MatrixType_h 1

#include "octave-config.h"

#include "mx-fwd.h"

#include "MSparse.h"

class MatrixType
{
public:
  enum matrix_type
  {
    Unknown = 0,
    Full,
    Diagonal,
    Permuted_Diagonal,
    Upper,
    Lower,
    Permuted_Upper,
    Permuted_Lower,
    Banded,
    Hermitian,
    Banded_Hermitian,
    Tridiagonal,
    Tridiagonal_Hermitian,
    Rectangular
  };

  OCTAVE_API MatrixType ();

  OCTAVE_API MatrixType (const MatrixType& a);

  OCTAVE_API MatrixType (const Matrix& a);

  OCTAVE_API MatrixType (const ComplexMatrix& a);

  OCTAVE_API MatrixType (const FloatMatrix& a);

  OCTAVE_API MatrixType (const FloatComplexMatrix& a);

  template <typename T>
  OCTAVE_API
  MatrixType (const MSparse<T>& a);

  OCTAVE_API MatrixType (const matrix_type t, bool _full = false);

  OCTAVE_API MatrixType (const matrix_type t, const octave_idx_type np,
                         const octave_idx_type *p, bool _full = false);

  OCTAVE_API MatrixType (const matrix_type t, const octave_idx_type ku,
                         const octave_idx_type kl, bool _full = false);

  OCTAVE_API ~MatrixType ();

  OCTAVE_API MatrixType& operator = (const MatrixType& a);

  OCTAVE_API int type (bool quiet = true);

  OCTAVE_API int type (const Matrix& a);

  OCTAVE_API int type (const ComplexMatrix& a);

  OCTAVE_API int type (const FloatMatrix& a);

  OCTAVE_API int type (const FloatComplexMatrix& a);

  OCTAVE_API int type (const SparseMatrix& a);

  OCTAVE_API int type (const SparseComplexMatrix& a);

  double band_density () const { return m_bandden; }

  int nupper () const { return m_upper_band; }

  int nlower () const { return m_lower_band; }

  bool is_dense () const { return m_dense; }

  bool isdiag () const
  { return (m_type == Diagonal || m_type == Permuted_Diagonal); }

  bool istriu () const
  { return (m_type == Upper || m_type == Permuted_Upper); }

  bool istril () const
  { return (m_type == Lower || m_type == Permuted_Lower); }

  bool isbanded () const
  { return (m_type == Banded || m_type == Banded_Hermitian); }

  bool is_tridiagonal () const
  { return (m_type == Tridiagonal || m_type == Tridiagonal_Hermitian); }

  bool ishermitian () const
  {
    return (m_type == Banded_Hermitian || m_type == Tridiagonal_Hermitian
            || m_type == Hermitian);
  }

  bool is_rectangular () const { return (m_type == Rectangular); }

  bool is_known () const { return (m_type != Unknown); }

  bool is_unknown () const { return (m_type == Unknown); }

  OCTAVE_API void info () const;

  octave_idx_type * triangular_perm () const { return m_perm; }

  void invalidate_type () { m_type = Unknown; }

  void mark_as_diagonal () { m_type = Diagonal; }

  void mark_as_permuted_diagonal () { m_type = Permuted_Diagonal; }

  void mark_as_upper_triangular () { m_type = Upper; }

  void mark_as_lower_triangular () { m_type = Lower; }

  void mark_as_tridiagonal () {m_type = Tridiagonal; }

  void mark_as_banded (const octave_idx_type ku, const octave_idx_type kl)
  { m_type = Banded; m_upper_band = ku; m_lower_band = kl; }

  void mark_as_full () { m_type = Full; }

  void mark_as_rectangular () { m_type = Rectangular; }

  void mark_as_dense () { m_dense = true; }

  void mark_as_not_dense () { m_dense = false; }

  OCTAVE_API void mark_as_symmetric ();

  OCTAVE_API void mark_as_unsymmetric ();

  OCTAVE_API void mark_as_permuted (const octave_idx_type np, const octave_idx_type *p);

  OCTAVE_API void mark_as_unpermuted ();

  OCTAVE_API MatrixType transpose () const;

private:
  void type (int new_typ) { m_type = static_cast<matrix_type> (new_typ); }

  matrix_type m_type;
  double m_sp_bandden;
  double m_bandden;
  octave_idx_type m_upper_band;
  octave_idx_type m_lower_band;
  bool m_dense;
  bool m_full;
  octave_idx_type m_nperm;
  octave_idx_type *m_perm;
};

#endif
