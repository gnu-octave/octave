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

#if ! defined (octave_sparse_lu_h)
#define octave_sparse_lu_h 1

#include "octave-config.h"

#include "mx-fwd.h"

#include "MArray.h"
#include "dMatrix.h"
#include "dSparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

// If the sparse matrix classes become templated on the element type
// (i.e., sparse_matrix<double>), then it might be best to make the
// template parameter of this class also be the element type instead
// of the matrix type.

template <typename lu_type>
class
OCTAVE_API
sparse_lu
{
public:

  typedef typename lu_type::element_type lu_elt_type;

  sparse_lu (void)
    : m_L (), m_U (), m_R (), m_cond (0), m_P (), m_Q () { }

  OCTAVE_API
  sparse_lu (const lu_type& a, const Matrix& piv_thres = Matrix (),
             bool scale = false);

  OCTAVE_API
  sparse_lu (const lu_type& a, const ColumnVector& Qinit,
             const Matrix& piv_thres, bool scale = false,
             bool FixedQ = false, double droptol = -1.0,
             bool milu = false, bool udiag = false);

  sparse_lu (const sparse_lu& a)
    : m_L (a.m_L), m_U (a.m_U), m_R (),
      m_cond (a.m_cond), m_P (a.m_P), m_Q (a.m_Q)
  { }

  sparse_lu& operator = (const sparse_lu& a)
  {
    if (this != &a)
      {
        m_L = a.m_L;
        m_U = a.m_U;
        m_cond = a.m_cond;
        m_P = a.m_P;
        m_Q = a.m_Q;
      }

    return *this;
  }

  virtual ~sparse_lu (void) = default;

  lu_type L (void) const { return m_L; }

  lu_type U (void) const { return m_U; }

  SparseMatrix R (void) const { return m_R; }

  OCTAVE_API lu_type Y (void) const;

  OCTAVE_API SparseMatrix Pc (void) const;

  OCTAVE_API SparseMatrix Pr (void) const;

  OCTAVE_API ColumnVector Pc_vec (void) const;

  OCTAVE_API ColumnVector Pr_vec (void) const;

  OCTAVE_API PermMatrix Pc_mat (void) const;

  OCTAVE_API PermMatrix Pr_mat (void) const;

  const octave_idx_type * row_perm (void) const { return m_P.data (); }

  const octave_idx_type * col_perm (void) const { return m_Q.data (); }

  double rcond (void) const { return m_cond; }

protected:

  lu_type m_L;
  lu_type m_U;
  SparseMatrix m_R;

  double m_cond;

  MArray<octave_idx_type> m_P;
  MArray<octave_idx_type> m_Q;
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
