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

#if ! defined (octave_sparse_chol_h)
#define octave_sparse_chol_h 1

#include "octave-config.h"

#include <memory>

#include "mx-fwd.h"

#include "CSparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

// If the sparse matrix classes become templated on the element type
// (i.e., sparse_matrix<double>), then it might be best to make the
// template parameter of this class also be the element type instead
// of the matrix type.

template <typename chol_type>
class
OCTAVE_API
sparse_chol
{
public:

  sparse_chol (void);

  sparse_chol (const chol_type& a, bool natural, bool force);

  sparse_chol (const chol_type& a, octave_idx_type& info,
               bool natural, bool force);

  sparse_chol (const chol_type& a, octave_idx_type& info, bool natural);

  sparse_chol (const chol_type& a, octave_idx_type& info);

  sparse_chol (const sparse_chol<chol_type>& a) = default;

  virtual ~sparse_chol (void) = default;

  sparse_chol<chol_type>&
  operator = (const sparse_chol<chol_type>& a) = default;

  chol_type L (void) const;

  chol_type R (void) const { return L ().hermitian (); }

  octave_idx_type P (void) const;

  RowVector perm (void) const;

  SparseMatrix Q (void) const;

  bool is_positive_definite (void) const;

  double rcond (void) const;

  chol_type inverse (void) const;

protected:

  typedef typename chol_type::element_type chol_elt;

  class sparse_chol_rep;

private:

  std::shared_ptr<sparse_chol_rep> m_rep;
};

template <typename chol_type>
OCTAVE_API
chol_type
chol2inv (const chol_type& r);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
