////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#if ! defined (octave_PermMatrix_h)
#define octave_PermMatrix_h 1

#include "octave-config.h"

#include "Array.h"
#include "mx-defs.h"

// Array<T> is inherited privately so that some methods, like index, don't
// produce unexpected results.

class OCTAVE_API PermMatrix : protected Array<octave_idx_type>
{
public:

  PermMatrix (void) = default;

  PermMatrix (const PermMatrix& m) = default;

  PermMatrix& operator = (const PermMatrix& m) = default;

  ~PermMatrix (void) = default;

  OCTAVE_API PermMatrix (octave_idx_type n);

  OCTAVE_API PermMatrix (const Array<octave_idx_type>& p, bool colp,
                         bool check = true);

  OCTAVE_API PermMatrix (const octave::idx_vector& idx, bool colp,
                         octave_idx_type n = 0);

  octave_idx_type dim1 (void) const
  { return Array<octave_idx_type>::numel (); }
  octave_idx_type dim2 (void) const
  { return Array<octave_idx_type>::numel (); }

  octave_idx_type rows (void) const { return dim1 (); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }

  octave_idx_type perm_length (void) const
  { return Array<octave_idx_type>::numel (); }
  // FIXME: a dangerous ambiguity?
  octave_idx_type length (void) const
  { return perm_length (); }

  octave_idx_type numel (void) const { return dim1 () * dim2 (); }

  std::size_t byte_size (void) const
  { return Array<octave_idx_type>::byte_size (); }

  dim_vector dims (void) const { return dim_vector (dim1 (), dim2 ()); }

  bool isempty (void) const { return numel () == 0; }

  int ndims (void) const { return 2; }

  const Array<octave_idx_type>& col_perm_vec (void) const
  { return *this; }

  octave_idx_type
  elem (octave_idx_type i, octave_idx_type j) const
  {
    return (Array<octave_idx_type>::elem (j) == i) ? 1 : 0;
  }

  octave_idx_type
  checkelem (octave_idx_type i, octave_idx_type j) const;

  octave_idx_type
  operator () (octave_idx_type i, octave_idx_type j) const
  {
    return elem (i, j);
  }

  // These are, in fact, super-fast.
  OCTAVE_API PermMatrix transpose (void) const;
  OCTAVE_API PermMatrix inverse (void) const;

  // Determinant, i.e., the sign of permutation.
  OCTAVE_API octave_idx_type determinant (void) const;

  // Efficient integer power of a permutation.
  OCTAVE_API PermMatrix power (octave_idx_type n) const;

  bool is_col_perm (void) const { return true; }
  bool is_row_perm (void) const { return false; }

  void print_info (std::ostream& os, const std::string& prefix) const
  { Array<octave_idx_type>::print_info (os, prefix); }

  static OCTAVE_API PermMatrix eye (octave_idx_type n);

private:

  PermMatrix pos_power (octave_idx_type m) const;

  void setup (const Array<octave_idx_type>& p, bool colp, bool check);

  void setup (const octave::idx_vector& idx, bool colp, octave_idx_type n);
};

// Multiplying permutations together.
OCTAVE_API PermMatrix
operator * (const PermMatrix& a, const PermMatrix& b);

#endif
