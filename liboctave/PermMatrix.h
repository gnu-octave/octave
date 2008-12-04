/*

Copyright (C) 2008 Jaroslav Hajek

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

#if !defined (octave_PermMatrix_h)
#define octave_PermMatrix_h 1

#include "Array.h"
#include "mx-defs.h"

class PermMatrix : public Array<octave_idx_type>
{
private:

  octave_idx_type get (octave_idx_type i) const { return Array<octave_idx_type>::xelem (i); }

public:

  PermMatrix (void) : Array<octave_idx_type> (), _colp (false) { }

  PermMatrix (octave_idx_type n);

  PermMatrix (const Array<octave_idx_type>& p, bool colp = false, 
              bool check = true);

  PermMatrix (const PermMatrix& m)
    : Array<octave_idx_type> (m), _colp(m._colp) 
    { this->dimensions = m.dims (); }
  
  PermMatrix (const idx_vector& idx, bool colp = false, octave_idx_type n = 0); 

  octave_idx_type 
  elem (octave_idx_type i, octave_idx_type j) const
    {
      return (_colp 
              ? ((get(j) != i) ? 1 : 0)
              : ((get(i) != j) ? 1 : 0));
    }

  octave_idx_type 
  checkelem (octave_idx_type i, octave_idx_type j) const;

  octave_idx_type
  operator () (octave_idx_type i, octave_idx_type j) const
    {
#if defined (BOUNDS_CHECKING)
      return checkelem (i, j);
#else
      return elem (i, j);
#endif
    }
  
  // These are, in fact, super-fast.
  PermMatrix transpose (void) const;
  PermMatrix inverse (void) const;

  // Determinant, i.e. the sign of permutation.
  octave_idx_type determinant (void) const;

  bool is_col_perm (void) const { return _colp; }
  bool is_row_perm (void) const { return !_colp; }

  friend PermMatrix operator *(const PermMatrix& a, const PermMatrix& b);

private:
  bool _colp;
};

// Multiplying permutations together.
PermMatrix 
operator *(const PermMatrix& a, const PermMatrix& b);

#endif
