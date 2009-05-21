/*

Copyright (C) 2008, 2009 Jaroslav Hajek

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

// Array<T> is inherited privately so that some methods, like index, don't
// produce unexpected results.

class OCTAVE_API PermMatrix : protected Array<octave_idx_type>
{

public:

  PermMatrix (void) : Array<octave_idx_type> (), _colp (false) { }

  PermMatrix (octave_idx_type n);

  PermMatrix (const Array<octave_idx_type>& p, bool colp = false, 
              bool check = true);

  PermMatrix (const PermMatrix& m)
    : Array<octave_idx_type> (m), _colp(m._colp) { }
  
  PermMatrix (const idx_vector& idx, bool colp = false, octave_idx_type n = 0); 

  octave_idx_type dim1 (void) const 
    { return Array<octave_idx_type>::length (); }
  octave_idx_type dim2 (void) const 
    { return Array<octave_idx_type>::length (); }

  octave_idx_type rows (void) const { return dim1 (); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }

  octave_idx_type perm_length (void) const 
    { return Array<octave_idx_type>::length (); }
  octave_idx_type length (void) const 
    { return dim1 () * dim2 (); }
  octave_idx_type nelem (void) const { return dim1 () * dim2 (); }
  octave_idx_type numel (void) const { return nelem (); }

  size_t byte_size (void) const { return perm_length () * sizeof (octave_idx_type); }

  dim_vector dims (void) const { return dim_vector (dim1 (), dim2 ()); }

  Array<octave_idx_type> pvec (void) const
    { return *this; }

  octave_idx_type 
  elem (octave_idx_type i, octave_idx_type j) const
    {
      return (_colp 
              ? ((Array<octave_idx_type>::elem (j) == i) ? 1 : 0)
              : ((Array<octave_idx_type>::elem (i) == j) ? 1 : 0));
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

  // Efficient integer power of a permutation.
  PermMatrix power (octave_idx_type n) const;

  bool is_col_perm (void) const { return _colp; }
  bool is_row_perm (void) const { return !_colp; }

  friend OCTAVE_API PermMatrix operator *(const PermMatrix& a, const PermMatrix& b);

  const octave_idx_type *data (void) const 
    { return Array<octave_idx_type>::data (); }

  const octave_idx_type *fortran_vec (void) const 
    { return Array<octave_idx_type>::fortran_vec (); }

  octave_idx_type *fortran_vec (void) 
    { return Array<octave_idx_type>::fortran_vec (); }

  void print_info (std::ostream& os, const std::string& prefix) const
    { Array<octave_idx_type>::print_info (os, prefix); }

private:
  bool _colp;
};

// Multiplying permutations together.
PermMatrix 
OCTAVE_API
operator *(const PermMatrix& a, const PermMatrix& b);

#endif
