/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_boolSparse_h)
#define octave_boolSparse_h 1

#include "Sparse.h"
#include "MSparse-defs.h"
#include "Sparse-op-defs.h"

class
SparseBoolMatrix : public Sparse<bool>
{
public:

  SparseBoolMatrix (void) : Sparse<bool> () { }

  SparseBoolMatrix (int r, int c) : Sparse<bool> (r, c) { }

  explicit SparseBoolMatrix (int r, int c, bool val) 
    : Sparse<bool> (r, c, val) { }

  SparseBoolMatrix (const Sparse<bool>& a) : Sparse<bool> (a) { }

  SparseBoolMatrix (const SparseBoolMatrix& a) : Sparse<bool> (a) { }

  SparseBoolMatrix (const SparseBoolMatrix& a, const dim_vector& dv) 
    : Sparse<bool> (a, dv) { }

  explicit SparseBoolMatrix (const boolMatrix& a) : Sparse<bool> (a) { }

  explicit SparseBoolMatrix (const boolNDArray& a) : Sparse<bool> (a) { }

  explicit SparseBoolMatrix (const Array<bool> a, const Array<int>& r, 
			     const Array<int>& c, int nr = -1, 
			     int nc = -1, bool sum_terms = true)
    : Sparse<bool> (a, r, c, nr, nc, sum_terms) { }

  explicit SparseBoolMatrix (const Array<bool> a, const Array<double>& r, 
			     const Array<double>& c, int nr = -1, 
			     int nc = -1, bool sum_terms = true)
    : Sparse<bool> (a, r, c, nr, nc, sum_terms) { }

  SparseBoolMatrix (int r, int c, int num_nz) : Sparse<bool> (r, c, num_nz) { }

  SparseBoolMatrix& operator = (const SparseBoolMatrix& a)
    {
      Sparse<bool>::operator = (a);
      return *this;
    }

  bool operator == (const SparseBoolMatrix& a) const;
  bool operator != (const SparseBoolMatrix& a) const;

  SparseBoolMatrix transpose (void) const 
    { return Sparse<bool>::transpose (); }

  // destructive insert/delete/reorder operations

  SparseBoolMatrix& insert (const SparseBoolMatrix& a, int r, int c);

  SparseBoolMatrix concat (const SparseBoolMatrix& rb, 
			   const Array<int>& ra_idx);

  boolMatrix matrix_value (void) const;

  SparseBoolMatrix squeeze (void) const;

  SparseBoolMatrix index (idx_vector& i, int resize_ok) const;

  SparseBoolMatrix index (idx_vector& i, idx_vector& j, int resize_ok) const;
  
  SparseBoolMatrix index (Array<idx_vector>& ra_idx, int resize_ok) const;

  SparseBoolMatrix reshape (const dim_vector& new_dims) const;

  SparseBoolMatrix permute (const Array<int>& vec, bool inv = false) const;

  SparseBoolMatrix ipermute (const Array<int>& vec) const;

  // unary operations

  SparseBoolMatrix operator ! (void) const;

  // other operations

  SparseBoolMatrix all (int dim = -1) const;
  SparseBoolMatrix any (int dim = -1) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const SparseBoolMatrix& a);
  friend std::istream& operator >> (std::istream& is, SparseBoolMatrix& a);
};

SPARSE_SMS_EQNE_OP_DECLS (SparseBoolMatrix, bool)
SPARSE_SMS_BOOL_OP_DECLS (SparseBoolMatrix, bool)

SPARSE_SSM_EQNE_OP_DECLS (bool, SparseBoolMatrix)
SPARSE_SSM_BOOL_OP_DECLS (bool, SparseBoolMatrix)

SPARSE_SMSM_EQNE_OP_DECLS (SparseBoolMatrix, SparseBoolMatrix)
SPARSE_SMSM_BOOL_OP_DECLS (SparseBoolMatrix, SparseBoolMatrix)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
