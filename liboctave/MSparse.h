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

#if !defined (octave_MSparse_h)
#define octave_MSparse_h 1

#include "MArray2.h"

#include "Sparse.h"

// Two dimensional sparse array with math ops.

// But first, some preprocessor abuse...

#include "MSparse-defs.h"

SPARSE_OPS_FORWARD_DECLS (MSparse, MArray2)

template <class T>
class
MSparse : public Sparse<T>
{
public:

  MSparse (void) : Sparse<T> () { }

  MSparse (int n, int m) : Sparse<T> (n, m) { }

  MSparse (const MSparse<T>& a) : Sparse<T> (a) { }

  MSparse (const MSparse<T>& a, const dim_vector& dv) : Sparse<T> (a, dv) { }

  MSparse (const Sparse<T>& a) : Sparse<T> (a) { }

  MSparse (const Array<T> a, const Array<int>& r, 
	   const Array<int>& c, int nr = -1, 
	   int nc = -1, bool sum_terms = true)
    : Sparse<T> (a, r, c, nr, nc, sum_terms) { }

  MSparse (const Array<T> a, const Array<double>& r, 
	   const Array<double>& c, int nr = -1, 
	   int nc = -1, bool sum_terms = true)
    : Sparse<T> (a, r, c, nr, nc, sum_terms) { }

  explicit MSparse (int r, int c, T val) : Sparse<T> (r, c, val) { }

  MSparse (int r, int c, int num_nz) : Sparse<T> (r, c, num_nz) { }

  ~MSparse (void) { }

  MSparse<T>& operator = (const MSparse<T>& a)
    {
      Sparse<T>::operator = (a);
      return *this;
    }

  MSparse<T>& insert (const Sparse<T>& a, int r, int c)
  {
    Sparse<T>::insert (a, r, c);
    return *this;
  }

  MSparse<T> transpose (void) const { return Sparse<T>::transpose (); }

  MSparse<T> squeeze (void) const { return Sparse<T>::squeeze (); }

  MSparse<T> index (idx_vector& i, int resize_ok) const 
    { return Sparse<T>::index (i, resize_ok); }

  MSparse<T> index (idx_vector& i, idx_vector& j, int resize_ok) const 
    { return Sparse<T>::index (i, j, resize_ok); }
  
  MSparse<T> index (Array<idx_vector>& ra_idx, int resize_ok) const 
    { return Sparse<T>::index (ra_idx, resize_ok); }

  MSparse<T> reshape (const dim_vector& new_dims) const
    { return Sparse<T>::reshape (new_dims); }
     
  MSparse<T> permute (const Array<int>& vec, bool inv = false) const
    { return Sparse<T>::permute (vec, inv); }

  MSparse<T> ipermute (const Array<int>& vec) const
    { return Sparse<T>::ipermute (vec); }


  // Currently, the OPS functions don't need to be friends, but that
  // may change.

  // SPARSE_OPS_FRIEND_DECLS (MSparse, MArray2)
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
