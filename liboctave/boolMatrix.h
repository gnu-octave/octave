/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

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

#if !defined (octave_boolMatrix_int_h)
#define octave_boolMatrix_int_h 1

#include "Array2.h"

#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
boolMatrix : public Array2<bool>
{
public:

  boolMatrix (void) : Array2<bool> () { }
  boolMatrix (octave_idx_type r, octave_idx_type c) : Array2<bool> (r, c) { }
  boolMatrix (octave_idx_type r, octave_idx_type c, bool val) : Array2<bool> (r, c, val) { }
  boolMatrix (const dim_vector& dv) : Array2<bool> (dv) { }
  boolMatrix (const dim_vector& dv, bool val) : Array2<bool> (dv, val) { }
  boolMatrix (const Array2<bool>& a) : Array2<bool> (a) { }
  boolMatrix (const boolMatrix& a) : Array2<bool> (a) { }

  boolMatrix& operator = (const boolMatrix& a)
    {
      Array2<bool>::operator = (a);
      return *this;
    }

  bool operator == (const boolMatrix& a) const;
  bool operator != (const boolMatrix& a) const;

  boolMatrix transpose (void) const { return Array2<bool>::transpose (); }

  // destructive insert/delete/reorder operations

  boolMatrix& insert (const boolMatrix& a, octave_idx_type r, octave_idx_type c);

  // unary operations

  boolMatrix operator ! (void) const;

  // other operations

  boolMatrix diag (octave_idx_type k = 0) const;

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

#if 0
  // i/o

  friend std::ostream& operator << (std::ostream& os, const Matrix& a);
  friend std::istream& operator >> (std::istream& is, Matrix& a);
#endif

  static bool resize_fill_value (void) { return false; }

private:

  boolMatrix (bool *b, octave_idx_type r, octave_idx_type c) : Array2<bool> (b, r, c) { }
};

MM_CMP_OP_DECLS (boolMatrix, boolMatrix, OCTAVE_API)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
