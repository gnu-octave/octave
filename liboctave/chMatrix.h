/*

Copyright (C) 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_chMatrix_int_h)
#define octave_chMatrix_int_h 1

#include <string>

#include "MArray2.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "str-vec.h"

class
OCTAVE_API
charMatrix : public MArray2<char>
{
friend class ComplexMatrix;

public:

  charMatrix (void) : MArray2<char> () { }
  charMatrix (octave_idx_type r, octave_idx_type c) : MArray2<char> (r, c) { }
  charMatrix (octave_idx_type r, octave_idx_type c, char val) : MArray2<char> (r, c, val) { }
  charMatrix (const dim_vector& dv) : MArray2<char> (dv) { }
  charMatrix (const dim_vector& dv, char val) : MArray2<char> (dv, val) { }
  charMatrix (const MArray2<char>& a) : MArray2<char> (a) { }
  charMatrix (const charMatrix& a) : MArray2<char> (a) { }
  charMatrix (char c);
  charMatrix (const char *s);
  charMatrix (const std::string& s);
  charMatrix (const string_vector& s);

  charMatrix& operator = (const charMatrix& a)
    {
      MArray2<char>::operator = (a);
      return *this;
    }

  bool operator == (const charMatrix& a) const;
  bool operator != (const charMatrix& a) const;

  charMatrix transpose (void) const { return MArray2<char>::transpose (); }

  // destructive insert/delete/reorder operations

  charMatrix& insert (const char *s, octave_idx_type r, octave_idx_type c);
  charMatrix& insert (const charMatrix& a, octave_idx_type r, octave_idx_type c);

  std::string row_as_string (octave_idx_type, bool strip_ws = false, bool raw = false) const;

  // resize is the destructive equivalent for this one

  charMatrix extract (octave_idx_type r1, octave_idx_type c1, octave_idx_type r2, octave_idx_type c2) const;

  charMatrix diag (octave_idx_type k = 0) const;

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

#if 0
  // i/o

  friend std::ostream& operator << (std::ostream& os, const Matrix& a);
  friend std::istream& operator >> (std::istream& is, Matrix& a);
#endif

  static char resize_fill_value (void) { return '\0'; }

private:

  charMatrix (char *ch, octave_idx_type r, octave_idx_type c) : MArray2<char> (ch, r, c) { }
};

MS_CMP_OP_DECLS (charMatrix, char, OCTAVE_API)
MS_BOOL_OP_DECLS (charMatrix, char, OCTAVE_API)

SM_CMP_OP_DECLS (char, charMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (char, charMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (charMatrix, charMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (charMatrix, charMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray2, charMatrix, char)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
