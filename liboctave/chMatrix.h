/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_chMatrix_int_h)
#define octave_chMatrix_int_h 1

#include <string>

#include "MArray2.h"

#include "mx-defs.h"
#include "str-vec.h"

class
charMatrix : public MArray2<char>
{
friend class ComplexMatrix;

public:

  charMatrix (void) : MArray2<char> () { }
  charMatrix (int r, int c) : MArray2<char> (r, c) { }
  charMatrix (int r, int c, char val) : MArray2<char> (r, c, val) { }
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

  charMatrix& insert (const char *s, int r, int c);
  charMatrix& insert (const charMatrix& a, int r, int c);

  std::string row_as_string (int, bool strip_ws = false, bool raw = false) const;

  // resize is the destructive equivalent for this one

  charMatrix extract (int r1, int c1, int r2, int c2) const;

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

#if 0
  // i/o

  friend std::ostream& operator << (std::ostream& os, const Matrix& a);
  friend std::istream& operator >> (std::istream& is, Matrix& a);
#endif

  static char resize_fill_value (void) { return '\0'; }

private:

  charMatrix (char *ch, int r, int c) : MArray2<char> (ch, r, c) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
