/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_boolMatrix_int_h)
#define octave_boolMatrix_int_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "MArray2.h"

#include "mx-defs.h"

class
boolMatrix : public MArray2<bool>
{
public:

  boolMatrix (void) : MArray2<bool> () { }
  boolMatrix (int r, int c) : MArray2<bool> (r, c) { }
  boolMatrix (int r, int c, bool val) : MArray2<bool> (r, c, val) { }
  boolMatrix (const MArray2<bool>& a) : MArray2<bool> (a) { }
  boolMatrix (const boolMatrix& a) : MArray2<bool> (a) { }

  boolMatrix& operator = (const boolMatrix& a)
    {
      MArray2<bool>::operator = (a);
      return *this;
    }

  bool operator == (const boolMatrix& a) const;
  bool operator != (const boolMatrix& a) const;

  // destructive insert/delete/reorder operations

  boolMatrix& insert (const boolMatrix& a, int r, int c);

  boolMatrix transpose (void) const;

#if 0
  // i/o

  friend ostream& operator << (ostream& os, const Matrix& a);
  friend istream& operator >> (istream& is, Matrix& a);
#endif

private:

  boolMatrix (bool *b, int r, int c) : MArray2<bool> (b, r, c) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
