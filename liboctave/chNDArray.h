/*

Copyright (C) 2003 John W. Eaton

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

#if !defined (octave_charNDArray_h)
#define octave_charNDArray_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include "MArrayN.h"
#include "chMatrix.h"

#include "mx-defs.h"
#include "mx-op-defs.h"

class
charNDArray : public MArrayN<char>
{
public:
  
  charNDArray (void) : MArrayN<char> () { }

  charNDArray (dim_vector& dv) : MArrayN<char> (dv) { }

  charNDArray (dim_vector& dv, char val) : MArrayN<char> (dv, val) { }
  
  charNDArray (const charNDArray& a) : MArrayN<char> (a) { }

  charNDArray (const charMatrix& a) : MArrayN<char> (a) { }

  charNDArray (char c) : MArrayN<char> (charMatrix (c)) { }

  charNDArray (const char *s) : MArrayN<char> (charMatrix (s)) { }

  charNDArray (const std::string& s) : MArrayN<char> (charMatrix (s)) { }

  charNDArray (const string_vector& s) : MArrayN<char> (charMatrix (s)) { }

  charNDArray (const ArrayN<char>& a) : MArrayN<char> (a) { }

  charNDArray& operator = (const charNDArray& a)
    {
      MArrayN<char>::operator = (a);
      return *this;
    }

  // XXX FIXME XXX -- this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;
  charNDArray concat (const charNDArray& rb, const Array<int>& ra_idx);
  charNDArray concat (const NDArray& rb, const Array<int>& ra_idx);

  charNDArray& insert (const charNDArray& a, int r, int c);
  charNDArray& insert (const charNDArray& a, const Array<int>& ra_idx);
  
  charMatrix matrix_value (void) const;

  charNDArray squeeze (void) const { return ArrayN<char>::squeeze (); }

  static void increment_index (Array<int>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension = 0);

  static int compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions);

  // i/o

  // friend std::ostream& operator << (std::ostream& os, const charNDArray& a);
  // friend std::istream& operator >> (std::istream& is, charNDArray& a);

  static char resize_fill_value (void) { return '\0'; }

private:

  charNDArray (char *d, dim_vector& dv) : MArrayN<char> (d, dv) { }
};

MARRAY_FORWARD_DEFS (MArrayN, charNDArray, char)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
