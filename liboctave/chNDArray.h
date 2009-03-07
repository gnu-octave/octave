/*

Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_charNDArray_h)
#define octave_charNDArray_h 1

#include "MArrayN.h"
#include "chMatrix.h"

#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
charNDArray : public MArrayN<char>
{
public:

  charNDArray (void) : MArrayN<char> () { }

  charNDArray (const dim_vector& dv) : MArrayN<char> (dv) { }

  charNDArray (const dim_vector& dv, char val) : MArrayN<char> (dv, val) { }
  
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

  bool any_element_is_nan (void) const { return false; }

  // FIXME -- this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;
  charNDArray concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx);
  charNDArray concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx);

  charNDArray& insert (const charNDArray& a, octave_idx_type r, octave_idx_type c);
  charNDArray& insert (const charNDArray& a, const Array<octave_idx_type>& ra_idx);
  
  charMatrix matrix_value (void) const;

  charNDArray squeeze (void) const { return ArrayN<char>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
			    const dim_vector& dimensions);

  // i/o

  // friend std::ostream& operator << (std::ostream& os, const charNDArray& a);
  // friend std::istream& operator >> (std::istream& is, charNDArray& a);

  static char resize_fill_value (void) { return '\0'; }

  charNDArray diag (octave_idx_type k = 0) const;

  typedef int (*mapper) (int);
  boolNDArray bmap (mapper fcn) const;
  NDArray dmap (mapper fcn) const;
  charNDArray smap (mapper fcn) const;

private:

  charNDArray (char *d, dim_vector& dv) : MArrayN<char> (d, dv) { }
};

NDS_CMP_OP_DECLS (charNDArray, char, OCTAVE_API)
NDS_BOOL_OP_DECLS (charNDArray, char, OCTAVE_API)

SND_CMP_OP_DECLS (char, charNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (char, charNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (charNDArray, charNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (charNDArray, charNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArrayN, charNDArray, char)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
