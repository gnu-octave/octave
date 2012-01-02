/*

Copyright (C) 2003-2012 John W. Eaton

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

#include "Array.h"
#include "chMatrix.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "bsxfun-decl.h"

class
OCTAVE_API
charNDArray : public Array<char>
{
public:

  typedef charMatrix matrix_type;

  charNDArray (void) : Array<char> () { }

  charNDArray (const dim_vector& dv) : Array<char> (dv) { }

  charNDArray (const dim_vector& dv, char val) : Array<char> (dv, val) { }

  charNDArray (const charNDArray& a) : Array<char> (a) { }

  charNDArray (const charMatrix& a) : Array<char> (a) { }

  charNDArray (char c) : Array<char> (charMatrix (c)) { }

  charNDArray (const char *s) : Array<char> (charMatrix (s)) { }

  charNDArray (const std::string& s) : Array<char> (charMatrix (s)) { }

  charNDArray (const string_vector& s) : Array<char> (charMatrix (s)) { }

  charNDArray (const Array<char>& a) : Array<char> (a) { }

  charNDArray& operator = (const charNDArray& a)
    {
      Array<char>::operator = (a);
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

  charNDArray squeeze (void) const { return Array<char>::squeeze (); }

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

};

NDS_CMP_OP_DECLS (charNDArray, char, OCTAVE_API)
NDS_BOOL_OP_DECLS (charNDArray, char, OCTAVE_API)

SND_CMP_OP_DECLS (char, charNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (char, charNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (charNDArray, charNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (charNDArray, charNDArray, OCTAVE_API)

BSXFUN_STDREL_DECLS (charNDArray, OCTAVE_API)

#endif
