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

#if !defined (octave_NDArray_h)
#define octave_NDArray_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include "MArrayN.h"
#include "dMatrix.h"

#include "mx-defs.h"
#include "mx-op-defs.h"

#include "data-conv.h"
#include "mach-info.h"

class
NDArray : public MArrayN<double>
{
public:
  
  NDArray (void) : MArrayN<double> () { }

  NDArray (const dim_vector& dims) : MArrayN<double> (dims) { }

  NDArray (const dim_vector& dims, double val)
    : MArrayN<double> (dims, val) { }
  
  NDArray (const NDArray& a) : MArrayN<double> (a) { }

  NDArray (const Matrix& a) : MArrayN<double> (a) { }

  NDArray (const MArrayN<double>& a) : MArrayN<double> (a) { }

  NDArray (const ArrayN<double>& a) : MArrayN<double> (a) { }

  explicit NDArray (const boolNDArray& a);

  explicit NDArray (const charNDArray& a);

  NDArray& operator = (const NDArray& a)
    {
      MArrayN<double>::operator = (a);
      return *this;
    }

  // unary operations

  boolNDArray operator ! (void) const;

  // XXX FIXME XXX -- this is not quite the right thing.

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

  Matrix matrix_value (void) const;

  NDArray squeeze (void) const { return ArrayN<double>::squeeze (); }

  static void increment_index (Array<int>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension = 0);

  // i/o

  // friend std::ostream& operator << (std::ostream& os, const NDArray& a);
  // friend std::istream& operator >> (std::istream& is, NDArray& a);

  static double resize_fill_value (void) { return 0; }

  bool any_element_is_negative (bool = false) const;
  bool all_integers (double& max_val, double& min_val) const;

private:

  NDArray (double *d, dim_vector& dims) : MArrayN<double> (d, dims) { }
};

NDS_CMP_OP_DECLS (NDArray, double)
NDS_BOOL_OP_DECLS (NDArray, double)

SND_CMP_OP_DECLS (double, NDArray)
SND_BOOL_OP_DECLS (double, NDArray)

NDND_CMP_OP_DECLS (NDArray, NDArray)
NDND_BOOL_OP_DECLS (NDArray, NDArray)

MARRAY_FORWARD_DEFS (MArrayN, NDArray, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
