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

#if !defined (octave_ComplexNDArray_h)
#define octave_ComplexNDArray_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include "MArrayN.h"
#include "CMatrix.h"

#include "mx-defs.h"
#include "mx-op-defs.h"

#include "data-conv.h"
#include "mach-info.h"

class
ComplexNDArray : public MArrayN<Complex>
{
public:
  
  ComplexNDArray (void) : MArrayN<Complex> () { }

  ComplexNDArray (dim_vector& dims) : MArrayN<Complex> (dims) { }

  ComplexNDArray (dim_vector& dims, const Complex& val)
    : MArrayN<Complex> (dims, val) { }
  
  ComplexNDArray (const ComplexNDArray& a) : MArrayN<Complex> (a) { }

  ComplexNDArray (const ComplexMatrix& a) : MArrayN<Complex> (a) { }

  ComplexNDArray (const MArrayN<Complex>& a) : MArrayN<Complex> (a) { }

  ComplexNDArray (const ArrayN<Complex>& a) : MArrayN<Complex> (a) { }

  ComplexNDArray& operator = (const ComplexNDArray& a)
    {
      MArrayN<Complex>::operator = (a);
      return *this;
    }

  // XXX FIXME XXX -- this is not quite the right thing.

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

  ComplexMatrix matrix_value (void) const;

  // i/o

  // friend std::ostream& operator << (std::ostream& os, const NDArray& a);
  // friend std::istream& operator >> (std::istream& is, NDArray& a);

  static Complex resize_fill_value (void) { return Complex (0.0, 0.0); }

  //  bool all_elements_are_real (void) const;
  //  bool all_integers (double& max_val, double& min_val) const;

private:

  ComplexNDArray (Complex *d, dim_vector& dims)
    : MArrayN<Complex> (d, dims) { }
};

MARRAY_FORWARD_DEFS (MArrayN, ComplexNDArray, Complex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
