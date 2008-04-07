/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005, 2006,
              2007 John W. Eaton

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

// updating/downdating by Jaroslav Hajek 2008

#if !defined (octave_ComplexQR_h)
#define octave_ComplexQR_h 1

#include <iostream>

#include "CMatrix.h"
#include "CColVector.h"
#include "CRowVector.h"
#include "dbleQR.h"


class
OCTAVE_API
ComplexQR
{
public:

  ComplexQR (void) : q (), r () { }

  ComplexQR (const ComplexMatrix&, QR::type = QR::std);

  ComplexQR (const ComplexMatrix& q, const ComplexMatrix& r);

  ComplexQR (const ComplexQR& a) : q (a.q), r (a.r) { }

  ComplexQR& operator = (const ComplexQR& a)
    {
      if (this != &a)
	{
	  q = a.q;
	  r = a.r;
	}
      return *this;
    }

  ~ComplexQR (void) { }

  void init (const ComplexMatrix&, QR::type = QR::std);

  ComplexMatrix Q (void) const { return q; }

  ComplexMatrix R (void) const { return r; }

  void update (const ComplexMatrix& u, const ComplexMatrix& v);

  void insert_col (const ComplexMatrix& u, octave_idx_type j);

  void delete_col (octave_idx_type j);

  void insert_row (const ComplexMatrix& u, octave_idx_type j);

  void delete_row (octave_idx_type j);

  void shift_cols (octave_idx_type i, octave_idx_type j);

  void economize();

  friend std::ostream&  operator << (std::ostream&, const ComplexQR&);

protected:

  ComplexMatrix q;
  ComplexMatrix r;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
