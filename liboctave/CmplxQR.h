/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005, 2006,
              2007 John W. Eaton
Copyright (C) 2008, 2009 Jaroslav Hajek              

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

#if !defined (octave_ComplexQR_h)
#define octave_ComplexQR_h 1

#include <iosfwd>

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

  QR::type get_type (void) const;

  void update (const ComplexColumnVector& u, const ComplexColumnVector& v);

  void update (const ComplexMatrix& u, const ComplexMatrix& v);

  void insert_col (const ComplexColumnVector& u, octave_idx_type j);

  void insert_col (const ComplexMatrix& u, const Array<octave_idx_type>& j);

  void delete_col (octave_idx_type j);

  void delete_col (const Array<octave_idx_type>& j);

  void insert_row (const ComplexRowVector& u, octave_idx_type j);

  void delete_row (octave_idx_type j);

  void shift_cols (octave_idx_type i, octave_idx_type j);

  friend std::ostream&  operator << (std::ostream&, const ComplexQR&);

protected:

  void form (octave_idx_type n, ComplexMatrix& afact, 
             Complex *tau, QR::type qr_type);

  ComplexMatrix q;
  ComplexMatrix r;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
