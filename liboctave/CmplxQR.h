//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_ComplexQR_h)
#define octave_ComplexQR_h 1

class ostream;

#include "CMatrix.h"
#include "dbleQR.h"

extern "C++" {

class ComplexQR
{
public:

  ComplexQR (void) {}

  ComplexQR (const ComplexMatrix& A, QR::type qr_type = QR::std);

  ComplexQR (const ComplexQR& a);

  ComplexQR& operator = (const ComplexQR& a);

  ComplexMatrix Q (void) const;
  ComplexMatrix R (void) const;

  friend ostream&  operator << (ostream& os, const ComplexQR& a);

protected:

  ComplexMatrix q;
  ComplexMatrix r;
};

inline ComplexQR::ComplexQR (const ComplexQR& a)
{
  q = a.q;
  r = a.r;
}

inline ComplexQR& ComplexQR::operator = (const ComplexQR& a)
{
  q = a.q;
  r = a.r;
  return *this;
}

inline ComplexMatrix ComplexQR::Q (void) const
{
  return q;
}

inline ComplexMatrix ComplexQR::R (void) const
{
  return r;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
