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

#if !defined (octave_ComplexQRP_h)
#define octave_ComplexQRP_h 1

class ostream;

#include "CmplxQR.h"

extern "C++" {

class ComplexQRP : public ComplexQR
{
public:

  ComplexQRP (void) {}

  ComplexQRP (const ComplexMatrix& A, QR::type qr_type = QR::std);

  ComplexQRP (const ComplexQRP& a);

  ComplexQRP& operator = (const ComplexQRP& a);

  Matrix P (void) const;

  friend ostream&  operator << (ostream& os, const ComplexQRP& a);

private:

  Matrix p;
};

inline ComplexQRP::ComplexQRP (const ComplexQRP& a) : ComplexQR (a)
{
  p = a.p;
}

inline ComplexQRP& ComplexQRP::operator = (const ComplexQRP& a)
{
  ComplexQR::operator = (a);
  p = a.p;
  return *this;
}

inline Matrix ComplexQRP::P (void) const
{
  return p;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
