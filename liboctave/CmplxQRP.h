//                                  -*- C++ -*-
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

#if !defined (octave_ComplexQRP_h)
#define octave_ComplexQRP_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "CmplxQR.h"

class
ComplexQRP : public ComplexQR
{
public:

  ComplexQRP (void) : ComplexQR (), p (), jpvt (0), rwork (0) { }

  ComplexQRP (const ComplexMatrix& A, QR::type qr_type = QR::std);

  ComplexQRP (const ComplexQRP& a) : ComplexQR (a), p (a.p) { }

  ComplexQRP& operator = (const ComplexQRP& a)
    {
      if (this != &a)
	{
	  ComplexQR::operator = (a);
	  p = a.p;
	}
      return *this;
    }

  ~ComplexQRP (void)
    {
      delete [] jpvt;
      delete [] rwork;
    }

  Matrix P (void) const { return p; }

  friend ostream&  operator << (ostream& os, const ComplexQRP& a);

private:

  Matrix p;

  int *jpvt;
  double *rwork;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
