/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_ComplexQRP_h)
#define octave_ComplexQRP_h 1

#include <iostream>

#include "CmplxQR.h"

class
OCTAVE_API
ComplexQRP : public ComplexQR
{
public:

  ComplexQRP (void) : ComplexQR (), p () { }

  ComplexQRP (const ComplexMatrix&, QR::type = QR::std);

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

  ~ComplexQRP (void) { }

  void init (const ComplexMatrix&, QR::type = QR::std);

  Matrix P (void) const { return p; }

  friend std::ostream&  operator << (std::ostream&, const ComplexQRP&);

private:

  Matrix p;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
