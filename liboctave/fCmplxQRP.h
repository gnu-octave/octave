/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

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

#if !defined (octave_FloatComplexQRP_h)
#define octave_FloatComplexQRP_h 1

#include <iosfwd>

#include "fCmplxQR.h"
#include "PermMatrix.h"
#include "fColVector.h"

class
OCTAVE_API
FloatComplexQRP : public FloatComplexQR
{
public:

  FloatComplexQRP (void) : FloatComplexQR (), p () { }

  FloatComplexQRP (const FloatComplexMatrix&, QR::type = QR::std);

  FloatComplexQRP (const FloatComplexQRP& a) : FloatComplexQR (a), p (a.p) { }

  FloatComplexQRP& operator = (const FloatComplexQRP& a)
    {
      if (this != &a)
	{
	  FloatComplexQR::operator = (a);
	  p = a.p;
	}
      return *this;
    }

  ~FloatComplexQRP (void) { }

  void init (const FloatComplexMatrix&, QR::type = QR::std);

  PermMatrix P (void) const { return p; }

  FloatColumnVector Pvec (void) const;

  friend std::ostream&  operator << (std::ostream&, const FloatComplexQRP&);

private:

  PermMatrix p;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
