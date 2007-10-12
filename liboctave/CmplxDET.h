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

#if !defined (octave_ComplexDET_h)
#define octave_ComplexDET_h 1

#include <iostream>

#include "oct-cmplx.h"

// FIXME -- we could use templates here; compare with dbleDET.h

class
OCTAVE_API
ComplexDET
{
friend class ComplexMatrix;
friend class SparseComplexMatrix;

public:

  ComplexDET (void) : c2 (0), c10 (0), e2 (0), e10 (0), base2 (false) { }

  ComplexDET (const ComplexDET& a)
    : c2 (a.c2), c10 (a.c10), e2 (a.e2), e10 (a.e10), base2 (a.base2)
    { }

  ComplexDET& operator = (const ComplexDET& a)
    {
      if (this != &a)
	{
	  c2 = a.c2;
	  e2 = a.e2;

	  c10 = a.c10;
	  e10 = a.e10;

	  base2 = a.base2;
	}
      return *this;
    }

  bool value_will_overflow (void) const;
  bool value_will_underflow (void) const;

  // These two functions were originally defined in base 10, so we are
  // preserving that interface here.

  Complex coefficient (void) const { return coefficient10 (); }
  int exponent (void) const { return exponent10 (); }

  Complex coefficient10 (void) const { return c10; }
  int exponent10 (void) const { return e10; }

  Complex coefficient2 (void) const { return c2; }
  int exponent2 (void) const { return e2; }

  Complex value (void) const;

  friend std::ostream&  operator << (std::ostream& os, const ComplexDET& a);

private:

  // Constructed this way, we assume base 2.

  ComplexDET (const Complex& c, int e)
    : c2 (c), c10 (0), e2 (e), e10 (0), base2 (true)
    {
      initialize10 ();
    }

  // Original interface had only this constructor and it was assumed
  // to be base 10, so we are preserving that interface here.

  ComplexDET (const Complex *d)
    : c2 (0), c10 (d[0]), e2 (0), e10 (static_cast<int> (d[1].real ())),
      base2 (false)
    {
      initialize2 ();
    }

  void initialize2 (void);
  void initialize10 (void);

  Complex c2;
  Complex c10;

  int e2;
  int e10;

  // TRUE means the original values were provided in base 2.
  bool base2;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
