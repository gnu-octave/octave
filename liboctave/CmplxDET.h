//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_ComplexDET_h)
#define octave_ComplexDET_h 1

class ostream;

#include <Complex.h>

extern "C++" {

class ComplexDET
{
  friend class ComplexMatrix;

public:

  ComplexDET (void);

  ComplexDET (const ComplexDET& a);

  ComplexDET& operator = (const ComplexDET& a);

  int value_will_overflow (void) const;
  int value_will_underflow (void) const;
  Complex coefficient (void) const;
  int exponent (void) const;
  Complex value (void) const;

  friend ostream&  operator << (ostream& os, const ComplexDET& a);

private:

  ComplexDET (const Complex *d);

  Complex det [2];
};

inline ComplexDET::ComplexDET (void)
{
}

inline ComplexDET::ComplexDET (const ComplexDET& a)
{
  det[0] = a.det[0];
  det[1] = a.det[1];
}

inline ComplexDET& ComplexDET::operator = (const ComplexDET& a)
{
  det[0] = a.det[0];
  det[1] = a.det[1];
  return *this;
}

inline ComplexDET::ComplexDET (const Complex *d)
{
  det[0] = d[0];
  det[1] = d[1];
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
