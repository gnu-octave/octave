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

#if !defined (octave_ComplexLU_h)
#define octave_Complex_LU_h 1

class ostream;

#include "dMatrix.h"
#include "CMatrix.h"

extern "C++" {

class ComplexLU
{
friend class ComplexMatrix;

public:

  ComplexLU (void) {}

  ComplexLU (const ComplexMatrix& a);

  ComplexLU (const ComplexLU& a);

  ComplexLU& operator = (const ComplexLU& a);

  ComplexMatrix L (void) const;
  ComplexMatrix U (void) const;
  Matrix P (void) const;

  friend ostream&  operator << (ostream& os, const ComplexLU& a);

private:

  ComplexMatrix l;
  ComplexMatrix u;
  Matrix p;
};

inline ComplexLU::ComplexLU (const ComplexLU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
}

inline ComplexLU& ComplexLU::operator = (const ComplexLU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
  return *this;
}

inline ComplexMatrix ComplexLU::L (void) const
{
  return l;
}

inline ComplexMatrix ComplexLU::U (void) const
{
  return u;
}

inline Matrix ComplexLU::P (void) const
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
