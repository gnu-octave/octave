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

#if !defined (octave_LU_h)
#define octave_LU_h 1

class ostream;

#include "dMatrix.h"

extern "C++" {

class LU
{
friend class Matrix;

public:

  LU (void) {}

  LU (const Matrix& a);

  LU (const LU& a);

  LU& operator = (const LU& a);

  Matrix L (void) const;
  Matrix U (void) const;
  Matrix P (void) const;

  friend ostream&  operator << (ostream& os, const LU& a);

private:

  Matrix l;
  Matrix u;
  Matrix p;
};

inline LU::LU (const LU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
}

inline LU& LU::operator = (const LU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
  return *this;
}

inline Matrix LU::L (void) const
{
  return l;
}

inline Matrix LU::U (void) const
{
  return u;
}

inline Matrix LU::P (void) const
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
