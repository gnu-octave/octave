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

#if !defined (octave_CHOL_h)
#define octave_CHOL_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"

extern "C++" {

class CHOL
{
friend class Matrix;

public:

  CHOL (void) {}

  CHOL (const Matrix& a);
  CHOL (const Matrix& a, int& info);

  CHOL (const CHOL& a);

  CHOL& operator = (const CHOL& a);
  Matrix chol_matrix (void) const;
  friend ostream& operator << (ostream& os, const CHOL& a);

private:

  int init (const Matrix& a);

  Matrix chol_mat;
};

inline CHOL::CHOL (const Matrix& a)
{
  init (a);
}

inline CHOL::CHOL (const Matrix& a, int& info)
{
  info = init (a);
}

inline CHOL::CHOL (const CHOL& a)
{
  chol_mat = a.chol_mat;
}

inline CHOL&
CHOL::operator = (const CHOL& a)
{
  chol_mat = a.chol_mat;

  return *this;
}

inline Matrix CHOL::chol_matrix (void) const
{
  return chol_mat;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
