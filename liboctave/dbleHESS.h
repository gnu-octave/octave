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

#if !defined (octave_HESS_h)
#define octave_HESS_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"

extern "C++" {

class HESS
{
friend class Matrix;

public:

  HESS (void) {}

  HESS (const Matrix& a);
  HESS (const Matrix&a, int& info);

  HESS (const HESS& a);

  HESS& operator = (const HESS& a);
  Matrix hess_matrix (void) const;
  Matrix unitary_hess_matrix (void) const;
  friend ostream& operator << (ostream& os, const HESS& a);

private:

  int init (const Matrix& a);

  Matrix hess_mat;
  Matrix unitary_hess_mat;
};

inline HESS::HESS (const Matrix& a)
{
  init (a);
}

inline HESS::HESS (const Matrix& a, int& info)
{
  info = init (a);
}

inline HESS::HESS (const HESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;
}

inline HESS&
HESS::operator = (const HESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;

  return *this;
}

inline Matrix HESS::hess_matrix (void) const
{
  return hess_mat;
}

inline Matrix HESS::unitary_hess_matrix (void) const
{
  return unitary_hess_mat;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
