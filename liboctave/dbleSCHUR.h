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

#if !defined (octave_SCHUR_h)
#define octave_SCHUR_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"

class SCHUR
{
friend class Matrix;

public:

  SCHUR (void) {}

  SCHUR (const Matrix& a, const char *ord);
  SCHUR (const Matrix& a, const char *ord, int& info);

  SCHUR (const SCHUR& a, const char *ord);

  SCHUR& operator = (const SCHUR& a);

  Matrix schur_matrix (void) const;
  Matrix unitary_matrix (void) const;

  friend ostream& operator << (ostream& os, const SCHUR& a);

private:

  int init (const Matrix& a, const char *ord);

  Matrix schur_mat;
  Matrix unitary_mat;
};

inline SCHUR::SCHUR (const Matrix& a, const char *ord)
{
  init (a, ord);
}

inline SCHUR::SCHUR (const Matrix& a, const char *ord, int& info) 
{
  info = init (a, ord);
}

inline SCHUR::SCHUR (const SCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
}

inline SCHUR&
SCHUR::operator = (const SCHUR& a)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
  
  return *this;
}

inline Matrix SCHUR::schur_matrix (void) const
{
  return schur_mat;
}

inline Matrix SCHUR::unitary_matrix (void) const
{
  return unitary_mat;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
