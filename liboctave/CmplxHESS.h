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

#if !defined (octave_ComplexHESS_h)
#define octave_ComplexHESS_h 1

class ostream;

#include "CMatrix.h"

extern "C++" {

class ComplexHESS
{
friend class ComplexMatrix;

public:

  ComplexHESS (void) {}
  ComplexHESS (const ComplexMatrix& a);
  ComplexHESS (const ComplexMatrix& a, int& info);
  ComplexHESS (const ComplexHESS& a);
  ComplexHESS& operator = (const ComplexHESS& a);
  ComplexMatrix hess_matrix (void) const;
  ComplexMatrix unitary_hess_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexHESS& a);

private:

  int init (const ComplexMatrix& a);

  ComplexMatrix hess_mat;
  ComplexMatrix unitary_hess_mat;
};

inline ComplexHESS::ComplexHESS (const ComplexMatrix& a)
{
  init (a);
}

inline ComplexHESS::ComplexHESS (const ComplexMatrix& a, int& info)
{
  info = init (a);
}

inline ComplexHESS::ComplexHESS (const ComplexHESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;
}

inline ComplexHESS&
ComplexHESS::operator = (const ComplexHESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;

  return *this;
}

inline ComplexMatrix ComplexHESS::hess_matrix (void) const
{
  return hess_mat;
}

inline ComplexMatrix ComplexHESS::unitary_hess_matrix (void) const
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
