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

#if !defined (octave_ComplexSCHUR_h)
#define octave_ComplexSCHUR_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "CMatrix.h"

class ComplexSCHUR
{
friend class ComplexMatrix;

public:

  ComplexSCHUR (void) {}

  ComplexSCHUR (const ComplexMatrix& a, const char *ord);
  ComplexSCHUR (const ComplexMatrix& a, const char *ord, int& info);

  ComplexSCHUR (const ComplexSCHUR& a, const char *ord);

  ComplexSCHUR& operator = (const ComplexSCHUR& a);

  ComplexMatrix schur_matrix (void) const;
  ComplexMatrix unitary_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexSCHUR& a);

private:

  int init (const ComplexMatrix& a, const char *ord);

  ComplexMatrix schur_mat;
  ComplexMatrix unitary_mat;
};

inline ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& a, const char *ord) 
{
  init (a,ord);
}

inline ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& a, const char *ord,
				   int& info)
{
  info = init (a,ord);
}

inline ComplexSCHUR::ComplexSCHUR (const ComplexSCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
}

inline ComplexSCHUR&
ComplexSCHUR::operator = (const ComplexSCHUR& a)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;

  return *this;
}

inline ComplexMatrix ComplexSCHUR::schur_matrix (void) const
{
  return schur_mat;
}

inline ComplexMatrix ComplexSCHUR::unitary_matrix (void) const
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
