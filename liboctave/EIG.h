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

#if !defined (octave_EIG_h)
#define octave_EIG_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"
#include "CMatrix.h"
#include "CColVector.h"

class EIG
{
friend class Matrix;
friend class ComplexMatrix;

public:

  EIG (void) {}

  EIG (const Matrix& a);
  EIG (const Matrix& a, int& info);

  EIG (const ComplexMatrix& a);
  EIG (const ComplexMatrix& a, int& info);

  EIG (const EIG& a);

  EIG& operator = (const EIG& a);

  ComplexColumnVector eigenvalues (void) const;
  ComplexMatrix eigenvectors (void) const;

  friend ostream&  operator << (ostream& os, const EIG& a);

private:

  int init (const Matrix& a);
  int init (const ComplexMatrix& a);

  ComplexColumnVector lambda;
  ComplexMatrix v;
};

inline EIG::EIG (const Matrix& a)
{
  init (a);
}

inline EIG::EIG (const Matrix& a, int& info)
{
  info = init (a);
}

inline EIG::EIG (const ComplexMatrix& a)
{
  init (a);
}

inline EIG::EIG (const ComplexMatrix& a, int& info)
{
  info = init (a);
}

inline EIG::EIG (const EIG& a)
{
  lambda = a.lambda;
  v = a.v;
}

inline EIG& EIG::operator = (const EIG& a)
{
  lambda = a.lambda;
  v = a.v;
  return *this;
}

inline ComplexColumnVector EIG::eigenvalues (void) const
{
  return lambda;
}

inline ComplexMatrix EIG::eigenvectors (void) const
{
  return v;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
