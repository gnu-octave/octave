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

#if !defined (octave_ComplexSVD_h)
#define octave_ComplexSVD_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dDiagMatrix.h"
#include "CMatrix.h"

extern "C++" {

class ComplexSVD
{
friend class ComplexMatrix;

public:

  ComplexSVD (void) {}

  ComplexSVD (const ComplexMatrix& a);
  ComplexSVD (const ComplexMatrix& a, int& info);

  ComplexSVD (const ComplexSVD& a);

  ComplexSVD& operator = (const ComplexSVD& a);

  DiagMatrix singular_values (void) const;
  ComplexMatrix left_singular_matrix (void) const;
  ComplexMatrix right_singular_matrix (void) const;

  friend ostream&  operator << (ostream& os, const ComplexSVD& a);

private:

  int init (const ComplexMatrix& a);

  DiagMatrix sigma;
  ComplexMatrix left_sm;
  ComplexMatrix right_sm;
};

inline ComplexSVD::ComplexSVD (const ComplexMatrix& a)
{
  init (a);
}

inline ComplexSVD::ComplexSVD (const ComplexMatrix& a, int& info)
{
  info = init (a);
} 

inline ComplexSVD::ComplexSVD (const ComplexSVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;
}

inline ComplexSVD&
ComplexSVD::operator = (const ComplexSVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;

  return *this;
}

inline DiagMatrix ComplexSVD::singular_values (void) const
{
  return sigma;
}

inline ComplexMatrix ComplexSVD::left_singular_matrix (void) const
{
  return left_sm;
}

inline ComplexMatrix ComplexSVD::right_singular_matrix (void) const
{
  return right_sm;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
