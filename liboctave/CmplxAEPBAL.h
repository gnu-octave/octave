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

#if !defined (octave_ComplexAEPBALANCE_h)
#define octave_ComplexAEPBALANCE_h 1

class ostream;

#include "CMatrix.h"

extern "C++" {

class ComplexAEPBALANCE
{
friend class ComplexMatrix;

public:

  ComplexAEPBALANCE (void) {}
  ComplexAEPBALANCE (const ComplexMatrix& a, const char *balance_job);
  ComplexAEPBALANCE (const ComplexAEPBALANCE& a);
  ComplexAEPBALANCE& operator = (const ComplexAEPBALANCE& a);
  ComplexMatrix balanced_matrix (void) const;
  ComplexMatrix balancing_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexAEPBALANCE& a);

private:

  int init (const ComplexMatrix& a, const char * balance_job);

  ComplexMatrix balanced_mat;
  ComplexMatrix balancing_mat;
};

inline ComplexAEPBALANCE::ComplexAEPBALANCE (const ComplexMatrix& a,
					     const char * balance_job)
{
  init (a, balance_job); 
}

inline ComplexAEPBALANCE::ComplexAEPBALANCE (const ComplexAEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;
}

inline ComplexAEPBALANCE&
ComplexAEPBALANCE::operator = (const ComplexAEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;

  return *this;
}

inline ComplexMatrix ComplexAEPBALANCE::balanced_matrix (void) const
{
  return balanced_mat;
}

inline ComplexMatrix ComplexAEPBALANCE::balancing_matrix (void) const
{
  return balancing_mat;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
