/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_EIG_h)
#define octave_EIG_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <iostream>

#include "dMatrix.h"
#include "CMatrix.h"
#include "CColVector.h"

class
EIG
{
friend class Matrix;
friend class ComplexMatrix;

public:

  EIG (void)
    : lambda (), v () { }

  EIG (const Matrix& a) { init (a); }

  EIG (const Matrix& a, int& info) { info = init (a); }

  EIG (const ComplexMatrix& a) { init (a); }

  EIG (const ComplexMatrix& a, int& info) { info = init (a); }

  EIG (const EIG& a)
    : lambda (a.lambda), v (a.v) { }

  EIG& operator = (const EIG& a)
    {
      if (this != &a)
	{
	  lambda = a.lambda;
	  v = a.v;
	}
      return *this;
    }

  ~EIG (void) { }

  ComplexColumnVector eigenvalues (void) const { return lambda; }

  ComplexMatrix eigenvectors (void) const { return v; }

  friend std::ostream&  operator << (std::ostream& os, const EIG& a);

private:

  ComplexColumnVector lambda;
  ComplexMatrix v;

  int init (const Matrix& a);
  int init (const ComplexMatrix& a);

  int symmetric_init (const Matrix& a);
  int hermitian_init (const ComplexMatrix& a);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
