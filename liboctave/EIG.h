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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_EIG_h)
#define octave_EIG_h 1

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

  EIG (const Matrix& a, bool calc_eigenvectors = true)
    { init (a, calc_eigenvectors); }

  EIG (const Matrix& a, octave_idx_type& info, bool calc_eigenvectors = true)
    { info = init (a, calc_eigenvectors); }

  EIG (const ComplexMatrix& a, bool calc_eigenvectors = true)
    { init (a, calc_eigenvectors); }

  EIG (const ComplexMatrix& a, octave_idx_type& info, bool calc_eigenvectors = true)
    { info = init (a, calc_eigenvectors); }

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

  octave_idx_type init (const Matrix& a, bool calc_eigenvectors);
  octave_idx_type init (const ComplexMatrix& a, bool calc_eigenvectors);

  octave_idx_type symmetric_init (const Matrix& a, bool calc_eigenvectors);
  octave_idx_type hermitian_init (const ComplexMatrix& a, bool calc_eigenvectors);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
