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

#if !defined (octave_CHOL_h)
#define octave_CHOL_h 1

#include <iostream>

#include "dMatrix.h"

class
OCTAVE_API
CHOL
{
public:

  CHOL (void) : chol_mat () { }

  CHOL (const Matrix& a) { init (a); }

  CHOL (const Matrix& a, octave_idx_type& info) { info = init (a); }

  CHOL (const CHOL& a) : chol_mat (a.chol_mat) { }

  CHOL& operator = (const CHOL& a)
    {
      if (this != &a)
	chol_mat = a.chol_mat;

      return *this;
    }

  Matrix chol_matrix (void) const { return chol_mat; }

  // Compute the inverse of a matrix using the Cholesky factorization.
  Matrix inverse (void) const;

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const CHOL& a);

private:

  Matrix chol_mat;

  octave_idx_type init (const Matrix& a);
};

Matrix OCTAVE_API chol2inv (const Matrix& r);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
