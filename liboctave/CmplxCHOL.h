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

#if !defined (octave_ComplexCHOL_h)
#define octave_ComplexCHOL_h 1

#include <iostream>

#include "CMatrix.h"

class
OCTAVE_API
ComplexCHOL
{
public:

  ComplexCHOL (void) : chol_mat () { }

  ComplexCHOL (const ComplexMatrix& a) { init (a); }

  ComplexCHOL (const ComplexMatrix& a, octave_idx_type& info)
    {
      info = init (a);
    }

  ComplexCHOL (const ComplexCHOL& a)
    : chol_mat (a.chol_mat) { }

  ComplexCHOL& operator = (const ComplexCHOL& a)
    {
      if (this != &a)
	chol_mat = a.chol_mat;

      return *this;
    }

  ComplexMatrix chol_matrix (void) const { return chol_mat; }

  ComplexMatrix inverse (void) const;

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const ComplexCHOL& a);

private:

  ComplexMatrix chol_mat;

  octave_idx_type init (const ComplexMatrix& a);
};

ComplexMatrix OCTAVE_API chol2inv (const ComplexMatrix& r);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
