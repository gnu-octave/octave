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

#if !defined (octave_SVD_h)
#define octave_SVD_h 1

#include <iostream>

#include "dDiagMatrix.h"
#include "dMatrix.h"

class
SVD
{
public:

  enum type
    {
      std,
      economy,
      sigma_only
    };

  SVD (void) : sigma (), left_sm (), right_sm () { }

  SVD (const Matrix& a, type svd_type = SVD::std) { init (a, svd_type); }

  SVD (const Matrix& a, int& info, type svd_type = SVD::std)
    {
      info = init (a, svd_type);
    }

  SVD (const SVD& a)
    : type_computed (a.type_computed),
      sigma (a.sigma), left_sm (a.left_sm), right_sm (a.right_sm) { }

  SVD& operator = (const SVD& a)
    {
      if (this != &a)
	{
	  type_computed = a.type_computed;
	  sigma = a.sigma;
	  left_sm = a.left_sm;
	  right_sm = a.right_sm;
	}

      return *this;
    }

  ~SVD (void) { }

  DiagMatrix singular_values (void) const { return sigma; }

  Matrix left_singular_matrix (void) const;

  Matrix right_singular_matrix (void) const;

  friend std::ostream&  operator << (std::ostream& os, const SVD& a);

private:

  SVD::type type_computed;

  DiagMatrix sigma;
  Matrix left_sm;
  Matrix right_sm;

  int init (const Matrix& a, type svd_type = std);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
