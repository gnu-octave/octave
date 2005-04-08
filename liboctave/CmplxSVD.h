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

#if !defined (octave_ComplexSVD_h)
#define octave_ComplexSVD_h 1

#include <iostream>

#include "dDiagMatrix.h"
#include "CMatrix.h"
#include "dbleSVD.h"

class
ComplexSVD
{
public:

  ComplexSVD (void) { }

  ComplexSVD (const ComplexMatrix& a, SVD::type svd_type = SVD::std)
    {
      init (a, svd_type);
    }

  ComplexSVD (const ComplexMatrix& a, octave_idx_type& info,
	      SVD::type svd_type = SVD::std)
    {
      info = init (a, svd_type);
    }

  ComplexSVD (const ComplexSVD& a)
    : type_computed (a.type_computed),
      sigma (a.sigma), left_sm (a.left_sm), right_sm (a.right_sm) { }

  ComplexSVD& operator = (const ComplexSVD& a)
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

  ~ComplexSVD (void) { }

  DiagMatrix singular_values (void) const { return sigma; }

  ComplexMatrix left_singular_matrix (void) const;

  ComplexMatrix right_singular_matrix (void) const;

  friend std::ostream&  operator << (std::ostream& os, const ComplexSVD& a);

private:

  SVD::type type_computed;

  DiagMatrix sigma;
  ComplexMatrix left_sm;
  ComplexMatrix right_sm;

  octave_idx_type init (const ComplexMatrix& a, SVD::type svd_type = SVD::std);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
