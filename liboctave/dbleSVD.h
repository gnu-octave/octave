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

#if !defined (octave_SVD_h)
#define octave_SVD_h 1

class ostream;

#include "dDiagMatrix.h"
#include "dMatrix.h"

extern "C++" {

class SVD
{
friend class Matrix;

public:

  enum type
    {
      std,
      economy,
    };

  SVD (void) {}

  SVD (const Matrix& a, SVD::type svd_type = SVD::std);
  SVD (const Matrix& a, int& info, SVD::type svd_type = SVD::std);

  SVD (const SVD& a);

  SVD& operator = (const SVD& a);

  DiagMatrix singular_values (void) const;
  Matrix left_singular_matrix (void) const;
  Matrix right_singular_matrix (void) const;

  friend ostream&  operator << (ostream& os, const SVD& a);

private:

  int init (const Matrix& a, SVD::type svd_type = SVD::std);

  DiagMatrix sigma;
  Matrix left_sm;
  Matrix right_sm;
};

inline SVD::SVD (const Matrix& a, SVD::type svd_type)
{
  init (a, svd_type);
}

inline SVD::SVD (const Matrix& a, int& info, SVD::type svd_type)
{
  info = init (a, svd_type);
}

inline SVD::SVD (const SVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;
}

inline SVD&
SVD::operator = (const SVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;

  return *this;
}

inline DiagMatrix SVD::singular_values (void) const
{
  return sigma;
}

inline Matrix SVD::left_singular_matrix (void) const
{
  return left_sm;
}

inline Matrix SVD::right_singular_matrix (void) const
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
