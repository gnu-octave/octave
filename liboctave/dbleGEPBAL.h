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

#if !defined (octave_GEPBALANCE_h)
#define octave_GEPBALANCE_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"

extern "C++" {

class GEPBALANCE
{
friend class Matrix;

public:

  GEPBALANCE (void) {}

  GEPBALANCE (const Matrix& a, const Matrix &, const char *balance_job);

  GEPBALANCE (const GEPBALANCE& a);

  GEPBALANCE& operator = (const GEPBALANCE& a);
  Matrix balanced_a_matrix (void) const;
  Matrix balanced_b_matrix (void) const;
  Matrix left_balancing_matrix (void) const;
  Matrix right_balancing_matrix (void) const;
  friend ostream& operator << (ostream& os, const GEPBALANCE& a);

private:

  int init (const Matrix& a, const Matrix& b, const char * balance_job);

  Matrix balanced_a_mat;
  Matrix balanced_b_mat;
  Matrix left_balancing_mat;
  Matrix right_balancing_mat;
};

inline GEPBALANCE::GEPBALANCE (const Matrix& a, const Matrix& b, 
			       const char * balance_job) 
{
  init (a, b, balance_job); 
}

inline GEPBALANCE::GEPBALANCE (const GEPBALANCE& a)
{
  balanced_a_mat = a.balanced_a_mat;
  balanced_b_mat = a.balanced_b_mat;
  left_balancing_mat = a.left_balancing_mat;
  right_balancing_mat = a.right_balancing_mat;
}

inline GEPBALANCE&
GEPBALANCE::operator = (const GEPBALANCE& a)
{
  balanced_a_mat = a.balanced_a_mat;
  balanced_b_mat = a.balanced_b_mat;
  left_balancing_mat = a.left_balancing_mat;
  right_balancing_mat = a.right_balancing_mat;

  return *this;
}

inline Matrix GEPBALANCE::balanced_a_matrix (void) const 
{
  return balanced_a_mat;
}

inline Matrix GEPBALANCE::balanced_b_matrix (void) const 
{
  return balanced_b_mat;
}

inline Matrix GEPBALANCE::left_balancing_matrix (void) const 
{
  return left_balancing_mat;
}

inline Matrix GEPBALANCE::right_balancing_matrix (void) const 
{
  return right_balancing_mat;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
