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

#if !defined (octave_GEPBALANCE_h)
#define octave_GEPBALANCE_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include <string>

#include "dMatrix.h"

class
GEPBALANCE
{
public:

  GEPBALANCE (void)
    : balanced_a_mat (), balanced_b_mat (), left_balancing_mat (),
      right_balancing_mat () { }

  GEPBALANCE (const Matrix& a, const Matrix& b, const string& balance_job)
    {
      init (a, b, balance_job); 
    }

  GEPBALANCE (const GEPBALANCE& a)
    : balanced_a_mat (a.balanced_a_mat),
      balanced_b_mat (a.balanced_b_mat),
      left_balancing_mat (a.left_balancing_mat),
      right_balancing_mat (a.right_balancing_mat) { }

  GEPBALANCE& operator = (const GEPBALANCE& a)
    {
      if (this != &a)
	{
	  balanced_a_mat = a.balanced_a_mat;
	  balanced_b_mat = a.balanced_b_mat;
	  left_balancing_mat = a.left_balancing_mat;
	  right_balancing_mat = a.right_balancing_mat;
	}
      return *this;
    }

  ~GEPBALANCE (void) { }

  Matrix balanced_a_matrix (void) const { return balanced_a_mat; }
  Matrix balanced_b_matrix (void) const { return balanced_b_mat; }

  Matrix left_balancing_matrix (void) const { return left_balancing_mat; }
  Matrix right_balancing_matrix (void) const { return right_balancing_mat; }

  friend ostream& operator << (ostream& os, const GEPBALANCE& a);

private:

  Matrix balanced_a_mat;
  Matrix balanced_b_mat;
  Matrix left_balancing_mat;
  Matrix right_balancing_mat;

  int init (const Matrix& a, const Matrix& b, const string& balance_job);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
