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

#if !defined (octave_AEPBALANCE_h)
#define octave_AEPBALANCE_h 1

#include <iostream>
#include <string>

#include "dMatrix.h"

class
AEPBALANCE
{
public:

  AEPBALANCE (void) : balanced_mat (), balancing_mat () { }

  AEPBALANCE (const Matrix& a,const std::string& balance_job)
    {
      init (a, balance_job); 
    }

  AEPBALANCE (const AEPBALANCE& a)
    : balanced_mat (a.balanced_mat), balancing_mat (a.balancing_mat) { }

  AEPBALANCE& operator = (const AEPBALANCE& a)
    {
      if (this != &a)
	{
	  balanced_mat = a.balanced_mat;
	  balancing_mat = a.balancing_mat;
	}
      return *this;
    }

  ~AEPBALANCE (void) { }

  Matrix balanced_matrix (void) const { return balanced_mat; }

  Matrix balancing_matrix (void) const { return balancing_mat; }

  friend std::ostream& operator << (std::ostream& os, const AEPBALANCE& a);

private:

  Matrix balanced_mat;
  Matrix balancing_mat;

  octave_idx_type init (const Matrix& a, const std::string& balance_job);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
