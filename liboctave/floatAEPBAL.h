/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005, 2006,
              2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_FloatAEPBALANCE_h)
#define octave_FloatAEPBALANCE_h 1

#include <iostream>
#include <string>

#include "fMatrix.h"

class
OCTAVE_API
FloatAEPBALANCE
{
public:

  FloatAEPBALANCE (void) : balanced_mat (), balancing_mat () { }

  FloatAEPBALANCE (const FloatMatrix& a,const std::string& balance_job)
    {
      init (a, balance_job); 
    }

  FloatAEPBALANCE (const FloatAEPBALANCE& a)
    : balanced_mat (a.balanced_mat), balancing_mat (a.balancing_mat) { }

  FloatAEPBALANCE& operator = (const FloatAEPBALANCE& a)
    {
      if (this != &a)
	{
	  balanced_mat = a.balanced_mat;
	  balancing_mat = a.balancing_mat;
	}
      return *this;
    }

  ~FloatAEPBALANCE (void) { }

  FloatMatrix balanced_matrix (void) const { return balanced_mat; }

  FloatMatrix balancing_matrix (void) const { return balancing_mat; }

  friend std::ostream& operator << (std::ostream& os, const FloatAEPBALANCE& a);

private:

  FloatMatrix balanced_mat;
  FloatMatrix balancing_mat;

  octave_idx_type init (const FloatMatrix& a, const std::string& balance_job);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
