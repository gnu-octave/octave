//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_AEPBALANCE_h)
#define octave_AEPBALANCE_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"

class AEPBALANCE
{
friend class Matrix;

public:

  AEPBALANCE (void) {}

  AEPBALANCE (const Matrix& a, const char *balance_job);

  AEPBALANCE (const AEPBALANCE& a);

  AEPBALANCE& operator = (const AEPBALANCE& a);
  Matrix balanced_matrix (void) const;
  Matrix balancing_matrix (void) const;
  friend ostream& operator << (ostream& os, const AEPBALANCE& a);

private:

  int init (const Matrix& a, const char * balance_job);

  Matrix balanced_mat;
  Matrix balancing_mat;
};

inline AEPBALANCE::AEPBALANCE (const Matrix& a,const char * balance_job) 
{
  init (a, balance_job); 
}

inline AEPBALANCE::AEPBALANCE (const AEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;
}

inline AEPBALANCE&
AEPBALANCE::operator = (const AEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;

  return *this;
}

inline Matrix AEPBALANCE::balanced_matrix (void) const
{
  return balanced_mat;
}

inline Matrix AEPBALANCE::balancing_matrix (void) const
{
  return balancing_mat;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
