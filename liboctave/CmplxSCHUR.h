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

#if !defined (octave_ComplexSCHUR_h)
#define octave_ComplexSCHUR_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>
#include <string>

#include "CMatrix.h"

class
ComplexSCHUR
{
public:

  ComplexSCHUR (void)
    : schur_mat (), unitary_mat () { }

  ComplexSCHUR (const ComplexMatrix& a, const std::string& ord,
		bool calc_unitary = true)
    : schur_mat (), unitary_mat () { init (a, ord, calc_unitary); }

  ComplexSCHUR (const ComplexMatrix& a, const std::string& ord, int& info,
		bool calc_unitary = true)
    : schur_mat (), unitary_mat () { info = init (a, ord, calc_unitary); }

  ComplexSCHUR (const ComplexSCHUR& a)
    : schur_mat (a.schur_mat), unitary_mat (a.unitary_mat) { }

  ComplexSCHUR& operator = (const ComplexSCHUR& a)
    {
      if (this != &a)
	{
	  schur_mat = a.schur_mat;
	  unitary_mat = a.unitary_mat;
	}
      return *this;
    }

  ~ComplexSCHUR (void) { }

  ComplexMatrix schur_matrix (void) const { return schur_mat; }

  ComplexMatrix unitary_matrix (void) const { return unitary_mat; }

  friend std::ostream& operator << (std::ostream& os, const ComplexSCHUR& a);

  typedef int (*select_function) (const Complex&);

private:

  ComplexMatrix schur_mat;
  ComplexMatrix unitary_mat;

  select_function selector;

  int init (const ComplexMatrix& a, const std::string& ord, bool calc_unitary);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
