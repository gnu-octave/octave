// LinConst.h                                                -*- C++ -*-
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

#if !defined (octave_LinConst_h)
#define octave_LinConst_h 1

class ostream;

class ColumnVector;

#include <float.h>

#include "dMatrix.h"
#include "Bounds.h"

extern "C++" {

#ifndef Vector
#define Vector ColumnVector
#endif

class LinConst : public Bounds
{
public:

  LinConst (void);
  LinConst (int nclin, int nx);

  LinConst (int nclin_eq, int nclin_ineq, int nx);

  LinConst (const Vector& lb, const Matrix& A, const Vector& ub);

  LinConst (const Matrix& A_eq, const Vector& b_eq,
	    const Matrix& A_ineq, const Vector& b_ineq);

  LinConst (const LinConst& a);

  LinConst& operator = (const LinConst& a);

  LinConst& resize (int nclin, int n);

  Matrix constraint_matrix (void) const;

  LinConst& set_constraint_matrix (const Matrix& A);

  Matrix eq_constraint_matrix (void) const;
  Matrix ineq_constraint_matrix (void) const;

  Vector eq_constraint_vector (void) const;
  Vector ineq_constraint_vector (void) const;

  friend ostream& operator << (ostream& os, const LinConst& b);

protected:

  Matrix A;

private:

  void error (const char *msg);

};

inline LinConst::LinConst (void) : Bounds () {}

inline LinConst::LinConst (int nc, int n) : Bounds (nc), A (nb, n) {}

inline LinConst::LinConst (int eq, int ineq, int n)
  : Bounds (eq+ineq), A (nb, n) {}

inline LinConst::LinConst (const Vector& l, const Matrix& amat,
			   const Vector& u)
  : Bounds (l, u), A (amat)
{
  if (nb != amat.rows ())
    error ("inconsistent sizes for constraint matrix and bounds vectors");
}

inline LinConst::LinConst (const LinConst& a)
  : Bounds (a.lb, a.ub), A (a.constraint_matrix ()) {}

inline LinConst&
LinConst::operator = (const LinConst& a)
{
  nb = a.nb;
  lb = a.lb;
  A  = a.A;
  ub = a.ub;

  return *this;
}

inline Matrix
LinConst::constraint_matrix (void) const
{
  return A;
}

inline LinConst&
LinConst::set_constraint_matrix (const Matrix& amat)
{
  if (lb.capacity () != amat.rows ())
    error ("inconsistent size for new linear constraint matrix");

  A = amat;

  return *this;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
