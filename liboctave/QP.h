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

#if !defined (octave_QP_h)
#define octave_QP_h 1

#include "dMatrix.h"
#include "dColVector.h"
#include "Bounds.h"
#include "LinConst.h"
#include "base-min.h"

class
QP : public base_minimizer
{
public:

  QP (void)
    : base_minimizer (), H (), c (), bnds (), lc () { }

  QP (const ColumnVector& x, const Matrix& H_arg)
    : base_minimizer (x), H (H_arg), c (), bnds (), lc ()
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg)
    : base_minimizer (x), H (H_arg), c (c_arg), bnds (), lc ()
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const Bounds& b)
    : base_minimizer (x), H (H_arg), c (), bnds (b), lc ()
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const LinConst& l)
    : base_minimizer (x), H (H_arg), c (), bnds (), lc (l)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg,
      const Bounds& b)
    : base_minimizer (x), H (H_arg), c (c_arg), bnds (b), lc ()
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg,
      const LinConst& l)
    : base_minimizer (x), H (H_arg), c (c_arg), bnds (), lc (l)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const Bounds& b,
      const LinConst& l)
    : base_minimizer (x), H (H_arg), c (), bnds (b), lc (l)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg,
      const Bounds& b, const LinConst& l)
    : base_minimizer (x), H (H_arg), c (c_arg), bnds (b), lc (l)
      { make_h_symmetric (); }

  QP (const QP& qp)
    : base_minimizer (qp), H (qp.H), c (qp.c), bnds (qp.bnds), lc (qp.lc) { }

  QP& operator = (const QP& qp)
    {
      if (this != &qp)
	{
	  base_minimizer::operator = (qp);

	  H = qp.H;
	  c = qp.c;
	  bnds = qp.bnds;
	  lc = qp.lc;
	}
      return *this;
    }

  virtual ~QP (void) { }

  Matrix hessian (void) const { return H; }

  ColumnVector linear_obj_coeff (void) const { return c; }

  Bounds bounds (void) const { return bnds; }

  LinConst linear_constraints (void) const { return lc; }

protected:

  Matrix H;  
  ColumnVector c;
  Bounds bnds;
  LinConst lc;

private:

  Matrix make_h_symmetric (void) { return 0.5 * (H + H.transpose ()); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
