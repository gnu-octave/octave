// QP.h                                                -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_QP_h)
#define octave_QP_h 1

#include "dMatrix.h"
#include "dColVector.h"
#include "Bounds.h"
#include "LinConst.h"
#include "base-min.h"

class QP : public base_minimizer
{
 public:

  QP (void) : base_minimizer () { }

  QP (const ColumnVector& x, const Matrix& H_arg)
    : base_minimizer (x), H (H_arg)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg)
    : base_minimizer (x), H (H_arg), c (c_arg)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const Bounds& b)
    : base_minimizer (x), H (H_arg), bnds (b)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const LinConst& l)
    : base_minimizer (x), H (H_arg), lc (l)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg,
      const Bounds& b)
    : base_minimizer (x), H (H_arg), c (c_arg), bnds (b)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg,
      const LinConst& l)
    : base_minimizer (x), H (H_arg), c (c_arg), lc (l)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const Bounds& b,
      const LinConst& l)
    : base_minimizer (x), H (H_arg), bnds (b), lc (l)
      { make_h_symmetric (); }

  QP (const ColumnVector& x, const Matrix& H_arg, const ColumnVector& c_arg,
      const Bounds& b, const LinConst& l)
    : base_minimizer (x), H (H_arg), c (c_arg), bnds (b), lc (l)
      { make_h_symmetric (); }

  virtual ~QP (void) { }

 protected:

  ColumnVector x;
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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
