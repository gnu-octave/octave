// QP.h                                                -*- C++ -*-
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

#if !defined (octave_QP_h)
#define octave_QP_h 1

extern "C++" {

#include "Matrix.h"
#include "Bounds.h"
#include "LinConst.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class QP
{
 public:

  QP (void);
  QP (const Vector& x, const Matrix& H);
  QP (const Vector& x, const Matrix& H, const Vector& c);
  QP (const Vector& x, const Matrix& H, const Bounds& b);
  QP (const Vector& x, const Matrix& H, const LinConst& lc);
  QP (const Vector& x, const Matrix& H, const Vector& c, const Bounds& b);
  QP (const Vector& x, const Matrix& H, const Vector& c, const LinConst& lc);
  QP (const Vector& x, const Matrix& H, const Bounds& b, const LinConst& lc);
  QP (const Vector& x, const Matrix& H, const Vector& c, const Bounds& b,
      const LinConst& lc);

  virtual Vector minimize (void);
  virtual Vector minimize (double& objf);
  virtual Vector minimize (double& objf, int& inform);
  virtual Vector minimize (double& objf, int& inform, Vector& lambda) = 0;

  virtual Vector minimize (const Vector& x);
  virtual Vector minimize (const Vector& x, double& objf);
  virtual Vector minimize (const Vector& x, double& objf, int& inform);
  virtual Vector minimize (const Vector& x, double& objf, int& inform,
			   Vector& lambda);

 protected:

  Vector x;
  Matrix H;  
  Vector c;
  Bounds bnds;
  LinConst lc;

 private:

  Matrix make_h_symmetric (void);
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
