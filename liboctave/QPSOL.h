// QPSOL.h                                                -*- C++ -*-
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

#if !defined (octave_QPSOL_h)
#define octave_QPSOL_h 1

extern "C++" {

#ifndef QPSOL_MISSING

#include "Matrix.h"
#include "QP.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class QPSOL_options
{
 public:

  QPSOL_options (void);
  QPSOL_options (const QPSOL_options& opt);

  QPSOL_options& operator = (const QPSOL_options& opt);

  ~QPSOL_options (void);

  void init (void);
  void copy (const QPSOL_options& opt);

  void set_default_options (void);

  void set_feasibility_tolerance (double);
  void set_infinite_bound (double);
  void set_iteration_limit (int);
  void set_print_level (int);

  double feasibility_tolerance (void);
  double infinite_bound (void);
  int iteration_limit (void);
  int print_level (void);

 private:

  double x_feasibility_tolerance;
  double x_infinite_bound;
  int x_iteration_limit;
  int x_print_level;
};

class QPSOL : public QP, public QPSOL_options
{
 public:

  QPSOL (void) : QP ()
    { }

  QPSOL (const Vector& x, const Matrix& H) : QP (x, H)
    { }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c) : QP (x, H, c)
    { }

  QPSOL (const Vector& x, const Matrix& H, const Bounds& b) : QP (x, H, b)
    { }

  QPSOL (const Vector& x, const Matrix& H, const LinConst& lc) : QP (x, H, lc)
    { }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c, const Bounds& b)
    : QP (x, H, c, b) { }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c, const LinConst& lc)
    : QP (x, H, c, lc) { }

  QPSOL (const Vector& x, const Matrix& H, const Bounds& b, const LinConst& lc)
    : QP (x, H, b, lc) { }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c, const Bounds& b,
      const LinConst& lc)
    : QP (x, H, c, b, lc) { }

  QPSOL (const QPSOL& a);

  QPSOL& operator = (const QPSOL& a);

  Vector minimize (double& objf, int& inform, Vector& lambda);
};

inline QPSOL::QPSOL (const QPSOL& a) : QP (a.x, a.H, a.c, a.bnds, a.lc)
  { }

inline QPSOL&
QPSOL::operator = (const QPSOL& a)
{
  x = a.x;
  H = a.H;
  c = a.c;
  bnds = a.bnds;
  lc = a.lc;
  return *this;
}

#endif /* QPSOL_MISSING */

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
