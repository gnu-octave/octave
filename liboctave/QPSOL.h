// QPSOL.h                                                -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_QPSOL_h)
#define octave_QPSOL_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#ifndef QPSOL_MISSING

#include <cfloat>
#include <cmath>

#include "dMatrix.h"
#include "dColVector.h"
#include "QP.h"

class
QPSOL_options
{
public:

  QPSOL_options (void) { init (); }

  QPSOL_options (const QPSOL_options& opt) { set_options (opt); }

  QPSOL_options& operator = (const QPSOL_options& opt)
    {
      if (this != &opt)
	set_options (opt);

      return *this;
    }

  ~QPSOL_options (void) { }

  void init (void)
    {
      x_feasibility_tolerance = ::sqrt (DBL_EPSILON);
      x_infinite_bound = 1.0e+30;
      x_iteration_limit = -1;
      x_print_level = 0;
    }

  void set_default_options (void) { init (); }

  void set_options (const QPSOL_options& opt)
    {
      x_feasibility_tolerance = opt.x_feasibility_tolerance;
      x_infinite_bound = opt.x_infinite_bound;
      x_iteration_limit = opt.x_iteration_limit;
      x_print_level = opt.x_print_level;
    }

  void set_feasibility_tolerance (double val)
    { x_feasibility_tolerance = (val > 0.0) ? val : ::sqrt (DBL_EPSILON); }

  void set_infinite_bound (double val)
    { x_infinite_bound = (val > 0.0) ? val : 1.0e+30; }

  void set_iteration_limit (int val)
    { x_iteration_limit = (val > 0) ? val : -1; }

  void set_print_level (int val)
    { x_print_level = (val >= 0) ? val : 0; }

  double feasibility_tolerance (void) { return x_feasibility_tolerance; }

  double infinite_bound (void) { return x_infinite_bound; }

  int iteration_limit (void) { return x_iteration_limit; }

  int print_level (void) { return x_print_level; }

private:

  double x_feasibility_tolerance;
  double x_infinite_bound;
  int x_iteration_limit;
  int x_print_level;
};

class
QPSOL : public QP, public QPSOL_options
{
public:

  QPSOL (void)
    : QP (), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H)
    : QP (x, H), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const ColumnVector& c)
    : QP (x, H, c), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const Bounds& b)
    : QP (x, H, b), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const LinConst& lc)
    : QP (x, H, lc), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
	 const Bounds& b)
    : QP (x, H, c, b), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
	 const LinConst& lc)
    : QP (x, H, c, lc), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const Bounds& b,
	 const LinConst& lc)
    : QP (x, H, b, lc), QPSOL_options () { }

  QPSOL (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
	 const Bounds& b, const LinConst& lc)
    : QP (x, H, c, b, lc), QPSOL_options () { }

  QPSOL (const QPSOL& a)
    : QP (a), QPSOL_options (a) { }

  QPSOL& operator = (const QPSOL& a)
    {
      QP::operator = (a);
      QPSOL_options::operator = (a);
      return *this;
    }

  ColumnVector do_minimize (double& objf, int& inform, ColumnVector& lambda);
};

#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
