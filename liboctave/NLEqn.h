// NLEqn.h                                                -*- C++ -*-
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

#if !defined (octave_NLEqn_h)
#define octave_NLEqn_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cfloat>
#include <cmath>

#include "dColVector.h"
#include "NLFunc.h"

class NLEqn_options
{
 public:

  NLEqn_options (void) { init (); }

  NLEqn_options (const NLEqn_options& opt) { copy (opt); }

  NLEqn_options& operator = (const NLEqn_options& opt)
    {
      if (this != &opt)
	copy (opt);

      return *this;
    }

  ~NLEqn_options (void) { }

  void init (void)
    {
      double sqrt_eps = sqrt (DBL_EPSILON);
      x_tolerance = sqrt_eps;
    }

  void copy (const NLEqn_options& opt)
    {
      x_tolerance = opt.x_tolerance;
    }

  void set_default_options (void) { init (); }

  void set_tolerance (double val)
    {
      x_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
    }

  double tolerance (void) { return x_tolerance; }

 private:

  double x_tolerance;
};

class NLEqn : public NLFunc, public NLEqn_options
{
 public:

// Constructors

  NLEqn (void) : NLFunc (), n (0), x () { }

  NLEqn (const ColumnVector& xvec, const NLFunc f) 
    : NLFunc (f), n (xvec.capacity ()), x (xvec) { }

  NLEqn (const NLEqn& a) : NLFunc (a.fun, a.jac), n (a.n), x (a.x) { }

  NLEqn& operator = (const NLEqn& a)
    {
      fun = a.fun;
      jac = a.jac;
      x = a.n;

      return *this;
    }

  void resize (int nn)
    {
      if (n != nn)
	{
	  n = nn;
	  x.resize (n);
	}
    }

  void set_states (const ColumnVector&);

  ColumnVector states (void) const { return x; }

  int size (void) const { return n; }

  ColumnVector solve (void)
    {
      int info;
      return solve (info);
    }

  ColumnVector solve (const ColumnVector& xvec)
    {
      set_states (xvec);
      int info;
      return solve (info);
    }

  ColumnVector solve (const ColumnVector& xvec, int& info)
    {
      set_states (xvec);
      return solve (info);
    }

  ColumnVector solve (int& info);

 private:

  int n;
  ColumnVector x;

  void error (const char* msg);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
