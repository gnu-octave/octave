/*

Copyright (C) 2002 John W. Eaton

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

#if !defined (octave_DASRT_h)
#define octave_DASRT_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cfloat>
#include <cmath>

#include "DAERT.h"

class
DASRT_options
{
public:

  DASRT_options (void) { init (); }

  DASRT_options (const DASRT_options& opt) { copy (opt); }

  DASRT_options& operator = (const DASRT_options& opt)
    {
      if (this != &opt)
	copy (opt);

      return *this;
    }

  ~DASRT_options (void) { }

  void init (void)
    {
      double sqrt_eps = ::sqrt (DBL_EPSILON);
      x_absolute_tolerance = sqrt_eps;
      x_initial_step_size = -1.0;
      x_maximum_step_size = -1.0;
      x_minimum_step_size = 0.0;
      x_relative_tolerance = sqrt_eps;
      x_step_limit = -1;
    }

  void copy (const DASRT_options& opt)
    {
      x_absolute_tolerance = opt.x_absolute_tolerance;
      x_initial_step_size = opt.x_initial_step_size;
      x_maximum_step_size = opt.x_maximum_step_size;
      x_minimum_step_size = opt.x_minimum_step_size;
      x_relative_tolerance = opt.x_relative_tolerance;
      x_step_limit = opt.x_step_limit;
    }

  void set_default_options (void) { init (); }

  void set_absolute_tolerance (double val)
    { x_absolute_tolerance = (val > 0.0) ? val : ::sqrt (DBL_EPSILON); }

  void set_initial_step_size (double val)
    { x_initial_step_size = (val >= 0.0) ? val : -1.0; }

  void set_maximum_step_size (double val)
    { x_maximum_step_size = (val >= 0.0) ? val : -1.0; }

  void set_minimum_step_size (double val)
    { x_minimum_step_size = (val >= 0.0) ? val : 0.0; }

  void set_relative_tolerance (double val)
    { x_relative_tolerance = (val > 0.0) ? val : ::sqrt (DBL_EPSILON); }

  void set_step_limit (int val)
    { x_step_limit = (val >= 0) ? val : -1; }

  double absolute_tolerance (void) { return x_absolute_tolerance; }

  double initial_step_size (void) { return x_initial_step_size; }

  double maximum_step_size (void) { return x_maximum_step_size; }

  double minimum_step_size (void) { return x_minimum_step_size; }

  double relative_tolerance (void) { return x_relative_tolerance; }

  int step_limit (void) { return x_step_limit; }

private:

  double x_absolute_tolerance;
  double x_initial_step_size;
  double x_maximum_step_size;
  double x_minimum_step_size;
  double x_relative_tolerance;
  int x_step_limit;
};

class
DASRT_result
{
public:

  DASRT_result (void) { }

  DASRT_result (const Matrix& xx, const Matrix& xxdot, const ColumnVector& tt)
    : x (xx), xdot (xxdot), t (tt) { }

  DASRT_result (const DASRT_result& r)
    : x (r.x), xdot (r.xdot), t (r.t) { }

  DASRT_result& operator = (const DASRT_result& r)
    {
      if (this != &r)
	{
	  x = r.x;
	  xdot = r.xdot;
          t = r.t;
	}
      return *this;
    }

  ~DASRT_result (void) { }

  Matrix state (void) const { return x; }
  Matrix deriv (void) const { return xdot; }
  ColumnVector times (void) const { return t; }

private:

  Matrix x;
  Matrix xdot;
  ColumnVector t;
};

class
DASRT : public DAERT, public DASRT_options
{
public:

  DASRT (void);

  DASRT (const ColumnVector& state, double time, DAERTFunc& f);

  DASRT (const ColumnVector& state, const ColumnVector& deriv,
	 double time, DAERTFunc& f);

  ~DASRT (void) { }

  DASRT_result integrate (const ColumnVector& tout);

  DASRT_result integrate (const ColumnVector& tout,
			  const ColumnVector& tcrit); 

  std::string error_message (void) const;

private:

  bool initialized;

  bool sanity_checked;

  int liw;  
  int lrw;
  int idid;

  int n;
  int ng;

  Array<int> info;
  Array<int> iwork;
  Array<int> jroot;

  Array<double> rwork;

  double abs_tol;
  double rel_tol;

  double *px;
  double *pxdot;
  int *pinfo;
  int *piwork;
  double *prwork;
  int *pjroot;

  void integrate (double t);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
