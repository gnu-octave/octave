// LSODE.h                                                -*- C++ -*-
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

#if !defined (octave_LSODE_h)
#define octave_LSODE_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#if 0
class ostream;
#endif

#include <cfloat>
#include <cmath>

#include "ODE.h"

class
LSODE_options
{
public:

  LSODE_options (void) { init (); }

  LSODE_options (const LSODE_options& opt) { copy (opt); }

  LSODE_options& operator = (const LSODE_options& opt)
    {
      if (this != &opt)
	copy (opt);

      return *this;
    }

  ~LSODE_options (void) { }

  void init (void)
    {
      double sqrt_eps = ::sqrt (DBL_EPSILON);

      x_absolute_tolerance = sqrt_eps;
      x_initial_step_size = -1.0;
      x_maximum_step_size = -1.0;
      x_minimum_step_size = 0.0;
      x_relative_tolerance = sqrt_eps;
    }

  void copy (const LSODE_options& opt)
    {
      x_absolute_tolerance = opt.x_absolute_tolerance;
      x_initial_step_size = opt.x_initial_step_size;
      x_maximum_step_size = opt.x_maximum_step_size;
      x_minimum_step_size = opt.x_minimum_step_size;
      x_relative_tolerance = opt.x_relative_tolerance;
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

  double absolute_tolerance (void)
    { return x_absolute_tolerance; }

  double initial_step_size (void)
    { return x_initial_step_size; }

  double maximum_step_size (void)
    { return x_maximum_step_size; }

  double minimum_step_size (void)
    { return x_minimum_step_size; }

  double relative_tolerance (void)
    {  return x_relative_tolerance; }

private:

  double x_absolute_tolerance;
  double x_initial_step_size;
  double x_maximum_step_size;
  double x_minimum_step_size;
  double x_relative_tolerance;
};

class
LSODE : public ODE, public LSODE_options
{
public:

  LSODE (void);

  LSODE (int n);
  
  LSODE (const ColumnVector& state, double time, const ODEFunc& f);

  ~LSODE (void);

  void force_restart (void);

  void set_stop_time (double t);
  void clear_stop_time (void);

  ColumnVector do_integrate (double t);

  Matrix do_integrate (const ColumnVector& tout);

#if 0
  void integrate (int nsteps, double tstep, ostream& s);
#endif

  Matrix integrate (const ColumnVector& tout)
    { return do_integrate (tout); }

  Matrix integrate (const ColumnVector& tout, const ColumnVector& tcrit);

private:

  double stop_time;
  int stop_time_set;

  int n;
  int integration_error;
  int restart;
  int method_flag;
  int *iwork;
  double *rwork;
  int istate;
  int itol;
  int itask;
  int iopt;
  int liw;
  int lrw;

  friend int lsode_f (int *neq, double *t, double *y, double *ydot);

  friend int lsode_j (int *neq, double *t, double *y, int *ml, int *mu,
		      double *pd, int *nrowpd);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
