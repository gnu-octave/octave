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

#if !defined (octave_ODESSA_h)
#define octave_ODESSA_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cfloat>
#include <cmath>

#include "ODES.h"

class
ODESSA_options
{
public:

  ODESSA_options (void) { init (); }

  ODESSA_options (const ODESSA_options& opt) { copy (opt); }

  ODESSA_options& operator = (const ODESSA_options& opt)
    {
      if (this != &opt)
	copy (opt);

      return *this;
    }

  ~ODESSA_options (void) { }

  void init (void)
    {
      double sqrt_eps = ::sqrt (DBL_EPSILON);
      x_absolute_tolerance.resize (1);
      x_absolute_tolerance(0) = sqrt_eps;
      x_initial_step_size = -1.0;
      x_integration_method = "stiff";
      x_maximum_step_size = -1.0;
      x_minimum_step_size = 0.0;
      x_relative_tolerance = sqrt_eps;

      // This is consistent with earlier versions of Octave, and is
      // much larger than the default of 500 specified in the LSODE
      // sources.
      x_step_limit = 100000;
    }

  void copy (const ODESSA_options& opt)
    {
      x_absolute_tolerance = opt.x_absolute_tolerance;
      x_initial_step_size = opt.x_initial_step_size;
      x_integration_method = opt.x_integration_method;
      x_maximum_step_size = opt.x_maximum_step_size;
      x_minimum_step_size = opt.x_minimum_step_size;
      x_relative_tolerance = opt.x_relative_tolerance;
      x_step_limit = opt.x_step_limit;
    }

  void set_default_options (void) { init (); }

  void set_absolute_tolerance (double val)
    {
      x_absolute_tolerance.resize (1);
      x_absolute_tolerance(0) = (val > 0.0) ? val : ::sqrt (DBL_EPSILON);
    }

  void set_absolute_tolerance (const Array<double>& val)
    { x_absolute_tolerance = val; }

  void set_initial_step_size (double val)
    { x_initial_step_size = (val >= 0.0) ? val : -1.0; }

  void set_integration_method (const std::string& val);


  void set_maximum_step_size (double val)
    { x_maximum_step_size = (val >= 0.0) ? val : -1.0; }

  void set_minimum_step_size (double val)
    { x_minimum_step_size = (val >= 0.0) ? val : 0.0; }

  void set_relative_tolerance (double val)
    { x_relative_tolerance = (val > 0.0) ? val : ::sqrt (DBL_EPSILON); }

  void set_step_limit (int val)
    { x_step_limit = val; }

  Array<double> absolute_tolerance (void) const
    { return x_absolute_tolerance; }

  double initial_step_size (void) const
    { return x_initial_step_size; }

  std::string integration_method (void) const
    { return x_integration_method; }

  double maximum_step_size (void) const
    { return x_maximum_step_size; }

  double minimum_step_size (void) const
    { return x_minimum_step_size; }

  double relative_tolerance (void) const
    {  return x_relative_tolerance; }

  int step_limit (void) const
    { return x_step_limit; }

private:

  Array<double> x_absolute_tolerance;
  double x_initial_step_size;
  std::string x_integration_method;
  double x_maximum_step_size;
  double x_minimum_step_size;
  double x_relative_tolerance;

  int x_step_limit;
};


class
ODESSA_result
{
public:

  ODESSA_result (void) { }

  ODESSA_result (const Matrix& xx, 
		 const Array<Matrix>& xx_s)

    : x (xx), x_s (xx_s) { }

  ODESSA_result (const ODESSA_result& r)
    : x (r.x), x_s (r.x_s) { }

  ODESSA_result& operator = (const ODESSA_result& r)
    {
      if (this != &r)
	{
	  x = r.x;
	  x_s = r.x_s;
	}
      return *this;
    }

  ~ODESSA_result (void) { }

  Matrix state (void) const { return x; }
  Array<Matrix> state_sensitivity (void) const { return x_s; }

private:

  Matrix x;
  Array<Matrix> x_s;
};

class
ODESSA : public ODES, public ODESSA_options
{
public:

  ODESSA (void);

  ODESSA (const ColumnVector& x, double time, ODESFunc& f);

  ODESSA (const ColumnVector& x, const ColumnVector& theta,
	  const Matrix& sensitivity_guess, double time, ODESFunc& f);

  ~ODESSA (void) { }

  ODESSA_result integrate (const ColumnVector& tout);

  ODESSA_result integrate (const ColumnVector& tout,
			   const ColumnVector& tcrit); 

  std::string error_message (void) const;

private:

  bool initialized;

  bool sanity_checked;

  int liw;  
  int lrw;
  int method_flag;
  Array<int> iwork;
  Array<double> rwork;
  int itask;
  Array<int> iopt;
  int isopt;

  Array<int> neq;

  int n;
  int npar;

  // Hey, check out this crap: ZZZZ
  Array<double> par;

  Matrix sx0;

  Matrix y;


  double *py;
  double *ppar;
  int *piwork;
  int *piopt;
  int *pneq;
  double *prwork;

  void init_work_size (int);

  void integrate (double t);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
