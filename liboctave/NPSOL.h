// NPSOL.h                                                -*- C++ -*-
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

#if !defined (octave_NPSOL_h)
#define octave_NPSOL_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#ifndef NPSOL_MISSING

#include <cfloat>
#include <cmath>

#include "dColVector.h"
#include "NLP.h"

class
NPSOL_options
{
public:

  NPSOL_options (void) { init (); }

  NPSOL_options (const NPSOL_options& opt) { set_options (opt); }

  NPSOL_options& operator = (const NPSOL_options& opt)
    {
      if (this != &opt)
	set_options (opt);

      return *this;
    }

  ~NPSOL_options (void) { }

  void init (void);

  void set_default_options (void) { init (); }

  void set_options (const NPSOL_options& opt);

// XXX FIXME XXX -- is this a good idea?

// Passing invalid values to the set_* functions will result in
// setting the default option.

  void set_central_difference_interval (double val)
    { x_central_difference_interval = (val > 0.0) ? val : -1.0; }

  void set_crash_tolerance (double val)
    { x_crash_tolerance = (val >= 0.0) ? val : 0.1; }

  void set_difference_interval (double val)
    { x_difference_interval = (val > 0.0) ? val : -1.0; }

  void set_function_precision (double val)
    { x_function_precision = (val > 0.0) ? val : ::pow (DBL_EPSILON, 0.9); }

  void set_infinite_bound (double val)
    { x_infinite_bound = (val > 0.0) ? val : 1.0e+30; }

  void set_infinite_step (double val)
    { x_infinite_step = (val > 0.0) ? val : 1.0e+30; }

  void set_linear_feasibility_tolerance (double val)
    {
      x_linear_feasibility_tolerance
	= (val > 0.0) ? val : ::sqrt (DBL_EPSILON);
    }

  void set_linesearch_tolerance (double val)
    { x_linesearch_tolerance = (val >= 0.0 && val < 1.0) ? val : 0.9; }

  void set_nonlinear_feasibility_tolerance (double val)
    {
      x_nonlinear_feasibility_tolerance
	= (val > 0.0) ? val : ::sqrt (DBL_EPSILON);
    }

  void set_optimality_tolerance (double val)
    { x_optimality_tolerance = (val > 0.0) ? val : ::pow (DBL_EPSILON, 0.8); }

  void set_derivative_level (int val)
    { x_derivative_level = (val >= 0 && val < 4) ? val : 0; }

  void set_major_iteration_limit (int val)
    { x_major_iteration_limit = (val > 0) ? val : -1; }

  void set_minor_iteration_limit (int val)
    { x_minor_iteration_limit = (val > 0) ? val : -1; }

  void set_major_print_level (int val)
    { x_major_print_level = (val >= 0) ? val : -1; }

  void set_minor_print_level (int val)
    { x_minor_print_level = (val >= 0) ? val : -1; }

  void set_start_objective_check (int val)
    { x_start_objective_check = (val >= 0) ? val : -1; }

  void set_start_constraint_check (int val)
    { x_start_constraint_check = (val >= 0) ? val : -1; }

  void set_stop_objective_check (int val)
    { x_stop_objective_check = (val >= 0) ? val : -1; }

  void set_stop_constraint_check (int val)
    { x_stop_constraint_check = (val >= 0) ? val : -1; }

  void set_verify_level (int val)
    {
      x_verify_level
	= ((val > -1 && val < 4) || (val > 9 && val < 14)) ? val : 0;
    }

  double central_difference_interval (void) const
    { return x_central_difference_interval; }

  double crash_tolerance (void) const
    { return x_crash_tolerance; }

  double difference_interval (void) const
    { return x_difference_interval; }

  double function_precision (void) const
    { return x_function_precision; }

  double infinite_bound (void) const
    { return x_infinite_bound; }

  double infinite_step (void) const
    { return x_infinite_step; }

  double linear_feasibility_tolerance (void) const
    { return x_linear_feasibility_tolerance; }

  double linesearch_tolerance (void) const
    { return x_linesearch_tolerance; }

  double nonlinear_feasibility_tolerance (void) const
    { return x_nonlinear_feasibility_tolerance; }

  double optimality_tolerance (void) const
    { return x_optimality_tolerance; }

  int derivative_level (void) const
    { return x_derivative_level; }

  int major_iteration_limit (void) const
    { return x_major_iteration_limit; }

  int minor_iteration_limit (void) const
    { return x_minor_iteration_limit; }

  int major_print_level (void) const
    { return x_major_print_level; }

  int minor_print_level (void) const
    { return x_minor_print_level; }

  int start_objective_check (void) const
    { return x_start_objective_check; }

  int start_constraint_check (void) const
    { return x_start_constraint_check; }

  int stop_objective_check (void) const
    { return x_stop_objective_check; }

  int stop_constraint_check (void) const
    { return x_stop_constraint_check; }

  int verify_level (void) const
    { return x_verify_level; }

protected:

  void pass_options_to_npsol (void);

  void set_option (const char *key, int opt);
  void set_option (const char *key, double opt);

private:

  double x_central_difference_interval;
  double x_crash_tolerance;
  double x_difference_interval;
  double x_function_precision;
  double x_infinite_bound;
  double x_infinite_step;
  double x_linear_feasibility_tolerance;
  double x_linesearch_tolerance;
  double x_nonlinear_feasibility_tolerance;
  double x_optimality_tolerance;

  int x_derivative_level;
  int x_major_iteration_limit;
  int x_minor_iteration_limit;
  int x_major_print_level;
  int x_minor_print_level;
  int x_start_objective_check;
  int x_start_constraint_check;
  int x_stop_objective_check;
  int x_stop_constraint_check;
  int x_verify_level;
};

class
NPSOL : public NLP, public NPSOL_options
{
public:

  NPSOL (void)
    : NLP (), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi)
    : NLP (x, phi), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi, const Bounds& b)
    : NLP (x, phi, b), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi, const Bounds& b,
	 const LinConst& lc)
    : NLP (x, phi, b, lc), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi, const Bounds& b,
	 const LinConst& lc, const NLConst& nlc)
    : NLP (x, phi, b, lc, nlc), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi, const LinConst& lc)
    : NLP (x, phi, lc), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi, const LinConst& lc,
	 const NLConst& nlc)
    : NLP (x, phi, lc, nlc), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi,
	 const NLConst& nlc)
    : NLP (x, phi, nlc), NPSOL_options () { }

  NPSOL (const ColumnVector& x, const Objective& phi, const Bounds& b,
	 const NLConst& nlc)
    : NLP (x, phi, b, nlc), NPSOL_options () { }

  NPSOL (const NPSOL& a)
    : NLP (a), NPSOL_options () { }

  NPSOL& operator = (const NPSOL& a)
    {
      if (this != &a)
	{
	  NLP::operator = (a);
	  NPSOL_options::operator = (a);
	}
      return *this;
    }

  ~NPSOL (void) { }

  ColumnVector do_minimize (double& objf, int& inform, ColumnVector& lambda);

private:

  int attempt;
};

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the objective
// function, and the user wants us to quit.
extern int npsol_objective_error;

#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
