// NPSOL.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_NPSOL_h)
#define _NPSOL_h 1

#ifndef NPSOL_MISSING

#include "Matrix.h"
#include "NLP.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class NPSOL_options
{
 public:

  NPSOL_options (void);
  NPSOL_options (const NPSOL_options& opt);

  NPSOL_options& operator = (const NPSOL_options& opt);

  ~NPSOL_options (void);

  void init (void);
  void copy (const NPSOL_options& opt);

  void set_default_options (void);

  void set_central_difference_interval (double val);
  void set_crash_tolerance (double val);
  void set_difference_interval (double val);
  void set_function_precision (double val);
  void set_infinite_bound (double val);
  void set_infinite_step (double val);
  void set_linear_feasibility_tolerance (double val);
  void set_linesearch_tolerance (double val);
  void set_nonlinear_feasibility_tolerance (double val);
  void set_optimality_tolerance (double val);

  void set_derivative_level (int val);
  void set_major_iteration_limit (int val);
  void set_minor_iteration_limit (int val);
  void set_major_print_level (int val);
  void set_minor_print_level (int val);
  void set_start_objective_check (int val);
  void set_start_constraint_check (int val);
  void set_stop_objective_check (int val);
  void set_stop_constraint_check (int val);
  void set_verify_level (int val);

  double central_difference_interval (void) const;
  double crash_tolerance (void) const;
  double difference_interval (void) const;
  double function_precision (void) const;
  double infinite_bound (void) const;
  double infinite_step (void) const;
  double linear_feasibility_tolerance (void) const;
  double linesearch_tolerance (void) const;
  double nonlinear_feasibility_tolerance (void) const;
  double optimality_tolerance (void) const;

  int derivative_level (void) const;
  int major_iteration_limit (void) const;
  int minor_iteration_limit (void) const;
  int major_print_level (void) const;
  int minor_print_level (void) const;
  int start_objective_check (void) const;
  int start_constraint_check (void) const;
  int stop_objective_check (void) const;
  int stop_constraint_check (void) const;
  int verify_level (void) const;

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

class NPSOL : public NLP, public NPSOL_options
{
 public:

  NPSOL (void) : NLP () { }

  NPSOL (const Vector& x, const Objective& phi) : NLP (x, phi) { }

  NPSOL (const Vector& x, const Objective& phi,
	 const Bounds& b) : NLP (x, phi, b)
    { }

  NPSOL (const Vector& x, const Objective& phi, const Bounds& b,
	 const LinConst& lc) : NLP (x, phi, b, lc)
    { }

  NPSOL (const Vector& x, const Objective& phi, const Bounds& b,
	 const LinConst& lc, const NLConst& nlc) : NLP (x, phi, b, lc, nlc)
    { }

  NPSOL (const Vector& x, const Objective& phi,
	 const LinConst& lc) : NLP (x, phi, lc)
    { }

  NPSOL (const Vector& x, const Objective& phi, const LinConst& lc,
	 const NLConst& nlc) : NLP (x, phi, lc, nlc)
    { }

  NPSOL (const Vector& x, const Objective& phi,
	 const NLConst& nlc) : NLP (x, phi, nlc)
    { }

  NPSOL (const Vector& x, const Objective& phi, const Bounds& b,
	 const NLConst& nlc) : NLP (x, phi, b, nlc)
    { }

  NPSOL (const NPSOL& a);

  Vector minimize (void);
  Vector minimize (double& objf);
  Vector minimize (double& objf, int& inform);
  Vector minimize (double& objf, int& inform, Vector& lambda);

  Vector minimize (const Vector& x);
  Vector minimize (const Vector& x, double& objf);
  Vector minimize (const Vector& x, double& objf, int& inform);
  Vector minimize (const Vector& x, double& objf, int& inform, Vector& lambda);

  NPSOL& option (char *s);

 private:
};

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the objective
// function, and the user wants us to quit.
extern int npsol_objective_error;

inline NPSOL::NPSOL (const NPSOL& a) : NLP (a.x, a.phi, a.bnds, a.lc, a.nlc)
  { }

#endif /* NPSOL_MISSING */

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
