// NPSOL.cc                                                -*- C++ -*-
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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>
#include <cstring>

#include <strstream.h>

#ifndef NPSOL_MISSING

#include "NPSOL.h"
#include "dMatrix.h"
#include "f77-uscore.h"
#include "sun-utils.h"

extern "C"
{
  int F77_FCN (npoptn, NPOPTN) (const char*, long);

  int F77_FCN (npsol, NPSOL) (int&, int&, int&, int&, int&, int&,
			      double*, double*, double*,
			      int (*)(int&, const int&, const int&,
				      const int&, int*, double*,
				      double*, double*, int*),
			      int (*)(int&, const int&, double*,
				      double*, double*, int*),
			      int&, int&, int*, double*, double*,
			      double*, double&, double*, double*,
			      double*, int*, int&, double*, int&);
}

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the objective
// function, and the user wants us to quit.
int npsol_objective_error = 0;

static Objective::objective_fcn user_phi;
static Objective::gradient_fcn user_grad;
static NLFunc::nonlinear_fcn user_g;
static NLFunc::jacobian_fcn user_jac;

int
npsol_objfun (int& mode, const int& n, double *xx, double *objf,
	      double *objgrd, int *)
{
  ColumnVector tmp_x (n);

  npsol_objective_error = 0;

  for (int i = 0; i < n; i++)
    tmp_x.elem (i) = xx[i];

  if (mode == 0 || mode == 2)
    {
      double value = (*user_phi) (tmp_x);

      if (npsol_objective_error)
	{
	  mode = -1;
	  return 0;
	}

#if defined (sun) && defined (__GNUC__)
      assign_double (objf, value);
#else
      *objf = value;
#endif
    }

  if ((mode == 1 || mode == 2) && user_grad)
    {
      ColumnVector tmp_grad (n);

      tmp_grad = (*user_grad) (tmp_x);

      if (tmp_grad.length () == 0)
	mode = -1;
      else
	{
	  for (int i = 0; i < n; i++)
	    objgrd[i] = tmp_grad.elem (i);
	}
    }

  return 0;
}

int
npsol_confun (int& mode, const int& ncnln, const int& n,
	      const int& nrowj, int *, double *xx, double *cons,
	      double *cjac, int *)
{
  ColumnVector tmp_x (n);
  ColumnVector tmp_c (ncnln);

  for (int i = 0; i < n; i++)
    tmp_x.elem (i) = xx[i];

  tmp_c = (*user_g) (tmp_x);

  if (tmp_c.length () == 0)
    {
      mode = -1;
      return 0;
    }
  else
    {
      for (int i = 0; i < ncnln; i++)
	cons[i] = tmp_c.elem (i);
    }

  if (user_jac)
    {
      Matrix tmp_jac (ncnln, n);

      tmp_jac = (*user_jac) (tmp_x);

      if (tmp_jac.rows () == 0 || tmp_jac.columns () == 0)
	mode = -1;
      else
	{
	  int ld = nrowj;
	  for (int j = 0; j < n; j++)
	    for (int i = 0; i < ncnln; i++)
	      cjac[i+j*ld] = tmp_jac (i, j);
	}
    }

  return 0;
}

ColumnVector
NPSOL::do_minimize (double& objf, int& inform, ColumnVector& lambda)
{
  // Dimensions of various things.

  int n     = x.capacity ();
  int nclin = lc.size ();
  int ncnln = nlc.size ();
  int nrowa = 1 > nclin ? 1 : nclin;
  int nrowj = 1 > ncnln ? 1 : ncnln;
  int nrowr = n;

  // Informative stuff.

  int iter;
  int *istate = new int [n+nclin+ncnln];

  // User defined function stuff is defined above in the functions
  // npsol_confun() and npsol_objfun();

  // Constraint stuff.

  double *pclin = 0;
  Matrix clin;
  if (nclin > 0)
    {
      clin = lc.constraint_matrix ();
      pclin  = clin.fortran_vec ();
    }

  double *clow = new double [n+nclin+ncnln];
  double *cup = new double [n+nclin+ncnln];

  if (bnds.size () > 0)
    {
      for (int i = 0; i < n; i++)
	{
	  clow[i] = bnds.lower_bound (i);
	  cup[i] = bnds.upper_bound (i);
	}
    }
  else
    {
      double huge = 1.0e30;
      for (int i = 0; i < n; i++)
	{
	  clow[i] = -huge;
	  cup[i] = huge;
	}
    }

  for (int i = 0; i < nclin; i++)
    {
      clow[i+n] = lc.lower_bound (i);
      cup[i+n] = lc.upper_bound (i);
    }

  for (int i = 0; i < ncnln; i++)
    {
      clow[i+n+nclin] = nlc.lower_bound (i);
      cup[i+n+nclin] = nlc.upper_bound (i);
    }

  double *c = 0;
  double *cjac = 0;
  if (ncnln > 0)
    {
      c = new double [ncnln];
      cjac = new double [nrowj*n];
    }

  // Objective stuff.

  double *objgrd = new double [n];

  // Other stuff.

  double *r = new double [n*n];

  lambda.resize (n+nclin+ncnln);
  double *pclambda = lambda.fortran_vec ();

  // Decision variable stuff.

  double *px = x.fortran_vec ();

  // Workspace parameters.

  int lenw;
  int leniw = 3 * n + nclin + 2 * ncnln;
  if (nclin == 0 && ncnln == 0)
    lenw = 20*n;
  else if (ncnln == 0)
    lenw = 2*n*(10 + n) + 11*nclin;
  else
    lenw = 2*n*(n + 10) + nclin*(n + 11) + ncnln*(2*n + 21);

  int *iw = new int [leniw];
  double *w = new double [lenw];

  user_phi  = phi.objective_function ();
  user_grad = phi.gradient_function ();
  user_g    = nlc.function ();
  user_jac  = nlc.jacobian_function ();

  pass_options_to_npsol ();

  if (! user_jac && ! user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 0", 18L);
  else if (! user_jac && user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 1", 18L);
  else if (user_jac && ! user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 2", 18L);
  else if (user_jac && user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 3", 18L);

  int attempt = 0;
  while (attempt++ < 5)
    {

      F77_FCN (npsol, NPSOL) (n, nclin, ncnln, nrowa, nrowj, nrowr,
			      pclin, clow, cup, npsol_confun,
			      npsol_objfun, inform, iter, istate, c,
			      cjac, pclambda, objf, objgrd, r, px, iw,
			      leniw, w, lenw);

      if (inform == 6 || inform == 1)
	continue;
      else
	break;
    }

  // Clean up.

  delete [] istate;
  delete [] clow;
  delete [] cup;
  delete [] c;
  delete [] cjac;
  delete [] objgrd;
  delete [] r;
  delete [] iw;
  delete [] w;

  // See how it went.

  return x;
}

NPSOL&
NPSOL::option (char *)
{
  cerr << "This function no longer has any effect.\n"
       << "Use the NPSOL_option class instead\n";

  return *this;
}

NPSOL_options::NPSOL_options (void)
{
  init ();
}

NPSOL_options::NPSOL_options (const NPSOL_options& opt)
{
  copy (opt);
}

NPSOL_options&
NPSOL_options::operator = (const NPSOL_options& opt)
{
  if (this != &opt)
    copy (opt);

  return *this;
}

NPSOL_options::~NPSOL_options (void)
{
}

void
NPSOL_options::init (void)
{
  x_central_difference_interval = -1.0;
  x_crash_tolerance = 0.1;
  x_difference_interval = -1.0;
  x_function_precision = pow (DBL_EPSILON, 0.9);
  x_infinite_bound = 1.0e+30;
  x_infinite_step = 1.0e+30;
  x_linear_feasibility_tolerance = sqrt (DBL_EPSILON);
  x_linesearch_tolerance = 0.9;
  x_nonlinear_feasibility_tolerance = sqrt (DBL_EPSILON);
  x_optimality_tolerance = pow (DBL_EPSILON, 0.8);
  x_derivative_level = 0;
  x_major_iteration_limit = -1;
  x_minor_iteration_limit = -1;
  x_major_print_level = 0;
  x_minor_print_level = 0;
  x_start_objective_check = 1;
  x_start_constraint_check = 1;
  x_stop_objective_check = -1;
  x_stop_constraint_check = -1;
  x_verify_level = 0;
}

void
NPSOL_options::copy (const NPSOL_options& opt)
{
  x_central_difference_interval = opt.x_central_difference_interval;
  x_crash_tolerance = opt.x_crash_tolerance;
  x_difference_interval = opt.x_difference_interval;
  x_function_precision = opt.x_function_precision;
  x_infinite_bound = opt.x_infinite_bound;
  x_infinite_step = opt.x_infinite_step;
  x_linear_feasibility_tolerance = opt.x_linear_feasibility_tolerance;
  x_linesearch_tolerance = opt.x_linesearch_tolerance;
  x_nonlinear_feasibility_tolerance = opt.x_nonlinear_feasibility_tolerance;
  x_optimality_tolerance = opt.x_optimality_tolerance;
  x_derivative_level = opt.x_derivative_level;
  x_major_iteration_limit = opt.x_major_iteration_limit;
  x_minor_iteration_limit = opt.x_minor_iteration_limit;
  x_major_print_level = opt.x_major_print_level;
  x_minor_print_level = opt.x_minor_print_level;
  x_start_objective_check = opt.x_start_objective_check;
  x_start_constraint_check = opt.x_start_constraint_check;
  x_stop_objective_check = opt.x_stop_objective_check;
  x_stop_constraint_check = opt.x_stop_constraint_check;
  x_verify_level = opt.x_verify_level;
}

void
NPSOL_options::set_default_options (void)
{
  init ();
}

// Passing invalid values to the set_* functions will result in
// setting the default option.

void
NPSOL_options::set_central_difference_interval (double val)
{
  x_central_difference_interval = (val > 0.0) ? val : -1.0;
}

void
NPSOL_options::set_crash_tolerance (double val)
{
  x_crash_tolerance = (val >= 0.0) ? val : 0.1;
}

void
NPSOL_options::set_difference_interval (double val)
{
  x_difference_interval = (val > 0.0) ? val : -1.0;
}

void
NPSOL_options::set_function_precision (double val)
{
  x_function_precision = (val > 0.0) ? val : pow (DBL_EPSILON, 0.9);
}

void
NPSOL_options::set_infinite_bound (double val)
{
  x_infinite_bound = (val > 0.0) ? val : 1.0e+30;
}

void
NPSOL_options::set_infinite_step (double val)
{
  x_infinite_step = (val > 0.0) ? val : 1.0e+30;
}

void
NPSOL_options::set_linear_feasibility_tolerance (double val)
{
  x_linear_feasibility_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

void
NPSOL_options::set_linesearch_tolerance (double val)
{
  x_linesearch_tolerance = (val >= 0.0 && val < 1.0) ? val : 0.9;
}

void
NPSOL_options::set_nonlinear_feasibility_tolerance (double val)
{
  x_nonlinear_feasibility_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

void
NPSOL_options::set_optimality_tolerance (double val)
{
  x_optimality_tolerance = (val > 0.0) ? val : pow (DBL_EPSILON, 0.8);
}

void
NPSOL_options::set_derivative_level (int val)
{
  x_derivative_level = (val >= 0 && val < 4) ? val : 0;
}

void
NPSOL_options::set_major_iteration_limit (int val)
{
  x_major_iteration_limit = (val > 0) ? val : -1;
}

void
NPSOL_options::set_minor_iteration_limit (int val)
{
  x_minor_iteration_limit = (val > 0) ? val : -1;
}

void
NPSOL_options::set_major_print_level (int val)
{
  x_major_print_level = (val >= 0) ? val : -1;
}

void
NPSOL_options::set_minor_print_level (int val)
{
  x_minor_print_level = (val >= 0) ? val : -1;
}

void
NPSOL_options::set_start_objective_check (int val)
{
  x_start_objective_check = (val >= 0) ? val : -1;
}

void
NPSOL_options::set_start_constraint_check (int val)
{
  x_start_constraint_check = (val >= 0) ? val : -1;
}

void
NPSOL_options::set_stop_objective_check (int val)
{
  x_stop_objective_check = (val >= 0) ? val : -1;
}

void
NPSOL_options::set_stop_constraint_check (int val)
{
  x_stop_constraint_check = (val >= 0) ? val : -1;
}

void
NPSOL_options::set_verify_level (int val)
{
  x_verify_level = ((val > -1 && val < 4) || (val > 9 && val < 14)) ? val : 0;
}

double
NPSOL_options::central_difference_interval (void) const
{
  return x_central_difference_interval;
}

double
NPSOL_options::crash_tolerance (void) const
{
  return x_crash_tolerance;
}

double
NPSOL_options::difference_interval (void) const
{
  return x_difference_interval;
}

double
NPSOL_options::function_precision (void) const
{
  return x_function_precision;
}

double
NPSOL_options::infinite_bound (void) const
{
  return x_infinite_bound;
}

double
NPSOL_options::infinite_step (void) const
{
  return x_infinite_step;
}

double
NPSOL_options::linear_feasibility_tolerance (void) const
{
  return x_linear_feasibility_tolerance;
}

double
NPSOL_options::linesearch_tolerance (void) const
{
  return x_linesearch_tolerance;
}

double
NPSOL_options::nonlinear_feasibility_tolerance (void) const
{
  return x_nonlinear_feasibility_tolerance;
}

double
NPSOL_options::optimality_tolerance (void) const
{
  return x_optimality_tolerance;
}

int
NPSOL_options::derivative_level (void) const
{
  return x_derivative_level;
}

int
NPSOL_options::major_iteration_limit (void) const
{
  return x_major_iteration_limit;
}

int
NPSOL_options::minor_iteration_limit (void) const
{
  return x_minor_iteration_limit;
}

int
NPSOL_options::major_print_level (void) const
{
  return x_major_print_level;
}

int
NPSOL_options::minor_print_level (void) const
{
  return x_minor_print_level;
}

int
NPSOL_options::start_objective_check (void) const
{
  return x_start_objective_check;
}

int
NPSOL_options::start_constraint_check (void) const
{
  return x_start_constraint_check;
}

int
NPSOL_options::stop_objective_check (void) const
{
  return x_stop_objective_check;
}

int
NPSOL_options::stop_constraint_check (void) const
{
  return x_stop_constraint_check;
}

int
NPSOL_options::verify_level (void) const
{
  return x_verify_level;
}

void
NPSOL_options::pass_options_to_npsol (void)
{
  F77_FCN (npoptn, NPOPTN) ("Nolist", 6L);
  F77_FCN (npoptn, NPOPTN) ("Defaults", 8L);

  if (x_central_difference_interval > 0.0)
    set_option ("Central Difference", x_central_difference_interval);

  set_option ("Crash Tolerance", x_crash_tolerance);

  if (x_difference_interval > 0.0)
    set_option ("Difference Interval", x_difference_interval);

  set_option ("Function Precision", x_function_precision);

  set_option ("Infinite Bound", x_infinite_bound);

  set_option ("Infinite Step", x_infinite_step);

  set_option ("Linear Feasibility", x_linear_feasibility_tolerance);

  set_option ("Linesearch Tolerance", x_linesearch_tolerance);

  set_option ("Nonlinear Feasibility", x_nonlinear_feasibility_tolerance);

  set_option ("Optimality Tolerance", x_optimality_tolerance);

  set_option ("Derivative Level", x_derivative_level);

  if (x_major_iteration_limit > 0)
    set_option ("Major Iteration", x_major_iteration_limit);

  if (x_minor_iteration_limit > 0)
    set_option ("Minor Iteration", x_minor_iteration_limit);

  set_option ("Major Print", x_major_print_level);

  set_option ("Minor Print", x_minor_print_level);

  set_option ("Start Objective", x_start_objective_check);

  set_option ("Start Constraint", x_start_constraint_check);

  if (x_stop_objective_check > 0)
    set_option ("Stop Objective", x_stop_objective_check);

  if (x_stop_constraint_check > 0)
    set_option ("Stop Constraint", x_stop_constraint_check);

  set_option ("Verify Level", x_verify_level);
}

void
NPSOL_options::set_option (const char *key, int opt)
{
  ostrstream buf;
  buf << key << " " << opt << ends;
  char *command = buf.str ();
  size_t len = strlen (command);
  F77_FCN (npoptn, NPOPTN) (command, (long) len);
  delete [] command;
}

void
NPSOL_options::set_option (const char *key, double opt)
{
  ostrstream buf;
  buf << key << " " << opt << ends;
  char *command = buf.str ();
  size_t len = strlen (command);
  F77_FCN (npoptn, NPOPTN) (command, (long) len);
  delete [] command;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
