// ODE.h                                                -*- C++ -*-
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

#if !defined (_ODE_h)
#define _ODE_h 1

class ostream;

#include "Matrix.h"
#include "ODEFunc.h"

class ODE_options
{
 public:

  ODE_options (void);
  ODE_options (const ODE_options& opt);

  ODE_options& operator = (const ODE_options& opt);

  ~ODE_options (void);

  void init (void);
  void copy (const ODE_options& opt);

  void set_default_options (void);

  void set_absolute_tolerance (double);
  void set_initial_step_size (double);
  void set_maximum_step_size (double);
  void set_minimum_step_size (double);
  void set_relative_tolerance (double);

  double absolute_tolerance (void);
  double initial_step_size (void);
  double maximum_step_size (void);
  double minimum_step_size (void);
  double relative_tolerance (void);

 private:

  double x_absolute_tolerance;
  double x_initial_step_size;
  double x_maximum_step_size;
  double x_minimum_step_size;
  double x_relative_tolerance;
};

class ODE : public ODEFunc, public ODE_options
{
public:

  ODE (void);

  ODE (int n);
  
  ODE (const ColumnVector& state, double time, const ODEFunc& f);

  virtual ~ODE (void);

  virtual int size (void) const;
  virtual ColumnVector state (void) const;
  virtual double time (void) const;

  virtual void force_restart (void);
  virtual void initialize (const ColumnVector& x, double t);
  virtual void set_stop_time (double t);
  virtual void clear_stop_time (void);

  virtual ColumnVector integrate (double t);

  void integrate (int nsteps, double tstep, ostream& s);

  Matrix integrate (const ColumnVector& tout);
  Matrix integrate (const ColumnVector& tout, const ColumnVector& tcrit);

protected:

/*
 * Some of this is probably too closely related to LSODE, but hey,
 * this is just a first attempt...
 */

  int n;
  double t;
  ColumnVector x;

  double stop_time;
  int stop_time_set;

private:

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
