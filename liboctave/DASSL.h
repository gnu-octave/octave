// DASSL.h                                                -*- C++ -*-
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

#if !defined (octave_DASSL_h)
#define octave_DASSL_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "DAE.h"

class DASSL_options
{
 public:

  DASSL_options (void);
  DASSL_options (const DASSL_options& opt);

  DASSL_options& operator = (const DASSL_options& opt);

  ~DASSL_options (void);

  void init (void);
  void copy (const DASSL_options& opt);

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

class DASSL : public DAE, public DASSL_options
{
public:

  DASSL (void);

  DASSL (const ColumnVector& x, double time, DAEFunc& f);

  DASSL (const ColumnVector& x, const ColumnVector& xdot,
	 double time, DAEFunc& f);

 ~DASSL (void);

  void force_restart (void);

  void set_stop_time (double t);
  void clear_stop_time (void);

  ColumnVector do_integrate (double t);

  Matrix do_integrate (const ColumnVector& tout);

  Matrix integrate (const ColumnVector& tout, Matrix& xdot_out);

  Matrix integrate (const ColumnVector& tout, Matrix& xdot_out,
		    const ColumnVector& tcrit); 

private:

  double stop_time;
  int stop_time_set;

  int n;
  int integration_error;
  int restart;
  int liw;  
  int lrw;
  int idid;
  int *info;
  int *iwork;
  double *rwork;

  friend int ddassl_j (double *time, double *state, double *deriv,
		       double *pd, double *cj, double *rpar, int *ipar);

  friend int ddassl_f (double *time, double *state, double *deriv,
		       double *delta, int *ires, double *rpar, int *ipar);

};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
