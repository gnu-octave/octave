// DAE.h                                                -*- C++ -*-
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

#if !defined (_DAE_h)
#define _DAE_h 1

#include "ODE.h"
#include "DAEFunc.h"
#include "Matrix.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class DAE : public ODE, public DAEFunc
{
public:

  DAE (void);

  DAE (int);

  DAE (Vector& x, double time, DAEFunc& f);

  DAE (Vector& x, Vector& xdot, double time, DAEFunc& f);

 ~DAE (void);

  Vector deriv (void);

  virtual void initialize (Vector& x, double t);
  virtual void initialize (Vector& x, Vector& xdot, double t);

  Vector integrate (double t);

  Matrix integrate (const Vector& tout, Matrix& xdot_out);
  Matrix integrate (const Vector& tout, Matrix& xdot_out,
		    const Vector& tcrit); 

protected:

/*
 * Some of this is probably too closely related to DASSL, but hey,
 * this is just a first attempt...
 */

  Vector xdot;

private:

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
