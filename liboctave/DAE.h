// DAE.h                                                -*- C++ -*-
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

#if !defined (octave_DAE_h)
#define octave_DAE_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "dColVector.h"
#include "ODE.h"
#include "DAEFunc.h"

class DAE : public ODE, public DAEFunc
{
public:

  DAE (void);

  DAE (int);

  DAE (const ColumnVector& x, double time, DAEFunc& f);

  DAE (const ColumnVector& x, const ColumnVector& xdot,
       double time, DAEFunc& f);

 ~DAE (void);

  ColumnVector deriv (void);

  virtual void initialize (const ColumnVector& x, double t);
  virtual void initialize (const ColumnVector& x,
			   const ColumnVector& xdot, double t);

  ColumnVector integrate (double t);

  Matrix integrate (const ColumnVector& tout, Matrix& xdot_out);
  Matrix integrate (const ColumnVector& tout, Matrix& xdot_out,
		    const ColumnVector& tcrit); 

protected:

  // Some of this is probably too closely related to DASSL, but hey,
  // this is just a first attempt...

  ColumnVector xdot;

private:

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
