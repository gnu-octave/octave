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

#include <cfloat>
#include <cmath>

#include "ODESSA-opts.h"

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
  int maxord;
  Array<int> iwork;
  Array<double> rwork;
  int itask;
  Array<int> iopt;
  int isopt;

  Array<int> neq;

  int n;
  int npar;

  // XXX FIXME XXX -- ???
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
