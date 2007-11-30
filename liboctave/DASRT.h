/*

Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_DASRT_h)
#define octave_DASRT_h 1

#include <cfloat>

#include "DASRT-opts.h"
#include "lo-math.h"

class
DASRT_result
{
public:

  DASRT_result (void) { }

  DASRT_result (const Matrix& xx, const Matrix& xxdot, const ColumnVector& tt)
    : x (xx), xdot (xxdot), t (tt) { }

  DASRT_result (const DASRT_result& r)
    : x (r.x), xdot (r.xdot), t (r.t) { }

  DASRT_result& operator = (const DASRT_result& r)
    {
      if (this != &r)
	{
	  x = r.x;
	  xdot = r.xdot;
          t = r.t;
	}
      return *this;
    }

  ~DASRT_result (void) { }

  Matrix state (void) const { return x; }
  Matrix deriv (void) const { return xdot; }
  ColumnVector times (void) const { return t; }

private:

  Matrix x;
  Matrix xdot;
  ColumnVector t;
};

class
OCTAVE_API
DASRT : public DAERT, public DASRT_options
{
public:

  DASRT (void) : DAERT (), DASRT_options (), initialized (false) { }

  DASRT (const ColumnVector& s, double tm, DAERTFunc& f)
    : DAERT (s, tm, f), DASRT_options (), initialized (false) { }

  DASRT (const ColumnVector& s, const ColumnVector& deriv,
	 double tm, DAERTFunc& f)
    : DAERT (s, deriv, tm, f), DASRT_options (), initialized (false) { }

  ~DASRT (void) { }

  DASRT_result integrate (const ColumnVector& tout);

  DASRT_result integrate (const ColumnVector& tout,
			  const ColumnVector& tcrit); 

  std::string error_message (void) const;

private:

  bool initialized;

  octave_idx_type liw;  
  octave_idx_type lrw;

  octave_idx_type ng;

  Array<octave_idx_type> info;
  Array<octave_idx_type> iwork;
  Array<octave_idx_type> jroot;

  Array<double> rwork;

  Array<double> abs_tol;
  Array<double> rel_tol;

  double *px;
  double *pxdot;
  double *pabs_tol;
  double *prel_tol;
  octave_idx_type *pinfo;
  octave_idx_type *piwork;
  double *prwork;
  octave_idx_type *pjroot;

  void integrate (double t);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
