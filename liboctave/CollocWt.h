// CollocWt.h                                                -*- C++ -*-
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

#if !defined (octave_CollocWt_h)
#define octave_CollocWt_h 1

#include "Matrix.h"

extern "C++" {

class ostream;

#ifndef Vector
#define Vector ColumnVector
#endif

class CollocWt
{
public:

  CollocWt (void);
  CollocWt (int ncol, int include_left, int include_right);
  CollocWt (int ncol, int include_left, int include_right, double left,
	    double right);
  CollocWt (int ncol, double alpha, double beta, int include_left,
	    int include_right);  
  CollocWt (int ncol, double alpha, double beta, int include_left,
	    int include_right, double left, double right);

  CollocWt (const CollocWt&);

  CollocWt& operator = (const CollocWt&);

  CollocWt& resize (int ncol);

  CollocWt& add_left (void);
  CollocWt& delete_left (void);
  CollocWt& set_left (double val);

  CollocWt& add_right (void);
  CollocWt& delete_right (void);
  CollocWt& set_right (double val);

  CollocWt& set_alpha (double val);
  CollocWt& set_beta (double val);

  int ncol (void) const;
  int left_included (void) const;
  int right_included (void) const;

  double left (void) const;
  double right (void) const;
  double width (void) const;

  double alpha (void) const;
  double beta (void) const;

  Vector roots (void);
  Vector quad (void);
  Vector quad_weights (void);

  Matrix first (void);
  Matrix second (void);

  friend ostream& operator << (ostream&, const CollocWt&);

protected:

  int n;
  int nt;

  int inc_left;
  int inc_right;

  double lb;
  double rb;

  double Alpha;
  double Beta;

  Vector r;
  Vector q;

  Matrix A;
  Matrix B;

  int initialized;

  void init (void);

  void error (const char *msg);
};

inline int
CollocWt::ncol (void) const 
{
  return n;
}

inline int CollocWt::left_included (void) const { return inc_left; }
inline int CollocWt::right_included (void) const { return inc_right; }
inline double CollocWt::left (void) const { return lb; }
inline double CollocWt::right (void) const { return rb; }
inline double CollocWt::width (void) const { return rb - lb; }
inline double CollocWt::alpha (void) const { return Alpha; }
inline double CollocWt::beta (void) const { return Beta; }

inline Vector CollocWt::roots (void)
  { if (!initialized) init (); return r; }

inline Vector CollocWt::quad (void)
  { if (!initialized) init (); return q; }

inline Vector CollocWt::quad_weights (void)
  { return quad (); }

inline Matrix CollocWt::first (void)
  { if (!initialized) init (); return A; }

inline Matrix CollocWt::second (void)
  { if (!initialized) init (); return B; }

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
