// Quad.h                                           -*- C++ -*-
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

#if !defined (octave_Quad_h)
#define octave_Quad_h 1

#include "dColVector.h"

extern "C++" {

#ifndef Vector
#define Vector ColumnVector
#endif

#if !defined (octave_Quad_typedefs)
#define octave_Quad_typedefs 1

typedef double (*integrand_fcn) (double x);

#endif

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the integrand
// function, and the user wants us to quit.
extern int quad_integration_error;

class Quad_options
{
 public:

  Quad_options (void);
  Quad_options (const Quad_options& opt);

  Quad_options& operator = (const Quad_options& opt);

  ~Quad_options (void);

  void init (void);
  void copy (const Quad_options& opt);

  void set_default_options (void);

  void set_absolute_tolerance (double);
  void set_relative_tolerance (double);

  double absolute_tolerance (void);
  double relative_tolerance (void);

 private:

  double x_absolute_tolerance;
  double x_relative_tolerance;
};

class Quad : public Quad_options
{
 public:

  Quad (integrand_fcn fcn);
  Quad (integrand_fcn fcn, double abs, double rel);

  virtual double integrate (void);
  virtual double integrate (int& ier);
  virtual double integrate (int& ier, int& neval);
  virtual double integrate (int& ier, int& neval, double& abserr) = 0;

 protected:

  integrand_fcn f;
};

class DefQuad : public Quad
{
 public:

  DefQuad (integrand_fcn fcn);
  DefQuad (integrand_fcn fcn, double ll, double ul);
  DefQuad (integrand_fcn fcn, double ll, double ul, double abs, double rel);
  DefQuad (integrand_fcn fcn, double ll, double ul, const Vector& sing);
  DefQuad (integrand_fcn fcn, const Vector& sing, double abs, double rel);
  DefQuad (integrand_fcn fcn, const Vector& sing);
  DefQuad (integrand_fcn fcn, double ll, double ul, const Vector& sing,
	   double abs, double rel);

  double integrate (int& ier, int& neval, double& abserr);

 private:

  double lower_limit;
  double upper_limit;

  Vector singularities;
};

class IndefQuad : public Quad
{
 public:

  enum IntegralType { bound_to_inf, neg_inf_to_bound, doubly_infinite };

  IndefQuad (integrand_fcn fcn);
  IndefQuad (integrand_fcn fcn, double b, IntegralType t);
  IndefQuad (integrand_fcn fcn, double b, IntegralType t, double abs,
	     double rel);
  IndefQuad (integrand_fcn fcn, double abs, double rel);

  double integrate (int& ier, int& neval, double& abserr);

 private:

  int integration_error;
  double bound;
  IntegralType type;
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
