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

#if !defined (octave_Quad_h)
#define octave_Quad_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cfloat>
#include <cmath>

#include "dColVector.h"

#if !defined (octave_Quad_typedefs)
#define octave_Quad_typedefs 1

typedef double (*integrand_fcn) (double x);

#endif

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the integrand
// function, and the user wants us to quit.
extern int quad_integration_error;

class
Quad_options
{
 public:

  Quad_options (void) { init (); }

  // XXX FIXME XXX -- check for invalid values?
  Quad_options (double abs, double rel)
    : x_absolute_tolerance (abs), x_relative_tolerance (rel) { }

  Quad_options (const Quad_options& opt)
    : x_absolute_tolerance (opt.x_absolute_tolerance),
      x_relative_tolerance (opt.x_relative_tolerance) { }

  Quad_options& operator = (const Quad_options& opt)
    {
      if (this != &opt)
	set_options (opt);

      return *this;
    }

  ~Quad_options (void) { }

  void init (void)
    {
      double sqrt_eps = ::sqrt (DBL_EPSILON);

      x_absolute_tolerance = sqrt_eps;
      x_relative_tolerance = sqrt_eps;
    }

  void set_default_options (void) { init (); }

  void set_options (const Quad_options& opt)
    {
      x_absolute_tolerance = opt.x_absolute_tolerance;
      x_relative_tolerance = opt.x_relative_tolerance;
    }

  // XXX FIXME XXX -- check for invalid values?
  void set_absolute_tolerance (double val) { x_absolute_tolerance = val; }
  void set_relative_tolerance (double val) { x_relative_tolerance = val; }

  double absolute_tolerance (void) { return x_absolute_tolerance; }
  double relative_tolerance (void) { return x_relative_tolerance; }

 private:

  double x_absolute_tolerance;
  double x_relative_tolerance;
};

class
Quad : public Quad_options
{
 public:

  Quad (integrand_fcn fcn)
    : Quad_options (), f (fcn) { }

  Quad (integrand_fcn fcn, double abs, double rel)
    : Quad_options (abs, rel), f (fcn) { }

  ~Quad (void) { }

  virtual double integrate (void)
    {
      int ier, neval;
      double abserr;
      return integrate (ier, neval, abserr);
    }

  virtual double integrate (int& ier)
    {
      int neval;
      double abserr;
      return integrate (ier, neval, abserr);
    }

  virtual double integrate (int& ier, int& neval)
    {
      double abserr;
      return integrate (ier, neval, abserr);
    }

  virtual double integrate (int& ier, int& neval, double& abserr) = 0;

 protected:

  integrand_fcn f;
};

class
DefQuad : public Quad
{
 public:

  DefQuad (integrand_fcn fcn)
    : Quad (fcn), lower_limit (0.0), upper_limit (1.0), singularities () { }

  DefQuad (integrand_fcn fcn, double ll, double ul)
    : Quad (fcn), lower_limit (ll), upper_limit (ul), singularities () { }

  DefQuad (integrand_fcn fcn, double ll, double ul, double abs,
	   double rel)
    : Quad (fcn, abs, rel), lower_limit (ll), upper_limit (ul),
      singularities () { }

  DefQuad (integrand_fcn fcn, double ll, double ul,
	   const ColumnVector& sing)
    : Quad (fcn), lower_limit (ll), upper_limit (ul),
      singularities (sing) { }

  DefQuad (integrand_fcn fcn, const ColumnVector& sing, double abs,
	   double rel)
    : Quad (fcn, abs, rel), lower_limit (0.0), upper_limit (1.0),
      singularities (sing) { }

  DefQuad (integrand_fcn fcn, const ColumnVector& sing)
    : Quad (fcn), lower_limit (0.0), upper_limit (1.0),
      singularities (sing) { }

  DefQuad (integrand_fcn fcn, double ll, double ul, const ColumnVector& sing,
	   double abs, double rel)
    : Quad (fcn, abs, rel), lower_limit (ll), upper_limit (ul),
      singularities (sing) { }

  ~DefQuad (void) { }

  double integrate (int& ier, int& neval, double& abserr);

 private:

  double lower_limit;
  double upper_limit;

  ColumnVector singularities;
};

class
IndefQuad : public Quad
{
 public:

  enum IntegralType { bound_to_inf, neg_inf_to_bound, doubly_infinite };

  IndefQuad (integrand_fcn fcn)
    : Quad (fcn), bound (0.0), type (bound_to_inf) { }

  IndefQuad (integrand_fcn fcn, double b, IntegralType t)
    : Quad (fcn), bound (b), type (t) { }

  IndefQuad (integrand_fcn fcn, double b, IntegralType t, double abs,
	     double rel)
    : Quad (fcn, abs, rel), bound (b), type (t) { }

  IndefQuad (integrand_fcn fcn, double abs, double rel)
    : Quad (fcn, abs, rel), bound (0.0), type (bound_to_inf) { }

  ~IndefQuad (void) { }

  double integrate (int& ier, int& neval, double& abserr);

 private:

  double bound;
  IntegralType type;
  int integration_error;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
