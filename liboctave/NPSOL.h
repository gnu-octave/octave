// NPSOL.h                                                -*- C++ -*-
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

#if !defined (_NPSOL_h)
#define _NPSOL_h 1

#ifndef NPSOL_MISSING

#include "Matrix.h"
#include "NLP.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class NPSOL : public NLP
{
 public:

  NPSOL (void) : NLP ()
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi) : NLP (x, phi)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi,
	 const Bounds& b) : NLP (x, phi, b)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi, const Bounds& b,
	 const LinConst& lc) : NLP (x, phi, b, lc)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi, const Bounds& b,
	 const LinConst& lc, const NLConst& nlc) : NLP (x, phi, b, lc, nlc)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi,
	 const LinConst& lc) : NLP (x, phi, lc)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi, const LinConst& lc,
	 const NLConst& nlc) : NLP (x, phi, lc, nlc)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi,
	 const NLConst& nlc) : NLP (x, phi, nlc)
    { set_default_options (); }

  NPSOL (const Vector& x, const Objective& phi, const Bounds& b,
	 const NLConst& nlc) : NLP (x, phi, b, nlc)
    { set_default_options (); }

  NPSOL (const NPSOL& a);

  NPSOL& operator = (const NPSOL& a);

  Vector minimize (void);
  Vector minimize (double& objf);
  Vector minimize (double& objf, int& inform);
  Vector minimize (double& objf, int& inform, Vector& lambda);

  Vector minimize (const Vector& x);
  Vector minimize (const Vector& x, double& objf);
  Vector minimize (const Vector& x, double& objf, int& inform);
  Vector minimize (const Vector& x, double& objf, int& inform, Vector& lambda);

  NPSOL& option (char *s);

private:
  void set_default_options (void);

};

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the objective
// function, and the user wants us to quit.
extern int npsol_objective_error;

inline NPSOL::NPSOL (const NPSOL& a) : NLP (a.x, a.phi, a.bnds, a.lc, a.nlc)
  { set_default_options (); }

inline NPSOL&
NPSOL::operator = (const NPSOL& a)
{
  x = a.x;
  phi = a.phi;
  bnds = a.bnds;
  lc = a.lc;
  nlc = a.nlc;

  cerr << "warning: NPSOL options reset to default values\n";

  set_default_options ();

  return *this;
}

#endif /* NPSOL_MISSING */

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
