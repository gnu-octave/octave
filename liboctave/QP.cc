// QP.cc                                                -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "QP.h"

QP::QP (void) {}

QP::QP (const Vector& x0, const Matrix& H_arg) : x (x0), H (H_arg)
{
  make_h_symmetric ();
}

QP::QP (const Vector& x0, const Matrix& H_arg, const Vector& c_arg)
  : x (x0), H (H_arg), c (c_arg)
{
  make_h_symmetric ();
}


QP::QP (const Vector& x0, const Matrix& H_arg, const Bounds& b)
  : x (x0), H (H_arg), bnds (b)
{
  make_h_symmetric ();
}


QP::QP (const Vector& x0, const Matrix& H_arg, const LinConst& l)
  : x (x0), H (H_arg), lc (l)
{
  make_h_symmetric ();
}


QP::QP (const Vector& x0, const Matrix& H_arg, const Vector& c_arg,
	       const Bounds& b)
  : x (x0), H (H_arg), c (c_arg), bnds (b)
{
  make_h_symmetric ();
}


QP::QP (const Vector& x0, const Matrix& H_arg, const Vector& c_arg,
	       const LinConst& l)
  : x (x0), H (H_arg), c (c_arg), lc (l)
{
  make_h_symmetric ();
}


QP::QP (const Vector& x0, const Matrix& H_arg, const Bounds& b,
	       const LinConst& l)
  : x (x0), H (H_arg), bnds (b), lc (l)
{
  make_h_symmetric ();
}


QP::QP (const Vector& x0, const Matrix& H_arg, const Vector& c_arg,
	       const Bounds& b, const LinConst& l)
  : x (x0), H (H_arg), c (c_arg), bnds (b), lc (l)
{
  make_h_symmetric ();
}

Matrix
QP::make_h_symmetric (void)
{
  return 0.5 * (H + H.transpose ());
}

Vector
QP::minimize (void)
{
  double objf;
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
QP::minimize (double& objf)
{
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
QP::minimize (double& objf, int& inform)
{
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
QP::minimize (const Vector& x0)
{
  x = x0;
  double objf;
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
QP::minimize (const Vector& x0, double& objf)
{
  x = x0;
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
QP::minimize (const Vector& x0, double& objf, int& inform)
{
  x = x0;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
QP::minimize (const Vector& x0, double& objf, int& inform, Vector& lambda)
{
  x = x0;
  return minimize (objf, inform, lambda);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
