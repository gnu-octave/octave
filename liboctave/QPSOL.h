// QPSOL.h                                                -*- C++ -*-
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

#if !defined (_QPSOL_h)
#define _QPSOL_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "QP.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class QPSOL : public QP
{
 public:

  QPSOL (void) : QP ()
    { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H) : QP (x, H)
    { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c) : QP (x, H, c)
    { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const Bounds& b) : QP (x, H, b)
    { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const LinConst& lc) : QP (x, H, lc)
    { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c, const Bounds& b)
    : QP (x, H, c, b) { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c, const LinConst& lc)
    : QP (x, H, c, lc) { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const Bounds& b, const LinConst& lc)
    : QP (x, H, b, lc) { set_default_options (); }

  QPSOL (const Vector& x, const Matrix& H, const Vector& c, const Bounds& b,
      const LinConst& lc)
    : QP (x, H, c, b, lc) { set_default_options (); }

  QPSOL (const QPSOL& a);

  QPSOL& operator = (const QPSOL& a);

  Vector minimize (double& objf, int& inform, Vector& lambda);

private:
  void set_default_options (void);
  int iprint;
};

inline QPSOL::QPSOL (const QPSOL& a) : QP (a.x, a.H, a.c, a.bnds, a.lc)
  { set_default_options (); }

inline QPSOL&
QPSOL::operator = (const QPSOL& a)
{
  x = a.x;
  H = a.H;
  c = a.c;
  bnds = a.bnds;
  lc = a.lc;
  iprint = a.iprint;
  return *this;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
