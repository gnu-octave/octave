// QLD.h                                                -*- C++ -*-
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

#if !defined (octave_QLD_h)
#define octave_QLD_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class Matrix;
class ColumnVector;

#include "QP.h"

class QLD : public QP
{
 public:

  QLD (void) : QP ()
    { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H) : QP (x, H)
    { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c)
    : QP (x, H, c) { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const Bounds& b) : QP (x, H, b)
    { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const LinConst& lc)
    : QP (x, H, lc)
      { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
       const Bounds& b) : QP (x, H, c, b) { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
       const LinConst& lc) : QP (x, H, c, lc) { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const Bounds& b,
       const LinConst& lc) : QP (x, H, b, lc) { set_default_options (); }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
       const Bounds& b, const LinConst& lc) : QP (x, H, c, b, lc)
   { set_default_options (); }

  QLD (const QLD& a) : QP (a.x, a.H, a.c, a.bnds, a.lc)
    { set_default_options (); }

  QLD& operator = (const QLD& a)
    {
      x = a.x;
      H = a.H;
      c = a.c;
      bnds = a.bnds;
      lc = a.lc;
      iprint = a.iprint;

      return *this;
    }

  ColumnVector minimize (double& objf, int& inform);

private:
  void set_default_options (void);
  int iprint;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
