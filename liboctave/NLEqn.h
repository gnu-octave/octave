// NLEqn.h                                                -*- C++ -*-
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

#if !defined (octave_NLEqn_h)
#define octave_NLEqn_h 1

extern "C++" {

#include "Matrix.h"
#include "NLFunc.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class NLEqn_options
{
 public:

  NLEqn_options (void);
  NLEqn_options (const NLEqn_options& opt);

  NLEqn_options& operator = (const NLEqn_options& opt);

  ~NLEqn_options (void);

  void init (void);
  void copy (const NLEqn_options& opt);

  void set_default_options (void);

  void set_tolerance (double);

  double tolerance (void);

 private:

  double x_tolerance;
};

class NLEqn : public NLFunc, public NLEqn_options
{
 public:

  NLEqn (void);
  NLEqn (const Vector&, const NLFunc);

  NLEqn (const NLEqn &);

  NLEqn& operator = (const NLEqn& a);

  void resize (int);

  void set_states (const Vector&);

  Vector states (void) const;

  int size (void) const;

  Vector solve (void);
  Vector solve (const Vector&);

  Vector solve (int& info);
  Vector solve (const Vector&, int& info);

 private:

  int n;
  Vector x;

  void error (const char* msg);
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
