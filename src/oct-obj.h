// oct-obj.h                                            -*- C -*-
/*

Copyright (C) 1994, 1995 John W. Eaton

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

#if !defined (octave_oct_obj_h)
#define octave_oct_obj_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Array.h"

// Including this is all we need because pt-const.h gives us
// declarations for all the data types Octave knows about.

#include "pt-const.h"

class
Octave_object : public Array<tree_constant>
{
public:

  Octave_object (void) : Array<tree_constant> () { }
  Octave_object (int n, const tree_constant& val)
    : Array<tree_constant> (n, val) { }

  Octave_object (const tree_constant& tc) : Array<tree_constant> (1, tc) { }

  Octave_object (double d);
  Octave_object (const Matrix& m);
  Octave_object (const DiagMatrix& d);
  Octave_object (const RowVector& v, int pcv = -1);
  Octave_object (const ColumnVector& v, int pcv = -1);

  Octave_object (const Complex& c);
  Octave_object (const ComplexMatrix& m);
  Octave_object (const ComplexDiagMatrix& d);
  Octave_object (const ComplexRowVector& v, int pcv = -1);
  Octave_object (const ComplexColumnVector& v, int pcv = -1);

  Octave_object (const char *s);
  Octave_object (const string& s);

  Octave_object (double base, double limit, double inc);
  Octave_object (const Range& r);

  Octave_object (const Octave_object& obj) : Array<tree_constant> (obj) { }

  Octave_object& operator = (const Octave_object& obj)
    {
      Array<tree_constant>::operator = (obj);
      return *this;
    }

// Assignment will resize on range errors.

  tree_constant& operator () (int n);

  tree_constant operator () (int n) const;

private:

// This constructor is private with no definition to keep statements
// like
//
//   Octave_object foo = 5;
//   Octave_object foo = 5.0;
//
// from doing different things.  Instead, you have to use the
// constructor
//
//   Octave_object (n, val);
//
// and supply a default value to create a vector-valued Octave_object.

  Octave_object (int n);

  void maybe_resize (int n);

  tree_constant& elem (int n);
  tree_constant& checkelem (int n);

  tree_constant& xelem (int n);

  tree_constant elem (int n) const;
  tree_constant checkelem (int n) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
