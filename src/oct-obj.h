// oct-obj.h                                            -*- C -*-
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

#if !defined (octave_oct_obj_h)
#define octave_oct_obj_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "Array.h"
#include "str-vec.h"

// Including this is all we need because pt-const.h gives us
// declarations for all the data types Octave knows about.

#include "pt-const.h"

class
Octave_object : public Array<tree_constant>
{
public:

  Octave_object (void)
    : Array<tree_constant> () { }

  Octave_object (int n, const tree_constant& val)
    : Array<tree_constant> (n, val) { }

  Octave_object (const tree_constant& tc)
    : Array<tree_constant> (1, tc) { }

  Octave_object (double d)
    : Array<tree_constant> (1, tree_constant (d)) { }

  Octave_object (const Matrix& m)
    : Array<tree_constant> (1, tree_constant (m)) { }

  Octave_object (const DiagMatrix& d)
    : Array<tree_constant> (1, tree_constant (d)) { }

  Octave_object (const RowVector& v, int pcv)
    : Array<tree_constant> (1, tree_constant (v, pcv)) { }

  Octave_object (const ColumnVector& v, int pcv)
    : Array<tree_constant> (1, tree_constant (v, pcv)) { }

  Octave_object (const Complex& c)
    : Array<tree_constant> (1, tree_constant (c)) { }

  Octave_object (const ComplexMatrix& m)
    : Array<tree_constant> (1, tree_constant (m)) { }

  Octave_object (const ComplexDiagMatrix& d)
    : Array<tree_constant> (1, tree_constant (d)) { }

  Octave_object (const ComplexRowVector& v, int pcv)
    : Array<tree_constant> (1, tree_constant (v, pcv)) { }

  Octave_object (const ComplexColumnVector& v, int pcv)
    : Array<tree_constant> (1, tree_constant (v, pcv)) { }

  Octave_object (const char *s)
    : Array<tree_constant> (1, tree_constant (s)) { }

  Octave_object (const string& s)
    : Array<tree_constant> (1, tree_constant (s)) { }

  Octave_object (const string_vector& s)
    : Array<tree_constant> (1, tree_constant (s)) { }

  Octave_object (double base, double limit, double inc)
    : Array<tree_constant> (1, tree_constant (base, limit, inc)) { }

  Octave_object (const Range& r)
    : Array<tree_constant> (1, tree_constant (r)) { }

  Octave_object (const Octave_object& obj)
    : Array<tree_constant> (obj) { }

  Octave_object& operator = (const Octave_object& obj)
    {
      if (this != &obj)
	Array<tree_constant>::operator = (obj);

      return *this;
    }

// Assignment will resize on range errors.

  tree_constant& operator () (int n) { return elem (n); }

  tree_constant operator () (int n) const { return elem (n); }

  int all_strings (void) const;

  string_vector make_argv (const string&) const;

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

  void maybe_resize (int n)
    {
      if (n >= length ())
	resize (n + 1, Matrix ());
    }

  tree_constant& elem (int n)
    {
      maybe_resize (n);
      return Array<tree_constant>::elem (n);
    }

  tree_constant& checkelem (int n);

  tree_constant& xelem (int n);

  tree_constant elem (int n) const
    {
      return Array<tree_constant>::operator () (n);
    }

  tree_constant checkelem (int n) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
