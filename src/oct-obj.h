/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include "ov.h"

class
octave_value_list : public Array<octave_value>
{
public:

  octave_value_list (void)
    : Array<octave_value> () { }

  octave_value_list (int n, const octave_value& val)
    : Array<octave_value> (n, val) { }

  octave_value_list (const octave_value& tc)
    : Array<octave_value> (1, tc) { }

  octave_value_list (double d)
    : Array<octave_value> (1, octave_value (d)) { }

  octave_value_list (const Matrix& m)
    : Array<octave_value> (1, octave_value (m)) { }

  octave_value_list (const DiagMatrix& d)
    : Array<octave_value> (1, octave_value (d)) { }

  octave_value_list (const RowVector& v, int pcv)
    : Array<octave_value> (1, octave_value (v, pcv)) { }

  octave_value_list (const ColumnVector& v, int pcv)
    : Array<octave_value> (1, octave_value (v, pcv)) { }

  octave_value_list (const Complex& c)
    : Array<octave_value> (1, octave_value (c)) { }

  octave_value_list (const ComplexMatrix& m)
    : Array<octave_value> (1, octave_value (m)) { }

  octave_value_list (const ComplexDiagMatrix& d)
    : Array<octave_value> (1, octave_value (d)) { }

  octave_value_list (const ComplexRowVector& v, int pcv)
    : Array<octave_value> (1, octave_value (v, pcv)) { }

  octave_value_list (const ComplexColumnVector& v, int pcv)
    : Array<octave_value> (1, octave_value (v, pcv)) { }

  octave_value_list (const char *s)
    : Array<octave_value> (1, octave_value (s)) { }

  octave_value_list (const string& s)
    : Array<octave_value> (1, octave_value (s)) { }

  octave_value_list (const string_vector& s)
    : Array<octave_value> (1, octave_value (s)) { }

  octave_value_list (double base, double limit, double inc)
    : Array<octave_value> (1, octave_value (base, limit, inc)) { }

  octave_value_list (const Range& r)
    : Array<octave_value> (1, octave_value (r)) { }

  octave_value_list (const octave_value_list& obj)
    : Array<octave_value> (obj) { }

  octave_value_list& operator = (const octave_value_list& obj)
    {
      if (this != &obj)
	Array<octave_value>::operator = (obj);

      return *this;
    }

// Assignment will resize on range errors.

  octave_value& operator () (int n) { return elem (n); }

  octave_value operator () (int n) const { return elem (n); }

  int all_strings (void) const;

  string_vector make_argv (const string&) const;

private:

// This constructor is private with no definition to keep statements
// like
//
//   octave_value_list foo = 5;
//   octave_value_list foo = 5.0;
//
// from doing different things.  Instead, you have to use the
// constructor
//
//   octave_value_list (n, val);
//
// and supply a default value to create a vector-valued octave_value_list.

  octave_value_list (int n);

  void maybe_resize (int n)
    {
      if (n >= length ())
	resize (n + 1, Matrix ());
    }

  octave_value& elem (int n)
    {
      maybe_resize (n);
      return Array<octave_value>::elem (n);
    }

  octave_value& checkelem (int n);

  octave_value& xelem (int n);

  octave_value elem (int n) const
    {
      return Array<octave_value>::operator () (n);
    }

  octave_value checkelem (int n) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
