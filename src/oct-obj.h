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
#include "oct-alloc.h"
#include "str-vec.h"

#include "ov.h"

class
octave_value_list
{
public:

  octave_value_list (void)
    : data () { }

  octave_value_list (int n, const octave_value& val)
    : data (n, val) { }

  octave_value_list (const octave_value& tc)
    : data (1, tc) { }

  octave_value_list (double d)
    : data (1, octave_value (d)) { }

  octave_value_list (const Matrix& m)
    : data (1, octave_value (m)) { }

  octave_value_list (const DiagMatrix& d)
    : data (1, octave_value (d)) { }

  octave_value_list (const RowVector& v, int pcv)
    : data (1, octave_value (v, pcv)) { }

  octave_value_list (const ColumnVector& v, int pcv)
    : data (1, octave_value (v, pcv)) { }

  octave_value_list (const Complex& c)
    : data (1, octave_value (c)) { }

  octave_value_list (const ComplexMatrix& m)
    : data (1, octave_value (m)) { }

  octave_value_list (const ComplexDiagMatrix& d)
    : data (1, octave_value (d)) { }

  octave_value_list (const ComplexRowVector& v, int pcv)
    : data (1, octave_value (v, pcv)) { }

  octave_value_list (const ComplexColumnVector& v, int pcv)
    : data (1, octave_value (v, pcv)) { }

  octave_value_list (const char *s)
    : data (1, octave_value (s)) { }

  octave_value_list (const string& s)
    : data (1, octave_value (s)) { }

  octave_value_list (const string_vector& s)
    : data (1, octave_value (s)) { }

  octave_value_list (double base, double limit, double inc)
    : data (1, octave_value (base, limit, inc)) { }

  octave_value_list (const Range& r)
    : data (1, octave_value (r)) { }

  octave_value_list (const octave_value_list& obj)
    : data (obj.data) { }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  octave_value_list& operator = (const octave_value_list& obj)
    {
      if (this != &obj)
	data = obj.data;

      return *this;
    }

  // Assignment will resize on range errors.

  octave_value& operator () (int n) { return elem (n); }

  octave_value operator () (int n) const { return elem (n); }

  int length (void) const { return data.length (); }

  bool empty (void) const { return length () == 0; }

  void resize (int n) { data.resize (n); }

  void resize (int n, const octave_value& val) { data.resize (n, val); }

  octave_value_list& prepend (const octave_value& val);

  octave_value_list& append (const octave_value& val);

  octave_value_list& append (const octave_value_list& lst);

  octave_value_list& reverse (void);

  bool all_strings_p (void) const;

  string_vector make_argv (const string&) const;

  void stash_name_tags (const string_vector& nm) { names = nm; }

  string_vector name_tags (void) const { return names; }

private:

  static octave_allocator allocator;

  Array<octave_value> data;

  // This list of strings can be used to tag each element of data with
  // a name.  By default, it is empty.
  string_vector names;

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
  // and supply a default value to create a vector-valued
  // octave_value_list.

  octave_value_list (int n);

  void maybe_resize (int n)
    {
      if (n >= length ())
	data.resize (n + 1, Matrix ());
    }

  octave_value& elem (int n)
    {
      maybe_resize (n);
      return data.elem (n);
    }

  octave_value elem (int n) const
    {
      return data.elem (n);
    }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
