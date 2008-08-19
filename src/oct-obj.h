/*

Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2002, 2003, 2004,
              2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_oct_obj_h)
#define octave_oct_obj_h 1

#include <string>
#include <vector>

#include "oct-alloc.h"
#include "str-vec.h"

#include "ov.h"

class
OCTINTERP_API
octave_value_list
{
public:

  octave_value_list (void)
    : data () { }

  octave_value_list (octave_idx_type n, const octave_value& val)
    : data (n, val) { }

  octave_value_list (const octave_value& tc)
    : data (1, tc) { }

  octave_value_list (const octave_value_list& obj)
    : data (obj.data), names (obj.names) { }

  ~octave_value_list (void) { }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  // FIXME -- without this, I have errors with the stack of
  // octave_value_list objects in ov-usr-fcn.h.  Why?
  void *operator new (size_t size, void *p)
    { return ::operator new (size, p); }

  void operator delete (void *p, void *)
    {
#if defined (HAVE_PLACEMENT_DELETE)
      ::operator delete (p, static_cast<void *> (0));
#else
      ::operator delete (p);
#endif
    }

  octave_value_list& operator = (const octave_value_list& obj)
    {
      if (this != &obj)
	{
	  data = obj.data;
	  names = obj.names;
	}

      return *this;
    }

  bool valid_scalar_indices (void) const;

  // Assignment will resize on range errors.

  octave_value& operator () (octave_idx_type n) { return elem (n); }

  octave_value operator () (octave_idx_type n) const { return elem (n); }

  octave_idx_type length (void) const { return data.size (); }

  bool empty (void) const { return length () == 0; }

  void resize (octave_idx_type n) { data.resize (n); }

  void resize (octave_idx_type n, const octave_value& val);

  octave_value_list& prepend (const octave_value& val);

  octave_value_list& append (const octave_value& val);

  octave_value_list& append (const octave_value_list& lst);

  octave_value_list& reverse (void);

  octave_value_list
  splice (octave_idx_type offset, octave_idx_type len,
	  const octave_value_list& lst = octave_value_list ()) const;

  bool all_strings_p (void) const;

  bool has_magic_colon (void) const;

  string_vector make_argv (const std::string& = std::string()) const;

  void stash_name_tags (const string_vector& nm) { names = nm; }

  string_vector name_tags (void) const { return names; }

private:

  static octave_allocator allocator;

  std::vector<octave_value> data;

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

  octave_value_list (octave_idx_type n);

  octave_value_list (const Array<octave_value>& d);

  octave_value& elem (octave_idx_type n)
    {
      static Matrix empty_matrix;

      if (n >= length ())
	resize (n+1, empty_matrix);

      return data[n];
    }

  octave_value elem (octave_idx_type n) const
    {
#if defined (BOUNDS_CHECKING)
      return data.at (n);
#else
      return data[n];
#endif
    }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
