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

#if !defined (octave_range_h)
#define octave_range_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "Range.h"

#include "lo-utils.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "error.h"
#include "mappers.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Range values.

class
octave_range : public octave_base_value
{
public:

  octave_range (void)
    : octave_base_value () { }

  octave_range (double base, double limit, double inc)
    : octave_base_value (), range (base, limit, inc)
      {
	if (range.nelem () < 0)
	  ::error ("invalid range");
      }

  octave_range (const Range& r)
    : octave_base_value (), range (r)
      {
	if (range.nelem () < 0)
	  ::error ("invalid range");
      }

  octave_range (const octave_range& r)
    : octave_base_value (), range (r.range) { }

  ~octave_range (void) { }

  octave_value *clone (void) { return new octave_range (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  type_conv_fcn numeric_conversion_function (void) const;

  octave_value *try_narrowing_conversion (void);

  octave_value index (const octave_value_list& idx) const;

  idx_vector index_vector (void) const { return idx_vector (range); }

  int rows (void) const { return (columns () > 0); }
  int columns (void) const { return range.nelem (); }

  bool is_defined (void) const { return true; }

  bool is_range (void) const { return true; }

  // XXX DO ME XXX
  octave_value all (void) const;
  octave_value any (void) const;

  bool is_real_type (void) const { return true; }

  bool valid_as_scalar_index (void) const
    {
      return (range.nelem () == 1
	      && ! xisnan (range.base ())
	      && NINT (range.base ()) == 1);
    }

  bool valid_as_zero_index (void) const
    {
      return (range.nelem () == 1
	      && ! xisnan (range.base ())
	      && NINT (range.base ()) == 0);
    }

  // XXX DO ME XXX
  bool is_true (void) const;

  double double_value (bool) const;

  Matrix matrix_value (bool) const
    { return range.matrix_value (); }

  Complex complex_value (bool) const;

  ComplexMatrix complex_matrix_value (bool) const
    { return range.matrix_value (); }

  Range range_value (void) const { return range; }

  octave_value not (void) const;

  octave_value uminus (void) const { return octave_value (- range); }

  octave_value transpose (void) const;

  octave_value hermitian (void) const;

  octave_value convert_to_str (void) const;

  void print (ostream& os, bool pr_as_read_syntax = false);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  Range range;

  static octave_allocator allocator;

  // Type id of range objects, set by register_type ().
  static int t_id;

  // Type name of scalar objects, defined in ov-range.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
