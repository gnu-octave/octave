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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_file_h)
#define octave_file_h 1

#include <cstdlib>

#include <iostream>
#include <string>

#include "oct-alloc.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"

class tree_walker;
class octave_stream;
class octave_value;
class octave_value_list;

// Lists.

class
octave_file : public octave_base_value
{
public:

  octave_file (void)
    : octave_base_value (), stream (), number (-1) { }

  octave_file (const octave_stream& s, int n)
    : octave_base_value (), stream (s), number (n) { }

  octave_file (const octave_file& f)
    : octave_base_value (), stream (f.stream), number (f.number) { }

  ~octave_file (void) { }

  octave_value *clone (void) const { return new octave_file (*this); }

  // For compatibility, a file object should appear as if it is a
  // scalar object, in contexts where that is needed.
  octave_value *empty_clone (void) const { return new octave_scalar (); }

  type_conv_fcn numeric_conversion_function (void) const;

  double double_value (bool) const { return number; }

  double scalar_value (bool) const { return number; }

  octave_stream stream_value (void) const { return stream; }

  int stream_number (void) const { return number; }

  bool is_defined (void) const { return true; }

  bool is_stream (void) const { return true; }

  // Pretend we are a real scalar for better compatibility, maybe.

  bool is_real_scalar (void) const { return true; }

  bool is_real_type (void) const { return true; }

  dim_vector dims (void) const { static dim_vector dv (1, 1); return dv; }

  octave_value all (int = 0) const { return (number != 0.0); }

  octave_value any (int = 0) const { return (number != 0.0); }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

private:

  // The stream object.
  octave_stream stream;

  // The number of the beast.
  int number;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
