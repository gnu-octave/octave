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

#if !defined (octave_file_h)
#define octave_file_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "oct-alloc.h"
#include "ov-base.h"
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
    : octave_base_value (), stream (0), number (-1) { }

  octave_file (octave_stream *s, int n)
    : octave_base_value (), stream (s), number (n) { }

  octave_file (const octave_file& f)
    : octave_base_value (), stream (f.stream), number (f.number) { }

  ~octave_file (void) { }

  octave_value *clone (void) { return new octave_file (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  type_conv_fcn numeric_conversion_function (void) const;

  double double_value (void) const { return static_cast<double> (number); }

  octave_stream *stream_value (void) const { return stream; }

  int stream_number (void) const { return number; }

  bool is_defined (void) const { return true; }

  bool is_file (void) const { return true; }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // The stream object.
  octave_stream *stream;

  // The number of the beast.
  int number;

  // For custom memory management.
  static octave_allocator allocator;

  // Type id of list objects, set by register_type().
  static int t_id;

  // Type name of list objects, defined in ov-list.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
