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

#if !defined (octave_struct_h)
#define octave_struct_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-alloc.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Data structures.

class
octave_struct : public octave_base_value
{
public:

  octave_struct (void)
    : octave_base_value () { }

  octave_struct (const Octave_map& m)
    : octave_base_value (), map (m) { }

  octave_struct (const octave_struct& s)
    : octave_base_value (), map (s.map) { }

  ~octave_struct (void) { }

  octave_value *clone (void) const { return new octave_struct (*this); }
  octave_value *empty_clone (void) const { return new octave_struct (); }

  octave_value_list dotref (const octave_value_list& idx);

  octave_value subsref (const std::string type,
			const SLList<octave_value_list>& idx);

  static octave_value numeric_conv (const octave_value_list& val,
				    const std::string& type);

  octave_value subsasgn (const std::string type,
			 const SLList<octave_value_list>& idx,
			 const octave_value& rhs);

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_map (void) const { return true; }

  Octave_map map_value (void) const { return map; }

  string_vector map_keys (void) const { return map.keys (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

private:

  // The associative array used to manage the structure data.
  Octave_map map;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
