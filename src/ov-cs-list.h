/*

Copyright (C) 2002 John W. Eaton

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

#if !defined (octave_cs_list_h)
#define octave_cs_list_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-alloc.h"
#include "oct-obj.h"
#include "ov-list.h"
#include "ov-typeinfo.h"

class tree_walker;

// Lists.

class
octave_cs_list : public octave_list
{
public:

  octave_cs_list (void)
    : octave_list () { }

  octave_cs_list (const octave_value_list& l)
    : octave_list (l) { }

  octave_cs_list (const octave_cs_list& l)
    : octave_list (l) { }

  ~octave_cs_list (void) { }

  octave_value *clone (void) const { return new octave_cs_list (*this); }
  octave_value *empty_clone (void) const { return new octave_cs_list (); }

#if 0
  octave_value subsref (const std::string type,
			const std::list<octave_value_list>& idx);

  octave_value do_index_op (const octave_value_list& idx, int resize_ok);

  octave_value subsasgn (const std::string type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  void assign (const octave_value_list& idx, const octave_value& rhs);

  int length (void) const { return lst.length (); }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

#endif

  bool is_cs_list (void) const { return true; }

#if 0

  bool is_list (void) const { return true; }

  octave_value_list list_value (void) const { return lst; }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;
#endif

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  DECLARE_OCTAVE_ALLOCATOR
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
