/*

Copyright (C) 2003 John W. Eaton

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

#if !defined (octave_fcn_handle_h)
#define octave_fcn_handle_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>
#include <string>

#include "oct-alloc.h"

#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "symtab.h"

// Function handles.

class
octave_fcn_handle : public octave_base_value
{
public:
  enum fcn_type { fcn_handle = 1, fcn_inline = 2 };

  octave_fcn_handle (void)
    : typ (fcn_handle), fcn (), nm (), iftext (), ifargs () { }

  octave_fcn_handle (const octave_value& f,  const std::string& n)
    : typ (fcn_handle), fcn (f), nm (n), iftext (), ifargs () { }

  octave_fcn_handle (const std::string& f, const string_vector& a, 
		     const std::string& n = std::string ());

  ~octave_fcn_handle (void) { }

  octave_value subsref (const std::string&,
			const std::list<octave_value_list>&)
    {
      panic_impossible ();
      return octave_value ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  bool is_defined (void) const { return true; }

  bool is_function_handle (void) const { return true; }

  octave_function *function_value (bool = false)
    { return fcn.function_value (); }

  std::string inline_fcn_name (void) const { return nm; }

  std::string inline_fcn_text (void) const { return iftext; }

  string_vector inline_fcn_arg_names (void) const { return ifargs; }

  bool is_inline (void) const { return (typ == fcn_inline); }

  octave_fcn_handle *fcn_handle_value (bool = false) { return this; }

  octave_value convert_to_str_internal (bool, bool) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:

  // No copying!

  octave_fcn_handle (const octave_fcn_handle& fh);

  octave_fcn_handle& operator = (const octave_fcn_handle& fh);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  // The type of function handle 
  fcn_type typ;

  // The function we are handling.
  octave_value fcn;

  // The name of the handle, including the "@".
  std::string nm;

  // The expression of an inline function
  std::string iftext;

  // The args of an inline function
  string_vector ifargs;
};

extern octave_value make_fcn_handle (const std::string& nm);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
