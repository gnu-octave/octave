/*

Copyright (C) 2004 David Bateman

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

In addition to the terms of the GPL, you are permitted to link
this program with any Open Source program, as defined by the
Open Source Initiative (www.opensource.org)

*/

#if !defined (octave_fcn_inline_h)
#define octave_fcn_inline_h 1

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
#include "ov-fcn-handle.h"

// Inline functions.

class
octave_fcn_inline : public octave_fcn_handle
{
public:

  octave_fcn_inline (void)
    : octave_fcn_handle(), iftext (), ifargs () { }

  octave_fcn_inline (const std::string& f, const string_vector& a, 
		     const std::string& n = std::string ());

  ~octave_fcn_inline (void) { }

  bool is_inline_function (void) const { return true; }

  octave_fcn_inline *fcn_inline_value (bool = false) { return this; }

  std::string fcn_text (void) const { return iftext; }

  string_vector fcn_arg_names (void) const { return ifargs; }

  octave_value convert_to_str_internal (bool, bool) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:

  // No copying!

  octave_fcn_inline (const octave_fcn_inline& fh);

  octave_fcn_inline& operator = (const octave_fcn_inline& fh);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  // The expression of an inline function.
  std::string iftext;

  // The args of an inline function.
  string_vector ifargs;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

