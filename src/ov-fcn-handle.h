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
#include "ov-fcn.h"
#include "symtab.h"

// Function handles.

class
octave_fcn_handle : public octave_base_value
{
public:

  octave_fcn_handle (octave_function *f, const std::string& n)
    : fcn (f), nm (n) { }

  octave_fcn_handle (const octave_fcn_handle& fh)
    : fcn (fh.fcn), nm (fh.nm) { }

  octave_fcn_handle& operator = (const octave_fcn_handle& fh)
    {
      if (this != &fh)
	{
	  fcn = fh.fcn;
	  nm  = fh.nm;
	}

      return *this;
    }

  ~octave_fcn_handle (void) { }

  bool is_defined (void) const { return fcn; }

  octave_function *function_value (bool) { return fcn; }

  octave_fcn_handle *fcn_handle_value (bool) { return this; }

  bool print_as_scalar (void) const { return true; }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  std::string name (void) const { return nm; }

private:

  // The function we are handling.
  octave_function *fcn;

  // The name of the handle.
  std::string nm;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  DECLARE_OCTAVE_ALLOCATOR
};

extern octave_value make_fcn_handle (const std::string& nm);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
