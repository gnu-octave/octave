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

#if !defined (octave_function_h)
#define octave_function_h 1

#include <string>

#include "oct-time.h"
#include "str-vec.h"

#include "oct-alloc.h"
#include "oct-obj.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class tree_walker;

// Functions.

class
OCTINTERP_API
octave_function : public octave_base_value
{
public:

  octave_function (void) { }

  ~octave_function (void) { }

  octave_base_value *clone (void) const;
  octave_base_value *empty_clone (void) const;

  bool is_defined (void) const { return true; }

  bool is_function (void) const { return true; }

  virtual bool is_dynamically_loaded_function (void) const { return false; }

  virtual bool is_system_fcn_file (void) const { return false; }

  virtual std::string fcn_file_name (void) const { return std::string (); }

  virtual std::string parent_fcn_name (void) const { return std::string (); }

  virtual void mark_fcn_file_up_to_date (const octave_time&) { }

  virtual octave_time time_parsed (void) const
    { return octave_time (static_cast<time_t> (0)); }

  virtual octave_time time_checked (void) const
    { return octave_time (static_cast<time_t> (0)); }

  virtual bool is_nested_function (void) const { return false; }

  virtual bool is_user_script (void) const { return false; }

  virtual bool is_user_function (void) const { return false; }

  virtual bool takes_varargs (void) const { return false; }

  virtual bool takes_var_return (void) const { return false; }

  void mark_relative (void) { relative = true; }

  bool is_relative (void) const { return relative; }

  std::string name (void) const { return my_name; }

  void document (const std::string& ds) { doc = ds; }

  std::string doc_string (void) const { return doc; }

  virtual void unload (void) { }

  virtual void accept (tree_walker&) { }

protected:

  octave_function (const std::string& nm,
		   const std::string& ds = std::string ())
    : relative (false), my_name (nm), doc (ds) { }

  // TRUE if this function was found from a relative path element.
  bool relative;

  // The name of this function.
  std::string my_name;

  // The help text for this function.
  std::string doc;

private:

  // No copying!

  octave_function (const octave_function& f);

  octave_function& operator = (const octave_function& f);

  DECLARE_OCTAVE_ALLOCATOR
};

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
