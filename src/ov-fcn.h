/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_function_h)
#define octave_function_h 1

#include <string>

#include "oct-time.h"
#include "str-vec.h"

#include "oct-alloc.h"
#include "oct-obj.h"
#include "ov-base.h"
#include "ov-typeinfo.h"
#include "symtab.h"

class tree_walker;

// Functions.

class
OCTINTERP_API
octave_function : public octave_base_value
{
public:

  octave_function (void)
    : relative (false), locked (false), private_function (false),
      xdispatch_class (), my_name (), my_dir_name (), doc () { }

  ~octave_function (void) { }

  octave_base_value *clone (void) const;
  octave_base_value *empty_clone (void) const;

  bool is_defined (void) const { return true; }

  bool is_function (void) const { return true; }

  virtual bool is_system_fcn_file (void) const { return false; }

  virtual std::string fcn_file_name (void) const { return std::string (); }

  virtual std::string parent_fcn_name (void) const { return std::string (); }

  virtual symbol_table::scope_id parent_fcn_scope (void) const { return -1; }

  virtual void mark_fcn_file_up_to_date (const octave_time&) { }

  virtual symbol_table::scope_id scope (void) { return -1; }

  virtual octave_time time_parsed (void) const
    { return octave_time (static_cast<time_t> (0)); }

  virtual octave_time time_checked (void) const
    { return octave_time (static_cast<time_t> (0)); }

  virtual bool is_nested_function (void) const { return false; }

  virtual bool is_class_constructor (void) const { return false; }

  virtual bool is_class_method (void) const { return false; }

  virtual bool takes_varargs (void) const { return false; }

  virtual bool takes_var_return (void) const { return false; }

  void stash_dispatch_class (const std::string& nm) { xdispatch_class = nm; }

  std::string dispatch_class (void) const { return xdispatch_class; }

  void mark_as_private_function (const std::string& cname = std::string ())
  {
    private_function = true;
    xdispatch_class = cname;
  }

  bool is_private_function (void) const { return private_function; }

  bool is_private_function_of_class (const std::string& nm)
    { return private_function && xdispatch_class == nm; }

  std::string dir_name (void) const { return my_dir_name; }

  void stash_dir_name (const std::string& dir) { my_dir_name = dir; }

  void lock (void)
  {
    this->lock_subfunctions ();
    locked = true;
  }

  void unlock (void)
  {
    this->unlock_subfunctions ();
    locked = false;
  }

  bool islocked (void) const { return locked; }

  virtual void lock_subfunctions (void) { }

  virtual void unlock_subfunctions (void) { }

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

  // TRUE if this function is tagged so that it can't be cleared.
  bool locked;

  // TRUE means this is a private function.
  bool private_function;

  // If this object is a class method or constructor, or a private
  // function inside a class directory, this is the name of the class
  // to which the method belongs. 
  std::string xdispatch_class;

  // The name of this function.
  std::string my_name;

  // The name of the directory in the path where we found this
  // function.  May be relative.
  std::string my_dir_name;

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
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
