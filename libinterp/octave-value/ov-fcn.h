////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_ov_fcn_h)
#define octave_ov_fcn_h 1

#include "octave-config.h"

#include <string>

#include "oct-time.h"
#include "str-vec.h"

#include "ovl.h"
#include "ov-base.h"
#include "ov-typeinfo.h"
#include "symscope.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class stack_frame;
class tree_evaluator;
class tree_walker;

OCTAVE_END_NAMESPACE(octave)

// Functions.

class
OCTINTERP_API
octave_function : public octave_base_value
{
public:

  octave_function (void)
    : m_relative (false), m_locked (false), m_private (false),
      m_dispatch_class (), m_package_name (), m_name (), m_dir_name (),
      m_doc () { }

  // No copying!

  octave_function (const octave_function& f) = delete;

  octave_function& operator = (const octave_function& f) = delete;

  ~octave_function (void) = default;

  octave_base_value * clone (void) const;
  octave_base_value * empty_clone (void) const;

  bool is_defined (void) const { return true; }

  bool is_function (void) const { return true; }

  virtual bool is_system_fcn_file (void) const { return false; }

  virtual std::string fcn_file_name (void) const { return ""; }

  virtual std::string src_file_name (void) const { return ""; }

  // The name to show in the profiler (also used as map-key).
  virtual std::string profiler_name (void) const { return name (); }

  virtual std::string parent_fcn_name (void) const { return ""; }

  virtual octave::symbol_scope parent_fcn_scope (void) const
  { return octave::symbol_scope (); }

  virtual std::list<std::string> parent_fcn_names (void) const
  { return std::list<std::string> (); }

  virtual void mark_fcn_file_up_to_date (const octave::sys::time&) { }

  virtual octave::symbol_scope scope (void) { return octave::symbol_scope (); }

  virtual octave::sys::time time_parsed (void) const
  { return octave::sys::time (static_cast<OCTAVE_TIME_T> (0)); }

  virtual octave::sys::time time_checked (void) const
  { return octave::sys::time (static_cast<OCTAVE_TIME_T> (0)); }

  virtual int call_depth (void) const { return 0; }

  virtual bool is_nested_function (void) const { return false; }

  virtual bool is_parent_function (void) const { return false; }

  virtual bool is_subfunction (void) const { return false; }

  bool is_class_constructor (const std::string& cname = "") const
  {
    return (is_classdef_constructor (cname) || is_legacy_constructor (cname));
  }

  bool is_class_method (const std::string& cname = "") const
  {
    return (is_classdef_method (cname) || is_legacy_method (cname));
  }

  virtual bool
  is_legacy_constructor (const std::string& = "") const
  { return false; }

  virtual bool
  is_classdef_constructor (const std::string& = "") const
  { return false; }

  virtual bool is_legacy_method (const std::string& = "") const
  { return false; }

  virtual bool is_classdef_method (const std::string& = "") const
  { return false; }

  virtual bool takes_varargs (void) const { return false; }

  virtual bool takes_var_return (void) const { return false; }

  // The next two functions are for dispatching to built-in
  // functions given built-in classes.

  virtual void push_dispatch_class (const std::string&) { }

  virtual bool handles_dispatch_class (const std::string&) const
  { return false; }

  void stash_dispatch_class (const std::string& nm) { m_dispatch_class = nm; }

  std::string dispatch_class (void) const { return m_dispatch_class; }

  void stash_package_name (const std::string& pack) { m_package_name = pack; }

  std::string package_name (void) const { return m_package_name; }

  virtual void
  mark_as_private_function (const std::string& cname = "")
  {
    m_private = true;
    m_dispatch_class = cname;
  }

  bool is_private_function (void) const { return m_private; }

  bool is_private_function_of_class (const std::string& nm) const
  { return m_private && m_dispatch_class == nm; }

  virtual bool
  is_anonymous_function_of_class (const std::string& = "") const
  { return false; }

  std::string dir_name (void) const { return m_dir_name; }

  void stash_dir_name (const std::string& dir) { m_dir_name = dir; }

  void lock (void)
  {
    this->lock_subfunctions ();
    m_locked = true;
  }

  void unlock (void)
  {
    this->unlock_subfunctions ();
    m_locked = false;
  }

  bool islocked (void) const { return m_locked; }

  virtual void lock_subfunctions (void) { }

  virtual void unlock_subfunctions (void) { }

  virtual void maybe_relocate_end (void) { }

  // Not valid until after the function is completely parsed.
  virtual bool has_subfunctions (void) const { return false; }

  virtual void stash_subfunction_names (const std::list<std::string>&) { }

  virtual std::list<std::string> subfunction_names (void) const
  { return std::list<std::string> (); }

  void mark_relative (void) { m_relative = true; }

  bool is_relative (void) const { return m_relative; }

  std::string name (void) const { return m_name; }

  std::string canonical_name (void) const
  {
    if (m_package_name.empty ())
      return m_name;
    else
      return m_package_name + '.' + m_name;
  }

  void document (const std::string& ds) { m_doc = ds; }

  virtual std::string
  doc_string (const std::string& /*meth_name*/ = "") const { return m_doc; }

  virtual void unload (void) { }

  virtual void accept (octave::tree_walker&) { }

  virtual bool accepts_postfix_index (char type) const
  { return (type == '('); }

  // Push new stack frame (if necessary) and execute function.
  virtual octave_value_list
  call (octave::tree_evaluator& tw, int nargout = 0,
        const octave_value_list& args = octave_value_list ());

  // Execute function without pushing new stack frame (assumes that has
  // already been done).
  virtual octave_value_list
  execute (octave::tree_evaluator& tw, int nargout = 0,
           const octave_value_list& args = octave_value_list ()) = 0;

protected:

  octave_function (const std::string& nm,
                   const std::string& ds = "")
    : m_relative (false), m_locked (false), m_private (false),
      m_dispatch_class (), m_name (nm), m_dir_name (), m_doc (ds) { }

  // TRUE if this function was found from a m_relative path element.
  bool m_relative;

  // TRUE if this function is tagged so that it can't be cleared.
  bool m_locked;

  // TRUE means this is a private function.
  bool m_private;

  // If this object is a class method or constructor, or a private
  // function inside a class directory, this is the name of the class
  // to which the method belongs.
  std::string m_dispatch_class;

  // If this function is part of a package, this is the full name
  // of the package to which the function belongs.
  std::string m_package_name;

  // The name of this function.
  std::string m_name;

  // The name of the directory in the path where we found this
  // function.  May be m_relative.
  std::string m_dir_name;

  // The help text for this function.
  std::string m_doc;
};

#endif
