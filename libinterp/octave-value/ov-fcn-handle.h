////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2020 The Octave Project Developers
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

#if ! defined (octave_ov_fcn_handle_h)
#define octave_ov_fcn_handle_h 1

#include "octave-config.h"

#include <iosfwd>
#include <list>
#include <string>

#include "ov-base.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "symscope.h"

namespace octave
{
  class interpreter;
  class stack_frame;
  class tree_evaluator;
}

// Function handles.

class
OCTINTERP_API
octave_fcn_handle : public octave_base_value
{
public:

  static const std::string anonymous;

  octave_fcn_handle (void)
    : m_fcn (), m_obj (), m_name (), m_scope (), m_is_nested (false),
      m_closure_frames (nullptr), m_dispatch_class ()
  { }

  octave_fcn_handle (const octave::symbol_scope& scope, const std::string& n);

  octave_fcn_handle (const octave::symbol_scope& scope,
                     const octave_value& f,
                     const std::string& n = anonymous);

  octave_fcn_handle (const octave_value& f,
                     const std::string& n = anonymous);

  octave_fcn_handle (const octave_fcn_handle& fh) = default;

  ~octave_fcn_handle (void);

  octave_base_value * clone (void) const
  { return new octave_fcn_handle (*this); }
  octave_base_value * empty_clone (void) const
  { return new octave_fcn_handle (); }

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::subsref;

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list tmp = subsref (type, idx, 1);
    return tmp.length () > 0 ? tmp(0) : octave_value ();
  }

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout);

  bool is_defined (void) const { return true; }

  bool is_function_handle (void) const { return true; }

  bool is_nested (void) const { return m_is_nested; }

  dim_vector dims (void) const;

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false);
  octave_user_function * user_function_value (bool = false);

  octave_fcn_handle * fcn_handle_value (bool = false) { return this; }

  octave_value fcn_val (void) const { return m_fcn; }

  std::string fcn_name (void) const { return m_name; }

  void push_closure_context (octave::tree_evaluator& tw);

  octave_value workspace (void) const;

  void set_dispatch_class (const std::string& class_name)
  {
    m_dispatch_class = class_name;
  }

  std::string get_dispatch_class (void) const { return m_dispatch_class; }

  bool is_equal_to (const octave_fcn_handle&) const;

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  // Simple function handles are printed without a newline.
  bool print_as_scalar (void) const { return m_name != anonymous; }

private:

  bool set_fcn (const std::string& octaveroot, const std::string& fpath);

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

protected:

  // The function we are handling (this should be valid for handles to
  // anonymous functions and some other special cases).  Otherwise, we
  // perform dynamic lookup based on the name of the function we are
  // handling and the scope where the function handle object was created.
  octave_value m_fcn;

  // If defined, this is the classdef object corresponding to the
  // classdef method we are handling.
  octave_value m_obj;

  // The function we would find without considering argument types.  We
  // cache this value so that the function_value and user_function_value
  // methods may continue to work.
  octave_value m_generic_fcn;

  // The name of the handle, not including the "@".
  std::string m_name;

  // The scope where this object was defined.
  octave::symbol_scope m_scope;

  // TRUE means this is a handle to a nested function.
  bool m_is_nested;

  // Saved stack frames for handles to nested functions.  This allows us
  // to access non-locals and other context info when calling nested
  // functions indirectly through handles.
  std::list<octave::stack_frame *> *m_closure_frames;

  // The name of the class in which this handle was created, if any.
  // Used to determine access permission when the referenced function is
  // called.
  std::string m_dispatch_class;

  bool parse_anon_fcn_handle (const std::string& fcn_text);

  virtual octave_value_list call (int nargout, const octave_value_list& args);
};

namespace octave
{
  extern octave_value
  make_fcn_handle (interpreter& interp, const std::string& name);
}

#endif
