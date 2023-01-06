////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
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
#include <memory>
#include <string>

#include "oct-map.h"
#include "ov-base.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "stack-frame.h"
#include "symscope.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;
class tree_evaluator;

// Function handles.

class base_fcn_handle
{
public:

  base_fcn_handle (const std::string& name = "",
                   const std::string& file = "")
    : m_name (name), m_file (file)
  { }

  base_fcn_handle (const base_fcn_handle&) = default;

  virtual ~base_fcn_handle (void) = default;

  virtual base_fcn_handle * clone (void) const = 0;

  virtual std::string type (void) const = 0;

  virtual bool is_internal (void) const { return false; }

  virtual bool is_simple (void) const { return false; }

  virtual bool is_scoped (void) const { return false; }

  virtual bool is_nested (void) const { return false; }

  virtual bool is_nested (const std::shared_ptr<stack_frame>&) const
  {
    return false;
  }

  virtual bool is_weak_nested (void) const { return false; }

  virtual bool is_class_simple (void) const { return false; }

  virtual bool is_anonymous (void) const { return false; }

  virtual bool is_weak_anonymous (void) const { return false; }

  virtual octave_value make_weak_nested_handle (void) const;

  virtual octave_value make_weak_anonymous_handle (void) const;

  std::string fcn_name (void) const { return m_name; }

  std::string file (void) const { return m_file; }

  octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout);

  virtual octave_value_list
  call (int nargout, const octave_value_list& args) = 0;

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  virtual octave_function * function_value (bool = false)
  {
    return nullptr;
  }

  virtual octave_user_function * user_function_value (bool = false)
  {
    return nullptr;
  }

  virtual octave_value fcn_val (void) { return octave_value (); }

  virtual octave_value workspace (void) const { return octave_value (); }

  // Should be const.
  virtual octave_scalar_map info (void) { return octave_scalar_map (); }

  virtual void set_dispatch_class (const std::string& /*class_name*/) { }

  virtual std::string get_dispatch_class (void) const { return ""; }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  virtual bool save_ascii (std::ostream& os);

  virtual bool load_ascii (std::istream& is);

  virtual bool save_binary (std::ostream& os, bool save_as_floats);

  virtual bool load_binary (std::istream& is, bool swap,
                            mach_info::float_format fmt);

  virtual bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                          bool save_as_floats);

  virtual bool load_hdf5 (octave_hdf5_id& group_hid,
                          octave_hdf5_id& space_hid,
                          octave_hdf5_id& type_hid);

  virtual void print_raw (std::ostream&, bool /*pr_as_read_syntax*/,
                          int /*current_print_indent_level*/) const
  { }

  // Function handles are printed without a newline by default.
  virtual bool print_as_scalar (void) const { return true; }

  virtual bool
  set_fcn (const std::string& /*octaveroot*/, const std::string& /*fpath*/)
  {
    return false;
  }

protected:

  void warn_load (const char *file_type) const;
  void warn_save (const char *file_type) const;

  void unimplemented (const char *op, const char *fmt) const;

  // The name of the handle, not including the "@", or the text of the
  // anonymous function.
  std::string m_name;

  // The name of the file where the named function was defined.
  std::string m_file;
};

OCTAVE_END_NAMESPACE(octave)

class
OCTINTERP_API
octave_fcn_handle : public octave_base_value
{
public:

  static const std::string anonymous;

  // Creates an invalid function handle.  Used to create generic
  // function handle objects when loading function handles.  Further
  // dispatch happens in the octave_fcn_handle load/save functions.
  octave_fcn_handle (void);

  // Create a handle to a built-in or internal function.
  octave_fcn_handle (const octave_value& fcn);

  // Create a simple function handle that is not bound to a function.
  // Lookup happens when a function call is attempted.
  octave_fcn_handle (const std::string& name);

  // Create a simple function handle that is bound to a function.
  octave_fcn_handle (const octave_value& fcn, const std::string& name);

  // Create a function handle that might be bound to a class method.
  octave_fcn_handle (const std::string& class_nm, const std::string& meth_nm);

  // Create a function handle bound to a class method.
  octave_fcn_handle (const octave_value& fcn, const std::string& class_nm,
                     const std::string& meth_nm);

  // Create a function handle bound to a class method.
  octave_fcn_handle (const octave_value& obj, const octave_value& fcn,
                     const std::string& class_nm,
                     const std::string& meth_nm);

  // Create a function handle bound to a scoped function.
  octave_fcn_handle (const octave_value& fcn, const std::string& name,
                     const std::list<std::string>& parentage);

  // Create a handle to a nested function.
  octave_fcn_handle (const octave_value& fcn, const std::string& name,
                     const std::shared_ptr<octave::stack_frame>& closure_frames);

  // Create an anonymous function handle with local variable values
  // provided in LOCAL_VARS.
  octave_fcn_handle (const octave_value& fcn,
                     const octave::stack_frame::local_vars_map& local_vars,
                     const std::shared_ptr<octave::stack_frame>& closure_frames
                     = std::shared_ptr<octave::stack_frame> ());

  octave_fcn_handle (octave::base_fcn_handle *rep);

  octave_fcn_handle (const octave_fcn_handle& fh);

  ~octave_fcn_handle (void) = default;

  octave_base_value * clone (void) const
  {
    return new octave_fcn_handle (*this);
  }

  octave_base_value * empty_clone (void) const
  {
    return new octave_fcn_handle ();
  }

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
                             int nargout)
  {
    return m_rep->subsref (type, idx, nargout);
  }

  octave_value_list call (int nargout, const octave_value_list& args);

  bool is_defined (void) const { return true; }

  builtin_type_t builtin_type (void) const { return btyp_func_handle; }

  bool is_function_handle (void) const { return true; }

  bool is_internal (void) const { return m_rep->is_internal (); }

  bool is_simple (void) const { return m_rep->is_simple (); }

  bool is_scoped (void) const { return m_rep->is_scoped (); }

  bool is_nested (void) const { return m_rep->is_nested (); }

  bool is_nested (const std::shared_ptr<octave::stack_frame>& frame) const
  {
    return m_rep->is_nested (frame);
  }

  bool is_weak_nested (void) const { return m_rep->is_weak_nested (); }

  bool is_class_simple (void) const { return m_rep->is_class_simple (); }

  bool is_anonymous (void) const { return m_rep->is_anonymous (); }

  bool is_weak_anonymous (void) const { return m_rep->is_weak_anonymous (); }

  octave_value make_weak_nested_handle (void) const
  {
    return m_rep->make_weak_nested_handle ();
  }

  octave_value make_weak_anonymous_handle (void) const
  {
    return m_rep->make_weak_anonymous_handle ();
  }

  dim_vector dims (void) const;

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false)
  {
    return m_rep->function_value ();
  }

  octave_user_function * user_function_value (bool = false)
  {
    return m_rep->user_function_value ();
  }

  octave_fcn_handle * fcn_handle_value (bool = false) { return this; }

  octave_value fcn_val (void) { return m_rep->fcn_val (); }

  // FCN_NAME should be eliminated.
  std::string fcn_name (void) const { return m_rep->fcn_name (); }

  octave_value workspace (void) const
  {
    return m_rep->workspace ();
  }

  octave_scalar_map info (void) { return m_rep->info (); }

  void set_dispatch_class (const std::string& class_name)
  {
    m_rep->set_dispatch_class (class_name);
  }

  std::string get_dispatch_class (void) const
  {
    return m_rep->get_dispatch_class ();
  }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const
  {
    return m_rep->convert_to_str_internal (pad, force, type);
  }

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
  bool print_as_scalar (void) const { return m_rep->print_as_scalar (); }

  friend bool
  is_equal_to (const octave_fcn_handle& fh1, const octave_fcn_handle& fh2);

private:

  std::shared_ptr<octave::base_fcn_handle> m_rep;

  octave::base_fcn_handle * get_rep (void) const { return m_rep.get (); }

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

extern bool
is_equal_to (const octave_fcn_handle& fh1, const octave_fcn_handle& fh2);

#endif
