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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <istream>
#include <list>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include "file-ops.h"
#include "oct-locbuf.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "file-stat.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "oct-env.h"
#include "oct-hdf5.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-cell.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "parse.h"
#include "pr-output.h"
#include "pt-arg-list.h"
#include "pt-assign.h"
#include "pt-cmd.h"
#include "pt-eval.h"
#include "pt-exp.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "stack-frame.h"
#include "syminfo.h"
#include "symscope.h"
#include "unwind-prot.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-oct-text.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
                                     "function handle",
                                     "function_handle");

const std::string octave_fcn_handle::anonymous ("@<anonymous>");

OCTAVE_BEGIN_NAMESPACE(octave)

class invalid_fcn_handle : public base_fcn_handle
{
public:

  invalid_fcn_handle (void) : base_fcn_handle ("<invalid>") { }

  invalid_fcn_handle (const invalid_fcn_handle&) = default;

  ~invalid_fcn_handle (void) = default;

  invalid_fcn_handle * clone (void) const
  {
    return new invalid_fcn_handle (*this);
  }

  std::string type (void) const { return "<invalid>"; }

  octave_value_list call (int nargout, const octave_value_list& args);
};

// Create a handle to an unnamed internal function.  There will be no
// way to save and reload it.  See, for example, the F__fltk_check__
// function in __init_fltk__.cc.

class internal_fcn_handle : public base_fcn_handle
{
public:

  internal_fcn_handle (const octave_value& fcn)
    : base_fcn_handle ("<internal>"), m_fcn (fcn)
  { }

  internal_fcn_handle (const internal_fcn_handle&) = default;

  ~internal_fcn_handle (void) = default;

  internal_fcn_handle * clone (void) const
  {
    return new internal_fcn_handle (*this);
  }

  std::string type (void) const { return "<internal>"; }

  bool is_internal (void) const { return true; }

  octave_value_list call (int nargout, const octave_value_list& args);

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false)
  {
    return m_fcn.function_value ();
  }

  octave_user_function * user_function_value (bool = false)
  {
    return m_fcn.user_function_value ();
  }

  octave_value fcn_val (void) { return m_fcn; }

  // Should be const.
  octave_scalar_map info (void);

  friend bool is_equal_to (const internal_fcn_handle& fh1,
                           const internal_fcn_handle& fh2);

private:

  octave_value m_fcn;
};

class simple_fcn_handle : public base_fcn_handle
{
public:

  // FIXME: octaveroot is temporary information used when loading
  // handles.  Can we avoid using it in the constructor?

  simple_fcn_handle (const std::string& name = "",
                     const std::string& file = "",
                     const std::string& /*octaveroot*/ = "")
    : base_fcn_handle (name, file), m_fcn ()
  { }

  simple_fcn_handle (const octave_value& fcn, const std::string& name)
    : base_fcn_handle (name), m_fcn (fcn)
  {
    if (m_fcn.is_defined ())
      {
        octave_function *oct_fcn = m_fcn.function_value ();

        if (oct_fcn)
          m_file = oct_fcn->fcn_file_name ();
      }
  }

  simple_fcn_handle (const simple_fcn_handle&) = default;

  ~simple_fcn_handle (void) = default;

  simple_fcn_handle * clone (void) const
  {
    return new simple_fcn_handle (*this);
  }

  std::string type (void) const { return "simple"; }

  bool is_simple (void) const { return true; }

  octave_value_list call (int nargout, const octave_value_list& args);

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool);

  octave_user_function * user_function_value (bool);

  octave_value fcn_val (void);

  // Should be const.
  octave_scalar_map info (void);

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap, mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_hid, const char *name,
                  bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id& group_hid, octave_hdf5_id& space_hid,
                  octave_hdf5_id& type_hid);

  void print_raw (std::ostream& os, bool pr_as_read_syntax,
                  int current_print_indent_level) const;

  friend bool is_equal_to (const simple_fcn_handle& fh1,
                           const simple_fcn_handle& fh2);

private:

  octave_value m_fcn;
};

class scoped_fcn_handle : public base_fcn_handle
{
public:

  // FIXME: octaveroot is temporary information used when loading
  // handles.  Can we avoid using it in the constructor?

  scoped_fcn_handle (const std::string& name = "",
                     const std::string& file = "",
                     const std::string& /*octaveroot*/ = "")
    : base_fcn_handle (name, file)
  { }

  scoped_fcn_handle (const octave_value& fcn, const std::string& name,
                     const std::list<std::string>& parentage);

  scoped_fcn_handle (const scoped_fcn_handle&) = default;

  ~scoped_fcn_handle (void) = default;

  scoped_fcn_handle * clone (void) const
  {
    return new scoped_fcn_handle (*this);
  }

  std::string type (void) const { return "scopedfunction"; }

  bool is_scoped (void) const { return true; }

  octave_value_list call (int nargout, const octave_value_list& args);

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false)
  {
    return m_fcn.function_value ();
  }

  octave_user_function * user_function_value (bool = false)
  {
    return m_fcn.user_function_value ();
  }

  octave_value fcn_val (void) { return m_fcn; }

  // Should be const.
  octave_scalar_map info (void);

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap, mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id& group_hid, octave_hdf5_id& space_hid,
                  octave_hdf5_id& type_hid);

  void print_raw (std::ostream&, bool pr_as_read_syntax,
                  int current_print_indent_level) const;

  friend bool is_equal_to (const scoped_fcn_handle& fh1,
                           const scoped_fcn_handle& fh2);

protected:

  void find_function (void);

  // The function we are handling.
  octave_value m_fcn;

  // List of parent function names.  The first element is the name of
  // m_fcn.
  std::list<std::string> m_parentage;
};

class base_nested_fcn_handle : public base_fcn_handle
{
public:

  // FIXME: octaveroot is temporary information used when loading
  // handles.  Can we avoid using it in the constructor?

  base_nested_fcn_handle (const std::string& name = "",
                          const std::string& file = "",
                          const std::string& /*octaveroot*/ = "")
    : base_fcn_handle (name, file)
  { }

  base_nested_fcn_handle (const octave_value& fcn, const std::string& name)
    : base_fcn_handle (name), m_fcn (fcn)
  { }

  std::string type (void) const { return "nested"; }

  using base_fcn_handle::is_nested;

  bool is_nested (void) const { return true; }

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false)
  {
    return m_fcn.function_value ();
  }

  octave_user_function * user_function_value (bool = false)
  {
    return m_fcn.user_function_value ();
  }

  octave_value fcn_val (void) { return m_fcn; }

  virtual octave_value workspace (void) const = 0;

  // Should be const.
  octave_scalar_map info (void);

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap, mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id& group_hid, octave_hdf5_id& space_hid,
                  octave_hdf5_id& type_hid);

  void print_raw (std::ostream&, bool pr_as_read_syntax,
                  int current_print_indent_level) const;

protected:

  // The function we are handling.
  octave_value m_fcn;
};

class nested_fcn_handle : public base_nested_fcn_handle
{
public:

  // FIXME: octaveroot is temporary information used when loading
  // handles.  Can we avoid using it in the constructor?

  nested_fcn_handle (const std::string& name = "",
                     const std::string& file = "",
                     const std::string& octaveroot = "")
    : base_nested_fcn_handle (name, file, octaveroot)
  { }

  nested_fcn_handle (const octave_value& fcn, const std::string& name,
                     const std::shared_ptr<stack_frame>& stack_context)
    : base_nested_fcn_handle (fcn, name), m_stack_context (stack_context)
  {
    if (m_stack_context)
      m_stack_context->mark_closure_context ();
  }

  nested_fcn_handle (const nested_fcn_handle&) = default;

  ~nested_fcn_handle (void) = default;

  using base_nested_fcn_handle::is_nested;

  bool is_nested (const std::shared_ptr<stack_frame>& frame) const
  {
    return frame == m_stack_context;
  }

  nested_fcn_handle * clone (void) const
  {
    return new nested_fcn_handle (*this);
  }

  octave_value make_weak_nested_handle (void) const;

  octave_value_list call (int nargout, const octave_value_list& args);

  octave_value workspace (void) const;

  friend bool is_equal_to (const nested_fcn_handle& fh1,
                           const nested_fcn_handle& fh2);

  std::shared_ptr<stack_frame> stack_context (void) const
  {
    return m_stack_context;
  }

protected:

  // Pointer to closure stack frames.
  std::shared_ptr<stack_frame> m_stack_context;
};

class weak_nested_fcn_handle : public base_nested_fcn_handle
{
public:

  weak_nested_fcn_handle (const nested_fcn_handle& nfh)
    : base_nested_fcn_handle (nfh), m_stack_context (nfh.stack_context ())
  { }

  weak_nested_fcn_handle (const weak_nested_fcn_handle&) = default;

  ~weak_nested_fcn_handle (void) = default;

  weak_nested_fcn_handle * clone (void) const
  {
    return new weak_nested_fcn_handle (*this);
  }

  bool is_weak_nested (void) const { return true; }

  octave_value_list call (int nargout, const octave_value_list& args);

  octave_value workspace (void) const;

  friend bool is_equal_to (const weak_nested_fcn_handle& fh1,
                           const weak_nested_fcn_handle& fh2);

protected:

  // Pointer to closure stack frames.
  std::weak_ptr<stack_frame> m_stack_context;
};

class class_simple_fcn_handle : public base_fcn_handle
{
public:

  // FIXME: octaveroot is temporary information used when loading
  // handles.  Can we avoid using it in the constructor?

  class_simple_fcn_handle (const std::string& name,
                           const std::string& file,
                           const std::string& /*octaveroot*/)
    : base_fcn_handle (name, file)
  { }

  // FIXME: is the method name supposed to be just the method name or
  // also contain the object name?

  class_simple_fcn_handle (const std::string& class_nm,
                           const std::string& meth_nm);

  class_simple_fcn_handle (const octave_value& fcn,
                           const std::string& class_nm,
                           const std::string& meth_nm);

  class_simple_fcn_handle (const octave_value& obj, const octave_value& fcn,
                           const std::string& class_nm,
                           const std::string& meth_nm);

  class_simple_fcn_handle (const class_simple_fcn_handle&) = default;

  ~class_simple_fcn_handle (void) = default;

  class_simple_fcn_handle * clone (void) const
  {
    return new class_simple_fcn_handle (*this);
  }

  std::string type (void) const { return "classsimple"; }

  bool is_class_simple (void) const { return true; }

  octave_value_list call (int nargout, const octave_value_list& args);

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false)
  {
    // FIXME: Shouldn't the lookup rules here match those used in the
    // call method?

    if (m_fcn.is_defined ())
      return m_fcn.function_value ();

    symbol_table& symtab = __get_symbol_table__ ();

    // FIXME: is caching the correct thing to do?
    // Cache this value so that the pointer will be valid as long as the
    // function handle object is valid.

    // FIXME: This should probably dispatch to the respective class method.
    // But that breaks if a function handle is used in a class method with
    // e.g. bsxfun with arguments of a different class (see bug #59661).
    // m_fcn = symtab.find_method (m_name, m_dispatch_class);
    m_fcn = symtab.find_function (m_name, octave_value_list ());

    return m_fcn.is_defined () ? m_fcn.function_value () : nullptr;
  }

  octave_user_function * user_function_value (bool = false)
  {
    return m_fcn.user_function_value ();
  }

  octave_value fcn_val (void) { return m_fcn; }

  // Should be const.
  octave_scalar_map info (void);

  std::string dispatch_class (void) const { return m_dispatch_class; }

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap, mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id& group_hid, octave_hdf5_id& space_hid,
                  octave_hdf5_id& type_hid);

  void print_raw (std::ostream&, bool pr_as_read_syntax,
                  int current_print_indent_level) const;

  friend bool is_equal_to (const class_simple_fcn_handle& fh1,
                           const class_simple_fcn_handle& fh2);

protected:

  // The object containing the method we are handing.
  octave_value m_obj;

  // The method we are handling.
  octave_value m_fcn;

  // Name of the class that m_fcn belongs to.
  std::string m_dispatch_class;
};

// Handles to anonymous functions are similar to handles to nested
// functions.  If they are created in a context that contains nested
// functions, then they store a link to the parent call stack frames
// that are active when they are created.  These call stack frames
// (closure frames) provide access to variables needed by any nested
// functions that are called from the anonymous function.  Anonymous
// functions also store a list of values from their parent scope
// corresponding to the symbols in the anonymous function.  This list
// of values captures the variable values that are visible in the
// scope where they are created.
//
// Note that because handles to anonymous and nested functions capture
// call stack frames when they are created, they will cause deletion
// of the values in those frames to be deferred until the handles to
// the anonymous or nested functions are deleted.
//
// Would it be possible to avoid storing the closure frames for
// handles to anonymous functions if we can determine that the
// anonymous function has no unbound variables (or parameters, which
// could be handles to nested functions?) or if it is not created in a
// context that contains nested functions?
//
// Would it be possible to define anonymous functions as a special
// type of nested function object that also has an variable
// initialization list associated with it?

class base_anonymous_fcn_handle : public base_fcn_handle
{
public:

  static const std::string anonymous;

  // Setting NAME here is a bit of a kluge to cope with a bad choice
  // made to append the number of local variables to the @<anonymous>
  // tag in the binary file format.  See also the save_binary and
  // load_binary functions.

  base_anonymous_fcn_handle (const std::string& name = "")
    : base_fcn_handle (name)
  { }

  base_anonymous_fcn_handle (const octave_value& fcn,
                             const stack_frame::local_vars_map& local_vars)
    : base_fcn_handle (anonymous), m_fcn (fcn), m_local_vars (local_vars)
  { }

  base_anonymous_fcn_handle (const base_anonymous_fcn_handle&) = default;

  ~base_anonymous_fcn_handle (void) = default;

  std::string type (void) const { return "anonymous"; }

  bool is_anonymous (void) const { return true; }

  // FIXME: These must go away.  They don't do the right thing for
  // scoping or overloads.
  octave_function * function_value (bool = false)
  {
    return m_fcn.function_value ();
  }

  octave_user_function * user_function_value (bool = false)
  {
    return m_fcn.user_function_value ();
  }

  octave_value fcn_val (void) { return m_fcn; }

  virtual octave_value workspace (void) const = 0;

  // Should be const.
  octave_scalar_map info (void);

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap, mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id& group_hid, octave_hdf5_id& space_hid,
                  octave_hdf5_id& type_hid);

  void print_raw (std::ostream&, bool pr_as_read_syntax,
                  int current_print_indent_level) const;

  // Anonymous function handles are printed without a newline.
  bool print_as_scalar (void) const { return false; }

  bool parse (const std::string& fcn_text);

protected:

  // The function we are handling.
  octave_value m_fcn;

  // List of captured variable values for anonymous fucntions.
  stack_frame::local_vars_map m_local_vars;
};

class anonymous_fcn_handle : public base_anonymous_fcn_handle
{
public:

  using base_anonymous_fcn_handle::anonymous;

  // Setting NAME here is a bit of a kluge to cope with a bad choice
  // made to append the number of local variables to the @<anonymous>
  // tag in the binary file format.  See also the save_binary and
  // load_binary functions.

  anonymous_fcn_handle (const std::string& name = "")
    : base_anonymous_fcn_handle (name), m_stack_context ()
  { }

  anonymous_fcn_handle (const octave_value& fcn,
                        const stack_frame::local_vars_map& local_vars,
                        const std::shared_ptr<stack_frame>& stack_context = std::shared_ptr<stack_frame> ());

  anonymous_fcn_handle (const anonymous_fcn_handle&) = default;

  ~anonymous_fcn_handle (void) = default;

  anonymous_fcn_handle * clone (void) const
  {
    return new anonymous_fcn_handle (*this);
  }

  octave_value make_weak_anonymous_handle (void) const;

  octave_value_list call (int nargout, const octave_value_list& args);

  octave_value workspace (void) const;

  friend bool is_equal_to (const anonymous_fcn_handle& fh1,
                           const anonymous_fcn_handle& fh2);

  std::shared_ptr<stack_frame> stack_context (void) const
  {
    return m_stack_context;
  }

protected:

  // Pointer to closure stack frames.
  std::shared_ptr<stack_frame> m_stack_context;
};

class weak_anonymous_fcn_handle : public base_anonymous_fcn_handle
{
public:

  using base_anonymous_fcn_handle::anonymous;

  weak_anonymous_fcn_handle (const anonymous_fcn_handle& afh)
    : base_anonymous_fcn_handle (afh), m_stack_context (afh.stack_context ())
  { }

  weak_anonymous_fcn_handle (const weak_anonymous_fcn_handle&) = default;

  ~weak_anonymous_fcn_handle (void) = default;

  weak_anonymous_fcn_handle * clone (void) const
  {
    return new weak_anonymous_fcn_handle (*this);
  }

  bool is_weak_anonymous (void) const { return true; }

  octave_value_list call (int nargout, const octave_value_list& args);

  octave_value workspace (void) const;

  friend bool is_equal_to (const weak_anonymous_fcn_handle& fh1,
                           const weak_anonymous_fcn_handle& fh2);

protected:

  // Pointer to closure stack frames.
  std::weak_ptr<stack_frame> m_stack_context;
};

extern bool is_equal_to (const anonymous_fcn_handle& fh1,
                         const anonymous_fcn_handle& fh2);

static void err_invalid_fcn_handle (const std::string& name)
{
  error ("invalid function handle, unable to find function for @%s",
         name.c_str ());
}

octave_value base_fcn_handle::make_weak_nested_handle (void) const
{
  std::string type_str = type ();
  error ("invalid conversion from %s handle to weak nestead handle",
         type_str.c_str ());
}

octave_value base_fcn_handle::make_weak_anonymous_handle (void) const
{
  std::string type_str = type ();
  error ("invalid conversion from %s handle to weak anonymous handle",
         type_str.c_str ());
}

octave_value_list
base_fcn_handle::subsref (const std::string& type,
                          const std::list<octave_value_list>& idx,
                          int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
        int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

        retval = call (tmp_nargout, idx.front ());
      }
      break;

    case '{':
    case '.':
      error ("function handle cannot be indexed with %c", type[0]);

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

octave_value
base_fcn_handle::convert_to_str_internal (bool, bool, char type) const
{
  std::ostringstream buf;
  print_raw (buf, true, 0);
  return octave_value (buf.str (), type);
}

bool
base_fcn_handle::save_ascii (std::ostream&)
{
  unimplemented ("save", "text");

  return true;
}

bool
base_fcn_handle::load_ascii (std::istream&)
{
  unimplemented ("load", "text");

  return true;
}

bool
base_fcn_handle::save_binary (std::ostream&, bool)
{
  unimplemented ("save", "binary");

  return true;
}

bool
base_fcn_handle::load_binary (std::istream&, bool, mach_info::float_format)
{
  unimplemented ("load", "binary");

  return true;
}

bool
base_fcn_handle::save_hdf5 (octave_hdf5_id, const char *, bool)
{
  unimplemented ("save", "hdf5");

  return true;
}

bool
base_fcn_handle::load_hdf5 (octave_hdf5_id&, octave_hdf5_id&, octave_hdf5_id&)
{
  unimplemented ("load", "hdf5");

  return true;
}

void base_fcn_handle::warn_load (const char *file_type) const
{
  std::string obj_type = type ();

  warning_with_id
  ("Octave:load-save-unavailable",
   "%s: loading %s files not available in this version of Octave",
   obj_type.c_str (), file_type);
}

void base_fcn_handle::warn_save (const char *file_type) const
{
  std::string obj_type = type ();

  warning_with_id
  ("Octave:load-save-unavailable",
   "%s: saving %s files not available in this version of Octave",
   obj_type.c_str (), file_type);
}

void base_fcn_handle::unimplemented (const char *op, const char *fmt) const
{
  std::string htype = type ();

  warning ("%s for %s handles with %s format is not implemented",
           op, htype.c_str (), fmt);
}

octave_value_list
invalid_fcn_handle::call (int, const octave_value_list&)
{
  error ("invalid call to invalid function handle");
}

octave_value_list
internal_fcn_handle::call (int nargout, const octave_value_list& args)
{
  interpreter& interp = __get_interpreter__ ();

  return interp.feval (m_fcn, args, nargout);
}

octave_scalar_map internal_fcn_handle::info (void)
{
  octave_scalar_map m;

  m.setfield ("function", fcn_name ());
  m.setfield ("type", type ());
  m.setfield ("file", "");

  return m;
}

bool is_equal_to (const internal_fcn_handle& fh1,
                  const internal_fcn_handle& fh2)
{
  if (fh1.m_name == fh2.m_name
      && fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

octave_value_list
simple_fcn_handle::call (int nargout, const octave_value_list& args)
{
  // FIXME: if m_name has a '.' in the name, lookup first component.  If
  // it is a classdef meta object, then build TYPE and IDX arguments and
  // make a subsref call using them.

  interpreter& interp = __get_interpreter__ ();

  octave_value fcn_to_call;

  // The following code is similar to part of
  // tree_evaluator::visit_index_expression but simpler because it
  // handles a more restricted case.

  symbol_table& symtab = interp.get_symbol_table ();

  std::size_t pos = m_name.find ('.');

  if (pos != std::string::npos)
    {
      // FIXME: check to see which of these cases actually work in
      // Octave and Matlab.  For the last two, assume handle is
      // created before object is defined as an object.
      //
      // We can have one of
      //
      //   pkg-list . fcn  (args)
      //   pkg-list . cls . meth (args)
      //   class-name . method  (args)
      //   class-name . static-method  (args)
      //   object . method  (args)
      //   object . static-method  (args)

      // Evaluate package elements until we find a function,
      // classdef object, or classdef_meta object that is not a
      // package.  An object may only appear as the first element,
      // then it must be followed directly by a function name.

      std::size_t beg = 0;
      std::size_t end = pos;

      std::vector<std::string> idx_elts;

      while (true)
        {
          end = m_name.find ('.', beg);

          idx_elts.push_back (m_name.substr (beg, end-beg));

          if (end == std::string::npos)
            break;

          beg = end+1;
        }

      std::size_t n_elts = idx_elts.size ();

      bool have_object = false;
      octave_value partial_expr_val;

      // Lazy evaluation.  The first element was not known to be defined
      // as an object in the scope where the handle was created.  See if
      // there is a definition in the current scope.

      partial_expr_val = interp.varval (idx_elts[0]);

      if (partial_expr_val.is_defined ())
        {
          if (! partial_expr_val.is_classdef_object () || n_elts != 2)
            err_invalid_fcn_handle (m_name);

          have_object = true;
        }
      else
        partial_expr_val = symtab.find_function (idx_elts[0], ovl ());

      std::string type;
      std::list<octave_value_list> arg_list;

      for (std::size_t i = 1; i < n_elts; i++)
        {
          if (partial_expr_val.is_package ())
            {
              if (have_object)
                err_invalid_fcn_handle (m_name);

              type = ".";
              arg_list.push_back (ovl (idx_elts[i]));

              try
                {
                  // Silently ignore extra output values.

                  octave_value_list tmp_list
                    = partial_expr_val.subsref (type, arg_list, 0);

                  partial_expr_val
                    = tmp_list.length () ? tmp_list(0) : octave_value ();

                  if (partial_expr_val.is_cs_list ())
                    err_invalid_fcn_handle (m_name);

                  arg_list.clear ();
                }
              catch (const index_exception&)
                {
                  err_invalid_fcn_handle (m_name);
                }
            }
          else if (have_object || partial_expr_val.is_classdef_meta ())
            {
              // Object or class name must be the next to the last
              // element (it was the previous one, so if this is the
              // final element, it should be a classdef method,
              // but we'll let the classdef or classdef_meta subsref
              // function sort that out.

              if (i != n_elts-1)
                err_invalid_fcn_handle (m_name);

              type = ".(";
              arg_list.push_back (ovl (idx_elts[i]));
              arg_list.push_back (args);

              return partial_expr_val.subsref (type, arg_list, nargout);
            }
          else
            err_invalid_fcn_handle (m_name);
        }

      // If we get here, we must have a function to call.

      if (! partial_expr_val.is_function ())
        err_invalid_fcn_handle (m_name);

      fcn_to_call = partial_expr_val;
    }
  else
    {
      // No "." in the name.

      // Perform function lookup given current arguments.  We'll need
      // to do this regardless of whether a function was found when
      // the handle was created.

      octave_value ov_fcn = symtab.find_function (m_name, args);

      if (m_fcn.is_defined ())
        {
          // A simple function was found when the handle was created.
          // Use that unless we find a class method to override it.

          fcn_to_call = m_fcn;

          if (ov_fcn.is_defined ())
            {
              octave_function *fcn = ov_fcn.function_value ();

              std::string dispatch_class = fcn->dispatch_class ();

              if (fcn->is_class_method ())
                {
                  // Function found through lookup is a class method
                  // so use it instead of the simple one found when
                  // the handle was created.

                  fcn_to_call = ov_fcn;
                }
            }
        }
      else
        {
          // There was no simple function found when the handle was
          // created so use the one found here (if any).

          fcn_to_call = ov_fcn;
        }
    }

  if (! fcn_to_call.is_defined ())
    err_invalid_fcn_handle (m_name);

  return interp.feval (fcn_to_call, args, nargout);
}

octave_function *simple_fcn_handle::function_value (bool)
{
  // FIXME: Shouldn't the lookup rules here match those used in the
  // call method?

  if (m_fcn.is_defined ())
    return m_fcn.function_value ();

  symbol_table& symtab = __get_symbol_table__ ();

  // FIXME: is caching the correct thing to do?
  // Cache this value so that the pointer will be valid as long as the
  // function handle object is valid.

  m_fcn = symtab.find_function (m_name, octave_value_list ());

  return m_fcn.is_defined () ? m_fcn.function_value () : nullptr;
}

octave_user_function *simple_fcn_handle::user_function_value (bool)
{
  // FIXME: Shouldn't the lookup rules here match those used in the
  // call method?

  if (m_fcn.is_defined ())
    return m_fcn.user_function_value ();

  symbol_table& symtab = __get_symbol_table__ ();

  // FIXME: is caching the correct thing to do?
  // Cache this value so that the pointer will be valid as long as the
  // function handle object is valid.

  m_fcn = symtab.find_user_function (m_name);

  return m_fcn.is_defined () ? m_fcn.user_function_value () : nullptr;
}

octave_value simple_fcn_handle::fcn_val (void)
{
  if (m_fcn.is_defined ())
    return m_fcn;

  symbol_table& symtab = __get_symbol_table__ ();

  // FIXME: is caching the correct thing to do?
  // Cache this value so that the pointer will be valid as long as the
  // function handle object is valid.

  m_fcn = symtab.find_user_function (m_name);

  return m_fcn;
}

octave_scalar_map simple_fcn_handle::info (void)
{
  octave_scalar_map m;

  m.setfield ("function", fcn_name ());
  m.setfield ("type", type ());
  // When is FILE defined for simple function handles?
  m.setfield ("file", file ());

  return m;
}

bool simple_fcn_handle::save_ascii (std::ostream& os)
{
  os << "# octaveroot: " << config::octave_exec_home () << "\n";

  std::string fnm = file ();
  if (! fnm.empty ())
    os << "# path: " << fnm << "\n";

  os << "# subtype: " << type () << "\n";

  os << m_name << "\n";

  return true;
}

bool simple_fcn_handle::load_ascii (std::istream& is)
{
  // FIXME: If m_file is not empty, try to load the file and define
  // the function?  Is it an error if that fails?  Or should this job
  // always be deferred until the handle is used?

  return is.good ();
}

bool simple_fcn_handle::save_binary (std::ostream& os, bool)
{
  std::ostringstream nmbuf;

  // When is FILE defined for simple function handles?
  std::string fnm;

  nmbuf << m_name << "@<simple>\n" << config::octave_exec_home ()
        << "\n" << fnm;

  std::string buf_str = nmbuf.str ();
  int32_t tmp = buf_str.length ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  os.write (buf_str.c_str (), buf_str.length ());

  return true;
}

bool simple_fcn_handle::load_binary (std::istream& is, bool,
                                     mach_info::float_format)
{
  return is.good ();
}

bool simple_fcn_handle::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                   bool)
{
#if defined (HAVE_HDF5)

  bool retval = true;

  octave_hdf5_id group_hid = -1;
#if defined (HAVE_HDF5_18)
  group_hid = H5Gcreate (loc_id, name, octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                         octave_H5P_DEFAULT);
#else
  group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    return false;

  octave_hdf5_id space_hid, data_hid, type_hid;
  space_hid = data_hid = type_hid = -1;

  // attach the type of the variable
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, m_name.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, 2);
  hdims[0] = 0;
  hdims[1] = 0;
  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0
      || H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, m_name.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  std::string octaveroot = config::octave_exec_home ();

  // When is FILE defined for simple fucntion handles?
  std::string fpath;

  H5Sclose (space_hid);
  hdims[0] = 1;
  hdims[1] = octaveroot.length ();
  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Tclose (type_hid);
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, octaveroot.length () + 1);
  octave_hdf5_id a_id;
#if defined (HAVE_HDF5_18)
  a_id = H5Acreate (group_hid, "OCTAVEROOT", type_hid, space_hid,
                    octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
  a_id = H5Acreate (group_hid, "OCTAVEROOT", type_hid, space_hid,
                    octave_H5P_DEFAULT);
#endif

  if (a_id >= 0)
    {
      retval = (H5Awrite (a_id, type_hid, octaveroot.c_str ()) >= 0);

      H5Aclose (a_id);
    }
  else
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  hdims[0] = 1;
  hdims[1] = fpath.length ();
  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Tclose (type_hid);
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, fpath.length () + 1);

#if defined (HAVE_HDF5_18)
  a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid,
                    octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
  a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid,
                    octave_H5P_DEFAULT);
#endif

  if (a_id >= 0)
    {
      retval = (H5Awrite (a_id, type_hid, fpath.c_str ()) >= 0);

      H5Aclose (a_id);
    }
  else
    retval = false;

  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");

  return false;

#endif
}

bool simple_fcn_handle::load_hdf5 (octave_hdf5_id& group_hid,
                                   octave_hdf5_id& space_hid,
                                   octave_hdf5_id& type_hid)
{
#if defined (HAVE_HDF5)

  unimplemented ("load", "hdf5");

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return true;

#else

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return false;

#endif
}

void simple_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax,
                                   int current_print_indent_level) const
{
  octave_print_internal (os, '@' + m_name, pr_as_read_syntax,
                         current_print_indent_level);
}

bool is_equal_to (const simple_fcn_handle& fh1, const simple_fcn_handle& fh2)
{
  if (fh1.m_name == fh2.m_name)
    {
      if (fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
        return fh1.m_fcn.is_copy_of (fh2.m_fcn);

      if (fh1.m_fcn.is_undefined () && fh2.m_fcn.is_undefined ())
        return true;
    }

  return false;
}

scoped_fcn_handle::scoped_fcn_handle (const octave_value& fcn,
                                      const std::string& name,
                                      const std::list<std::string>& parentage)
  : base_fcn_handle (name), m_fcn (fcn), m_parentage (parentage)
{
  // FIXME: should it be an error if FCN is undefined?

  if (m_fcn.is_defined ())
    {
      octave_function *oct_fcn = m_fcn.function_value ();

      if (oct_fcn)
        m_file = oct_fcn->fcn_file_name ();
    }

  m_parentage.push_front (name);
}

octave_value_list
scoped_fcn_handle::call (int nargout, const octave_value_list& args)
{
  // FIXME: we aren't really using the scope yet.  Hmm.

  interpreter& interp = __get_interpreter__ ();

  if (! m_fcn.is_defined ())
    {
      // Try to find it?

      find_function ();
    }

  if (! m_fcn.is_defined ())
    err_invalid_fcn_handle (m_name);

  return interp.feval (m_fcn, args, nargout);
}

octave_scalar_map scoped_fcn_handle::info (void)
{
  octave_scalar_map m;

  m.setfield ("function", fcn_name ());
  m.setfield ("type", type ());
  m.setfield ("file", file ());

  m.setfield ("parentage", Cell (m_parentage));

  return m;
}

bool scoped_fcn_handle::save_ascii (std::ostream& os)
{
  os << "# octaveroot: " << config::octave_exec_home () << "\n";

  std::string fnm = file ();
  if (! fnm.empty ())
    os << "# path: " << fnm << "\n";

  os << "# subtype: " << type () << "\n";

  os << m_name << "\n";

  octave_value tmp = Cell (m_parentage);
  tmp.save_ascii (os);

  return os.good ();
}

bool scoped_fcn_handle::load_ascii (std::istream& is)
{
  octave_cell ov_cell;
  ov_cell.load_ascii (is);

  if (ov_cell.iscellstr ())
    {
      Array<std::string> cellstr_val = ov_cell.cellstr_value ();

      for (octave_idx_type i = 0; i < cellstr_val.numel (); i++)
        m_parentage.push_back (cellstr_val(i));
    }

  return is.good ();
}

bool scoped_fcn_handle::save_binary (std::ostream& os, bool save_as_floats)
{
  std::ostringstream nmbuf;

  std::string fnm = file ();

  nmbuf << m_name << "@<scopedfunction>\n" << config::octave_exec_home ()
        << "\n" << fnm;

  std::string buf_str = nmbuf.str ();
  int32_t len = buf_str.length ();
  os.write (reinterpret_cast<char *> (&len), 4);
  os.write (buf_str.c_str (), buf_str.length ());

  octave_value tmp = Cell (m_parentage);
  tmp.save_binary (os, save_as_floats);

  return os.good ();
}

bool scoped_fcn_handle::load_binary (std::istream& is, bool swap,
                                     mach_info::float_format fmt)
{
  octave_cell ov_cell;
  ov_cell.load_binary (is, swap, fmt);

  if (ov_cell.iscellstr ())
    {
      Array<std::string> cellstr_val = ov_cell.cellstr_value ();

      for (octave_idx_type i = 0; i < cellstr_val.numel (); i++)
        m_parentage.push_back (cellstr_val(i));
    }

  return is.good ();
}

bool scoped_fcn_handle::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                   bool)
{
#if defined (HAVE_HDF5)

  unimplemented ("save", "hdf5");

  // FIXME: save parentage.

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  return true;

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");

  return false;

#endif
}

bool scoped_fcn_handle::load_hdf5 (octave_hdf5_id& group_hid,
                                   octave_hdf5_id& space_hid,
                                   octave_hdf5_id& type_hid)
{
#if defined (HAVE_HDF5)

  unimplemented ("load", "hdf5");

  // FIXME: load parentage.

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return true;

#else

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return false;

#endif
}

void scoped_fcn_handle::print_raw (std::ostream& os,
                                   bool pr_as_read_syntax,
                                   int current_print_indent_level) const
{
  octave_print_internal (os, '@' + m_name, pr_as_read_syntax,
                         current_print_indent_level);
}

bool is_equal_to (const scoped_fcn_handle& fh1, const scoped_fcn_handle& fh2)
{
  if (fh1.m_name == fh2.m_name
      && fh2.m_parentage == fh2.m_parentage
      && fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

void scoped_fcn_handle::find_function (void)
{
  // Since a scoped function is not visible by itself, try to load the
  // file named in m_file then find and define the scoped function.
  // It is not an error if this fails.  We can report later that the
  // handle is invalid.

  symbol_table& symtab = __get_symbol_table__ ();

  if (m_parentage.size () == 1)
    {
      std::string dir_name = sys::file_ops::dirname (m_file);

      std::size_t pos = dir_name.find_last_of (sys::file_ops::dir_sep_chars ());

      if (pos != std::string::npos)
        dir_name = dir_name.substr (0, pos);
      else if (dir_name == "private")
        dir_name = ".";

      std::string fcn_name = m_parentage.front ();

      // FIXME: Does dir_name need to be in the load path for this to work?

      m_fcn = symtab.find_private_function (dir_name, m_name);

      // FIXME: Verify that it is a private function?
    }
  else
    {
      std::string primary_parent_name = m_parentage.back ();

      octave_value ov_parent_fcn
        = symtab.find_user_function (primary_parent_name);

      if (ov_parent_fcn.is_defined ())
        {
          octave_user_function *fcn = ov_parent_fcn.user_function_value ();

          if (fcn)
            {
              std::string file_name = fcn->fcn_file_name ();

              std::string oct_home = config::octave_exec_home ();

              if (file_name.substr (0, oct_home.size ()) == oct_home)
                file_name = file_name.substr (oct_home.size ());

              octave_value subfcn = fcn->find_subfunction (m_name);

              if (subfcn.is_defined ())
                m_fcn = subfcn;
            }
        }
    }
}

octave_scalar_map base_nested_fcn_handle::info (void)
{
  octave_scalar_map m;

  m.setfield ("function", fcn_name ());
  m.setfield ("type", type ());
  m.setfield ("file", "");
  m.setfield ("workspace", workspace ());

  return m;
}

// FIXME: For save, we need a way to save the (possibly shared)
// workspace.  For load, we need a way to load and link to the
// (possibly shared) workspace that was saved.
//
// Since a nested function is not visible by itself, do we need to try
// to load the file named in m_file then find and define the function?
// Is it an error if that fails?  Or should this job always be
// deferred until the handle is used?

bool base_nested_fcn_handle::save_ascii (std::ostream& os)
{
  unimplemented ("save", "text");

  octave_unused_parameter (os);

  return true;
}

bool base_nested_fcn_handle::load_ascii (std::istream& is)
{
  unimplemented ("load", "text");

  octave_unused_parameter (is);

  return true;
}

bool base_nested_fcn_handle::save_binary (std::ostream& os,
    bool save_as_floats)
{
  unimplemented ("save", "binary");

  octave_unused_parameter (os);
  octave_unused_parameter (save_as_floats);

  return true;
}

bool base_nested_fcn_handle::load_binary (std::istream& is, bool swap,
    mach_info::float_format fmt)
{
  unimplemented ("load", "binary");

  octave_unused_parameter (is);
  octave_unused_parameter (swap);
  octave_unused_parameter (fmt);

  return true;
}

bool base_nested_fcn_handle::save_hdf5 (octave_hdf5_id loc_id,
                                        const char *name, bool)
{
#if defined (HAVE_HDF5)

  unimplemented ("save", "hdf5");

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  return true;

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");

  return false;

#endif
}

bool base_nested_fcn_handle::load_hdf5 (octave_hdf5_id& group_hid,
                                        octave_hdf5_id& space_hid,
                                        octave_hdf5_id& type_hid)
{
#if defined (HAVE_HDF5)

  unimplemented ("load", "hdf5");

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return true;

#else

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return false;

#endif
}

void base_nested_fcn_handle::print_raw (std::ostream& os,
                                        bool pr_as_read_syntax,
                                        int current_print_indent_level) const
{
  octave_print_internal (os, '@' + m_name, pr_as_read_syntax,
                         current_print_indent_level);
}

octave_value nested_fcn_handle::make_weak_nested_handle (void) const
{
  return octave_value (new octave_fcn_handle
                       (new weak_nested_fcn_handle (*this)));
}

octave_value_list
nested_fcn_handle::call (int nargout, const octave_value_list& args)
{
  tree_evaluator& tw = __get_evaluator__ ();

  octave_user_function *oct_usr_fcn = m_fcn.user_function_value ();

  tw.push_stack_frame (oct_usr_fcn, m_stack_context);

  unwind_action act ([&tw] () { tw.pop_stack_frame (); });

  return oct_usr_fcn->execute (tw, nargout, args);
}

octave_value nested_fcn_handle::workspace (void) const
{
  return m_stack_context->workspace ();
}

bool is_equal_to (const nested_fcn_handle& fh1, const nested_fcn_handle& fh2)
{
  if (fh1.m_name == fh2.m_name
      && fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

octave_value_list
weak_nested_fcn_handle::call (int nargout, const octave_value_list& args)
{
  tree_evaluator& tw = __get_evaluator__ ();

  octave_user_function *oct_usr_fcn = m_fcn.user_function_value ();

  std::shared_ptr<stack_frame> frames = m_stack_context.lock ();

  tw.push_stack_frame (oct_usr_fcn, frames);

  unwind_action act ([&tw] () { tw.pop_stack_frame (); });

  return oct_usr_fcn->execute (tw, nargout, args);
}

octave_value weak_nested_fcn_handle::workspace (void) const
{
  std::shared_ptr<stack_frame> frames = m_stack_context.lock ();

  return frames ? frames->workspace () : octave_value ();
}

bool is_equal_to (const weak_nested_fcn_handle& fh1,
                  const weak_nested_fcn_handle& fh2)
{
  if (fh1.m_name == fh2.m_name
      && fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

class_simple_fcn_handle::class_simple_fcn_handle (const std::string& class_nm,
    const std::string& meth_nm)
  : base_fcn_handle (meth_nm), m_obj (), m_fcn (),
    m_dispatch_class (class_nm)
{ }

class_simple_fcn_handle::class_simple_fcn_handle (const octave_value& fcn,
    const std::string& class_nm,
    const std::string& meth_nm)
  : base_fcn_handle (meth_nm), m_obj (), m_fcn (fcn),
    m_dispatch_class (class_nm)
{ }

class_simple_fcn_handle::class_simple_fcn_handle (const octave_value& obj,
    const octave_value& fcn,
    const std::string& class_nm,
    const std::string& meth_nm)
  : base_fcn_handle (meth_nm), m_obj (obj), m_fcn (fcn),
    m_dispatch_class (class_nm)
{ }

octave_value_list
class_simple_fcn_handle::call (int nargout, const octave_value_list& args)
{
  interpreter& interp = __get_interpreter__ ();

  if (m_obj.is_defined ())
    {
      octave_value_list tmp_args = args;
      tmp_args.prepend (m_obj);

      return interp.feval (m_fcn, tmp_args, nargout);
    }

  // FIXME: is this the best approach?  Should we be saving current
  // dispatch class and restoring that value instead of
  // unconditionally setting it to "" when we return from this
  // function?

  tree_evaluator& tw = interp.get_evaluator ();

  unwind_action act ([&tw] () { tw.set_dispatch_class (""); });

  tw.set_dispatch_class (m_dispatch_class);

  if (m_fcn.is_defined ())
    return interp.feval (m_fcn, args, nargout);

  return interp.feval (fcn_name (), args, nargout);
}

octave_scalar_map class_simple_fcn_handle::info (void)
{
  octave_scalar_map m;

  m.setfield ("function", fcn_name ());
  m.setfield ("type", type ());
  m.setfield ("file", "");
  m.setfield ("class", dispatch_class ());

  return m;
}

// FIXME: Since a class method is not visible by itself, do we need to
// try to load the file named in m_file then find and define the
// function?  Is it an error if that fails?  Or should this job always
// be deferred until the handle is used?

bool class_simple_fcn_handle::save_ascii (std::ostream& os)
{
  unimplemented ("save", "text");

  octave_unused_parameter (os);

  return true;
}

bool class_simple_fcn_handle::load_ascii (std::istream& is)
{
  unimplemented ("load", "text");

  octave_unused_parameter (is);

  return true;
}

bool class_simple_fcn_handle::save_binary (std::ostream& os,
    bool save_as_floats)
{
  unimplemented ("save", "binary");

  octave_unused_parameter (os);
  octave_unused_parameter (save_as_floats);

  return true;
}

bool class_simple_fcn_handle::load_binary (std::istream& is, bool swap,
    mach_info::float_format fmt)
{
  unimplemented ("load", "binary");

  octave_unused_parameter (is);
  octave_unused_parameter (swap);
  octave_unused_parameter (fmt);

  return true;
}

bool class_simple_fcn_handle::save_hdf5 (octave_hdf5_id loc_id,
    const char *name, bool)
{
#if defined (HAVE_HDF5)

  unimplemented ("save", "hdf5");

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  return true;

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");

  return false;

#endif
}

bool class_simple_fcn_handle::load_hdf5 (octave_hdf5_id& group_hid,
    octave_hdf5_id& space_hid,
    octave_hdf5_id& type_hid)
{
#if defined (HAVE_HDF5)

  unimplemented ("load", "hdf5");

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return true;

#else

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return false;

#endif
}

void class_simple_fcn_handle::print_raw (std::ostream& os,
    bool pr_as_read_syntax,
    int current_print_indent_level) const
{
  octave_print_internal (os, '@' + m_name, pr_as_read_syntax,
                         current_print_indent_level);
}

bool is_equal_to (const class_simple_fcn_handle& fh1,
                  const class_simple_fcn_handle& fh2)
{
  // FIXME: Also need to check object values are equivalent?

  if (fh1.m_name == fh2.m_name
      && fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

const std::string base_anonymous_fcn_handle::anonymous ("@<anonymous>");

octave_scalar_map base_anonymous_fcn_handle::info (void)
{
  octave_scalar_map m;

  std::ostringstream buf;
  print_raw (buf, true, 0);
  m.setfield ("function", buf.str ());

  m.setfield ("type", type ());
  m.setfield ("file", "");
  m.setfield ("workspace", workspace ());
  m.setfield ("within_file_path", "");

  return m;
}

bool base_anonymous_fcn_handle::save_ascii (std::ostream& os)
{
  // FIXME: can we ensure that m_fcn is always defined?

  if (m_fcn.is_undefined ())
    return false;

  os << m_name << "\n";

  print_raw (os, true, 0);
  os << "\n";

  std::size_t varlen = m_local_vars.size ();

  if (varlen > 0)
    {
      os << "# length: " << varlen << "\n";

      for (const auto& nm_val : m_local_vars)
        {
          if (! save_text_data (os, nm_val.second, nm_val.first, false, 0))
            return ! os.fail ();
        }
    }

  return true;
}

bool base_anonymous_fcn_handle::load_ascii (std::istream& is)
{
  octave::skip_preceeding_newline (is);

  std::string buf;

  if (is)
    {
      // Get a line of text whitespace characters included, leaving
      // newline in the stream.

      buf = octave::read_until_newline (is, true);
    }

  std::streampos pos = is.tellg ();

  // Set up temporary scope to use for evaluating the text that
  // defines the anonymous function.

  interpreter& interp = __get_interpreter__ ();

  tree_evaluator& tw = interp.get_evaluator ();

  tw.push_dummy_scope (buf);
  unwind_action_safe restore_scope (&tree_evaluator::pop_scope, &tw);

  octave_idx_type len = 0;

  if (extract_keyword (is, "length", len, true) && len >= 0)
    {
      if (len > 0)
        {
          for (octave_idx_type i = 0; i < len; i++)
            {
              octave_value t2;
              bool dummy;

              std::string name = read_text_data (is, "", dummy, t2, i);

              if (! is)
                error ("load: failed to load anonymous function handle");

              m_local_vars[name] = t2;
            }
        }
    }
  else
    {
      is.seekg (pos);
      is.clear ();
    }

  if (is)
    return parse (buf);

  return false;
}

bool base_anonymous_fcn_handle::save_binary (std::ostream& os,
    bool save_as_floats)
{
  // FIXME: can we ensure that m_fcn is always defined?

  if (m_fcn.is_undefined ())
    return false;

  std::ostringstream nmbuf;

  std::size_t varlen = m_local_vars.size ();

  nmbuf << anonymous;
  if (varlen > 0)
    nmbuf << ' ' << varlen;

  std::string buf_str = nmbuf.str ();
  int32_t tmp = buf_str.length ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  os.write (buf_str.c_str (), buf_str.length ());

  std::ostringstream buf;
  print_raw (buf, true, 0);
  std::string stmp = buf.str ();
  tmp = stmp.length ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  os.write (stmp.c_str (), stmp.length ());

  if (varlen > 0)
    {
      for (const auto& nm_val : m_local_vars)
        {
          if (! save_binary_data (os, nm_val.second, nm_val.first,
                                  "", 0, save_as_floats))
            return ! os.fail ();
        }
    }

  return true;
}

bool base_anonymous_fcn_handle::load_binary (std::istream& is, bool swap,
    mach_info::float_format fmt)
{
  // Read extra characters in m_name as the number of local variable
  // values in this anonymous function.

  octave_idx_type len = 0;
  std::size_t anl = anonymous.length ();
  if (m_name.length () > anl)
    {
      std::istringstream nm_is (m_name.substr (anl));
      nm_is >> len;

      // Anonymous functions don't have names.  We just used this
      // string as temporary storage to pass the number of local
      // variable values.

      m_name = "";
    }

  int32_t tmp;

  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;
  if (swap)
    swap_bytes<4> (&tmp);

  OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
  // is.get (ctmp2, tmp+1, 0); caused is.eof () to be true though
  // effectively not reading over file end
  is.read (ctmp2, tmp);
  ctmp2[tmp] = 0;

  // Set up temporary scope to use for evaluating the text that
  // defines the anonymous function.

  interpreter& interp = __get_interpreter__ ();

  tree_evaluator& tw = interp.get_evaluator ();

  tw.push_dummy_scope (ctmp2);
  unwind_action_safe restore_scope (&tree_evaluator::pop_scope, &tw);

  if (len > 0)
    {
      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_value t2;
          bool dummy;
          std::string doc;

          std::string name
            = read_binary_data (is, swap, fmt, "", dummy, t2, doc);

          if (! is)
            error ("load: failed to load anonymous function handle");

          m_local_vars[name] = t2;
        }
    }

  if (is)
    return parse (ctmp2);

  return false;
}

bool base_anonymous_fcn_handle::save_hdf5 (octave_hdf5_id loc_id,
    const char *name,
    bool save_as_floats)
{
#if defined (HAVE_HDF5)

  bool retval = true;

  octave_hdf5_id group_hid = -1;
#if defined (HAVE_HDF5_18)
  group_hid = H5Gcreate (loc_id, name, octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                         octave_H5P_DEFAULT);
#else
  group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    return false;

  octave_hdf5_id space_hid, data_hid, type_hid;
  space_hid = data_hid = type_hid = -1;

  // attach the type of the variable
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, m_name.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, 2);
  hdims[0] = 0;
  hdims[1] = 0;
  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0
      || H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, m_name.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  std::ostringstream buf;
  print_raw (buf, true, 0);
  std::string stmp = buf.str ();

  // attach the type of the variable
  H5Tset_size (type_hid, stmp.length () + 1);
  if (type_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0
      || H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, stmp.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

  std::size_t varlen = m_local_vars.size ();

  if (varlen > 0)
    {
      octave_hdf5_id as_id = H5Screate (H5S_SCALAR);

      if (as_id >= 0)
        {
          octave_hdf5_id a_id;
#if defined (HAVE_HDF5_18)
          a_id = H5Acreate (group_hid, "SYMBOL_TABLE", H5T_NATIVE_IDX, as_id,
                            octave_H5P_DEFAULT, octave_H5P_DEFAULT);

#else
          a_id = H5Acreate (group_hid, "SYMBOL_TABLE", H5T_NATIVE_IDX, as_id,
                            octave_H5P_DEFAULT);
#endif

          if (a_id >= 0)
            {
              retval = (H5Awrite (a_id, H5T_NATIVE_IDX, &varlen) >= 0);

              H5Aclose (a_id);
            }
          else
            retval = false;

          H5Sclose (as_id);
        }
      else
        retval = false;
#if defined (HAVE_HDF5_18)
      data_hid = H5Gcreate (group_hid, "symbol table",
                            octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                            octave_H5P_DEFAULT);
#else
      data_hid = H5Gcreate (group_hid, "symbol table", 0);
#endif
      if (data_hid < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      for (const auto& nm_val : m_local_vars)
        {
          if (! add_hdf5_data (data_hid, nm_val.second, nm_val.first,
                               "", false, save_as_floats))
            break;
        }

      H5Gclose (data_hid);
    }

  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  warn_save ("hdf5");

  return false;

#endif
}

bool base_anonymous_fcn_handle::load_hdf5 (octave_hdf5_id& group_hid,
    octave_hdf5_id& space_hid,
    octave_hdf5_id& type_hid)
{
#if defined (HAVE_HDF5)

  bool success = true;

#if defined (HAVE_HDF5_18)
  octave_hdf5_id data_hid = H5Dopen (group_hid, "fcn", octave_H5P_DEFAULT);
#else
  octave_hdf5_id data_hid = H5Dopen (group_hid, "fcn");
#endif

  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Tclose (type_hid);
  type_hid = H5Dget_type (data_hid);
  octave_hdf5_id type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  int slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, fcn_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  octave_hdf5_id st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, fcn_tmp)
      < 0)
    {
      H5Tclose (st_id);
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);

  octave_idx_type len = 0;

  // we have to pull some shenanigans here to make sure
  // HDF5 doesn't print out all sorts of error messages if we
  // call H5Aopen for a non-existing attribute

  H5E_auto_t err_fcn;
  void *err_fcn_data;

  // turn off error reporting temporarily, but save the error
  // reporting function:
#if defined (HAVE_HDF5_18)
  H5Eget_auto (octave_H5E_DEFAULT, &err_fcn, &err_fcn_data);
  H5Eset_auto (octave_H5E_DEFAULT, nullptr, nullptr);
#else
  H5Eget_auto (&err_fcn, &err_fcn_data);
  H5Eset_auto (nullptr, nullptr);
#endif

  octave_hdf5_id attr_id = H5Aopen_name (group_hid, "SYMBOL_TABLE");

  if (attr_id >= 0)
    {
      if (H5Aread (attr_id, H5T_NATIVE_IDX, &len) < 0)
        success = false;

      H5Aclose (attr_id);
    }

  // restore error reporting:
#if defined (HAVE_HDF5_18)
  H5Eset_auto (octave_H5E_DEFAULT, err_fcn, err_fcn_data);
#else
  H5Eset_auto (err_fcn, err_fcn_data);
#endif

  // Set up temporary scope to use for evaluating the text that
  // defines the anonymous function.

  interpreter& interp = __get_interpreter__ ();

  tree_evaluator& tw = interp.get_evaluator ();

  tw.push_dummy_scope (fcn_tmp);
  unwind_action_safe restore_scope (&tree_evaluator::pop_scope, &tw);

  if (len > 0 && success)
    {
      hsize_t num_obj = 0;
#if defined (HAVE_HDF5_18)
      data_hid = H5Gopen (group_hid, "symbol table", octave_H5P_DEFAULT);
#else
      data_hid = H5Gopen (group_hid, "symbol table");
#endif
      H5Gget_num_objs (data_hid, &num_obj);
      H5Gclose (data_hid);

      if (num_obj != static_cast<hsize_t> (len))
        error ("load: failed to load anonymous function handle");

      hdf5_callback_data dsub;
      int current_item = 0;
      for (octave_idx_type i = 0; i < len; i++)
        {
          if (hdf5_h5g_iterate (group_hid, "symbol table", &current_item,
                                &dsub) <= 0)
            error ("load: failed to load anonymous function handle");

          m_local_vars[dsub.name] = dsub.tc;
        }
    }

  if (success)
    return parse (fcn_tmp);

  return false;

#else

  octave_unused_parameter (group_hid);
  octave_unused_parameter (space_hid);
  octave_unused_parameter (type_hid);

  return false;

#endif
}

void base_anonymous_fcn_handle::print_raw (std::ostream& os, bool, int) const
{
  tree_print_code tpc (os);

  octave_user_function *f = m_fcn.user_function_value ();

  if (! f)
    error ("invalid anonymous function handle");

  os << "@";

  // The parameter list should always be valid for anonymous
  // functions, so we should always call accept for it, and it will
  // print the parens for us.

  tree_parameter_list *p = f->parameter_list ();

  if (p)
    p->accept (tpc);

  os << " ";

  tree_statement_list *b = f->body ();

  panic_if (b->length () != 1);

  tree_statement *s = b->front ();

  if (! s)
    error ("invalid anonymous function handle");

  panic_unless (s->is_expression ());

  tree_expression *e = s->expression ();

  if (! e)
    error ("invalid anonymous function handle");

  tpc.print_fcn_handle_body (e);
}

bool base_anonymous_fcn_handle::parse (const std::string& fcn_text)
{
  // FIXME: If evaluation of the string gives us an anonymous function
  // handle object, then why extract the function and create a new
  // anonymous function object?  Why not just attach the workspace
  // values to the object returned by eval_string?  This code is also is
  // duplicated in read_mat5_binary_element in ls-mat5.cc.

  interpreter& interp = __get_interpreter__ ();

  // Set up temporary scope to use for evaluating the text that defines
  // the anonymous function so that we don't pick up values of random
  // variables that might be in the current scope.

  tree_evaluator& tw = interp.get_evaluator ();
  tw.push_dummy_scope ("read_mat5_binary_element");

  unwind_action act ([&tw] () { tw.pop_scope (); });

  int parse_status;
  octave_value anonymous_fcn_hdl
    = interp.eval_string (fcn_text, true, parse_status);

  if (parse_status != 0)
    return false;

  octave_fcn_handle *fh = anonymous_fcn_hdl.fcn_handle_value ();

  if (! fh)
    return false;

  m_fcn = fh->fcn_val ();

  octave_user_function *uf = m_fcn.user_function_value (true);

  if (uf)
    {
      symbol_scope uf_scope = uf->scope ();

      if (uf_scope)
        uf_scope.cache_name (m_name);
    }

  return true;
}

anonymous_fcn_handle::anonymous_fcn_handle (const octave_value& fcn,
    const stack_frame::local_vars_map& local_vars,
    const std::shared_ptr<stack_frame>& stack_context)
  : base_anonymous_fcn_handle (fcn, local_vars),
    m_stack_context (stack_context)
{
  if (m_stack_context)
    m_stack_context->mark_closure_context ();
}

octave_value anonymous_fcn_handle::make_weak_anonymous_handle (void) const
{
  return octave_value (new octave_fcn_handle
                       (new weak_anonymous_fcn_handle (*this)));
}

octave_value_list
anonymous_fcn_handle::call (int nargout, const octave_value_list& args)
{
  tree_evaluator& tw = __get_evaluator__ ();

  octave_user_function *oct_usr_fcn = m_fcn.user_function_value ();

  tw.push_stack_frame (oct_usr_fcn, m_local_vars, m_stack_context);

  unwind_action act ([&tw] () { tw.pop_stack_frame (); });

  return oct_usr_fcn->execute (tw, nargout, args);
}

octave_value anonymous_fcn_handle::workspace (void) const
{
  octave_scalar_map local_vars_map;

  for (const auto& nm_val : m_local_vars)
    local_vars_map.assign (nm_val.first, nm_val.second);

  // FIXME: it would be more convenient if stack_frame::workspace
  // returned a Cell object directly instead of a Cell in an
  // octave_value object.

  Cell cell_frames;

  if (m_stack_context)
    {
      octave_value ov_frames = m_stack_context->workspace ();
      cell_frames = ov_frames.cell_value ();
    }

  octave_idx_type num_frames = cell_frames.numel ();
  // FIXME: It seems there should be a simple way to concatenate cells...
  Cell retval = Cell (num_frames+1, 1);
  retval(0) = m_local_vars;
  for (octave_idx_type i = 0; i < num_frames; i++)
    retval(i+1) = cell_frames(i);

  return retval;
}

bool is_equal_to (const anonymous_fcn_handle& fh1,
                  const anonymous_fcn_handle& fh2)
{
  if (fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

octave_value_list
weak_anonymous_fcn_handle::call (int nargout, const octave_value_list& args)
{
  tree_evaluator& tw = __get_evaluator__ ();

  octave_user_function *oct_usr_fcn = m_fcn.user_function_value ();

  std::shared_ptr<stack_frame> frames = m_stack_context.lock ();

  tw.push_stack_frame (oct_usr_fcn, m_local_vars, frames);

  unwind_action act ([&tw] () { tw.pop_stack_frame (); });

  return oct_usr_fcn->execute (tw, nargout, args);
}

octave_value weak_anonymous_fcn_handle::workspace (void) const
{
  octave_scalar_map local_vars_map;

  for (const auto& nm_val : m_local_vars)
    local_vars_map.assign (nm_val.first, nm_val.second);

  // FIXME: it would be more convenient if stack_frame::workspace
  // returned a Cell object directly instead of a Cell in an
  // octave_value object.

  std::shared_ptr<stack_frame> frames = m_stack_context.lock ();

  Cell cell_frames;

  if (frames)
    {
      octave_value ov_frames = frames->workspace ();
      cell_frames = ov_frames.cell_value ();
    }

  octave_idx_type num_frames = cell_frames.numel ();

  // FIXME: It seems there should be a simple way to concatenate
  // cells...
  Cell retval = Cell (num_frames+1, 1);
  retval(0) = m_local_vars;
  for (octave_idx_type i = 0; i < num_frames; i++)
    retval(i+1) = cell_frames(i);

  return retval;
}

bool is_equal_to (const weak_anonymous_fcn_handle& fh1,
                  const weak_anonymous_fcn_handle& fh2)
{
  if (fh1.m_name == fh2.m_name
      && fh1.m_fcn.is_defined () && fh2.m_fcn.is_defined ())
    return fh1.m_fcn.is_copy_of (fh2.m_fcn);
  else
    return false;
}

OCTAVE_END_NAMESPACE(octave)

octave_fcn_handle::octave_fcn_handle (void)
  : octave_base_value (), m_rep (new octave::invalid_fcn_handle ())
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& fcn)
  : octave_base_value (), m_rep (new octave::internal_fcn_handle (fcn))
{ }

octave_fcn_handle::octave_fcn_handle (const std::string& name)
  : octave_base_value (), m_rep (new octave::simple_fcn_handle (name))
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& fcn,
                                      const std::string& name)
  : octave_base_value (), m_rep (new octave::simple_fcn_handle (fcn, name))
{ }

octave_fcn_handle::octave_fcn_handle (const std::string& class_nm,
                                      const std::string& meth_nm)
  : octave_base_value (),
    m_rep (new octave::class_simple_fcn_handle (class_nm, meth_nm))
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& fcn,
                                      const std::string& class_nm,
                                      const std::string& meth_nm)
  : octave_base_value (),
    m_rep (new octave::class_simple_fcn_handle (fcn, class_nm, meth_nm))
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& obj,
                                      const octave_value& fcn,
                                      const std::string& class_nm,
                                      const std::string& meth_nm)
  : octave_base_value (),
    m_rep (new octave::class_simple_fcn_handle (obj, fcn, class_nm, meth_nm))
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& fcn,
                                      const std::string& name,
                                      const std::list<std::string>& parentage)
  : octave_base_value (),
    m_rep (new octave::scoped_fcn_handle (fcn, name, parentage))
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& fcn,
                                      const std::string& name,
                                      const std::shared_ptr<octave::stack_frame>& stack_context)
  : octave_base_value (),
    m_rep (new octave::nested_fcn_handle (fcn, name, stack_context))
{ }

octave_fcn_handle::octave_fcn_handle (const octave_value& fcn,
                                      const octave::stack_frame::local_vars_map& local_vars,
                                      const std::shared_ptr<octave::stack_frame>& stack_context)
  : octave_base_value (),
    m_rep (new octave::anonymous_fcn_handle (fcn, local_vars, stack_context))
{ }

octave_fcn_handle::octave_fcn_handle (octave::base_fcn_handle *rep)
  : octave_base_value (), m_rep (rep)
{ }

octave_fcn_handle::octave_fcn_handle (const octave_fcn_handle& fh)
  : octave_base_value (fh)
{
  m_rep.reset (fh.m_rep->clone ());
}

dim_vector
octave_fcn_handle::dims (void) const
{
  static dim_vector dv (1, 1);
  return dv;
}

bool
octave_fcn_handle::save_ascii (std::ostream& os)
{
  return m_rep->save_ascii (os);
}

bool
octave_fcn_handle::load_ascii (std::istream& is)
{
  std::shared_ptr<octave::base_fcn_handle> new_rep;

  // Read enough to detect type then create new rep object and dispatch
  // to finish loading object.

  std::streampos pos = is.tellg ();

  std::string octaveroot = extract_keyword (is, "octaveroot", true);
  if (octaveroot.empty ())
    {
      is.seekg (pos);
      is.clear ();
    }

  pos = is.tellg ();

  std::string fpath = extract_keyword (is, "path", true);
  if (fpath.empty ())
    {
      is.seekg (pos);
      is.clear ();
    }

  if (! (octaveroot.empty () || fpath.empty ()))
    {
      std::size_t len = octaveroot.size ();
      if (octaveroot == fpath.substr (0, len))
        fpath = octave::config::octave_exec_home () + fpath.substr (len);
    }

  pos = is.tellg ();

  std::string subtype = extract_keyword (is, "subtype", true);
  if (subtype.empty ())
    {
      is.seekg (pos);
      is.clear ();

      // We have a legacy file that can contain either an anonymous
      // function or a simple function handle.

      std::string name;
      is >> name;

      if (name == anonymous)
        new_rep.reset (new octave::anonymous_fcn_handle ());
      else
        new_rep.reset (new octave::simple_fcn_handle (name, fpath, octaveroot));
    }
  else
    {
      // Load individual function handle types.

      if (subtype == "simple")
        {
          std::string name;
          is >> name;

          new_rep.reset (new octave::simple_fcn_handle (name, fpath,
                         octaveroot));
        }
      else if (subtype == "scopedfunction")
        {
          std::string name;
          is >> name;

          new_rep.reset (new octave::scoped_fcn_handle (name, fpath,
                         octaveroot));
        }
      else if (subtype == "anonymous")
        new_rep.reset (new octave::anonymous_fcn_handle ());
      else if (subtype == "nested")
        {
          std::string name;
          is >> name;

          new_rep.reset (new octave::nested_fcn_handle (name, fpath,
                         octaveroot));
        }
      else if (subtype == "classsimple")
        {
          std::string name;
          is >> name;

          new_rep.reset (new octave::class_simple_fcn_handle (name, fpath,
                         octaveroot));
        }
    }

  if (! new_rep)
    return false;

  if (! new_rep->load_ascii (is))
    return false;

  m_rep = new_rep;

  return true;
}

bool
octave_fcn_handle::save_binary (std::ostream& os, bool save_as_floats)
{
  return m_rep->save_binary (os, save_as_floats);
}

bool
octave_fcn_handle::load_binary (std::istream& is, bool swap,
                                octave::mach_info::float_format fmt)
{
  // Read enough to detect type then create new rep object and dispatch
  // to finish loading object.

  int32_t tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;
  if (swap)
    swap_bytes<4> (&tmp);

  OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
  // is.get (ctmp1, tmp+1, 0); caused is.eof () to be true though
  // effectively not reading over file end
  is.read (ctmp1, tmp);
  ctmp1[tmp] = 0;
  std::string name (ctmp1);

  if (! is)
    return false;

  std::shared_ptr<octave::base_fcn_handle> new_rep;

  std::size_t anl = anonymous.length ();

  if (name.length () >= anl && name.substr (0, anl) == anonymous)
    {
      // Even with extra info stored in the function name, anonymous
      // functions look the same.  Note that NAME here may have the
      // number of local variables appended.  We decode that inside the
      // load_binary function.

      new_rep.reset (new octave::anonymous_fcn_handle (name));
    }
  else
    {
      // Unpack extra info stored with the function name and load
      // individual function handle types.
      // FIXME: is there a better way?

      std::string octaveroot;
      std::string fpath;
      std::string subtype = "simple";

      if (name.find_first_of ('\n') != std::string::npos)
        {
          std::size_t pos1 = name.find_first_of ('\n');
          std::size_t pos2 = name.find_first_of ('\n', pos1 + 1);
          octaveroot = name.substr (pos1 + 1, pos2 - pos1 - 1);
          fpath = name.substr (pos2 + 1);
          name = name.substr (0, pos1);
        }

      std::size_t pos1 = name.find ('@');
      if (pos1 != std::string::npos)
        {
          if (name[pos1+1] == '<')
            {
              std::size_t pos2 = name.find ('>', pos1 + 2);

              if (pos2 != std::string::npos)
                subtype = name.substr (pos1 + 2, pos2 - pos1 - 2);
            }

          name = name.substr (0, pos1);
        }

      // Anonymous should have been handled above so it is not in the
      // following list.

      if (subtype == "simple")
        new_rep.reset (new octave::simple_fcn_handle (name, fpath, octaveroot));
      else if (subtype == "scopedfunction")
        new_rep.reset (new octave::scoped_fcn_handle (name, fpath, octaveroot));
      else if (subtype == "nested")
        new_rep.reset (new octave::nested_fcn_handle (name, fpath, octaveroot));
      else if (subtype == "classsimple")
        new_rep.reset (new octave::class_simple_fcn_handle (name, fpath,
                       octaveroot));
    }

  if (! new_rep)
    return false;

  if (! new_rep->load_binary (is, swap, fmt))
    return false;

  m_rep = new_rep;

  return true;
}

bool
octave_fcn_handle::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                              bool save_as_floats)
{
  return m_rep->save_hdf5 (loc_id, name, save_as_floats);
}

bool
octave_fcn_handle::load_hdf5 (octave_hdf5_id loc_id, const char *name_arg)
{
#if defined (HAVE_HDF5)

#if defined (HAVE_HDF5_18)
  octave_hdf5_id group_hid = H5Gopen (loc_id, name_arg, octave_H5P_DEFAULT);
#else
  octave_hdf5_id group_hid = H5Gopen (loc_id, name_arg);
#endif
  if (group_hid < 0)
    return false;

#if defined (HAVE_HDF5_18)
  octave_hdf5_id data_hid = H5Dopen (group_hid, "nm", octave_H5P_DEFAULT);
#else
  octave_hdf5_id data_hid = H5Dopen (group_hid, "nm");
#endif

  if (data_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  octave_hdf5_id type_hid = H5Dget_type (data_hid);
  octave_hdf5_id type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  octave_hdf5_id space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  int slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, nm_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  octave_hdf5_id st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, nm_tmp)
      < 0)
    {
      H5Tclose (st_id);
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);

  std::string name (nm_tmp);

  std::shared_ptr<octave::base_fcn_handle> new_rep;

  if (name == anonymous)
    {
      // Even with extra info stored in the function name, anonymous
      // functions look the same.

      new_rep.reset (new octave::anonymous_fcn_handle ());
    }
  else
    {
      // Unpack extra info stored with the function name and load
      // individual function handle types.
      // FIXME: is there a better way?

      std::string octaveroot;
      std::string fpath;
      std::string subtype = "simple";

      if (name.find_first_of ('\n') != std::string::npos)
        {
          std::size_t pos1 = name.find_first_of ('\n');
          std::size_t pos2 = name.find_first_of ('\n', pos1 + 1);
          octaveroot = name.substr (pos1 + 1, pos2 - pos1 - 1);
          fpath = name.substr (pos2 + 1);
          name = name.substr (0, pos1);
        }

      std::size_t pos1 = name.find ('@');
      if (pos1 != std::string::npos)
        {
          if (name[pos1+1] == '<')
            {
              std::size_t pos2 = name.find ('>', pos1 + 2);

              if (pos2 != std::string::npos)
                subtype = name.substr (pos1 + 2, pos2 - pos1 - 2);
            }

          name = name.substr (0, pos1);
        }

      // Anonymous should have been handled above so it is not in the
      // following list.

      if (subtype == "simple")
        new_rep.reset (new octave::simple_fcn_handle (name, fpath, octaveroot));
      else if (subtype == "scopedfunction")
        new_rep.reset (new octave::scoped_fcn_handle (name, fpath, octaveroot));
      else if (subtype == "nested")
        new_rep.reset (new octave::nested_fcn_handle (name, fpath, octaveroot));
      else if (subtype == "classsimple")
        new_rep.reset (new octave::class_simple_fcn_handle (name, fpath,
                       octaveroot));
    }

  bool status = false;

  if (new_rep && new_rep->load_hdf5 (group_hid, space_hid, type_hid))
    {
      m_rep = new_rep;
      status = true;
    }

  // FIXME: manage these with an unwind_action object?

  H5Tclose (type_hid);
  H5Sclose (space_hid);
  H5Gclose (group_hid);

  return status;

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name_arg);

  warn_load ("hdf5");

  return false;

#endif
}

/*
%!test <*33857>
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! hm = @version;
%! hdld = @svd;
%! hbi = @log2;
%! f2 = f;
%! g2 = g;
%! hm2 = hm;
%! hdld2 = hdld;
%! hbi2 = hbi;
%! modes = {"-text", "-binary"};
%! if (isfield (__octave_config_info__, "HAVE_HDF5")
%!     && __octave_config_info__ ("HAVE_HDF5"))
%!   modes(end+1) = "-hdf5";
%! endif
%! for i = 1:numel (modes)
%!   mode = modes{i};
%!   nm = tempname ();
%!   unwind_protect
%!     f2 (1);
%!     save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!     clear f2 g2 hm2 hdld2 hbi2
%!     load (nm);
%!     assert (f (2), f2 (2));
%!     assert (g (2), g2 (2));
%!     assert (g (3), g2 (3));
%!     unlink (nm);
%!     save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endfor
*/

/*
%!function fcn_handle_save_recurse (n, mode, nm, f2, g2, hm2, hdld2, hbi2)
%!  if (n == 0)
%!    save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!  else
%!    fcn_handle_save_recurse (n - 1, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!  endif
%!endfunction
%!function [f2, g2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (n, nm)
%!  if (n == 0)
%!    load (nm);
%!  else
%!    [f2, g2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (n - 1, nm);
%!  endif
%!endfunction

%!test <*35876>
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! hm = @version;
%! hdld = @svd;
%! hbi = @log2;
%! f2 = f;
%! g2 = g;
%! hm2 = hm;
%! hdld2 = hdld;
%! hbi2 = hbi;
%! modes = {"-text", "-binary"};
%! if (isfield (__octave_config_info__, "HAVE_HDF5")
%!     && __octave_config_info__ ("HAVE_HDF5"))
%!   modes(end+1) = "-hdf5";
%! endif
%! for i = 1:numel (modes)
%!   mode = modes{i};
%!   nm = tempname ();
%!   unwind_protect
%!     fcn_handle_save_recurse (2, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!     clear f2 g2 hm2 hdld2 hbi2
%!     [f2, f2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (2, nm);
%!     load (nm);
%!     assert (f (2), f2 (2));
%!     assert (g (2), g2 (2));
%!     assert (g (3), g2 (3));
%!     unlink (nm);
%!     fcn_handle_save_recurse (2, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endfor
*/

void
octave_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  m_rep->print_raw (os, pr_as_read_syntax, current_print_indent_level ());
}

bool
is_equal_to (const octave_fcn_handle& fh1, const octave_fcn_handle& fh2)
{
  // FIXME: Maybe there is a better way?  Possibly by using typeid or
  // typeindex?

  // Don't include invalid_fcn_handle in the list of types to compare.
  // Consider them to be like NaN values so comparisons between any two
  // invalid handles are always false.

  if (fh1.is_internal () && fh2.is_internal ())
    return is_equal_to (*dynamic_cast<octave::internal_fcn_handle *> (fh1.get_rep ()),
                        *dynamic_cast<octave::internal_fcn_handle *> (fh2.get_rep ()));
  else if (fh1.is_simple () && fh2.is_simple ())
    return is_equal_to (*dynamic_cast<octave::simple_fcn_handle *> (fh1.get_rep ()),
                        *dynamic_cast<octave::simple_fcn_handle *> (fh2.get_rep ()));
  else if (fh1.is_scoped () && fh2.is_scoped ())
    return is_equal_to (*dynamic_cast<octave::scoped_fcn_handle *> (fh1.get_rep ()),
                        *dynamic_cast<octave::scoped_fcn_handle *> (fh2.get_rep ()));
  else if (fh1.is_nested () && fh2.is_nested ())
    return is_equal_to (*dynamic_cast<octave::nested_fcn_handle *> (fh1.get_rep ()),
                        *dynamic_cast<octave::nested_fcn_handle *> (fh2.get_rep ()));
  else if (fh1.is_class_simple () && fh2.is_class_simple ())
    return is_equal_to (*dynamic_cast<octave::class_simple_fcn_handle *> (fh1.get_rep ()),
                        *dynamic_cast<octave::class_simple_fcn_handle *> (fh2.get_rep ()));
  else if (fh1.is_anonymous () && fh2.is_anonymous ())
    return is_equal_to (*dynamic_cast<octave::anonymous_fcn_handle *> (fh1.get_rep ()),
                        *dynamic_cast<octave::anonymous_fcn_handle *> (fh2.get_rep ()));
  else
    return false;
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (functions, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{s} =} functions (@var{fcn_handle})
Return a structure containing information about the function handle
@var{fcn_handle}.

The structure @var{s} always contains these three fields:

@table @asis
@item function
The function name.  For an anonymous function (no name) this will be the
actual function definition.

@item type
Type of the function.

@table @asis
@item anonymous
The function is anonymous.

@item private
The function is private.

@item overloaded
The function overloads an existing function.

@item simple
The function is a built-in or m-file function.

@item subfunction
The function is a subfunction within an m-file.
@end table

@item nested
The function is nested.

@item file
The m-file that will be called to perform the function.  This field is empty
for anonymous and built-in functions.
@end table

In addition, some function types may return more information in additional
fields.

@strong{Warning:} @code{functions} is provided for debugging purposes only.
Its behavior may change in the future and programs should not depend on any
particular output format.

@seealso{func2str, str2func}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_fcn_handle *fh = args(
                            0).xfcn_handle_value ("functions: FCN_HANDLE argument must be a function handle object");

  return ovl (fh->info ());
}

DEFUN (func2str, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{str} =} func2str (@var{fcn_handle})
Return a string containing the name of the function referenced by the function
handle @var{fcn_handle}.
@seealso{str2func, functions}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_fcn_handle *fh = args(
                            0).xfcn_handle_value ("func2str: FCN_HANDLE argument must be a function handle object");

  if (! fh)
    error ("func2str: FCN_HANDLE must be a valid function handle");

  octave_value retval;

  std::string fh_nm = fh->fcn_name ();

  if (fh->is_anonymous ())
    {
      std::ostringstream buf;

      fh->print_raw (buf);

      retval = buf.str ();
    }
  else
    retval = fh_nm;

  return retval;
}

DEFMETHOD (str2func, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hfcn} =} str2func (@var{str})
Return a function handle constructed from the string @var{str}.

The input may be the name of a function such as @qcode{"sin"} or a string
defining a function such as @qcode{"@@(x) sin (x + pi)"}.

Programming Note: In most cases it will be better to use anonymous function
syntax and let the Octave parser create the function handle rather than use
@code{str2func}.  For example:

@example
@group
hfcn = @@sin ;
hfcn = @@(x) sin (x + pi) ;
@end group
@end example

@seealso{func2str, functions}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string nm
    = args(0).xstring_value ("str2func: FCN_NAME must be a string");

  if (nm.empty ())
    error ("str2func: invalid function name");

  if (nm[0] == '@')
    {
      // Unlike the anonymous_fcn_handle::parse method, don't set up
      // temporary scope to use for evaluating the text that defines
      // the anonymous function.  Here we want
      //
      //   str2func ("@(args) expr")
      //
      // to behave the same as if
      //
      //   @(args) expr
      //
      // were evaluated in the current scope.

      int parse_status;
      octave_value afh = interp.eval_string (nm, true, parse_status);

      if (parse_status == 0)
        return afh;
    }
  else
    {
      if (nargin == 2)
        warning_with_id ("Octave:str2func-global-argument",
                         "str2func: second argument ignored");

      tree_evaluator& tw = interp.get_evaluator ();

      return tw.make_fcn_handle (nm);
    }

  return ovl ();
}

/*
%!test
%! f = str2func ("<");
%! assert (class (f), "function_handle");
%! assert (func2str (f), "lt");
%! assert (f (1, 2), true);
%! assert (f (2, 1), false);

%!test
%! f = str2func ("@(x) sin (x)");
%! assert (func2str (f), "@(x) sin (x)");
%! assert (f (0:3), sin (0:3));

%!error <FCN_NAME must be a string> str2func ({"sin"})
*/

/*
%!function y = __testrecursionfcn (f, x, n)
%!  if (nargin < 3)
%!    n = 0;
%!  endif
%!  if (n > 2)
%!    y = f (x);
%!  else
%!    n++;
%!    y = __testrecursionfcn (@(x) f (2*x), x, n);
%!  endif
%!endfunction
%!
%!assert (__testrecursionfcn (@(x) x, 1), 8)
*/

DEFUN (is_function_handle, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} is_function_handle (@var{x})
Return true if @var{x} is a function handle.
@seealso{isa, typeinfo, class, functions}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_function_handle ());
}

/*
%!shared fh
%! fh = @(x) x;

%!assert (is_function_handle (fh))
%!assert (! is_function_handle ({fh}))
%!assert (! is_function_handle (1))

%!error is_function_handle ()
%!error is_function_handle (1, 2)
*/

/*
%!test
%! f = @(t) eval ('2*t');
%! assert (f (21), 42);
*/

/*
%!test <*58389>
%! s = "x";
%! a.(s) = [e, pi];
%! f = @(x) a.(s)(x);
%! assert (f(1), e);
%! assert (f(2), pi);
%! assert (f([2,1]), [pi, e]);
*/

/*
%!function r = __f (g, i)
%!  r = g(i);
%!endfunction
%!test
%! x = [1,2;3,4];
%! assert (__f (@(i) x(:,i), 1), [1;3]);
*/

OCTAVE_END_NAMESPACE(octave)
