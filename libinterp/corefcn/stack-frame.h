////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_stack_frame_h)
#define octave_stack_frame_h 1

#include "octave-config.h"

#include <deque>
#include <iosfwd>
#include <list>
#include <map>
#include <memory>
#include <string>

class octave_value;
class octave_value_list;

#include "error.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "syminfo.h"
#include "symscope.h"

// Variable values are stored in the stack_frame objects that make up
// the call_stack.  There are four separate stack_frame objects
// corresponding to the following language elements:
//
//  * user-defined functions
//
//    These are .m files.  They have local variables.
//
//  * scripts
//
//    These are .m files, but not functions.  They access variables,
//    but do not store any values directly.  All values are stored in
//    the stack frame corresponding to the scope in which they are
//    executed.
//
//  * scopes that do not correspond to functions
//
//    This is primarily used by the top-level scope but the
//    interpreter may also create temporary scopes in which to
//    evaluate functions or scripts.
//
// * compiled functions
//
//   These are built-in functions and dynamically-loaded compiled
//   functions (.mex and .oct files) and do not contain variable
//   values of their own.  They are skipped when Octave displays a
//   stack trace.
//
// All stack frames also contain the following data:
//
//  * a reference to the evaluator that contains the frame
//
//    Global variables are now stored in the evaluator and this link
//    gives us immediate access to them.
//
//  * line and column in the source file where the stack frame was created
//
//    These values are used to print stack traces.
//
//  * A pointer to the nearest parent frame that contains variable
//    info (the "static" link)
//
//    A frame that contains variable info may be a user-defined
//    function, script, or scope frame.  This pointer should never
//    point to a compiled function stack frame.
//
//  * A pointer to the nearest lexical parent frame (the "access" link)
//
//    Used to access non-local variables for nested and anonymous
//    functions or as a link to the parent frame in which a script is
//    executed.  This pointer should only point to a parent function
//    stack frame.

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;
class symbol_info_list;
class unwind_protect;

class stack_frame_walker;

class stack_frame
{
public:

  typedef std::map<std::string, octave_value> local_vars_map;

  // Markers indicating the type of a variable.  Values for local
  // variables are stored in the stack frame.  Values for
  // global variables are stored in the tree_evaluator object that
  // contains the stack frame.  Values for persistent variables are
  // stored in the function scope corresponding to the stack frame.

  enum scope_flags
  {
    LOCAL,
    GLOBAL,
    PERSISTENT
  };

  // Index into the list of automatic variables for user-defined
  // function stack frames.

  enum auto_var_type
  {
    ARG_NAMES,
    IGNORED,
    NARGIN,
    NARGOUT,
    SAVED_WARNING_STATES,
    NUM_AUTO_VARS
  };

  stack_frame (void) = delete;

  stack_frame (tree_evaluator& tw, std::size_t index,
               const std::shared_ptr<stack_frame>& parent_link,
               const std::shared_ptr<stack_frame>& static_link,
               const std::shared_ptr<stack_frame>& access_link)
    : m_evaluator (tw), m_is_closure_context (false),
      m_line (-1), m_column (-1), m_index (index),
      m_parent_link (parent_link), m_static_link (static_link),
      m_access_link (access_link), m_dispatch_class ()
  { }

  // Compiled function.
  static stack_frame *
  create (tree_evaluator& tw, octave_function *fcn, std::size_t index,
          const std::shared_ptr<stack_frame>& parent_link,
          const std::shared_ptr<stack_frame>& static_link);

  // Script.
  static stack_frame *
  create (tree_evaluator& tw, octave_user_script *script, std::size_t index,
          const std::shared_ptr<stack_frame>& parent_link,
          const std::shared_ptr<stack_frame>& static_link);

  // User-defined function.
  static stack_frame *
  create (tree_evaluator& tw, octave_user_function *fcn, std::size_t index,
          const std::shared_ptr<stack_frame>& parent_link,
          const std::shared_ptr<stack_frame>& static_link,
          const std::shared_ptr<stack_frame>& access_link = std::shared_ptr<stack_frame> ());

  // Anonymous user-defined function with init vars.
  static stack_frame *
  create (tree_evaluator& tw, octave_user_function *fcn, std::size_t index,
          const std::shared_ptr<stack_frame>& parent_link,
          const std::shared_ptr<stack_frame>& static_link,
          const local_vars_map& local_vars,
          const std::shared_ptr<stack_frame>& access_link = std::shared_ptr<stack_frame> ());

  // Scope.
  static stack_frame *
  create (tree_evaluator& tw, const symbol_scope& scope, std::size_t index,
          const std::shared_ptr<stack_frame>& parent_link,
          const std::shared_ptr<stack_frame>& static_link);

  stack_frame (const stack_frame& elt) = default;

  stack_frame& operator = (const stack_frame& elt) = delete;

  virtual ~stack_frame (void) = default;

  // FIXME: It would be nice to eliminate these but there are a few
  // places where we still need to know the specific type of the
  // stack frame that we are handling.

  virtual bool is_compiled_fcn_frame (void) const { return false; }
  virtual bool is_user_script_frame (void) const { return false; }
  virtual bool is_user_fcn_frame (void) const { return false; }
  virtual bool is_scope_frame (void) const { return false; }

  virtual void clear_values (void);

  std::size_t index (void) const { return m_index; }

  void line (int l) { m_line = l; }
  int line (void) const { return m_line; }

  void column (int c) { m_column = c; }
  int column (void) const { return m_column; }

  std::string fcn_file_name (void) const
  {
    octave_function *fcn = function ();

    return fcn ? fcn->fcn_file_name () : "";
  }

  std::string fcn_name (bool print_subfn = true) const
  {
    std::string retval;

    octave_function *fcn = function ();

    if (fcn)
      {
        std::string parent_fcn_name = fcn->parent_fcn_name ();

        if (print_subfn && ! parent_fcn_name.empty ())
          retval = parent_fcn_name + '>';

        if (fcn->is_anonymous_function ())
          retval += "@<anonymous>";
        else
          retval += fcn->name ();
      }
    else
      retval = "<unknown>";

    return retval;
  }

  virtual symbol_scope get_scope (void) const = 0;

  virtual octave_function * function (void) const { return nullptr; }

  virtual unwind_protect * unwind_protect_frame (void) { return nullptr; }

  symbol_info_list
  make_symbol_info_list (const std::list<symbol_record>& symrec_list) const;

  octave_value who (const string_vector& patterns, bool have_regexp,
                    bool return_list, bool verbose,
                    const std::string& whos_line_fmt,
                    const std::string& msg);

  symbol_info_list all_variables (void);

  octave_value workspace (void);

  std::list<std::string> variable_names (void) const;

  // Look for named symbol visible from current scope.  Don't
  // attempt to insert if missing.
  virtual symbol_record lookup_symbol (const std::string&) const = 0;

  // Look for named symbol visible from current scope.  Attempt to
  // insert if missing.
  virtual symbol_record insert_symbol (const std::string&) = 0;

  symbol_info_list glob_symbol_info (const std::string& pattern);

  symbol_info_list regexp_symbol_info (const std::string& pattern);

  symbol_info_list get_symbol_info (void)
  {
    return all_variables ();
  }

  void make_persistent (const symbol_record& sym)
  {
    if (sym.is_formal ())
      {
        std::string nm = sym.name ();
        error ("can't make function parameter %s persistent", nm.c_str ());
      }

    if (is_global (sym))
      {
        std::string nm = sym.name ();
        error ("can't make global variable '%s' persistent", nm.c_str ());
      }

    install_variable (sym, octave_value (), false);

    mark_persistent (sym);
  }

  void make_global (const symbol_record& sym)
  {
    if (is_persistent (sym))
      {
        std::string nm = sym.name ();
        error ("can't make persistent variable '%s' global", nm.c_str ());
      }

    install_variable (sym, octave_value (), true);

    mark_global (sym);
  }

  std::shared_ptr<stack_frame>
  parent_link (void) const {return m_parent_link; }

  std::shared_ptr<stack_frame>
  static_link (void) const {return m_static_link; }

  std::shared_ptr<stack_frame>
  access_link (void) const {return m_access_link; }

  virtual std::size_t size (void) const;

  virtual void resize (std::size_t);

  void mark_global (const symbol_record& sym)
  {
    mark_scope (sym, GLOBAL);
  }

  void unmark_global (const symbol_record& sym)
  {
    mark_scope (sym, LOCAL);
  }

  void mark_persistent (const symbol_record& sym)
  {
    mark_scope (sym, PERSISTENT);
  }

  void unmark_persistent (const symbol_record& sym)
  {
    mark_scope (sym, LOCAL);
  }

  bool is_defined (const symbol_record& sym) const
  {
    octave_value val = varval (sym);

    return val.is_defined ();
  }

  bool is_variable (const symbol_record& sym) const
  {
    octave_value val = varval (sym);

    return val.is_defined ();
  }

  bool is_variable (const std::string& name) const
  {
    symbol_record sym = lookup_symbol (name);

    return sym ? is_variable (sym) : false;
  }

  bool is_local_variable (const std::string& name) const
  {
    symbol_record sym = lookup_symbol (name);

    return sym ? (is_variable (sym) && ! is_global (sym)) : false;
  }

  bool is_object (const symbol_record& sym) const
  {
    octave_value val = varval (sym);

    return val.isobject ();
  }

  bool is_object (const std::string& name) const
  {
    symbol_record sym = lookup_symbol (name);

    return sym ? is_object (sym) : false;
  }

  virtual scope_flags scope_flag (const symbol_record&) const = 0;

  virtual scope_flags get_scope_flag (std::size_t) const;

  virtual void set_scope_flag (std::size_t, scope_flags);

  bool is_global (const symbol_record& sym) const
  {
    return scope_flag (sym) == GLOBAL;
  }

  bool is_global (const std::string& name) const
  {
    symbol_record sym = lookup_symbol (name);

    return sym ? is_global (sym) : false;
  }

  bool is_persistent (const symbol_record& sym) const
  {
    return scope_flag (sym) == PERSISTENT;
  }

  bool is_persistent (const std::string& name) const
  {
    symbol_record sym = lookup_symbol (name);

    return sym ? is_persistent (sym) : false;
  }

  void install_variable (const symbol_record& sym,
                         const octave_value& value, bool global);

  void install_variable (const std::string& name,
                         const octave_value& value, bool global)
  {
    symbol_record sym = insert_symbol (name);

    install_variable (sym, value, global);
  }

  virtual octave_value get_auto_fcn_var (auto_var_type) const = 0;

  virtual void set_auto_fcn_var (auto_var_type, const octave_value&) = 0;

  virtual octave_value varval (const symbol_record& sym) const = 0;

  virtual octave_value varval (std::size_t data_offset) const;

  octave_value varval (const std::string& name) const
  {
    symbol_record sym = lookup_symbol (name);

    return sym ? varval (sym) : octave_value ();
  }

  virtual octave_value& varref (const symbol_record& sym) = 0;

  virtual octave_value& varref (std::size_t data_offset);

  void assign (const symbol_record& sym, const octave_value& val)
  {
    octave_value& lhs = varref (sym);

    if (lhs.get_count () == 1)
      lhs.call_object_destructor ();

    // Regularize a null matrix if stored into a variable.
    lhs = val.storable_value ();
  }

  void assign (const std::string& name, const octave_value& val)
  {
    symbol_record sym = insert_symbol (name);

    assign (sym, val);
  }

  void assign (octave_value::assign_op op, const symbol_record& sym,
               const std::string& type,
               const std::list<octave_value_list>& idx,
               const octave_value& rhs)
  {
    if (idx.empty ())
      {
        if (op == octave_value::op_asn_eq)
          assign (sym, rhs);
        else
          varref (sym).assign (op, rhs);
      }
    else
      varref (sym).assign (op, type, idx, rhs);
  }

  void non_const_unary_op (octave_value::unary_op op,
                           const symbol_record& sym,
                           const std::string& type,
                           const std::list<octave_value_list>& idx)
  {
    if (idx.empty ())
      varref (sym).non_const_unary_op (op);
    else
      varref (sym).non_const_unary_op (op, type, idx);
  }

  octave_value value (const symbol_record& sym, const std::string& type,
                      const std::list<octave_value_list>& idx) const
  {
    octave_value retval = varval (sym);

    if (! idx.empty ())
      {
        if (retval.is_constant ())
          retval = retval.subsref (type, idx);
        else
          {
            octave_value_list t = retval.subsref (type, idx, 1);

            retval = t.length () > 0 ? t(0) : octave_value ();
          }
      }

    return retval;
  }

  octave_value find_subfunction (const std::string& name) const
  {
    symbol_scope scope = get_scope ();

    return scope.find_subfunction (name);
  }

  void clear (const symbol_record& sym)
  {
    if (is_global (sym))
      unmark_global (sym);

    assign (sym, octave_value ());

    if (is_persistent (sym))
      unmark_persistent (sym);
  }

  void clear_objects (void);

  void clear_variable (const std::string& name);

  void clear_variable_pattern (const std::string& pattern);
  void clear_variable_pattern (const string_vector& patterns);

  void clear_variable_regexp (const std::string& pattern);
  void clear_variable_regexp (const string_vector& patterns);

  void clear_variables (void);

  std::string get_dispatch_class (void) const { return m_dispatch_class; }

  void set_dispatch_class (const std::string& class_name)
  {
    m_dispatch_class = class_name;
  }

  void display_stopped_in_message (std::ostream& os) const;

  virtual void mark_scope (const symbol_record&, scope_flags) = 0;

  virtual void display (bool follow = true) const;

  virtual void accept (stack_frame_walker& sfw) = 0;

  virtual void break_closure_cycles (const std::shared_ptr<stack_frame>&) { }

  void mark_closure_context (void) { m_is_closure_context = true; }
  bool is_closure_context (void) const { return m_is_closure_context; }

protected:

  // Reference to the call stack that contains this frame.  Global
  // variables are stored in the call stack.  This link gives us
  // immediate access to them.
  tree_evaluator& m_evaluator;

  // TRUE if this stack frame is saved with a handle to a nested
  // function (closure).
  bool m_is_closure_context;

  // The line and column of the source file where this stack frame
  // was created.  Used to print stack traces.
  int m_line;
  int m_column;

  // Index in call stack.
  std::size_t m_index;

  // Pointer to the nearest parent frame.  May include compiled
  // functions.
  std::shared_ptr<stack_frame> m_parent_link;

  // Pointer to the nearest parent frame that contains variable
  // information (script, function, or scope).  This link skips over
  // compiled function parent frames.
  std::shared_ptr<stack_frame> m_static_link;

  // Pointer to the nearest lexical parent frame.  Used to access
  // non-local variables for nested and anonymous functions or as a
  // link to the parent frame in which a script is executed.
  std::shared_ptr<stack_frame> m_access_link;

  // Allow function handles to temporarily store their dispatch class
  // in the call stack.
  std::string m_dispatch_class;
};

OCTAVE_END_NAMESPACE(octave)

#endif
