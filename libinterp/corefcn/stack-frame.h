////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2020 The Octave Project Developers
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
#include <string>

class octave_value;
class octave_value_list;

#include "error.h"
#include "ov-fcn.h"
#include "ov-fcn.h"
#include "ov-fcn-handle.h"
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

namespace octave
{
  class tree_evaluator;
  class symbol_info_list;
  class unwind_protect;

  class compiled_fcn_stack_frame;
  class script_stack_frame;
  class user_fcn_stack_frame;
  class scope_stack_frame;

  class stack_frame_walker;

  class stack_frame
  {
  public:

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

    stack_frame (tree_evaluator& tw, size_t index,
                 stack_frame *static_link, stack_frame *access_link)
      : m_evaluator (tw), m_line (-1), m_column (-1), m_index (index),
        m_static_link (static_link), m_access_link (access_link),
        m_dispatch_class ()
    { }

    stack_frame (const stack_frame& elt) = default;

    stack_frame& operator = (const stack_frame& elt) = delete;

    virtual ~stack_frame (void) = default;

    virtual stack_frame * dup (void) const = 0;

    // FIXME: It would be nice to eliminate these but there are a few
    // places where we still need to know the specific type of the
    // stack frame that we are handling.

    virtual bool is_compiled_fcn_frame (void) const { return false; }
    virtual bool is_user_script_frame (void) const { return false; }
    virtual bool is_user_fcn_frame (void) const { return false; }
    virtual bool is_scope_frame (void) const { return false; }

    virtual void clear_values (void);

    size_t index (void) const { return m_index; }

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
            retval += octave_fcn_handle::anonymous;
          else
            retval += fcn->name ();
        }
      else
        retval = "<unknown>";

      return retval;
    }

    virtual symbol_scope get_scope (void) const = 0;

    virtual octave_function * function (void) const { return nullptr; }

    virtual unwind_protect *
    unwind_protect_frame (void) const { return nullptr; }

    symbol_info_list
    make_symbol_info_list (const std::list<symbol_record>& symrec_list) const;

    symbol_info_list all_variables (void);

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

    stack_frame * static_link (void) const {return m_static_link; }

    stack_frame * access_link (void) const {return m_access_link; }

    void set_closure_links (stack_frame *dup_frame)
    {
      m_static_link = dup_frame;
      m_access_link = dup_frame;
    }

    virtual size_t size (void) const;

    virtual void resize (size_t);

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

    virtual scope_flags get_scope_flag (size_t) const;

    virtual void set_scope_flag (size_t, scope_flags);

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

    virtual octave_value varval (const symbol_record& sym) const = 0;;

    virtual octave_value varval (size_t data_offset) const;

    octave_value varval (const std::string& name) const
    {
      symbol_record sym = lookup_symbol (name);

      return sym ? varval (sym) : octave_value ();
    }

    virtual octave_value& varref (const symbol_record& sym) = 0;

    virtual octave_value& varref (size_t data_offset);

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

    void do_non_const_unary_op (octave_value::unary_op op,
                                const symbol_record& sym,
                                const std::string& type,
                                const std::list<octave_value_list>& idx)
    {
      if (idx.empty ())
        varref (sym).do_non_const_unary_op (op);
      else
        varref (sym).do_non_const_unary_op (op, type, idx);
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

  protected:

    // Reference to the call stack that contains this frame.  Global
    // variables are stored in the call stack.  This link gives us
    // immediate access to them.
    tree_evaluator& m_evaluator;

    // The line and column of the source file where this stack frame
    // was created.  Used to print stack traces.
    int m_line;
    int m_column;

    // Index in call stack.
    size_t m_index;

    // Pointer to the nearest parent frame that contains variable
    // information (script, function, or scope).
    stack_frame *m_static_link;

    // Pointer to the nearest lexical parent frame.  Used to access
    // non-local variables for nested and anonymous functions or as a
    // link to the parent frame in which a script is executed.
    stack_frame *m_access_link;

    // Allow function handles to temporarily store their dispatch class
    // in the call stack.
    std::string m_dispatch_class;
  };

  class compiled_fcn_stack_frame : public stack_frame
  {
  public:

    compiled_fcn_stack_frame (void) = delete;

    compiled_fcn_stack_frame (tree_evaluator& tw, octave_function *fcn,
                              size_t index, stack_frame *static_link)
      : stack_frame (tw, index, static_link, static_link->access_link ()),
        m_fcn (fcn)
    { }

    compiled_fcn_stack_frame (const compiled_fcn_stack_frame& elt) = default;

    compiled_fcn_stack_frame&
    operator = (const compiled_fcn_stack_frame& elt) = delete;

    ~compiled_fcn_stack_frame (void) = default;

    compiled_fcn_stack_frame * dup (void) const;

    bool is_compiled_fcn_frame (void) const { return true; }

    symbol_scope get_scope (void) const
    {
      return m_static_link->get_scope ();
    }

    octave_function * function (void) const { return m_fcn; }

    symbol_record lookup_symbol (const std::string& name) const
    {
      return m_static_link->lookup_symbol (name);
    }

    symbol_record insert_symbol (const std::string& name)
    {
      return m_static_link->insert_symbol (name);
    }

    stack_frame::scope_flags scope_flag (const symbol_record& sym) const
    {
      // Look in closest stack frame that contains values (either the
      // top scope, or a user-defined function or script).

      return m_static_link->scope_flag (sym);
    }

    void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
    {
      m_static_link->set_auto_fcn_var (avt, val);
    }

    octave_value get_auto_fcn_var (auto_var_type avt) const
    {
      return m_static_link->get_auto_fcn_var (avt);
    }

    // We only need to override one of each of these functions.  The
    // using declaration will avoid warnings about partially-overloaded
    // virtual functions.
    using stack_frame::varval;
    using stack_frame::varref;

    octave_value varval (const symbol_record& sym) const
    {
      // Look in closest stack frame that contains values (either the
      // top scope, or a user-defined function or script).

      return m_static_link->varval (sym);
    }

    octave_value& varref (const symbol_record& sym)
    {
      // Look in closest stack frame that contains values (either the
      // top scope, or a user-defined function or script).

      return m_static_link->varref (sym);
    }

    void mark_scope (const symbol_record& sym, scope_flags flag)
    {
      // Look in closest stack frame that contains values (either the
      // top scope, or a user-defined function or script).

      m_static_link->mark_scope (sym, flag);
    }

    void display (bool follow = true) const;

    void accept (stack_frame_walker& sfw);

  private:

    // Compiled function object associated with this stack frame.
    // Should always be a built-in, .oct or .mex file function and
    // should always be valid.
    octave_function *m_fcn;
  };

  // Scripts have a symbol_scope object to store the set of variables
  // in the script, but values for those variables are stored in the
  // stack frame corresponding to the nearest calling function or in
  // the top-level scope (the evaluation stack frame).
  //
  // Accessing values in a scope requires a mapping from the index of
  // the variable for the script scope to the list of values in the
  // evaluation frame(s).  The frame offset tells us how many access
  // links we must follow to find the stack frame that holds the
  // value.  The value offset is the index into the vector of values
  // in that stack frame that we should use to find the value.
  //
  // Frame and value offsets are set in this stack frame when it is
  // created using information from the script and enclosing scopes.
  //
  // If a script is invoked in a nested function context, the frame
  // offsets for individual values may be different.  Some may be
  // accessed from the invoking function and some may come from a
  // parent function.

  class script_stack_frame : public stack_frame
  {
  public:

    script_stack_frame (void) = delete;

    script_stack_frame (tree_evaluator& tw, octave_user_script *script,
                        unwind_protect *up_frame, size_t index,
                        stack_frame *static_link);

    script_stack_frame (const script_stack_frame& elt) = default;

    script_stack_frame& operator = (const script_stack_frame& elt) = delete;

    ~script_stack_frame (void) = default;

    script_stack_frame * dup (void) const;

    bool is_user_script_frame (void) const { return true; }

    static stack_frame * get_access_link (stack_frame *static_link);

    static size_t get_num_symbols (octave_user_script *script);

    void set_script_offsets (void);

    void set_script_offsets_internal (const std::map<std::string,
                                                     symbol_record>& symbols);

    void resize_and_update_script_offsets (const symbol_record& sym);

    symbol_scope get_scope (void) const { return m_script->scope (); }

    octave_function * function (void) const { return m_script; }

    unwind_protect *
    unwind_protect_frame (void) const { return m_unwind_protect_frame; }

    symbol_record lookup_symbol (const std::string& name) const;

    symbol_record insert_symbol (const std::string&);

    size_t size (void) const { return m_lexical_frame_offsets.size (); }

    void resize (size_t size)
    {
      m_lexical_frame_offsets.resize (size, 0);
      m_value_offsets.resize (size, 0);
    }

    void get_val_offsets_with_insert (const symbol_record& sym,
                                      size_t& frame_offset,
                                      size_t& data_offset);

    bool get_val_offsets_internal (const symbol_record& sym,
                                   size_t& frame_offset,
                                   size_t& data_offset) const;

    bool get_val_offsets (const symbol_record& sym, size_t& frame_offset,
                          size_t& data_offset) const;

    scope_flags scope_flag (const symbol_record& sym) const;

    void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
    {
      m_access_link->set_auto_fcn_var (avt, val);
    }

    octave_value get_auto_fcn_var (auto_var_type avt) const
    {
      return m_access_link->get_auto_fcn_var (avt);
    }

    // We only need to override one of each of these functions.  The
    // using declaration will avoid warnings about partially-overloaded
    // virtual functions.
    using stack_frame::varval;
    using stack_frame::varref;

    octave_value varval (const symbol_record& sym) const;

    octave_value& varref (const symbol_record& sym);

    void mark_scope (const symbol_record& sym, scope_flags flag);

    void display (bool follow = true) const;

    void accept (stack_frame_walker& sfw);

  private:

    // Script object associated with this stack frame.  Should always
    // be valid.
    octave_user_script *m_script;

    // The nearest unwind protect frame that was active when this
    // stack frame was created.  Should always be valid.
    unwind_protect *m_unwind_protect_frame;

    // Mapping between the symbols in the symbol_scope object of the
    // script to the stack frame in which the script is executed.  The
    // frame offsets may be greater than one if the script is executed
    // in a nested function context.

    std::vector<size_t> m_lexical_frame_offsets;
    std::vector<size_t> m_value_offsets;
  };

  // Base class for values and offsets shared by user_fcn and scope
  // frames.

  class base_value_stack_frame : public stack_frame
  {
  public:

    base_value_stack_frame (void) = delete;

    base_value_stack_frame (tree_evaluator& tw, size_t num_symbols,
                            size_t index, stack_frame *static_link,
                            stack_frame *access_link)
      : stack_frame (tw, index, static_link, access_link),
        m_values (num_symbols, octave_value ()),
        m_flags (num_symbols, LOCAL),
        m_auto_vars (NUM_AUTO_VARS, octave_value ())
    { }

    base_value_stack_frame (const base_value_stack_frame& elt) = default;

    base_value_stack_frame&
    operator = (const base_value_stack_frame& elt) = delete;

    ~base_value_stack_frame (void) = default;

    size_t size (void) const
    {
      return m_values.size ();
    }

    void resize (size_t size)
    {
      m_values.resize (size, octave_value ());
      m_flags.resize (size, LOCAL);
    }

    stack_frame::scope_flags get_scope_flag (size_t data_offset) const
    {
      return m_flags.at (data_offset);
    }

    void set_scope_flag (size_t data_offset, scope_flags flag)
    {
      m_flags.at (data_offset) = flag;
    }

    octave_value get_auto_fcn_var (auto_var_type avt) const
    {
      return m_auto_vars.at (avt);
    }

    void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
    {
      m_auto_vars.at (avt) = val;
    }

    // We only need to override one of each of these functions.  The
    // using declaration will avoid warnings about partially-overloaded
    // virtual functions.
    using stack_frame::varval;
    using stack_frame::varref;

    octave_value varval (size_t data_offset) const
    {
      return m_values.at (data_offset);
    }

    octave_value& varref (size_t data_offset)
    {
      return m_values.at (data_offset);
    }

    void display (bool follow = true) const;

  protected:

    // Variable values.  This array is indexed by the data_offset
    // value stored in the symbol_record objects of the scope
    // associated with this stack frame.
    std::vector<octave_value> m_values;

    // The type of each variable (local, global, persistent) of each
    // value.  This array is indexed by the data_offset value stored
    // in the symbol_record objects of the scope associated with this
    // stack frame.  Local values are found in the M_VALUES array.
    // Global values are stored in the tree_evaluator object that contains
    // the stack frame.  Persistent values are stored in the function
    // scope corresponding to the stack frame.
    std::vector<scope_flags> m_flags;

    // A fixed list of Automatic variables created for this function.
    // The elements of this vector correspond to the auto_var_type
    // enum.
    std::vector<octave_value> m_auto_vars;
  };

  // User-defined functions have a symbol_scope object to store the set
  // of variables in the function and values are stored in the stack
  // frame corresponding to the invocation of the function or one of
  // its parents.  The frame offset tells us how many access links we
  // must follow to find the stack frame that holds the value.  The
  // value offset is the index into the vector of values in that stack
  // frame that we should use to find the value.
  //
  // Frame and value offsets are determined when the corresponding
  // function is parsed.

  class user_fcn_stack_frame : public base_value_stack_frame
  {
  public:

    user_fcn_stack_frame (void) = delete;

    user_fcn_stack_frame (tree_evaluator& tw, octave_user_function *fcn,
                          unwind_protect *up_frame, size_t index,
                          stack_frame *static_link,
                          stack_frame *access_link = nullptr)
      : base_value_stack_frame (tw, get_num_symbols (fcn), index, static_link,
                                (access_link
                                 ? access_link
                                 : get_access_link (fcn, static_link))),
        m_fcn (fcn), m_unwind_protect_frame (up_frame)
    { }

    user_fcn_stack_frame (const user_fcn_stack_frame& elt) = default;

    user_fcn_stack_frame&
    operator = (const user_fcn_stack_frame& elt) = delete;

    ~user_fcn_stack_frame (void) = default;

    user_fcn_stack_frame * dup (void) const;

    bool is_user_fcn_frame (void) const { return true; }

    static stack_frame *
    get_access_link (octave_user_function *fcn, stack_frame *static_link);

    static size_t get_num_symbols (octave_user_function *fcn)
    {
      symbol_scope fcn_scope = fcn->scope ();

      return fcn_scope.num_symbols ();
    }

    void clear_values (void);

    symbol_scope get_scope (void) const { return m_fcn->scope (); }

    octave_function * function (void) const { return m_fcn; }

    unwind_protect *
    unwind_protect_frame (void) const { return m_unwind_protect_frame; }

    symbol_record lookup_symbol (const std::string& name) const;

    symbol_record insert_symbol (const std::string&);

    scope_flags scope_flag (const symbol_record& sym) const;

    // We only need to override one of each of these functions.  The
    // using declaration will avoid warnings about partially-overloaded
    // virtual functions.
    using base_value_stack_frame::varval;
    using base_value_stack_frame::varref;

    octave_value varval (const symbol_record& sym) const;

    octave_value& varref (const symbol_record& sym);

    void mark_scope (const symbol_record& sym, scope_flags flag);

    void display (bool follow = true) const;

    void accept (stack_frame_walker& sfw);

  private:

    // User-defined object associated with this stack frame.  Should
    // always be valid.
    octave_user_function *m_fcn;

    // The nearest unwind protect frame that was active when this
    // stack frame was created.  Should always be valid.
    unwind_protect *m_unwind_protect_frame;
  };

  // Pure scope stack frames (primarily the top-level workspace) have
  // a set of variables and values are stored in the stack frame.  All
  // variable accesses are direct as there are no parent stack frames.
  //
  // Value offsets are determined when the corresponding variable is
  // entered into the symbol_scope object corresponding to the frame.

  class scope_stack_frame : public base_value_stack_frame
  {
  public:

    scope_stack_frame (void) = delete;

    scope_stack_frame (tree_evaluator& tw, const symbol_scope& scope,
                       size_t index, stack_frame *static_link)
      : base_value_stack_frame (tw, scope.num_symbols (), index,
                                static_link, nullptr),
        m_scope (scope)
    { }

    scope_stack_frame (const scope_stack_frame& elt) = default;

    scope_stack_frame& operator = (const scope_stack_frame& elt) = delete;

    ~scope_stack_frame (void) = default;

    scope_stack_frame * dup (void) const;

    bool is_scope_frame (void) const { return true; }

    symbol_scope get_scope (void) const { return m_scope; }

    symbol_record lookup_symbol (const std::string& name) const
    {
      return m_scope.lookup_symbol (name);
    }

    symbol_record insert_symbol (const std::string&);

    scope_flags scope_flag (const symbol_record& sym) const;

    // We only need to override one of each of these functions.  The
    // using declaration will avoid warnings about partially-overloaded
    // virtual functions.
    using base_value_stack_frame::varval;
    using base_value_stack_frame::varref;

    octave_value varval (const symbol_record& sym) const;

    octave_value& varref (const symbol_record& sym);

    void mark_scope (const symbol_record& sym, scope_flags flag);

    void display (bool follow = true) const;

    void accept (stack_frame_walker& sfw);

  private:

    // The scope object associated with this stack frame.
    symbol_scope m_scope;
  };
}

#endif
