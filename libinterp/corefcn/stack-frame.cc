////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1995-2023 The Octave Project Developers
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

#include "lo-regexp.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-eval.h"
#include "stack-frame.h"
#include "syminfo.h"
#include "symrec.h"
#include "symscope.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class compiled_fcn_stack_frame : public stack_frame
{
public:

  compiled_fcn_stack_frame (void) = delete;

  compiled_fcn_stack_frame (tree_evaluator& tw, octave_function *fcn,
                            std::size_t index,
                            const std::shared_ptr<stack_frame>& parent_link,
                            const std::shared_ptr<stack_frame>& static_link)
    : stack_frame (tw, index, parent_link, static_link,
                   static_link->access_link ()),
      m_fcn (fcn)
  { }

  compiled_fcn_stack_frame (const compiled_fcn_stack_frame& elt) = default;

  compiled_fcn_stack_frame&
  operator = (const compiled_fcn_stack_frame& elt) = delete;

  ~compiled_fcn_stack_frame (void) = default;

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
                      std::size_t index,
                      const std::shared_ptr<stack_frame>& parent_link,
                      const std::shared_ptr<stack_frame>& static_link);

  script_stack_frame (const script_stack_frame& elt) = default;

  script_stack_frame& operator = (const script_stack_frame& elt) = delete;

  ~script_stack_frame (void)
  {
    delete m_unwind_protect_frame;
  }

  bool is_user_script_frame (void) const { return true; }

  static std::shared_ptr<stack_frame>
  get_access_link (const std::shared_ptr<stack_frame>& static_link);

  static std::size_t get_num_symbols (octave_user_script *script);

  void set_script_offsets (void);

  void set_script_offsets_internal (const std::map<std::string,
                                    symbol_record>& symbols);

  void resize_and_update_script_offsets (const symbol_record& sym);

  symbol_scope get_scope (void) const { return m_script->scope (); }

  octave_function * function (void) const { return m_script; }

  unwind_protect * unwind_protect_frame (void);

  symbol_record lookup_symbol (const std::string& name) const;

  symbol_record insert_symbol (const std::string&);

  std::size_t size (void) const { return m_lexical_frame_offsets.size (); }

  void resize (std::size_t size)
  {
    m_lexical_frame_offsets.resize (size, 0);
    m_value_offsets.resize (size, 0);
  }

  void get_val_offsets_with_insert (const symbol_record& sym,
                                    std::size_t& frame_offset,
                                    std::size_t& data_offset);

  bool get_val_offsets_internal (const symbol_record& sym,
                                 std::size_t& frame_offset,
                                 std::size_t& data_offset) const;

  bool get_val_offsets (const symbol_record& sym, std::size_t& frame_offset,
                        std::size_t& data_offset) const;

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

  std::vector<std::size_t> m_lexical_frame_offsets;
  std::vector<std::size_t> m_value_offsets;
};

// Base class for values and offsets shared by user_fcn and scope
// frames.

class base_value_stack_frame : public stack_frame
{
public:

  base_value_stack_frame (void) = delete;

  base_value_stack_frame (tree_evaluator& tw, std::size_t num_symbols,
                          std::size_t index,
                          const std::shared_ptr<stack_frame>& parent_link,
                          const std::shared_ptr<stack_frame>& static_link,
                          const std::shared_ptr<stack_frame>& access_link)
    : stack_frame (tw, index, parent_link, static_link, access_link),
      m_values (num_symbols, octave_value ()),
      m_flags (num_symbols, LOCAL),
      m_auto_vars (NUM_AUTO_VARS, octave_value ())
  { }

  base_value_stack_frame (const base_value_stack_frame& elt) = default;

  base_value_stack_frame&
  operator = (const base_value_stack_frame& elt) = delete;

  ~base_value_stack_frame (void) = default;

  std::size_t size (void) const
  {
    return m_values.size ();
  }

  void resize (std::size_t size)
  {
    m_values.resize (size, octave_value ());
    m_flags.resize (size, LOCAL);
  }

  stack_frame::scope_flags get_scope_flag (std::size_t data_offset) const
  {
    return m_flags.at (data_offset);
  }

  void set_scope_flag (std::size_t data_offset, scope_flags flag)
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

  octave_value varval (std::size_t data_offset) const
  {
    return m_values.at (data_offset);
  }

  octave_value& varref (std::size_t data_offset)
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
                        std::size_t index,
                        const std::shared_ptr<stack_frame>& parent_link,
                        const std::shared_ptr<stack_frame>& static_link,
                        const std::shared_ptr<stack_frame>& access_link = std::shared_ptr<stack_frame> ())
    : base_value_stack_frame (tw, get_num_symbols (fcn), index,
                              parent_link, static_link,
                              (access_link
                               ? access_link
                               : get_access_link (fcn, static_link))),
      m_fcn (fcn), m_unwind_protect_frame (nullptr)
  { }

  user_fcn_stack_frame (tree_evaluator& tw, octave_user_function *fcn,
                        std::size_t index,
                        const std::shared_ptr<stack_frame>& parent_link,
                        const std::shared_ptr<stack_frame>& static_link,
                        const local_vars_map& local_vars,
                        const std::shared_ptr<stack_frame>& access_link = std::shared_ptr<stack_frame> ())
    : base_value_stack_frame (tw, get_num_symbols (fcn), index,
                              parent_link, static_link,
                              (access_link
                               ? access_link
                               : get_access_link (fcn, static_link))),
      m_fcn (fcn), m_unwind_protect_frame (nullptr)
  {
    // Initialize local variable values.

    for (const auto& nm_ov : local_vars)
      assign (nm_ov.first, nm_ov.second);
  }

  user_fcn_stack_frame (const user_fcn_stack_frame& elt) = default;

  user_fcn_stack_frame&
  operator = (const user_fcn_stack_frame& elt) = delete;

  ~user_fcn_stack_frame (void)
  {
    delete m_unwind_protect_frame;
  }

  bool is_user_fcn_frame (void) const { return true; }

  static std::shared_ptr<stack_frame>
  get_access_link (octave_user_function *fcn,
                   const std::shared_ptr<stack_frame>& static_link);

  static std::size_t get_num_symbols (octave_user_function *fcn)
  {
    symbol_scope fcn_scope = fcn->scope ();

    return fcn_scope.num_symbols ();
  }

  void clear_values (void);

  symbol_scope get_scope (void) const { return m_fcn->scope (); }

  octave_function * function (void) const { return m_fcn; }

  unwind_protect * unwind_protect_frame (void);

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

  void break_closure_cycles (const std::shared_ptr<stack_frame>& frame);

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
                     std::size_t index,
                     const std::shared_ptr<stack_frame>& parent_link,
                     const std::shared_ptr<stack_frame>& static_link)
    : base_value_stack_frame (tw, scope.num_symbols (), index,
                              parent_link, static_link, nullptr),
      m_scope (scope)
  { }

  scope_stack_frame (const scope_stack_frame& elt) = default;

  scope_stack_frame& operator = (const scope_stack_frame& elt) = delete;

  ~scope_stack_frame (void) = default;

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

// FIXME: There should probably be a display method for the script,
// fcn, and scope objects and the script and function objects should
// be responsible for displaying the scopes they contain.

static void display_scope (std::ostream& os, const symbol_scope& scope)
{
  if (scope)
    {
      os << "scope: " << scope.name () << std::endl;

      if (scope.num_symbols () > 0)
        {
          os << "name (frame offset, data offset, storage class):"
             << std::endl;

          std::list<symbol_record> symbols = scope.symbol_list ();

          for (auto& sym : symbols)
            {
              os << "  " << sym.name () << " (" << sym.frame_offset ()
                 << ", " << sym.data_offset () << ", " << sym.storage_class ()
                 << ")" << std::endl;
            }
        }
    }
}

class stack_frame_walker
{
protected:

  stack_frame_walker (void) { }

  virtual ~stack_frame_walker (void) = default;

public:

  // No copying!

  stack_frame_walker (const stack_frame_walker&) = delete;

  stack_frame_walker& operator = (const stack_frame_walker&) = delete;

  virtual void
  visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame&) = 0;

  virtual void
  visit_script_stack_frame (script_stack_frame&) = 0;

  virtual void
  visit_user_fcn_stack_frame (user_fcn_stack_frame&) = 0;

  virtual void
  visit_scope_stack_frame (scope_stack_frame&) = 0;
};

class symbol_cleaner : public stack_frame_walker
{
public:

  symbol_cleaner (const std::string& pattern, bool have_regexp = false)
    : stack_frame_walker (), m_patterns (pattern),
      m_clear_all_names (false), m_clear_objects (false),
      m_have_regexp (have_regexp), m_cleared_names ()
  { }

  symbol_cleaner (const string_vector& patterns, bool have_regexp = false)
    : stack_frame_walker (), m_patterns (patterns),
      m_clear_all_names (false), m_clear_objects (false),
      m_have_regexp (have_regexp), m_cleared_names ()
  { }

  symbol_cleaner (bool clear_all_names = true, bool clear_objects = false)
    : stack_frame_walker (), m_patterns (),
      m_clear_all_names (clear_all_names), m_clear_objects (clear_objects),
      m_have_regexp (false), m_cleared_names ()
  { }

  symbol_cleaner (const symbol_cleaner&) = delete;

  symbol_cleaner& operator = (const symbol_cleaner&) = delete;

  ~symbol_cleaner (void) = default;

  void visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame& frame)
  {
    // This one follows static link always.  Hmm, should the access
    // link for a compiled_fcn_stack_frame be the same as the static
    // link?

    std::shared_ptr<stack_frame> slink = frame.static_link ();

    if (slink)
      slink->accept (*this);
  }

  void visit_script_stack_frame (script_stack_frame& frame)
  {
    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_user_fcn_stack_frame (user_fcn_stack_frame& frame)
  {
    clean_frame (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_scope_stack_frame (scope_stack_frame& frame)
  {
    clean_frame (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

private:

  void maybe_clear_symbol (stack_frame& frame, const symbol_record& sym)
  {
    std::string name = sym.name ();

    if (m_cleared_names.find (name) == m_cleared_names.end ())
      {
        // FIXME: Should we check that the name is defined and skip if
        // it is not?  Is it possible for another symbol with the same
        // name to appear in a later stack frame?

        // FIXME: If we are clearing objects and a symbol is found,
        // should we add it to the list of cleared names (since
        // we did find a symbol) but skip clearing the object?

        if (m_clear_objects && ! frame.is_object (sym))
          return;

        m_cleared_names.insert (name);

        frame.clear (sym);
      }
  }

  // FIXME: It would be nice to avoid the duplication in the following
  // function.

  void clear_symbols (stack_frame& frame,
                      const std::list<symbol_record>& symbols)
  {
    if (m_clear_all_names)
      {
        for (const auto& sym : symbols)
          maybe_clear_symbol (frame, sym);
      }
    else if (m_have_regexp)
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            regexp pat (pattern);

            for (const auto& sym : symbols)
              {
                if (pat.is_match (sym.name ()))
                  maybe_clear_symbol (frame, sym);
              }
          }
      }
    else
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            glob_match pat (pattern);

            for (const auto& sym : symbols)
              {
                if (pat.match (sym.name ()))
                  maybe_clear_symbol (frame, sym);
              }
          }
      }
  }

  void clean_frame (stack_frame& frame)
  {
    symbol_scope scope = frame.get_scope ();

    std::list<symbol_record> symbols = scope.symbol_list ();

    if (m_clear_all_names || ! m_patterns.empty ())
      clear_symbols (frame, symbols);
  }

  string_vector m_patterns;

  bool m_clear_all_names;
  bool m_clear_objects;
  bool m_have_regexp;

  std::set<std::string> m_cleared_names;
};

class symbol_info_accumulator : public stack_frame_walker
{
public:

  symbol_info_accumulator (const std::string& pattern,
                           bool have_regexp = false)
    : stack_frame_walker (), m_patterns (pattern), m_match_all (false),
      m_first_only (false), m_have_regexp (have_regexp), m_sym_inf_list (),
      m_found_names ()
  { }

  symbol_info_accumulator (const string_vector& patterns,
                           bool have_regexp = false)
    : stack_frame_walker (), m_patterns (patterns), m_match_all (false),
      m_first_only (false), m_have_regexp (have_regexp), m_sym_inf_list (),
      m_found_names ()
  { }

  symbol_info_accumulator (bool match_all = true, bool first_only = true)
    : stack_frame_walker (), m_patterns (), m_match_all (match_all),
      m_first_only (first_only), m_have_regexp (false),
      m_sym_inf_list (), m_found_names ()
  { }

  symbol_info_accumulator (const symbol_info_accumulator&) = delete;

  symbol_info_accumulator& operator = (const symbol_info_accumulator&) = delete;

  ~symbol_info_accumulator (void) = default;

  bool is_empty  (void) const
  {
    for (const auto& nm_sil : m_sym_inf_list)
      {
        const symbol_info_list& lst = nm_sil.second;

        if (! lst.empty ())
          return false;
      }

    return true;
  }

  std::list<std::string> names (void) const
  {
    std::list<std::string> retval;

    for (const auto& nm_sil : m_sym_inf_list)
      {
        const symbol_info_list& lst = nm_sil.second;

        std::list<std::string> nm_list = lst.names ();

        for (const auto& nm : nm_list)
          retval.push_back (nm);
      }

    return retval;
  }

  symbol_info_list symbol_info (void) const
  {
    symbol_info_list retval;

    for (const auto& nm_sil : m_sym_inf_list)
      {
        const symbol_info_list& lst = nm_sil.second;

        for (const auto& syminf : lst)
          retval.push_back (syminf);
      }

    return retval;
  }

  octave_map map_value (void) const
  {
    octave_map retval;

    // FIXME: is there a better way to concatenate structures?

    std::size_t n_frames = m_sym_inf_list.size ();

    OCTAVE_LOCAL_BUFFER (octave_map, map_list, n_frames);

    std::size_t j = 0;
    for (const auto& nm_sil : m_sym_inf_list)
      {
        std::string scope_name = nm_sil.first;
        const symbol_info_list& lst = nm_sil.second;

        map_list[j] = lst.map_value (scope_name, n_frames-j);

        j++;
      }

    return octave_map::cat (-1, n_frames, map_list);
  }

  void display (std::ostream& os, const std::string& format) const
  {
    for (const auto& nm_sil : m_sym_inf_list)
      {
        os << "\nvariables in scope: " << nm_sil.first << "\n\n";

        const symbol_info_list& lst = nm_sil.second;

        lst.display (os, format);
      }
  }

  void visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame& frame)
  {
    // This one follows static link always.  Hmm, should the access
    // link for a compiled_fcn_stack_frame be the same as the static
    // link?

    std::shared_ptr<stack_frame> slink = frame.static_link ();

    if (slink)
      slink->accept (*this);
  }

  void visit_script_stack_frame (script_stack_frame& frame)
  {
    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_user_fcn_stack_frame (user_fcn_stack_frame& frame)
  {
    append_list (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_scope_stack_frame (scope_stack_frame& frame)
  {
    append_list (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

private:

  typedef std::pair<std::string, symbol_info_list> syminf_list_elt;

  // FIXME: the following is too complex and duplicates too much
  // code.  Maybe it should be split up so we have separate classes
  // that do each job that is needed?

  std::list<symbol_record>
  filter (stack_frame& frame, const std::list<symbol_record>& symbols)
  {
    std::list<symbol_record> new_symbols;

    if (m_match_all)
      {
        for (const auto& sym : symbols)
          {
            if (frame.is_defined (sym))
              {
                std::string name = sym.name ();

                if (m_first_only
                    && m_found_names.find (name) != m_found_names.end ())
                  continue;

                m_found_names.insert (name);

                new_symbols.push_back (sym);
              }
          }
      }
    else if (m_have_regexp)
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            regexp pat (pattern);

            for (const auto& sym : symbols)
              {
                std::string name = sym.name ();

                if (pat.is_match (name) && frame.is_defined (sym))
                  {
                    if (m_first_only
                        && m_found_names.find (name) != m_found_names.end ())
                      continue;

                    m_found_names.insert (name);

                    new_symbols.push_back (sym);
                  }
              }
          }
      }
    else
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            glob_match pat (pattern);

            for (const auto& sym : symbols)
              {
                std::string name = sym.name ();

                if (pat.match (name) && frame.is_defined (sym))
                  {
                    if (m_first_only
                        && m_found_names.find (name) == m_found_names.end ())
                      continue;

                    m_found_names.insert (name);

                    new_symbols.push_back (sym);
                  }
              }
          }
      }

    return new_symbols;
  }

  void append_list (stack_frame& frame)
  {
    symbol_scope scope = frame.get_scope ();

    std::list<symbol_record> symbols = scope.symbol_list ();

    if (m_match_all || ! m_patterns.empty ())
      symbols = filter (frame, symbols);

    symbol_info_list syminf_list = frame.make_symbol_info_list (symbols);

    m_sym_inf_list.push_back (syminf_list_elt (scope.name (), syminf_list));
  }

  string_vector m_patterns;

  bool m_match_all;
  bool m_first_only;
  bool m_have_regexp;

  std::list<std::pair<std::string, symbol_info_list>> m_sym_inf_list;

  std::set<std::string> m_found_names;
};

stack_frame *stack_frame::create (tree_evaluator& tw, octave_function *fcn,
                                  std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link)
{
  return new compiled_fcn_stack_frame (tw, fcn, index,
                                       parent_link, static_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  octave_user_script *script,
                                  std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link)
{
  return new script_stack_frame (tw, script, index, parent_link, static_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  octave_user_function *fcn, std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link,
                                  const std::shared_ptr<stack_frame>& access_link)
{
  return new user_fcn_stack_frame (tw, fcn, index,
                                   parent_link, static_link, access_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  octave_user_function *fcn, std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link,
                                  const local_vars_map& local_vars,
                                  const std::shared_ptr<stack_frame>& access_link)
{
  return new user_fcn_stack_frame (tw, fcn, index,
                                   parent_link, static_link, local_vars,
                                   access_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  const symbol_scope& scope, std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link)
{
  return new scope_stack_frame (tw, scope, index, parent_link, static_link);
}

// This function is only implemented and should only be called for
// user_fcn stack frames.  Anything else indicates an error in the
// implementation, but we'll simply warn if that happens.

void stack_frame::clear_values (void)
{
  warning ("invalid call to stack_frame::clear_values; please report");
}

symbol_info_list
stack_frame::make_symbol_info_list (const std::list<symbol_record>& symrec_list) const
{
  symbol_info_list symbol_stats;

  for (const auto& sym : symrec_list)
    {
      octave_value value = varval (sym);

      if (! value.is_defined ()
          || (is_user_fcn_frame () && sym.frame_offset () > 0))
        continue;

      symbol_info syminf (sym.name (), value, sym.is_formal (),
                          is_global (sym), is_persistent (sym));

      symbol_stats.append (syminf);
    }

  return symbol_stats;
}

octave_value stack_frame::who (const string_vector& patterns,
                               bool have_regexp, bool return_list,
                               bool verbose, const std::string& whos_line_fmt,
                               const std::string& msg)
{
  symbol_info_accumulator sym_inf_accum (patterns, have_regexp);

  accept (sym_inf_accum);

  if (return_list)
    {
      if (verbose)
        return sym_inf_accum.map_value ();
      else
        return Cell (string_vector (sym_inf_accum.names ()));
    }
  else if (! sym_inf_accum.is_empty ())
    {

      if (msg.empty ())
        octave_stdout << "Variables visible from the current scope:\n";
      else
        octave_stdout << msg;

      if (verbose)
        sym_inf_accum.display (octave_stdout, whos_line_fmt);
      else
        {
          octave_stdout << "\n";
          string_vector names (sym_inf_accum.names ());
          names.list_in_columns (octave_stdout);
        }

      octave_stdout << "\n";
    }

  return octave_value ();
}

// Return first occurrence of variables in current stack frame and any
// parent frames reachable through access links.

symbol_info_list stack_frame::all_variables (void)
{
  symbol_info_accumulator sia (true, true);

  accept (sia);

  return sia.symbol_info ();
}

octave_value stack_frame::workspace (void)
{
  std::list<octave_scalar_map> ws_list;

  stack_frame *frame = this;

  while (frame)
    {
      symbol_info_list symbols = frame->all_variables ();

      octave_scalar_map ws;

      for (const auto& sym_name : symbols.names ())
        {
          octave_value val = symbols.varval (sym_name);

          if (val.is_defined ())
            ws.assign (sym_name, val);
        }

      ws_list.push_back (ws);

      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  Cell ws_frames (ws_list.size (), 1);

  octave_idx_type i = 0;
  for (const auto& elt : ws_list)
    ws_frames(i++) = elt;

  return ws_frames;
}

// FIXME: Should this function also find any variables in parent
// scopes accessible through access_links?

std::list<std::string> stack_frame::variable_names (void) const
{
  std::list<std::string> retval;

  symbol_scope scope = get_scope ();

  const std::map<std::string, symbol_record>& symbols = scope.symbols ();

  for (const auto& nm_sr : symbols)
    {
      if (is_variable (nm_sr.second))
        retval.push_back (nm_sr.first);
    }

  retval.sort ();

  return retval;
}

symbol_info_list stack_frame::glob_symbol_info (const std::string& pattern)
{
  symbol_info_accumulator sia (pattern, false);

  accept (sia);

  return sia.symbol_info ();
}

symbol_info_list stack_frame::regexp_symbol_info (const std::string& pattern)
{
  symbol_info_accumulator sia (pattern, true);

  accept (sia);

  return sia.symbol_info ();
}

std::size_t stack_frame::size (void) const
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::resize (std::size_t)
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

stack_frame::scope_flags stack_frame::get_scope_flag (std::size_t) const
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::set_scope_flag (std::size_t, scope_flags)
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::install_variable (const symbol_record& sym,
                                    const octave_value& value, bool global)
{
  if (global && ! is_global (sym))
    {
      octave_value val = varval (sym);

      if (val.is_defined ())
        {
          std::string nm = sym.name ();

          warning_with_id ("Octave:global-local-conflict",
                           "global: '%s' is defined in the current scope.\n",
                           nm.c_str ());
          warning_with_id ("Octave:global-local-conflict",
                           "global: in a future version, global variables must be declared before use.\n");

          // If the symbol is defined in the local but not the
          // global scope, then use the local value as the
          // initial value.  This value will also override any
          // initializer in the global statement.
          octave_value global_val = m_evaluator.global_varval (nm);

          if (global_val.is_defined ())
            {
              warning_with_id ("Octave:global-local-conflict",
                               "global: global value overrides existing local value");

              clear (sym);
            }
          else
            {
              warning_with_id ("Octave:global-local-conflict",
                               "global: existing local value used to initialize global variable");

              m_evaluator.global_varref (nm) = val;
            }
        }

      mark_global (sym);
    }

  if (value.is_defined ())
    assign (sym, value);
}

octave_value stack_frame::varval (std::size_t) const
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

octave_value& stack_frame::varref (std::size_t)
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::clear_objects (void)
{
  symbol_cleaner sc (true, true);

  accept (sc);
}

void stack_frame::clear_variable (const std::string& name)
{
  symbol_cleaner sc (name);

  accept (sc);
}

void stack_frame::clear_variable_pattern (const std::string& pattern)
{
  symbol_cleaner sc (pattern);

  accept (sc);
}

void stack_frame::clear_variable_pattern (const string_vector& patterns)
{
  symbol_cleaner sc (patterns);

  accept (sc);
}

void stack_frame::clear_variable_regexp (const std::string& pattern)
{
  symbol_cleaner sc (pattern, true);

  accept (sc);
}

void stack_frame::clear_variable_regexp (const string_vector& patterns)
{
  symbol_cleaner sc (patterns, true);

  accept (sc);
}

void stack_frame::clear_variables (void)
{
  symbol_cleaner sc;

  accept (sc);
}

void stack_frame::display_stopped_in_message (std::ostream& os) const
{
  if (index () == 0)
    os << "at top level" << std::endl;
  else
    {
      os << "stopped in " << fcn_name ();

      int l = line ();
      if (l > 0)
        os << " at line " << line ();

      os << " [" << fcn_file_name () << "] " << std::endl;
    }
}

void stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [stack_frame] (" << this << ") --" << std::endl;

  os << "parent link: ";
  if (m_parent_link)
    os << m_parent_link.get ();
  else
    os << "NULL";
  os << std::endl;

  os << "static link: ";
  if (m_static_link)
    os << m_static_link.get ();
  else
    os << "NULL";
  os << std::endl;

  os << "access link: ";
  if (m_access_link)
    os << m_access_link.get ();
  else
    os << "NULL";
  os << std::endl;

  os << "line: " << m_line << std::endl;
  os << "column: " << m_column << std::endl;
  os << "index: " << m_index << std::endl;

  os << std::endl;

  if (! follow)
    return;

  os << "FOLLOWING ACCESS LINKS:" << std::endl;
  std::shared_ptr<stack_frame> frm = access_link ();
  while (frm)
    {
      frm->display (false);
      os << std::endl;

      frm = frm->access_link ();
    }
}

void compiled_fcn_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [compiled_fcn_stack_frame] (" << this << ") --" << std::endl;
  stack_frame::display (follow);

  os << "fcn: " << m_fcn->name ()
     << " (" << m_fcn->type_name () << ")" << std::endl;
}

void compiled_fcn_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_compiled_fcn_stack_frame (*this);
}

script_stack_frame::script_stack_frame (tree_evaluator& tw,
                                        octave_user_script *script,
                                        std::size_t index,
                                        const std::shared_ptr<stack_frame>& parent_link,
                                        const std::shared_ptr<stack_frame>& static_link)
  : stack_frame (tw, index, parent_link, static_link,
                 get_access_link (static_link)),
    m_script (script), m_unwind_protect_frame (nullptr),
    m_lexical_frame_offsets (get_num_symbols (script), 1),
    m_value_offsets (get_num_symbols (script), 0)
{
  set_script_offsets ();
}

std::size_t script_stack_frame::get_num_symbols (octave_user_script *script)
{
  symbol_scope script_scope = script->scope ();

  return script_scope.num_symbols ();
}

void script_stack_frame::set_script_offsets (void)
{
  // Set frame and data offsets inside stack frame based on enclosing
  // scope(s).

  symbol_scope script_scope = m_script->scope ();

  std::size_t num_script_symbols = script_scope.num_symbols ();

  resize (num_script_symbols);

  const std::map<std::string, symbol_record>& script_symbols
    = script_scope.symbols ();

  set_script_offsets_internal (script_symbols);
}

void script_stack_frame::set_script_offsets_internal
(const std::map<std::string, symbol_record>& script_symbols)
{
  // This scope will be used to evaluate the script.  Find (or
  // possibly insert) symbols from the dummy script scope here.

  symbol_scope eval_scope = m_access_link->get_scope ();

  if (eval_scope.is_nested ())
    {
      bool found = false;

      for (const auto& nm_sr : script_symbols)
        {
          std::string name = nm_sr.first;
          symbol_record script_sr = nm_sr.second;

          symbol_scope parent_scope = eval_scope;

          std::size_t count = 1;

          while (parent_scope)
            {
              const std::map<std::string, symbol_record>& parent_scope_symbols
                = parent_scope.symbols ();

              auto p = parent_scope_symbols.find (name);

              if (p != parent_scope_symbols.end ())
                {
                  found = true;
                  symbol_record parent_scope_sr = p->second;

                  std::size_t script_sr_data_offset = script_sr.data_offset ();

                  m_lexical_frame_offsets.at (script_sr_data_offset)
                    = parent_scope_sr.frame_offset () + count;

                  m_value_offsets.at (script_sr_data_offset)
                    = parent_scope_sr.data_offset ();

                  break;
                }
              else
                {
                  count++;
                  parent_scope = parent_scope.parent_scope ();
                }
            }

          if (! found)
            error ("symbol '%s' cannot be added to static scope",
                   name.c_str ());
        }
    }
  else
    {
      const std::map<std::string, symbol_record>& eval_scope_symbols
        = eval_scope.symbols ();

      for (const auto& nm_sr : script_symbols)
        {
          std::string name = nm_sr.first;
          symbol_record script_sr = nm_sr.second;

          auto p = eval_scope_symbols.find (name);

          symbol_record eval_scope_sr;

          if (p == eval_scope_symbols.end ())
            eval_scope_sr = eval_scope.insert (name);
          else
            eval_scope_sr = p->second;

          std::size_t script_sr_data_offset = script_sr.data_offset ();

          // The +1 is for going from the script frame to the eval
          // frame.  Only one access_link should need to be followed.

          m_lexical_frame_offsets.at (script_sr_data_offset)
            = eval_scope_sr.frame_offset () + 1;

          m_value_offsets.at (script_sr_data_offset)
            = eval_scope_sr.data_offset ();
        }
    }
}

void script_stack_frame::resize_and_update_script_offsets (const symbol_record& sym)
{
  std::size_t data_offset = sym.data_offset ();

  // This function is called when adding new symbols to a script
  // scope.  If the symbol wasn't present before, it should be outside
  // the range so we need to resize then update offsets.

  panic_unless (data_offset >= size ());

  resize (data_offset+1);

  // FIXME: We should be able to avoid creating the map object and the
  // looping in the set_scripts_offsets_internal function.  Can we do
  // that without (or with minimal) code duplication?

  std::map<std::string, symbol_record> tmp_symbols;
  tmp_symbols[sym.name ()] = sym;
  set_script_offsets_internal (tmp_symbols);
}

// If this is a nested scope, set access_link to nearest parent
// stack frame that corresponds to the lexical parent of this scope.

std::shared_ptr<stack_frame>
script_stack_frame::get_access_link (const std::shared_ptr<stack_frame>& static_link)
{
  // If this script is called from another script, set access
  // link to ultimate parent stack frame.

  std::shared_ptr<stack_frame> alink = static_link;

  while (alink->is_user_script_frame ())
    {
      if (alink->access_link ())
        alink = alink->access_link ();
      else
        break;
    }

  return alink;
}

unwind_protect *script_stack_frame::unwind_protect_frame (void)
{
  if (! m_unwind_protect_frame)
    m_unwind_protect_frame = new unwind_protect ();

  return m_unwind_protect_frame;
}

symbol_record script_stack_frame::lookup_symbol (const std::string& name) const
{
  symbol_scope scope = get_scope ();

  symbol_record sym = scope.lookup_symbol (name);

  if (sym)
    {
      panic_unless (sym.frame_offset () == 0);

      return sym;
    }

  sym = m_access_link->lookup_symbol (name);

  // Return symbol record with adjusted frame offset.
  symbol_record new_sym = sym.dup ();

  new_sym.set_frame_offset (sym.frame_offset () + 1);

  return new_sym;
}

symbol_record script_stack_frame::insert_symbol (const std::string& name)
{
  // If the symbols is already in the immediate scope, there is
  // nothing more to do.

  symbol_scope scope = get_scope ();

  symbol_record sym = scope.lookup_symbol (name);

  if (sym)
    {
      // All symbol records in a script scope should have zero offset,
      // which means we redirect our lookup using
      // lexical_frame_offsets and values_offets.
      panic_unless (sym.frame_offset () == 0);

      return sym;
    }

  // Insert the symbol in the current scope then resize and update
  // offsets.  This operation should never fail.

  sym = scope.find_symbol (name);

  panic_unless (sym.is_valid ());

  resize_and_update_script_offsets (sym);

  return sym;
}

// Similar to set_script_offsets_internal except that we only return
// frame and data offsets for symbols found by name in parent scopes
// instead of updating the offsets stored in the script frame itself.

bool
script_stack_frame::get_val_offsets_internal (const symbol_record& script_sr,
    std::size_t& frame_offset,
    std::size_t& data_offset) const
{
  bool found = false;

  // This scope will be used to evaluate the script.  Find symbols
  // here by name.

  symbol_scope eval_scope = m_access_link->get_scope ();

  if (eval_scope.is_nested ())
    {
      std::string name = script_sr.name ();

      symbol_scope parent_scope = eval_scope;

      std::size_t count = 1;

      while (parent_scope)
        {
          const std::map<std::string, symbol_record>& parent_scope_symbols
            = parent_scope.symbols ();

          auto p = parent_scope_symbols.find (name);

          if (p != parent_scope_symbols.end ())
            {
              found = true;
              symbol_record parent_scope_sr = p->second;

              frame_offset = parent_scope_sr.frame_offset () + count;

              data_offset = parent_scope_sr.data_offset ();

              break;
            }
          else
            {
              count++;
              parent_scope = parent_scope.parent_scope ();
            }
        }
    }
  else
    {
      const std::map<std::string, symbol_record>& eval_scope_symbols
        = eval_scope.symbols ();

      std::string name = script_sr.name ();

      auto p = eval_scope_symbols.find (name);

      symbol_record eval_scope_sr;

      if (p != eval_scope_symbols.end ())
        {
          found = true;
          eval_scope_sr = p->second;

          // The +1 is for going from the script frame to the eval
          // frame.  Only one access_link should need to be followed.

          frame_offset = eval_scope_sr.frame_offset () + 1;

          data_offset = eval_scope_sr.data_offset ();
        }
    }

  return found;
}

bool script_stack_frame::get_val_offsets (const symbol_record& sym,
    std::size_t& frame_offset,
    std::size_t& data_offset) const
{
  data_offset = sym.data_offset ();
  frame_offset = sym.frame_offset ();

  if (frame_offset == 0)
    {
      // An out of range data_offset value here means that we have a
      // symbol that was not originally in the script.  But this
      // function is called in places where we can't insert a new
      // symbol, so we fail and it is up to the caller to decide what
      // to do.

      if (data_offset >= size ())
        return get_val_offsets_internal (sym, frame_offset, data_offset);

      // Use frame and value offsets stored in this stack frame,
      // indexed by data_offset from the symbol_record to find the
      // values.  These offsets were determined by
      // script_stack_frame::set_script_offsets when this script was
      // invoked.

      frame_offset = m_lexical_frame_offsets.at (data_offset);

      if (frame_offset == 0)
        {
          // If the frame offset stored in m_lexical_frame_offsets is
          // zero, then the data offset in the evaluation scope has
          // not been determined so try to do that now.  The symbol
          // may have been added by eval and without calling
          // resize_and_update_script_offsets.

          return get_val_offsets_internal (sym, frame_offset, data_offset);
        }

      data_offset = m_value_offsets.at (data_offset);
    }
  else
    {
      // If frame_offset is not zero, then then we must have a symbol
      // that was not originally in the script.  The values should
      // have been determined by the script_stack_frame::lookup function.
    }

  return true;
}

void script_stack_frame::get_val_offsets_with_insert (const symbol_record& sym,
    std::size_t& frame_offset,
    std::size_t& data_offset)
{
  data_offset = sym.data_offset ();
  frame_offset = sym.frame_offset ();

  if (frame_offset == 0)
    {
      if (data_offset >= size ())
        {
          // If the data_offset is out of range, then we must have a
          // symbol that was not originally in the script.  Resize and
          // update the offsets.

          resize_and_update_script_offsets (sym);
        }

      // Use frame and value offsets stored in this stack frame,
      // indexed by data_offset from the symbol_record to find the
      // values.  These offsets were determined by
      // script_stack_frame::set_script_offsets when this script was
      // invoked.

      frame_offset = m_lexical_frame_offsets.at (data_offset);

      if (frame_offset == 0)
        {
          // If the frame offset stored in m_lexical_frame_offsets is
          // zero, then the data offset in the evaluation scope has
          // not been determined so try to do that now.  The symbol
          // may have been added by eval and without calling
          // resize_and_update_script_offsets.

          // We don't need to resize here.  That case is handled above.

          // FIXME: We should be able to avoid creating the map object
          // and the looping in the set_scripts_offsets_internal
          // function.  Can we do that without (or with minimal) code
          // duplication?

          std::map<std::string, symbol_record> tmp_symbols;
          tmp_symbols[sym.name ()] = sym;
          set_script_offsets_internal (tmp_symbols);

          // set_script_offsets_internal may have modified
          // m_lexical_frame_offsets and m_value_offsets.

          frame_offset = m_lexical_frame_offsets.at (data_offset);
        }

      data_offset = m_value_offsets.at (data_offset);
    }
  else
    {
      // If frame_offset is not zero, then then we must have a symbol
      // that was not originally in the script.  The values were
      // determined by the script_stack_frame::lookup function.
    }
}

stack_frame::scope_flags
script_stack_frame::scope_flag (const symbol_record& sym) const
{
  std::size_t frame_offset;
  std::size_t data_offset;

  bool found = get_val_offsets (sym, frame_offset, data_offset);

  // It can't be global or persistent, so call it local.
  if (! found)
    return LOCAL;

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return LOCAL;

  return frame->get_scope_flag (data_offset);
}

octave_value script_stack_frame::varval (const symbol_record& sym) const
{
  std::size_t frame_offset;
  std::size_t data_offset;

  bool found = get_val_offsets (sym, frame_offset, data_offset);

  if (! found)
    return octave_value ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return octave_value ();

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varval (data_offset);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varval (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varval (sym.name ());
    }

  error ("internal error: invalid switch case");
}

octave_value& script_stack_frame::varref (const symbol_record& sym)
{
  std::size_t frame_offset;
  std::size_t data_offset;
  get_val_offsets_with_insert (sym, frame_offset, data_offset);

  // Follow frame_offset access links to stack frame that holds
  // the value.

  stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (data_offset >= frame->size ())
    frame->resize (data_offset+1);

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varref (data_offset);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varref (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varref (sym.name ());
    }

  error ("internal error: invalid switch case");
}

void script_stack_frame::mark_scope (const symbol_record& sym,
                                     scope_flags flag)
{
  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize_and_update_script_offsets (sym);

  // Redirection to evaluation context for the script.

  std::size_t frame_offset = m_lexical_frame_offsets.at (data_offset);
  data_offset = m_value_offsets.at (data_offset);

  if (frame_offset > 1)
    error ("variables must be made PERSISTENT or GLOBAL in the first scope in which they are used");

  std::shared_ptr<stack_frame> frame = access_link ();

  if (data_offset >= frame->size ())
    frame->resize (data_offset+1);

  frame->set_scope_flag (data_offset, flag);
}

void script_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [script_stack_frame] (" << this << ") --" << std::endl;
  stack_frame::display (follow);

  os << "script: " << m_script->name ()
     << " (" << m_script->type_name () << ")" << std::endl;

  os << "lexical_offsets: " << m_lexical_frame_offsets.size ()
     << " elements:";

  for (std::size_t i = 0; i < m_lexical_frame_offsets.size (); i++)
    os << "  " << m_lexical_frame_offsets.at (i);
  os << std::endl;

  os << "value_offsets: " << m_value_offsets.size () << " elements:";
  for (std::size_t i = 0; i < m_value_offsets.size (); i++)
    os << "  " << m_value_offsets.at (i);
  os << std::endl;

  display_scope (os, get_scope ());
}

void script_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_script_stack_frame (*this);
}

void base_value_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [base_value_stack_frame] (" << this << ") --" << std::endl;
  stack_frame::display (follow);

  os << "values: " << m_values.size ()
     << " elements (idx, scope flag, type):" << std::endl;

  for (std::size_t i = 0; i < m_values.size (); i++)
    {
      os << "  (" << i << ", " << m_flags.at (i) << ", ";

      octave_value val = varval (i);

      os << (val.is_defined () ? val.type_name () : " UNDEFINED") << ")"
         << std::endl;
    }
}

// If this is a nested scope, set access_link to nearest parent
// stack frame that corresponds to the lexical parent of this scope.

std::shared_ptr<stack_frame>
user_fcn_stack_frame::get_access_link (octave_user_function *fcn,
                                       const std::shared_ptr<stack_frame>& static_link)
{
  std::shared_ptr<stack_frame> alink;

  symbol_scope fcn_scope = fcn->scope ();

  if (fcn_scope.is_nested ())
    {
      if (! static_link)
        error ("internal call stack error (invalid static link)");

      symbol_scope caller_scope = static_link->get_scope ();

      int nesting_depth = fcn_scope.nesting_depth ();
      int caller_nesting_depth = caller_scope.nesting_depth ();

      if (caller_nesting_depth < nesting_depth)
        {
          // FIXME: do we need to ensure that the called
          // function is a child of the caller?  Does it hurt
          // to panic_unless this condition, at least for now?

          alink = static_link;
        }
      else
        {
          // FIXME: do we need to check that the parent of the
          // called function is also a parent of the caller?
          // Does it hurt to panic_unless this condition, at least
          // for now?

          int links_to_follow = caller_nesting_depth - nesting_depth + 1;

          alink = static_link;

          while (alink && --links_to_follow >= 0)
            alink = alink->access_link ();

          if (! alink)
            error ("internal function nesting error (invalid access link)");
        }
    }

  return alink;
}

void user_fcn_stack_frame::clear_values (void)
{
  symbol_scope fcn_scope = m_fcn->scope ();

  const std::list<symbol_record>& symbols = fcn_scope.symbol_list ();

  if (size () == 0)
    return;

  for (const auto& sym : symbols)
    {
      std::size_t frame_offset = sym.frame_offset ();

      if (frame_offset > 0)
        continue;

      std::size_t data_offset = sym.data_offset ();

      if (data_offset >= size ())
        continue;

      if (get_scope_flag (data_offset) == LOCAL)
        {
          octave_value& ref = m_values.at (data_offset);

          if (ref.get_count () == 1)
            {
              ref.call_object_destructor ();
              ref = octave_value ();
            }
        }
    }
}

unwind_protect *user_fcn_stack_frame::unwind_protect_frame (void)
{
  if (! m_unwind_protect_frame)
    m_unwind_protect_frame = new unwind_protect ();

  return m_unwind_protect_frame;
}

symbol_record user_fcn_stack_frame::lookup_symbol (const std::string& name) const
{
  const stack_frame *frame = this;

  while (frame)
    {
      symbol_scope scope = frame->get_scope ();

      symbol_record sym = scope.lookup_symbol (name);

      if (sym)
        return sym;

      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  return symbol_record ();
}

symbol_record user_fcn_stack_frame::insert_symbol (const std::string& name)
{
  // If the symbols is already in the immediate scope, there is
  // nothing more to do.

  symbol_scope scope = get_scope ();

  symbol_record sym = scope.lookup_symbol (name);

  if (sym)
    return sym;

  // FIXME: This needs some thought... We may need to add a symbol to
  // a static workspace, but the symbol can never be defined as a
  // variable.  This currently works by tagging the added symbol as
  // "added_static".  Aside from the bad name, this doesn't seem like
  // the best solution.  Maybe scopes should have a separate set of
  // symbols that may only be defined as functions?

  // Insert the symbol in the current scope.  This is not possible for
  // anonymous functions, nested functions, or functions that contain
  // nested functions (their scopes will all be marked static).

  //    if (scope.is_static ())
  //      error ("can not add variable '%s' to a static workspace",
  //             name.c_str ());

  // At this point, non-local references are not possible so we only
  // need to look in the current scope and insert there.  This
  // operation should never fail.

  sym = scope.find_symbol (name);

  panic_unless (sym.is_valid ());

  return sym;
}

stack_frame::scope_flags
user_fcn_stack_frame::scope_flag (const symbol_record& sym) const
{
  std::size_t frame_offset = sym.frame_offset ();
  std::size_t data_offset = sym.data_offset ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return LOCAL;

  return frame->get_scope_flag (data_offset);
}

octave_value user_fcn_stack_frame::varval (const symbol_record& sym) const
{
  std::size_t frame_offset = sym.frame_offset ();
  std::size_t data_offset = sym.data_offset ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return octave_value ();

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varval (data_offset);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varval (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varval (sym.name ());
    }

  error ("internal error: invalid switch case");
}

octave_value& user_fcn_stack_frame::varref (const symbol_record& sym)
{
  std::size_t frame_offset = sym.frame_offset ();
  std::size_t data_offset = sym.data_offset ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (data_offset >= frame->size ())
    frame->resize (data_offset+1);

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varref (data_offset);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varref (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varref (sym.name ());
    }

  error ("internal error: invalid switch case");
}

void user_fcn_stack_frame::mark_scope (const symbol_record& sym, scope_flags flag)
{
  std::size_t frame_offset = sym.frame_offset ();

  if (frame_offset > 0 && (flag == PERSISTENT || flag == GLOBAL))
    error ("variables must be made PERSISTENT or GLOBAL in the first scope in which they are used");

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize (data_offset+1);

  set_scope_flag (data_offset, flag);
}

void user_fcn_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [user_fcn_stack_frame] (" << this << ") --" << std::endl;
  base_value_stack_frame::display (follow);

  os << "fcn: " << m_fcn->name ()
     << " (" << m_fcn->type_name () << ")" << std::endl;

  display_scope (os, get_scope ());
}

void user_fcn_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_user_fcn_stack_frame (*this);
}

void user_fcn_stack_frame::break_closure_cycles (const std::shared_ptr<stack_frame>& frame)
{
  for (auto& val : m_values)
    val.break_closure_cycles (frame);

  if (m_access_link)
    m_access_link->break_closure_cycles (frame);
}

symbol_record scope_stack_frame::insert_symbol (const std::string& name)
{
  // There is no access link for scope frames, so there is no other
  // frame to search in and the offset must be zero.

  symbol_record sym = m_scope.lookup_symbol (name);

  if (sym)
    return sym;

  // If the symbol is not found, insert it.  We only need to search in
  // the local scope object.  This operation should never fail.

  sym = m_scope.find_symbol (name);

  panic_unless (sym.is_valid ());

  return sym;
}

stack_frame::scope_flags
scope_stack_frame::scope_flag (const symbol_record& sym) const
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    return LOCAL;

  return get_scope_flag (data_offset);
}

octave_value scope_stack_frame::varval (const symbol_record& sym) const
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    return octave_value ();

  switch (get_scope_flag (data_offset))
    {
    case LOCAL:
      return m_values.at (data_offset);

    case PERSISTENT:
      return m_scope.persistent_varval (data_offset);

    case GLOBAL:
      return m_evaluator.global_varval (sym.name ());
    }

  error ("internal error: invalid switch case");
}

octave_value& scope_stack_frame::varref (const symbol_record& sym)
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize (data_offset+1);

  switch (get_scope_flag (data_offset))
    {
    case LOCAL:
      return m_values.at (data_offset);

    case PERSISTENT:
      return m_scope.persistent_varref (data_offset);

    case GLOBAL:
      return m_evaluator.global_varref (sym.name ());
    }

  error ("internal error: invalid switch case");
}

void scope_stack_frame::mark_scope (const symbol_record& sym,
                                    scope_flags flag)
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize (data_offset+1);

  set_scope_flag (data_offset, flag);
}

void scope_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [scope_stack_frame] (" << this << ") --" << std::endl;
  base_value_stack_frame::display (follow);

  display_scope (os, m_scope);
}

void scope_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_scope_stack_frame (*this);
}

OCTAVE_END_NAMESPACE(octave)
