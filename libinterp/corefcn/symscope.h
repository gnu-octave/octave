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

#if ! defined (octave_symscope_h)
#define octave_symscope_h 1

#include "octave-config.h"

#include <deque>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "glob-match.h"
#include "lo-regexp.h"
#include "oct-refcount.h"

class tree_argument_list;
class octave_user_code;

#include "ov.h"
#include "ovl.h"
#include "symrec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;

class symbol_scope_rep
  : public std::enable_shared_from_this<symbol_scope_rep>
{
public:

  typedef std::map<std::string, symbol_record>::const_iterator
    table_const_iterator;
  typedef std::map<std::string, symbol_record>::iterator
    table_iterator;

  typedef std::map<std::string, octave_value>::const_iterator
    subfunctions_const_iterator;
  typedef std::map<std::string, octave_value>::iterator
    subfunctions_iterator;

  symbol_scope_rep (const std::string& name = "")
    : m_name (name), m_symbols (), m_subfunctions (),
      m_persistent_values (), m_code (nullptr), m_fcn_name (),
      m_fcn_file_name (), m_dir_name (), m_parent (),
      m_primary_parent (), m_children (), m_nesting_depth (0),
      m_is_static (false), m_is_primary_fcn_scope (false)
  {
    // All scopes have ans as the first symbol, initially undefined.

    insert_local ("ans");
  }

  // No copying!

  symbol_scope_rep (const symbol_scope&) = delete;

  symbol_scope_rep& operator = (const symbol_scope&) = delete;

  ~symbol_scope_rep (void) = default;

  std::size_t num_symbols (void) const { return m_symbols.size (); }

  // Simply inserts symbol.  No non-local searching.

  symbol_record insert_local (const std::string& name);

  void insert_symbol_record (symbol_record& sr);

  bool is_nested (void) const { return m_nesting_depth > 0; }

  std::size_t nesting_depth (void) const { return m_nesting_depth; }

  void set_nesting_depth (std::size_t depth) { m_nesting_depth = depth; }

  bool is_parent (void) const { return ! m_children.empty (); }

  bool is_static (void) const { return m_is_static; }

  void mark_static (void) { m_is_static = true; }

  std::shared_ptr<symbol_scope_rep> parent_scope_rep (void) const
  {
    return m_parent.lock ();
  }

  std::shared_ptr<symbol_scope_rep> primary_parent_scope_rep (void) const
  {
    return m_primary_parent.lock ();
  }

  std::shared_ptr<symbol_scope_rep> dup (void) const
  {
    std::shared_ptr<symbol_scope_rep> new_sid
      = std::shared_ptr<symbol_scope_rep> (new symbol_scope_rep (m_name));

    for (const auto& nm_sr : m_symbols)
      new_sid->m_symbols[nm_sr.first] = nm_sr.second.dup ();

    new_sid->m_subfunctions = m_subfunctions;
    new_sid->m_persistent_values = m_persistent_values;
    new_sid->m_subfunction_names = m_subfunction_names;
    new_sid->m_code = m_code;
    new_sid->m_fcn_name = m_fcn_name;
    new_sid->m_fcn_file_name = m_fcn_file_name;
    new_sid->m_dir_name = m_dir_name;
    new_sid->m_parent = m_parent;
    new_sid->m_primary_parent = m_primary_parent;
    new_sid->m_children = m_children;
    new_sid->m_nesting_depth = m_nesting_depth;
    new_sid->m_is_static = m_is_static;
    new_sid->m_is_primary_fcn_scope = m_is_primary_fcn_scope;

    return new_sid;
  }

  octave_value& persistent_varref (std::size_t data_offset)
  {
    return m_persistent_values[data_offset];
  }

  octave_value persistent_varval (std::size_t data_offset) const
  {
    auto p = m_persistent_values.find (data_offset);

    return p == m_persistent_values.end () ? octave_value () : p->second;
  }

  symbol_record find_symbol (const std::string& name)
  {
    auto p = m_symbols.find (name);

    if (p == m_symbols.end ())
      return insert (name);
    else
      return p->second;
  }

  symbol_record lookup_symbol (const std::string& name) const
  {
    auto p = m_symbols.find (name);

    return p == m_symbols.end () ? symbol_record () : p->second;
  }

  symbol_record insert (const std::string& name);

  void rename (const std::string& old_name, const std::string& new_name)
  {
    auto p = m_symbols.find (old_name);

    if (p != m_symbols.end ())
      {
        symbol_record sr = p->second;

        sr.rename (new_name);

        m_symbols.erase (p);

        m_symbols[new_name] = sr;
      }
  }

  void install_subfunction (const std::string& name,
                            const octave_value& fval)
  {
    m_subfunctions[name] = fval;
  }

  void install_nestfunction (const std::string& name,
                             const octave_value& fval,
                             const symbol_scope& fcn_scope)
  {
    m_subfunctions[name] = fval;

    m_children.push_back (fcn_scope);
  }

  octave_value find_subfunction (const std::string& name) const;

  void lock_subfunctions (void)
  {
    for (auto& nm_sf : m_subfunctions)
      nm_sf.second.lock ();
  }

  void unlock_subfunctions (void)
  {
    for (auto& nm_sf : m_subfunctions)
      nm_sf.second.unlock ();
  }

  // Pairs of name, function objects.
  std::map<std::string, octave_value> subfunctions (void) const
  {
    return m_subfunctions;
  }

  void erase_subfunctions (void)
  {
    m_subfunctions.clear ();
  }

  void mark_subfunctions_in_scope_as_private (const std::string& class_name);

  bool has_subfunctions (void) const
  {
    return ! m_subfunction_names.empty ();
  }

  void stash_subfunction_names (const std::list<std::string>& names)
  {
    m_subfunction_names = names;
  }

  std::list<std::string> subfunction_names (void) const
  {
    return m_subfunction_names;
  }

  std::list<octave_value> localfunctions (void) const;

  octave_value dump (void) const;

  std::string name (void) const { return m_name; }

  void cache_name (const std::string& name) { m_name = name; }

  std::string fcn_name (void) const { return m_fcn_name; }

  void cache_fcn_name (const std::string& name) { m_fcn_name = name; }

  std::list<std::string> parent_fcn_names (void) const;

  octave_user_code * user_code (void) const { return m_code; }

  void set_user_code (octave_user_code *code) { m_code = code; }

  void set_parent (const std::shared_ptr<symbol_scope_rep>& parent);

  void set_primary_parent (const std::shared_ptr<symbol_scope_rep>& parent);

  void cache_fcn_file_name (const std::string& name)
  {
    m_fcn_file_name = name;
  }

  std::string fcn_file_name (void) const { return m_fcn_file_name; }

  void cache_dir_name (const std::string& name);

  std::string dir_name (void) const { return m_dir_name; }

  void mark_primary_fcn_scope (void) { m_is_primary_fcn_scope = true; }

  bool is_primary_fcn_scope (void) const { return m_is_primary_fcn_scope; }

  bool is_relative (const std::shared_ptr<symbol_scope_rep>& scope) const;

  void mark_as_variable (const std::string& nm);
  void mark_as_variables (const std::list<std::string>& lst);

  bool is_variable (const std::string& nm) const;

  void update_nest (void);

  bool look_nonlocal (const std::string& name, std::size_t offset,
                      symbol_record& result);

  octave_value dump_symbols_map (void) const;

  const std::map<std::string, symbol_record>& symbols (void) const
  {
    return m_symbols;
  }

  std::map<std::string, symbol_record>& symbols (void)
  {
    return m_symbols;
  }

  std::list<symbol_record> symbol_list (void) const;

private:

  //! Name for this scope (usually the corresponding filename of the
  //! function corresponding to the scope).

  std::string m_name;

  //! Map from symbol names to symbol info.

  std::map<std::string, symbol_record> m_symbols;

  //! Map from symbol names to subfunctions.

  std::map<std::string, octave_value> m_subfunctions;

  //! Map from data offset to persistent values in this scope.
  std::map<std::size_t, octave_value> m_persistent_values;

  //! The list of subfunctions (if any) in the order they appear in
  //! the function file.

  std::list<std::string> m_subfunction_names;

  //! The associated user code (may be null).

  octave_user_code *m_code;

  //! Simple name of the function corresponding to this scope.

  std::string m_fcn_name;

  //! The file name associated with m_code.

  std::string m_fcn_file_name;

  //! The directory associated with m_code.

  std::string m_dir_name;

  //! Parent of nested function (may be null).

  std::weak_ptr<symbol_scope_rep> m_parent;

  //! Primary (top) parent of nested function (may be null).  Used
  //! to determine whether two nested functions are related.

  std::weak_ptr<symbol_scope_rep> m_primary_parent;

  //! Child nested functions.

  std::vector<symbol_scope> m_children;

  //! If true, then this scope belongs to a nested function.

  std::size_t m_nesting_depth;

  //! If true then no variables can be added.

  bool m_is_static;

  //! If true, this is the scope of a primary function.
  bool m_is_primary_fcn_scope;
};

class symbol_scope
{
public:

  // Create a valid but possibly unnamed scope.
  symbol_scope (const std::string& name)
    : m_rep (new symbol_scope_rep (name))
  { }

  // NEW_REP must be dynamically allocated or nullptr.  If it is
  // nullptr, the scope is invalid.
  symbol_scope (const std::shared_ptr<symbol_scope_rep> new_rep = nullptr)
    : m_rep (new_rep)
  { }

  symbol_scope (const symbol_scope&) = default;

  symbol_scope& operator = (const symbol_scope&) = default;

  ~symbol_scope (void) = default;

  bool is_valid (void) const { return bool (m_rep); }

  explicit operator bool () const { return bool (m_rep); }

  std::size_t num_symbols (void) const
  {
    return m_rep ? m_rep->num_symbols () : 0;
  }

  symbol_record insert_local (const std::string& name)
  {
    return m_rep ? m_rep->insert_local (name) : symbol_record ();
  }

  void insert_symbol_record (symbol_record& sr)
  {
    if (m_rep)
      m_rep->insert_symbol_record (sr);
  }

  bool is_nested (void) const
  {
    return m_rep ? m_rep->is_nested () : false;
  }

  bool is_parent (void) const
  {
    return m_rep ? m_rep->is_parent () : false;
  }

  void set_nesting_depth (std::size_t depth)
  {
    if (m_rep)
      m_rep->set_nesting_depth (depth);
  }

  std::size_t nesting_depth (void) const
  {
    return m_rep ? m_rep->nesting_depth () : 0;
  }

  bool is_static (void) const
  {
    return m_rep ? m_rep->is_static () : false;
  }

  void mark_static (void)
  {
    if (m_rep)
      m_rep->mark_static ();
  }

  std::shared_ptr<symbol_scope_rep> parent_scope (void) const
  {
    return m_rep ? m_rep->parent_scope_rep () : nullptr;
  }

  std::shared_ptr<symbol_scope_rep> primary_parent_scope (void) const
  {
    return m_rep ? m_rep->primary_parent_scope_rep () : nullptr;
  }

  symbol_scope dup (void) const
  {
    return symbol_scope (m_rep ? m_rep->dup () : nullptr);
  }

  octave_value& persistent_varref (std::size_t data_offset)
  {
    static octave_value dummy_value;

    return m_rep ? m_rep->persistent_varref (data_offset) : dummy_value;
  }

  octave_value persistent_varval (std::size_t data_offset) const
  {
    return m_rep ? m_rep->persistent_varval (data_offset) : octave_value ();
  }

  symbol_record find_symbol (const std::string& name)
  {
    return m_rep ? m_rep->find_symbol (name) : symbol_record ();
  }

  // Like find_symbol, but does not insert.
  symbol_record lookup_symbol (const std::string& name) const
  {
    return m_rep ? m_rep->lookup_symbol (name) : symbol_record ();
  }

  symbol_record insert (const std::string& name)
  {
    return m_rep ? m_rep->insert (name) : symbol_record ();
  }

  void rename (const std::string& old_name, const std::string& new_name)
  {
    if (m_rep)
      m_rep->rename (old_name, new_name);
  }

  void install_subfunction (const std::string& name,
                            const octave_value& fval)
  {
    if (m_rep)
      m_rep->install_subfunction (name, fval);
  }

  void install_nestfunction (const std::string& name,
                             const octave_value& fval,
                             const symbol_scope& fcn_scope)
  {
    if (m_rep)
      m_rep->install_nestfunction (name, fval, fcn_scope);
  }

  octave_value find_subfunction (const std::string& name) const
  {
    return m_rep ? m_rep->find_subfunction (name) : octave_value ();
  }

  void lock_subfunctions (void)
  {
    if (m_rep)
      m_rep->lock_subfunctions ();
  }

  void unlock_subfunctions (void)
  {
    if (m_rep)
      m_rep->unlock_subfunctions ();
  }

  std::map<std::string, octave_value> subfunctions (void) const
  {
    return (m_rep
            ? m_rep->subfunctions ()
            : std::map<std::string, octave_value> ());
  }

  void erase_subfunctions (void)
  {
    if (m_rep)
      m_rep->erase_subfunctions ();
  }

  void mark_subfunctions_in_scope_as_private (const std::string& class_name)
  {
    if (m_rep)
      m_rep->mark_subfunctions_in_scope_as_private (class_name);
  }

  bool has_subfunctions (void) const
  {
    return m_rep ? m_rep->has_subfunctions () : false;
  }

  void stash_subfunction_names (const std::list<std::string>& names)
  {
    if (m_rep)
      m_rep->stash_subfunction_names (names);
  }

  std::list<std::string> subfunction_names (void) const
  {
    return m_rep ? m_rep->subfunction_names () : std::list<std::string> ();
  }

  // List of function handle objects.
  std::list<octave_value> localfunctions (void) const;

  octave_value dump (void) const
  {
    return m_rep ? m_rep->dump () : octave_value ();
  }

  std::string name (void) const
  {
    return m_rep ? m_rep->name () : "";
  }

  void cache_name (const std::string& name)
  {
    if (m_rep)
      m_rep->cache_name (name);
  }

  std::string fcn_name (void) const
  {
    return m_rep ? m_rep->fcn_name () : "";
  }

  void cache_fcn_name (const std::string& name)
  {
    if (m_rep)
      m_rep->cache_fcn_name (name);
  }

  std::list<std::string> parent_fcn_names (void) const
  {
    return m_rep ? m_rep->parent_fcn_names () : std::list<std::string> ();
  }

  octave_user_code * user_code (void) const
  {
    return m_rep ? m_rep->user_code () : nullptr;
  }

  void set_user_code (octave_user_code *code)
  {
    if (m_rep)
      m_rep->set_user_code (code);
  }

  void set_parent (const symbol_scope& p)
  {
    if (m_rep)
      m_rep->set_parent (p.get_rep ());
  }

  void set_primary_parent (const symbol_scope& p)
  {
    if (m_rep)
      m_rep->set_primary_parent (p.get_rep ());
  }

  void cache_fcn_file_name (const std::string& name)
  {
    if (m_rep)
      m_rep->cache_fcn_file_name (name);
  }

  void cache_dir_name (const std::string& name)
  {
    if (m_rep)
      m_rep->cache_dir_name (name);
  }

  std::string fcn_file_name (void) const
  {
    return m_rep ? m_rep->fcn_file_name () : "";
  }

  std::string dir_name (void) const
  {
    return m_rep ? m_rep->dir_name () : "";
  }

  void mark_primary_fcn_scope (void)
  {
    if (m_rep)
      m_rep->mark_primary_fcn_scope ();
  }

  bool is_primary_fcn_scope (void) const
  {
    return m_rep ? m_rep->is_primary_fcn_scope () : false;
  }

  bool is_relative (const symbol_scope& scope) const
  {
    return m_rep ? m_rep->is_relative (scope.get_rep ()) : false;
  }

  void mark_as_variable (const std::string& nm)
  {
    if (m_rep)
      m_rep->mark_as_variable (nm);
  }

  void mark_as_variables (const std::list<std::string>& lst)
  {
    if (m_rep)
      m_rep->mark_as_variables (lst);
  }

  bool is_variable (const std::string& nm) const
  {
    return m_rep ? m_rep->is_variable (nm) : false;
  }

  void update_nest (void)
  {
    if (m_rep)
      m_rep->update_nest ();
  }

  bool look_nonlocal (const std::string& name, std::size_t offset,
                      symbol_record& result)
  {
    return m_rep ? m_rep->look_nonlocal (name, offset, result) : false;
  }

  std::shared_ptr<symbol_scope_rep> get_rep (void) const
  {
    return m_rep;
  }

  friend bool operator == (const symbol_scope& a, const symbol_scope& b)
  {
    return a.m_rep == b.m_rep;
  }

  friend bool operator != (const symbol_scope& a, const symbol_scope& b)
  {
    return a.m_rep != b.m_rep;
  }

  const std::map<std::string, symbol_record>& symbols (void) const
  {
    static const std::map<std::string, symbol_record> empty_map;

    return m_rep ? m_rep->symbols () : empty_map;
  }

  std::map<std::string, symbol_record>& symbols (void)
  {
    static std::map<std::string, symbol_record> empty_map;

    return m_rep ? m_rep->symbols () : empty_map;
  }

  std::list<symbol_record> symbol_list (void) const
  {
    static const std::list<symbol_record> empty_list;

    return m_rep ? m_rep->symbol_list () : empty_list;
  }

private:

  std::shared_ptr<symbol_scope_rep> m_rep;
};

OCTAVE_END_NAMESPACE(octave)

#endif
