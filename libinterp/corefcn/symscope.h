/*

Copyright (C) 1993-2017 John W. Eaton
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_symscope_h)
#define octave_symscope_h 1

#include "octave-config.h"

#include <deque>
#include <limits>
#include <list>
#include <map>
#include <set>
#include <string>

#include "glob-match.h"
#include "lo-regexp.h"
#include "oct-refcount.h"

class tree_argument_list;
class octave_user_function;

#include "ov.h"
#include "ovl.h"
#include "symrec.h"
#include "workspace-element.h"

namespace octave
{
  class symbol_scope
  {
  public:

    typedef symbol_record::context_id context_id;

    typedef std::map<std::string, symbol_record>::const_iterator
    table_const_iterator;
    typedef std::map<std::string, symbol_record>::iterator
    table_iterator;

    typedef std::map<std::string, octave_value>::const_iterator
    subfunctions_const_iterator;
    typedef std::map<std::string, octave_value>::iterator
    subfunctions_iterator;

    symbol_scope (const std::string& name = "")
      : m_name (name), m_symbols (), m_subfunctions (),
        m_fcn (nullptr), m_parent (nullptr), m_parent_fcn (),
        m_children (), m_is_nested (false),
        m_is_static (false), m_context (0)
    { }

    // No copying!

    symbol_scope (const symbol_scope&) = delete;

    symbol_scope& operator = (const symbol_scope&) = delete;

    ~symbol_scope (void) = default;

    void insert_symbol_record (const symbol_record& sr)
    {
      m_symbols[sr.name ()] = sr;
    }

    bool is_nested (void) const { return m_is_nested; }

    void mark_nested (void) { m_is_nested = true; }

    bool is_static (void) const { return m_is_static; }

    void mark_static (void) { m_is_static = true; }

    symbol_scope * parent_scope (void) const { return m_parent; }
    octave_value parent_fcn (void) const { return m_parent_fcn; }

    symbol_scope * dup (void) const
    {
      symbol_scope *new_sid = new symbol_scope (m_name);

      for (const auto& nm_sr : m_symbols)
        new_sid->insert_symbol_record (nm_sr.second.dup (new_sid));

      new_sid->m_parent = m_parent;
      new_sid->m_parent_fcn = m_parent_fcn;

      return new_sid;
    }

    void set_context (context_id context) { m_context = context; }

    context_id current_context (void) const { return m_context; }

    symbol_record find_symbol (const std::string& name)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        return insert (name);
      else
        return p->second;
    }

    void inherit_internal (symbol_scope& donor_scope)
    {
      for (auto& nm_sr : m_symbols)
        {
          symbol_record& sr = nm_sr.second;

          if (! (sr.is_automatic () || sr.is_formal ()))
            {
              std::string nm = sr.name ();

              if (nm != "__retval__")
                {
                  octave_value val = donor_scope.varval (nm);

                  if (val.is_defined ())
                    {
                      sr.assign (val, m_context);

                      sr.mark_inherited ();
                    }
                }
            }
        }
    }

    void inherit (symbol_scope *donor_scope)
    {
      while (donor_scope)
        {
          inherit_internal (*donor_scope);

          if (donor_scope->is_nested ())
            donor_scope = donor_scope->parent_scope ();
          else
            break;
        }
    }


    octave_value
    find (const std::string& name, const octave_value_list& args,
          bool skip_variables, bool local_funcs);

    symbol_record&
    insert (const std::string& name, bool force_add = false);

    void rename (const std::string& old_name, const std::string& new_name)
    {
      table_iterator p = m_symbols.find (old_name);

      if (p != m_symbols.end ())
        {
          symbol_record sr = p->second;

          sr.rename (new_name);

          m_symbols.erase (p);

          m_symbols[new_name] = sr;
        }
    }

    void assign (const std::string& name, const octave_value& value,
                 bool force_add)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_record& sr = insert (name, force_add);

          sr.assign (value, m_context);
        }
      else
        p->second.assign (value, m_context);
    }

    void assign (const std::string& name,
                 const octave_value& value = octave_value ())
    {
      assign (name, value, false);
    }

    void force_assign (const std::string& name, const octave_value& value)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_record& sr = insert (name, true);

          sr.assign (value, m_context);
        }
      else
        p->second.assign (value, m_context);
    }

    octave_value varval (const std::string& name) const
    {
      table_const_iterator p = m_symbols.find (name);

      return (p != m_symbols.end ()
              ? p->second.varval (m_context) : octave_value ());
    }

    bool is_variable (const std::string& name) const
    {
      bool retval = false;

      table_const_iterator p = m_symbols.find (name);

      if (p != m_symbols.end ())
        {
          const symbol_record& sr = p->second;

          retval = sr.is_variable (m_context);
        }

      return retval;
    }

    void push_context (void)
    {
      for (auto& nm_sr : m_symbols)
        nm_sr.second.push_context ();
    }

    void pop_context (void)
    {
      table_iterator tbl_it = m_symbols.begin ();

      while (tbl_it != m_symbols.end ())
        {
          if (tbl_it->second.pop_context () == 0)
            m_symbols.erase (tbl_it++);
          else
            tbl_it++;
        }
    }

    void refresh (void)
    {
      for (auto& nm_sr : m_symbols)
        {
          symbol_record& sr = nm_sr.second;

          if (! sr.is_persistent ())
            sr.clear (m_context);
        }
    }

    void clear_variables (void)
    {
      for (auto& nm_sr : m_symbols)
        nm_sr.second.clear (m_context);
    }

    void clear_objects (void)
    {
      for (auto& nm_sr : m_symbols)
        {
          symbol_record& sr = nm_sr.second;
          octave_value val = sr.varval (m_context);
          if (val.isobject ())
            nm_sr.second.clear (m_context);
        }
    }

    void clear_variable (const std::string& name)
    {
      table_iterator p = m_symbols.find (name);

      if (p != m_symbols.end ())
        p->second.clear (m_context);
    }

    void clear_variable_pattern (const std::string& pat)
    {
      glob_match pattern (pat);

      for (auto& nm_sr : m_symbols)
        {
          symbol_record& sr = nm_sr.second;

          if (sr.is_defined (m_context) || sr.is_global ())
            {
              if (pattern.match (sr.name ()))
                sr.clear (m_context);
            }
        }
    }

    void clear_variable_regexp (const std::string& pat)
    {
      octave::regexp pattern (pat);

      for (auto& nm_sr : m_symbols)
        {
          symbol_record& sr = nm_sr.second;

          if (sr.is_defined (m_context) || sr.is_global ())
            {
              if (pattern.is_match (sr.name ()))
                sr.clear (m_context);
            }
        }
    }

    void mark_automatic (const std::string& name)
    {
      insert (name).mark_automatic ();
    }

    void mark_hidden (const std::string& name)
    {
      insert (name).mark_hidden ();
    }

    void mark_global (const std::string& name)
    {
      insert (name).mark_global ();
    }

    std::list<symbol_record>
    all_variables (bool defined_only = true,
                   unsigned int exclude = symbol_record::hidden) const
    {
      std::list<symbol_record> retval;

      for (const auto& nm_sr : m_symbols)
        {
          const symbol_record& sr = nm_sr.second;

          if ((defined_only && ! sr.is_defined (m_context))
              || (sr.storage_class () & exclude))
            continue;

          retval.push_back (sr);
        }

      return retval;
    }

    std::list<symbol_record>
    glob (const std::string& pattern, bool vars_only = false) const
    {
      std::list<symbol_record> retval;

      glob_match pat (pattern);

      for (const auto& nm_sr : m_symbols)
        {
          if (pat.match (nm_sr.first))
            {
              const symbol_record& sr = nm_sr.second;

              if (vars_only && ! sr.is_variable (m_context))
                continue;

              retval.push_back (sr);
            }
        }

      return retval;
    }

    std::list<symbol_record>
    regexp (const std::string& pattern, bool vars_only = false) const
    {
      std::list<symbol_record> retval;

      octave::regexp pat (pattern);

      for (const auto& nm_sr : m_symbols)
        {
          if (pat.is_match (nm_sr.first))
            {
              const symbol_record& sr = nm_sr.second;

              if (vars_only && ! sr.is_variable (m_context))
                continue;

              retval.push_back (sr);
            }
        }

      return retval;
    }

    std::list<std::string> variable_names (void)
    {
      std::list<std::string> retval;

      for (const auto& nm_sr : m_symbols)
        {
          if (nm_sr.second.is_variable (m_context))
            retval.push_back (nm_sr.first);
        }

      retval.sort ();

      return retval;
    }

    bool is_local_variable (const std::string& name) const
    {
      table_const_iterator p = m_symbols.find (name);

      return (p != m_symbols.end ()
              && ! p->second.is_global ()
              && p->second.is_defined (m_context));
    }

    bool is_global (const std::string& name) const
    {
      table_const_iterator p = m_symbols.find (name);

      return p != m_symbols.end () && p->second.is_global ();
    }

    void install_subfunction (const std::string& name,
                              const octave_value& fval,
                              bool is_nested = false);

    void install_nestfunction (const std::string& name,
                               const octave_value& fval)
    {
      install_subfunction (name, fval, true);
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

    std::map<std::string, octave_value> subfunctions (void)
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

    std::list<workspace_element> workspace_info (void) const;

    octave_value dump (void) const;

    std::string name (void) const { return m_name; }

    void cache_name (const std::string& name) { m_name = name; }

    octave_user_function *function (void) { return m_fcn; }

    void set_function (octave_user_function *fcn) { m_fcn = fcn; }

    void set_parent (symbol_scope *p);

    void update_nest (void);

    bool look_nonlocal (const std::string& name, symbol_record& result);

    void bind_script_symbols (symbol_scope *curr_scope);

    void unbind_script_symbols (void);

  private:

    // Name for this scope (usually the corresponding filename of the
    // function corresponding to the scope).
    std::string m_name;

    // Map from symbol names to symbol info.
    std::map<std::string, symbol_record> m_symbols;

    // Map from symbol names to subfunctions.
    std::map<std::string, octave_value> m_subfunctions;

    // The list of subfunctions (if any) in the order they appear in
    // the function file.
    std::list<std::string> m_subfunction_names;

    // The associated user code (may be null).
    octave_user_function *m_fcn;

    // Parent of nested function (may be null).
    symbol_scope *m_parent;
    octave_value m_parent_fcn;

    // Child nested functions.
    std::vector<symbol_scope*> m_children;

    // If true, then this scope belongs to a nested function.
    bool m_is_nested;

    // If true then no variables can be added.
    bool m_is_static;

    context_id m_context;

    octave_value dump_symbols_map (void) const;
  };
}

#endif
