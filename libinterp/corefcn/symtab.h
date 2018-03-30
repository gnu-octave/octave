/*

Copyright (C) 1993-2018 John W. Eaton
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_symtab_h)
#define octave_symtab_h 1

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

#include "fcn-info.h"
#include "ov.h"
#include "ovl.h"
#include "symscope.h"

namespace octave
{
  class OCTINTERP_API symbol_table
  {
  public:

    typedef octave::symbol_record symbol_record;
    typedef octave::symbol_scope scope;
    typedef octave::fcn_info fcn_info;

    symbol_table (void)
      : m_fcn_table (), m_class_precedence_table (),
        m_parent_map (), m_global_scope ("global scope"),
        m_top_scope ("top scope"), m_current_scope (m_top_scope)
      {
        install_builtins ();
      }

    // No copying!

    symbol_table (const symbol_table&) = delete;

    symbol_table& operator = (const symbol_table&) = delete;

    ~symbol_table (void) = default;

    symbol_scope global_scope (void) { return m_global_scope; }
    symbol_scope top_scope (void) { return m_top_scope; }

    symbol_scope current_scope (void) { return m_current_scope; }

    symbol_scope require_current_scope (const std::string& who)
    {
      if (! m_current_scope)
        error ("%s: missing scope", who.c_str ());

      return m_current_scope;
    }

    symbol_record::context_id current_context (void) const
    {
      return m_current_scope ? m_current_scope.current_context () : 0;
    }

    void set_scope (const symbol_scope& sid)
    {
      set_scope_and_context (sid, 0);
    }

    void set_scope_and_context (const symbol_scope& sid,
                                symbol_record::context_id context)
    {
      if (sid == m_global_scope)
        error ("can't set scope to global");

      m_current_scope = sid;

      if (m_current_scope)
        m_current_scope.set_context (context);
    }

    symbol_record find_symbol (const std::string& name, symbol_scope& sid)
    {
      return sid ? sid.find_symbol (name) : symbol_record ();
    }

    symbol_record find_symbol (const std::string& name)
    {
      return find_symbol (name, m_current_scope);
    }

    symbol_record find_global_symbol (const std::string& name)
    {
      symbol_record sym = find_symbol (name, m_global_scope);

      sym.mark_global ();

      return sym;
    }

    void
    inherit (symbol_scope& recipient_scope, const symbol_scope& donor_scope)
    {
      if (recipient_scope)
        recipient_scope.inherit (donor_scope);
    }

    void inherit (symbol_scope& recipient_scope)
    {
      inherit (recipient_scope, m_current_scope);
    }

    bool at_top_level (void) { return m_current_scope == m_top_scope; }

    // Find a value corresponding to the given name in the table.
    octave_value
      find (const std::string& name,
            const octave_value_list& args = octave_value_list (),
            bool skip_variables = false,
            bool local_funcs = true);

    void assign (const std::string& name, const octave_value& value, bool force_add)
    {
      if (m_current_scope)
        m_current_scope.assign (name, value, force_add);
    }

    void assign (const std::string& name,
                 const octave_value& value = octave_value ())
    {
      if (m_current_scope)
        m_current_scope.assign (name, value);
    }

    octave_value varval (const std::string& name) const
    {
      return (m_current_scope
              ? m_current_scope.varval (name) : octave_value ());
    }

    void global_assign (const std::string& name,
                        const octave_value& value = octave_value ())
    {
      m_global_scope.assign (name, value);
    }

    octave_value global_varval (const std::string& name) const
    {
      return m_global_scope.varval (name);
    }

    void
      top_level_assign (const std::string& name,
                        const octave_value& value = octave_value ())
    {
      m_top_scope.assign (name, value);
    }

    octave_value top_level_varval (const std::string& name) const
    {
      return m_top_scope.varval (name);
    }

    bool
      is_built_in_function_name (const std::string& name)
    {
      octave_value val = find_built_in_function (name);

      return val.is_defined ();
    }

    octave_value
      find_method (const std::string& name, const std::string& dispatch_type)
    {
      fcn_table_const_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          octave_value fcn = p->second.find_method (dispatch_type);

          if (! fcn.is_defined ())
            fcn = find_submethod (name, dispatch_type);

          return fcn;
        }
      else
        {
          fcn_info finfo (name);

          octave_value fcn = finfo.find_method (dispatch_type);

          if (! fcn.is_defined ())
            fcn = find_submethod (name, dispatch_type);

          if (fcn.is_defined ())
            m_fcn_table[name] = finfo;

          return fcn;
        }
    }

    octave_value
      find_submethod (const std::string& name, const std::string& dispatch_type);

    octave_value
      find_built_in_function (const std::string& name)
    {
      fcn_table_const_iterator p = m_fcn_table.find (name);

      return (p != m_fcn_table.end ()
              ? p->second.find_built_in_function () : octave_value ());
    }

    octave_value
      find_autoload (const std::string& name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      return (p != m_fcn_table.end ()
              ? p->second.find_autoload () : octave_value ());
    }

    octave_value builtin_find (const std::string& name);

    octave_value
    fcn_table_find (const std::string& name,
                    const octave_value_list& args = octave_value_list (),
                    bool local_funcs = true);

    octave_value
      find_function (const std::string& name,
                     const octave_value_list& args = octave_value_list (),
                     bool local_funcs = true);

    octave_value find_user_function (const std::string& name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      return (p != m_fcn_table.end ()
              ? p->second.find_user_function () : octave_value ());
    }

    octave_value find_cmdline_function (const std::string& name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      return (p != m_fcn_table.end ()
              ? p->second.find_cmdline_function () : octave_value ());
    }

    void install_cmdline_function (const std::string& name,
                                   const octave_value& fcn)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.install_cmdline_function (fcn);
        }
      else
        {
          fcn_info finfo (name);

          finfo.install_cmdline_function (fcn);

          m_fcn_table[name] = finfo;
        }
    }

    // Install local function FCN named NAME.  FILE_NAME is the name of
    // the file containing the local function.

    void install_local_function (const std::string& name,
                                 const octave_value& fcn,
                                 const std::string& file_name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.install_local_function (fcn, file_name);
        }
      else
        {
          fcn_info finfo (name);

          finfo.install_local_function (fcn, file_name);

          m_fcn_table[name] = finfo;
        }
    }

    void install_user_function (const std::string& name,
                                const octave_value& fcn)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.install_user_function (fcn);
        }
      else
        {
          fcn_info finfo (name);

          finfo.install_user_function (fcn);

          m_fcn_table[name] = finfo;
        }
    }

    // FIXME: should we ensure that FCN really is a built-in function
    // object?
    void install_built_in_function (const std::string& name,
                                    const octave_value& fcn)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.install_built_in_function (fcn);
        }
      else
        {
          fcn_info finfo (name);

          finfo.install_built_in_function (fcn);

          m_fcn_table[name] = finfo;
        }
    }

    void clear_all (bool force = false)
    {
      m_current_scope.clear_variables ();
      m_global_scope.clear_variables ();

      clear_functions (force);
    }

    void clear_global (const std::string& name);

    void clear_global_pattern (const std::string& pattern);

    // This is written as two separate functions instead of a single
    // function with default values so that it will work properly with
    // unwind_protect.

    void clear_functions (bool force = false)
    {
      fcn_table_iterator p = m_fcn_table.begin ();

      while (p != m_fcn_table.end ())
        (p++)->second.clear (force);
    }

    void clear_function (const std::string& name)
    {
      clear_user_function (name);
    }

    void clear_symbol (const std::string& name)
    {
      // FIXME: are we supposed to do both here?

      if (m_current_scope)
        m_current_scope.clear_variable (name);

      clear_function (name);
    }

    void clear_function_pattern (const std::string& pat)
    {
      glob_match pattern (pat);

      fcn_table_iterator p = m_fcn_table.begin ();

      while (p != m_fcn_table.end ())
        {
          if (pattern.match (p->first))
            (p++)->second.clear_user_function ();
          else
            p++;
        }
    }

    void clear_symbol_pattern (const std::string& pat)
    {
      // FIXME: are we supposed to do both here?

      if (m_current_scope)
        m_current_scope.clear_variable_pattern (pat);

      clear_function_pattern (pat);
    }

    void clear_user_function (const std::string& name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.clear_user_function ();
        }
      // FIXME: is this necessary, or even useful?
      // else
      //   error ("clear: no such function '%s'", name.c_str ());
    }

    // This clears oct and mex files, including autoloads.
    void clear_dld_function (const std::string& name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.clear_autoload_function ();
          finfo.clear_user_function ();
        }
    }

    void clear_mex_functions (void)
    {
      fcn_table_iterator p = m_fcn_table.begin ();

      while (p != m_fcn_table.end ())
        (p++)->second.clear_mex_function ();
    }

    bool set_class_relationship (const std::string& sup_class,
                                 const std::string& inf_class);

    bool is_superiorto (const std::string& a, const std::string& b);

    void alias_built_in_function (const std::string& alias,
                                  const std::string& name)
    {
      octave_value fcn = find_built_in_function (name);

      if (fcn.is_defined ())
        {
          fcn_info finfo (alias);

          finfo.install_built_in_function (fcn);

          m_fcn_table[alias] = finfo;
        }
      else
        panic ("alias: '%s' is undefined", name.c_str ());
    }

    void install_built_in_dispatch (const std::string& name,
                                    const std::string& klass)
    {
      fcn_table_iterator p = m_fcn_table.find (name);

      if (p != m_fcn_table.end ())
        {
          fcn_info& finfo = p->second;

          finfo.install_built_in_dispatch (klass);
        }
      else
        error ("install_built_in_dispatch: '%s' is undefined", name.c_str ());
    }

    std::list<symbol_record> glob (const std::string& pattern)
    {
      return (m_current_scope
              ? m_current_scope.glob (pattern) : std::list<symbol_record> ());
    }

    std::list<symbol_record> glob_global_variables (const std::string& pattern)
    {
      return m_global_scope.glob (pattern);
    }

    std::list<symbol_record>
    regexp_global_variables (const std::string& pattern)
    {
      return m_global_scope.regexp (pattern);
    }

    std::list<symbol_record> glob_variables (const string_vector& patterns)
    {
      std::list<symbol_record> retval;

      if (! m_current_scope)
        return retval;

      size_t len = patterns.numel ();

      for (size_t i = 0; i < len; i++)
        {
          std::list<symbol_record> tmp = m_current_scope.glob (patterns[i]);

          retval.insert (retval.begin (), tmp.begin (), tmp.end ());
        }

      return retval;
    }

    std::list<symbol_record> regexp_variables (const string_vector& patterns)
    {
      std::list<symbol_record> retval;

      if (! m_current_scope)
        return retval;

      size_t len = patterns.numel ();

      for (size_t i = 0; i < len; i++)
        {
          std::list<symbol_record> tmp = m_current_scope.regexp (patterns[i]);

          retval.insert (retval.begin (), tmp.begin (), tmp.end ());
        }

      return retval;
    }

    std::list<std::string> user_function_names (void)
    {
      std::list<std::string> retval;

      for (const auto& nm_finfo : m_fcn_table)
        {
          if (nm_finfo.second.is_user_function_defined ())
            retval.push_back (nm_finfo.first);
        }

      if (! retval.empty ())
        retval.sort ();

      return retval;
    }

    std::list<std::string> global_variable_names (void)
    {
      return m_global_scope.variable_names ();
    }

    std::list<std::string> top_level_variable_names (void)
    {
      return (m_top_scope
              ? m_top_scope.variable_names () : std::list<std::string> ());
    }

    std::list<std::string> variable_names (void)
    {
      return (m_current_scope
              ? m_current_scope.variable_names () : std::list<std::string> ());
    }

    std::list<std::string> built_in_function_names (void)
    {
      std::list<std::string> retval;

      for (const auto& nm_finfo : m_fcn_table)
        {
          octave_value fcn = nm_finfo.second.find_built_in_function ();

          if (fcn.is_defined ())
            retval.push_back (nm_finfo.first);
        }

      if (! retval.empty ())
        retval.sort ();

      return retval;
    }

    std::list<std::string> cmdline_function_names (void)
    {
      std::list<std::string> retval;

      for (const auto& nm_finfo : m_fcn_table)
        {
          octave_value fcn = nm_finfo.second.find_cmdline_function ();

          if (fcn.is_defined ())
            retval.push_back (nm_finfo.first);
        }

      if (! retval.empty ())
        retval.sort ();

      return retval;
    }

    octave_value dump (void) const;

    void add_to_parent_map (const std::string& classname,
                            const std::list<std::string>& parent_list)
    {
      m_parent_map[classname] = parent_list;
    }

    std::list<std::string>
      parent_classes (const std::string& dispatch_type)
    {
      std::list<std::string> retval;

      const_parent_map_iterator it = m_parent_map.find (dispatch_type);

      if (it != m_parent_map.end ())
        retval = it->second;

      for (const auto& nm : retval)
        {
          // Search for parents of parents and append them to the list.

          // FIXME: should we worry about a circular inheritance graph?

          std::list<std::string> parents = parent_classes (nm);

          if (! parents.empty ())
            retval.insert (retval.end (), parents.begin (), parents.end ());
        }

      return retval;
    }

    octave_user_function * get_curr_fcn (void)
    {
      return m_current_scope ? m_current_scope.function () : nullptr;
    }

    void cleanup (void);

    fcn_info * get_fcn_info (const std::string& name)
    {
      fcn_table_iterator p = m_fcn_table.find (name);
      return p != m_fcn_table.end () ? &p->second : nullptr;
    }

  private:

    typedef std::map<std::string, octave_value>::const_iterator
      global_symbols_const_iterator;
    typedef std::map<std::string, octave_value>::iterator
      global_symbols_iterator;

    typedef std::map<std::string, fcn_info>::const_iterator
      fcn_table_const_iterator;
    typedef std::map<std::string, fcn_info>::iterator
      fcn_table_iterator;

    // Map from function names to function info (private
    // functions, class constructors, class methods, etc.)
    // Note that subfunctions are defined in the scope that contains
    // them.
    std::map<std::string, fcn_info> m_fcn_table;

    // Map from class names to set of classes that have lower
    // precedence.
    std::map<std::string, std::set<std::string>> m_class_precedence_table;

    typedef std::map<std::string, std::set<std::string>>::const_iterator
      class_precedence_table_const_iterator;
    typedef std::map<std::string, std::set<std::string>>::iterator
      class_precedence_table_iterator;

    // Map from class names to parent class names.
    std::map<std::string, std::list<std::string>> m_parent_map;

    typedef std::map<std::string, std::list<std::string>>::const_iterator
      const_parent_map_iterator;
    typedef std::map<std::string, std::list<std::string>>::iterator
      parent_map_iterator;

    symbol_scope m_global_scope;
    symbol_scope m_top_scope;

    symbol_scope m_current_scope;

    octave_value dump_fcn_table_map (void) const;

    // This function is generated automatically by mk-builtins.pl.
    void install_builtins (void);
  };

  extern bool out_of_date_check (octave_value& function,
                                 const std::string& dispatch_type = "",
                                 bool check_relative = true);

  extern OCTINTERP_API std::string
  get_dispatch_type (const octave_value_list& args);

  extern OCTINTERP_API std::string
  get_dispatch_type (const octave_value_list& args,
                     builtin_type_t& builtin_type);
}

#endif
