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

class tree_argument_list;
class octave_user_function;

#include "oct-refcount.h"
#include "ov.h"
#include "ovl.h"
#include "workspace-element.h"

class
OCTINTERP_API
symbol_table
{
public:

  static octave_value dummy_octave_value;

  typedef int scope_id;
  typedef size_t context_id;

  class scope;

  class scope_id_cache
  {
  public:

    typedef std::set<scope_id>::iterator set_iterator;
    typedef std::set<scope_id>::const_iterator set_const_iterator;

    // We start with 2 because we allocate 0 for the global symbols
    // and 1 for the top-level workspace.

    scope_id_cache (void)
      : m_next_available (2), m_in_use (), m_free_list () { }

    // No copying!

    scope_id_cache (const scope_id_cache&) = delete;

    scope_id_cache& operator = (const scope_id_cache&) = delete;

    ~scope_id_cache (void) = default;

    scope_id alloc (void)
    {
      scope_id retval;

      set_iterator p = m_free_list.begin ();

      if (p != m_free_list.end ())
        {
          retval = *p;
          m_free_list.erase (p);
        }
      else
        retval = m_next_available++;

      m_in_use.insert (retval);

      return retval;
    }

    void free (scope_id sid)
    {
      set_iterator p = m_in_use.find (sid);

      if (p == m_in_use.end ())
        error ("free_scope: scope %d not found!", sid);

      m_in_use.erase (p);
      m_free_list.insert (sid);
    }

    std::list<scope_id> scopes (void) const
    {
      std::list<scope_id> retval;

      for (const auto& scope_id : m_in_use)
        retval.push_back (scope_id);

      retval.sort ();

      return retval;
    }

  private:

    // The next available scope not in the free list.
    scope_id m_next_available;

    // The set of scope IDs that are currently allocated.
    std::set<scope_id> m_in_use;

    // The set of scope IDs that are currently available.
    std::set<scope_id> m_free_list;
  };

  class fcn_info;

  class
  symbol_record
  {
  public:

    // generic variable
    static const unsigned int local = 1;

    // varargin, argn, .nargin., .nargout.
    // (FIXME: is this really used now?)
    static const unsigned int automatic = 2;

    // formal parameter
    static const unsigned int formal = 4;

    // not listed or cleared (.nargin., .nargout.)
    static const unsigned int hidden = 8;

    // inherited from parent scope; not cleared at function exit
    static const unsigned int inherited = 16;

    // global (redirects to global scope)
    static const unsigned int global = 32;

    // not cleared at function exit
    static const unsigned int persistent = 64;

    // this symbol may NOT become a variable.
    // (symbol added to a static workspace)
    static const unsigned int added_static = 128;

  private:

    class
    symbol_record_rep
    {
    public:

      symbol_record_rep (scope_id sid, const std::string& nm,
                         const octave_value& v, unsigned int sc)
        : m_decl_scope (sid), curr_fcn (0), name (nm), value_stack (),
          storage_class (sc), /* finfo (), */ valid (true), count (1)
      {
        value_stack.push_back (v);
      }

      // No copying!

      symbol_record_rep (const symbol_record_rep& ov) = delete;

      symbol_record_rep& operator = (const symbol_record_rep&) = delete;

      ~symbol_record_rep (void) = default;

      void assign (const octave_value& value,
                   context_id context = xdefault_context)
      {
        varref (context) = value;
      }

      void assign (octave_value::assign_op op,
                   const std::string& type,
                   const std::list<octave_value_list>& idx,
                   const octave_value& value,
                   context_id context = xdefault_context)
      {
        varref(context).assign (op, type, idx, value);
      }

      void assign (octave_value::assign_op op, const octave_value& value,
                   context_id context = xdefault_context)
      {
        varref(context).assign (op, value);
      }

      void do_non_const_unary_op (octave_value::unary_op op,
                                  context_id context = xdefault_context)
      {
        varref(context).do_non_const_unary_op (op);
      }

      void do_non_const_unary_op (octave_value::unary_op op,
                                  const std::string& type,
                                  const std::list<octave_value_list>& idx,
                                  context_id context = xdefault_context)
      {
        varref(context).do_non_const_unary_op (op, type, idx);
      }

      octave_value& varref (context_id context = xdefault_context)
      {
        if (is_global ())
          return xglobal_varref ();
        else if (is_persistent ())
          return xpersistent_varref ();
        else
          {
            if (context == xdefault_context)
              context = active_context ();

            context_id n = value_stack.size ();
            while (n++ <= context)
              value_stack.push_back (octave_value ());

            return value_stack[context];
          }
      }

      octave_value varval (context_id context = xdefault_context) const
      {
        if (is_global ())
          return xglobal_varval ();
        else if (is_persistent ())
          return xpersistent_varval ();
        else
          {
            if (context == xdefault_context)
              context = active_context ();

            if (context < value_stack.size ())
              return value_stack[context];
            else
              return octave_value ();
          }
      }

      void push_context (scope_id sid)
      {
        if (! (is_persistent () || is_global ())
            && sid == decl_scope ())
          value_stack.push_back (octave_value ());
      }

      // If pop_context returns 0, we are out of values and this element
      // of the symbol table should be deleted.  This can happen for
      // functions like
      //
      //   function foo (n)
      //     if (n > 0)
      //       foo (n-1);
      //     else
      //       eval ("x = 1");
      //     endif
      //   endfunction
      //
      // Here, X should only exist in the final stack frame.

      size_t pop_context (scope_id sid)
      {
        size_t retval = 1;

        if (! (is_persistent () || is_global ())
            && sid == decl_scope ())
          {
            value_stack.pop_back ();
            retval = value_stack.size ();
          }

        return retval;
      }

      void clear (void) { clear (decl_scope ()); }

      void clear (scope_id sid);

      bool is_defined (context_id context = xdefault_context) const
      {
        if (context == xdefault_context)
          context = active_context ();

        return varval (context).is_defined ();
      }

      bool is_valid (void) const
      {
        return valid;
      }

      bool is_variable (context_id context) const
      {
        if (context == xdefault_context)
          context = active_context ();

        return (! is_local () || is_defined (context));
      }

      bool is_local (void) const { return storage_class & local; }
      bool is_automatic (void) const { return storage_class & automatic; }
      bool is_formal (void) const { return storage_class & formal; }
      bool is_hidden (void) const { return storage_class & hidden; }
      bool is_inherited (void) const { return storage_class & inherited; }
      bool is_global (void) const { return storage_class & global; }
      bool is_persistent (void) const { return storage_class & persistent; }
      bool is_added_static (void) const {return storage_class & added_static; }

      void mark_local (void) { storage_class |= local; }
      void mark_automatic (void) { storage_class |= automatic; }
      void mark_formal (void) { storage_class |= formal; }
      void mark_hidden (void) { storage_class |= hidden; }
      void mark_inherited (void) { storage_class |= inherited; }
      void mark_global (void)
      {
        if (is_persistent ())
          error ("can't make persistent variable %s global", name.c_str ());

        storage_class |= global;
      }
      void mark_persistent (void)
      {
        if (is_global ())
          error ("can't make global variable %s persistent", name.c_str ());

        storage_class |= persistent;
      }
      void mark_added_static (void) { storage_class |= added_static; }

      void unmark_local (void) { storage_class &= ~local; }
      void unmark_automatic (void) { storage_class &= ~automatic; }
      void unmark_formal (void) { storage_class &= ~formal; }
      void unmark_hidden (void) { storage_class &= ~hidden; }
      void unmark_inherited (void) { storage_class &= ~inherited; }
      void unmark_global (void) { storage_class &= ~global; }
      void unmark_persistent (void) { storage_class &= ~persistent; }
      void unmark_added_static (void) { storage_class &= ~added_static; }

      void init_persistent (void);

      void invalidate (void)
      {
        valid = false;
      }

      void erase_persistent (void);

      OCTINTERP_API context_id active_context (void) const;

      scope_id decl_scope (void) const { return m_decl_scope; }

      void set_curr_fcn (octave_user_function *fcn)
      {
        curr_fcn = fcn;
      }

      symbol_record_rep * dup (scope_id new_scope) const
      {
        return new symbol_record_rep (new_scope, name, varval (),
                                      storage_class);
      }

      void dump (std::ostream& os, const std::string& prefix) const;

      scope_id m_decl_scope;

      octave_user_function *curr_fcn;

      std::string name;

      std::deque<octave_value> value_stack;

      unsigned int storage_class;

      //      fcn_info *finfo;

      bool valid;

      octave::refcount<size_t> count;

    private:

      octave_value& xglobal_varref (void);

      octave_value& xpersistent_varref (void);

      octave_value xglobal_varval (void) const;

      octave_value xpersistent_varval (void) const;
    };

  public:

    symbol_record (void);

    symbol_record (scope_id s, const std::string& nm = "",
                   const octave_value& v = octave_value (),
                   unsigned int sc = local)
      : rep (new symbol_record_rep (s, nm, v, sc)) { }

    symbol_record (const symbol_record& sr)
      : rep (sr.rep)
    {
      rep->count++;
    }

    symbol_record& operator = (const symbol_record& sr)
    {
      if (this != &sr)
        {
          if (--rep->count == 0)
            delete rep;

          rep = sr.rep;
          rep->count++;
        }

      return *this;
    }

    ~symbol_record (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    symbol_record dup (scope_id new_scope) const
    {
      return symbol_record (rep->dup (new_scope));
    }

    const std::string& name (void) const { return rep->name; }

    void rename (const std::string& new_name) { rep->name = new_name; }

    octave_value
    find (const octave_value_list& args = octave_value_list ()) const;

    void assign (const octave_value& value,
                 context_id context = xdefault_context)
    {
      rep->assign (value, context);
    }

    void assign (octave_value::assign_op op,
                 const std::string& type,
                 const std::list<octave_value_list>& idx,
                 const octave_value& value,
                 context_id context = xdefault_context)
    {
      rep->assign (op, type, idx, value, context);
    }

    void assign (octave_value::assign_op op, const octave_value& value,
                 context_id context = xdefault_context)
    {
      rep->assign (op, value, context);
    }

    void do_non_const_unary_op (octave_value::unary_op op)
    {
      rep->do_non_const_unary_op (op);
    }

    void do_non_const_unary_op (octave_value::unary_op op,
                                const std::string& type,
                                const std::list<octave_value_list>& idx)
    {
      rep->do_non_const_unary_op (op, type, idx);
    }

    // Delete when deprecated varref functions are removed.
    octave_value& varref (context_id context = xdefault_context)
    {
      return rep->varref (context);
    }

    octave_value varval (context_id context = xdefault_context) const
    {
      return rep->varval (context);
    }

    void push_context (scope_id sid) { rep->push_context (sid); }

    size_t pop_context (scope_id sid) { return rep->pop_context (sid); }

    void clear (void) { rep->clear (); }

    void clear (scope_id sid) { rep->clear (sid); }

    bool is_defined (context_id context = xdefault_context) const
    {
      return rep->is_defined (context);
    }

    bool is_undefined (context_id context = xdefault_context) const
    {
      return ! rep->is_defined (context);
    }

    bool is_valid (void) const
    {
      return rep->is_valid ();
    }

    bool is_variable (context_id context = xdefault_context) const
    {
      return rep->is_variable (context);
    }

    bool is_local (void) const { return rep->is_local (); }
    bool is_automatic (void) const { return rep->is_automatic (); }
    bool is_formal (void) const { return rep->is_formal (); }
    bool is_global (void) const { return rep->is_global (); }
    bool is_hidden (void) const { return rep->is_hidden (); }
    bool is_inherited (void) const { return rep->is_inherited (); }
    bool is_persistent (void) const { return rep->is_persistent (); }
    bool is_added_static (void) const { return rep->is_added_static (); }

    void mark_local (void) { rep->mark_local (); }
    void mark_automatic (void) { rep->mark_automatic (); }
    void mark_formal (void) { rep->mark_formal (); }
    void mark_hidden (void) { rep->mark_hidden (); }
    void mark_inherited (void) { rep->mark_inherited (); }
    void mark_global (void) { rep->mark_global (); }
    void mark_persistent (void) { rep->mark_persistent (); }
    void mark_added_static (void) { rep->mark_added_static (); }

    void unmark_local (void) { rep->unmark_local (); }
    void unmark_automatic (void) { rep->unmark_automatic (); }
    void unmark_formal (void) { rep->unmark_formal (); }
    void unmark_hidden (void) { rep->unmark_hidden (); }
    void unmark_inherited (void) { rep->unmark_inherited (); }
    void unmark_global (void) { rep->unmark_global (); }
    void unmark_persistent (void) { rep->unmark_persistent (); }
    void unmark_added_static (void) { rep->unmark_added_static (); }

    void init_persistent (void) { rep->init_persistent (); }

    void erase_persistent (void) { rep->erase_persistent (); }

    void invalidate (void) { rep->invalidate (); }

    context_id active_context (void) const { return rep->active_context (); }

    scope_id decl_scope (void) const { return rep->decl_scope (); }

    unsigned int xstorage_class (void) const { return rep->storage_class; }

    void set_curr_fcn (octave_user_function *fcn) { rep->set_curr_fcn (fcn); }

    void
    dump (std::ostream& os, const std::string& prefix = "") const
    {
      rep->dump (os, prefix);
    }

  private:

    symbol_record_rep *rep;

    symbol_record (symbol_record_rep *new_rep) : rep (new_rep) { }
  };

  static symbol_record dummy_symbol_record;

  // Always access a symbol from the current scope.
  // Useful for scripts, as they may be executed in more than one scope.
  class
  symbol_reference
  {
  public:

    symbol_reference (void) : m_scope (-1) { }

    symbol_reference (const symbol_record& record);

    symbol_reference (const symbol_record& record, scope_id curr_scope)
      : m_scope (curr_scope), m_sym (record)
    { }

    symbol_reference (const symbol_reference& ref) = default;

    symbol_reference& operator = (const symbol_reference& ref) = default;

    bool is_black_hole (void) const { return m_scope < 0; }

    // The name is the same regardless of scope.
    const std::string& name (void) const { return m_sym.name (); }

    symbol_record *operator-> (void)
    {
      update ();
      return &m_sym;
    }

    symbol_record *operator-> (void) const
    {
      update ();
      return &m_sym;
    }

    // can be used to place symbol_reference in maps, we don't overload < as
    // it doesn't make any sense for symbol_reference
    struct comparator
    {
      bool operator ()(const symbol_reference& lhs,
                       const symbol_reference& rhs) const
      {
        return lhs.name () < rhs.name ();
      }
    };
  private:

    void update (void) const;

    mutable scope_id m_scope;
    mutable symbol_record m_sym;
  };

  class
  fcn_info
  {
  public:

    typedef std::map<std::string, octave_value>::const_iterator
      str_val_const_iterator;
    typedef std::map<std::string, octave_value>::iterator str_val_iterator;

  private:

    class
    fcn_info_rep
    {
    public:

      fcn_info_rep (const std::string& nm)
        : name (nm), package_name (), local_functions (),
          private_functions (), class_constructors (), class_methods (),
          cmdline_function (), autoload_function (), function_on_path (),
          built_in_function (), count (1)
      {
        size_t pos = name.rfind ('.');

        if (pos != std::string::npos)
          {
            package_name = name.substr (0, pos);
            name = name.substr (pos+1);
          }
      }

      // No copying!

      fcn_info_rep (const fcn_info_rep&) = delete;

      fcn_info_rep& operator = (const fcn_info_rep&) = delete;

      ~fcn_info_rep (void) = default;

      octave_value install_local_function (const std::string& file_name);

      octave_value load_private_function (const std::string& dir_name);

      octave_value load_class_constructor (void);

      octave_value load_class_method (const std::string& dispatch_type);

      octave_value find (const octave_value_list& args, bool local_funcs);

      octave_value builtin_find (void);

      octave_value find_method (const std::string& dispatch_type);

      octave_value find_autoload (void);

      octave_value find_package (void);

      octave_value find_user_function (void);

      bool is_user_function_defined (void) const
      {
        return function_on_path.is_defined ();
      }

      octave_value find_function (const octave_value_list& args,
                                  bool local_funcs)
      {
        return find (args, local_funcs);
      }

      void install_cmdline_function (const octave_value& f)
      {
        cmdline_function = f;
      }

      void install_local_function (const octave_value& f,
                                   const std::string& file_name)
      {
        local_functions[file_name] = f;
      }

      void install_user_function (const octave_value& f)
      {
        function_on_path = f;
      }

      void install_built_in_function (const octave_value& f)
      {
        built_in_function = f;
      }

      void install_built_in_dispatch (const std::string& klass);

      template <typename T>
      void
      clear_map (std::map<T, octave_value>& map, bool force = false)
      {
        typename std::map<T, octave_value>::iterator p = map.begin ();

        while (p != map.end ())
          {
            if (force || ! p->second.islocked ())
              map.erase (p++);
            else
              p++;
          }
      }

      void clear_autoload_function (bool force = false)
      {
        if (force || ! autoload_function.islocked ())
          autoload_function = octave_value ();
      }

      // We also clear command line functions here, as these are both
      // "user defined"
      void clear_user_function (bool force = false)
      {
        clear_autoload_function (force);

        if (force || ! function_on_path.islocked ())
          function_on_path = octave_value ();

        if (force || ! cmdline_function.islocked ())
          cmdline_function = octave_value ();
      }

      void clear_mex_function (void)
      {
        if (function_on_path.is_mex_function ())
          clear_user_function ();
      }

      void clear_package (void)
      {
        package = octave_value ();
      }

      void clear (bool force = false)
      {
        clear_map (local_functions, force);
        clear_map (private_functions, force);
        clear_map (class_constructors, force);
        clear_map (class_methods, force);

        clear_autoload_function (force);
        clear_user_function (force);
        clear_package ();
      }

      void dump (std::ostream& os, const std::string& prefix) const;

      std::string full_name (void) const
      {
        if (package_name.empty ())
          return name;
        else
          return package_name + "." + name;
      }

      std::string name;

      std::string package_name;

      // File name to function object.
      std::map<std::string, octave_value> local_functions;

      // Directory name to function object.
      std::map<std::string, octave_value> private_functions;

      // Class name to function object.
      std::map<std::string, octave_value> class_constructors;

      // Dispatch type to function object.
      std::map<std::string, octave_value> class_methods;

      octave_value cmdline_function;

      octave_value autoload_function;

      octave_value function_on_path;

      octave_value package;

      octave_value built_in_function;

      octave::refcount<size_t> count;

    private:

      octave_value xfind (const octave_value_list& args, bool local_funcs);

      octave_value x_builtin_find (void);
    };

  public:

    fcn_info (const std::string& nm = "")
      : rep (new fcn_info_rep (nm)) { }

    fcn_info (const fcn_info& fi) : rep (fi.rep)
    {
      rep->count++;
    }

    fcn_info& operator = (const fcn_info& fi)
    {
      if (this != &fi)
        {
          if (--rep->count == 0)
            delete rep;

          rep = fi.rep;
          rep->count++;
        }

      return *this;
    }

    ~fcn_info (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    octave_value find (const octave_value_list& args = octave_value_list (),
                       bool local_funcs = true)
    {
      return rep->find (args, local_funcs);
    }

    octave_value builtin_find (void)
    {
      return rep->builtin_find ();
    }

    octave_value find_method (const std::string& dispatch_type) const
    {
      return rep->find_method (dispatch_type);
    }

    octave_value find_built_in_function (void) const
    {
      return rep->built_in_function;
    }

    octave_value find_cmdline_function (void) const
    {
      return rep->cmdline_function;
    }

    octave_value find_autoload (void)
    {
      return rep->find_autoload ();
    }

    octave_value find_user_function (void)
    {
      return rep->find_user_function ();
    }

    bool is_user_function_defined (void) const
    {
      return rep->is_user_function_defined ();
    }

    octave_value find_function (const octave_value_list& args
                                = octave_value_list (),
                                bool local_funcs = true)
    {
      return rep->find_function (args, local_funcs);
    }

    void install_cmdline_function (const octave_value& f)
    {
      rep->install_cmdline_function (f);
    }

    void install_local_function (const octave_value& f,
                                 const std::string& file_name)
    {
      rep->install_local_function (f, file_name);
    }

    void install_user_function (const octave_value& f)
    {
      rep->install_user_function (f);
    }

    void install_built_in_function (const octave_value& f)
    {
      rep->install_built_in_function (f);
    }

    void install_built_in_dispatch (const std::string& klass)
    {
      rep->install_built_in_dispatch (klass);
    }

    void clear (bool force = false) { rep->clear (force); }

    void clear_user_function (bool force = false)
    {
      rep->clear_user_function (force);
    }

    void clear_autoload_function (bool force = false)
    {
      rep->clear_autoload_function (force);
    }

    void clear_mex_function (void) { rep->clear_mex_function (); }

    void
    dump (std::ostream& os, const std::string& prefix = "") const
    {
      rep->dump (os, prefix);
    }

  private:

    fcn_info_rep *rep;
  };

  symbol_table (void)
    : m_scope_id_cache (), m_global_symbols (), m_all_scopes (),
      m_fcn_table (), m_class_precedence_table (), m_parent_map (),
      m_current_scope (xtop_scope)
  { }

  // No copying!

  symbol_table (const symbol_table&) = delete;

  symbol_table& operator = (const symbol_table&) = delete;

  ~symbol_table (void) = default;

  static scope_id global_scope (void) { return xglobal_scope; }
  static scope_id top_scope (void) { return xtop_scope; }

  scope_id current_scope (void) { return m_current_scope; }

  context_id current_context (void) { return xcurrent_context; }

  scope_id alloc_scope (void) { return m_scope_id_cache.alloc (); }

  void set_scope (scope_id sid)
  {
    set_scope_and_context (sid, 0);
  }

  void set_scope_and_context (scope_id sid, context_id context)
  {
    if (sid == xglobal_scope)
      error ("can't set scope to global");

    all_scopes_iterator p = m_all_scopes.find (sid);

    if (p == m_all_scopes.end ())
      {
        // Can only create a new scope if we are starting at the
        // base context.

        if (context != 0)
          error ("can't find scope %d", sid);

        scope *s = new scope (sid);

        if (! s)
          error ("failed to create scope %d", sid);

        m_all_scopes[sid] = s;
      }

    m_current_scope = sid;
    xcurrent_context = context;
  }

  void erase_scope (scope_id sid)
  {
    assert (sid != xglobal_scope);

    all_scopes_iterator p = m_all_scopes.find (sid);

    if (p != m_all_scopes.end ())
      {
        m_all_scopes.erase (p);

        free_scope (sid);
      }
  }

  void erase_subfunctions_in_scope (scope_id sid)
  {
    scope *s = get_scope (sid);

    if (s)
      s->erase_subfunctions ();
  }

  void mark_nested (scope_id sid)
  {
    scope *s = get_scope (sid);

    if (s)
      s->mark_nested ();
  }

  void
  mark_subfunctions_in_scope_as_private (scope_id sid,
                                         const std::string& class_name)
  {
    scope *s = get_scope (sid);

    if (s)
      s->mark_subfunctions_in_scope_as_private (class_name);
  }

  scope_id dup_scope (scope_id sid)
  {
    scope_id retval = -1;

    scope *s = get_scope (sid);

    if (s)
      {
        scope_id new_sid = alloc_scope ();

        m_all_scopes[new_sid] = s->dup (new_sid);

        retval = new_sid;
      }

    return retval;
  }

  std::list<scope_id> scopes (void)
  {
    return m_scope_id_cache.scopes ();
  }

  symbol_record
  find_symbol (const std::string& name, scope_id sid)
  {
    scope *s = get_scope (sid);

    return s ? s->find_symbol (name) : symbol_record (sid);
  }

  symbol_record find_symbol (const std::string& name)
  {
    return find_symbol (name, m_current_scope);
  }

  void
  inherit (scope_id sid, scope_id donor_sid, context_id donor_context)
  {
    scope *s = get_scope (sid);

    if (s)
      {
        while (donor_sid > 0)
          {
            scope *donor_scope = get_scope (donor_sid);

            if (donor_scope)
              {
                s->inherit (*donor_scope, donor_context);

                if (donor_scope->is_nested ())
                  donor_sid = donor_scope->parent_scope_id ();
                else
                  break;
              }
          }
      }
  }

  void
  inherit (scope_id sid, scope_id donor_sid)
  {
    inherit (sid, donor_sid, xcurrent_context);
  }

  void inherit (scope_id sid)
  {
    inherit (sid, m_current_scope);
  }

  bool at_top_level (void) { return m_current_scope == xtop_scope; }

  // Find a value corresponding to the given name in the table.
  octave_value
  find (const std::string& name,
        const octave_value_list& args = octave_value_list (),
        bool skip_variables = false,
        bool local_funcs = true);

  octave_value builtin_find (const std::string& name);

  // Insert a new name in the table.
  OCTAVE_DEPRECATED ("use 'get_scope' with 'insert (name)' instead")
  symbol_record& insert (const std::string& name, scope_id sid)
  {
    scope *s = get_scope (sid);

    return s ? s->insert (name) : symbol_table::dummy_symbol_record;
  }

  symbol_record& insert (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->insert (name) : symbol_table::dummy_symbol_record;
  }

  void rename (const std::string& old_name, const std::string& new_name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->rename (old_name, new_name);
  }

  void assign (const std::string& name, const octave_value& value,
               scope_id sid, context_id context, bool force_add)
  {
    scope *s = get_scope (sid);

    if (s)
      s->assign (name, value, context, force_add);
  }

  void assign (const std::string& name, const octave_value& value,
               scope_id sid, context_id context)
  {
    assign (name, value, sid, context, false);
  }

  void assign (const std::string& name, const octave_value& value,
               scope_id sid)
  {
    assign (name, value, sid, xdefault_context);
  }

  void assign (const std::string& name,
               const octave_value& value = octave_value ())
  {
    assign (name, value, m_current_scope);
  }

  // use 'assign' instead
  // octave_value&
  // varref (const std::string& name, scope_id sid = xcurrent_scope,
  //         context_id context = xdefault_context, bool force_add = false);

  // Convenience function to simplify
  // octave_user_function::bind_automatic_vars

  void force_assign (const std::string& name, const octave_value& value,
                     scope_id sid, context_id context)
  {
    assign (name, value, sid, context, true);
  }

  void force_assign (const std::string& name, const octave_value& value,
                     scope_id sid)
  {
    assign (name, value, sid, xdefault_context);
  }

  void force_assign (const std::string& name,
                     const octave_value& value = octave_value ())
  {
    assign (name, value, m_current_scope);
  }

  // use 'force_assign' instead
  // octave_value&
  // force_varref (const std::string& name, scope_id sid = xcurrent_scope,
  //               context_id context = xdefault_context);

  octave_value varval (const std::string& name, scope_id sid,
                       context_id context)
  {
    scope *s = get_scope (sid);

    return s ? s->varval (name, context) : octave_value ();
  }

  octave_value varval (const std::string& name, scope_id sid)
  {
    return varval (name, sid, xdefault_context);
  }

  octave_value varval (const std::string& name)
  {
    return varval (name, m_current_scope);
  }

  void
  global_assign (const std::string& name,
                 const octave_value& value = octave_value ())

  {
    global_symbols_iterator p = m_global_symbols.find (name);

    if (p == m_global_symbols.end ())
      m_global_symbols[name] = value;
    else
      p->second = value;
  }

  // use 'global_assign' instead
  // octave_value&
  // global_varref (const std::string& name);

  octave_value
  global_varval (const std::string& name)
  {
    global_symbols_const_iterator p = m_global_symbols.find (name);

    return (p != m_global_symbols.end ()) ? p->second : octave_value ();
  }

  void
  top_level_assign (const std::string& name,
                    const octave_value& value = octave_value ())
  {
    assign (name, value, top_scope (), 0);
  }

  // use 'top_level_assign' instead
  // octave_value&
  // top_level_varref (const std::string& name);

  octave_value
  top_level_varval (const std::string& name)
  {
    return varval (name, top_scope (), 0);
  }

  void
  persistent_assign (const std::string& name, scope_id sid,
                     const octave_value& value = octave_value ())
  {
    scope *s = get_scope (sid);

    if (s)
      s->persistent_assign (name, value);
  }

  void
  persistent_assign (const std::string& name,
                     const octave_value& value = octave_value ())
  {
    persistent_assign (name, m_current_scope, value);
  }

  // use 'persistent_assign' instead
  // octave_value&
  // persistent_varref (const std::string& name);

  octave_value persistent_varval (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->persistent_varval (name) : octave_value ();
  }

  void erase_persistent (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->erase_persistent (name);
  }

  OCTAVE_DEPRECATED ("use 'get_scope' with 'is_variable (name)' instead")
  bool is_variable (const std::string& name, scope_id sid)
  {
    scope *s = get_scope (sid);

    return s ? s->is_variable (name) : false;
  }

  bool is_variable (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->is_variable (name) : false;
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

  // Install subfunction FCN named NAME.  SCOPE is the scope of the
  // primary function corresponding to this subfunction.

  void install_subfunction (const std::string& name,
                            const octave_value& fcn,
                            scope_id parent_scope)
  {
    scope *s = get_scope (parent_scope);

    if (s)
      s->install_subfunction (name, fcn);
  }

  void install_nestfunction (const std::string& name,
                             const octave_value& fcn,
                             scope_id parent_scope)
  {
    scope *s = get_scope (parent_scope);

    if (s)
      s->install_subfunction (name, fcn, true);
  }

  void update_nest (scope_id sid)
  {
    scope *s = get_scope (sid);

    if (s)
      s->update_nest ();
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

  void clear (const std::string& name)
  {
    clear_variable (name);
  }

  void clear_all (bool force = false)
  {
    clear_variables ();

    clear_global_pattern ("*");

    clear_functions (force);
  }

  // This is written as two separate functions instead of a single
  // function with default values so that it will work properly with
  // unwind_protect.

  void clear_variables (scope_id sid)
  {
    scope *s = get_scope (sid);

    if (s)
      s->clear_variables ();
  }

  void clear_variables (void)
  {
    clear_variables (m_current_scope);
  }

  void clear_objects (void)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->clear_objects ();
  }

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

  void clear_global (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->clear_global (name);
  }

  void clear_variable (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->clear_variable (name);
  }

  void clear_symbol (const std::string& name)
  {
    // FIXME: are we supposed to do both here?

    clear_variable (name);
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

  void clear_global_pattern (const std::string& pat)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->clear_global_pattern (pat);
  }

  void clear_variable_pattern (const std::string& pat)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->clear_variable_pattern (pat);
  }

  void clear_variable_regexp (const std::string& pat)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->clear_variable_regexp (pat);
  }

  void clear_symbol_pattern (const std::string& pat)
  {
    // FIXME: are we supposed to do both here?

    clear_variable_pattern (pat);
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

  void push_context (void)
  {
    if (m_current_scope == xtop_scope)
      error ("invalid call to symtab::push_context");

    scope *s = get_scope (m_current_scope);

    if (s)
      s->push_context ();
  }

  // This is written as two separate functions instead of a single
  // function with default values so that it will work properly with
  // unwind_protect.

  void pop_context (void)
  {
    if (m_current_scope == xtop_scope)
      error ("invalid call to symtab::pop_context");

    scope *s = get_scope (m_current_scope);

    if (s)
      s->pop_context ();
  }

  // For unwind_protect where a pointer argument is needed.

  void pop_context (void *) { pop_context (); }

  void mark_automatic (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->mark_automatic (name);
  }

  void mark_hidden (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->mark_hidden (name);
  }

  void mark_global (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    if (s)
      s->mark_global (name);
  }

  // exclude: Storage classes to exclude, you can OR them together
  std::list<symbol_record>
  all_variables (scope_id sid, context_id context, bool defined_only,
                 unsigned int exclude)
  {
    scope *s = get_scope (sid);

    return (s
            ? s->all_variables (context, defined_only, exclude)
            : std::list<symbol_record> ());
  }

  std::list<symbol_record>
  all_variables (scope_id sid, context_id context, bool defined_only)
  {
    return all_variables (sid, context, defined_only, symbol_record::hidden);
  }

  std::list<symbol_record>
  all_variables (scope_id sid, context_id context)
  {
    return all_variables (sid, context, true);
  }

  std::list<symbol_record>
  all_variables (scope_id sid)
  {
    return all_variables (sid, xdefault_context);
  }

  std::list<symbol_record>
  all_variables (void)
  {
    return all_variables (m_current_scope);
  }

  std::list<symbol_record> glob (const std::string& pattern)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->glob (pattern) : std::list<symbol_record> ();
  }

  std::list<symbol_record> regexp (const std::string& pattern)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->regexp (pattern) : std::list<symbol_record> ();
  }

  std::list<symbol_record> glob_variables (const std::string& pattern)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->glob (pattern, true) : std::list<symbol_record> ();
  }

  std::list<symbol_record> regexp_variables (const std::string& pattern)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->regexp (pattern, true) : std::list<symbol_record> ();
  }

  std::list<symbol_record>
  glob_global_variables (const std::string& pattern)
  {
    std::list<symbol_record> retval;

    glob_match pat (pattern);

    for (const auto& nm_val : m_global_symbols)
      {
        // We generate a list of symbol_record objects so that the results from
        // glob_variables and glob_global_variables may be handled the same
        // way.
        if (pat.match (nm_val.first))
          retval.push_back (symbol_record (xglobal_scope,
                                           nm_val.first, nm_val.second,
                                           symbol_record::global));
      }

    return retval;
  }

  std::list<symbol_record>
  regexp_global_variables (const std::string& pattern)
  {
    std::list<symbol_record> retval;

    octave::regexp pat (pattern);

    for (const auto& nm_val : m_global_symbols)
      {
        // We generate a list of symbol_record objects so that the results from
        // regexp_variables and regexp_global_variables may be handled the same
        // way.
        if (pat.is_match (nm_val.first))
          retval.push_back (symbol_record (xglobal_scope,
                                           nm_val.first, nm_val.second,
                                           symbol_record::global));
      }

    return retval;
  }

  std::list<symbol_record> glob_variables (const string_vector& patterns)
  {
    std::list<symbol_record> retval;

    size_t len = patterns.numel ();

    for (size_t i = 0; i < len; i++)
      {
        std::list<symbol_record> tmp = glob_variables (patterns[i]);

        retval.insert (retval.begin (), tmp.begin (), tmp.end ());
      }

    return retval;
  }

  std::list<symbol_record> regexp_variables (const string_vector& patterns)
  {
    std::list<symbol_record> retval;

    size_t len = patterns.numel ();

    for (size_t i = 0; i < len; i++)
      {
        std::list<symbol_record> tmp = regexp_variables (patterns[i]);

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
    std::list<std::string> retval;

    for (const auto& nm_val : m_global_symbols)
      retval.push_back (nm_val.first);

    retval.sort ();

    return retval;
  }

  std::list<std::string> top_level_variable_names (void)
  {
    scope *s = get_scope (xtop_scope);

    return s ? s->variable_names () : std::list<std::string> ();
  }

  std::list<std::string> variable_names (void)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->variable_names () : std::list<std::string> ();
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

  bool is_local_variable (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->is_local_variable (name) : false;
  }

  bool is_global (const std::string& name)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->is_global (name) : false;
  }

  std::list<workspace_element> workspace_info (void)
  {
    scope *s = get_scope (m_current_scope);

    return s ? s->workspace_info () : std::list<workspace_element> ();
  }

  void dump (std::ostream& os, scope_id sid);

  void dump_global (std::ostream& os);

  void dump_functions (std::ostream& os);

  void cache_name (scope_id sid, const std::string& name)
  {
    scope *s = get_scope (sid, false);

    if (s)
      s->cache_name (name);
  }

  void lock_subfunctions (scope_id sid)
  {
    scope *s = get_scope (sid);

    if (s)
      s->lock_subfunctions ();
  }

  void unlock_subfunctions (scope_id sid)
  {
    scope *s = get_scope (sid);

    if (s)
      s->unlock_subfunctions ();
  }

  std::map<std::string, octave_value>
  subfunctions_defined_in_scope (scope_id sid)
  {
    scope *s = get_scope (sid);

    return (s
            ? s->subfunctions ()
            : std::map<std::string, octave_value> ());
  }

  void free_scope (scope_id sid)
  {
    if (sid == xglobal_scope || sid == xtop_scope)
      error ("can't free global or top-level scopes!");

    m_scope_id_cache.free (sid);
  }

  void stash_dir_name_for_subfunctions (scope_id sid,
                                        const std::string& dir_name)
  {
    scope *s = get_scope (sid);

    if (s)
      s->stash_dir_name_for_subfunctions (dir_name);
  }

  void set_parent (scope_id child_scope_id, scope_id parent_scope_id)
  {
    scope *child_scope = get_scope (child_scope_id);

    if (child_scope)
      child_scope->set_parent (get_scope (parent_scope_id));
  }

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

  OCTAVE_DEPRECATED ("use 'get_scope' with 'get_curr_fcn (name)' instead")
  octave_user_function * get_curr_fcn (scope_id sid)
  {
    scope *s = get_scope (sid);
    return s->function ();
  }

  octave_user_function * get_curr_fcn (void)
  {
    scope *s = get_scope (m_current_scope);
    return s->function ();
  }

  OCTAVE_DEPRECATED ("set_curr_fcn")
  void set_curr_fcn (octave_user_function *curr_fcn, scope_id sid)
  {
    assert (sid != xtop_scope && sid != xglobal_scope);
    scope *s = get_scope (sid);
    // FIXME: normally, functions should not usurp each other's scope.
    // If for any incredible reason this is needed, call
    // set_user_function (0, scope) first.  This may cause problems with
    // nested functions, as the curr_fcn of symbol_records must be updated.
    assert (s->function () == 0 || curr_fcn == 0);
    s->set_function (curr_fcn);
  }

  scope * get_scope (scope_id sid, bool create = true)
  {
    scope *retval = nullptr;

    bool ok = true;

    if (sid == xglobal_scope)
      error ("can't get global scope");

    all_scopes_iterator p = m_all_scopes.find (sid);

    if (p == m_all_scopes.end ())
      {
        if (create)
          {
            retval = new scope (sid);

            if (retval)
              {
                m_all_scopes[sid] = retval;

                if (sid == xtop_scope)
                  retval->cache_name ("top-level");
              }
            else
              ok = false;
          }
        else
          ok = false;
      }
    else
      retval = p->second;

    if (! ok)
      error ("unable to %s scope object for scope id %d!",
             create ? "create" : "find", sid);

    return retval;
  }

  void cleanup (void);

  class scope
  {
  public:

    typedef std::map<std::string, symbol_table::symbol_record>::const_iterator
      table_const_iterator;
    typedef std::map<std::string, symbol_table::symbol_record>::iterator
      table_iterator;

    typedef std::map<std::string, octave_value>::const_iterator
      m_persistent_symbols_const_iterator;
    typedef std::map<std::string, octave_value>::iterator
      m_persistent_symbols_iterator;

    typedef std::map<std::string, octave_value>::const_iterator
      subfunctions_const_iterator;
    typedef std::map<std::string, octave_value>::iterator subfunctions_iterator;

    scope (scope_id sid)
      : m_scope (sid), m_name (), m_symbols (), m_children (),
        m_subfunctions (), m_parent (0), m_fcn (0), m_is_nested (false),
        m_is_static (false), m_persistent_symbols ()
    { }

    // No copying!

    scope (const scope&) = delete;

    scope& operator = (const scope&) = delete;

    ~scope (void) = default;

    void insert_symbol_record (const symbol_table::symbol_record& sr)
    {
      m_symbols[sr.name ()] = sr;
    }

    bool is_nested (void) const { return m_is_nested; }

    void mark_nested (void) { m_is_nested = true; }

    scope_id parent_scope_id (void) const
    {
      return m_parent ? m_parent->m_scope : -1;
    }

    scope * dup (scope_id new_sid) const
    {
      scope *new_scope = new scope (new_sid);

      for (const auto& nm_sr : m_symbols)
        new_scope->insert_symbol_record (nm_sr.second.dup (new_sid));

      new_scope->m_parent = m_parent;

      return new_scope;
    }

    symbol_table::symbol_record find_symbol (const std::string& name)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        return insert (name);
      else
        return p->second;
    }

    void inherit (scope& donor_table, context_id donor_context)
    {
      for (auto& nm_sr : m_symbols)
        {
          symbol_table::symbol_record& sr = nm_sr.second;

          if (! (sr.is_automatic () || sr.is_formal ()))
            {
              std::string nm = sr.name ();

              if (nm != "__retval__")
                {
                  octave_value val = donor_table.varval (nm, donor_context);

                  if (val.is_defined ())
                    {
                      sr.assign (val, 0);

                      sr.mark_inherited ();
                    }
                }
            }
        }
    }

    octave_value
    find (const std::string& name, const octave_value_list& args,
          bool skip_variables, bool local_funcs);

    octave_value builtin_find (const std::string& name);

    symbol_table::symbol_record&
    insert (const std::string& name, bool force_add = false)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_table::symbol_record ret (m_scope, name);

          if (m_is_nested && m_parent && m_parent->look_nonlocal (name, ret))
            return m_symbols[name] = ret;
          else
            {
              if (m_is_static && ! force_add)
                ret.mark_added_static ();

              return m_symbols[name] = ret;
            }
        }
      else
        return p->second;
    }

    void rename (const std::string& old_name, const std::string& new_name)
    {
      table_iterator p = m_symbols.find (old_name);

      if (p != m_symbols.end ())
        {
          symbol_table::symbol_record sr = p->second;

          sr.rename (new_name);

          m_symbols.erase (p);

          m_symbols[new_name] = sr;
        }
    }

    void assign (const std::string& name, const octave_value& value,
                 context_id context, bool force_add)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_table::symbol_record& sr = insert (name, force_add);

          sr.assign (value, context);
        }
      else
        p->second.assign (value, context);
    }

    void assign (const std::string& name,
                 const octave_value& value = octave_value ())
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_table::symbol_record& sr = insert (name, false);

          sr.assign (value);
        }
      else
        p->second.assign (value);
    }

    void force_assign (const std::string& name, const octave_value& value)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_table::symbol_record& sr = insert (name, true);

          sr.assign (value);
        }
      else
        p->second.assign (value);
    }

    // Use assign (name, value, context, force_add) instead.
    // Delete when deprecated varref functions are removed.
    octave_value&
    varref (const std::string& name, context_id context, bool force_add)
    {
      table_iterator p = m_symbols.find (name);

      if (p == m_symbols.end ())
        {
          symbol_table::symbol_record& sr = insert (name, force_add);

          return sr.varref (context);
        }
      else
        return p->second.varref (context);
    }

    octave_value varval (const std::string& name, context_id context) const
    {
      table_const_iterator p = m_symbols.find (name);

      return (p != m_symbols.end ()
              ? p->second.varval (context) : octave_value ());
    }

    octave_value varval (const std::string& name) const
    {
      table_const_iterator p = m_symbols.find (name);

      return p != m_symbols.end () ? p->second.varval () : octave_value ();
    }

    void persistent_assign (const std::string& name, const octave_value& value)
    {
      m_persistent_symbols_iterator p = m_persistent_symbols.find (name);

      if (p == m_persistent_symbols.end ())
        m_persistent_symbols[name] = value;
      else
        p->second = value;
    }

    // Use persistent_assign (name, value) instead.
    // Delete when deprecated varref functions are removed.
    octave_value& persistent_varref (const std::string& name)
    {
      m_persistent_symbols_iterator p = m_persistent_symbols.find (name);

      return (p == m_persistent_symbols.end ()
              ? m_persistent_symbols[name] : p->second);
    }

    octave_value persistent_varval (const std::string& name)
    {
      m_persistent_symbols_const_iterator p = m_persistent_symbols.find (name);

      return (p != m_persistent_symbols.end ()) ? p->second : octave_value ();
    }

    void erase_persistent (const std::string& name)
    {
      m_persistent_symbols_iterator p = m_persistent_symbols.find (name);

      if (p != m_persistent_symbols.end ())
        m_persistent_symbols.erase (p);
    }

    bool is_variable (const std::string& name) const
    {
      bool retval = false;

      table_const_iterator p = m_symbols.find (name);

      if (p != m_symbols.end ())
        {
          const symbol_table::symbol_record& sr = p->second;

          retval = sr.is_variable ();
        }

      return retval;
    }

    void push_context (void)
    {
      for (auto& nm_sr : m_symbols)
        nm_sr.second.push_context (m_scope);
    }

    void pop_context (void)
    {
      table_iterator tbl_it = m_symbols.begin ();

      while (tbl_it != m_symbols.end ())
        {
          if (tbl_it->second.pop_context (m_scope) == 0)
            m_symbols.erase (tbl_it++);
          else
            tbl_it++;
        }
    }

    void clear_variables (void)
    {
      for (auto& nm_sr : m_symbols)
        nm_sr.second.clear (m_scope);
    }

    void clear_objects (void)
    {
      for (auto& nm_sr : m_symbols)
        {
          symbol_table::symbol_record& sr = nm_sr.second;
          octave_value val = sr.varval ();
          if (val.is_object ())
            nm_sr.second.clear (m_scope);
        }
    }

    void clear_global (const std::string& name);

    void clear_variable (const std::string& name)
    {
      table_iterator p = m_symbols.find (name);

      if (p != m_symbols.end ())
        p->second.clear (m_scope);
    }

    void clear_global_pattern (const std::string& pat);

    void clear_variable_pattern (const std::string& pat)
    {
      glob_match pattern (pat);

      for (auto& nm_sr : m_symbols)
        {
          symbol_table::symbol_record& sr = nm_sr.second;

          if (sr.is_defined () || sr.is_global ())
            {
              if (pattern.match (sr.name ()))
                sr.clear (m_scope);
            }
        }
    }

    void clear_variable_regexp (const std::string& pat)
    {
      octave::regexp pattern (pat);

      for (auto& nm_sr : m_symbols)
        {
          symbol_table::symbol_record& sr = nm_sr.second;

          if (sr.is_defined () || sr.is_global ())
            {
              if (pattern.is_match (sr.name ()))
                sr.clear (m_scope);
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

    std::list<symbol_table::symbol_record>
    all_variables (context_id context, bool defined_only,
                   unsigned int exclude) const
    {
      std::list<symbol_table::symbol_record> retval;

      for (const auto& nm_sr : m_symbols)
        {
          const symbol_table::symbol_record& sr = nm_sr.second;

          if ((defined_only && ! sr.is_defined (context))
              || (sr.xstorage_class () & exclude))
            continue;

          retval.push_back (sr);
        }

      return retval;
    }

    std::list<symbol_table::symbol_record>
    glob (const std::string& pattern, bool vars_only = false) const
    {
      std::list<symbol_table::symbol_record> retval;

      glob_match pat (pattern);

      for (const auto& nm_sr : m_symbols)
        {
          if (pat.match (nm_sr.first))
            {
              const symbol_table::symbol_record& sr = nm_sr.second;

              if (vars_only && ! sr.is_variable ())
                continue;

              retval.push_back (sr);
            }
        }

      return retval;
    }

    std::list<symbol_table::symbol_record>
    regexp (const std::string& pattern, bool vars_only = false) const
    {
      std::list<symbol_table::symbol_record> retval;

      octave::regexp pat (pattern);

      for (const auto& nm_sr : m_symbols)
        {
          if (pat.is_match (nm_sr.first))
            {
              const symbol_table::symbol_record& sr = nm_sr.second;

              if (vars_only && ! sr.is_variable ())
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
          if (nm_sr.second.is_variable ())
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
              && p->second.is_defined ());
    }

    bool is_global (const std::string& name) const
    {
      table_const_iterator p = m_symbols.find (name);

      return p != m_symbols.end () && p->second.is_global ();
    }

    void install_subfunction (const std::string& name,
                              const octave_value& fval,
                              bool is_nested = false);

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

    void stash_dir_name_for_subfunctions (const std::string& dir_name);

    void mark_subfunctions_in_scope_as_private (const std::string& class_name);

    std::list<workspace_element> workspace_info (void) const;

    void dump (std::ostream& os);

    std::string name (void) const { return m_name; }

    void cache_name (const std::string& name) { m_name = name; }

    octave_user_function *function (void) { return m_fcn; }

    void set_function (octave_user_function *fcn) { m_fcn = fcn; }

    void set_parent (scope *p) { m_parent = p; }

    void update_nest (void);

    bool look_nonlocal (const std::string& name,
                        symbol_table::symbol_record& result)
    {
      table_iterator p = m_symbols.find (name);
      if (p == m_symbols.end ())
        {
          if (m_is_nested && m_parent)
            return m_parent->look_nonlocal (name, result);
        }
      else if (! p->second.is_automatic ())
        {
          result = p->second;
          return true;
        }

      return false;
    }

  private:

    // The ID for this scope.
    scope_id m_scope;

    // Name for this scope (usually the corresponding filename of the
    // function corresponding to the scope).
    std::string m_name;

    // Map from symbol names to symbol info.
    std::map<std::string, symbol_table::symbol_record> m_symbols;

    // Child nested functions.
    std::vector<scope*> m_children;

    // Map from symbol names to subfunctions.
    std::map<std::string, octave_value> m_subfunctions;

    // Parent of nested function (may be null).
    scope *m_parent;

    // The associated user code (may be null).
    octave_user_function *m_fcn;

    // If true, then this scope belongs to a nested function.
    bool m_is_nested;

    // If true then no variables can be added.
    bool m_is_static;

    // Map from names of persistent variables to values.
    std::map<std::string, octave_value> m_persistent_symbols;
  };

private:

  typedef std::map<std::string, octave_value>::const_iterator
    global_symbols_const_iterator;
  typedef std::map<std::string, octave_value>::iterator
    global_symbols_iterator;

  typedef std::map<scope_id, scope*>::const_iterator all_scopes_const_iterator;
  typedef std::map<scope_id, scope*>::iterator
    all_scopes_iterator;

  typedef std::map<std::string, fcn_info>::const_iterator
    fcn_table_const_iterator;
  typedef std::map<std::string, fcn_info>::iterator
    fcn_table_iterator;

  scope_id_cache m_scope_id_cache;

  // Map from names of global variables to values.
  std::map<std::string, octave_value> m_global_symbols;

  // Map from scope id to symbol table scopes.
  std::map<scope_id, scope*> m_all_scopes;

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

  scope_id m_current_scope;

  static const scope_id xglobal_scope = 0;
  static const scope_id xtop_scope = 1;

  static const context_id xdefault_context
    = std::numeric_limits<context_id>::max ();

  static context_id xcurrent_context;

  fcn_info * get_fcn_info (const std::string& name)
  {
    fcn_table_iterator p = m_fcn_table.find (name);
    return p != m_fcn_table.end () ? &p->second : 0;
  }
};

extern bool out_of_date_check (octave_value& function,
                               const std::string& dispatch_type = "",
                               bool check_relative = true);

extern OCTINTERP_API std::string
get_dispatch_type (const octave_value_list& args);

extern OCTINTERP_API std::string
get_dispatch_type (const octave_value_list& args, builtin_type_t& builtin_type);

#endif
