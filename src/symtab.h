/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2003,
              2004, 2005, 2006, 2007, 2008 John W. Eaton
  
This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_symtab_h)
#define octave_symtab_h 1

#include <deque>
#include <list>
#include <map>
#include <set>
#include <string>

#include "glob-match.h"

class tree_argument_list;

#include "oct-obj.h"
#include "ov.h"

class
OCTINTERP_API
symbol_table
{
public:

  typedef int scope_id;
  typedef size_t context_id;

  class
  symbol_record
  {
  public:

    // generic variable
    static const unsigned int local = 1;

    // varargin, argn, .nargin., .nargout.
    // (FIXME -- is this really used now?)
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

  private:

    class
    symbol_record_rep
    {
    public:

      symbol_record_rep (const std::string& nm, const octave_value& v,
			 unsigned int sc)
	: name (nm), value_stack (), storage_class (sc), count (1)
      {
	value_stack.push_back (v);
      }

      octave_value& varref (void)
      {
	if (is_global ())
	  return symbol_table::global_varref (name);
	else if (is_persistent ())
	  return symbol_table::persistent_varref (name);
	else
	  {
	    context_id n = value_stack.size ();
	    while (n++ <= symbol_table::xcurrent_context)
	      value_stack.push_back (octave_value ());

	    return value_stack[symbol_table::xcurrent_context];
	  }
      }

      octave_value varval (void) const
      {
	if (is_global ())
	  return symbol_table::global_varval (name);
	else if (is_persistent ())
	  return symbol_table::persistent_varval (name);
	else
	  {
	    if (symbol_table::xcurrent_context < value_stack.size ())
	      return value_stack[symbol_table::xcurrent_context];
	    else
	      return octave_value ();
	  }
      }

      void push_context (void)
      {
	if (! (is_persistent () || is_global ()))
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

      size_t pop_context (void)
      {
	size_t retval = 1;

	if (! (is_persistent () || is_global ()))
	  {
	    value_stack.pop_back ();
	    retval = value_stack.size ();
	  }

	return retval;
      }

      void clear (void)
      {
	if (! (is_hidden () || is_inherited ()))
	  {
	    if (is_global ())
	      unmark_global ();

	    if (is_persistent ())
	      {
		symbol_table::persistent_varref (name) = varval ();
		unmark_persistent ();
	      }

	    varref () = octave_value ();
	  }
      }

      bool is_defined (void) const { return varval ().is_defined (); }

      bool is_variable (void) const
      {
	return (storage_class != local || is_defined ());
      }

      bool is_local (void) const { return storage_class & local; }
      bool is_automatic (void) const { return storage_class & automatic; }
      bool is_formal (void) const { return storage_class & formal; }
      bool is_hidden (void) const { return storage_class & hidden; }
      bool is_inherited (void) const { return storage_class & inherited; }
      bool is_global (void) const { return storage_class & global; }
      bool is_persistent (void) const { return storage_class & persistent; }

      void mark_local (void) { storage_class |= local; }
      void mark_automatic (void) { storage_class |= automatic; }
      void mark_formal (void) { storage_class |= formal; }
      void mark_hidden (void) { storage_class |= hidden; }
      void mark_inherited (void) { storage_class |= inherited; }
      void mark_global (void)
      {
	if (is_persistent ())
	  error ("can't make persistent variable %s global", name.c_str ());
	else
	  storage_class |= global;
      }
      void mark_persistent (void)
      {
	if (is_global ())
	  error ("can't make global variable %s persistent", name.c_str ());
	else
	  storage_class |= persistent;
      }

      void unmark_local (void) { storage_class &= ~local; }
      void unmark_automatic (void) { storage_class &= ~automatic; }
      void unmark_formal (void) { storage_class &= ~formal; }
      void unmark_hidden (void) { storage_class &= ~hidden; }
      void unmark_inherited (void) { storage_class &= ~inherited; }
      void unmark_global (void) { storage_class &= ~global; }
      void unmark_persistent (void) { storage_class &= ~persistent; }

      void init_persistent (void)
      {
	if (! is_defined ())
	  {
	    mark_persistent ();

	    varref () = symbol_table::persistent_varval (name);
	  }
	// FIXME -- this causes trouble with recursive calls.
	// else
	//   error ("unable to declare existing variable persistent");
      }

      void erase_persistent (void)
      {
	unmark_persistent ();
	symbol_table::erase_persistent (name);
      }

      symbol_record_rep *dup (void)
      {
	return new symbol_record_rep (name, varval (), storage_class);
      }

      std::string name;

      std::deque<octave_value> value_stack;

      unsigned int storage_class;

      size_t count;

    private:

      // No copying!

      symbol_record_rep (const symbol_record_rep& ov);

      symbol_record_rep& operator = (const symbol_record_rep&);
    };

  public:

    symbol_record (const std::string& nm = std::string (),
		   const octave_value& v = octave_value (),
		   unsigned int sc = local)
      : rep (new symbol_record_rep (nm, v, sc)) { }

    symbol_record (const symbol_record& sr)
      : rep (sr.rep)
    { 
      rep->count++;
    }

    symbol_record& operator = (const symbol_record& sr)
    {
      if (this != &sr)
	{
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

    symbol_record dup (void) const { return symbol_record (rep->dup ()); }

    std::string name (void) const { return rep->name; }

    octave_value
    find (tree_argument_list *args, const string_vector& arg_names,
	  octave_value_list& evaluated_args, bool& args_evaluated) const;

    octave_value& varref (void) { return rep->varref (); }

    octave_value varval (void) const { return rep->varval (); }

    void push_context (void) { rep->push_context (); }

    size_t pop_context (void) { return rep->pop_context (); }

    void clear (void) { rep->clear (); }

    bool is_defined (void) const { return rep->is_defined (); }
    bool is_variable (void) const { return rep->is_variable (); }

    bool is_local (void) const { return rep->is_local (); }
    bool is_automatic (void) const { return rep->is_automatic (); }
    bool is_formal (void) const { return rep->is_formal (); }
    bool is_global (void) const { return rep->is_global (); }
    bool is_hidden (void) const { return rep->is_hidden (); }
    bool is_inherited (void) const { return rep->is_inherited (); }
    bool is_persistent (void) const { return rep->is_persistent (); }

    void mark_local (void) { rep->mark_local (); }
    void mark_automatic (void) { rep->mark_automatic (); }
    void mark_formal (void) { rep->mark_formal (); }
    void mark_hidden (void) { rep->mark_hidden (); }
    void mark_inherited (void) { rep->mark_inherited (); }
    void mark_global (void) { rep->mark_global (); }
    void mark_persistent (void) { rep->mark_persistent (); }

    void unmark_local (void) { rep->unmark_local (); }
    void unmark_automatic (void) { rep->unmark_automatic (); }
    void unmark_formal (void) { rep->unmark_formal (); }
    void unmark_hidden (void) { rep->unmark_hidden (); }
    void unmark_inherited (void) { rep->unmark_inherited (); }
    void unmark_global (void) { rep->unmark_global (); }
    void unmark_persistent (void) { rep->unmark_persistent (); }

    void init_persistent (void) { rep->init_persistent (); }

    void erase_persistent (void) { rep->erase_persistent (); }

    unsigned int xstorage_class (void) const { return rep->storage_class; }

  private:

    symbol_record_rep *rep;

    symbol_record (symbol_record_rep *new_rep) : rep (new_rep) { }
  };

  class
  fcn_info
  {
  public:

    typedef std::map<std::string, std::string> dispatch_map_type;

    typedef std::map<scope_id, octave_value>::const_iterator const_scope_val_iterator;
    typedef std::map<scope_id, octave_value>::iterator scope_val_iterator;

    typedef std::map<std::string, octave_value>::const_iterator const_str_val_iterator;
    typedef std::map<std::string, octave_value>::iterator str_val_iterator;

    typedef dispatch_map_type::const_iterator const_dispatch_map_iterator;
    typedef dispatch_map_type::iterator dispatch_map_iterator;

  private:

    class
    fcn_info_rep
    {
    public:

      fcn_info_rep (const std::string& nm)
	: name (nm), subfunctions (), private_functions (),
	  class_constructors (), class_methods (), cmdline_function (),
	  autoload_function (), function_on_path (), built_in_function (),
	  count (1) { }

      octave_value load_private_function (const std::string& dir_name);

      octave_value load_class_constructor (void);

      octave_value load_class_method (const std::string& dispatch_type);

      octave_value
      find (tree_argument_list *args, const string_vector& arg_names,
	    octave_value_list& evaluated_args, bool& args_evaluated);

      octave_value find_method (const std::string& dispatch_type);

      octave_value find_autoload (void);

      octave_value find_user_function (void);

      bool is_user_function_defined (void) const
      {
	return function_on_path.is_defined ();
      }

      octave_value find_function (void)
      {
	octave_value_list args;

	return find_function (args);
      }

      octave_value find_function (const octave_value_list& args)
      {
	string_vector arg_names;
	octave_value_list evaluated_args = args;
	bool args_evaluated = false;

	return find (0, arg_names, evaluated_args, args_evaluated);
      }

      void install_cmdline_function (const octave_value& f)
      {
	cmdline_function = f;
      }

      void install_subfunction (const octave_value& f, scope_id scope)
      {
	subfunctions[scope] = f;
      }

      void install_user_function (const octave_value& f)
      {
	function_on_path = f;
      }

      void install_built_in_function (const octave_value& f)
      {
	built_in_function = f;
      }

      template <class T>
      void
      clear_unlocked (std::map<T, octave_value>& map)
      {
	typename std::map<T, octave_value>::iterator p = map.begin ();

	while (p != map.end ())
	  {
	    if (p->second.islocked ())
	      p++;
	    else
	      map.erase (p++);
	  }
      }

      void clear_cmdline_function (void)
      {
	if (! cmdline_function.islocked ())
	  cmdline_function = octave_value ();
      }

      void clear_autoload_function (void)
      {
	if (! autoload_function.islocked ())
	  autoload_function = octave_value ();
      }

      // FIXME -- should this also clear the cmdline and other "user
      // defined" functions?
      void clear_user_function (void)
      {
	if (! function_on_path.islocked ())
	  function_on_path = octave_value ();
      }

      void clear_mex_function (void)
      {
	if (function_on_path.is_mex_function ())
	  clear_user_function ();
      }

      void clear (void)
      {
	clear_unlocked (subfunctions);
	clear_unlocked (private_functions);
	clear_unlocked (class_constructors);
	clear_unlocked (class_methods);
	clear_cmdline_function ();
	clear_autoload_function ();
	clear_user_function ();
      }

      void add_dispatch (const std::string& type, const std::string& fname)
      {
	dispatch_map[type] = fname;
      }

      void clear_dispatch (const std::string& type)
      {
	dispatch_map_iterator p = dispatch_map.find (type);

	if (p != dispatch_map.end ())
	  dispatch_map.erase (p);
      }

      void print_dispatch (std::ostream& os) const;

      std::string help_for_dispatch (void) const;

      dispatch_map_type get_dispatch (void) const { return dispatch_map; }

      std::string name;

      // Scope id to function object.
      std::map<scope_id, octave_value> subfunctions;

      // Directory name to function object.
      std::map<std::string, octave_value> private_functions;

      // Class name to function object.
      std::map<std::string, octave_value> class_constructors;

      // Dispatch type to function object.
      std::map<std::string, octave_value> class_methods;

      // Legacy dispatch map (dispatch type name to function name).
      dispatch_map_type dispatch_map;

      octave_value cmdline_function;

      octave_value autoload_function;

      octave_value function_on_path;

      octave_value built_in_function;

      size_t count;

    private:

      // No copying!

      fcn_info_rep (const fcn_info_rep&);

      fcn_info_rep& operator = (const fcn_info_rep&);
    };

  public:

    fcn_info (const std::string& nm = std::string ())
      : rep (new fcn_info_rep (nm)) { }

    fcn_info (const fcn_info& ov) : rep (ov.rep)
    { 
      rep->count++;
    }

    fcn_info& operator = (const fcn_info& ov)
    {
      if (this != &ov)
	{
	  rep = ov.rep;
	  rep->count++;
	}

      return *this;
    }

    ~fcn_info (void)
    {
      if (--rep->count == 0)
	delete rep;
    }

    octave_value
    find (tree_argument_list *args, const string_vector& arg_names,
	  octave_value_list& evaluated_args, bool& args_evaluated);

    octave_value find_method (const std::string& dispatch_type) const
    {
      return rep->find_method (dispatch_type);
    }

    octave_value find_built_in_function (void) const
    {
      return rep->built_in_function;
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

    octave_value find_function (void)
    {
      return rep->find_function ();
    }

    octave_value find_function (const octave_value_list& args)
    {
      return rep->find_function (args);
    }

    void install_cmdline_function (const octave_value& f)
    {
      rep->install_cmdline_function (f);
    }

    void install_subfunction (const octave_value& f, scope_id scope)
    {
      rep->install_subfunction (f, scope);
    }

    void install_user_function (const octave_value& f)
    {
      rep->install_user_function (f);
    }

    void install_built_in_function (const octave_value& f)
    {
      rep->install_built_in_function (f);
    }

    void clear (void) { rep->clear (); }
    
    void clear_user_function (void) { rep->clear_user_function (); }
    
    void clear_mex_function (void) { rep->clear_mex_function (); }
    
    void add_dispatch (const std::string& type, const std::string& fname)
    {
      rep->add_dispatch (type, fname);
    }

    void clear_dispatch (const std::string& type)
    {
      rep->clear_dispatch (type);
    }

    void print_dispatch (std::ostream& os) const
    {
      rep->print_dispatch (os);
    }

    std::string help_for_dispatch (void) const { return rep->help_for_dispatch (); }

    dispatch_map_type get_dispatch (void) const
    {
      return rep->get_dispatch ();
    }

  private:

    fcn_info_rep *rep;
  };

  static scope_id global_scope (void) { return xglobal_scope; }
  static scope_id top_scope (void) { return xtop_scope; }

  static scope_id current_scope (void) { return xcurrent_scope; }
  static scope_id current_caller_scope (void) { return xcurrent_caller_scope; }

  static context_id current_context (void) { return xcurrent_context; }

  // We use parent_scope to handle parsing subfunctions.
  static scope_id parent_scope (void) { return xparent_scope; }

  static scope_id alloc_scope (void)
  {
    scope_id retval;

    scope_ids_free_list_iterator p = scope_ids_free_list.begin ();

    if (p != scope_ids_free_list.end ())
      {
	retval = *p;
	scope_ids_free_list.erase (p);
      }
    else
      retval = next_available_scope++;

    scope_ids_in_use.insert (retval);

    return retval;
  }

  static void set_scope (scope_id scope)
  {
    if (scope == xglobal_scope)
      error ("can't set scope to global");
    else if (scope != xcurrent_scope)
      {
	all_instances_iterator p = all_instances.find (scope);

	if (p == all_instances.end ())
	  {
	    instance = new symbol_table ();

	    all_instances[scope] = instance;
	  }
	else
	  instance = p->second;

	xcurrent_scope = scope;
	xcurrent_context = instance->xcurrent_context_this_table;
      }
  }

  static void set_scope_and_context (scope_id scope, context_id context)
  {
    if (scope == xglobal_scope)
      error ("can't set scope to global");
    else
      {
	if (scope != xcurrent_scope)
	  {
	    all_instances_iterator p = all_instances.find (scope);

	    if (p == all_instances.end ())
	      error ("scope not found!");
	    else
	      {
		instance = p->second;

		xcurrent_scope = scope;
	      }
	  }

	if (! error_state)
	  xcurrent_context = context;
      }
  }

  static void push_scope (scope_id scope)
  {
    if (scope_stack.empty ())
      scope_stack.push_front (xtop_scope);

    xcurrent_caller_scope = xcurrent_scope;

    set_scope (scope);

    scope_stack.push_front (scope);
  }

  static void pop_scope (void)
  {
    scope_stack.pop_front ();

    set_scope (scope_stack[0]);

    xcurrent_caller_scope = scope_stack.size () > 1 ? scope_stack[1] : -1;
  }

  static void pop_scope (void *) { pop_scope (); }

  static void reset_scope (void)
  {
    scope_stack.clear ();

    scope_stack.push_front (xtop_scope);

    set_scope (xtop_scope);

    xcurrent_caller_scope = -1;
  }

  static void set_parent_scope (scope_id scope)
  {
    xparent_scope = scope;
  }

  static void reset_parent_scope (void)
  {
    set_parent_scope (-1);
  }

  static void erase_scope (scope_id scope)
  {
    assert (scope != xglobal_scope);

    all_instances_iterator p = all_instances.find (scope);

    if (p != all_instances.end ())
      all_instances.erase (p);

    // free_scope (scope);
  }

  static scope_id dup_scope (scope_id scope)
  {
    scope_id retval = -1;

    symbol_table *inst = get_instance (scope);

    if (inst)
      {
	scope_id new_scope = alloc_scope ();

	symbol_table *new_symbol_table = new symbol_table ();

	if (new_symbol_table)
	  {
	    all_instances[new_scope] = new_symbol_table;

	    inst->do_dup_scope (*new_symbol_table);

	    retval = new_scope;
	  }
      }

    return retval;
  }

#if 0
  static void print_scope (const std::string& tag)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_print_scope (std::cerr);
  }

  void do_print_scope (std::ostream& os) const
  {
    for (const_table_iterator p = table.begin (); p != table.end (); p++)
      {
	symbol_record sr = p->second;

	octave_value val = sr.varval ();

	if (val.is_defined ())
	  sr.varval ().print_with_name (os, sr.name ());
	else
	  os << sr.name () << " is not defined" << std::endl;
      }
  }
#endif

  static symbol_record
  find_symbol (const std::string& name, scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_find_symbol (name) : symbol_record ();
  }

  static void inherit (scope_id scope, scope_id donor_scope)
  {
    symbol_table *inst = get_instance (scope);

    if (inst)
      inst->do_inherit (donor_scope);
  }

  static bool at_top_level (void) { return xcurrent_scope == xtop_scope; }

  // Find a value corresponding to the given name in the table.
  static octave_value
  find (const std::string& name, tree_argument_list *args,
	const string_vector& arg_names,
	octave_value_list& evaluated_args, bool& args_evaluated,
	bool skip_variables = false);

  // Insert a new name in the table.
  static symbol_record& insert (const std::string& name)
  {
    static symbol_record foobar;

    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_insert (name) : foobar;
  }

  static octave_value& varref (const std::string& name,
			       scope_id scope = xcurrent_scope)
  {
    static octave_value foobar;

    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_varref (name) : foobar;
  }

  static octave_value varval (const std::string& name,
			      scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_varval (name) : octave_value ();
  }

  static octave_value&
  global_varref (const std::string& name)
  {
    global_table_iterator p = global_table.find (name);

    return (p == global_table.end ()) ? global_table[name] : p->second;
  }

  static octave_value
  global_varval (const std::string& name)
  {
    const_global_table_iterator p = global_table.find (name);

    return (p != global_table.end ()) ? p->second : octave_value ();
  }

  static octave_value& persistent_varref (const std::string& name)
  {
    static octave_value foobar;

    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_persistent_varref (name) : foobar;
  }

  static octave_value persistent_varval (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_persistent_varval (name) : octave_value ();
  }

  static void erase_persistent (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_erase_persistent (name);
  }

  static bool is_variable (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_is_variable (name) : false;
  }

  static bool
  is_built_in_function_name (const std::string& name)
  {
    octave_value val = find_built_in_function (name);

    return val.is_defined ();
  }

  static octave_value
  find_method (const std::string& name, const std::string& dispatch_type)
  {
    const_fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      return p->second.find_method (dispatch_type);
    else
      {
	fcn_info finfo (name);

	octave_value fcn = finfo.find_method (dispatch_type);

	if (fcn.is_defined ())
	  fcn_table[name] = finfo;

	return fcn;
      }
  }

  static octave_value
  find_built_in_function (const std::string& name)
  {
    const_fcn_table_iterator p = fcn_table.find (name);

    return (p != fcn_table.end ())
      ? p->second.find_built_in_function () : octave_value ();
  }

  static octave_value
  find_autoload (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    return (p != fcn_table.end ())
      ? p->second.find_autoload () : octave_value ();
  }

  static octave_value
  find_function (const std::string& name, tree_argument_list *args,
		 const string_vector& arg_names,
		 octave_value_list& evaluated_args, bool& args_evaluated);

  static octave_value find_user_function (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    return (p != fcn_table.end ())
      ? p->second.find_user_function () : octave_value ();
  }

  static octave_value find_function (const std::string& name)
  {
    octave_value_list evaluated_args;

    return find_function (name, evaluated_args);
  }

  static octave_value
  find_function (const std::string& name, const octave_value_list& args)
  {
    string_vector arg_names;
    octave_value_list evaluated_args = args;
    bool args_evaluated = ! args.empty ();

    return find_function (name, 0, arg_names, evaluated_args, args_evaluated);
  }

  static void install_cmdline_function (const std::string& name,
					const octave_value& fcn)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.install_cmdline_function (fcn);
      }
    else
      {
	fcn_info finfo (name);

	finfo.install_cmdline_function (fcn);

	fcn_table[name] = finfo;
      }
  }

  static void install_subfunction (const std::string& name,
				   const octave_value& fcn,
				   scope_id scope = xparent_scope)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.install_subfunction (fcn, scope);
      }
    else
      {
	fcn_info finfo (name);

	finfo.install_subfunction (fcn, scope);

	fcn_table[name] = finfo;
      }
  }

  static void install_user_function (const std::string& name,
				     const octave_value& fcn)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.install_user_function (fcn);
      }
    else
      {
	fcn_info finfo (name);

	finfo.install_user_function (fcn);

	fcn_table[name] = finfo;
      }
  }

  static void install_built_in_function (const std::string& name,
					 const octave_value& fcn)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.install_built_in_function (fcn);
      }
    else
      {
	fcn_info finfo (name);

	finfo.install_built_in_function (fcn);

	fcn_table[name] = finfo;
      }
  }

  static void clear (const std::string& name)
  {
    clear_variable (name);
  }

  static void clear_all (void)
  {
    clear_variables ();

    clear_functions ();
  }

  static void clear_variables (void)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_variables ();
  }

  // For unwind_protect.
  static void clear_variables (void *) { clear_variables (); }

  static void clear_functions (void)
  {
    for (fcn_table_iterator p = fcn_table.begin (); p != fcn_table.end (); p++)
      p->second.clear ();
  }

  static void clear_function (const std::string& name)
  {
    clear_user_function (name);
  }

  static void clear_global (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_global (name);
  }

  static void clear_variable (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_variable (name);
  }

  static void clear_symbol (const std::string& name)
  {
    // FIXME -- are we supposed to do both here?

    clear_variable (name);
    clear_function (name);
  }

  static void clear_function_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (fcn_table_iterator p = fcn_table.begin (); p != fcn_table.end (); p++)
      {
	if (pattern.match (p->first))
	  p->second.clear_user_function ();
      }
  }

  static void clear_global_pattern (const std::string& pat)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_global_pattern (pat);
  }

  static void clear_variable_pattern (const std::string& pat)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_variable_pattern (pat);
  }

  static void clear_symbol_pattern (const std::string& pat)
  {
    // FIXME -- are we supposed to do both here?

    clear_variable_pattern (pat);
    clear_function_pattern (pat);
  }

  static void clear_user_function (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.clear_user_function ();
      }
    // FIXME -- is this necessary, or even useful?
    // else
    //   error ("clear: no such function `%s'", name.c_str ());
  }

  static void clear_mex_functions (void)
  {
    for (fcn_table_iterator p = fcn_table.begin (); p != fcn_table.end (); p++)
      {
	fcn_info& finfo = p->second;

	finfo.clear_mex_function ();
      }
  }

  static void alias_built_in_function (const std::string& alias,
				       const std::string& name)
  {
    octave_value fcn = find_built_in_function (name);

    if (fcn.is_defined ())
      {
	fcn_info finfo (alias);

	finfo.install_built_in_function (fcn);

	fcn_table[alias] = finfo;
      }
    else
      panic ("alias: `%s' is undefined", name.c_str ());
  }

  static void add_dispatch (const std::string& name, const std::string& type,
			    const std::string& fname)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.add_dispatch (type, fname);
      }
    else
      {
	fcn_info finfo (name);

	finfo.add_dispatch (type, fname);

	fcn_table[name] = finfo;
      }
  }

  static void clear_dispatch (const std::string& name, const std::string& type)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.clear_dispatch (type);
      }
  }

  static void print_dispatch (std::ostream& os, const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	finfo.print_dispatch (os);
      }
  }

  static fcn_info::dispatch_map_type get_dispatch (const std::string& name)
  {
    fcn_info::dispatch_map_type retval;

    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	retval = finfo.get_dispatch ();
      }

    return retval;
  }

  static std::string help_for_dispatch (const std::string& name)
  {
    std::string retval;

    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
	fcn_info& finfo = p->second;

	retval = finfo.help_for_dispatch ();
      }

    return retval;
  }

  static void push_context (void)
  {
    if (xcurrent_scope == xglobal_scope || xcurrent_scope == xtop_scope)
      error ("invalid call to xymtab::push_context");
    else
      {
	symbol_table *inst = get_instance (xcurrent_scope);

	if (inst)
	  inst->do_push_context ();
      }
  }

  static void pop_context (void)
  {
    if (xcurrent_scope == xglobal_scope || xcurrent_scope == xtop_scope)
      error ("invalid call to xymtab::pop_context");
    else
      {
	symbol_table *inst = get_instance (xcurrent_scope);

	if (inst)
	  inst->do_pop_context ();
      }
  }

  // For unwind_protect.
  static void pop_context (void *) { pop_context (); }

  static void mark_hidden (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_mark_hidden (name);
  }

  static void mark_global (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_mark_global (name);
  }

  static std::list<symbol_record>
  all_variables (scope_id scope = xcurrent_scope, bool defined_only = true)
  {
    symbol_table *inst = get_instance (scope);

    return inst
      ? inst->do_all_variables (defined_only) : std::list<symbol_record> ();
  }

  static std::list<symbol_record> glob (const std::string& pattern)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_glob (pattern) : std::list<symbol_record> ();
  }

  static std::list<symbol_record> glob_variables (const std::string& pattern)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_glob (pattern, true) : std::list<symbol_record> ();
  }

  static std::list<symbol_record>
  glob_global_variables (const std::string& pattern)
  {
    std::list<symbol_record> retval;

    glob_match pat (pattern);

    for (const_global_table_iterator p = global_table.begin ();
	 p != global_table.end (); p++)
      {
	// We generate a list of symbol_record objects so that
	// the results from glob_variables and glob_global_variables
	// may be handled the same way.

	if (pat.match (p->first))
	  retval.push_back (symbol_record (p->first, p->second,
					   symbol_record::global));
      }

    return retval;
  }

  static std::list<symbol_record> glob_variables (const string_vector& patterns)
  {
    std::list<symbol_record> retval;

    size_t len = patterns.length ();

    for (size_t i = 0; i < len; i++)
      {
	std::list<symbol_record> tmp = glob_variables (patterns[i]);

	retval.insert (retval.begin (), tmp.begin (), tmp.end ());
      }

    return retval;
  }

  static std::list<std::string> user_function_names (void)
  {
    std::list<std::string> retval;

    for (fcn_table_iterator p = fcn_table.begin ();
	 p != fcn_table.end (); p++)
      {
	if (p->second.is_user_function_defined ())
	  retval.push_back (p->first);
      }

    if (! retval.empty ())
      retval.sort ();

    return retval;
  }

  static std::list<std::string> global_variable_names (void)
  {
    std::list<std::string> retval;

    for (const_global_table_iterator p = global_table.begin ();
	 p != global_table.end (); p++)
      retval.push_back (p->first);

    retval.sort ();

    return retval;
  }

  static std::list<std::string> top_level_variable_names (void)
  {
    symbol_table *inst = get_instance (xtop_scope);

    return inst ? inst->do_variable_names () : std::list<std::string> ();
  }

  static std::list<std::string> variable_names (void)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_variable_names () : std::list<std::string> ();
  }

  static std::list<std::string> built_in_function_names (void)
  {
    std::list<std::string> retval;

    for (const_fcn_table_iterator p = fcn_table.begin ();
	 p != fcn_table.end (); p++)
      {
	octave_value fcn = p->second.find_built_in_function ();

	if (fcn.is_defined ())
	  retval.push_back (p->first);
      }

    if (! retval.empty ())
      retval.sort ();

    return retval;
  }

  static bool is_local_variable (const std::string& name)
  {
    if (xcurrent_scope == xglobal_scope)
      return false;
    else
      {
	symbol_table *inst = get_instance (xcurrent_scope);

	return inst ? inst->do_is_local_variable (name) : false;
      }
  }

  static bool is_global (const std::string& name)
  {
    if (xcurrent_scope == xglobal_scope)
      return true;
    else
      {
	symbol_table *inst = get_instance (xcurrent_scope);

	return inst ? inst->do_is_global (name) : false;
      }
  }

private:

  typedef std::map<std::string, symbol_record>::const_iterator const_table_iterator;
  typedef std::map<std::string, symbol_record>::iterator table_iterator;

  typedef std::map<std::string, octave_value>::const_iterator const_global_table_iterator;
  typedef std::map<std::string, octave_value>::iterator global_table_iterator;

  typedef std::map<std::string, octave_value>::const_iterator const_persistent_table_iterator;
  typedef std::map<std::string, octave_value>::iterator persistent_table_iterator;

  typedef std::map<scope_id, symbol_table*>::const_iterator all_instances_const_iterator;
  typedef std::map<scope_id, symbol_table*>::iterator all_instances_iterator;

  typedef std::map<std::string, fcn_info>::const_iterator const_fcn_table_iterator;
  typedef std::map<std::string, fcn_info>::iterator fcn_table_iterator;

  typedef std::set<scope_id>::const_iterator scope_ids_free_list_const_iterator;
  typedef std::set<scope_id>::iterator scope_ids_free_list_iterator;

  typedef std::set<scope_id>::const_iterator scope_ids_in_use_const_iterator;
  typedef std::set<scope_id>::iterator scope_ids_in_use_iterator;

  // Map from symbol names to symbol info.
  std::map<std::string, symbol_record> table;

  // Map from names of global variables to values.
  static std::map<std::string, octave_value> global_table;

  // Map from names of persistent variables to values.
  std::map<std::string, octave_value> persistent_table;

  // Pointer to symbol table for current scope (variables only).
  static symbol_table *instance;

  // Map from scope id to symbol table instances.
  static std::map<scope_id, symbol_table*> all_instances;

  // Map from function names to function info (subfunctions, private
  // functions, class constructors, class methods, etc.)
  static std::map<std::string, fcn_info> fcn_table;

  static const scope_id xglobal_scope;
  static const scope_id xtop_scope;

  static scope_id xcurrent_scope;
  static scope_id xcurrent_caller_scope;

  // We use parent_scope to handle parsing subfunctions.
  static scope_id xparent_scope;

  // Used to handle recursive calls.
  context_id xcurrent_context_this_table;
  static context_id xcurrent_context;

  static std::deque<scope_id> scope_stack;

  // The next available scope ID.
  static scope_id next_available_scope;

  // The set of scope IDs that are currently allocated.
  static std::set<scope_id> scope_ids_in_use;

  // The set of scope IDs that are currently available.
  static std::set<scope_id> scope_ids_free_list;

  symbol_table (void) : table (), xcurrent_context_this_table () { }

  ~symbol_table (void) { }

  static void free_scope (scope_id scope)
  {
    if (scope == xglobal_scope || scope == xtop_scope)
      error ("can't free global or top-level scopes!");
    else
      {
	scope_ids_in_use_iterator p = scope_ids_in_use.find (scope);

	if (p != scope_ids_in_use.end ())
	  {
	    scope_ids_in_use.erase (p);
	    scope_ids_free_list.insert (*p);
	  }
	else
	  error ("scope id = %ld not found!", scope);
      }
  }

  static symbol_table *get_instance (scope_id scope)
  {
    symbol_table *retval = 0;

    if (scope != xglobal_scope)
      {
	if (scope == xcurrent_scope)
	  {
	    if (! instance)
	      {
		instance = new symbol_table ();

		all_instances[scope] = instance;
	      }

	    if (! instance)
	      error ("unable to create symbol_table object!");

	    retval = instance;
	  }
	else
	  {
	    all_instances_iterator p = all_instances.find (scope);

	    if (p == all_instances.end ())
	      {
		retval = new symbol_table ();

		all_instances[scope] = retval;
	      }
	    else
	      retval = p->second;
	  }
      }

    return retval;
  }

  void insert_symbol_record (const symbol_record& sr)
  {
    table[sr.name ()] = sr;
  }

  void
  do_dup_scope (symbol_table& new_symbol_table) const
  {
    for (const_table_iterator p = table.begin (); p != table.end (); p++)
      new_symbol_table.insert_symbol_record (p->second.dup ());
  }

  symbol_record do_find_symbol (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p == table.end ())
      return do_insert (name);
    else
      return p->second;
  }

  void do_inherit (scope_id donor_scope)
  {
    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
	symbol_record& sr = p->second;

	std::string nm = sr.name ();

	if (! (sr.is_automatic () || sr.is_formal () || nm == "__retval__"))
	  {
	    octave_value val = symbol_table::varval (nm, donor_scope);

	    if (val.is_defined ())
	      {
		sr.varref () = val;

		sr.mark_inherited ();
	      }
	  }
      }
  }

  octave_value
  do_find (const std::string& name, tree_argument_list *args,
	   const string_vector& arg_names,
	   octave_value_list& evaluated_args, bool& args_evaluated,
	   bool skip_variables);

  symbol_record& do_insert (const std::string& name)
  {
    table_iterator p = table.find (name);

    return p == table.end ()
      ? (table[name] = symbol_record (name)) : p->second;
  }

  octave_value& do_varref (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p == table.end ())
      {
	symbol_record& sr = do_insert (name);

	return sr.varref ();
      }
    else
      return p->second.varref ();
  }

  octave_value do_varval (const std::string& name) const
  {
    const_table_iterator p = table.find (name);

    return (p != table.end ()) ? p->second.varval () : octave_value ();
  }

  octave_value& do_persistent_varref (const std::string& name)
  {
    persistent_table_iterator p = persistent_table.find (name);

    return (p == persistent_table.end ())
      ? persistent_table[name] : p->second;
  }

  octave_value do_persistent_varval (const std::string& name)
  {
    const_persistent_table_iterator p = persistent_table.find (name);

    return (p != persistent_table.end ()) ? p->second : octave_value ();
  }

  void do_erase_persistent (const std::string& name)
  {
    persistent_table_iterator p = persistent_table.find (name);

    if (p != persistent_table.end ())
      persistent_table.erase (p);
  }

  bool do_is_variable (const std::string& name) const
  {
    bool retval = false;

    const_table_iterator p = table.find (name);

    if (p != table.end ())
      {
	const symbol_record& sr = p->second;

	retval = sr.is_variable ();
      }

    return retval;
  }

  void do_push_context (void)
  {
    xcurrent_context = ++xcurrent_context_this_table;

    for (table_iterator p = table.begin (); p != table.end (); p++)
      p->second.push_context ();
  }

  void do_pop_context (void)
  {
    xcurrent_context = --xcurrent_context_this_table;

    for (table_iterator p = table.begin (); p != table.end (); )
      {
	if (p->second.pop_context () == 0)
	  table.erase (p++);
	else
	  p++;
      }
  }

  void do_clear_variables (void)
  {
    for (table_iterator p = table.begin (); p != table.end (); p++)
      p->second.clear ();
  }

  void do_clear_global (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p != table.end ())
      {
	symbol_record& sr = p->second;

	if (sr.is_global ())
	  {
	    global_table_iterator q = global_table.find (name);

	    if (q != global_table.end ())
	      global_table.erase (q);

	    sr.unmark_global ();
	  }
      }
  }

  void do_clear_variable (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p != table.end ())
      p->second.clear ();
  }

  void do_clear_global_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
	symbol_record& sr = p->second;

	if (sr.is_global ())
	  {
	    if (pattern.match (sr.name ()))
	      {
		global_table_iterator q = global_table.find (sr.name ());

		if (q != global_table.end ())
		  global_table.erase (q);

		sr.unmark_global ();
	      }
	  }
      }
  }

  void do_clear_variable_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
	symbol_record& sr = p->second;

	if (sr.is_defined () || sr.is_global ())
	  {
	    if (pattern.match (sr.name ()))
	      sr.clear ();
	  }
      }
  }

  void do_mark_hidden (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p != table.end ())
      p->second.mark_hidden ();
  }

  void do_mark_global (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p != table.end ())
      p->second.mark_global ();
  }

  std::list<symbol_record> do_all_variables (bool defined_only) const
  {
    std::list<symbol_record> retval;

    for (const_table_iterator p = table.begin (); p != table.end (); p++)
      {
	const symbol_record& sr = p->second;

	if (defined_only && ! sr.is_defined ())
	  continue;

	retval.push_back (sr);
      }

    return retval;
  }

  std::list<symbol_record> do_glob (const std::string& pattern,
				    bool vars_only = false) const
  {
    std::list<symbol_record> retval;

    glob_match pat (pattern);

    for (const_table_iterator p = table.begin (); p != table.end (); p++)
      {
	if (pat.match (p->first))
	  {
	    const symbol_record& sr = p->second;

	    if (vars_only && ! sr.is_variable ())
	      continue;

	    retval.push_back (sr);
	  }
      }

    return retval;
  }

  std::list<std::string> do_variable_names (void)
  {
    std::list<std::string> retval;

    for (const_table_iterator p = table.begin (); p != table.end (); p++)
      retval.push_back (p->first);

    retval.sort ();

    return retval;
  }

  bool do_is_local_variable (const std::string& name) const
  {
    const_table_iterator p = table.find (name);

    return (p != table.end ()
	    && ! p->second.is_global ()
	    && p->second.is_defined ());
  }

  bool do_is_global (const std::string& name) const
  {
    const_table_iterator p = table.find (name);

    return p != table.end () && p->second.is_global ();
  }
};

extern bool out_of_date_check (octave_value& function);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
