/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cctype>
#include <climits>

#include <iomanip>
#include <fstream>

#include "glob-match.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-pr-code.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

// Should variables be allowed to hide functions of the same name?  A
// positive value means yes.  A negative value means yes, but print a
// warning message.  Zero means it should be considered an error.
static int Vvariables_can_hide_functions;

octave_allocator
symbol_record::symbol_def::allocator (sizeof (symbol_record::symbol_def));

#define SYMBOL_DEF symbol_record::symbol_def

std::string
SYMBOL_DEF::type_as_string (void) const
{
  std::string retval = "<unknown type>";

  if (is_user_variable ())
    retval = "user-defined variable";
  else if (is_text_function ())
    retval = "built-in text function";
  else if (is_mapper_function ())
    retval = "built-in mapper function";
  else if (is_user_function ())
    retval = "user-defined function";
  else if (is_builtin_constant ())
    retval = "built-in constant";
  else if (is_builtin_variable ())
    retval = "built-in variable";
  else if (is_builtin_function ())
    retval = "built-in function";
  else if (is_dld_function ())
    retval = "dynamically-linked function";

  return retval;
}

void
SYMBOL_DEF::type (std::ostream& os, const std::string& name, bool pr_type_info,
		  bool quiet, bool pr_orig_txt)
{
  if (is_user_function ())
    {
      octave_function *defn = definition.function_value ();

      std::string fn = defn ? defn->fcn_file_name () : std::string ();

      if (pr_orig_txt && ! fn.empty ())
	{
	  std::ifstream fs (fn.c_str (), std::ios::in);

	  if (fs)
	    {
	      if (pr_type_info && ! quiet)
		os << name << " is the " << type_as_string ()
		   << " defined from: " << fn << "\n\n";

	      char ch;

	      while (fs.get (ch))
		os << ch;
	    }
	  else
	    os << "unable to open `" << fn << "' for reading!\n";
	}
      else
	{
	  if (pr_type_info && ! quiet)
	    os << name << " is a " << type_as_string () << ":\n\n";

	  tree_print_code tpc (os, "", pr_orig_txt);

	  defn->accept (tpc);
	}
    }
  else if (is_user_variable ()
	   || is_builtin_variable ()
	   || is_builtin_constant ())
    {
      if (pr_type_info && ! quiet)
	os << name << " is a " << type_as_string () << "\n";

      definition.print_raw (os, true);

      if (pr_type_info)
	os << "\n";
    }
  else
    os << name << " is a " << type_as_string () << "\n";
}

std::string
SYMBOL_DEF::which (const std::string& name)
{
  std::string retval;

  if (is_user_function () || is_dld_function ())
    {
      octave_function *defn = definition.function_value ();

      if (defn)
	retval = defn->fcn_file_name ();
    }
  else
    retval = name + " is a " + type_as_string ();

  return retval;
}

void
SYMBOL_DEF::which (std::ostream& os, const std::string& name)
{
  os << name;

  if (is_user_function () || is_dld_function ())
    {
      octave_function *defn = definition.function_value ();

      std::string fn = defn ? defn->fcn_file_name () : std::string ();

      if (! fn.empty ())
	{
	  os << " is the " << type_as_string () << " from the file\n"
	     << fn << "\n";

	  return;
	}
    }

  os << " is a " << type_as_string () << "\n";
}

void
SYMBOL_DEF::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "symbol_def::count: " << count << "\n";

  definition.print_info (os, prefix + "  ");
}

// Individual records in a symbol table.

// XXX FIXME XXX -- there are lots of places below where we should
// probably be temporarily ignoring interrupts.

void
symbol_record::rename (const std::string& new_name)
{
  if (! read_only_error ("rename"))
    nm = new_name;
}

void
symbol_record::define (const octave_value& v, unsigned int sym_type)
{
  if (! (is_variable () && read_only_error ("redefine")))
    {
      if (definition->type () == symbol_record::BUILTIN_VARIABLE)
	sym_type = symbol_record::BUILTIN_VARIABLE;

      definition->define (v, sym_type);
    }
}

void
symbol_record::define_builtin_var (const octave_value& v)
{
  define (v, symbol_record::BUILTIN_VARIABLE);

  if (chg_fcn)
    chg_fcn ();
}

bool
symbol_record::define_builtin_const (const octave_value& v)
{
  bool retval = false;

  if (! read_only_error ("redefine"))
    {
      definition->define (v, symbol_record::BUILTIN_CONSTANT);

      retval = true;
    }

  return retval;
}

bool
symbol_record::define (octave_function *f, unsigned int sym_type)
{
  bool retval = false;

  if (! read_only_error ("redefine"))
    {
      octave_value tmp (f);

      delete definition;

      definition = new symbol_def (tmp, sym_type);

      retval = true;
    }

  return retval;
}

void
symbol_record::clear (void)
{
  if (! tagged_static)
    {
      if (--definition->count <= 0)
	delete definition;

      definition = new symbol_def ();
    }

  if (linked_to_global)
    linked_to_global = 0;
}

void
symbol_record::alias (symbol_record *s)
{
  chg_fcn = s->chg_fcn;

  if (--definition->count <= 0)
    delete definition;

  definition = (s->definition);

  definition->count++;
}

void
symbol_record::mark_as_formal_parameter (void)
{
  if (is_linked_to_global ())
    error ("can't mark global variable `%s' as function parameter",
	   nm.c_str ());
  else if (is_static ())
    error ("can't mark static variable `%s' as function paraemter",
	   nm.c_str ());
  else
    formal_param = 1;
}

void
symbol_record::mark_as_linked_to_global (void)
{
  if (is_formal_parameter ())
    error ("can't make function parameter `%s' global", nm.c_str ());
  else if (is_static ())
    error ("can't make static variable `%s' global", nm.c_str ());
  else
    linked_to_global = 1;
}

void
symbol_record::mark_as_static (void)
{
  if (is_linked_to_global ())
    error ("can't make global variable `%s' static", nm.c_str ());
  else if (is_formal_parameter ())
    error ("can't make formal parameter `%s' static", nm.c_str ());
  else
    tagged_static = 1;
}

octave_value&
symbol_record::variable_value (void)
{
  static octave_value foo;

  return is_variable () ? def () : foo;
}

inline void
symbol_record::link_to_builtin_variable (void)
{
  symbol_record *tmp_sym = fbi_sym_tab->lookup (name ());

  if (tmp_sym && tmp_sym->is_builtin_variable ())
    alias (tmp_sym);
}


octave_lvalue
symbol_record::variable_reference (void)
{
  if (Vvariables_can_hide_functions <= 0
      && (is_function ()
	  || (! is_defined () && is_valid_function (nm))))
    {
      if (Vvariables_can_hide_functions < 0)
	warning ("variable `%s' hides function", nm.c_str ());
      else
	{
	  error ("variable `%s' hides function", nm.c_str ());
	  return octave_lvalue ();
	}
    }

  if (is_function () || is_constant ())
    clear ();

  if (! is_defined ())
    {
      if (! (is_formal_parameter () || is_linked_to_global ()))
	link_to_builtin_variable ();

      if (! is_defined ())
	{
	  octave_value tmp;
	  define (tmp);
	}
    }

  return octave_lvalue (&(def ()), chg_fcn);
}

void
symbol_record::push_context (void)
{
  if (! is_static ())
    {
      context.push (definition);

      definition = new symbol_def ();

      global_link_context.push (static_cast<unsigned int> (linked_to_global));

      linked_to_global = 0;
    }
}

void
symbol_record::pop_context (void)
{
  // It is possible for context to be empty if new symbols have been
  // inserted in the symbol table during recursive calls.  This can
  // happen as a result of calls to eval() and feval().

  if (! context.empty ())
    {
      if (--definition->count <= 0)
	delete definition;

      definition = context.pop ();

      linked_to_global = global_link_context.pop ();
    }
}

void
symbol_record::print_symbol_info_line (std::ostream& os) const
{
  os << (is_read_only () ? " r-" : " rw")
     << (is_eternal () ? "-" : "d")
     << "  "
     << std::setiosflags (std::ios::left) << std::setw (24)
     << type_name () . c_str ();

  os << std::resetiosflags (std::ios::left);

  int nr = rows ();
  int nc = columns ();

  if (nr < 0)
    os << "      -";
  else
    os << std::setiosflags (std::ios::right) << std::setw (7) << nr;

  if (nc < 0)
    os << "      -";
  else
    os << std::setiosflags (std::ios::right) << std::setw (7) << nc;

  os << std::resetiosflags (std::ios::right);

  os << "  " << name () << "\n";
}

void
symbol_record::print_info (std::ostream& os, const std::string& prefix) const
{
  if (definition)
    definition->print_info (os, prefix);
  else
    os << prefix << "symbol " << name () << " is undefined\n";
}

bool
symbol_record::read_only_error (const char *action)
{
  if (is_read_only ())
    {
      if (is_variable () || is_constant ())
	::error ("can't %s read-only constant `%s'", action, nm.c_str ());
      else if (is_function ())
	::error ("can't %s read-only function `%s'", action, nm.c_str ());
      else
	::error ("can't %s read-only symbol `%s'", action, nm.c_str ());

      return true;
    }
  else
    return false;
}

// A symbol table.

symbol_record *
symbol_table::lookup (const std::string& nm, bool insert, bool warn)
{
  unsigned int index = hash (nm);

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm)
	return ptr;

      ptr = ptr->next ();
    }

  if (insert)
    {
      symbol_record *sr = new symbol_record (nm, table[index].next ());

      table[index].chain (sr);

      return sr;
    }
  else if (warn)
    warning ("lookup: symbol `%s' not found", nm.c_str ());

  return 0;
}

void
symbol_table::rename (const std::string& old_name, const std::string& new_name)
{
  unsigned int index = hash (old_name);

  symbol_record *prev = &table[index];
  symbol_record *ptr = prev->next ();

  while (ptr)
    {
      if (ptr->name () == old_name)
	{
	  ptr->rename (new_name);

	  if (! error_state)
	    {
	      prev->chain (ptr->next ());

	      index = hash (new_name);
	      ptr->chain (table[index].next ());
	      table[index].chain (ptr);

	      return;
	    }

	  break;
	}

      prev = ptr;
      ptr = ptr->next ();
    }

  error ("unable to rename `%s' to `%s'", old_name.c_str (),
	 new_name.c_str ());
}

// XXX FIXME XXX -- it would be nice to eliminate a lot of the
// following duplicate code.

void
symbol_table::clear (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  ptr->clear ();

	  ptr = ptr->next ();
	}
    }
}

void
symbol_table::clear_variables (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_variable ())
	    ptr->clear ();

	  ptr = ptr->next ();
	}
    }
}

// Really only clear functions that can be reloaded.

void
symbol_table::clear_functions (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_function () || ptr->is_dld_function ())
	    ptr->clear ();

	  ptr = ptr->next ();
	}
    }
}

void
symbol_table::clear_globals (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_variable () && ptr->is_linked_to_global ())
	    ptr->clear ();

	  ptr = ptr->next ();
	}
    }
}

bool
symbol_table::clear (const std::string& nm)
{
  unsigned int index = hash (nm);

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm)
	{
	  ptr->clear ();
	  return true;
	}
      ptr = ptr->next ();
    }

  return false;
}

bool
symbol_table::clear_variable (const std::string& nm)
{
  unsigned int index = hash (nm);

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm && ptr->is_user_variable ())
	{
	  ptr->clear ();
	  return true;
	}
      ptr = ptr->next ();
    }

  return false;
}

bool
symbol_table::clear_global (const std::string& nm)
{
  unsigned int index = hash (nm);

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm
	  && ptr->is_user_variable ()
	  && ptr->is_linked_to_global ())
	{
	  ptr->clear ();
	  return true;
	}
      ptr = ptr->next ();
    }

  return false;
}

// Really only clear functions that can be reloaded.

bool
symbol_table::clear_function (const std::string& nm)
{
  unsigned int index = hash (nm);

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm
	  && (ptr->is_user_function () || ptr->is_dld_function ()))
	{
	  ptr->clear ();
	  return true;
	}
      ptr = ptr->next ();
    }

  return false;
}

bool
symbol_table::clear_variable_pattern (const std::string& pat)
{
  bool retval = false;

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_variable ())
	    {
	      glob_match pattern (pat);

	      if (pattern.match (ptr->name ()))
		{
		  ptr->clear ();

		  retval = true;
		}
	    }

	  ptr = ptr->next ();
	}
    }

  return retval;
}

bool
symbol_table::clear_global_pattern (const std::string& pat)
{
  bool retval = false;

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_variable () && ptr->is_linked_to_global ())
	    {
	      glob_match pattern (pat);

	      if (pattern.match (ptr->name ()))
		{
		  ptr->clear ();

		  retval = true;
		}
	    }

	  ptr = ptr->next ();
	}
    }

  return retval;
}

// Really only clear functions that can be reloaded.

bool
symbol_table::clear_function_pattern (const std::string& pat)
{
  bool retval = false;

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_function () || ptr->is_dld_function ())
	    {
	      glob_match pattern (pat);

	      if (pattern.match (ptr->name ()))
		{
		  ptr->clear ();

		  retval = true;
		}
	    }

	  ptr = ptr->next ();
	}
    }

  return retval;
}

int
symbol_table::size (void) const
{
  int count = 0;

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  count++;
	  ptr = ptr->next ();
	}
    }

  return count;
}

static bool
matches_patterns (const std::string& name, const string_vector& pats)
{
  int npats = pats.length ();

  if (npats == 0)
    return true;

  glob_match pattern (pats);

  return pattern.match (name);
}

Array<symbol_record *>
symbol_table::symbol_list (const string_vector& pats,
			   unsigned int type, unsigned int scope) const
{
  int count = 0;

  int n = size ();

  Array<symbol_record *> symbols (n);

  if (n == 0)
    return symbols;

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  assert (count < n);

	  unsigned int my_scope = ptr->is_linked_to_global () + 1; // Tricky...

	  unsigned int my_type = ptr->type ();

	  std::string my_name = ptr->name ();

	  if ((type & my_type) && (scope & my_scope)
	      && matches_patterns (my_name, pats))
	    symbols(count++) = ptr;

	  ptr = ptr->next ();
	}
    }

  symbols.resize (count);

  return symbols;
}

string_vector
symbol_table::name_list (const string_vector& pats, bool sort,
			 unsigned int type, unsigned int scope) const
{
  Array<symbol_record *> symbols = symbol_list (pats, type, scope);

  string_vector names;

  int n = symbols.length ();

  if (n > 0)
    {
      names.resize (n);

      for (int i = 0; i < n; i++)
	names[i] = symbols(i)->name ();
    }

  if (sort)
    names.qsort ();

  return names;
}

static int
maybe_list_cmp_fcn (const void *a_arg, const void *b_arg)
{
  const symbol_record *a = *(X_CAST (const symbol_record **, a_arg));
  const symbol_record *b = *(X_CAST (const symbol_record **, b_arg));

  std::string a_nm = a->name ();
  std::string b_nm = b->name ();

  return a_nm.compare (b_nm);
}

int
symbol_table::maybe_list (const char *header, const string_vector& argv,
			  std::ostream& os, bool show_verbose,
			  unsigned type, unsigned scope)
{
  int status = 0;

  if (show_verbose)
    {
      Array<symbol_record *> symbols = symbol_list (argv, type, scope);

      int len = symbols.length ();

      if (len > 0)
	{
	  os << "\n" << header << "\n\n"
		     << "prot  type                       rows   cols  name\n"
		     << "====  ====                       ====   ====  ====\n";

	  symbols.qsort (maybe_list_cmp_fcn);

	  for (int i = 0; i < len; i++)
	    symbols(i)->print_symbol_info_line (os);

	  status = 1;
	}
    }
  else
    {
      string_vector symbols = name_list (argv, 1, type, scope);

      if (! symbols.empty ())
	{
	  os << "\n" << header << "\n\n";

	  symbols.list_in_columns (os);

	  status = 1;
	}
    }

  return status;
}

Array<symbol_record *>
symbol_table::glob (const std::string& pat, unsigned int type,
		    unsigned int scope) const
{
  int count = 0;

  int n = size ();

  Array<symbol_record *> symbols (n);

  if (n == 0)
    return symbols;

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  assert (count < n);

	  unsigned int my_scope = ptr->is_linked_to_global () + 1; // Tricky...

	  unsigned int my_type = ptr->type ();

	  glob_match pattern (pat);

	  if ((type & my_type) && (scope & my_scope)
	      && pattern.match (ptr->name ()))
	    {
	      symbols(count++) = ptr;
	    }

	  ptr = ptr->next ();
	}
    }

  symbols.resize (count);

  return symbols;
}

void
symbol_table::push_context (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  ptr->push_context ();
	  ptr = ptr->next ();
	}
    }
}

void
symbol_table::pop_context (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  ptr->pop_context ();
	  ptr = ptr->next ();
	}
    }
}

void
symbol_table::print_info (std::ostream& os) const
{
  int count = 0;
  int empty_chains = 0;
  int max_chain_length = 0;
  int min_chain_length = INT_MAX;

  for (unsigned int i = 0; i < table_size; i++)
    {
      int num_this_chain = 0;

      symbol_record *ptr = table[i].next ();

      if (ptr)
	os << "chain number " << i << ":\n";
      else
	{
	  empty_chains++;
	  min_chain_length = 0;
	}

      while (ptr)
	{
	  num_this_chain++;

	  os << "  " << ptr->name () << "\n";

	  ptr->print_info (os, "    ");

	  ptr = ptr->next ();
	}

      count += num_this_chain;

      if (num_this_chain > max_chain_length)
	max_chain_length = num_this_chain;

      if (num_this_chain < min_chain_length)
	min_chain_length = num_this_chain;

      if (num_this_chain > 0)
	os << "\n";
    }

  os << "max chain length: " << max_chain_length << "\n";
  os << "min chain length: " << min_chain_length << "\n";
  os << "empty chains:     " << empty_chains << "\n";
  os << "total chains:     " << table_size << "\n";
  os << "total symbols:    " << count << "\n";
}

// Chris Torek's fave hash function.

unsigned int
symbol_table::hash (const std::string& str)
{
  unsigned int h = 0;

  for (unsigned int i = 0; i < str.length (); i++)
    h = h * 33 + str[i];

  return h & (table_size - 1);
}


static int
variables_can_hide_functions (void)
{
  Vvariables_can_hide_functions
    = check_preference ("variables_can_hide_functions");

  return 0;
}

void
symbols_of_symtab (void)
{
  DEFVAR (variables_can_hide_functions, 1.0, variables_can_hide_functions,
    "-*- texinfo -*-\n\
@defvr variables_can_hide_functions\n\
If the value of this variable is nonzero, assignments to variables may\n\
hide previously defined functions of the same name.  A negative value\n\
will cause Octave to print a warning, but allow the operation.\n\
@end defvr");

}


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
