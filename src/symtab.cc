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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cctype>
#include <climits>

#include <iomanip.h>

#include "glob-match.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

// Should variables be allowed to hide functions of the same name?  A
// positive value means yes.  A negative value means yes, but print a
// warning message.  Zero means it should be considered an error.
static int Vvariables_can_hide_functions;

octave_allocator
symbol_record::symbol_def::allocator (sizeof (symbol_record::symbol_def));

void
symbol_record::symbol_def::dump_symbol_info (void)
{
  octave_stdout << "symbol_def::count: " << count << "\n";
  octave_stdout << "def.type_name():   " << definition.type_name () << "\n";
  octave_stdout << "def.count():       " << definition.get_count () << "\n";
}

// Individual records in a symbol table.

// XXX FIXME XXX -- there are lots of places below where we should
// probably be temporarily ignoring interrupts.

void
symbol_record::rename (const string& new_name)
{
  if (! read_only_error ("rename"))
    nm = new_name;
}

void
symbol_record::define (const octave_value& v, unsigned int sym_type)
{
  if (! (is_variable () && read_only_error ("redefine")))
    {
      if (is_function () || is_constant ())
	{
	  if (Vvariables_can_hide_functions)
	    {
	      push_def (new symbol_def ());

	      if (Vvariables_can_hide_functions < 0)
		warning ("variable `%s' hides function", nm.c_str ());
	    }
	  else
	    error ("variable `%s' hides function", nm.c_str ());
	}

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
      replace_all_defs (new symbol_def (v, symbol_record::BUILTIN_CONSTANT));

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

      replace_all_defs (new symbol_def (tmp, sym_type));

      retval = true;
    }

  return retval;
}

void
symbol_record::clear (void)
{
  if (linked_to_global)
    {
      if (--definition->count <= 0)
	delete definition;

      definition = new symbol_def ();

      linked_to_global = 0;
    }
  else if (! tagged_static)
    {
      remove_top_def ();

      if (! definition)
	definition = new symbol_def ();
    }
}

void
symbol_record::alias (symbol_record *s, bool /* force */)
{
  chg_fcn = s->chg_fcn;

  replace_all_defs (s->definition);

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

bool
symbol_record::hides_fcn (void) const
{
  bool retval = false;

  if (is_variable () && is_defined ())
    {
      symbol_def *hidden_def = definition->next_elem;

      if (hidden_def && hidden_def->is_builtin_function ())
	retval = true;
    }

  return retval;
}

bool
symbol_record::hides_builtin (void) const
{
  bool retval = false;

  if (is_variable () && is_defined ())
    {
      symbol_def *hidden_def = definition->next_elem;

      if (hidden_def && hidden_def->is_user_function ())
	retval = true;
    }

  return retval;
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
  symbol_record *tmp_sym = global_sym_tab->lookup (name ());

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
      replace_all_defs (context.pop ());

      linked_to_global = global_link_context.pop ();
    }
}

void
symbol_record::print_symbol_info_line (ostream& os)
{
  os << (is_read_only () ? " r-" : " rw")
     << (is_eternal () ? "-" : "d")
#if 0
     << (hides_fcn () ? "f" : (hides_builtin () ? "F" : "-"))
#endif
     << "  "
     << setiosflags (ios::left) << setw (24) << type_name () . c_str ();

  os << resetiosflags (ios::left);

  int nr = rows ();
  int nc = columns ();

  if (nr < 0)
    os << "      -";
  else
    os << setiosflags (ios::right) << setw (7) << nr;

  if (nc < 0)
    os << "      -";
  else
    os << setiosflags (ios::right) << setw (7) << nc;

  os << resetiosflags (ios::right);

  os << "  " << name () << "\n";
}

void
symbol_record::dump_symbol_info (void)
{
  if (definition)
    definition->dump_symbol_info ();
  else
    octave_stdout << "symbol " << name () << " is undefined\n";
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

void
symbol_record::push_def (symbol_def *sd)
{
  if (! sd)
    return;

  assert (definition == 0 || definition->next_elem == 0);

  sd->next_elem = definition;

  definition = sd;
}

void
symbol_record::remove_top_def (void)
{
  symbol_def *top = definition;

  definition = definition->next_elem;

  if (--top->count <= 0)
    delete top;
}

void
symbol_record::replace_all_defs (symbol_def *sd)
{
  while (definition)
    remove_top_def ();

  push_def (sd);
}

// A symbol table.

symbol_record *
symbol_table::lookup (const string& nm, bool insert, bool warn)
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
    warning ("lookup: symbol`%s' not found", nm.c_str ());

  return 0;
}

void
symbol_table::rename (const string& old_name, const string& new_name)
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

void
symbol_table::clear (bool clear_user_functions)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  if (ptr->is_user_variable ()
	      || (clear_user_functions && ptr->is_user_function ()))
	    {
	      ptr->clear ();
	    }

	  ptr = ptr->next ();
	}
    }
}

bool
symbol_table::clear (const string& nm, bool clear_user_functions)
{
  unsigned int index = hash (nm);

  symbol_record *ptr = table[index].next ();

  while (ptr)
    {
      if (ptr->name () == nm
	  && (ptr->is_user_variable ()
	      || (clear_user_functions && ptr->is_user_function ())))
	{
	  ptr->clear ();
	  return true;
	}
      ptr = ptr->next ();
    }

  return false;
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
matches_patterns (const string& name, const string_vector& pats)
{
  int npats = pats.length ();

  if (npats == 0)
    return true;

  glob_match pattern (pats);

  return pattern.match (name);
}

Array<symbol_record *>
symbol_table::symbol_list (int& count, const string_vector& pats,
			   unsigned int type, unsigned int scope) const
{
  count = 0;

  int n = size ();

  if (n == 0)
    return 0;

  Array<symbol_record *> symbols (n);

  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  assert (count < n);

	  unsigned int my_scope = ptr->is_linked_to_global () + 1; // Tricky...

	  unsigned int my_type = ptr->type ();

	  string my_name = ptr->name ();

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
symbol_table::name_list (int& count, const string_vector& pats, bool sort,
			 unsigned int type, unsigned int scope) const
{
  Array<symbol_record *> symbols
    = symbol_list (count, pats, type, scope);

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

  string a_nm = a->name ();
  string b_nm = b->name ();

  return a_nm.compare (b_nm);
}

int
symbol_table::maybe_list (const char *header, const string_vector& argv,
			  ostream& os, bool show_verbose,
			  unsigned type, unsigned scope)
{
  int count;

  int status = 0;

  if (show_verbose)
    {
      Array<symbol_record *> symbols = symbol_list (count, argv, type, scope);

      int len = symbols.length ();

      if (len > 0 && count > 0)
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
      string_vector symbols = name_list (count, argv, 1, type, scope);

      if (symbols.length () > 0 && count > 0)
	{
	  os << "\n" << header << "\n\n";

	  symbols.list_in_columns (os);

	  status = 1;
	}
    }

  return status;
}

symbol_record **
symbol_table::glob (int& count, const string& pat, unsigned int type,
		    unsigned int scope) const
{
  count = 0;
  int n = size ();
  if (n == 0)
    return 0;

  symbol_record **symbols = new symbol_record * [n+1];

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
	      symbols[count++] = ptr;
	    }

	  ptr = ptr->next ();
	}
    }
  symbols[count] = 0;

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
symbol_table::print_stats (void)
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
	octave_stdout << "chain number " << i << ":\n";
      else
	{
	  empty_chains++;
	  min_chain_length = 0;
	}

      while (ptr)
	{
	  num_this_chain++;

	  octave_stdout << "  " << ptr->name () << "\n";

	  ptr = ptr->next ();
	}

      count += num_this_chain;

      if (num_this_chain > max_chain_length)
	max_chain_length = num_this_chain;

      if (num_this_chain < min_chain_length)
	min_chain_length = num_this_chain;

      if (num_this_chain > 0)
	octave_stdout << "\n";
    }

  octave_stdout << "max chain length: " << max_chain_length << "\n";
  octave_stdout << "min chain length: " << min_chain_length << "\n";
  octave_stdout << "empty chains:     " << empty_chains << "\n";
  octave_stdout << "total chains:     " << table_size << "\n";
  octave_stdout << "total symbols:    " << count << "\n";
}

// Chris Torek's fave hash function.

unsigned int
symbol_table::hash (const string& str)
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
    "Should variables be allowed to hide functions of the same name?");
}


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
