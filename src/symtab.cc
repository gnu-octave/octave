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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cctype>
#include <climits>
#include <cstdio>

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

#include "gripes.h"
#include "lo-mappers.h"

#include "parse.h"

unsigned long int symbol_table::symtab_count = 0;

// Should variables be allowed to hide functions of the same name?  A
// positive value means yes.  A negative value means yes, but print a
// warning message.  Zero means it should be considered an error.
static int Vvariables_can_hide_functions;

// Nonzero means we print debugging info about symbol table lookups.
static int Vdebug_symtab_lookups;

// Defines layout for the whos/who -long command
std::string Vwhos_line_format;

octave_allocator
symbol_record::symbol_def::allocator (sizeof (symbol_record::symbol_def));

#define SYMBOL_DEF symbol_record::symbol_def

std::string
SYMBOL_DEF::type_as_string (void) const
{
  std::string retval = "<unknown type>";

  if (is_user_variable ())
    retval = "user-defined variable";
  else if (is_command ())
    retval = "built-in command";
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

      if (! definition)
	definition = new symbol_def (tmp, sym_type);
      else
	definition->define (tmp, sym_type);

      retval = true;
    }

  return retval;
}

void
symbol_record::clear (void)
{
  if (is_defined ())
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
  if ((Vvariables_can_hide_functions <= 0 || ! can_hide_function)
      && (is_function ()
	  || (! is_defined () && is_valid_function (nm))))
    {
      if (Vvariables_can_hide_functions < 0 && can_hide_function)
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

      definition = context.top ();
      context.pop ();

      linked_to_global = global_link_context.top ();
      global_link_context.pop ();
    }
}

// Calculate how much space needs to be reserved for the first part of
// the dimensions string.  For example,
//
//   mat is a 12x3 matrix
//            ^^  => 2 columns

int
symbol_record::dimensions_string_req_first_space (int print_dims) const
{
  long dim = 0;
  int first_param_space = 0;

  // Calculating dimensions.

  std::string dim_str = "";
  std::stringstream ss;
  dim_vector dimensions;

  if (is_variable ())
    {
      if (is_matrix_type ())
        {
	  dimensions = dims ();
	  dim = dimensions.length ();
	}
    }

  first_param_space = (first_param_space >= 1 ? first_param_space : 1);

  // Preparing dimension string.

  if ((dim <= print_dims || print_dims < 0) && print_dims != 0)
    {
      // Dimensions string must be printed like this: 2x3x4x2.

      if (dim == 0 || dim == 1)
	first_param_space = 1; // First parameter is 1.
      else
        {
	  ss << dimensions (0);
	 
	  dim_str = ss.str ();
	  first_param_space = dim_str.length ();
	}
    }
  else
    {
      // Printing dimension string as: a-D.

      ss << dim;

      dim_str = ss.str ();
      first_param_space = dim_str.length ();
    }

  return first_param_space;
}

// Calculate how much space needs to be reserved for the the
// dimensions string.  For example,
//
//   mat is a 12x3 matrix
//            ^^^^ => 4 columns

int
symbol_record::dimensions_string_req_total_space (int print_dims) const
{
  std::string dim_str = "";
  std::stringstream ss;

  ss << make_dimensions_string (print_dims);
  dim_str = ss.str ();

  return dim_str.length ();
}

// Make the dimensions-string.  For example: mat is a 2x3 matrix.
//                                                    ^^^

std::string
symbol_record::make_dimensions_string (int print_dims) const
{
  long dim = 0;

  // Calculating dimensions.

  std::string dim_str = "";
  std::stringstream ss;
  dim_vector dimensions;

  if (is_variable ())
    {
      if (is_matrix_type ())
        {
	  dimensions = dims ();
	  dim = dimensions.length ();
	}
    }

  // Preparing dimension string.

  if ((dim <= print_dims || print_dims < 0) && print_dims != 0)
    {
      // Only printing the dimension string as: axbxc...

      if (dim == 0)
	ss << "1x1";
      else
        {
	  for (int i = 0; i < dim; i++)
	    {
	      if (i == 0)
		{
		  if (dim == 1)
		    {
		      // Looks like this is not going to happen in
		      // Octave, but ...

		      ss << "1x" << dimensions (i);
		    }
		  else
		    ss << dimensions (i);
		}
	      else if (i < dim && dim != 1)
		ss << "x" << dimensions (i);
	    }
	}
    }
  else
    {
      // Printing dimension string as: a-D.

      ss << dim << "-D";
    }

  dim_str = ss.str ();

  return dim_str;
}

// Print a line of information on a given symbol.

void
symbol_record::print_symbol_info_line (std::ostream& os,
				       std::list<whos_parameter>& params) const
{
  std::list<whos_parameter>::iterator i = params.begin ();
  while (i != params.end ())
    {
      whos_parameter param = * i;

      if (param.command != '\0')
        {
	  // Do the actual printing.

	  switch (param.modifier)
	    {
	    case 'l':
	      os << std::setiosflags (std::ios::left)
		 << std::setw (param.parameter_length);
	      break;

	    case 'r':
	      os << std::setiosflags (std::ios::right)
		 << std::setw (param.parameter_length);
	      break;

	    case 'c':
	      if (param.command == 's')
	        {
		  int front = param.first_parameter_length
		    - dimensions_string_req_first_space (param.dimensions);
		  int back = param.parameter_length
		    - dimensions_string_req_total_space (param.dimensions)
		    - front;
		  front = (front > 0) ? front : 0;
		  back = (back > 0) ? back : 0;

		  os << std::setiosflags (std::ios::left)
		     << std::setw (front)
		     << ""
		     << std::resetiosflags (std::ios::left)
		     << make_dimensions_string (param.dimensions)
		     << std::setiosflags (std::ios::left)
		     << std::setw (back)
		     << ""
		     << std::resetiosflags (std::ios::left);
		}
	      else
	        {
		  os << std::setiosflags (std::ios::left)
		     << std::setw (param.parameter_length);
		}
	      break;

	    default:
	      error ("whos_line_format: modifier `%c' unknown",
		     param.modifier);

	      os << std::setiosflags (std::ios::right)
		 << std::setw (param.parameter_length);
	    }

	  switch (param.command)
	    {
	    case 'b':
	      os << byte_size ();
	      break;

	    case 'e':
	      os << numel ();
	      break;

	    case 'n':
	      os << name ();
	      break;

	    case 'p':
	      {
		std::stringstream ss;
		std::string str;

		ss << (is_read_only () ? "r-" : "rw")
		   << (is_static () || is_eternal () ? "-" : "d");
		str = ss.str ();

		os << str;
	      }
	      break;

	    case 's':
	      if (param.modifier != 'c')
		os << make_dimensions_string (param.dimensions);
	      break;

	    case 't':
	      os << type_name ();
	      break;
	    
	    default:
	      error ("whos_line_format: command `%c' unknown", param.command);
	    }

	  os << std::resetiosflags (std::ios::left)
	     << std::resetiosflags (std::ios::right);
	  i++;
	}
      else
	{
	  os << param.text;
	  i++;
	}
    }
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

symbol_table::~symbol_table (void)
{
  for (unsigned int i = 0; i < table_size; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr)
	{
	  symbol_record *tmp = ptr;

	  ptr = ptr->next ();

	  delete tmp;
	}
    }

  delete [] table;
}

symbol_record *
symbol_table::lookup (const std::string& nm, bool insert, bool warn)
{
  if (Vdebug_symtab_lookups)
    {
      std::cerr << (table_name.empty () ? std::string ("???") : table_name)
		<< " symtab::lookup ["
		<< (insert ? "I" : "-")
		<< (warn ? "W" : "-")
		<< "] \"" << nm << "\"\n";
    }

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
  if (Vdebug_symtab_lookups)
    {
      std::cerr << (table_name.empty () ? std::string ("???") : table_name)
		<< " symtab::rename "
		<< "\"" << old_name << "\""
		<< " to "
		<< "\"" << new_name << "\"\n";
    }

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
symbol_table::subsymbol_list (const string_vector& pats,
			      unsigned int type, unsigned int scope) const
{
  int count = 0;

  int n = size ();

  Array<symbol_record *> subsymbols (n);
  int pats_length = pats.length ();

  if (n == 0)
    return subsymbols;

  // Look for separators like .({
  for (int j = 0; j < pats_length; j++)
    {
      std::string var_name = pats (j);

      size_t pos = var_name.find_first_of (".({");

      if ((pos != NPOS) && (pos > 0))
        {
	  std::string first_name = var_name.substr(0,pos);

	  for (unsigned int i = 0; i < table_size; i++)
	    {
	      symbol_record *ptr = table[i].next ();

	      while (ptr)
	        {
		  assert (count < n);

		  unsigned int my_scope = ptr->is_linked_to_global () + 1; // Tricky...

		  unsigned int my_type = ptr->type ();

		  std::string my_name = ptr->name ();

		  if ((type & my_type) && (scope & my_scope) && (first_name == my_name))
		    {
		      symbol_record *sym_ptr = new symbol_record ();
		      octave_value value;
		      int parse_status;
	 
		      value = eval_string (var_name, true, parse_status);
	 
		      sym_ptr->define (value);
		      sym_ptr->rename (var_name);
		      subsymbols(count++) = sym_ptr;
		    }

		  ptr = ptr->next ();
		}
	    }
	}
    }

  subsymbols.resize (count);

  return subsymbols;
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

	  if ((type & my_type) && (scope & my_scope) && (matches_patterns (my_name, pats)))
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

void
symbol_table::print_descriptor (std::ostream& os,
				std::list<whos_parameter> params) const
{
  // This method prints a line of information on a given symbol
  std::list<whos_parameter>::iterator i = params.begin ();
  OSSTREAM param_buf;

  while (i != params.end ())
    {
      whos_parameter param = * i;

      if (param.command != '\0')
        {
	  // Do the actual printing
	  switch (param.modifier)
	    {
	    case 'l':
	      os << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	      param_buf << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	      break;

	    case 'r':
	      os << std::setiosflags (std::ios::right) << std::setw (param.parameter_length);
	      param_buf << std::setiosflags (std::ios::right) << std::setw (param.parameter_length);
	      break;

	    case 'c':
	      if (param.command != 's')
	        {
		  os << std::setiosflags (std::ios::left)
		     << std::setw (param.parameter_length);
		  param_buf << std::setiosflags (std::ios::left)
			    << std::setw (param.parameter_length);
		}
	      break;

	    default:
	      os << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	      param_buf << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	    }

	  if (param.command == 's' && param.modifier == 'c')
	    {
	      int a, b;
	     
	      if (param.modifier == 'c')
	        {
		  a = param.first_parameter_length - param.balance;
		  a = (a < 0 ? 0 : a);
		  b = param.parameter_length - a - param.text . length ();
		  b = (b < 0 ? 0 : b);
		  os << std::setiosflags (std::ios::left) << std::setw (a)
		     << "" << std::resetiosflags (std::ios::left) << param.text
		     << std::setiosflags (std::ios::left)
		     << std::setw (b) << ""
		     << std::resetiosflags (std::ios::left);
		  param_buf << std::setiosflags (std::ios::left) << std::setw (a)
		     << "" << std::resetiosflags (std::ios::left) << param.line
		     << std::setiosflags (std::ios::left)
		     << std::setw (b) << ""
		     << std::resetiosflags (std::ios::left);
		}
	    }
	  else
	    {
	      os << param.text;
	      param_buf << param.line;
	    }
	  os << std::resetiosflags (std::ios::left)
	     << std::resetiosflags (std::ios::right);
	  param_buf << std::resetiosflags (std::ios::left)
		    << std::resetiosflags (std::ios::right);
	  i++;
	}
      else
	{
	  os << param.text;
	  param_buf << param.line;
	  i++;
	}
    }

  param_buf << OSSTREAM_ENDS;
  os << OSSTREAM_C_STR (param_buf);
  OSSTREAM_FREEZE (param_buf);
}

std::list<whos_parameter>
symbol_table::parse_whos_line_format (Array<symbol_record *>& symbols) const
{
  // This method parses the string whos_line_format, and returns
  // a parameter list, containing all information needed to print
  // the given attributtes of the symbols
  int idx;
  size_t format_len = Vwhos_line_format.length ();
  char garbage;
  std::list<whos_parameter> params;

  size_t bytes1;
  int elements1;

  int len = symbols.length (), i;

  std::string param_string = "benpst";
  Array<int> param_length(param_string.length ());
  Array<std::string> param_names(param_string.length ());
  size_t pos_b, pos_t, pos_e, pos_n, pos_p, pos_s;

  pos_b = param_string.find ('b'); // Bytes
  pos_t = param_string.find ('t'); // (Type aka) Class
  pos_e = param_string.find ('e'); // Elements
  pos_n = param_string.find ('n'); // Name
  pos_p = param_string.find ('p'); // Protected
  pos_s = param_string.find ('s'); // Size

  param_names(pos_b) = "Bytes";
  param_names(pos_t) = "Class";
  param_names(pos_e) = "Elements";
  param_names(pos_n) = "Name";
  param_names(pos_p) = "Prot";
  param_names(pos_s) = "Size";

  for (i = 0; i < 6; i++)
    param_length(i) = param_names(i) . length ();

  // Calculating necessary spacing for name column,
  // bytes column, elements column and class column
  for (i = 0; i < static_cast<int> (len); i++)
    {
      std::stringstream ss1, ss2;
      std::string str;

      str = symbols(i)->name ();
      param_length(pos_n) = ((str.length () > static_cast<size_t> (param_length(pos_n))) ?
			     str.length () : param_length(pos_n));

      str = symbols(i)->type_name ();
      param_length(pos_t) = ((str.length () > static_cast<size_t> (param_length(pos_t))) ?
			     str.length () : param_length(pos_t));

      elements1 = symbols(i)->numel ();
      ss1 << elements1;
      str = ss1.str ();
      param_length(pos_e) = ((str.length () > static_cast<size_t> (param_length(pos_e))) ?
			     str.length () : param_length(pos_e));

      bytes1 = symbols(i)->byte_size ();
      ss2 << bytes1;
      str = ss2.str ();
      param_length(pos_b) = ((str.length () > static_cast<size_t> (param_length(pos_b))) ?
			     str.length () : param_length (pos_b));
    }

  idx = 0;
  while (static_cast<size_t> (idx) < format_len)
    {
      whos_parameter param;
      param.command = '\0';

      if (Vwhos_line_format[idx] == '%')
        {
	  bool error_encountered = false;
	  param.modifier = 'r';
	  param.parameter_length = 0;
	  param.dimensions = 8;

	  int a = 0, b = -1, c = 8, balance = 1;
	  unsigned int items;
	  size_t pos;
	  std::string cmd;

	  // Parse one command from whos_line_format
	  cmd = Vwhos_line_format.substr (idx, Vwhos_line_format.length ());
	  pos = cmd.find (';');
	  if (pos != NPOS)
	    cmd = cmd.substr (0, pos+1);
	  else
	    error ("parameter without ; in whos_line_format");

	  idx += cmd.length ();

	  // XXX FIXME XXX -- use iostream functions instead of sscanf!

	  if (cmd.find_first_of ("crl") != 1)
	    items = sscanf (cmd.c_str (), "%c%c:%d:%d:%d:%d;",
			    &garbage, &param.command, &a, &b, &c, &balance);
	  else
	    items = sscanf (cmd.c_str (), "%c%c%c:%d:%d:%d:%d;",
			    &garbage, &param.modifier, &param.command,
			    &a, &b, &c, &balance) - 1;
	 
	  if (items < 2)
	    {
	      error ("whos_line_format: parameter structure without command in whos_line_format");
	      error_encountered = true;
	    }

	  // Insert data into parameter
	  param.first_parameter_length = 0;
	  pos = param_string.find (param.command);
	  if (pos != NPOS)
	    {
	      param.parameter_length = param_length(pos);
	      param.text = param_names(pos);
	      param.line.assign (param_names (pos).length (), '=');

	      param.parameter_length = (a > param.parameter_length ? 
		                       a : param.parameter_length);
	      if((param.command == 's') && (param.modifier == 'c') && (b > 0))
	      {
		param.first_parameter_length = b;
	      }
	    }
	  else
	    {
	      error ("whos_line_format: '%c' is not a command",
		     param.command);
	      error_encountered = true;
	    }

	  if (param.command == 's')
	    {
	      // Have to calculate space needed for printing matrix dimensions
	      // Space needed for Size column is hard to determine in prior,
	      // because it depends on dimensions to be shown. That is why it is
	      // recalculated for each Size-command
	      int j, first, rest = 0, total;
	      param.dimensions = c;
	      first = param.first_parameter_length;
	      total = param.parameter_length;
	     
	      for (j = 0; j < len; j++)
	      {
		int first1 = symbols(j)->dimensions_string_req_first_space (param.dimensions);
		int total1 = symbols(j)->dimensions_string_req_total_space (param.dimensions);
		int rest1 = total1 - first1;
		rest = (rest1 > rest ? rest1 : rest);
		first = (first1 > first ? first1 : first);
		total = (total1 > total ? total1 : total);
	      }

	      if (param.modifier == 'c')
	        {
		  if (first < balance)
		    first += balance - first;
		  if (rest + balance < param.parameter_length)
		    rest += param.parameter_length - rest - balance;

		  param.parameter_length = first + rest;
		  param.first_parameter_length = first;
		  param.balance = balance;
		}
	      else
	        {
		  param.parameter_length = total;
		  param.first_parameter_length = 0;
		}
	    }
	  else if (param.modifier == 'c')
	    {
	      error ("whos_line_format: modifier 'c' not available for command '%c'",
		     param.command);
	      error_encountered = true;
	    }

	  // What happens if whos_line_format contains negative numbers
	  // at param_length positions?
	  param.balance = ((b < 0) ? 0 : param.balance);
	  param.first_parameter_length = ((b < 0) ? 0 :
					  param.first_parameter_length);
	  param.parameter_length = ((a < 0) ? 0 :
				    (param.parameter_length <
				     param_length (pos_s)) ?
				    param_length (pos_s) :
				    param.parameter_length);

	  // Parameter will not be pushed into parameter list if ...
	  if (! error_encountered)
	    params.push_back (param);
	}
      else
        {
	  // Text string, to be printed as it is ...
	  std::string text;
	  size_t pos;
	  text = Vwhos_line_format.substr (idx, Vwhos_line_format.length ());
	  pos = text.find ('%');
	  if (pos != NPOS)
	    text = text.substr (0, pos);

	  // Push parameter into list ...
	  idx += text.length ();
	  param.text=text;
	  param.line.assign (text.length(), ' ');
	  params.push_back (param);
	}
    }

  return params;
}

int
symbol_table::maybe_list (const char *header, const string_vector& argv,
			  std::ostream& os, bool show_verbose,
			  unsigned type, unsigned scope)
{
  // This method prints information for sets of symbols, but only one
  // set at a time (like, for instance: all variables, or all
  // built-in-functions).

  // This method invokes print_symbol_info_line to print info on every
  // symbol.

  int status = 0;

  if (show_verbose)
    {
      // XXX FIXME XXX Should separate argv to lists with and without dots.
      Array<symbol_record *> xsymbols = symbol_list (argv, type, scope);
      Array<symbol_record *> xsubsymbols = subsymbol_list (argv, type, scope);

      int sym_len = xsymbols.length (), subsym_len = xsubsymbols.length (),
	len = sym_len + subsym_len;
 
      Array<symbol_record *> symbols (len);

      if (len > 0)
	{
	  size_t bytes = 0;
	  size_t elements = 0;

	  int i;

	  std::list<whos_parameter> params;

	  // Joining symbolic tables.
	  for (i = 0; i < sym_len; i++)
	    symbols(i) = xsymbols(i);

	  for (i = 0; i < subsym_len; i++)
	    symbols(i+sym_len) = xsubsymbols(i);

	  os << "\n" << header << "\n\n";

	  symbols.qsort (maybe_list_cmp_fcn);

	  params = parse_whos_line_format (symbols);

	  print_descriptor (os, params);

	  os << "\n";

	  for (int j = 0; j < len; j++)
	    {
	      symbols(j)->print_symbol_info_line (os, params);
	      elements += symbols(j)->numel ();
	      bytes += symbols(j)->byte_size ();
	    }

	  os << "\nTotal is "
	     << elements << (elements == 1 ? " element" : " elements")
	     << " using "
	     << bytes << (bytes == 1 ? " byte" : " bytes")
	     << "\n";

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

static int
debug_symtab_lookups (void)
{
  Vdebug_symtab_lookups = check_preference ("debug_symtab_lookups");

  return 0;
}

static int
whos_line_format (void)
{
  Vwhos_line_format = builtin_string_variable ("whos_line_format");

  return 0;
}

void
symbols_of_symtab (void)
{
  DEFVAR (variables_can_hide_functions, true, variables_can_hide_functions,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} variables_can_hide_functions\n\
If the value of this variable is nonzero, assignments to variables may\n\
hide previously defined functions of the same name.  A negative value\n\
will cause Octave to print a warning, but allow the operation.\n\
@end defvr");

  DEFVAR (debug_symtab_lookups, false, debug_symtab_lookups,
    "-*- texinfo -*-\n\
@defvr debug_symtab_lookups\n\
If the value of this variable is nonzero, print debugging info when\n\
searching for symbols in the symbol tables.\n\
@end defvr");

  DEFVAR (whos_line_format, "  %p:4; %ln:6; %cs:16:6:8:1;  %rb:12;  %lt:-1;\n", whos_line_format,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} whos_line_format\n\
This string decides in what order attributtes of variables are to be printed.\n\
The following commands are used:\n\
@table @code\n\
@item %b\n\
Prints number of bytes occupied by variables.\n\
@item %e\n\
Prints elements held by variables.\n\
@item %n\n\
Prints variable names.\n\
@item %p\n\
Prints protection attributtes of variables.\n\
@item %s\n\
Prints dimensions of variables.\n\
@item %t\n\
Prints type names of variables.\n\
@end table\n\
\n\
Every command may also have a modifier:\n\
@table @code\n\
@item l\n\
Left alignment.\n\
@item r\n\
Right alignment (this is the default).\n\
@item c\n\
Centered (may only be applied to command %s).\n\
@end table\n\
\n\
A command is composed like this:\n\
%[modifier]<command>[:size_of_parameter[:center-specific[:print_dims[:balance]]]];\n\
\n\
Command and modifier is already explained. Size_of_parameter\n\
tells how many columns the parameter will need for printing.\n\
print_dims tells how many dimensions to print. If number of\n\
dimensions exceeds print_dims, dimensions will be printed like\n\
x-D.\n\
center-specific and print_dims may only be applied to command\n\
%s. A negative value for print_dims will cause Octave to print all\n\
dimensions whatsoever.\n\
balance specifies the offset for printing of the dimensions string.\n\
\n\
Default format is \"  %p:4; %ln:6; %cs:16:6:8:1;  %rb:12;  %lt:-1;\\n\".\n\
@end defvr\n");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
