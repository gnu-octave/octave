// Symbol table classes.                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef __GNUG__
#pragma implementation
#endif

// Don't even think about moving the tree.h include to symtab.h...

#include "symtab.h"
#include "error.h"
#include "variables.h"
#include "utils.h"
#include "tree.h"
#include "tree-const.h"

/*
 * Variables and functions.
 */
symbol_def::symbol_def (void)
{
  help_string = (char *) NULL;
  type = unknown_type;
  lifespan = temporary;
  sym_class = read_write;
  definition = (tree *) NULL;
}

symbol_def::symbol_def (tree_constant *t)
{
  help_string = (char *) NULL;
  type = variable;
  lifespan = temporary;
  sym_class = read_write;
  definition = t;
}

symbol_def::symbol_def (tree_builtin *t)
{
  help_string = (char *) NULL;
  type = builtin_function;
  lifespan = temporary;
  sym_class = read_write;
  definition = t;
}

symbol_def::symbol_def (tree_function *t)
{
  help_string = (char *) NULL;
  type = user_function;
  lifespan = temporary;
  sym_class = read_write;
  definition = t;
}

symbol_def::~symbol_def (void)
{
  delete [] help_string;
  delete definition;
}

void
symbol_def::define (tree_constant *t)
{
  definition = t;
  type = variable;
}

void
symbol_def::define (tree_builtin *t)
{
  definition = t;
  type = builtin_function;
}

void
symbol_def::define (tree_function *t)
{
  definition = t;
  type = user_function;
}

tree *
symbol_def::def (void)
{
  return definition;
}

char *
symbol_def::help (void)
{
  return help_string;
}

void
symbol_def::document (char *h)
{
  delete [] help_string;
  help_string = strsave (h);
}

int
symbol_def::save (ostream& os, int mark_as_global)
{
  return definition->save (os, mark_as_global);
}

/*
 * Individual records in a symbol table.
 */
symbol_record::symbol_record (void)
{
  nm = (char *) NULL;
  formal_param = 0;
  var = (symbol_def *) NULL;
  fcn = (symbol_def *) NULL;
  sv_fcn = (sv_Function) NULL;
  next_elem = (symbol_record *) NULL;
}

symbol_record::symbol_record (char *n)
{
  nm = strsave (n);
  formal_param = 0;
  var = (symbol_def *) NULL;
  fcn = (symbol_def *) NULL;
  sv_fcn = (sv_Function) NULL;
  next_elem = (symbol_record *) NULL;
}

symbol_record::symbol_record (char *n, symbol_record *nxt)
{
  nm = strsave (n);
  formal_param = 0;
  var = (symbol_def *) NULL;
  fcn = (symbol_def *) NULL;
  sv_fcn = (sv_Function) NULL;
  next_elem = nxt;
}

symbol_record::~symbol_record (void)
{
  delete [] nm;

  if (var != (symbol_def *) NULL && --var->count <= 0)
    delete var;

  if (fcn != (symbol_def *) NULL && --fcn->count <= 0)
    delete fcn;
}

char *
symbol_record::name (void)
{
  return nm;
}

char *
symbol_record::help (void)
{
  if (var != (symbol_def *) NULL)
    return var->help ();
  else if (fcn != (symbol_def *) NULL)
    return fcn->help ();
  else
    return (char *) NULL;
}

tree *
symbol_record::def (void)
{
  if (var != (symbol_def *) NULL)
    return var->def ();
  else if (fcn != (symbol_def *) NULL)
    return fcn->def ();
  else
    return (tree *) NULL;
}

int
symbol_record::is_function (void)
{
  return (var == (symbol_def *) NULL && fcn != (symbol_def *) NULL);
}

int
symbol_record::is_variable (void)
{
  return (var != (symbol_def *) NULL);
}

int
symbol_record::is_defined (void)
{
  return (var != (symbol_def *) NULL || fcn != (symbol_def *) NULL);
}

void
symbol_record::set_sv_function (sv_Function f)
{
  sv_fcn = f;
}

int
symbol_record::var_read_only (void)
{
  if (var != (symbol_def *) NULL
       && var->sym_class == symbol_def::read_only)
    {
      error ("can't assign to read only symbol `%s'", nm);
      return 1;
    }
  else
    return 0;
}

int
symbol_record::read_only (void)
{
  if ((var != (symbol_def *) NULL
       && var->sym_class == symbol_def::read_only)
      || (fcn != (symbol_def *) NULL
	  && fcn->sym_class == symbol_def::read_only))
    {
      error ("can't assign to read only symbol `%s'", nm);
      return 1;
    }
  else
    return 0;
}

int
symbol_record::define (tree_constant *t)
{
  if (var_read_only ())
    return 0;

  tree_constant *saved_def = NULL_TREE_CONST;

  if (var != (symbol_def *) NULL)
    {
      saved_def = (tree_constant *) var->def ();  // XXX FIXME XXX
      var->define (t);
    }
  else
    {
      var = new symbol_def (t);
      var->count = 1;
    }

  if (sv_fcn != (sv_Function) NULL && sv_fcn () < 0)
    {
      var->define (saved_def);
      delete t;
      return 0;
    }

  delete saved_def;

  return 1;
}

int
symbol_record::define (tree_builtin *t)
{
  if (read_only ())
    return 0;

  if (var != (symbol_def *) NULL)
    {
      if (--var->count <= 0)
	delete var;
      var = (symbol_def *) NULL;
    }

  if (fcn != (symbol_def *) NULL)
    fcn->define (t);
  else
    {
      fcn = new symbol_def (t);
      fcn->count = 1;
    }

  return 1;
}

int
symbol_record::define (tree_function *t)
{
  if (read_only ())
    return 0;

  if (var != (symbol_def *) NULL)
    {
      if (--var->count <= 0)
	delete var;
      var = (symbol_def *) NULL;
    }

  if (fcn != (symbol_def *) NULL)
    fcn->define (t);
  else
    {
      fcn = new symbol_def (t);
      fcn->count = 1;
    }

  return 1;
}

int
symbol_record::define_as_fcn (tree_constant *t)
{
  if (read_only ())
    return 0;

  if (var != (symbol_def *) NULL)
    {
      if (--var->count <= 0)
	delete var;
      var = (symbol_def *) NULL;
    }

  if (fcn != (symbol_def *) NULL)
    fcn->define (t);
  else
    {
      fcn = new symbol_def (t);
      fcn->count = 1;
    }

  return 1;
}

void
symbol_record::document (char *h)
{
  if (var != (symbol_def *) NULL)
    var->document (h);
  else if (fcn != (symbol_def *) NULL)
    fcn->document (h);
  else
    warning ("couldn't document undefined variable `%s'", nm);
}

void
symbol_record::protect (void)
{
  if (var != (symbol_def *) NULL)
    var->sym_class = symbol_def::read_only;
  else if (fcn != (symbol_def *) NULL)
    fcn->sym_class = symbol_def::read_only;
  else
    warning ("couldn't protect undefined variable `%s'", nm);
}

void
symbol_record::unprotect (void)
{
  if (var != (symbol_def *) NULL)
    var->sym_class = symbol_def::read_write;
  else if (fcn != (symbol_def *) NULL)
    fcn->sym_class = symbol_def::read_write;
}

void
symbol_record::make_eternal (void)
{
  if (var != (symbol_def *) NULL)
    var->lifespan = symbol_def::eternal;
  else if (fcn != (symbol_def *) NULL)
    fcn->lifespan = symbol_def::eternal;
  else
    warning ("couldn't give eternal life to the variable `%s'", nm);
}

int
symbol_record::save (ostream& os, int mark_as_global = 0)
{
  int status = 0;

  if (var != (symbol_def *) NULL && var->def () != (tree *) NULL)
    {
// For now, eternal implies builtin.
      if (var->lifespan != symbol_def::eternal)
	{
// Should we also save the help string?  Maybe someday.
	 os << "# name: " << nm << "\n";
	 status = var->save (os, mark_as_global);
       }
    }
  else if (fcn != (symbol_def *) NULL)
    message ("save", "sorry, can't save functions yet");
  else
    {
// Kludge!  We probably don't want to print warnings for ans, but it
// does seem reasonable to print them for other undefined variables.
      if (strcmp (nm, "ans") != 0)
	warning ("not saving undefined symbol `%s'", nm);
    }

  return status;
}

void
symbol_record::clear_visible (void)
{
  if (var != (symbol_def *) NULL && var->lifespan != symbol_def::eternal)
    {
      if (--var->count <= 0)
	delete var;
      var = (symbol_def *) NULL;
    }
  else if (fcn != (symbol_def *) NULL && fcn->lifespan != symbol_def::eternal)
    {
      if (--fcn->count <= 0)
	delete fcn;
      fcn = (symbol_def *) NULL;
    }
}

void
symbol_record::clear_all (void)
{
  if (var != (symbol_def *) NULL && var->lifespan != symbol_def::eternal)
    {
      if (--var->count <= 0)
	delete var;
      var = (symbol_def *) NULL;
    }

  if (fcn != (symbol_def *) NULL && fcn->lifespan != symbol_def::eternal)
    {
      if (--fcn->count <= 0)
	delete fcn;
      fcn = (symbol_def *) NULL;
    }
}

void
symbol_record::undefine (void)
{
  if (var != (symbol_def *) NULL)
    {
      if (--var->count <= 0)
	delete var;
      var = (symbol_def *) NULL;
    }

  if (fcn != (symbol_def *) NULL)
    {
      if (--fcn->count <= 0)
	delete fcn;
      fcn = (symbol_def *) NULL;
    }
}

void
symbol_record::mark_as_formal_parameter (void)
{
  formal_param = 1;
}

int
symbol_record::is_formal_parameter (void)
{
  return formal_param;
}

void
symbol_record::alias (symbol_record *s, int force = 0)
{
  sv_fcn = s->sv_fcn; // Maybe this should go in the var symbol_def?

  formal_param = s->formal_param; // Hmm.

  if (force && s->var == (symbol_def *) NULL
      && s->fcn == (symbol_def *) NULL)
    {
      s->var = new symbol_def ();
      var = s->var;
      var->count = 2; // Yes, this is correct.
      return;
    }

  if (s->var != (symbol_def *) NULL)
    {
      var = s->var;
      var->count++;
    }
  else if (s->fcn != (symbol_def *) NULL)
    {
      fcn = s->fcn;
      fcn->count++;
    }
}

symbol_record *
symbol_record::next (void)
{
  return next_elem;
}

/*
 * A symbol table.
 */

symbol_table::symbol_table (void)
{
}

symbol_record *
symbol_table::lookup (char *nm, int insert = 0, int warn = 0)
{
  int index = hash (nm) & HASH_MASK;

  symbol_record *ptr = table[index].next ();

  while (ptr != (symbol_record *) NULL)
    {
      if (strcmp (ptr->name (), nm) == 0)
	return ptr;
      ptr = ptr->next ();
    }

  if (insert)
    {
      symbol_record *new_sym;
      new_sym = new symbol_record (nm, table[index].next ());
      table[index].next_elem = new_sym;
      return new_sym;
    }
  else if (warn)
    message ("lookup", "symbol`%s' not found", nm);

  return (symbol_record *) NULL;
}

void
symbol_table::clear (void)
{
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *prev = &table[i];
      symbol_record *curr = prev->next ();

      while (curr != (symbol_record *) NULL)
	{
	  curr->clear_all ();

// This record might have been read only.  If so, we shouldn't delete
// it from the table.
	  if (curr->is_defined ())
	    {
	      prev = curr;
	      curr = curr->next ();
	    }
	  else
	    {
	      prev->next_elem = curr->next ();
	      symbol_record *tmp = curr;
	      curr = curr->next ();
	      delete tmp;
	    }
	}
    }
}

int
symbol_table::clear (char *nm)
{
  int index = hash (nm) & HASH_MASK;

  symbol_record *prev = &table[index];
  symbol_record *curr = prev->next ();

  while (curr != (symbol_record *) NULL)
    {
      if (strcmp (curr->name (), nm) == 0)
	{
	  curr->clear_visible ();

	  if (! curr->is_defined ())
	    {
	      prev->next_elem = curr->next ();
	      symbol_record *tmp = curr;
	      curr = curr->next ();
	      delete tmp;
	    }

	  return 1;
	}
      prev = curr;
      curr = curr->next ();
    }

  return 0;
}

void
symbol_table::undefine (void)
{
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr != (symbol_record *) NULL)
	{
	  ptr->undefine ();
	  ptr = ptr->next ();
	}
    }
}

// Ugh.

void
symbol_table::bind_globals (void)
{
  assert (this != global_sym_tab);

  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();

      while (ptr != (symbol_record *) NULL && ! ptr->formal_param)
	{
	  char *nm = ptr->name ();
	  symbol_record *sr = global_sym_tab->lookup (nm, 0, 0);
	  if (sr != (symbol_record *) NULL)
	    ptr->alias (sr, 1);
	  ptr = ptr->next ();
	}
    }
}

int
symbol_table::save (ostream& os, int mark_as_global = 0)
{
  int status = 0;
  char **names = sorted_var_list ();
  if (names != (char **) NULL)
    {
      while (*names != (char *) NULL)
	{
	  if (save (os, *names, mark_as_global))
	    status++;

	  names++;
	}
    }
  return status;
}

int
symbol_table::save (ostream& os, char *name, int mark_as_global = 0)
{
  int status = 0;
  symbol_record *sr = lookup (name, 0, 0);
  if (sr != (symbol_record *) NULL)
    status = sr->save (os, mark_as_global);
  return status;
}

int
symbol_table::size (void)
{
  int count = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr != (symbol_record *) NULL)
	{
	  count++;
	  ptr = ptr->next ();
	}
    }
  return count;
}

char **
symbol_table::list (void)
{
  int count;
  return list (count);
}

char **
symbol_table::var_list (void)
{
  int count;
  return var_list (count);
}

char **
symbol_table::fcn_list (void)
{
  int count;
  return fcn_list (count);
}

char **
symbol_table::list (int& count)
{
  int n = size ();
  if (n == 0)
    return (char **) NULL;

  char **symbols = new char * [n+1];
  count = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr != (symbol_record *) NULL)
	{
	  assert (count < n);
	  symbols[count++] = strsave (ptr->name ());
	  ptr = ptr->next ();
	}
    }
  symbols[count] = (char *) NULL;
  return symbols;
}

char **
symbol_table::var_list (int& count)
{
  int n = size ();
  if (n == 0)
    return (char **) NULL;

  char **symbols = new char * [n+1];
  count = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr != (symbol_record *) NULL)
	{
	  assert (count < n);
	  if (ptr->is_variable ())
	    symbols[count++] = strsave (ptr->name ());
	  ptr = ptr->next ();
	}
    }
  symbols[count] = (char *) NULL;
  return symbols;
}

char **
symbol_table::fcn_list (int& count)
{
  int n = size ();
  if (n == 0)
    return (char **) NULL;

  char **symbols = new char * [n+1];
  count = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    {
      symbol_record *ptr = table[i].next ();
      while (ptr != (symbol_record *) NULL)
	{
	  assert (count < n);
	  if (ptr->is_function ())
	    symbols[count++] = strsave (ptr->name ());
	  ptr = ptr->next ();
	}
    }
  symbols[count] = (char *) NULL;
  return symbols;
}

static inline int
pstrcmp (char **a, char **b)
{
  return strcmp (*a, *b);
}

char **
symbol_table::sorted_list (void)
{
  int count = 0;
  return sorted_list (count);
}

char **
symbol_table::sorted_var_list (void)
{
  int count = 0;
  return sorted_var_list (count);
}

char **
symbol_table::sorted_fcn_list (void)
{
  int count = 0;
  return sorted_fcn_list (count);
}

char **
symbol_table::sorted_list (int& count)
{
  char **symbols = list (count);
  if (symbols != (char **) NULL)
    qsort ((void **) symbols, count, sizeof (char *),
	   (int (*)(void*, void*)) pstrcmp);
  return symbols;
}

char **
symbol_table::sorted_var_list (int& count)
{
  char **symbols = var_list (count);
  if (symbols != (char **) NULL)
    qsort ((void **) symbols, count, sizeof (char *),
	   (int (*)(void*, void*)) pstrcmp);
  return symbols;
}

char **
symbol_table::sorted_fcn_list (int& count)
{
  char **symbols = fcn_list (count);
  if (symbols != (char **) NULL)
    qsort ((void **) symbols, count, sizeof (char *),
	   (int (*)(void*, void*)) pstrcmp);
  return symbols;
}

// Chris Torek's fave hash function.

unsigned int
symbol_table::hash (const char *str)
{
  unsigned h = 0;
  while (*str)
    h = h * 33 + *str++;
  return h;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
