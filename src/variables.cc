// variables.cc                                              -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include <iostream.h>
#include <strstream.h>

#include "statdefs.h"
#include "tree-const.h"
#include "variables.h"
#include "user-prefs.h"
#include "symtab.h"
#include "builtins.h"
#include "g-builtins.h"
#include "t-builtins.h"
#include "error.h"
#include "utils.h"
#include "tree.h"
#include "help.h"

// Symbol table for symbols at the top level.
symbol_table *top_level_sym_tab;

// Symbol table for the current scope.
symbol_table *curr_sym_tab;

// Symbol table for global symbols.
symbol_table *global_sym_tab;

void
initialize_symbol_tables (void)
{
  global_sym_tab = new symbol_table ();

  top_level_sym_tab = new symbol_table ();

  curr_sym_tab = top_level_sym_tab;
}

/*
 * Is there a corresponding M-file that is newer than the symbol
 * definition?
 */
int
symbol_out_of_date (symbol_record *sr)
{
  int ignore = user_pref.ignore_function_time_stamp;

  if (ignore == 2)
    return 0;

  if (sr != (symbol_record *) NULL)
    {
      tree *ans = sr->def ();
      if (ans != NULL_TREE)
	{
	  char *mf = ans->m_file_name ();
	  if (! (mf == (char *) NULL
		 || (ignore && ans->is_system_m_file ())))
	    {
	      time_t tp = ans->time_parsed ();
	      char *fname = m_file_in_path (mf);
	      int status = is_newer (fname, tp);
	      delete [] fname;
	      if (status > 0)
		return 1;
	    }
	}
    }
  return 0;
}

void
document_symbol (const char *name, const char *help)
{
  if (is_builtin_variable (name))
    {
      error ("sorry, can't redefine help for builtin variables");
    }
  else
    {
      symbol_record *sym_rec = curr_sym_tab->lookup (name, 0);
      if (sym_rec == (symbol_record *) NULL)
	{
	  error ("document: no such symbol `%s'", name);
	}
      else
	{
	  sym_rec->document (help);
	}
    }
}

void
install_builtin_mapper_function (builtin_mapper_functions *mf)
{
  symbol_record *sym_rec = global_sym_tab->lookup (mf->name, 1);
  sym_rec->unprotect ();

  Mapper_fcn mfcn;
  mfcn.neg_arg_complex = mf->neg_arg_complex;
  mfcn.d_d_mapper = mf->d_d_mapper;
  mfcn.d_c_mapper = mf->d_c_mapper;
  mfcn.c_c_mapper = mf->c_c_mapper;

  tree_builtin *def = new tree_builtin (mf->nargin_max,
					mf->nargout_max, mfcn,
					mf->name);

  sym_rec->define (def);

  sym_rec->document (mf->help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_text_function (builtin_text_functions *tf)
{
  symbol_record *sym_rec = global_sym_tab->lookup (tf->name, 1);
  sym_rec->unprotect ();

  tree_builtin *def = new tree_builtin (tf->nargin_max, 1,
					tf->text_fcn, tf->name);

  sym_rec->define (def);

  sym_rec->document (tf->help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();

}

void
install_builtin_general_function (builtin_general_functions *gf)
{
  symbol_record *sym_rec = global_sym_tab->lookup (gf->name, 1);
  sym_rec->unprotect ();

  tree_builtin *def = new tree_builtin (gf->nargin_max,
					gf->nargout_max,
					gf->general_fcn, gf->name);

  sym_rec->define (def);

  sym_rec->document (gf->help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_variable (builtin_string_variables *sv)
{
  tree_constant *val = new tree_constant (sv->value);

  bind_builtin_variable (sv->name, val, 0, 1, sv->sv_function,
			 sv->help_string);
}

void
install_builtin_variable_as_function (const char *name, tree_constant *val,
				      int protect = 0, int eternal = 0)
{
  symbol_record *sym_rec = global_sym_tab->lookup (name, 1);
  sym_rec->unprotect ();

  char *tmp_help = sym_rec->help ();

  sym_rec->define_as_fcn (val);

  sym_rec->document (tmp_help);

  if (protect)
    sym_rec->protect ();

  if (eternal)
    sym_rec->make_eternal ();
}

void
bind_nargin_and_nargout (symbol_table *sym_tab, int nargin, int nargout)
{
  tree_constant *tmp;
  symbol_record *sr;

  sr = sym_tab->lookup ("nargin", 1, 0);
  sr->unprotect ();
  tmp = new tree_constant (nargin-1);
  sr->define (tmp);
  sr->protect ();

  sr = sym_tab->lookup ("nargout", 1, 0);
  sr->unprotect ();
  tmp = new tree_constant (nargout);
  sr->define (tmp);
  sr->protect ();
}

/*
 * Give a global variable a definition.  This will insert the symbol
 * in the global table if necessary.
 */
void
bind_builtin_variable (const char *varname, tree_constant *val,
		       int protect = 0, int eternal = 0,
		       sv_Function sv_fcn = (sv_Function) 0,
		       const char *help = (char *) 0)
{
  symbol_record *sr = global_sym_tab->lookup (varname, 1, 0);

// It is a programming error for a builtin symbol to be missing.
// Besides, we just inserted it, so it must be there.

  assert (sr != (symbol_record *) NULL);

  sr->unprotect ();

// Must do this before define, since define will call the special
// variable function only if it knows about it, and it needs to, so
// that user prefs can be properly initialized.

  if (sv_fcn)
    sr->set_sv_function (sv_fcn);

  sr->define_builtin_var (val);

  if (protect)
    sr->protect ();

  if (eternal)
    sr->make_eternal ();

  if (help)
    sr->document (help);    
}

/*
 * Look for the given name in the global symbol table.  If it refers
 * to a string, return a new copy.  If not, return NULL.
 */
char *
builtin_string_variable (const char *name)
{
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

// It is a prorgramming error to look for builtins that aren't.

  assert (sr != (symbol_record *) NULL);

  char *retval = (char *) NULL;

  tree *defn = sr->def ();

  if (defn != NULL_TREE)
    {
      tree_constant val = defn->eval (0);

      if (! error_state && val.is_string_type ())
	{
	  char *s = val.string_value ();
	  if (s != (char *) NULL)
	    retval = strsave (s);
	}
    }

  return retval;
}

/*
 * Look for the given name in the global symbol table.  If it refers
 * to a real scalar, place the value in d and return 0.  Otherwise,
 * return -1. 
 */
int
builtin_real_scalar_variable (const char *name, double& d)
{
  int status = -1;
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

// It is a prorgramming error to look for builtins that aren't.

  assert (sr != (symbol_record *) NULL);

  tree *defn = sr->def ();

  if (defn != NULL_TREE)
    {
      tree_constant val = defn->eval (0);

      if (! error_state
	  && val.const_type () == tree_constant_rep::scalar_constant)
	{
	  d = val.double_value ();
	  status = 0;
	}
    }

  return status;
}

/*
 * Make the definition of the symbol record sr be the same as the
 * definition of the global variable of the same name, creating it if
 * it doesn't already exist. 
 */
void
link_to_global_variable (symbol_record *sr)
{
  if (sr->is_linked_to_global ())
    return;

  symbol_record *gsr = global_sym_tab->lookup (sr->name (), 1, 0);

  if (sr->is_formal_parameter ())
    {
      error ("can't make function parameter `%s' global", sr->name ());
      return;
    }

// There must be a better way to do this.   XXX FIXME XXX

  if (sr->is_variable ())
    {
// Would be nice not to have this cast.  XXX FIXME XXX
      tree_constant *tmp = (tree_constant *) sr->def ();
      if (tmp == NULL_TREE_CONST)
	tmp = new tree_constant ();
      else
	tmp = new tree_constant (*tmp);
      gsr->define (tmp);
    }
  else
    {
      sr->clear ();
    }

// If the global symbol is currently defined as a function, we need to
// hide it with a variable.

  if (gsr->is_function ())
    gsr->define (NULL_TREE_CONST);

  sr->alias (gsr, 1);
  sr->mark_as_linked_to_global ();
}

/*
 * Make the definition of the symbol record sr be the same as the
 * definition of the builtin variable of the same name.
 */
void
link_to_builtin_variable (symbol_record *sr)
{
  symbol_record *tmp_sym = global_sym_tab->lookup (sr->name (), 0, 0);

  if (tmp_sym != (symbol_record *) NULL)
    {
      if (tmp_sym->is_builtin_variable ())
	{
	  sr->alias (tmp_sym);
	}
    }
}

/*
 * Make the definition of the symbol record sr be the same as the
 * definition of the builtin variable or function, or user function of
 * the same name, provided that the name has not been used as a formal
 * parameter.
 */
void
link_to_builtin_or_function (symbol_record *sr)
{
  symbol_record *tmp_sym = global_sym_tab->lookup (sr->name (), 0, 0);

  if (tmp_sym != (symbol_record *) NULL)
    {
      if ((tmp_sym->is_builtin_variable () || tmp_sym->is_function ())
	  && ! tmp_sym->is_formal_parameter ())
	{
	  sr->alias (tmp_sym);
	}
    }
}

/*
 * Force a link to a function in the current symbol table.  This is
 * used just after defining a function to avoid different behavior
 * depending on whether or not the function has been evaluated after
 * being defined.
 *
 * Return without doing anything if there isn't a function with the
 * given name defined in the global symbol table.
 */
void
force_link_to_function (const char *id_name)
{
  symbol_record *gsr = global_sym_tab->lookup (id_name, 1, 0);
  if (gsr->is_function ())
    {
      curr_sym_tab->clear (id_name);
      symbol_record *csr = curr_sym_tab->lookup (id_name, 1, 0);
      csr->alias (gsr);
    }
}

/*
 * Return 1 if the argument names a globally visible variable.
 * Otherwise, return 0.
 */
int
is_globally_visible (const char *name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  return (sr != (symbol_record *) NULL && sr->is_linked_to_global ());
}

/*
 * Extract a keyword and its value from a file.  Input should look
 * something like:
 *
 *  #[ \t]*keyword[ \t]*:[ \t]*string-value\n
 *
 * Returns a pointer to a static variable which is only valid until
 * the next time this function is called.
 */
char *
extract_keyword (istream& is, char *keyword)
{
  ostrstream buf;

  static char *retval = (char *) NULL;

  delete [] retval;
  retval = (char *) NULL;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << ends;
	  char *tmp = buf.str ();
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  delete [] tmp;

	  if (match)
	    {
	      ostrstream value;
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      if (c != '\n')
		{
		  value << c;
		  while (is.get (c) && c != '\n')
		    value << c;
		}
	      value << ends;
	      retval = value.str ();
	      break;
	    }
	}
    }
  return retval;
}

int
extract_keyword (istream& is, char *keyword, int& value)
{
  ostrstream buf;

  int status = 0;
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << ends;
	  char *tmp = buf.str ();
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  delete [] tmp;

	  if (match)
	    {
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      is.putback (c);
	      if (c != '\n')
		is >> value;
	      if (is)
		status = 1;
	      while (is.get (c) && c != '\n')
		; // Skip to beginning of next line;
	      break;
	    }
	}
    }
  return status;
}

/*
 * Skip trailing white space and
 */
void
skip_comments (istream& is)
{
  char c = '\0';
  while (is.get (c))
    {
      if (c == ' ' || c == '\t' || c == '\n')
	; // Skip whitespace on way to beginning of next line.
      else
	break;
    }

  for (;;)
    {
      if (is && c == '#')
	while (is.get (c) && c != '\n')
	  ; // Skip to beginning of next line, ignoring everything.
      else
	break;
    }
}

/*
 * Is `s' a valid identifier?
 */
int
valid_identifier (char *s)
{
  if (s == (char *) NULL || ! (isalnum (*s) || *s == '_'))
     return 0;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_'))
      return 0;

  return 1;
}

/*
 * See if the identifier is in scope.
 */
int
identifier_exists (char *name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  if (sr == (symbol_record *) NULL)
    sr = global_sym_tab->lookup (name, 0, 0);

  if (sr != (symbol_record *) NULL && sr->is_variable ())
    return 1;
  else if (sr != (symbol_record *) NULL && sr->is_function ())
    return 2;
  else
    {
      char *path = m_file_in_path (name);
      if (path != (char *) NULL)
	{
	  delete [] path;
	  return 2;
	}
      else
	{
	  struct stat buf;
	  if (stat (name, &buf) == 0 && S_ISREG (buf.st_mode))
	    return 2;
	}
    }
  return 0;
}

/*
 * Is this variable a builtin?
 */
int
is_builtin_variable (const char *name)
{
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);
  return (sr != (symbol_record *) NULL && sr->is_builtin_variable ());
}

/*
 * Is this tree_constant a valid function?
 */
tree *
is_valid_function (tree_constant& arg, char *warn_for, int warn = 0)
{
  tree *ans = NULL_TREE;

  if (! arg.is_string_type ())
    {
      if (warn)
	error ("%s: expecting function name as argument", warn_for);
      return ans;
    }

  char *fcn_name = arg.string_value ();
  symbol_record *sr = global_sym_tab->lookup (fcn_name, 0, 0);

  if (sr == (symbol_record *) NULL)
    {
      sr = global_sym_tab->lookup (fcn_name, 1, 0);
      tree_identifier tmp (sr);
      tmp.parse_m_file (0);
    }
  else if (symbol_out_of_date (sr))
    {
      tree_identifier tmp (sr);
      tmp.parse_m_file (0);
    }

  ans = sr->def ();
  if (ans == NULL_TREE || ! sr->is_function ())
    {
      if (warn)
	error ("%s: the symbol `%s' is not valid as a function",
	       warn_for, fcn_name);
      ans = NULL_TREE;
    }

  return ans;
}

/*
 * Does this function take the right number of arguments?
 */
int
takes_correct_nargs (tree *fcn, int expected_nargin, char *warn_for,
		     int warn = 0) 
{
  int nargs = fcn->max_expected_args () - 1;
  int e_nargs = expected_nargin - 1;
  if (nargs != e_nargs)
    {
      if (warn)
	error ("%s: expecting function to take %d argument%c", 
	       warn_for, e_nargs, s_plural (e_nargs));
      return 0;
    }
  return 1;
}

// It's not likely that this does the right thing now.  XXX FIXME XXX

char **
make_name_list (void)
{
  int key_len = 0;
  int glb_len = 0;
  int top_len = 0;
  int lcl_len = 0;
  int mfl_len = 0;

  char **key = (char **) NULL;
  char **glb = (char **) NULL;
  char **top = (char **) NULL;
  char **lcl = (char **) NULL;
  char **mfl = (char **) NULL;

// Each of these functions returns a new vector of pointers to new
// strings.

  key = names (keyword_help (), key_len);
  glb = global_sym_tab->list (glb_len);
  top = top_level_sym_tab->list (top_len);
  if (top_level_sym_tab != curr_sym_tab)
    lcl = curr_sym_tab->list (lcl_len);
  mfl = get_m_file_names (mfl_len, 1);

  int total_len = key_len + glb_len + top_len + lcl_len + mfl_len;

  char **list = new char * [total_len+1];
  
// Put all the symbols in one big list.  Only copy pointers, not the
// strings they point to, then only delete the original array of
// pointers, and not the strings they point to.

  int j = 0;
  int i = 0;
  for (i = 0; i < key_len; i++)
    list[j++] = key[i];

  for (i = 0; i < glb_len; i++)
    list[j++] = glb[i];

  for (i = 0; i < top_len; i++)
    list[j++] = top[i];

  for (i = 0; i < lcl_len; i++)
    list[j++] = lcl[i];

  for (i = 0; i < mfl_len; i++)
    list[j++] = mfl[i];

  list[j] = (char *) NULL;

  delete [] key;
  delete [] glb;
  delete [] top;
  delete [] lcl;
  delete [] mfl;

  return list;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
