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

#ifdef __GNUG__
#pragma implementation
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include <iostream.h>

#include "statdefs.h"
#include "tree-const.h"
#include "variables.h"
#include "symtab.h"
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

/*
 * Is there a corresponding M-file that is newer than the symbol
 * definition?
 */
int
symbol_out_of_date (symbol_record *sr)
{
  int status = 0;
  if (sr != (symbol_record *) NULL)
    {
      tree *ans = sr->def ();
      if (ans != NULL_TREE)
	{
	  char *mf = ans->m_file_name ();
	  if (mf != (char *) NULL)
	    {
	      time_t tp = ans->time_parsed ();
	      status = is_newer (mf, tp);
	    }
	}
    }
  return status;
}

/*
 * Force a symbol into the global symbol table.
 */
symbol_record *
force_global (char *name)
{
  symbol_record *retval = (symbol_record *) NULL;

  if (valid_identifier (name))
    {
      symbol_record *sr;
      sr = curr_sym_tab->lookup (name, 0, 0);
      if (sr == (symbol_record *) NULL)
	{
	  retval = global_sym_tab->lookup (name, 1, 0);
	  retval->mark_as_forced_global ();
	}
      else if (sr->is_formal_parameter ())
	{
	  error ("formal parameter `%s' can't be made global", name);
	}
      else
	{
	  retval = global_sym_tab->lookup (name, 1, 0);
	  retval->mark_as_forced_global ();
	  retval->alias (sr, 1);
	  curr_sym_tab->clear (name);
	}
    }
  else
    warning ("`%s' is invalid as an identifier", name);

  return retval;
}

int
bind_variable (char *varname, tree_constant *val)
{
// Look for the symbol in the current symbol table.  If it's there,
// great.  If not, don't insert it, but look for it in the global
// symbol table.  If it's there, great.  If not, insert it in the
// original current symbol table.

  symbol_record *sr;
  sr = curr_sym_tab->lookup (varname, 0, 0);
  if (sr == (symbol_record *) NULL)
    {
      sr = global_sym_tab->lookup (varname, 0, 0);
      if (sr == (symbol_record *) NULL)
	{
	  sr = curr_sym_tab->lookup (varname, 1);
	}
    }

  if (sr != (symbol_record *) NULL)
    {
      sr->define (val);
      return 0;
    }
  else
    return 1;
}

int
bind_protected_variable (char *varname, tree_constant *val)
{
// Look for the symbol in the current symbol table.  If it's there,
// great.  If not, don't insert it, but look for it in the global
// symbol table.  If it's there, great.  If not, insert it in the
// original current symbol table.

  symbol_record *sr;
  sr = curr_sym_tab->lookup (varname, 0, 0);
  if (sr == (symbol_record *) NULL)
    {
      sr = global_sym_tab->lookup (varname, 0, 0);
      if (sr == (symbol_record *) NULL)
	{
	  sr = curr_sym_tab->lookup (varname, 1);
	}
    }

  if (sr != (symbol_record *) NULL)
    {
      sr->unprotect ();
      sr->define (val);
      sr->protect ();
      return 0;
    }
  else
    return 1;
}

/*
 * Look for name first in current then in global symbol tables.  If
 * name is found and it refers to a string, return a new string
 * containing its value.  Otherwise, return NULL.
 */
char *
octave_string_variable (char *name)
{
  char *retval = (char *) NULL;
  symbol_record *sr;
  sr = curr_sym_tab->lookup (name, 0, 0);
  if (sr == (symbol_record *) NULL)
    {
      sr = global_sym_tab->lookup (name, 0, 0);
      if (sr == (symbol_record *) NULL)
	return retval;
    }

  tree *defn = sr->def ();
  if (defn != NULL_TREE)
    {
      tree_constant val = defn->eval (0);
      if (error_state)
	return retval;
      else if (val.is_string_type ())
	{
	  char *s = val.string_value ();
	  if (s != (char *) NULL)
	    retval = strsave (s);
	}
    }

  return retval;
}

/*
 * Look for name first in current then in global symbol tables.  If
 * name is found and it refers to a real scalar, place the value in d
 * and return 0.  Otherwise, return -1.
 */
int
octave_real_scalar_variable (char *name, double& d)
{
  int status = -1;
  symbol_record *sr;
  sr = curr_sym_tab->lookup (name, 0, 0);
  if (sr == (symbol_record *) NULL)
    {
      sr = global_sym_tab->lookup (name, 0, 0);
      if (sr == (symbol_record *) NULL)
	return status;
    }

  tree *defn = sr->def ();
  if (defn != NULL_TREE)
    {
      tree_constant val = defn->eval (0);
      if (error_state)
	return status;
      else if (val.const_type () == tree_constant_rep::scalar_constant)
	{
	  d = val.double_value ();
	  status = 0;
	}
    }

  return status;
}

/*
 * Extract a keyword and its value from a file.  Input should look
 * something like:
 *
 *  #[ \t]*keyword[ \t]*:[ \t]*string-value\n
 */
int
extract_keyword (istream& is, char *keyword, char *value)
{
  char *ptr = value;

  int status = 0;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    *ptr++ = c;

	  while (is.get (c) && isalpha (c))
	    *ptr++ = c;

	  if (strncmp (value, keyword, strlen (keyword)) == 0)
	    {
	      ptr = value;
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      if (c != '\n')
		{
		  *ptr++ = c;
		  while (is.get (c) && c != '\n')
		    *ptr++ = c;
		}
	      *ptr = '\0';
	      status = 1;
	      break;
	    }
	}
    }
  return status;
}

int
extract_keyword (istream& is, char *keyword, int& value)
{
  char buf [128];
  char *ptr = buf;

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
	    *ptr++ = c;

	  while (is.get (c) && isalpha (c))
	    *ptr++ = c;

	  if (strncmp (buf, keyword, strlen (keyword)) == 0)
	    {
	      ptr = buf;
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
 * Is this tree_constant a valid function?
 */
tree *
is_valid_function (tree_constant& arg, char *warn_for, int warn = 0)
{
  tree *ans = NULL_TREE;

  if (! arg.is_string_type ())
    {
      if (warn)
	message (warn_for, "expecting function name as argument");
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
	message (warn_for, "the symbol `%s' is not valid as a function",
		 fcn_name);
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
	message (warn_for, "expecting function to take %d argument%c", 
		 e_nargs, s_plural (e_nargs));
      return 0;
    }
  return 1;
}

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

  key = names (keyword_help (), key_len);
  glb = global_sym_tab->list (glb_len);
  top = top_level_sym_tab->list (top_len);
  if (top_level_sym_tab != curr_sym_tab)
    lcl = curr_sym_tab->list (lcl_len);
  mfl = get_m_file_names (mfl_len, 1);

  int total_len = key_len + glb_len + top_len + lcl_len + mfl_len;

  char **list = new char * [total_len+1];
  
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
