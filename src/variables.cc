// variables.cc                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
#include <float.h>
#include <string.h>
#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#include "octave-hist.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-const.h"
#include "variables.h"
#include "statdefs.h"
#include "defaults.h"
#include "version.h"
#include "mappers.h"
#include "oct-obj.h"
#include "sysdep.h"
#include "dirfns.h"
#include "symtab.h"
#include "octave.h"
#include "error.h"
#include "pager.h"
#include "utils.h"
#include "defun.h"
#include "input.h"
#include "parse.h"
#include "help.h"
#include "lex.h"

extern "C"
{
#include <readline/readline.h>
#include <readline/tilde.h>

#include "fnmatch.h"
}

// Symbol table for symbols at the top level.
symbol_table *top_level_sym_tab = 0;

// Symbol table for the current scope.
symbol_table *curr_sym_tab = 0;

// Symbol table for global symbols.
symbol_table *global_sym_tab = 0;

// Initialization.

// Create the initial symbol tables and set the current scope at the
// top level.

void
initialize_symbol_tables (void)
{
  if (! global_sym_tab)
    global_sym_tab = new symbol_table ();

  if (! top_level_sym_tab)
    top_level_sym_tab = new symbol_table ();

  curr_sym_tab = top_level_sym_tab;
}

// Attributes of variables and functions.

// Is this variable a builtin?

int
is_builtin_variable (const char *name)
{
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);
  return (sr && sr->is_builtin_variable ());
}

// Is this a text-style function?

int
is_text_function_name (const char *s)
{
  symbol_record *sr = global_sym_tab->lookup (s);
  return (sr && sr->is_text_function ());
}

// Is this function globally in this scope?

static int
is_globally_visible (const char *name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  return (sr && sr->is_linked_to_global ());
}

// Is this name a valid identifier?

static int
valid_identifier (char *s)
{
  if (! s || ! (isalnum (*s) || *s == '_'))
     return 0;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_'))
      return 0;

  return 1;
}

// Is this tree_constant a valid function?

tree_fvc *
is_valid_function (const tree_constant& arg, char *warn_for, int warn)
{
  tree_fvc *ans = 0;

  if (! arg.is_string_type ())
    {
      if (warn)
	error ("%s: expecting function name as argument", warn_for);
      return ans;
    }

  char *fcn_name = arg.string_value ();

  symbol_record *sr = 0;
  if (fcn_name)
    sr = lookup_by_name (fcn_name);

  if (sr)
    ans = sr->def ();

  if (! sr || ! ans || ! sr->is_function ())
    {
      if (warn)
	error ("%s: the symbol `%s' is not valid as a function",
	       warn_for, fcn_name);
      ans = 0;
    }

  return ans;
}

// Does this function take the right number of arguments?

int
takes_correct_nargs (tree_fvc *fcn, int expected_nargin, char *warn_for,
		     int warn)
{
  int nargin = fcn->max_expected_args () - 1;
  int e_nargin = expected_nargin - 1;
  if (nargin != e_nargin)
    {
      if (warn)
	error ("%s: expecting function to take %d argument%c", 
	       warn_for, e_nargin, (e_nargin == 1 ? "" : "s"));
      return 0;
    }
  return 1;
}

DEFUN ("is_global", Fis_global, Sis_global, 2, 1,
  "is_global (X): return 1 if the string X names a global variable\n\
otherwise, return 0.")
{
  Octave_object retval = 0.0;

  int nargin = args.length ();

  if (nargin != 2 || ! args(1).is_string_type ())
    {
      print_usage ("is_global");
      return retval;
    }

  char *name = args(1).string_value ();

  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);

  retval = (double) (sr && sr->is_linked_to_global ());

  return retval;
}

DEFUN ("exist", Fexist, Sexist, 2, 1,
  "exist (NAME): check if variable or file exists\n\
\n\
return 0 if NAME is undefined, 1 if it is a variable, or 2 if it is\n\
a function.") 
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2 || ! args(1).is_string_type ())
    {
      print_usage ("exist");
      return retval;
    }

  char *name = args(1).string_value ();

  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  if (! sr)
    sr = global_sym_tab->lookup (name, 0, 0);

  retval = 0.0;

  if (sr && sr->is_variable () && sr->is_defined ())
    retval = 1.0;
  else if (sr && sr->is_function ())
    retval = 2.0;
  else
    {
      char *path = fcn_file_in_path (name);
      if (path)
	{
	  delete [] path;
	  retval = 2.0;
	}
      else
	{
	  struct stat buf;
	  if (stat (name, &buf) == 0 && S_ISREG (buf.st_mode))
	    retval = 2.0;
	}
    }

  return retval;
}

// XXX FIXME XXX -- should these really be here?

static char *
octave_home (void)
{
#ifdef RUN_IN_PLACE
  static char *home = OCTAVE_HOME;
  return home;
#else
  static char *home = 0;
  delete [] home;
  char *oh = getenv ("OCTAVE_HOME");
  if (oh)
    home = strsave (oh);
  else
    home = strsave (OCTAVE_HOME);
  return home;
#endif
}

static char *
octave_info_dir (void)
{
#ifdef RUN_IN_PLACE
  static char *oi = OCTAVE_INFO_DIR;
  return oi;
#else
  static char *oi = 0;
  delete [] oi;
  char *oh = octave_home ();
  oi = strconcat (oh, "/info/");
  return oi;
#endif
}

static char *
default_pager (void)
{
  static char *pager_binary = 0;
  delete [] pager_binary;
  char *pgr = getenv ("PAGER");
  if (pgr)
    pager_binary = strsave (pgr);
  else
#ifdef DEFAULT_PAGER
    pager_binary = strsave (DEFAULT_PAGER);
#else
    pager_binary = strsave ("");
#endif

  return pager_binary;
}

char *
octave_lib_dir (void)
{
#ifdef RUN_IN_PLACE
  static char *ol = OCTAVE_LIB_DIR;
  return ol;
#else
  static char *ol = 0;
  delete [] ol;
  char *oh = octave_home ();
  char *tmp = strconcat (oh, "/lib/octave/");
  ol = strconcat (tmp, version_string);
  delete [] tmp;
  return ol;
#endif
}

// Handle OCTAVE_PATH from the environment like TeX handles TEXINPUTS.
// If the path starts with `:', prepend the standard path.  If it ends
// with `:' append the standard path.  If it begins and ends with
// `:', do both (which is useless, but the luser asked for it...).
//
// This function may eventually be called more than once, so be
// careful not to create memory leaks. 

char *
default_path (void)
{
  static char *pathstring = 0;
  delete [] pathstring;

  static char *std_path = 0;
  delete [] std_path;

  char *libdir = octave_lib_dir ();

  std_path = strconcat (".:", libdir);

  char *oct_path = getenv ("OCTAVE_PATH");

  if (oct_path)
    {
      pathstring = strsave (oct_path);

      if (pathstring[0] == ':')
	{
	  char *tmp = pathstring;
	  pathstring = strconcat (std_path, pathstring);
	  delete [] tmp;
	}

      int tmp_len = strlen (pathstring);
      if (pathstring[tmp_len-1] == ':')
	{
	  char *tmp = pathstring;
	  pathstring = strconcat (pathstring, std_path);
	  delete [] tmp;
	}
    }
  else
    pathstring = strsave (std_path);

  return pathstring;
}

char *
default_info_file (void)
{
  static char *info_file_string = 0;
  delete [] info_file_string;
  char *oct_info_file = getenv ("OCTAVE_INFO_FILE");
  if (oct_info_file)
    info_file_string = strsave (oct_info_file);
  else
    {
      char *infodir = octave_info_dir ();
      info_file_string = strconcat (infodir, "/octave.info");
    }
  return info_file_string;
}

char *
default_editor (void)
{
  static char *editor_string = 0;
  delete [] editor_string;
  char *env_editor = getenv ("EDITOR");
  if (env_editor && *env_editor)
    editor_string = strsave (env_editor);
  else
    editor_string = strsave ("vi");
  return editor_string;
}

char *
get_site_defaults (void)
{
  static char *sd = 0;
  delete [] sd;
  char *libdir = octave_lib_dir ();
  sd = strconcat (libdir, "/octaverc");
  return sd;
}

// Functions for looking up variables and functions.

// Is there a corresponding function file that is newer than the
// symbol definition?

static int
symbol_out_of_date (symbol_record *sr)
{
  int ignore = user_pref.ignore_function_time_stamp;

  if (ignore == 2)
    return 0;

  if (sr)
    {
      tree_fvc *ans = sr->def ();
      if (ans)
	{
	  char *ff = ans->fcn_file_name ();
	  if (ff && ! (ignore && ans->is_system_fcn_file ()))
	    {
	      time_t tp = ans->time_parsed ();
	      char *fname = fcn_file_in_path (ff);
	      int status = is_newer (fname, tp);
	      delete [] fname;
	      if (status > 0)
		return 1;
	    }
	}
    }
  return 0;
}

static void
gobble_leading_white_space (FILE *ffile)
{
  int in_comment = 0;
  int c;
  while ((c = getc (ffile)) != EOF)
    {
      if (in_comment)
	{
	  if (c == '\n')
	    in_comment = 0;
	}
      else
	{
	  if (c == ' ' || c == '\t' || c == '\n')
	    continue;
	  else if (c == '%' || c == '#')
	    in_comment = 1;
	  else
	    {
	      ungetc (c, ffile);
	      break;
	    }
	}
    }
}

static int
is_function_file (FILE *ffile)
{
  int status = 0;

  gobble_leading_white_space (ffile);

  long pos = ftell (ffile);

  char buf [10];
  fgets (buf, 10, ffile);
  int len = strlen (buf);
  if (len > 8 && strncmp (buf, "function", 8) == 0
      && ! (isalnum (buf[8]) || buf[8] == '_'))
    status = 1;

  fseek (ffile, pos, SEEK_SET);

  return status;
}

static int
parse_fcn_file (int exec_script, char *ff)
{
  begin_unwind_frame ("parse_fcn_file");

  int script_file_executed = 0;

  assert (ff);

// Open function file and parse.

  int old_reading_fcn_file_state = reading_fcn_file;

  unwind_protect_ptr (rl_instream);
  unwind_protect_ptr (ff_instream);

  unwind_protect_int (using_readline);
  unwind_protect_int (input_line_number);
  unwind_protect_int (current_input_column);
  unwind_protect_int (reading_fcn_file);

  using_readline = 0;
  reading_fcn_file = 1;
  input_line_number = 0;
  current_input_column = 1;

  FILE *ffile = get_input_from_file (ff, 0);

  if (ffile)
    {
// Check to see if this file defines a function or is just a list of
// commands.

      if (is_function_file (ffile))
	{
	  unwind_protect_int (echo_input);
	  unwind_protect_int (saving_history);
	  unwind_protect_int (reading_fcn_file);

	  echo_input = 0;
	  saving_history = 0;
	  reading_fcn_file = 1;

	  YY_BUFFER_STATE old_buf = current_buffer ();
	  YY_BUFFER_STATE new_buf = create_buffer (ffile);

	  add_unwind_protect (restore_input_buffer, (void *) old_buf);
	  add_unwind_protect (delete_input_buffer, (void *) new_buf);

	  switch_to_buffer (new_buf);

	  unwind_protect_ptr (curr_sym_tab);

	  reset_parser ();

	  int status = yyparse ();

	  if (status != 0)
	    {
	      error ("parse error while reading function file %s", ff);
	      global_sym_tab->clear (curr_fcn_file_name);
	    }
	}
      else if (exec_script)
	{
// The value of `reading_fcn_file' will be restored to the proper value
// when we unwind from this frame.
	  reading_fcn_file = old_reading_fcn_file_state;

	  unwind_protect_int (reading_script_file);
	  reading_script_file = 1;

	  parse_and_execute (ffile, 1);

	  script_file_executed = 1;
	}
      fclose (ffile);
    }

  run_unwind_frame ("parse_fcn_file");

  return script_file_executed;
}

static int
load_fcn_from_file (symbol_record *sym_rec, int exec_script)
{
  int script_file_executed = 0;

  char *nm = sym_rec->name ();

  curr_fcn_file_name = nm;

  char *oct_file = oct_file_in_path (curr_fcn_file_name);

  int loaded_oct_file = 0;

  if (oct_file)
    {
      cerr << "found: " << oct_file << "\n";

      delete [] oct_file;

// XXX FIXME XXX -- this is where we try to link to an external
// object...
      loaded_oct_file = 1;
    }

  if (! loaded_oct_file)
    {
      char *ff = fcn_file_in_path (curr_fcn_file_name);

      if (ff)
	{
	  script_file_executed = parse_fcn_file (exec_script, ff);
	  delete [] ff;
	}

      if (! (error_state || script_file_executed))
	force_link_to_function (nm);
    }

  return script_file_executed;
}

int
lookup (symbol_record *sym_rec, int exec_script)
{
  int script_file_executed = 0;

  if (! sym_rec->is_linked_to_global ())
    {
      if (sym_rec->is_defined ())
	{
	  if (sym_rec->is_function () && symbol_out_of_date (sym_rec))
	    script_file_executed = load_fcn_from_file (sym_rec, exec_script);
	}
      else if (! sym_rec->is_formal_parameter ())
	{
	  link_to_builtin_or_function (sym_rec);
	  
	  if (! sym_rec->is_defined ())
	    script_file_executed = load_fcn_from_file (sym_rec, exec_script);
	  else if (sym_rec->is_function () && symbol_out_of_date (sym_rec))
	    script_file_executed = load_fcn_from_file (sym_rec, exec_script);
	}
    }

  return script_file_executed;
}

// Get the symbol record for the given name that is visible in the
// current scope.  Reread any function definitions that appear to be
// out of date.  If a function is available in a file but is not
// currently loaded, this will load it and insert the name in the
// current symbol table.

symbol_record *
lookup_by_name (const char *nm, int exec_script)
{
  symbol_record *sym_rec = curr_sym_tab->lookup (nm, 1, 0);

  lookup (sym_rec, exec_script);

  return sym_rec;
}

// Variable values.

// Look for the given name in the global symbol table.  If it refers
// to a string, return a new copy.  If not, return 0;

char *
builtin_string_variable (const char *name)
{
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

// It is a prorgramming error to look for builtins that aren't.

  assert (sr);

  char *retval = 0;

  tree_fvc *defn = sr->def ();

  if (defn)
    {
      tree_constant val = defn->eval (0);

      if (! error_state && val.is_string_type ())
	{
	  char *s = val.string_value ();
	  if (s)
	    retval = strsave (s);
	}
    }

  return retval;
}

// Look for the given name in the global symbol table.  If it refers
// to a real scalar, place the value in d and return 0.  Otherwise,
// return -1. 

int
builtin_real_scalar_variable (const char *name, double& d)
{
  int status = -1;
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

// It is a prorgramming error to look for builtins that aren't.

  assert (sr);

  tree_fvc *defn = sr->def ();

  if (defn)
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

// Global stuff and links to builtin variables and functions.

// Make the definition of the symbol record sr be the same as the
// definition of the global variable of the same name, creating it if
// it doesn't already exist. 

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
      if (tmp)
	tmp = new tree_constant (*tmp);
      else
	tmp = new tree_constant ();
      gsr->define (tmp);
    }
  else
    sr->clear ();

// If the global symbol is currently defined as a function, we need to
// hide it with a variable.

  if (gsr->is_function ())
    gsr->define ((tree_constant *) 0);

  sr->alias (gsr, 1);
  sr->mark_as_linked_to_global ();
}

// Make the definition of the symbol record sr be the same as the
// definition of the builtin variable of the same name.

void
link_to_builtin_variable (symbol_record *sr)
{
  symbol_record *tmp_sym = global_sym_tab->lookup (sr->name (), 0, 0);

  if (tmp_sym && tmp_sym->is_builtin_variable ())
    sr->alias (tmp_sym);
}

// Make the definition of the symbol record sr be the same as the
// definition of the builtin variable or function, or user function of
// the same name, provided that the name has not been used as a formal
// parameter.

void
link_to_builtin_or_function (symbol_record *sr)
{
  symbol_record *tmp_sym = global_sym_tab->lookup (sr->name (), 0, 0);

  if (tmp_sym
      && (tmp_sym->is_builtin_variable () || tmp_sym->is_function ())
      && ! tmp_sym->is_formal_parameter ())
    sr->alias (tmp_sym);
}

// Force a link to a function in the current symbol table.  This is
// used just after defining a function to avoid different behavior
// depending on whether or not the function has been evaluated after
// being defined.
//
// Return without doing anything if there isn't a function with the
// given name defined in the global symbol table.

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

// Loading variables from files.

// Extract a keyword and its value from a file.  Input should look
// something like:
//
//  #[ \t]*keyword[ \t]*:[ \t]*string-value\n
//
// Returns a pointer to new storage.  The caller is responsible for
// deleting it.

static char *
extract_keyword (istream& is, char *keyword)
{
  ostrstream buf;

  char *retval = 0;

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

static int
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

// Skip trailing white space and

static void
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

static tree_constant
load_variable (istream& is, tree_constant_rep::constant_type t)
{
  tree_constant retval;

  switch (t)
    {
    case tree_constant_rep::scalar_constant:
      {
	double tmp;
	is >> tmp;
	if (is)
	  retval = tmp;
	else
	  error ("failed to load scalar constant");
      }
      break;
    case tree_constant_rep::matrix_constant:
      {
	int nr = 0, nc = 0;

	if (extract_keyword (is, "rows", nr) && nr > 0
	    && extract_keyword (is, "columns", nc) && nc > 0)
	  {
	    Matrix tmp (nr, nc);
	    is >> tmp;
	    if (is)
	      retval = tmp;
	    else
	      error ("failed to load matrix constant");
	  }
	else
	  error ("failed to extract number of rows and columns");
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex tmp;
	is >> tmp;
	if (is)
	  retval = tmp;
	else
	  error ("failed to load complex scalar constant");
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	int nr = 0, nc = 0;

	if (extract_keyword (is, "rows", nr) && nr > 0
	    && extract_keyword (is, "columns", nc) && nc > 0)
	  {
	    ComplexMatrix tmp (nr, nc);
	    is >> tmp;
	    if (is)
	      retval = tmp;
	    else
	      error ("failed to load complex matrix constant");
	  }
	else
	  error ("failed to extract number of rows and columns");
      }
      break;
    case tree_constant_rep::string_constant:
      {
	int len;
	if (extract_keyword (is, "length", len) && len > 0)
	  {
	    char *tmp = new char [len+1];
	    is.get (tmp, len+1, EOF);
	    if (is)
	      retval = tmp;
	    else
	      error ("failed to load string constant");
	  }
	else
	  error ("failed to extract string length");
      }
      break;
    case tree_constant_rep::range_constant:
      {
	skip_comments (is); // # base, limit, range comment added by save().
	Range tmp;
	is >> tmp;
	if (is)
	  retval = tmp;
	else
	  error ("failed to load range constant");
      }
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

static tree_constant
load_variable (istream& is, int& is_global)
{
  tree_constant retval;

  is_global = 0;

// Look for type keyword

  char *tag = extract_keyword (is, "type");

  if (tag && *tag)
    {
      char *ptr = strchr (tag, ' ');
      if (ptr)
	{
	  *ptr = '\0';
	  is_global = (strncmp (tag, "global", 6) == 0);
	  *ptr = ' ';
	  if (is_global)
	    ptr++;
	  else
	    ptr = tag;
	}
      else
	ptr = tag;

      if (strncmp (ptr, "scalar", 6) == 0)
	retval = load_variable (is, tree_constant_rep::scalar_constant);
      else if (strncmp (ptr, "matrix", 6) == 0)
	retval = load_variable (is, tree_constant_rep::matrix_constant);
      else if (strncmp (ptr, "complex scalar", 14) == 0)
	retval = load_variable (is, tree_constant_rep::complex_scalar_constant);
      else if (strncmp (ptr, "complex matrix", 14) == 0)
	retval = load_variable (is, tree_constant_rep::complex_matrix_constant);
      else if (strncmp (ptr, "string", 6) == 0)
	retval = load_variable (is, tree_constant_rep::string_constant);
      else if (strncmp (ptr, "range", 5) == 0)
	retval = load_variable (is, tree_constant_rep::range_constant);
      else
	error ("unknown constant type `%s'", tag);
    }
  else
    error ("failed to extract keyword specifying value type");

  delete [] tag;

  return retval;
}

static int
load_variable (char *nm, int force, istream& is)
{
// Is there already a symbol by this name?  If so, what is it?

  symbol_record *lsr = curr_sym_tab->lookup (nm, 0, 0);

  int is_undefined = 1;
  int is_variable = 0;
  int is_function = 0;
  int is_global = 0;

  if (lsr)
    {
      is_undefined = ! lsr->is_defined ();
      is_variable = lsr->is_variable ();
      is_function = lsr->is_function ();
      is_global = lsr->is_linked_to_global ();
    }

// Try to read data for this name.

  int global;
  tree_constant tc = load_variable (is, global);

  if (tc.const_type () == tree_constant_rep::unknown_constant)
    {
      error ("load: unable to load variable `%s'", nm);
      return 0;
    }

  symbol_record *sr = 0;

  if (global)
    {
      if (is_global || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope", nm);
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	panic_impossible ();
    }
  else
    {
      if (is_global)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope", nm);
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	panic_impossible ();
    }

  if (sr)
    {
      tree_constant *tmp_tc = new tree_constant (tc);
      sr->define (tmp_tc);
      return 1;
    }
  else
    error ("load: unable to load variable `%s'", nm);

  return 0;
}

DEFUN_TEXT ("load", Fload, Sload, -1, 1,
  "load [-force] file\n
\n\
load variables from a file")
{
  Octave_object retval;

  DEFINE_ARGV("load");

  argc--;
  argv++;

  int force = 0;
  if (argc > 0 && strcmp (*argv, "-force") == 0)
    {
      force++;
      argc--;
      argv++;
    }

  if (argc < 1)
    {
      error ("load: you must specify a single file to read");
      DELETE_ARGV;
      return retval;
    }

  static istream stream;
  static ifstream file;
  if (strcmp (*argv, "-") == 0)
    {
      stream = cin;
    }
  else
    {
      char *fname = tilde_expand (*argv);
      file.open (fname);
      if (! file)
	{
	  error ("load: couldn't open input file `%s'", *argv);
	  DELETE_ARGV;
	  return retval;
	}
      stream = file;
    }

  int count = 0;
  char *nm = 0;
  for (;;)
    {
// Read name for this entry or break on EOF.
      delete [] nm;
      nm = extract_keyword (stream, "name");
      if (nm)
	count++;
      else
	{
	  if (count == 0)
	    {
	      error ("load: no name keywords found in file `%s'", *argv);
	      error ("Are you sure this is an octave data file?");
	    }
	  break;
	}

      if (! *nm)
	continue;

      if (! valid_identifier (nm))
	{
	  warning ("load: skipping bogus identifier `%s'");
	  continue;
	}

      load_variable (nm, force, stream);

      if (error_state)
	{
	  error ("reading file %s", *argv);
	  break;
	}
    }

  if (file);
    file.close ();

  DELETE_ARGV;

  return retval;
}

// Return nonzero if PATTERN has any special globbing chars in it.

static int
glob_pattern_p (char *pattern)
{
  char *p = pattern;
  char c;
  int open = 0;

  while ((c = *p++) != '\0')
    {
      switch (c)
	{
	case '?':
	case '*':
	  return 1;

	case '[':	// Only accept an open brace if there is a close
	  open++;	// brace to match it.  Bracket expressions must be
	  continue;	// complete, according to Posix.2

	case ']':
	  if (open)
	    return 1;
	  continue;
	  
	case '\\':
	  if (*p++ == '\0')
	    return 0;

	default:
	  continue;
	}
    }

  return 0;
}

DEFUN_TEXT ("save", Fsave, Ssave, -1, 1,
  "save file [var ...]\n\
\n\
save variables in a file")
{
  Octave_object retval;

  DEFINE_ARGV("save");

  if (argc < 2)
    {
      print_usage ("save");
      DELETE_ARGV;
      return retval;
    }

  argc--;
  argv++;

  static ostream stream;
  static ofstream file;
  if (strcmp (*argv, "-") == 0)
    {
// XXX FIXME XXX -- should things intended for the screen end up in a 
// tree_constant (string)?
      stream = cout;
    }
  else if (argc == 1 && glob_pattern_p (*argv)) // Guard against things
    {						// like `save a*',
      print_usage ("save");			// which are probably
      DELETE_ARGV;				// mistakes...
      return retval;
    }
  else
    {
      char *fname = tilde_expand (*argv);
      file.open (fname);
      if (! file)
	{
	  error ("save: couldn't open output file `%s'", *argv);
	  DELETE_ARGV;
	  return retval;
	}
      stream = file;

    }

  int prec = user_pref.save_precision;

  if (argc == 1)
    {
      int count;
      char **vars = curr_sym_tab->list (count, 0,
					symbol_def::USER_VARIABLE,
					SYMTAB_ALL_SCOPES);

      for (int i = 0; i < count; i++)
	curr_sym_tab->save (stream, vars[i],
			    is_globally_visible (vars[i]), prec);

      delete [] vars;
    }
  else
    {
      while (--argc > 0)
	{
	  argv++;

	  int count;
	  char **lvars = curr_sym_tab->list (count, 0,
					     symbol_def::USER_VARIABLE);

	  int saved_or_error = 0;
	  int i;
	  for (i = 0; i < count; i++)
	    {
	      if (fnmatch (*argv, lvars[i], __FNM_FLAGS) == 0
		  && curr_sym_tab->save (stream, lvars[i],
					 is_globally_visible (lvars[i]),
					 prec) != 0)
		saved_or_error++;
	    }

	  char **bvars = global_sym_tab->list (count, 0,
					       symbol_def::BUILTIN_VARIABLE);

	  for (i = 0; i < count; i++)
	    {
	      if (fnmatch (*argv, bvars[i], __FNM_FLAGS) == 0
		  && global_sym_tab->save (stream, bvars[i], 0, prec) != 0)
		saved_or_error++;
	    }

	  delete [] lvars;
	  delete [] bvars;

	  if (! saved_or_error)
	    warning ("save: no such variable `%s'", *argv);
	}
    }

  if (file);
    file.close ();

  DELETE_ARGV;

  return retval;
}

// Help stuff.

// It's not likely that this does the right thing now.  XXX FIXME XXX

char **
make_name_list (void)
{
  int key_len = 0;
  int glb_len = 0;
  int top_len = 0;
  int lcl_len = 0;
  int ffl_len = 0;

  char **key = 0;
  char **glb = 0;
  char **top = 0;
  char **lcl = 0;
  char **ffl = 0;

// Each of these functions returns a new vector of pointers to new
// strings.

  key = names (keyword_help (), key_len);
  glb = global_sym_tab->list (glb_len);
  top = top_level_sym_tab->list (top_len);
  if (top_level_sym_tab != curr_sym_tab)
    lcl = curr_sym_tab->list (lcl_len);
  ffl = get_fcn_file_names (ffl_len, 1);

  int total_len = key_len + glb_len + top_len + lcl_len + ffl_len;

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

  for (i = 0; i < ffl_len; i++)
    list[j++] = ffl[i];

  list[j] = 0;

  delete [] key;
  delete [] glb;
  delete [] top;
  delete [] lcl;
  delete [] ffl;

  return list;
}

// List variable names.

static void
print_symbol_info_line (ostrstream& output_buf, const symbol_record_info& s)
{
  output_buf << (s.is_read_only () ? " -" : " w");
  output_buf << (s.is_eternal () ? "- " : "d ");
#if 0
  output_buf << (s.hides_fcn () ? "f" : (s.hides_builtin () ? "F" : "-"));
#endif
  output_buf.form ("  %-16s", s.type_as_string ());
  if (s.is_function ())
    output_buf << "      -      -";
  else
    {
      output_buf.form ("%7d", s.rows ());
      output_buf.form ("%7d", s.columns ());
    }
  output_buf << "  " << s.name () << "\n";
}

static void
print_long_listing (ostrstream& output_buf, symbol_record_info *s)
{
  if (! s)
    return;

  symbol_record_info *ptr = s;
  while (ptr->is_defined ())
    {
      print_symbol_info_line (output_buf, *ptr);
      ptr++;
    }
}

static int
maybe_list (const char *header, ostrstream& output_buf,
	    int show_verbose, symbol_table *sym_tab, unsigned type,
	    unsigned scope)
{
  int count;
  int status = 0;
  if (show_verbose)
    {
      symbol_record_info *symbols;
      symbols = sym_tab->long_list (count, 1, type, scope);
      if (symbols && count > 0)
	{
	  output_buf << "\n" << header << "\n\n"
		     << "prot  type               rows   cols  name\n"
		     << "====  ====               ====   ====  ====\n";

	  print_long_listing (output_buf, symbols);
	  status = 1;
	}
      delete [] symbols;
    }
  else
    {
      char **symbols = sym_tab->list (count, 1, type, scope);
      if (symbols && count > 0)
	{
	  output_buf << "\n" << header << "\n\n";
	  list_in_columns (output_buf, symbols);
	  status = 1;
	}
      delete [] symbols;
    }
  return status;
}

DEFUN_TEXT ("document", Fdocument, Sdocument, -1, 1,
  "document symbol string ...\n\
\n\
Associate a cryptic message with a variable name.")
{
  Octave_object retval;

  DEFINE_ARGV("document");

  if (argc == 3)
    {
      char *name = argv[1];
      char *help = argv[2];

      if (is_builtin_variable (name))
	error ("sorry, can't redefine help for builtin variables");
      else
	{
	  symbol_record *sym_rec = curr_sym_tab->lookup (name, 0);

	  if (sym_rec)
	    sym_rec->document (help);
	  else
	    error ("document: no such symbol `%s'", name);
	}
    }
  else
    print_usage ("document");

  DELETE_ARGV;

  return retval;
}

// XXX FIXME XXX -- this should take a list of regular expressions
// naming the variables to look for.

static Octave_object
do_who (int argc, char **argv, int nargout)
{
  Octave_object retval;

  int show_builtins = 0;
  int show_functions = (curr_sym_tab == top_level_sym_tab);
  int show_variables = 1;
  int show_verbose = 0;

  char *my_name = argv[0];

  if (argc > 1)
    {
      show_functions = 0;
      show_variables = 0;
    }

  for (int i = 1; i < argc; i++)
    {
      argv++;
      if (strcmp (*argv, "-all") == 0 || strcmp (*argv, "-a") == 0)
	{
	  show_builtins++;
	  show_functions++;
	  show_variables++;	  
	}
      else if (strcmp (*argv, "-builtins") == 0
	       || strcmp (*argv, "-b") == 0)
	show_builtins++;
      else if (strcmp (*argv, "-functions") == 0
	       || strcmp (*argv, "-f") == 0)
	show_functions++;
      else if (strcmp (*argv, "-long") == 0 
	       || strcmp (*argv, "-l") == 0)
	  show_verbose++;
      else if (strcmp (*argv, "-variables") == 0
	       || strcmp (*argv, "-v") == 0)
	show_variables++;
      else
	warning ("%s: unrecognized option `%s'", my_name, *argv);
    }

// If the user specified -l and nothing else, show variables.  If
// evaluating this at the top level, also show functions.

  if (show_verbose && ! (show_builtins || show_functions || show_variables))
    {
      show_functions = (curr_sym_tab == top_level_sym_tab);
      show_variables = 1;
    }

  ostrstream output_buf;
  int pad_after = 0;

  if (show_builtins)
    {
      pad_after += maybe_list ("*** built-in variables:",
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_VARIABLE,
			       SYMTAB_ALL_SCOPES);

      pad_after += maybe_list ("*** built-in functions:",
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_functions)
    {
      pad_after += maybe_list ("*** currently compiled functions:",
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::USER_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_variables)
    {
      pad_after += maybe_list ("*** local user variables:",
			       output_buf, show_verbose, curr_sym_tab,
			       symbol_def::USER_VARIABLE,
			       SYMTAB_LOCAL_SCOPE); 

      pad_after += maybe_list ("*** globally visible user variables:",
			       output_buf, show_verbose, curr_sym_tab,
			       symbol_def::USER_VARIABLE,
			       SYMTAB_GLOBAL_SCOPE);
    }

  if (pad_after)
    output_buf << "\n";

  output_buf << ends;
  maybe_page_output (output_buf);

  return retval;
}

DEFUN_TEXT ("who", Fwho, Swho, -1, 1,
  "who [-all] [-builtins] [-functions] [-long] [-variables]\n\
\n\
List currently defined symbol(s).  Options may be shortened to one\n\
character, but may not be combined.")
{
  Octave_object retval;

  DEFINE_ARGV("who");

  retval = do_who (argc, argv, nargout);

  DELETE_ARGV;

  return retval;
}

DEFUN_TEXT ("whos", Fwhos, Swhos, -1, 1,
  "whos [-all] [-builtins] [-functions] [-long] [-variables]\n\
\n\
List currently defined symbol(s).  Options may be shortened to one\n\
character, but may not be combined.")
{
  Octave_object retval;

  Octave_object tmp_args = args;
  tmp_args(args.length ()) = "-long";

  int argc = tmp_args.length ();
  char **argv = make_argv (tmp_args, "whos");

  if (error_state)
    return retval;

  retval = do_who (argc, argv, nargout);

  while (--argc >= 0)
    delete [] argv[argc];
  delete [] argv;

  return retval;
}

// Install variables and functions in the symbol tables.

void
install_builtin_mapper (builtin_mapper_function *mf)
{
  symbol_record *sym_rec = global_sym_tab->lookup (mf->name, 1);
  sym_rec->unprotect ();

  Mapper_fcn mfcn;
  mfcn.can_return_complex_for_real_arg = mf->can_return_complex_for_real_arg;
  mfcn.lower_limit = mf->lower_limit;
  mfcn.upper_limit = mf->upper_limit;
  mfcn.d_d_mapper = mf->d_d_mapper;
  mfcn.d_c_mapper = mf->d_c_mapper;
  mfcn.c_c_mapper = mf->c_c_mapper;

  tree_builtin *def = new tree_builtin (2, 1, mfcn, mf->name);

  sym_rec->define (def);

  sym_rec->document (mf->help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_function (builtin_function *f)
{
  symbol_record *sym_rec = global_sym_tab->lookup (f->name, 1);
  sym_rec->unprotect ();

  tree_builtin *def = new tree_builtin (f->nargin_max, f->nargout_max,
					f->fcn, f->name);

  sym_rec->define (def, f->is_text_fcn);

  sym_rec->document (f->help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_variable (builtin_variable *v)
{
  if (v->install_as_function)
    install_builtin_variable_as_function (v->name, v->value, v->protect,
					  v->eternal, v->help_string);
  else
    bind_builtin_variable (v->name, v->value, v->protect, v->eternal,
			   v->sv_function, v->help_string);
}

void
install_builtin_variable_as_function (const char *name, tree_constant *val,
				      int protect, int eternal,
				      const char *help)
{
  symbol_record *sym_rec = global_sym_tab->lookup (name, 1);
  sym_rec->unprotect ();

  const char *tmp_help = help;
  if (! help)
    tmp_help = sym_rec->help ();

  sym_rec->define_as_fcn (val);

  sym_rec->document (tmp_help);

  if (protect)
    sym_rec->protect ();

  if (eternal)
    sym_rec->make_eternal ();
}

void
alias_builtin (const char *alias, const char *name)
{
  symbol_record *sr_name = global_sym_tab->lookup (name, 0, 0);
  if (! sr_name)
    panic ("can't alias to undefined name!");

  symbol_record *sr_alias = global_sym_tab->lookup (alias, 1, 0);

  if (sr_alias)
    sr_alias->alias (sr_name);
  else
    panic_impossible ();
}

// Defining variables.

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

// Give a global variable a definition.  This will insert the symbol
// in the global table if necessary.

// How is this different than install_builtin_variable?  Are both
// functions needed?

void
bind_builtin_variable (const char *varname, tree_constant *val,
		       int protect, int eternal, sv_Function sv_fcn,
		       const char *help)
{
  symbol_record *sr = global_sym_tab->lookup (varname, 1, 0);

// It is a programming error for a builtin symbol to be missing.
// Besides, we just inserted it, so it must be there.

  assert (sr);

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

void
install_builtin_variables (void)
{
// XXX FIXME XX -- these should probably be moved to where they
// logically belong instead of being all grouped here.

  DEFVAR ("EDITOR", SBV_EDITOR, editor, 0, 0, 1, sv_editor,
    "name of the editor to be invoked by the edit_history command");

  DEFVAR ("I", SBV_I, Complex (0.0, 1.0), 0, 1, 1, 0,
    "sqrt (-1)");

  DEFVAR ("Inf", SBV_Inf, octave_Inf, 0, 1, 1, 0,
    "infinity");

  DEFVAR ("INFO_FILE", SBV_INFO_FILE, info_file, 0, 0, 1, sv_info_file,
    "name of the Octave info file");

  DEFVAR ("J", SBV_J, Complex (0.0, 1.0), 0, 1, 1, 0,
    "sqrt (-1)");

  #if defined (HAVE_ISNAN)
  DEFVAR ("NaN", SBV_NaN, octave_NaN, 0, 1, 1, 0,
    "not a number");
  #endif

  DEFVAR ("LOADPATH", SBV_LOADPATH, load_path, 0, 0, 1, sv_loadpath,
    "colon separated list of directories to search for scripts");

  DEFVAR ("PAGER", SBV_PAGER, default_pager (), 0, 0, 1, sv_pager_binary,
    "path to pager binary");

  DEFVAR ("PS1", SBV_PS1, "\\s:\\#> ", 0, 0, 1, sv_ps1,
    "primary prompt string");

  DEFVAR ("PS2", SBV_PS2, "> ", 0, 0, 1, sv_ps2,
    "secondary prompt string");

  DEFVAR ("PWD", SBV_PWD, get_working_directory ("initialize_globals"),
	  0, 1, 1, sv_pwd,
    "current working directory");

  DEFVAR ("SEEK_SET", SBV_SEEK_SET, 0.0, 0, 1, 1, 0,
    "used with fseek to position file relative to the beginning");

  DEFVAR ("SEEK_CUR", SBV_SEEK_CUR, 1.0, 0, 1, 1, 0,
    "used with fseek to position file relative to the current position");

  DEFVAR ("SEEK_END", SBV_SEEK_END, 2.0, 0, 1, 1, 0,
    "used with fseek to position file relative to the end");

  DEFVAR ("ans", SBV_ans, , 0, 0, 1, 0,
    "");

  DEFVAR ("commas_in_literal_matrix", SBV_commas_in_literal_matrix, "",
	  0, 0, 1, commas_in_literal_matrix,
    "control auto-insertion of commas in literal matrices");

  DEFVAR ("do_fortran_indexing", SBV_do_fortran_indexing, "false", 0, 0,
	  1, do_fortran_indexing,
    "allow single indices for matrices");

  DEFVAR ("empty_list_elements_ok", SBV_empty_list_elements_ok, "warn",
	  0, 0, 1, empty_list_elements_ok,
    "ignore the empty element in expressions like `a = [[], 1]'");

  DEFVAR ("eps", SBV_eps, DBL_EPSILON, 0, 1, 1, 0,
    "machine precision");

  DEFVAR ("gnuplot_binary", SBV_gnuplot_binary, "gnuplot", 0, 0, 1,
	  sv_gnuplot_binary,
    "path to gnuplot binary");

  DEFVAR ("i", SBV_i, Complex (0.0, 1.0), 1, 1, 1, 0,
    "sqrt (-1)");

  DEFVAR ("ignore_function_time_stamp", SBV_ignore_function_time_stamp,
	  "system", 0, 0, 1,
	  ignore_function_time_stamp,
    "don't check to see if function files have changed since they were\n\
  last compiled.  Possible values are \"system\" and \"all\"");

  DEFVAR ("implicit_str_to_num_ok", SBV_implicit_str_to_num_ok, "false",
	  0, 0, 1, implicit_str_to_num_ok,
    "allow implicit string to number conversion");

  DEFVAR ("inf", SBV_inf, octave_Inf, 0, 1, 1, 0,
    "infinity");

  DEFVAR ("j", SBV_j, Complex (0.0, 1.0), 1, 1, 1, 0,
    "sqrt (-1)");

  #if defined (HAVE_ISNAN)
  DEFVAR ("nan", SBV_nan, octave_NaN, 0, 1, 1, 0,
    "not a number");
  #endif

  DEFVAR ("ok_to_lose_imaginary_part", SBV_ok_to_lose_imaginary_part,
	  "warn", 0, 0, 1, ok_to_lose_imaginary_part,
    "silently convert from complex to real by dropping imaginary part");

  DEFVAR ("output_max_field_width", SBV_output_max_field_width, 10.0, 0,
	  0, 1, set_output_max_field_width,
    "maximum width of an output field for numeric output");

  DEFVAR ("output_precision", SBV_output_precision, 5.0, 0, 0, 1,
	  set_output_precision,
    "number of significant figures to display for numeric output");

  DEFVAR ("page_screen_output", SBV_page_screen_output, "true", 0, 0, 1,
	  page_screen_output,
    "if possible, send output intended for the screen through the pager");

  DEFVAR ("pi", SBV_pi, 4.0 * atan (1.0), 0, 1, 1, 0,
    "ratio of the circumference of a circle to its diameter");

  DEFVAR ("prefer_column_vectors", SBV_prefer_column_vectors, "true", 0,
	  0, 1, prefer_column_vectors,
    "prefer column/row vectors");

  DEFVAR ("prefer_zero_one_indexing", SBV_prefer_zero_one_indexing,
	  "false", 0, 0, 1, prefer_zero_one_indexing,
    "when there is a conflict, prefer zero-one style indexing");

  DEFVAR ("print_answer_id_name", SBV_print_answer_id_name, "true", 0,
	  0, 1, print_answer_id_name,
    "set output style to print `var_name = ...'");

  DEFVAR ("print_empty_dimensions", SBV_print_empty_dimensions, "true",
	  0, 0, 1, print_empty_dimensions,
    "also print dimensions of empty matrices");

  DEFVAR ("propagate_empty_matrices", SBV_propagate_empty_matrices,
	  "true", 0, 0, 1, propagate_empty_matrices,
    "operations on empty matrices return an empty matrix, not an error");

  DEFVAR ("resize_on_range_error", SBV_resize_on_range_error, "true", 0,
	  0, 1, resize_on_range_error,
    "enlarge matrices on assignment");

  DEFVAR ("return_last_computed_value", SBV_return_last_computed_value,
	  "false", 0, 0, 1,
	  return_last_computed_value,
    "if a function does not return any values explicitly, return the\n\
  last computed value");

  DEFVAR ("save_precision", SBV_save_precision, 17.0, 0, 0, 1,
	  set_save_precision,
    "number of significant figures kept by the ASCII save command");

  DEFVAR ("silent_functions", SBV_silent_functions, "false", 0, 0, 1,
	  silent_functions,
    "suppress printing results in called functions");

  DEFVAR ("split_long_rows", SBV_split_long_rows, "true", 0, 0, 1,
	  split_long_rows,
    "split long matrix rows instead of wrapping");

  DEFVAR ("stdin", SBV_stdin, 0.0, 0, 1, 1, 0,
    "file number of the standard input stream");

  DEFVAR ("stdout", SBV_stdout, 1.0, 0, 1, 1, 0,
    "file number of the standard output stream");

  DEFVAR ("stderr", SBV_stderr, 2.0, 0, 1, 1, 0,
    "file number of the standard error stream");

  DEFVAR ("treat_neg_dim_as_zero", SBV_treat_neg_dim_as_zero, "false",
	  0, 0, 1, treat_neg_dim_as_zero,
    "convert negative dimensions to zero");

  DEFVAR ("warn_assign_as_truth_value", SBV_warn_assign_as_truth_value,
	  "true", 0, 0, 1,
	  warn_assign_as_truth_value,
    "produce warning for assignments used as truth values");

  DEFVAR ("warn_comma_in_global_decl", SBV_warn_comma_in_global_decl,
	  "true", 0, 0, 1, warn_comma_in_global_decl,
    "produce warning for commas in global declarations");

  DEFVAR ("warn_divide_by_zero", SBV_warn_divide_by_zero, "true", 0, 0,
	  1, warn_divide_by_zero,
    "on IEEE machines, allow divide by zero errors to be suppressed");
}

// Deleting names from the symbol tables.

DEFUN_TEXT ("clear", Fclear, Sclear, -1, 1,
  "clear [name ...]\n\
\n\
clear symbol(s) matching a list of globbing patterns\n\
if no arguments are given, clear all user-defined variables and functions")
{
  Octave_object retval;

  DEFINE_ARGV("clear");

// Always clear the local table, but don't clear currently compiled
// functions unless we are at the top level.  (Allowing that to happen
// inside functions would result in pretty odd behavior...)

  int clear_user_functions = (curr_sym_tab == top_level_sym_tab);

  if (argc == 1)
    {
      curr_sym_tab->clear ();
      global_sym_tab->clear (clear_user_functions);
    }
  else
    {
      int lcount;
      char **lvars = curr_sym_tab->list (lcount, 0,
					 symbol_def::USER_VARIABLE,
					 SYMTAB_LOCAL_SCOPE);
      int gcount;
      char **gvars = curr_sym_tab->list (gcount, 0,
					 symbol_def::USER_VARIABLE,
					 SYMTAB_GLOBAL_SCOPE);
      int fcount;
      char **fcns = curr_sym_tab->list (fcount, 0,
					symbol_def::USER_FUNCTION,
					SYMTAB_ALL_SCOPES);

      while (--argc > 0)
	{
	  argv++;
	  if (*argv)
	    {
	      int i;
	      for (i = 0; i < lcount; i++)
		{
		  if (fnmatch (*argv, lvars[i], __FNM_FLAGS) == 0)
		    curr_sym_tab->clear (lvars[i]);
		}

	      int count;
	      for (i = 0; i < gcount; i++)
		{
		  if (fnmatch (*argv, gvars[i], __FNM_FLAGS) == 0)
		    {
		      count = curr_sym_tab->clear (gvars[i]);
		      if (count > 0)
			global_sym_tab->clear (gvars[i], clear_user_functions);
		    }
		}

	      for (i = 0; i < fcount; i++)
		{
		  if (fnmatch (*argv, fcns[i], __FNM_FLAGS) == 0)
		    {
		      count = curr_sym_tab->clear (fcns[i]);
		      if (count > 0)
			global_sym_tab->clear (fcns[i], clear_user_functions);
		    }
		}
	    }
	}

      delete [] lvars;
      delete [] gvars;
      delete [] fcns;

    }

  DELETE_ARGV;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
