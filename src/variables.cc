// variables.cc                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstring>

#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "defaults.h"
#include "defun.h"
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "oct-obj.h"
#include "octave-hist.h"
#include "octave.h"
#include "pager.h"
#include "parse.h"
#include "statdefs.h"
#include "symtab.h"
#include "sysdep.h"
#include "tree-base.h"
#include "tree-const.h"
#include "tree-expr.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

extern "C"
{
#include <readline/readline.h>

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

int
is_globally_visible (const char *name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  return (sr && sr->is_linked_to_global ());
}

// Is this tree_constant a valid function?

tree_fvc *
is_valid_function (const tree_constant& arg, char *warn_for, int warn)
{
  tree_fvc *ans = 0;

  const char *fcn_name = arg.string_value ();

  if (error_state)
    {
      if (warn)
	error ("%s: expecting function name as argument", warn_for);
      return ans;
    }

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
  int nargin = fcn->max_expected_args ();
  int e_nargin = expected_nargin;
  if (nargin != e_nargin)
    {
      if (warn)
	error ("%s: expecting function to take %d argument%s",
	       warn_for, e_nargin, (e_nargin == 1 ? "" : "s"));
      return 0;
    }
  return 1;
}

DEFUN ("is_global", Fis_global, Sis_global, 1, 1,
  "is_global (X): return 1 if the string X names a global variable\n\
otherwise, return 0.")
{
  Octave_object retval = 0.0;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("is_global");
      return retval;
    }

  const char *name = args(0).string_value ();

  if (error_state)
    {
      error ("is_global: expecting string argument");
      return retval;
    }

  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);

  retval = (double) (sr && sr->is_linked_to_global ());

  return retval;
}

DEFUN ("exist", Fexist, Sexist, 1, 1,
  "exist (NAME): check if variable or file exists\n\
\n\
return 0 if NAME is undefined, 1 if it is a variable, or 2 if it is\n\
a function.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("exist");
      return retval;
    }

  char *name = strsave (args(0).string_value ());

  if (error_state)
    {
      error ("exist: expecting string argument");
      delete [] name;
      return retval;
    }

  char *struct_elts = strchr (name, '.');
  if (struct_elts)
    {
      *struct_elts = '\0';
      struct_elts++;
    }

  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  if (! sr)
    sr = global_sym_tab->lookup (name, 0, 0);

  retval = 0.0;

  if (sr && sr->is_variable () && sr->is_defined ())
    {
      retval = 1.0;
      tree_fvc *def = sr->def ();
      if (struct_elts)
	{
	  retval = 0.0;
	  if (def->is_constant ())
	    {
	      tree_constant *tmp = (tree_constant *) def;
	      tree_constant ult;
	      ult = tmp->lookup_map_element (struct_elts, 0, 1);
	      if (ult.is_defined ())
		retval = 1.0;
	    }
	}
    }
  else if (sr && sr->is_builtin_function ())
    {
      retval = 5.0;
    }
  else if (sr && sr->is_user_function ())
    {
      retval = 2.0;
    }
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
	  path = oct_file_in_path (name);
	  if (path)
	    {
	      delete [] path;
	      retval = 3.0;
	    }
	  else
	    {
	      struct stat buf;
	      if (stat (name, &buf) == 0 && S_ISREG (buf.st_mode))
		retval = 2.0;
	    }
	}
    }

  delete [] name;

  return retval;
}

// XXX FIXME XXX -- should these really be here?

static char *
octave_home (void)
{
  char *oh = getenv ("OCTAVE_HOME");

  return (oh ? oh : OCTAVE_PREFIX);
}

static char *
subst_octave_home (char *s)
{
  char *home = octave_home ();
  char *prefix = OCTAVE_PREFIX;

  char *retval;

  if (strcmp (home, prefix) == 0)
    retval = strsave (s);
  else
    {
      int len_home = strlen (home);
      int len_prefix = strlen (prefix);

      int count = 0;
      char *ptr = s;
      char *next = 0;
      while ((next = strstr (ptr, prefix)))
	{
	  ptr = next + len_prefix;
	  count++;
	}

      int grow_size = count * (len_home - len_prefix);

      int len_s = strlen (s);

      int len_retval = len_s + count * grow_size;

      retval = new char [len_retval+1];

      char *p1 = s;
      char *p2 = p1;
      char *pdest = retval;

      // Is this really a good way to do this?

      while (count >= 0)
	{
	  p2 = strstr (p1, prefix);

	  if (! p2)
	    {
	      memcpy (pdest, p1, strlen (p1)+1);
	      break;
	    }
	  else if (p1 == p2)
	    {
	      memcpy (pdest, home, len_home);
	      pdest += len_home;
	      p1 += len_prefix;
	      count--;
	    }
	  else
	    {
	      int len = (int) (p2 - p1);
	      memcpy (pdest, p1, len);
	      pdest += len;
	      p1 += len;
	    }

	}
    }

  return retval;
}

static char *
octave_info_dir (void)
{
  static char *retval = subst_octave_home (OCTAVE_INFODIR);
  return retval;
}

char *
octave_arch_lib_dir (void)
{
  static char *retval = subst_octave_home (OCTAVE_ARCHLIBDIR);
  return retval;
}

char *
octave_bin_dir (void)
{
  static char *retval = subst_octave_home (OCTAVE_BINDIR);
  return retval;
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

// Always returns a new string.

char *
maybe_add_default_load_path (const char *p)
{
  static char *std_path = subst_octave_home (OCTAVE_FCNFILEPATH);

  char *pathstring = strsave (p);

  if (pathstring[0] == SEPCHAR)
    {
      char *tmp = pathstring;
      pathstring = strconcat (std_path, pathstring);
      delete [] tmp;
    }

  int tmp_len = strlen (pathstring);
  if (pathstring[tmp_len-1] == SEPCHAR)
    {
      char *tmp = pathstring;
      pathstring = strconcat (pathstring, std_path);
      delete [] tmp;
    }

  return pathstring;
}

char *
octave_lib_dir (void)
{
  static char *retval = subst_octave_home (OCTAVE_LIBDIR);
  return retval;
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
  static char *std_path = subst_octave_home (OCTAVE_FCNFILEPATH);

  static char *oct_path = getenv ("OCTAVE_PATH");

  static char *pathstring = 0;
  delete [] pathstring;

  return oct_path ? strsave (oct_path) : strsave (std_path);
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
  static char *startupdir = subst_octave_home (OCTAVE_STARTUPFILEDIR);
  static char *sd = strconcat (startupdir, "/octaverc");
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

static int
looks_like_octave_copyright (char *s)
{
  if (s && strncmp (s, " Copyright (C) ", 15) == 0)
    {
      s = strchr (s, '\n');
      if (s)
	{
	  s++;
	  s = strchr (s, '\n');
	  if (s)
	    {
	      s++;
	      if (strncmp (s, " This file is part of Octave.", 29) == 0)
		return 1;
	    }
	}
    }
  return 0;
}

// Eat whitespace and comments from FFILE, returning the text of the
// comments read if it doesn't look like a copyright notice.  If
// IN_PARTS, consider each block of comments separately; otherwise,
// grab them all at once.

static char *
gobble_leading_white_space (FILE *ffile, int in_parts)
{
  ostrstream buf;

  int first_comments_seen = 0;
  int have_help_text = 0;
  int in_comment = 0;
  int c;
  while ((c = getc (ffile)) != EOF)
    {
      current_input_column++;
      if (in_comment)
	{
	  if (! have_help_text)
	    {
	      first_comments_seen = 1;
	      buf << (char) c;
	    }

	  if (c == '\n')
	    {
	      input_line_number++;
	      current_input_column = 0;
	      in_comment = 0;
	      if (in_parts)
		{
		  if ((c = getc (ffile)) != EOF)
		    {
		      current_input_column--;
		      ungetc (c, ffile);
		      if (c == '\n')
			break;
		    }
		  else
		    break;
		}
	    }
	}
      else
	{
	  switch (c)
	    {
	    case ' ':
	    case '\t':
	      if (first_comments_seen)
		have_help_text = 1;
	      break;

	    case '\n':
	      if (first_comments_seen)
		have_help_text = 1;
	      input_line_number++;
	      current_input_column = 0;
	      continue;

	    case '%':
	    case '#':
	      in_comment = 1;
	      break;

	    default:
	      current_input_column--;
	      ungetc (c, ffile);
	      goto done;
	    }
	}
    }

 done:

  buf << ends;
  char *help_txt = buf.str ();

  if (help_txt)
    {
      if (looks_like_octave_copyright (help_txt)) 
	{
	  delete [] help_txt;
	  help_txt = 0;
	}

      if (in_parts && ! help_txt)
	help_txt = gobble_leading_white_space (ffile, in_parts);
    }

  if (help_txt && ! *help_txt)
    {
      delete [] help_txt;
      help_txt = 0;
    }

  return help_txt;
}

static int
is_function_file (FILE *ffile)
{
  int status = 0;

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
      // Check to see if this file defines a function or is just a
      // list of commands.

      char *tmp_help_txt = gobble_leading_white_space (ffile, 0);

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

	  delete [] help_buf;
	  help_buf = tmp_help_txt;

	  int status = yyparse ();

	  if (status != 0)
	    {
	      error ("parse error while reading function file %s", ff);
	      global_sym_tab->clear (curr_fcn_file_name);
	    }
	}
      else if (exec_script)
	{
	  // We don't need this now.

	  delete [] tmp_help_txt;

	  // The value of `reading_fcn_file' will be restored to the
	  // proper value when we unwind from this frame.

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

  // This is needed by yyparse.

  curr_fcn_file_name = nm;

#ifdef WITH_DLD

  if (load_octave_oct_file (nm))
    {
      force_link_to_function (nm);
    }
  else

#endif

    {
      char *ff = fcn_file_in_path (nm);

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
  int script_executed = 0;

  if (! sym_rec->is_linked_to_global ())
    {
      if (sym_rec->is_defined ())
	{
	  if (sym_rec->is_function () && symbol_out_of_date (sym_rec))
	    script_executed = load_fcn_from_file (sym_rec, exec_script);
	}
      else if (! sym_rec->is_formal_parameter ())
	{
	  link_to_builtin_or_function (sym_rec);

	  if (! sym_rec->is_defined ())
	    script_executed = load_fcn_from_file (sym_rec, exec_script);
	  else if (sym_rec->is_function () && symbol_out_of_date (sym_rec))
	    script_executed = load_fcn_from_file (sym_rec, exec_script);
	}
    }

  return script_executed;
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

char *
get_help_from_file (const char *path)
{
  if (path && *path)
    {
      FILE *fptr = fopen (path, "r");
      if (fptr)
	{
	  char *help_txt = gobble_leading_white_space (fptr, 1);
	  fclose (fptr);
	  return help_txt;
	}
    }
  return 0;
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

      if (! error_state && val.is_string ())
	{
	  const char *s = val.string_value ();

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

      if (! error_state && val.is_scalar_type ())
	{
	  d = val.double_value ();
	  status = 0;
	}
    }

  return status;
}

// Look for the given name in the global symbol table.

tree_constant
builtin_any_variable (const char *name)
{
  tree_constant retval;

  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

  // It is a prorgramming error to look for builtins that aren't.

  assert (sr);

  tree_fvc *defn = sr->def ();

  if (defn)
    retval = defn->eval (0);

  return retval;
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

  // If the global symbol is currently defined as a function, we need
  // to hide it with a variable.

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

// Help stuff.  Shouldn't this go in help.cc?

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
maybe_list (const char *header, char **argv, int argc,
	    ostrstream& output_buf, int show_verbose, symbol_table
	    *sym_tab, unsigned type, unsigned scope)
{
  int count;
  int status = 0;
  if (show_verbose)
    {
      symbol_record_info *symbols;
      symbols = sym_tab->long_list (count, argv, argc, 1, type, scope);
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
      char **symbols = sym_tab->list (count, argv, argc, 1, type, scope);
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

  while (--argc > 0)
    {
      argv++;

      if (strcmp (*argv, "-all") == 0 || strcmp (*argv, "-a") == 0)
	{
	  show_builtins++;
	  show_functions++;
	  show_variables++;
	}
      else if (strcmp (*argv, "-builtins") == 0 || strcmp (*argv, "-b") == 0)
	show_builtins++;
      else if (strcmp (*argv, "-functions") == 0 || strcmp (*argv, "-f") == 0)
	show_functions++;
      else if (strcmp (*argv, "-long") == 0 || strcmp (*argv, "-l") == 0)
	show_verbose++;
      else if (strcmp (*argv, "-variables") == 0 || strcmp (*argv, "-v") == 0)
	show_variables++;
      else if (*argv[0] == '-')
	warning ("%s: unrecognized option `%s'", my_name, *argv);
      else
	break;
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
      pad_after += maybe_list ("*** built-in variables:", argv, argc,
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_VARIABLE,
			       SYMTAB_ALL_SCOPES);

      pad_after += maybe_list ("*** built-in functions:", argv, argc,
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_functions)
    {
      pad_after += maybe_list ("*** currently compiled functions:",
			       argv, argc, output_buf, show_verbose,
			       global_sym_tab, symbol_def::USER_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_variables)
    {
      pad_after += maybe_list ("*** local user variables:", argv, argc,
			       output_buf, show_verbose, curr_sym_tab,
			       symbol_def::USER_VARIABLE,
			       SYMTAB_LOCAL_SCOPE);

      pad_after += maybe_list ("*** globally visible user variables:",
			       argv, argc, output_buf, show_verbose,
			       curr_sym_tab, symbol_def::USER_VARIABLE,
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

  int nargin = args.length ();

  Octave_object tmp_args;
  for (int i = nargin; i > 0; i--)
    tmp_args(i) = args(i-1);
  tmp_args(0) = "-long";

  int argc = tmp_args.length () + 1;
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
  mfcn.name = strsave (mf->name);
  mfcn.can_return_complex_for_real_arg = mf->can_return_complex_for_real_arg;
  mfcn.lower_limit = mf->lower_limit;
  mfcn.upper_limit = mf->upper_limit;
  mfcn.d_d_mapper = mf->d_d_mapper;
  mfcn.d_c_mapper = mf->d_c_mapper;
  mfcn.c_c_mapper = mf->c_c_mapper;

  tree_builtin *def = new tree_builtin (1, 1, mfcn, mf->name);

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
    panic ("can't find symbol record for builtin function `%s'", alias);
}

// Defining variables.

#if 0
void
bind_nargin_and_nargout (symbol_table *sym_tab, int nargin, int nargout)
{
  tree_constant *tmp;
  symbol_record *sr;

  sr = sym_tab->lookup ("nargin", 1, 0);
  sr->unprotect ();
  tmp = new tree_constant (nargin);
  sr->define (tmp);

  sr = sym_tab->lookup ("nargout", 1, 0);
  sr->unprotect ();
  tmp = new tree_constant (nargout);
  sr->define (tmp);
}
#endif

void
bind_ans (const tree_constant& val, int print)
{
  static symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

  tree_identifier *ans_id = new tree_identifier (sr);
  tree_constant *tmp = new tree_constant (val);

  // XXX FIXME XXX -- making ans_id static, passing its address to
  // tree_simple_assignment_expression along with a flag to not delete
  // it seems to create a memory leak.  Hmm.

  tree_simple_assignment_expression tmp_ass (ans_id, tmp, 0, 1);

  tmp_ass.eval (print);
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
bind_builtin_variable (const char *varname, const tree_constant& val,
		       int protect, int eternal, sv_Function sv_fcn,
		       const char *help)
{
  tree_constant *tc = new tree_constant (val);
  bind_builtin_variable (varname, tc, protect, eternal, sv_fcn, help);
}

void
install_builtin_variables (void)
{
  // XXX FIXME XX -- these should probably be moved to where they
  // logically belong instead of being all grouped here.

  DEFVAR ("EDITOR", SBV_EDITOR, editor, 0, sv_editor,
    "name of the editor to be invoked by the edit_history command");

  DEFCONST ("I", SBV_I, Complex (0.0, 1.0), 0, 0,
    "sqrt (-1)");

  DEFCONST ("Inf", SBV_Inf, octave_Inf, 0, 0,
    "infinity");

  DEFVAR ("INFO_FILE", SBV_INFO_FILE, info_file, 0, sv_info_file,
    "name of the Octave info file");

  DEFCONST ("J", SBV_J, Complex (0.0, 1.0), 0, 0,
    "sqrt (-1)");

  DEFCONST ("NaN", SBV_NaN, octave_NaN, 0, 0,
    "not a number");

  DEFVAR ("LOADPATH", SBV_LOADPATH, load_path, 0, sv_loadpath,
    "colon separated list of directories to search for scripts");

  DEFVAR ("IMAGEPATH", SBV_IMAGEPATH, OCTAVE_IMAGEPATH, 0,
	  sv_imagepath,
    "colon separated list of directories to search for image files");

  DEFCONST ("OCTAVE_VERSION", SBV_version, OCTAVE_VERSION, 0, 0,
    "Octave version");

  DEFVAR ("PAGER", SBV_PAGER, default_pager (), 0, sv_pager_binary,
    "path to pager binary");

  DEFVAR ("PS1", SBV_PS1, "\\s:\\#> ", 0, sv_ps1,
    "primary prompt string");

  DEFVAR ("PS2", SBV_PS2, "> ", 0, sv_ps2,
    "secondary prompt string");

  DEFVAR ("PS4", SBV_PS4, "+ ", 0, sv_ps4,
    "string printed before echoed input (enabled by --echo-input)");

  DEFCONST ("PWD", SBV_PWD,
	    get_working_directory ("initialize_globals"), 0, sv_pwd,
    "current working directory");

  DEFCONST ("SEEK_SET", SBV_SEEK_SET, 0.0, 0, 0,
    "used with fseek to position file relative to the beginning");

  DEFCONST ("SEEK_CUR", SBV_SEEK_CUR, 1.0, 0, 0,
    "used with fseek to position file relative to the current position");

  DEFCONST ("SEEK_END", SBV_SEEK_END, 2.0, 0, 0,
    "used with fseek to position file relative to the end");

  DEFVAR ("ans", SBV_ans, , 0, 0,
    "");

  DEFCONST ("argv", SBV_argv, , 0, 0,
    "the command line arguments this program was invoked with");

  DEFVAR ("automatic_replot", SBV_automatic_replot, "false",
	  0, automatic_replot,
    "if true, auto-insert a replot command when a plot changes");

  DEFVAR ("beep_on_error", SBV_beep_on_error, "false", 0,
	  beep_on_error,
    "if true, beep before printing error messages");

  DEFVAR ("default_return_value", SBV_default_return_value, Matrix (),
	  0, 0,
    "the default for value for unitialized variables returned from\n\
functions.  Only used if the variable initialize_return_values is\n\
set to \"true\".");

  DEFVAR ("default_save_format", SBV_default_save_format, "ascii",
	  0, sv_default_save_format,
    "default format for files created with save, may be one of\n\
\"binary\", \"text\", or \"mat-binary\"");

  DEFVAR ("define_all_return_values", SBV_define_all_return_values,
	  "false", 0, define_all_return_values,
    "control whether values returned from functions should have a\n\
value even if one has not been explicitly assigned.  See also\n\
default_return_value");

  DEFVAR ("do_fortran_indexing", SBV_do_fortran_indexing, "false", 0,
	  do_fortran_indexing,
    "allow single indices for matrices");

  DEFVAR ("empty_list_elements_ok", SBV_empty_list_elements_ok,
	  "warn", 0, empty_list_elements_ok,
    "ignore the empty element in expressions like `a = [[], 1]'");

  DEFCONST ("eps", SBV_eps, DBL_EPSILON, 0, 0,
    "machine precision");

  DEFVAR ("gnuplot_binary", SBV_gnuplot_binary, "gnuplot", 0,
	  sv_gnuplot_binary,
    "path to gnuplot binary");

  DEFCONST ("i", SBV_i, Complex (0.0, 1.0), 1, 0,
    "sqrt (-1)");

  DEFVAR ("ignore_function_time_stamp",
	  SBV_ignore_function_time_stamp, "system", 0,
	  ignore_function_time_stamp,
    "don't check to see if function files have changed since they were\n\
  last compiled.  Possible values are \"system\" and \"all\"");

  DEFVAR ("implicit_str_to_num_ok", SBV_implicit_str_to_num_ok,
	  "false", 0, implicit_str_to_num_ok,
    "allow implicit string to number conversion");

  DEFCONST ("inf", SBV_inf, octave_Inf, 0, 0,
    "infinity");

  DEFCONST ("j", SBV_j, Complex (0.0, 1.0), 1, 0,
    "sqrt (-1)");

  DEFCONST ("nan", SBV_nan, octave_NaN, 0, 0,
    "not a number");

  DEFVAR ("ok_to_lose_imaginary_part", SBV_ok_to_lose_imaginary_part,
	  "warn", 0, ok_to_lose_imaginary_part,
    "silently convert from complex to real by dropping imaginary part");

  DEFVAR ("output_max_field_width", SBV_output_max_field_width, 10.0,
	  0, set_output_max_field_width,
    "maximum width of an output field for numeric output");

  DEFVAR ("output_precision", SBV_output_precision, 5.0, 0,
	  set_output_precision,
    "number of significant figures to display for numeric output");

  DEFVAR ("page_screen_output", SBV_page_screen_output, "true", 0,
	  page_screen_output,
    "if possible, send output intended for the screen through the pager");

  DEFCONST ("pi", SBV_pi, 4.0 * atan (1.0), 0, 0,
    "ratio of the circumference of a circle to its diameter");

  DEFVAR ("prefer_column_vectors", SBV_prefer_column_vectors, "true",
	  0, prefer_column_vectors,
    "prefer column/row vectors");

  DEFVAR ("prefer_zero_one_indexing", SBV_prefer_zero_one_indexing,
	  "false", 0, prefer_zero_one_indexing,
    "when there is a conflict, prefer zero-one style indexing");

  DEFVAR ("print_answer_id_name", SBV_print_answer_id_name, "true", 0,
	  print_answer_id_name,
    "set output style to print `var_name = ...'");

  DEFVAR ("print_empty_dimensions", SBV_print_empty_dimensions,
	  "true", 0, print_empty_dimensions,
    "also print dimensions of empty matrices");

  DEFCONST ("program_invocation_name", SBV_program_invocation_name,
	    raw_prog_name, 0, 0,
    "the full name of the current program or script, including the\n\
directory specification");

  DEFCONST ("program_name", SBV_program_name, prog_name, 0, 0,
    "the name of the current program or script");

  DEFVAR ("propagate_empty_matrices", SBV_propagate_empty_matrices,
	  "true", 0, propagate_empty_matrices,
    "operations on empty matrices return an empty matrix, not an error");

#if 0
  DEFVAR ("read_only_constants", SBV_read_only_constants, "true", 0,
	  read_only_constants,
    "allow built-in constants to be modified");
#endif

  DEFCONST ("realmax", SBV_realmax, DBL_MAX, 0, 0,
    "realmax (): return largest representable floating point number");

  DEFCONST ("realmin", SBV_realmin, DBL_MIN, 0, 0,
    "realmin (): return smallest representable floating point number");

  DEFVAR ("resize_on_range_error", SBV_resize_on_range_error, "true",
	  0, resize_on_range_error,
    "enlarge matrices on assignment");

  DEFVAR ("return_last_computed_value",
	  SBV_return_last_computed_value, "false", 0,
	  return_last_computed_value,
    "if a function does not return any values explicitly, return the\n\
  last computed value");

  DEFVAR ("save_precision", SBV_save_precision, 17.0, 0,
	  set_save_precision,
    "number of significant figures kept by the ASCII save command");

  DEFVAR ("silent_functions", SBV_silent_functions, "false", 0,
	  silent_functions,
    "suppress printing results in called functions");

  DEFVAR ("split_long_rows", SBV_split_long_rows, "true", 0,
	  split_long_rows,
    "split long matrix rows instead of wrapping");

  DEFVAR ("struct_levels_to_print", SBV_struct_levels_to_print, 2.0,
	  0, struct_levels_to_print,
    "number of levels of structure elements to print");

#ifdef USE_GNU_INFO
  DEFVAR ("suppress_verbose_help_message",
	  SBV_suppress_verbose_help_message, "false", 0,
	  suppress_verbose_help_message,
    "suppress printing of message pointing to additional help in the\n\
help and usage functions");
#endif

  DEFCONST ("stdin", SBV_stdin, 0.0, 0, 0,
    "file number of the standard input stream");

  DEFCONST ("stdout", SBV_stdout, 1.0, 0, 0,
    "file number of the standard output stream");

  DEFCONST ("stderr", SBV_stderr, 2.0, 0, 0,
    "file number of the standard error stream");

  DEFVAR ("treat_neg_dim_as_zero", SBV_treat_neg_dim_as_zero, "false",
	  0, treat_neg_dim_as_zero,
    "convert negative dimensions to zero");

  DEFVAR ("warn_assign_as_truth_value",
	  SBV_warn_assign_as_truth_value, "true", 0,
	  warn_assign_as_truth_value,
    "produce warning for assignments used as truth values");

  DEFVAR ("warn_comma_in_global_decl", SBV_warn_comma_in_global_decl,
	  "true", 0, warn_comma_in_global_decl,
    "produce warning for commas in global declarations");

  DEFVAR ("warn_divide_by_zero", SBV_warn_divide_by_zero, "true", 0,
	  warn_divide_by_zero,
    "on IEEE machines, allow divide by zero errors to be suppressed");

  DEFVAR ("warn_function_name_clash", SBV_warn_function_name_clash,
	  "true", 0, warn_function_name_clash,
    "produce warning if function name conflicts with file name");

  DEFVAR ("whitespace_in_literal_matrix",
	  SBV_whitespace_in_literal_matrix, "", 0,
	  whitespace_in_literal_matrix,
    "control auto-insertion of commas and semicolons in literal matrices");
}

// Deleting names from the symbol tables.

DEFUN_TEXT ("clear", Fclear, Sclear, -1, 1,
  "clear [-x] [name ...]\n\
\n\
Clear symbol(s) matching a list of globbing patterns.\n\
\n\
If no arguments are given, clear all user-defined variables and
functions.\n\
\n\
With -x, exclude the named variables")
{
  Octave_object retval;

  DEFINE_ARGV("clear");

  argc--;
  argv++;

  // Always clear the local table, but don't clear currently compiled
  // functions unless we are at the top level.  (Allowing that to
  // happen inside functions would result in pretty odd behavior...)

  int clear_user_functions = (curr_sym_tab == top_level_sym_tab);

  if (argc == 0)
    {
      curr_sym_tab->clear ();
      global_sym_tab->clear (clear_user_functions);
    }
  else
    {
      int exclusive = 0;

      if (argc > 0)
	{
	  if (strcmp (*argv, "-x") == 0)
	    {
	      exclusive = 1;
	      argv++;
	      argc--;
	    }
	}

      int lcount = 0;
      int gcount = 0;
      int fcount = 0;

      char **lvars = 0;
      char **gvars = 0;
      char **fcns = 0;

      if (argc > 0)
	{
	  lvars = curr_sym_tab->list (lcount, 0, 0, 0,
				      SYMTAB_VARIABLES,
				      SYMTAB_LOCAL_SCOPE);

	  gvars = curr_sym_tab->list (gcount, 0, 0, 0,
				      SYMTAB_VARIABLES,
				      SYMTAB_GLOBAL_SCOPE);

	  fcns = global_sym_tab->list (fcount, 0, 0, 0,
				       symbol_def::USER_FUNCTION,
				       SYMTAB_ALL_SCOPES);
	}

      while (argc > 0)
	{
	  char *pat = *argv;

	  if (pat)
	    {
	      int i;
	      for (i = 0; i < lcount; i++)
		{
		  char *nm = lvars[i];
		  int match = (fnmatch (pat, nm, __FNM_FLAGS) == 0);
		  if ((exclusive && ! match) || (! exclusive && match))
		    curr_sym_tab->clear (nm);
		}

	      int count;
	      for (i = 0; i < gcount; i++)
		{
		  char *nm = gvars[i];
		  int match = (fnmatch (pat, nm, __FNM_FLAGS) == 0);
		  if ((exclusive && ! match) || (! exclusive && match))
		    {
		      count = curr_sym_tab->clear (nm);
		      if (count > 0)
			global_sym_tab->clear (nm, clear_user_functions);
		    }
		}

	      for (i = 0; i < fcount; i++)
		{
		  char *nm = fcns[i];
		  int match = (fnmatch (pat, nm, __FNM_FLAGS) == 0);
		  if ((exclusive && ! match) || (! exclusive && match))
		    {
		      count = curr_sym_tab->clear (nm);
		      global_sym_tab->clear (nm, clear_user_functions);
		    }
		}
	    }

	  argc--;
	  argv++;
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
