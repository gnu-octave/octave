/*

Copyright (C) 1996 John W. Eaton

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
#include <cmath>
#include <cstdio>
#include <cstring>

#include <string>

#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#if defined (USE_READLINE)
#include <readline/readline.h>
#endif

#include "file-ops.h"
#include "oct-glob.h"
#include "str-vec.h"

#include <defaults.h>
#include "data.h"
#include "defun.h"
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "file-io.h"
#include "fn-cache.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "load-save.h"
#include "mappers.h"
#include "oct-hist.h"
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "symtab.h"
#include "sysdep.h"
#include "pt-const.h"
#include "oct-obj.h"
#include "pt-exp.h"
#include "pt-fcn.h"
#include "pt-fvc.h"
#include "pt-mat.h"
#include "pt-plot.h"
#include "pr-output.h"
#include "syscalls.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

// Echo commands as they are executed?
//
//   1  ==>  echo commands read from script files
//   2  ==>  echo commands from functions
//   4  ==>  echo commands read from command line
//
// more than one state can be active at once.
int Vecho_executing_commands;

// Where history is saved.
static string Vhistory_file;

// The number of lines to keep in the history file.
static int Vhistory_size;

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp;

// TRUE if we are saving history.
static int Vsaving_history;

// Symbol table for symbols at the top level.
symbol_table *top_level_sym_tab = 0;

// Symbol table for the current scope.
symbol_table *curr_sym_tab = 0;

// Symbol table for global symbols.
symbol_table *global_sym_tab = 0;

octave_variable_reference::octave_variable_reference (tree_indirect_ref *i)
  : id (0), indir (i)
{
  if (indir->is_identifier_only ())
    {
      id = indir->ident ();
      indir = 0;
    }
}

void
octave_variable_reference::assign (const octave_value& rhs)
{
  if (id)
    id->assign (rhs);
  else if (indir)
    {
      octave_value& ult = indir->reference ();
      ult = rhs;
    }
  else
    panic_impossible ();
}

void
octave_variable_reference::assign (const octave_value_list& idx,
				   const octave_value& rhs)
{
  if (id)
    id->assign (idx, rhs);
  else if (indir)
    {
      octave_value& ult = indir->reference ();
      ult.assign (idx, rhs);
    }
  else
    panic_impossible ();
}

octave_value
octave_variable_reference::value (void)
{
  octave_value retval;

  if (id)
    retval = id->value ();
  else if (indir)
    retval = indir->value ();
  else
    panic_impossible ();

  return retval;
}
  
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

bool
is_builtin_variable (const string& name)
{
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);
  return (sr && sr->is_builtin_variable ());
}

// Is this a text-style function?

bool
is_text_function_name (const string& s)
{
  symbol_record *sr = global_sym_tab->lookup (s);
  return (sr && sr->is_text_function ());
}

// Is this a mapper function?

bool
is_builtin_function_name (const string& s)
{
  symbol_record *sr = global_sym_tab->lookup (s);
  return (sr && sr->is_builtin_function ());
}

// Is this a mapper function?

bool
is_mapper_function_name (const string& s)
{
  symbol_record *sr = global_sym_tab->lookup (s);
  return (sr && sr->is_mapper_function ());
}

// Is this function globally in this scope?

bool
is_globally_visible (const string& name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);
  return (sr && sr->is_linked_to_global ());
}

// Is this octave_value a valid function?

tree_fvc *
is_valid_function (const octave_value& arg, const string& warn_for, int warn)
{
  tree_fvc *ans = 0;

  string fcn_name;

  if (arg.is_string ())
    fcn_name = arg.string_value ();

  if (fcn_name.empty () || error_state)
    {
      if (warn)
	error ("%s: expecting function name as argument",
	       warn_for.c_str ());
      return ans;
    }

  symbol_record *sr = 0;

  if (! fcn_name.empty ())
    sr = lookup_by_name (fcn_name);

  if (sr)
    ans = sr->def ();

  if (! sr || ! ans || ! sr->is_function ())
    {
      if (warn)
	error ("%s: the symbol `%s' is not valid as a function",
	       warn_for.c_str (), fcn_name.c_str ());
      ans = 0;
    }

  return ans;
}

tree_fvc *
extract_function (const octave_value& arg, const string& warn_for,
		  const string& fname, const string& header,
		  const string& trailer)
{
  tree_fvc *retval = 0;

  retval = is_valid_function (arg, warn_for, 0);

  if (! retval)
    {
      string s = arg.string_value ();

      string cmd = header;
      cmd.append (s);
      cmd.append (trailer);

      if (! error_state)
	{
	  int parse_status;

	  eval_string (cmd, 0, parse_status);

	  if (parse_status == 0)
	    {
	      retval = is_valid_function (fname, warn_for, 0);
      
	      if (! retval)
		{
		  error ("%s: `%s' is not valid as a function",
			 warn_for.c_str (), fname.c_str ());
		  return retval;
		}
	    }
	  else
	    error ("%s: `%s' is not valid as a function",
		   warn_for.c_str (), fname.c_str ());
	}
      else
	error ("%s: expecting first argument to be a string",
	       warn_for.c_str ());
    }

  return retval;
}


DEFUN (is_global, args, ,
  "is_global (X): return 1 if the string X names a global variable\n\
otherwise, return 0.")
{
  octave_value_list retval = 0.0;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("is_global");
      return retval;
    }

  string name = args(0).string_value ();

  if (error_state)
    {
      error ("is_global: expecting string argument");
      return retval;
    }

  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);

  retval = static_cast<double> (sr && sr->is_linked_to_global ());

  return retval;
}

DEFUN (exist, args, ,
  "exist (NAME): check if variable or file exists\n\
\n\
returns:\n\
\n\
   0 : NAME is undefined\n\
   1 : NAME is a variable\n\
   2 : NAME is a function\n\
   3 : NAME is a .oct file in the current LOADPATH\n\
   5 : NAME is a built-in function")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("exist");
      return retval;
    }

  string name = args(0).string_value ();

  if (error_state)
    {
      error ("exist: expecting string argument");
      return retval;
    }

  string struct_elts;
  string symbol_name = name;

  size_t pos = name.find ('.');

  if (pos != NPOS && pos > 0)
    {
      struct_elts = name.substr (pos+1);
      symbol_name = name.substr (0, pos);
    }

  symbol_record *sr = curr_sym_tab->lookup (symbol_name, 0, 0);
  if (! sr)
    sr = global_sym_tab->lookup (symbol_name, 0, 0);

  retval = 0.0;

  if (sr && sr->is_variable () && sr->is_defined ())
    {
      if (struct_elts.empty () || sr->is_map_element (struct_elts))
	retval = 1.0;
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
      string path = fcn_file_in_path (name);

      if (path.length () > 0)
	{
	  retval = 2.0;
	}
      else
	{
	  path = oct_file_in_path (name);

	  if (path.length () > 0)
	    {
	      retval = 3.0;
	    }
	  else
	    {
	      file_stat fs (name);

	      if (fs && fs.is_reg ())
		retval = 2.0;
	    }
	}
    }

  return retval;
}

// Is there a corresponding function file that is newer than the
// symbol definition?

static int
symbol_out_of_date (symbol_record *sr)
{
  if (Vignore_function_time_stamp == 2)
    return 0;

  if (sr)
    {
      tree_fvc *ans = sr->def ();
      if (ans)
	{
	  string ff = ans->fcn_file_name ();
	  if (! ff.empty ()
	      && ! (Vignore_function_time_stamp
		    && ans->is_system_fcn_file ()))
	    {
	      time_t tp = ans->time_parsed ();

	      string fname = fcn_file_in_path (ff);

	      int status = is_newer (fname, tp);

	      if (status > 0)
		return 1;
	    }
	}
    }
  return 0;
}

static int
looks_like_octave_copyright (const string& s)
{
  string t = s.substr (0, 15);

  if (t == " Copyright (C) ")
    {
      size_t pos = s.find ('\n');

      if (pos != NPOS)
	{
	  pos = s.find ('\n', pos + 1);

	  if (pos != NPOS)
	    {
	      pos++;

	      t = s.substr (pos, 29);

	      if (t == " This file is part of Octave."
		  || t == " This program is free softwar")
		return 1;
	    }
	}
    }
  return 0;
}

// Eat whitespace and comments from FFILE, returning the text of the
// comments read if it doesn't look like a copyright notice.  If
// IN_PARTS, consider each block of comments separately; otherwise,
// grab them all at once.  If UPDATE_POS is TRUE, line and column
// number information is updated.

// XXX FIXME XXX -- grab_help_text() in lex.l duplicates some of this
// code!

static string
gobble_leading_white_space (FILE *ffile, bool in_parts, bool update_pos)
{
  string help_txt;

  bool first_comments_seen = false;
  bool begin_comment = false;
  bool have_help_text = false;
  bool in_comment = false;
  int c;

  while ((c = getc (ffile)) != EOF)
    {
      if (update_pos)
	current_input_column++;

      if (begin_comment)
	{
	  if (c == '%' || c == '#')
	    continue;
	  else
	    begin_comment = false;
	}

      if (in_comment)
	{
	  if (! have_help_text)
	    {
	      first_comments_seen = true;
	      help_txt += (char) c;
	    }

	  if (c == '\n')
	    {
	      if (update_pos)
		{
		  input_line_number++;
		  current_input_column = 0;
		}
	      in_comment = false;

	      if (in_parts)
		{
		  if ((c = getc (ffile)) != EOF)
		    {
		      if (update_pos)
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
		have_help_text = true;
	      break;

	    case '\n':
	      if (first_comments_seen)
		have_help_text = true;
	      if (update_pos)
		{
		  input_line_number++;
		  current_input_column = 0;
		}
	      continue;

	    case '%':
	    case '#':
	      begin_comment = true;
	      in_comment = true;
	      break;

	    default:
	      if (update_pos)
		current_input_column--;
	      ungetc (c, ffile);
	      goto done;
	    }
	}
    }

 done:

  if (! help_txt.empty ())
    {
      if (looks_like_octave_copyright (help_txt)) 
	help_txt.resize (0);

      if (in_parts && help_txt.empty ())
	help_txt = gobble_leading_white_space (ffile, in_parts, update_pos);
    }

  return help_txt;
}

static int
is_function_file (FILE *ffile)
{
  int status = 0;

  long pos = ftell (ffile);

  gobble_leading_white_space (ffile, false, false);

  char buf [10];
  fgets (buf, 10, ffile);
  int len = strlen (buf);
  if (len > 8 && strncmp (buf, "function", 8) == 0
      && ! (isalnum (buf[8]) || buf[8] == '_'))
    status = 1;

  fseek (ffile, pos, SEEK_SET);

  return status;
}

static void
restore_command_history (void *)
{
  octave_command_history.ignore_entries (! Vsaving_history);
}

static void
safe_fclose (void *f)
{
  if (f)
    fclose (static_cast<FILE *> (f));
}

static int
parse_fcn_file (int exec_script, const string& ff)
{
  begin_unwind_frame ("parse_fcn_file");

  int script_file_executed = 0;

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

  add_unwind_protect (safe_fclose, ffile);

  if (ffile)
    {
      // Check to see if this file defines a function or is just a
      // list of commands.

      if (is_function_file (ffile))
	{
	  // XXX FIXME XXX -- we shouldn't need both the
	  // octave_command_history object and the
	  // Vsaving_history variable...
	  octave_command_history.ignore_entries ();

	  add_unwind_protect (restore_command_history, 0);

	  unwind_protect_int (Vecho_executing_commands);
	  unwind_protect_int (Vsaving_history);
	  unwind_protect_int (reading_fcn_file);
	  unwind_protect_int (input_from_command_line_file);

	  Vecho_executing_commands = ECHO_OFF;
	  Vsaving_history = 0;
	  reading_fcn_file = 1;
	  input_from_command_line_file = 0;

	  YY_BUFFER_STATE old_buf = current_buffer ();
	  YY_BUFFER_STATE new_buf = create_buffer (ffile);

	  add_unwind_protect (restore_input_buffer, (void *) old_buf);
	  add_unwind_protect (delete_input_buffer, (void *) new_buf);

	  switch_to_buffer (new_buf);

	  unwind_protect_ptr (curr_sym_tab);

	  reset_parser ();

	  help_buf = gobble_leading_white_space (ffile, true, true);

	  // XXX FIXME XXX -- this should not be necessary.
	  gobble_leading_white_space (ffile, false, true);

	  int status = yyparse ();

	  if (status != 0)
	    {
	      error ("parse error while reading function file %s",
		     ff.c_str ());
	      global_sym_tab->clear (curr_fcn_file_name);
	    }
	}
      else if (exec_script)
	{
	  // The value of `reading_fcn_file' will be restored to the
	  // proper value when we unwind from this frame.
	  reading_fcn_file = old_reading_fcn_file_state;

	  // XXX FIXME XXX -- we shouldn't need both the
	  // octave_command_history object and the
	  // Vsaving_history variable...
	  octave_command_history.ignore_entries ();

	  add_unwind_protect (restore_command_history, 0);

	  unwind_protect_int (Vsaving_history);
	  unwind_protect_int (reading_script_file);

	  Vsaving_history = 0;
	  reading_script_file = 1;

	  parse_and_execute (ffile, 1);

	  script_file_executed = 1;
	}
    }

  run_unwind_frame ("parse_fcn_file");

  return script_file_executed;
}

static bool
load_fcn_from_file (symbol_record *sym_rec, int exec_script)
{
  bool script_file_executed = false;

  string nm = sym_rec->name ();

  if (load_octave_oct_file (nm))
    {
      force_link_to_function (nm);
    }
  else
    {
      string ff = fcn_file_in_path (nm);

      // These are needed by yyparse.

      begin_unwind_frame ("load_fcn_from_file");

      unwind_protect_str (curr_fcn_file_name);
      unwind_protect_str (curr_fcn_file_full_name);

      curr_fcn_file_name = nm;
      curr_fcn_file_full_name = ff;

      if (ff.length () > 0)
	script_file_executed = parse_fcn_file (exec_script, ff);

      if (! (error_state || script_file_executed))
	force_link_to_function (nm);

      run_unwind_frame ("load_fcn_from_file");
    }

  return script_file_executed;
}

bool
lookup (symbol_record *sym_rec, int exec_script)
{
  bool script_executed = false;

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
lookup_by_name (const string& nm, int exec_script)
{
  symbol_record *sym_rec = curr_sym_tab->lookup (nm, 1, 0);

  lookup (sym_rec, exec_script);

  return sym_rec;
}

string
get_help_from_file (const string& path)
{
  string retval;

  if (! path.empty ())
    {
      FILE *fptr = fopen (path.c_str (), "r");

      if (fptr)
	{
	  add_unwind_protect (safe_fclose, (void *) fptr);

	  retval = gobble_leading_white_space (fptr, true, true);

	  run_unwind_protect ();
	}
    }

  return retval;
}

// Variable values.

// Look for the given name in the global symbol table.  If it refers
// to a string, return a new copy.  If not, return 0;

string
builtin_string_variable (const string& name)
{
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

  // It is a prorgramming error to look for builtins that aren't.

  assert (sr);

  string retval;

  tree_fvc *defn = sr->def ();

  if (defn)
    {
      octave_value val = defn->eval (0);

      if (! error_state && val.is_string ())
	retval = val.string_value ();
    }

  return retval;
}

// Look for the given name in the global symbol table.  If it refers
// to a real scalar, place the value in d and return 1.  Otherwise,
// return 0.

int
builtin_real_scalar_variable (const string& name, double& d)
{
  int status = 0;
  symbol_record *sr = global_sym_tab->lookup (name, 0, 0);

  // It is a prorgramming error to look for builtins that aren't.

  assert (sr);

  tree_fvc *defn = sr->def ();

  if (defn)
    {
      octave_value val = defn->eval (0);

      if (! error_state && val.is_scalar_type ())
	{
	  d = val.double_value ();
	  status = 1;
	}
    }

  return status;
}

// Look for the given name in the global symbol table.

octave_value
builtin_any_variable (const string& name)
{
  octave_value retval;

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

  string nm = sr->name ();

  symbol_record *gsr = global_sym_tab->lookup (nm, 1, 0);

  if (sr->is_formal_parameter ())
    {
      error ("can't make function parameter `%s' global", nm.c_str ());
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
force_link_to_function (const string& id_name)
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

string_vector
make_name_list (void)
{
  int key_len = 0;
  int glb_len = 0;
  int top_len = 0;
  int lcl_len = 0;

  string_vector key;
  string_vector glb;
  string_vector top;
  string_vector lcl;
  string_vector ffl;

  // Each of these functions returns a new vector of pointers to new
  // strings.

  key = names (keyword_help (), key_len);

  glb = global_sym_tab->list (glb_len);

  top = top_level_sym_tab->list (top_len);

  if (top_level_sym_tab != curr_sym_tab)
    lcl = curr_sym_tab->list (lcl_len);

  ffl = octave_fcn_file_name_cache::list_no_suffix ();
  int ffl_len = ffl.length ();

  int total_len = key_len + glb_len + top_len + lcl_len + ffl_len;

  string_vector list (total_len);

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

  return list;
}

// List variable names.

static void
print_symbol_info_line (ostream& os, const symbol_record_info& s)
{
  os << (s.is_read_only () ? " -" : " w");
  os << (s.is_eternal () ? "- " : "d ");
#if 0
  os << (s.hides_fcn () ? "f" : (s.hides_builtin () ? "F" : "-"));
#endif
  os.form ("  %-16s", s.type_name ().c_str ());

  int nr = s.rows ();
  int nc = s.columns ();

  if (nr < 0)
    os << "      -";
  else
    os.form ("%7d", nr);

  if (nc < 0)
    os << "      -";
  else
    os.form ("%7d", nc);

  os << "  " << s.name () << "\n";
}

static void
print_long_listing (ostream& os, symbol_record_info *s)
{
  if (! s)
    return;

  symbol_record_info *ptr = s;
  while (ptr->is_defined ())
    {
      print_symbol_info_line (os, *ptr);
      ptr++;
    }
}

static int
maybe_list (const char *header, const string_vector& argv, int argc,
	    ostream& os, int show_verbose, symbol_table
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
	  os << "\n" << header << "\n\n"
		     << "prot  type               rows   cols  name\n"
		     << "====  ====               ====   ====  ====\n";

	  print_long_listing (os, symbols);
	  status = 1;
	}
      delete [] symbols;
    }
  else
    {
      string_vector symbols = sym_tab->list (count, argv, argc, 1,
					     type, scope);
      if (symbols.length () > 0 && count > 0)
	{
	  os << "\n" << header << "\n\n";
	  symbols.list_in_columns (os);
	  status = 1;
	}
    }
  return status;
}

DEFUN (document, args, ,
  "document (NAME, STRING)\n\
\n\
Associate a cryptic message with a variable name.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      string name = args(0).string_value ();

      if (! error_state)
	{
	  string help = args(1).string_value ();

	  if (! error_state)
	    {
	      if (is_builtin_variable (name)
		  || is_text_function_name (name)
		  || is_mapper_function_name (name)
		  || is_builtin_function_name (name))
		error ("document: can't redefine help for built-in variables and functions");
	      else
		{
		  symbol_record *sym_rec = curr_sym_tab->lookup (name, 0);

		  if (sym_rec)
		    sym_rec->document (help);
		  else
		    error ("document: no such symbol `%s'", name.c_str ());
		}
	    }
	}
    }
  else
    print_usage ("document");

  return retval;
}

// XXX FIXME XXX -- this should take a list of regular expressions
// naming the variables to look for.

static octave_value_list
do_who (int argc, const string_vector& argv)
{
  octave_value_list retval;

  int show_builtins = 0;
  int show_functions = (curr_sym_tab == top_level_sym_tab);
  int show_variables = 1;
  int show_verbose = 0;

  string my_name = argv[0];

  if (argc > 1)
    {
      show_functions = 0;
      show_variables = 0;
    }

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-all" || argv[i] == "-a")
	{
	  show_builtins++;
	  show_functions++;
	  show_variables++;
	}
      else if (argv[i] == "-builtins" || argv[i] == "-b")
	show_builtins++;
      else if (argv[i] == "-functions" || argv[i] == "-f")
	show_functions++;
      else if (argv[i] == "-long" || argv[i] == "-l")
	show_verbose++;
      else if (argv[i] == "-variables" || argv[i] == "-v")
	show_variables++;
      else if (argv[i][0] == '-')
	warning ("%s: unrecognized option `%s'", my_name.c_str (),
		 argv[i].c_str ());
      else
	break;
    }

  int npats = argc - i;
  string_vector pats (npats);
  for (int j = 0; j < npats; j++)
    pats[j] = argv[i+j];

  // If the user specified -l and nothing else, show variables.  If
  // evaluating this at the top level, also show functions.

  if (show_verbose && ! (show_builtins || show_functions || show_variables))
    {
      show_functions = (curr_sym_tab == top_level_sym_tab);
      show_variables = 1;
    }

  int pad_after = 0;

  if (show_builtins)
    {
      pad_after += maybe_list ("*** built-in variables:", pats, npats,
			       octave_stdout, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_VARIABLE,
			       SYMTAB_ALL_SCOPES);

      pad_after += maybe_list ("*** built-in functions:", pats, npats,
			       octave_stdout, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_functions)
    {
      pad_after += maybe_list ("*** currently compiled functions:",
			       pats, npats, octave_stdout, show_verbose,
			       global_sym_tab, symbol_def::USER_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_variables)
    {
      pad_after += maybe_list ("*** local user variables:", pats, npats,
			       octave_stdout, show_verbose, curr_sym_tab,
			       symbol_def::USER_VARIABLE,
			       SYMTAB_LOCAL_SCOPE);

      pad_after += maybe_list ("*** globally visible user variables:",
			       pats, npats, octave_stdout, show_verbose,
			       curr_sym_tab, symbol_def::USER_VARIABLE,
			       SYMTAB_GLOBAL_SCOPE);
    }

  if (pad_after)
    octave_stdout << "\n";

  return retval;
}

DEFUN_TEXT (who, args, ,
  "who [-all] [-builtins] [-functions] [-long] [-variables]\n\
\n\
List currently defined symbol(s).  Options may be shortened to one\n\
character, but may not be combined.")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("who");

  if (error_state)
    return retval;

  retval = do_who (argc, argv);

  return retval;
}

DEFUN_TEXT (whos, args, ,
  "whos [-all] [-builtins] [-functions] [-long] [-variables]\n\
\n\
List currently defined symbol(s).  Options may be shortened to one\n\
character, but may not be combined.")
{
  octave_value_list retval;

  int nargin = args.length ();

  octave_value_list tmp_args;
  for (int i = nargin; i > 0; i--)
    tmp_args(i) = args(i-1);
  tmp_args(0) = "-long";

  int argc = tmp_args.length () + 1;

  string_vector argv = tmp_args.make_argv ("whos");

  if (error_state)
    return retval;

  retval = do_who (argc, argv);

  return retval;
}

// Install variables and functions in the symbol tables.

void
install_builtin_mapper (const builtin_mapper_function& mf)
{
  symbol_record *sym_rec = global_sym_tab->lookup (mf.name, 1);
  sym_rec->unprotect ();

  tree_builtin *def = new tree_builtin (mf, mf.name);

  sym_rec->define (def);

  sym_rec->document (mf.help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_function (const builtin_function& f)
{
  symbol_record *sym_rec = global_sym_tab->lookup (f.name, 1);
  sym_rec->unprotect ();

  tree_builtin *def = new tree_builtin (f.fcn, f.name);

  sym_rec->define (def, f.is_text_fcn);

  sym_rec->document (f.help_string);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_variable (const builtin_variable& v)
{
  if (v.install_as_function)
    install_builtin_variable_as_function (v.name, v.value, v.protect,
					  v.eternal, v.help_string);
  else
    bind_builtin_variable (v.name, v.value, v.protect, v.eternal,
			   v.sv_function, v.help_string);
}

void
install_builtin_variable_as_function (const string& name,
				      const octave_value& val,
				      int protect, int eternal,
				      const string& help)
{
  symbol_record *sym_rec = global_sym_tab->lookup (name, 1);
  sym_rec->unprotect ();

  string tmp_help = help.empty () ? sym_rec->help () : help;

  sym_rec->define_as_fcn (val);

  sym_rec->document (tmp_help);

  if (protect)
    sym_rec->protect ();

  if (eternal)
    sym_rec->make_eternal ();
}

void
alias_builtin (const string& alias, const string& name)
{
  symbol_record *sr_name = global_sym_tab->lookup (name, 0, 0);

  if (! sr_name)
    panic ("can't alias to undefined name!");

  symbol_record *sr_alias = global_sym_tab->lookup (alias, 1, 0);

  if (sr_alias)
    sr_alias->alias (sr_name);
  else
    panic ("can't find symbol record for builtin function `%s'",
	   alias.c_str ());
}

// Defining variables.

void
bind_ans (const octave_value& val, int print)
{
  static symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

  tree_identifier *ans_id = new tree_identifier (sr);
  tree_constant *tmp = new tree_constant (val);

  // XXX FIXME XXX -- making ans_id static, passing its address to
  // tree_simple_assignment_expression along with a flag to not delete
  // it seems to create a memory leak.  Hmm.

  tree_simple_assignment_expression tmp_ass (ans_id, tmp, false, true);

  tmp_ass.eval (print);
}

void
bind_global_error_variable (void)
{
  *error_message_buffer << ends;

  char *error_text = error_message_buffer->str ();

  bind_builtin_variable ("__error_text__", error_text, 1);

  delete [] error_text;

  delete error_message_buffer;

  error_message_buffer = 0;
}

void
clear_global_error_variable (void *)
{
  delete error_message_buffer;
  error_message_buffer = 0;

  bind_builtin_variable ("__error_text__", "", 1);
}

// Give a global variable a definition.  This will insert the symbol
// in the global table if necessary.

// How is this different than install_builtin_variable?  Are both
// functions needed?

void
bind_builtin_variable (const string& varname, const octave_value& val,
		       int protect, int eternal, sv_Function sv_fcn,
		       const string& help)
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

  sr->document (help);
}

// XXX FIXME XXX -- some of these should do their own checking to be
// able to provide more meaningful warning or error messages.

static int
echo_executing_commands (void)
{
  Vecho_executing_commands = check_preference ("echo_executing_commands"); 

  return 0;
}

static int
history_size (void)
{
  double val;
  if (builtin_real_scalar_variable ("history_size", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && ival == val)
	{
	  Vhistory_size = ival;
	  octave_command_history.set_size (ival);
	  return 0;
	}
    }
  gripe_invalid_value_specified ("history_size");
  return -1;
}

static int
history_file (void)
{
  int status = 0;

  string s = builtin_string_variable ("history_file");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("history_file");
      status = -1;
    }
  else
    {
      Vhistory_file = s;
      octave_command_history.set_file (oct_tilde_expand (s));
    }

  return status;
}

static int
ignore_function_time_stamp (void)
{
  int pref = 0;

  string val = builtin_string_variable ("ignore_function_time_stamp");

  if (! val.empty ())
    {
      if (val.compare ("all", 0, 3) == 0)
	pref = 2;
      if (val.compare ("system", 0, 6) == 0)
	pref = 1;
    }

  Vignore_function_time_stamp = pref;

  return 0;
}

static int
saving_history (void)
{
  Vsaving_history = check_preference ("saving_history");

  octave_command_history.ignore_entries (! Vsaving_history);

  return 0;
}

// XXX FIXME XXX -- there still may be better places for some of these
// to be defined.

static void
symbols_of_variables (void)
{
  DEFVAR (ans, , 0, 0,
    "");

  DEFCONST (argv, , 0, 0,
    "the command line arguments this program was invoked with");

  DEFVAR (echo_executing_commands, static_cast<double> (ECHO_OFF), 0,
	  echo_executing_commands,
    "echo commands as they are executed");

  DEFCONST (error_text, "", 0, 0,
    "the text of error messages that would have been printed in the\n\
body of the most recent unwind_protect statement or the TRY part of\n\
the most recent eval() command.  Outside of unwind_protect and\n\
eval(), or if no error has ocurred within them, the value of\n\
__error_text__ is guaranteed to be the empty string.");

  DEFVAR (history_file, default_history_file (), 0, history_file,
    "name of command history file");

  double tmp_hist_size = default_history_size ();

  DEFVAR (history_size, tmp_hist_size, 0, history_size,
    "number of commands to save in the history list");

  DEFVAR (ignore_function_time_stamp, "system", 0, ignore_function_time_stamp,
    "don't check to see if function files have changed since they were\n\
  last compiled.  Possible values are \"system\" and \"all\"");

  DEFCONST (program_invocation_name, Vprogram_invocation_name, 0, 0,
    "the full name of the current program or script, including the\n\
directory specification");

  DEFCONST (program_name, Vprogram_name, 0, 0,
    "the name of the current program or script");

  DEFVAR (saving_history, 1.0, 0, saving_history,
    "save command history");
}

void
install_builtin_variables (void)
{
  symbols_of_data ();
  symbols_of_defaults ();
  symbols_of_dirfns ();
  symbols_of_error ();
  symbols_of_file_io ();
  symbols_of_help ();
  symbols_of_input ();
  symbols_of_lex ();
  symbols_of_load_save ();
  symbols_of_pager ();
  symbols_of_parse ();
  symbols_of_pr_output ();
  symbols_of_pt_fcn ();
  symbols_of_pt_mat ();
  symbols_of_pt_plot ();
  symbols_of_syscalls ();
  symbols_of_toplev ();
  symbols_of_value ();
  symbols_of_variables ();
}

// Deleting names from the symbol tables.

DEFUN_TEXT (clear, args, ,
  "clear [-x] [name ...]\n\
\n\
Clear symbol(s) matching a list of globbing patterns.\n\
\n\
If no arguments are given, clear all user-defined variables and\n\
functions.\n\
\n\
With -x, exclude the named variables")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("clear");

  if (error_state)
    return retval;

  // Always clear the local table, but don't clear currently compiled
  // functions unless we are at the top level.  (Allowing that to
  // happen inside functions would result in pretty odd behavior...)

  int clear_user_functions = (curr_sym_tab == top_level_sym_tab);

  if (argc == 1)
    {
      curr_sym_tab->clear ();
      global_sym_tab->clear (clear_user_functions);
    }
  else
    {
      int exclusive = 0;

      int idx = 1;

      if (argc > 1)
	{
	  if (argv[idx] == "-x")
	    exclusive = 1;
	}

      int lcount = 0;
      int gcount = 0;
      int fcount = 0;

      string_vector lvars;
      string_vector gvars;
      string_vector fcns;

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

      // XXX FIXME XXX -- this needs to be optimized to avoid the
      // pattern matching code if the string doesn't contain any
      // globbing patterns.

      for (int k = idx; k < argc; k++)
	{
	  string patstr = argv[k];

	  if (! patstr.empty ())
	    {
	      glob_match pattern (patstr);

	      int i;
	      for (i = 0; i < lcount; i++)
		{
		  string nm = lvars[i];
		  int match = pattern.match (nm);
		  if ((exclusive && ! match) || (! exclusive && match))
		    curr_sym_tab->clear (nm);
		}

	      int count;
	      for (i = 0; i < gcount; i++)
		{
		  string nm = gvars[i];
		  int match = pattern.match (nm);
		  if ((exclusive && ! match) || (! exclusive && match))
		    {
		      count = curr_sym_tab->clear (nm);
		      if (count > 0)
			global_sym_tab->clear (nm, clear_user_functions);
		    }
		}

	      for (i = 0; i < fcount; i++)
		{
		  string nm = fcns[i];
		  int match = pattern.match (nm);
		  if ((exclusive && ! match) || (! exclusive && match))
		    {
		      count = curr_sym_tab->clear (nm);
		      global_sym_tab->clear (nm, clear_user_functions);
		    }
		}
	    }
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
