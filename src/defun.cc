/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sstream>
#include <iostream>
#include <string>

#include "defun.h"
#include "dynamic-ld.h"
#include "error.h"
#include "help.h"
#include "ov.h"
#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "ov-fcn.h"
#include "ov-mapper.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "oct-obj.h"
#include "pager.h"
#include "symtab.h"
#include "toplev.h"
#include "variables.h"

// Print the usage part of the doc string of FCN (user-defined or DEFUN).

static void
print_usage (octave_function *fcn)
{
  if (fcn)
    {
      std::string nm = fcn->name ();

      std::string doc = fcn->doc_string ();

      if (doc.length () > 0)
	{
	  std::ostringstream buf;

	  buf << "\nInvalid call to " << nm << ".  Correct usage is:\n\n";

	  display_usage_text (buf, doc);

	  buf << "\n";

	  additional_help_message (buf);

	  defun_usage_message (buf.str ());
	}
      else
	error ("no usage message found for `%s'", nm.c_str ());
    }
  else
    error ("print_usage: invalid function");
}

// Print the usage part of the doc string of the current function
// (user-defined or DEFUN).

void
print_usage (void)
{
  print_usage (octave_call_stack::current ());
}

// Deprecated.
void
print_usage (const std::string&)
{
  print_usage ();
}

DEFUN (print_usage, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} print_usage ()\n\
Print the usage message for the currently executing function.  The\n\
@code{print_usage} function is only intended to work inside a\n\
user-defined function.\n\
@seealso{help}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    print_usage (octave_call_stack::caller_user_function ());
  else
    print_usage ();

  return retval;
}

void
check_version (const std::string& version, const std::string& fcn)
{
  if (version != OCTAVE_API_VERSION)
    {
      error ("API version %s found in .oct file function `%s'\n"
	     "       does not match the running Octave (API version %s)\n"
	     "       this can lead to incorrect results or other failures\n"
	     "       you can fix this problem by recompiling this .oct file",
	     version.c_str (), fcn.c_str (), OCTAVE_API_VERSION);
    }
}

// Install variables and functions in the symbol tables.

void
install_builtin_mapper (octave_mapper *mf)
{
  symbol_record *sym_rec = fbi_sym_tab->lookup (mf->name (), true);

  unsigned int t
    = symbol_record::BUILTIN_FUNCTION | symbol_record::MAPPER_FUNCTION;

  sym_rec->unprotect ();
  sym_rec->define (mf, t);
  sym_rec->document (mf->doc_string ());
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_function (octave_builtin::fcn f, const std::string& name,
			  const std::string& doc, bool is_text_fcn,
			  bool /* can_hide_function -- not yet implemented */)
{
  symbol_record *sym_rec = fbi_sym_tab->lookup (name, true);

  unsigned int t = symbol_record::BUILTIN_FUNCTION;

  if (is_text_fcn)
    t |= symbol_record::COMMAND;

  sym_rec->unprotect ();
  sym_rec->define (new octave_builtin (f, name, doc), t);
  sym_rec->document (doc);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
		      const octave_shlib& shl,
		      const std::string& doc, bool is_text_fcn,
		      bool relative)
{
  symbol_record *sym_rec = fbi_sym_tab->lookup (name, true);

  unsigned int t = symbol_record::DLD_FUNCTION;

  if (is_text_fcn)
    t |= symbol_record::COMMAND;

  sym_rec->unprotect ();

  octave_dld_function *df = new octave_dld_function (f, shl, name, doc);

  if (relative)
    df->mark_relative ();

  sym_rec->define (df, t);
  sym_rec->document (doc);

  // Also insert the full name in the symbol table.  This way, we can
  // properly cope with changes to LOAD_PATH.

  symbol_record *full_sr = fbi_sym_tab->lookup (shl.file_name (), true);

  full_sr->alias (sym_rec, true);
  full_sr->hide ();
}

void
install_mex_function (void *fptr, bool fmex, const std::string& name,
		      const octave_shlib& shl, bool is_text_fcn,
		      bool relative)
{
  symbol_record *sym_rec = fbi_sym_tab->lookup (name, true);

  unsigned int t = symbol_record::MEX_FUNCTION;

  if (is_text_fcn)
    t |= symbol_record::COMMAND;

  sym_rec->unprotect ();

  octave_mex_function *mf = new octave_mex_function (fptr, fmex, shl, name);

  if (relative)
    mf->mark_relative ();

  sym_rec->define (mf, t);

  // Also insert the full name in the symbol table.  This way, we can
  // properly cope with changes to LOAD_PATH.

  symbol_record *full_sr = fbi_sym_tab->lookup (shl.file_name (), true);

  full_sr->alias (sym_rec, true);
  full_sr->hide ();
}

void
alias_builtin (const std::string& alias, const std::string& name)
{
  symbol_record *sr_name = fbi_sym_tab->lookup (name);

  if (! sr_name)
    panic ("can't alias to undefined name!");

  symbol_record *sr_alias = fbi_sym_tab->lookup (alias, true);

  if (sr_alias)
    sr_alias->alias (sr_name);
  else
    panic ("can't find symbol record for builtin function `%s'",
	   alias.c_str ());
}

#if 0
// This is insufficient to really make it possible to define an alias
// for function.  There are a number of subtle problems related to
// automatically reloading functions.
DEFUN (alias, args, ,
  "alias (alias, name)")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      string alias = args(0).string_value ();
      string name = args(1).string_value ();

      if (! error_state)
	{
	  symbol_record *sr_name = lookup_by_name (name, false);

	  if (sr_name && sr_name->is_function ())
	    {
	      symbol_record *sr_alias = fbi_sym_tab->lookup (alias, true);

	      if (sr_alias)
		sr_alias->alias (sr_name);
	      else
		error ("alias: unable to insert `%s' in symbol table",
		       alias.c_str ());
	    }
	  else
	    error ("alias: function `%s' does not exist", name.c_str ());
	}
    }
  else
    print_usage ();

  return retval;
}
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
