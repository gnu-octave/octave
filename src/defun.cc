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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "defun-int.h"
#include "error.h"
#include "help.h"
#include "ov.h"
#include "ov-builtin.h"
#include "ov-mapper.h"
#include "pager.h"
#include "symtab.h"
#include "variables.h"

void
print_usage (const string& nm, bool just_usage)
{
  symbol_record *sym_rec = global_sym_tab->lookup (nm);

  if (sym_rec)
    {
      string h = sym_rec->help ();

      if (h.length () > 0)
	{
	  octave_stdout << "\n*** " << nm << ":\n\n"
	    << h << "\n";

	  if (! just_usage)
	    additional_help_message (octave_stdout);
	}
    }
  else
    warning ("no usage message found for `%s'", nm.c_str ());
}

// Install variables and functions in the symbol tables.

void
install_builtin_mapper (void *mf_arg)
{
  octave_mapper *mf = static_cast<octave_mapper *> (mf_arg);

  symbol_record *sym_rec = global_sym_tab->lookup (mf->name (), true);

  unsigned int t
    = symbol_record::BUILTIN_FUNCTION | symbol_record::MAPPER_FUNCTION;

  sym_rec->unprotect ();
  sym_rec->define (mf, t);
  sym_rec->document (mf->doc_string ());
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

void
install_builtin_function (void *f_arg, const string& name,
			  const string& doc, bool is_text_fcn)
{
  octave_builtin::fcn f = static_cast<octave_builtin::fcn> (f_arg);

  symbol_record *sym_rec = global_sym_tab->lookup (name, true);

  unsigned int t = symbol_record::BUILTIN_FUNCTION;

  if (is_text_fcn)
    t |= symbol_record::TEXT_FUNCTION;

  sym_rec->unprotect ();
  sym_rec->define (new octave_builtin (f, name, doc), t);
  sym_rec->document (doc);
  sym_rec->make_eternal ();
  sym_rec->protect ();
}

static void
install_builtin_variable_as_function (const string& name,
				      const octave_value& val,
				      bool protect, bool eternal,
				      const string& help)
{
  symbol_record *sym_rec = global_sym_tab->lookup (name, true);
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
install_builtin_variable (const string& name, const octave_value& value,
			  bool install_as_function, bool protect,
			  bool eternal, void *chg_fcn_arg,
			  const string& help_string)
{
  symbol_record::change_function chg_fcn
    = static_cast<symbol_record::change_function> (chg_fcn_arg);

  if (install_as_function)
    install_builtin_variable_as_function (name, value, protect,
					  eternal, help_string);
  else
    bind_builtin_variable (name, value, protect, eternal,
			   chg_fcn, help_string);
}

void
alias_builtin (const string& alias, const string& name)
{
  symbol_record *sr_name = global_sym_tab->lookup (name);

  if (! sr_name)
    panic ("can't alias to undefined name!");

  symbol_record *sr_alias = global_sym_tab->lookup (alias, true);

  if (sr_alias)
    sr_alias->alias (sr_name);
  else
    panic ("can't find symbol record for builtin function `%s'",
	   alias.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
