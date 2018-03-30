/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sstream>
#include <iostream>
#include <string>

#include "call-stack.h"
#include "defun.h"
#include "dynamic-ld.h"
#include "error.h"
#include "help.h"
#include "ov.h"
#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "ov-fcn.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "pager.h"
#include "pt-eval.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "symtab.h"
#include "variables.h"
#include "parse.h"

// Print the usage part of the doc string of FCN (user-defined or DEFUN).
void
print_usage (void)
{
  octave::call_stack& cs = octave::__get_call_stack__ ("print_usage");

  const octave_function *cur = cs.current ();

  if (cur)
    print_usage (cur->name ());
  else
    error ("print_usage: invalid function");
}

void
print_usage (const std::string& name)
{
  octave::feval ("print_usage", octave_value (name), 0);
}

void
check_version (const std::string& version, const std::string& fcn)
{
  if (version != OCTAVE_API_VERSION)
    {
      error ("API version %s found in .oct file function '%s'\n"
             "       does not match the running Octave (API version %s)\n"
             "       this can lead to incorrect results or other failures\n"
             "       you can fix this problem by recompiling this .oct file",
             version.c_str (), fcn.c_str (), OCTAVE_API_VERSION);
    }
}

// Install variables and functions in the symbol tables.

void
install_builtin_function (octave_builtin::fcn f, const std::string& name,
                          const std::string& file, const std::string& doc,
                          bool /* can_hide_function -- not yet implemented */)
{
  octave_value fcn (new octave_builtin (f, name, file, doc));

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("install_builtin_function");

  symtab.install_built_in_function (name, fcn);
}

void
install_builtin_function (octave_builtin::meth m, const std::string& name,
                          const std::string& file, const std::string& doc,
                          bool /* can_hide_function -- not yet implemented */)
{
  octave_value fcn (new octave_builtin (m, name, file, doc));

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("install_builtin_function");

  symtab.install_built_in_function (name, fcn);
}

void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
                      const octave::dynamic_library& shl, const std::string& doc,
                      bool relative)
{
  octave_dld_function *fcn = new octave_dld_function (f, shl, name, doc);

  if (relative)
    fcn->mark_relative ();

  octave_value fval (fcn);

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("install_dld_function");

  symtab.install_built_in_function (name, fval);
}

void
install_dld_function (octave_dld_function::meth m, const std::string& name,
                      const octave::dynamic_library& shl, const std::string& doc,
                      bool relative)
{
  octave_dld_function *fcn = new octave_dld_function (m, shl, name, doc);

  if (relative)
    fcn->mark_relative ();

  octave_value fval (fcn);

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("install_dld_function");

  symtab.install_built_in_function (name, fval);
}

void
install_mex_function (void *fptr, bool fmex, const std::string& name,
                      const octave::dynamic_library& shl, bool relative)
{
  octave_mex_function *fcn = new octave_mex_function (fptr, fmex, shl, name);

  if (relative)
    fcn->mark_relative ();

  octave_value fval (fcn);

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("install_mex_function");

  symtab.install_built_in_function (name, fval);
}

void
alias_builtin (const std::string& alias, const std::string& name)
{
  octave::symbol_table& symtab = octave::__get_symbol_table__ ("alias_builtin");

  symtab.alias_built_in_function (alias, name);
}

void
install_builtin_dispatch (const std::string& name, const std::string& klass)
{
  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("install_builtin_dispatch");

  symtab.install_built_in_dispatch (name, klass);
}

octave::dynamic_library
get_current_shlib (void)
{
  octave::dynamic_library retval;

  octave::call_stack& cs = octave::__get_call_stack__ ("get_current_shlib");

  octave_function *curr_fcn = cs.current ();

  if (curr_fcn)
    {
      if (curr_fcn->is_dld_function ())
        {
          octave_dld_function *dld
            = dynamic_cast<octave_dld_function *> (curr_fcn);
          retval = dld->get_shlib ();
        }
      else if (curr_fcn->is_mex_function ())
        {
          octave_mex_function *mex
            = dynamic_cast<octave_mex_function *> (curr_fcn);
          retval = mex->get_shlib ();
        }
    }

  return retval;
}

bool
defun_isargout (int nargout, int iout)
{
  octave::tree_evaluator& tw = octave::__get_evaluator__ ("defun_isargout");

  return tw.isargout (nargout, iout);
}

void
defun_isargout (int nargout, int nout, bool *isargout)
{
  octave::tree_evaluator& tw = octave::__get_evaluator__ ("defun_isargout");

  return tw.isargout (nargout, nout, isargout);
}
