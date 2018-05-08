/*

Copyright (C) 2017-2018 John W. Eaton

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

#include <string>

#include "bp-table.h"
#include "call-stack.h"
#include "child-list.h"
#include "error.h"
#include "gtk-manager.h"
#include "help.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-classdef.h"
#include "symtab.h"

namespace octave
{
  interpreter& __get_interpreter__ (const std::string& who)
  {
    interpreter *interp = interpreter::the_interpreter ();

    if (! interp)
      {
        abort ();
        error ("%s: interpreter context missing", who.c_str ());
      }

    return *interp;
  }

  dynamic_loader& __get_dynamic_loader__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_dynamic_loader ();
  }

  help_system& __get_help_system__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_help_system ();
  }

  load_path& __get_load_path__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_load_path ();
  }

  type_info& __get_type_info__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_type_info ();
  }

  symbol_table& __get_symbol_table__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_symbol_table ();
  }

  symbol_scope __get_current_scope__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_current_scope ();
  }

  symbol_scope __require_current_scope__ (const std::string& who)
  {
    symbol_scope scope = __get_current_scope__ (who);

    if (! scope)
      error ("%s: symbol table scope missing", who.c_str ());

    return scope;
  }

  tree_evaluator& __get_evaluator__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_evaluator ();
  }

  bp_table& __get_bp_table__ (const std::string& who)
  {
    tree_evaluator& tw = __get_evaluator__ (who);

    return tw.get_bp_table ();
  }

  call_stack& __get_call_stack__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_call_stack ();
  }

  child_list& __get_child_list__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_child_list ();
  }

  cdef_manager& __get_cdef_manager__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_cdef_manager ();
  }

  gtk_manager& __get_gtk_manager__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_gtk_manager ();
  }
}
