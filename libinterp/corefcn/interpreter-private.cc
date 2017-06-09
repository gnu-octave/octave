/*

Copyright (C) 2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "call-stack.h"
#include "error.h"
#include "load-path.h"
#include "interpreter-private.h"
#include "interpreter.h"
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

  load_path& __get_load_path__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_load_path ();
  }

  symbol_table& __get_symbol_table__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_symbol_table ();
  }

  symbol_table::scope_id __get_current_scope__ (const std::string& who)
  {
    symbol_table& symtab = __get_symbol_table__ (who);

    return symtab.current_scope ();
  }

  tree_evaluator& __get_evaluator__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_evaluator ();
  }

  call_stack& __get_call_stack__ (const std::string& who)
  {
    interpreter& interp = __get_interpreter__ (who);

    return interp.get_call_stack ();
  }
}
