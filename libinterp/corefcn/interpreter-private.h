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

#if ! defined (octave_interpreter_private_h)
#define octave_interpreter_private_h 1

#include "octave-config.h"

#include <string>

#include "symtab.h"

namespace octave
{
  class call_stack;
  class interpreter;
  class load_path;
  class tree_evaluator;

  extern interpreter& __get_interpreter__ (const std::string& who);

  extern load_path& __get_load_path__ (const std::string& who);

  extern symbol_table& __get_symbol_table__ (const std::string& who);

  extern symbol_table::scope *__get_current_scope__ (const std::string& who);

  extern symbol_table::scope *
  __require_current_scope__ (const std::string& who);

  extern tree_evaluator& __get_evaluator__ (const std::string& who);

  extern call_stack& __get_call_stack__ (const std::string& who);
}

#endif
