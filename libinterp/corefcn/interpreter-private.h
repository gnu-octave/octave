////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_interpreter_private_h)
#define octave_interpreter_private_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "symtab.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class bp_table;
class cdef_manager;
class child_list;
class display_info;
class dynamic_loader;
class error_system;
class event_manager;
class gh_manager;
class gtk_manager;
class help_system;
class input_system;
class interpreter;
class load_path;
class load_save_system;
class output_system;
class tree_evaluator;
class type_info;

extern OCTINTERP_API interpreter& __get_interpreter__ (void);

extern OCTINTERP_API dynamic_loader& __get_dynamic_loader__ (void);

extern OCTINTERP_API error_system& __get_error_system__ (void);

extern OCTINTERP_API gh_manager& __get_gh_manager__ (void);

extern OCTINTERP_API help_system& __get_help_system__ (void);

extern OCTINTERP_API input_system& __get_input_system__ (void);

extern OCTINTERP_API load_path& __get_load_path__ (void);

extern OCTINTERP_API load_save_system& __get_load_save_system__ (void);

extern OCTINTERP_API event_manager& __get_event_manager__ (void);

extern OCTINTERP_API output_system& __get_output_system__ (void);

extern OCTINTERP_API type_info& __get_type_info__ (void);

extern OCTINTERP_API symbol_table& __get_symbol_table__ (void);

extern OCTINTERP_API symbol_scope __get_current_scope__ (void);

extern OCTINTERP_API symbol_scope __require_current_scope__ (void);

extern OCTINTERP_API tree_evaluator& __get_evaluator__ (void);

extern OCTINTERP_API bp_table& __get_bp_table__ (void);

extern OCTINTERP_API child_list& __get_child_list__ (void);

extern OCTINTERP_API cdef_manager& __get_cdef_manager__ (void);

extern OCTINTERP_API display_info& __get_display_info__ (void);

extern OCTINTERP_API gtk_manager& __get_gtk_manager__ (void);

// Functions that could be methods in the interpreter class but maybe
// shouldn't be exposed as part of the public interface.

// Convert octave_value object ARG to be a function handle object.  It
// may be a function handle, inline function, the name of a function,
// or the text of an inline function that has the given argument names
// PARAMETER_NAMES.  Use of the latter form is discouraged.

OCTINTERP_API
octave_value
get_function_handle (interpreter& interp, const octave_value& arg,
                     const std::string& parameter_name);

OCTINTERP_API
octave_value
get_function_handle (interpreter& interp, const octave_value& arg,
                     const std::list<std::string>& parameter_names
                     = std::list<std::string> ());

OCTAVE_END_NAMESPACE(octave)

#endif
