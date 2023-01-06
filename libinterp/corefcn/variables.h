////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_variables_h)
#define octave_variables_h 1

#include "octave-config.h"

class octave_function;
class octave_user_function;

class octave_value;
class octave_value_list;
class octave_builtin;
class string_vector;

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_identifier;

OCTAVE_END_NAMESPACE(octave)

#include <limits>
#include <string>

#include "lo-ieee.h"

#include "ov-builtin.h"

// The following two functions should be removed or the return type
// should be changed when we remove octave_function from the interpreter
// interface.  See the discussion about this topic here:
// https://octave.discourse.group/t/refactoring-octave-value-function-objects-backward-incompatible-change/971

extern OCTINTERP_API octave_function *
is_valid_function (const octave_value&, const std::string& = "",
                   bool warn = false);

extern OCTINTERP_API octave_function *
is_valid_function (const std::string&, const std::string& = "",
                   bool warn = false);

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API int
symbol_exist (const std::string& name, const std::string& type = "any");

extern OCTINTERP_API std::string
unique_symbol_name (const std::string& basename);

extern OCTINTERP_API octave_value
set_internal_variable (bool& var, const octave_value_list& args,
                       int nargout, const char *nm);

extern OCTINTERP_API octave_value
set_internal_variable (char& var, const octave_value_list& args,
                       int nargout, const char *nm);

extern OCTINTERP_API octave_value
set_internal_variable (int& var, const octave_value_list& args,
                       int nargout, const char *nm,
                       int minval = std::numeric_limits<int>::min (),
                       int maxval = std::numeric_limits<int>::max ());

extern OCTINTERP_API octave_value
set_internal_variable (double& var, const octave_value_list& args,
                       int nargout, const char *nm,
                       double minval = -octave::numeric_limits<double>::Inf (),
                       double maxval = octave::numeric_limits<double>::Inf ());

extern OCTINTERP_API octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, bool empty_ok = true);

extern OCTINTERP_API octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, const char **choices);

extern OCTINTERP_API octave_value
set_internal_variable (int& var, const octave_value_list& args,
                       int nargout, const char *nm, const char **choices);

extern OCTINTERP_API std::string
maybe_missing_function_hook (const std::string& name);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::symbol_exist' instead")
inline int
symbol_exist (const std::string& name, const std::string& type = "any")
{
  return octave::symbol_exist (name, type);
}

OCTAVE_DEPRECATED (7, "use 'octave::unique_symbol_name' instead")
inline std::string
unique_symbol_name (const std::string& basename)
{
  return octave::unique_symbol_name (basename);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (bool& var, const octave_value_list& args, int nargout,
                       const char *nm)
{
  return octave::set_internal_variable (var, args, nargout, nm);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (char& var, const octave_value_list& args, int nargout,
                       const char *nm)
{
  return octave::set_internal_variable (var, args, nargout, nm);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (int& var, const octave_value_list& args, int nargout,
                       const char *nm,
                       int minval = std::numeric_limits<int>::min (),
                       int maxval = std::numeric_limits<int>::max ())
{
  return octave::set_internal_variable (var, args, nargout, nm, minval, maxval);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (double& var, const octave_value_list& args, int nargout,
                       const char *nm,
                       double minval = -octave::numeric_limits<double>::Inf (),
                       double maxval = octave::numeric_limits<double>::Inf ())
{
  return octave::set_internal_variable (var, args, nargout, nm, minval, maxval);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, bool empty_ok = true)
{
  return octave::set_internal_variable (var, args, nargout, nm, empty_ok);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, const char **choices)
{
  return octave::set_internal_variable (var, args, nargout, nm, choices);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_internal_variable' instead")
inline octave_value
set_internal_variable (int& var, const octave_value_list& args, int nargout,
                       const char *nm, const char **choices)
{
  return octave::set_internal_variable (var, args, nargout, nm, choices);
}

OCTAVE_DEPRECATED (7, "use 'octave::maybe_missing_function_hook' instead")
inline std::string
maybe_missing_function_hook (const std::string& name)
{
  return octave::maybe_missing_function_hook (name);
}

#endif

// The following macros should also be considered obsolete.

#define SET_INTERNAL_VARIABLE(NM)                       \
  set_internal_variable (V ## NM, args, nargout, #NM)

#define SET_NONEMPTY_INTERNAL_STRING_VARIABLE(NM)               \
  set_internal_variable (V ## NM, args, nargout, #NM, false)

#define SET_INTERNAL_VARIABLE_WITH_LIMITS(NM, MINVAL, MAXVAL)           \
  set_internal_variable (V ## NM, args, nargout, #NM, MINVAL, MAXVAL)

// in the following, CHOICES must be a C string array terminated by null.
#define SET_INTERNAL_VARIABLE_CHOICES(NM, CHOICES)              \
  set_internal_variable (V ## NM, args, nargout, #NM, CHOICES)

#endif
