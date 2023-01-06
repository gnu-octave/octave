////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "hook-fcn.h"
#include "parse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

hook_function::hook_function (const octave_value& f, const octave_value& d)
{
  if (f.is_string ())
    {
      std::string name = f.string_value ();

      m_rep = std::shared_ptr<base_hook_function> (new named_hook_function (name, d));
    }
  else if (f.is_function_handle ())
    {
      m_rep = std::shared_ptr<base_hook_function> (new fcn_handle_hook_function (f, d));
    }
  else
    error ("invalid hook function");
}

void named_hook_function::eval (const octave_value_list& initial_args)
{
  octave_value_list args = initial_args;

  if (m_data.is_defined ())
    args.append (m_data);

  feval (m_name, args, 0);
}

void fcn_handle_hook_function::eval (const octave_value_list& initial_args)
{
  octave_value_list args = initial_args;

  if (m_data.is_defined ())
    args.append (m_data);

  feval (m_fcn_handle, args, 0);
}

OCTAVE_END_NAMESPACE(octave)
