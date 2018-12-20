/*

Copyright (C) 2013-2018 John W. Eaton

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

#include "hook-fcn.h"
#include "parse.h"

hook_function::hook_function (const octave_value& f, const octave_value& d)
{
  if (f.is_string ())
    {
      std::string name = f.string_value ();

      rep = new named_hook_function (name, d);
    }
  else if (f.is_function_handle ())
    {
      rep = new fcn_handle_hook_function (f, d);
    }
  else
    error ("invalid hook function");
}

void named_hook_function::eval (const octave_value_list& initial_args)
{
  octave_value_list args = initial_args;

  if (data.is_defined ())
    args.append (data);

  octave::feval (name, args, 0);
}

void fcn_handle_hook_function::eval (const octave_value_list& initial_args)
{
  octave_value_list args = initial_args;

  if (data.is_defined ())
    args.append (data);

  octave::feval (fcn_handle, args, 0);
}

