/*

Copyright (C) 2013 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun.h"
#include "error.h"
#include "defun.h"
#include "hook-fcn.h"
#include "oct-obj.h"

static hook_function_list edit_hook_functions;

DEFUN (add_edit_hook, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{id} =} add_edit_hook (@var{fcn})\n\
@deftypefnx {Built-in Function} {@var{id} =} add_edit_hook (@var{fcn}, @var{data})\n\
Add the named function or function handle @var{fcn} to the list of functions to call\n\
to handle editing files.  The function should have the form\n\
\n\
@example\n\
@var{fcn} (@var{file}, @var{data})\n\
@end example\n\
\n\
If @var{data} is omitted, Octave calls the function without one argument.\n\
\n\
The returned identifier may be used to remove the function handle from\n\
the list of input hook functions.\n\
@seealso{remove_edit_hook}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_value user_data;

      if (nargin == 2)
        user_data = args(1);

      hook_function hook_fcn (args(0), user_data);

      if (! error_state)
        {
          edit_hook_functions.insert (hook_fcn.id (), hook_fcn);

          retval = hook_fcn.id ();
        }
      else
        error ("add_edit_hook: expecting string as first arg");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (remove_edit_hook, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} remove_edit_hook (@var{name})\n\
@deftypefnx {Built-in Function} {} remove_input_event_hook (@var{fcn_id})\n\
Remove the named function or function handle with the given identifier\n\
from the list of functions to call to handle editing files.\n\
@seealso{add_edit_hook}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string hook_fcn_id = args(0).string_value ();

      bool warn = (nargin < 2);

      if (! error_state)
        {
          hook_function_list::iterator p
            = edit_hook_functions.find (hook_fcn_id);

          if (p != edit_hook_functions.end ())
            edit_hook_functions.erase (p);
          else if (warn)
            warning ("remove_edit_hook: %s not found in list",
                     hook_fcn_id.c_str ());
        }
      else
        error ("remove_edit_hook: argument not valid as a hook function name or id");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__execute_edit_hook__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{status} =} __execute_edit_hook__ (@var{file})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (edit_hook_functions.empty ())
    retval = false;
  else
    {
      edit_hook_functions.run (args);

      retval = true;
    }

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
