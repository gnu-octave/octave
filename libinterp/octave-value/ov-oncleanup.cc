/*

Copyright (C) 2010-2015 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "ov-oncleanup.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "pt-misc.h"
#include "toplev.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_oncleanup, "onCleanup",
                                     "onCleanup");

octave_oncleanup::octave_oncleanup (const octave_value& f)
  : fcn (f)
{
  if (f.is_function_handle ())
    {
      octave_function *fptr = f.function_value (true);
      if (! fptr)
        error ("onCleanup: no default dispatch for function handle");

      octave_user_function *uptr
        = dynamic_cast<octave_user_function *> (fptr);

      if (uptr != 0)
        {
          tree_parameter_list *pl = uptr->parameter_list ();

          if (pl != 0 && pl->length () > 0)
            warning ("onCleanup: cleanup action takes parameters");
        }
    }
  else
    {
      fcn = octave_value ();
      error ("onCleanup: argument must be a function handle");
    }
}

octave_oncleanup::~octave_oncleanup (void)
{
  if (fcn.is_undefined ())
    return;

  octave::unwind_protect frame;

  // Clear interrupts.
  frame.protect_var (octave_interrupt_state);
  octave_interrupt_state = 0;

  // Disallow quit().
  frame.protect_var (quit_allowed);
  quit_allowed = false;

  try
    {
      // Run the actual code.
      fcn.do_multi_index_op (0, octave_value_list ());
    }
  catch (const octave_interrupt_exception&)
    {
      recover_from_exception ();

      warning ("onCleanup: interrupt occurred in cleanup action");
    }
  catch (const octave_execution_exception&)
    {
      throw;
    }
  catch (...) // Yes, the black hole.  We're in a d-tor.
    {
      // This shouldn't happen, in theory.
      error ("onCleanup: internal error: unhandled exception in cleanup action");
    }
}

octave_scalar_map
octave_oncleanup::scalar_map_value (void) const
{
  octave_scalar_map retval;
  retval.setfield ("task", fcn);
  return retval;
}

bool
octave_oncleanup::save_ascii (std::ostream& /* os */)
{
  warning ("save: unable to save onCleanup variables, skipping");

  return true;
}

bool
octave_oncleanup::load_ascii (std::istream& /* is */)
{
  // Silently skip object that was not saved
  return true;
}

bool
octave_oncleanup::save_binary (std::ostream& /* os */,
                               bool& /* save_as_floats */)
{
  warning ("save: unable to save onCleanup variables, skipping");

  return true;
}

bool
octave_oncleanup::load_binary (std::istream& /* is */, bool /* swap */,
                               octave::mach_info::float_format /* fmt */)
{
  // Silently skip object that was not saved
  return true;
}

bool
octave_oncleanup::save_hdf5 (octave_hdf5_id /* loc_id */,
                             const char * /* name */,
                             bool /* save_as_floats */)
{
  warning ("save: unable to save onCleanup variables, skipping");

  return true;
}

bool
octave_oncleanup::load_hdf5 (octave_hdf5_id /* loc_id */,
                             const char * /* name */)
{
  // Silently skip object that was not saved
  return true;
}

void
octave_oncleanup::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_oncleanup::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  os << "onCleanup (";
  if (fcn.is_defined ())
    fcn.print_raw (os, pr_as_read_syntax);
  os << ")";
}

DEFUN (onCleanup, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{obj} =} onCleanup (@var{function})
Create a special object that executes a given function upon destruction.

If the object is copied to multiple variables (or cell or struct array
elements) or returned from a function, @var{function} will be executed after
clearing the last copy of the object.  Note that if multiple local onCleanup
variables are created, the order in which they are called is unspecified.
For similar functionality @xref{The unwind_protect Statement}.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (new octave_oncleanup (args(0)));
}

/*
%!test
%! old_wstate = warning ("query");
%! unwind_protect
%!   trigger = onCleanup (@() warning ("on", "__MY_WARNING__"));
%!   warning ("off", "__MY_WARNING__");
%!   assert ((warning ("query", "__MY_WARNING__")).state, "off");
%!   clear trigger;
%!   assert ((warning ("query", "__MY_WARNING__")).state, "on");
%! unwind_protect_cleanup
%!   warning (old_wstate);
%! end_unwind_protect
*/
