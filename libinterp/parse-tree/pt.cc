/*

Copyright (C) 1996-2016 John W. Eaton

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

#include <iostream>
#include <sstream>
#include <string>

#include "ov-fcn.h"
#include "pt.h"
#include "pt-pr-code.h"
#include "unwind-prot.h"

// Hide the details of the string buffer so that we are less likely to
// create a memory leak.

std::string
tree::str_print_code (void)
{
  std::ostringstream buf;

  tree_print_code tpc (buf);

  accept (tpc);

  std::string retval = buf.str ();

  return retval;
}

// function from libinterp/parse-tree/oct-parse.cc, not listed in oct-parse.h
octave_value_list eval_string (const std::string&, bool, int&, int);
// Is the current breakpoint condition met?
bool
tree::meets_bp_condition () const
{
  bool retval;
  if (bp == 0)
    retval = false;
  else if (bp->empty ())     // empty condition always met
    retval = true;
  else
    {
      int parse_status = 0;

      octave::unwind_protect frame;
      frame.protect_var (buffer_error_messages);
      frame.protect_var (Vdebug_on_error);
      frame.protect_var (Vdebug_on_warning);

      buffer_error_messages++;
      Vdebug_on_error = false;
      Vdebug_on_warning = false;

      retval = true;                // default to stopping if any error
      try
        {
          octave_value_list val = eval_string (*bp, 1, parse_status, 1);
          if (parse_status == 0)
            {
              if (! val(0).is_scalar_type ())
                warning ("Breakpoint condition must be a scalar, not size %s",
                  val(0).dims ().str ('x').c_str ());
              else
                retval = val(0).bool_value ();
            }
          else
            warning ("Error parsing breakpoint condition");
        }
      catch (const octave_execution_exception& e)
        {
          warning ("Error evaluating breakpoint condition:\n    %s",
                   last_error_message ().c_str ());
        }
    }
  return retval;
}
