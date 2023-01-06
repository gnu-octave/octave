////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <sstream>
#include <string>

#include "interpreter.h"
#include "ov-fcn.h"
#include "pt.h"
#include "pt-eval.h"
#include "pt-pr-code.h"
#include "unwind-prot.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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

// Is the current breakpoint condition met?

bool
tree::meets_bp_condition (tree_evaluator& tw) const
{
  bool retval;
  if (m_bp_cond == nullptr)
    retval = false;
  else if (m_bp_cond->empty ())     // empty condition always met
    retval = true;
  else
    {
      int parse_status = 0;

      unwind_protect frame;

      octave::interpreter_try (frame);

      retval = true;                // default to stopping if any error
      try
        {
          octave_value_list val
            = tw.eval_string (*m_bp_cond, 1, parse_status, 1);

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
      catch (const execution_exception& ee)
        {
          interpreter& interp = tw.get_interpreter ();

          interp.recover_from_exception ();

          std::string tmp = ee.message ();

          warning ("Error evaluating breakpoint condition:\n    %s",
                   tmp.c_str ());
        }
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
