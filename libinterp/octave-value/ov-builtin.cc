////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2021 The Octave Project Developers
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

#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ov-builtin.h"
#include "ov.h"
#include "ovl.h"
#include "profiler.h"
#include "unwind-prot.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_builtin,
                                     "built-in function",
                                     "built-in function");

octave_value_list
octave_builtin::execute (octave::tree_evaluator& tw, int nargout,
                         const octave_value_list& args)
{
  octave_value_list retval;

  if (args.has_magic_colon ())
    error ("invalid use of colon in function argument list");

  octave::profiler& profiler = tw.get_profiler ();

  octave::profiler::enter<octave_builtin> block (profiler, *this);

  if (m_fcn)
    retval = (*m_fcn) (args, nargout);
  else
    {
      octave::interpreter& interp
        = octave::__get_interpreter__ ("octave_builtin::call");

      retval = (*m_meth) (interp, args, nargout);
    }

  // Do not allow null values to be returned from functions.
  // FIXME: perhaps true builtins should be allowed?

  retval.make_storable_values ();

  // Fix the case of a single undefined value.
  // This happens when a compiled function uses
  //
  //   octave_value retval;
  //
  // instead of
  //
  //   octave_value_list retval;
  //
  // the idiom is very common, so we solve that here.

  if (retval.length () == 1 && retval.xelem (0).is_undefined ())
    retval.clear ();

  return retval;
}

octave::jit_type *
octave_builtin::to_jit (void) const
{
  return m_jtype;
}

void
octave_builtin::stash_jit (octave::jit_type& type)
{
  m_jtype = &type;
}

octave_builtin::fcn
octave_builtin::function (void) const
{
  return m_fcn;
}

octave_builtin::meth
octave_builtin::method (void) const
{
  return m_meth;
}

void
octave_builtin::push_dispatch_class (const std::string& dispatch_type)
{
  m_dispatch_classes.insert (dispatch_type);
}

bool
octave_builtin::handles_dispatch_class (const std::string& dispatch_type) const
{
  return m_dispatch_classes.find (dispatch_type) != m_dispatch_classes.end ();
}
