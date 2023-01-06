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

#include "unwind-prot.h"

#include "error.h"
#include "ovl.h"
#include "ov-fcn.h"
#include "pt-eval.h"

octave_base_value *
octave_function::clone (void) const
{
  panic_impossible ();
}

octave_base_value *
octave_function::empty_clone (void) const
{
  panic_impossible ();
}

octave_value_list
octave_function::call (octave::tree_evaluator& tw, int nargout,
                       const octave_value_list& args)
{
  tw.push_stack_frame (this);

  octave::unwind_action act ([&tw] () { tw.pop_stack_frame (); });

  return execute (tw, nargout, args);
}
