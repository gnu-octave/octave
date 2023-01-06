////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022-2023 The Octave Project Developers
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

#include "defun.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// If TRUE, use VM evaluator rather than tree walker.

static bool V__enable_vm_eval__ = false;

DEFUN (__enable_vm_eval__, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} __enable_vm_eval__ ()
@deftypefnx {} {@var{old_val} =} __enable_vm_eval__ (@var{new_val})
@deftypefnx {} {@var{old_val} =} __enable_vm_eval__ (@var{new_val}, "local")
Query or set whether Octave uses a virtual machine (VM) for evaluation of
parsed statements.

The default value is false.  When false, Octave uses a traditional tree walker
to evaluate statements parsed from m-code.  When true, Octave translates parsed
statements to an intermediate representation that is then evaluated by a
virtual machine.

When called from inside a function with the @qcode{"local"} option, the setting
is changed locally for the function and any subroutines it calls.  The original
setting is restored when exiting the function.
@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_VM_EVALUATOR)

  return set_internal_variable (V__enable_vm_eval__, args, nargout,
                                "__enable_vm_eval__");

#else

  octave_unused_parameter (args);

  err_disabled_feature ("vm-evaluator",
                        "using a Virtual Machine for statement evaluation");

#endif
}

OCTAVE_END_NAMESPACE(octave)
