/*

Copyright (C) 2010 VZLU Prague

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-fcn-handle.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"

DEFBINOP (eq, fcn_handle, fcn_handle)
{
  CAST_BINOP_ARGS (const octave_fcn_handle&, const octave_fcn_handle&);

  // FIXME: this may not be entirely correct in case the handles were created
  // in different directories. Needs checking what recent Matlab does, and also
  // discussion whether we want to copy its behavior. Or to what extent.
  // Meanwhile, this catches all the "normal" usages, i.e.
  //   h == @sin
  // should yield true only if h is a handle to the global "sin", not a local one.

  return (v1.fcn_name () == v2.fcn_name () 
          && v1.fcn_val ().is_copy_of (v2.fcn_val ()));
}

void
install_fcn_ops (void)
{
  INSTALL_BINOP (op_eq, octave_fcn_handle, octave_fcn_handle, eq);
}
