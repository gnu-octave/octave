// f-fsqp.cc                                           -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef __GNUG__
#pragma implementation
#endif

#ifndef FSQP_MISSING

#include "FSQP.h"

#include "tree-const.h"
#include "error.h"
#include "f-fsqp.h"

// Global pointers for user defined functions required by fsqp.
static tree *fsqp_objective;
static tree *fsqp_constraints;

#ifdef WITH_DLD
tree_constant *
builtin_fsqp_2 (tree_constant *args, int nargin, int nargout)
{
  return fsqp (args, nargin, nargout);
}
#endif

double
fsqp_objective_function (ColumnVector& x)
{
  return 0.0;
}

ColumnVector
fsqp_constraint_function (ColumnVector& x)
{
  ColumnVector retval;
  return retval;
}

tree_constant *
fsqp (tree_constant *args, int nargin, int nargout)
{
/*

Handle all of the following:

  1. fsqp (x, phi)
  2. fsqp (x, phi, lb, ub)
  3. fsqp (x, phi, lb, ub, llb, c, lub)
  4. fsqp (x, phi, lb, ub, llb, c, lub, nllb, g, nlub)
  5. fsqp (x, phi, lb, ub,              nllb, g, nlub)
  6. fsqp (x, phi,         llb, c, lub, nllb, g, nlub)
  7. fsqp (x, phi,         llb, c, lub)
  8. fsqp (x, phi,                      nllb, g, nlub)

*/

// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;
  message ("fsqp", "not implemented yet...");
  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
