// f-dassl.cc                                           -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "DAE.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "f-dassl.h"

// Global pointer for user defined function required by dassl.
static tree *dassl_fcn;

#ifdef WITH_DLD
tree_constant *
builtin_dassl_2 (const tree_constant *args, int nargin, int nargout)
{
  return dassl (args, nargin, nargout);
}
#endif

ColumnVector
dassl_user_function (const ColumnVector& x, const ColumnVector& xdot, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

  assert (nstates == xdot.capacity ());

//  tree_constant name (dassl_fcn->name ());
  tree_constant *args = new tree_constant [4];
//  args[0] = name;
  args[3] = tree_constant (t);

  if (nstates > 1)
    {
      Matrix m1 (nstates, 1);
      Matrix m2 (nstates, 1);
      for (int i = 0; i < nstates; i++)
	{
	  m1 (i, 0) = x.elem (i);
	  m2 (i, 0) = xdot.elem (i);
	}
      tree_constant state (m1);
      tree_constant deriv (m2);
      args[1] = state;
      args[2] = deriv;
    }
  else
    {
      double d1 = x.elem (0);
      double d2 = xdot.elem (0);
      tree_constant state (d1);
      tree_constant deriv (d2);
      args[1] = state;
      args[2] = deriv;
    }

  if (dassl_fcn != NULL_TREE)
    {
      tree_constant *tmp = dassl_fcn->eval (args, 4, 1, 0);
      delete [] args;
      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  retval = tmp[0].to_vector ();
	  delete [] tmp;
	}
      else
	{
	  delete [] tmp;
	  gripe_user_supplied_eval ("dassl");
	  jump_to_top_level ();
	}
    }

  return retval;
}

tree_constant *
dassl (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  dassl_fcn = is_valid_function (args[1], "dassl", 1);
  if (dassl_fcn == NULL_TREE
      || takes_correct_nargs (dassl_fcn, 4, "dassl", 1) != 1)
    return retval;

  ColumnVector state = args[2].to_vector ();
  ColumnVector deriv = args[3].to_vector ();
  ColumnVector out_times = args[4].to_vector ();
  ColumnVector crit_times;
  int crit_times_set = 0;
  if (nargin > 5)
    {
      crit_times = args[5].to_vector ();
      crit_times_set = 1;
    }

  if (state.capacity () != deriv.capacity ())
    {
      error ("dassl: x and xdot must have the same size");
      return retval;
    }

  double tzero = out_times.elem (0);

  DAEFunc func (dassl_user_function);
  DAE dae (state, deriv, tzero, func);

  Matrix output;
  Matrix deriv_output;

  if (crit_times_set)
    output = dae.integrate (out_times, deriv_output, crit_times);
  else
    output = dae.integrate (out_times, deriv_output);

  retval = new tree_constant [3];
  retval[0] = tree_constant (output);
  retval[1] = tree_constant (deriv_output);
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

