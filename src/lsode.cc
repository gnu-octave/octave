// f-lsode.cc                                           -*- C++ -*-
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

#include "ODE.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "f-lsode.h"

// Global pointer for user defined function required by lsode.
static tree *lsode_fcn;

#ifdef WITH_DLD
tree_constant *
builtin_lsode_2 (const tree_constant *args, int nargin, int nargout)
{
  return lsode (args, nargin, nargout);
}

tree_constant *
builtin_lsode_options_2 (const tree_constant *args, int nargin, int nargout)
{
  return lsode_options (args, nargin, nargout);
}
#endif

ColumnVector
lsode_user_function (const ColumnVector& x, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

//  tree_constant name (lsode_fcn->name ());
  tree_constant *args = new tree_constant [3];
//  args[0] = name;
  args[2] = tree_constant (t);

  if (nstates > 1)
    {
      Matrix m (nstates, 1);
      for (int i = 0; i < nstates; i++)
	m (i, 0) = x.elem (i);
      tree_constant state (m);
      args[1] = state;
    }
  else
    {
      double d = x.elem (0);
      tree_constant state (d);
      args[1] = state;
    }

  if (lsode_fcn != NULL_TREE)
    {
      tree_constant *tmp = lsode_fcn->eval (args, 3, 1, 0);

      delete [] args;

      if (error_state)
	{
	  gripe_user_supplied_eval ("lsode");
	  return retval;
	}

      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  retval = tmp[0].to_vector ();

	  delete [] tmp;

	  if (retval.length () == 0)
	    gripe_user_supplied_eval ("lsode");
	}
      else
	{
	  delete [] tmp;
	  gripe_user_supplied_eval ("lsode");
	}
    }

  return retval;
}

tree_constant *
lsode (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  lsode_fcn = is_valid_function (args[1], "lsode", 1);
  if (lsode_fcn == NULL_TREE
      || takes_correct_nargs (lsode_fcn, 3, "lsode", 1) != 1)
    return retval;

  ColumnVector state = args[2].to_vector ();
  ColumnVector out_times = args[3].to_vector ();
  ColumnVector crit_times;
  int crit_times_set = 0;
  if (nargin > 4)
    {
      crit_times = args[4].to_vector ();
      crit_times_set = 1;
    }

  double tzero = out_times.elem (0);
  int nsteps = out_times.capacity ();

  ODEFunc func (lsode_user_function);
  ODE ode (state, tzero, func);

  int nstates = state.capacity ();
  Matrix output (nsteps, nstates + 1);

  if (crit_times_set)
    output = ode.integrate (out_times, crit_times);
  else
    output = ode.integrate (out_times);

  retval = new tree_constant [2];
  retval[0] = tree_constant (output);
  return retval;
}

tree_constant *
lsode_options (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;
  error ("lsode_options: not implemented yet");
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
