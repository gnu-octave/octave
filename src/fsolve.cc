// f-fsolve.cc                                           -*- C++ -*-
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

#include "NLEqn.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "f-fsolve.h"

// Global pointer for user defined function required by hybrd1.
static tree *fsolve_fcn;

#ifdef WITH_DLD
tree_constant *
builtin_fsolve (const tree_constant *args, int nargin, int nargout)
{
  return fsolve (args, nargin, nargout);
}

tree_constant *
builtin_fsolve_options (const tree_constant *args, int nargin, int nargout)
{
  return fsolve_options (args, nargin, nargout);
}
#endif

int
hybrd_info_to_fsolve_info (int info)
{
  switch (info)
    {
    case -1:
      info = -2;
      break;
    case 0:
      info = -1;
      break;
    case 1:
      break;
    case 2:
      info = 4;
      break;
    case 3:
    case 4:
    case 5:
      info = 3;
      break;
    default:
      panic_impossible ();
      break;
    }
  return info;
}

ColumnVector
fsolve_user_function (const ColumnVector& x)
{
  ColumnVector retval;

  int n = x.capacity ();

//  tree_constant name = tree_constant (fsolve_fcn->name ());
  tree_constant *args = new tree_constant [2];
//  args[0] = name;

  if (n > 1)
    {
      Matrix m (n, 1);
      for (int i = 0; i < n; i++)
	m (i, 0) = x.elem (i);
      tree_constant vars (m);
      args[1] = vars;
    }
  else
    {
      double d = x.elem (0);
      tree_constant vars (d);
      args[1] = vars;
    }

  if (fsolve_fcn != NULL_TREE)
    {
      tree_constant *tmp = fsolve_fcn->eval (args, 2, 1, 0);
      delete [] args;
      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  retval = tmp[0].to_vector ();

	  delete [] tmp;

	  if (retval.length () <= 0)
	    gripe_user_supplied_eval ("fsolve");
	}
      else
	{
	  delete [] tmp;
	  gripe_user_supplied_eval ("fsolve");
	}
    }

  return retval;
}

tree_constant *
fsolve (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  fsolve_fcn = is_valid_function (args[1], "fsolve", 1);
  if (fsolve_fcn == NULL_TREE
      || takes_correct_nargs (fsolve_fcn, 2, "fsolve", 1) != 1)
    return retval;

  ColumnVector x = args[2].to_vector ();

  if (nargin > 3)
    warning ("fsolve: ignoring optional arguments");

  if (nargout > 2)
    warning ("fsolve: can't compute path output yet");

  NLFunc foo_fcn (fsolve_user_function);
  NLEqn foo (x, foo_fcn);

  int info;
  ColumnVector soln = foo.solve (info);

  info = hybrd_info_to_fsolve_info (info);

  retval = new tree_constant [nargout+1];
  retval[0] = tree_constant (soln, 1);

  if (nargout > 1)
    retval[1] = tree_constant ((double) info);

  if (nargout > 2)
    retval[2] = tree_constant ();

  return retval;
}

tree_constant *
fsolve_options (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;
  error ("fsolve_options: not implemented yet");
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
