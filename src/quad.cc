// f-quad.cc                                           -*- C++ -*-
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

#include "Quad.h"

#include "tree-const.h"
#include "variables.h"
#include "mappers.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "f-quad.h"

// Global pointer for user defined function required by quadrature functions.
static tree *quad_fcn;

#ifdef WITH_DLD
tree_constant *
builtin_quad_2 (const tree_constant *args, int nargin, int nargout)
{
  return do_quad (args, nargin, nargout);
}

tree_constant *
builtin_quad_options_2 (const tree_constant *args, int nargin, int nargout)
{
  return quad_options (args, nargin, nargout);
}
#endif

double
quad_user_function (double x)
{
  double retval = 0.0;

//  tree_constant name = tree_constant (quad_fcn->name ());
  tree_constant *args = new tree_constant [2];
//  args[0] = name;
  args[1] = tree_constant (x);

  if (quad_fcn != NULL_TREE)
    {
      tree_constant *tmp = quad_fcn->eval (args, 2, 1, 0);

      delete [] args;

      if (error_state)
	{
	  delete [] tmp;
	  quad_integration_error = 1;  // XXX FIXME XXX
	  gripe_user_supplied_eval ("quad");
	  return retval;
	}

      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  retval = tmp[0].to_scalar ();
	  delete [] tmp;
	}
      else
	{
	  delete [] tmp;
	  quad_integration_error = 1;  // XXX FIXME XXX
	  gripe_user_supplied_eval ("quad");
	}
    }

  return retval;
}

tree_constant *
do_quad (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  quad_fcn = is_valid_function (args[1], "fsolve", 1);
  if (quad_fcn == NULL_TREE
      || takes_correct_nargs (quad_fcn, 2, "fsolve", 1) != 1)
    return retval;

  double a = args[2].to_scalar ();
  double b = args[3].to_scalar ();

  int indefinite = 0;
  IndefQuad::IntegralType indef_type = IndefQuad::doubly_infinite;
  double bound = 0.0;
  if ((int) xisinf (a) && (int) xisinf (b))
    {
      indefinite = 1;
      indef_type = IndefQuad::doubly_infinite;
    }
  else if ((int) xisinf (a))
    {
      indefinite = 1;
      bound = b;
      indef_type = IndefQuad::neg_inf_to_bound;
    }
  else if ((int) xisinf (b))
    {
      indefinite = 1;
      bound = a;
      indef_type = IndefQuad::bound_to_inf;
    }

  int ier = 0;
  int nfun = 0;
  double abserr = 0.0;
  double val = 0.0;
  double abstol = 1e-6;
  double reltol = 1e-6;
  Vector tol (2);
  Vector sing;
  int have_sing = 0;
  switch (nargin)
    {
    case 6:
      if (indefinite)
	{
	  error("quad: singularities not allowed on infinite intervals");
	  return retval;
	}
      have_sing = 1;
      sing = args[5].to_vector ();
    case 5:
      tol = args[4].to_vector ();
      switch (tol.capacity ())
	{
	case 2:
	  reltol = tol.elem (1);
	case 1:
	  abstol = tol.elem (0);
	  break;
	default:
	  error ("quad: expecting tol to contain no more than two values");
	  return retval;
	}
    case 4:
      if (indefinite)
	{
	  IndefQuad iq (quad_user_function, bound, indef_type, abstol, reltol);
	  val = iq.integrate (ier, nfun, abserr);
	}
      else
	{
	  if (have_sing)
	    {
	      DefQuad dq (quad_user_function, a, b, sing, abstol, reltol);
	      val = dq.integrate (ier, nfun, abserr);
	    }
	  else
	    {
	      DefQuad dq (quad_user_function, a, b, abstol, reltol);
	      val = dq.integrate (ier, nfun, abserr);
	    }
	}
      break;
    default:
      panic_impossible ();
      break;
    }

  retval = new tree_constant [5];

  retval[0] = tree_constant (val);
  retval[1] = tree_constant ((double) ier);
  retval[2] = tree_constant ((double) nfun);
  retval[3] = tree_constant (abserr);

  return retval;
}

tree_constant *
quad_options (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;
  error ("quad_options: not implemented yet");
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
