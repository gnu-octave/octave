// f-schur.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

#include "dbleSCHUR.h"
#include "CmplxSCHUR.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "utils.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("schur", Fschur, Sschur, 3, 2,
  "[U, S] = schur (A) or S = schur (A)\n\
\n\
or, for ordered Schur:\n\
\n\
  [U, S] = schur (A, TYPE) or S = schur (A, TYPE)\n\
where TYPE is a string that begins with one of the following\n\
characters:\n\
\n\
  A = continuous time poles\n\
  D = discrete time poles\n\
  U = unordered schur (default)")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ("schur");
      return retval;
    }

  tree_constant arg = args(0);

  char *ord = "U";
  if (nargin == 2)
    {
      ord = args(1).string_value ();

      if (error_state)
	{
	  error ("schur: expecting string as second argument");
	  return retval;
	}
    }

  if (*ord != 'U' && *ord != 'A' && *ord != 'D'
      && *ord != 'u' && *ord != 'a' && *ord != 'd')
    {
      warning ("schur: incorrect ordered schur argument `%c'", *ord);
      return retval;
    }

  int nr = arg.rows ();
  int nc = arg.columns ();

  if (empty_arg ("schur", nr, nc) < 0)
    return retval;

  if (nr != nc)
    {
      gripe_square_matrix_required ("schur");
      return retval;
    }

  if (arg.is_real_matrix ())
    {
      Matrix tmp = arg.matrix_value ();

      if (! error_state)
	{
	  SCHUR result (tmp,ord);

	  if (nargout == 0 || nargout == 1)
	    {
	      retval(0) = result.schur_matrix ();
	    }
	  else
	    {
	      retval(1) = result.schur_matrix ();
	      retval(0) = result.unitary_matrix ();
	    }
	}
    }
  else if (arg.is_complex_matrix ())
    {
      ComplexMatrix ctmp = arg.complex_matrix_value ();

      if (! error_state)
	{
	  ComplexSCHUR result (ctmp,ord);
 
	  if (nargout == 0 || nargout == 1)
	    {
	      retval(0) = result.schur_matrix ();
	    }
	  else
	    {
	      retval(1) = result.schur_matrix ();
	      retval(0) = result.unitary_matrix ();
	    }
	}
    }    
  else
    {
      gripe_wrong_type_arg ("schur", arg);
    }
 
  return retval; 
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
