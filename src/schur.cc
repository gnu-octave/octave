/*

Copyright (C) 1996 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "CmplxSCHUR.h"
#include "dbleSCHUR.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (schur, args, nargout,
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
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ("schur");
      return retval;
    }

  octave_value arg = args(0);

  string ord;

  if (nargin == 2)
    {
      ord = args(1).string_value (); 

      if (error_state)
	{
	  error ("schur: expecting string as second argument");
	  return retval;
	}
    }

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char != 'U' && ord_char != 'A' && ord_char != 'D'
      && ord_char != 'u' && ord_char != 'a' && ord_char != 'd')
    {
      warning ("schur: incorrect ordered schur argument `%c'",
	       ord.c_str ());
      return retval;
    }

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("schur", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("schur");
      return retval;
    }

  if (arg.is_real_type ())
    {
      Matrix tmp = arg.matrix_value ();

      if (! error_state)
	{
	  SCHUR result (tmp, ord);

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
  else if (arg.is_complex_type ())
    {
      ComplexMatrix ctmp = arg.complex_matrix_value ();

      if (! error_state)
	{
	  ComplexSCHUR result (ctmp, ord);
 
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
;;; End: ***
*/
