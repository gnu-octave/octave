// f-ifft.cc                                           -*- C++ -*-
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

#include "dMatrix.h"
#include "CMatrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD ("ifft", Fifft, Sifft,2, 1,
  "ifft (X): inverse fast fourier transform of a vector")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    {
      print_usage ("ifft");
      return retval;
    }

  tree_constant tmp = args(1).make_numeric ();
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("ifft", 0);

	  retval.resize (1, Matrix ());
	}
      else
	gripe_empty_arg ("ifft", 1);

      return retval;
    }

  if (tmp.is_real_matrix ())
    {
      Matrix m = tmp.matrix_value ();
      ComplexMatrix mifft = m.ifourier ();
      retval = mifft;
    }
  else if (tmp.is_complex_matrix ())
    {
      ComplexMatrix m = tmp.complex_matrix_value ();
      ComplexMatrix mifft = m.ifourier ();
      retval = mifft;
    }
  else if (tmp.is_scalar_type ())
    {
      error ("ifft: invalid scalar arguement");
    }
  else
    {
      gripe_wrong_type_arg ("ifft", tmp);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
