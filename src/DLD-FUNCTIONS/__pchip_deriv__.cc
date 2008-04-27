/* 

Copyright (C) 2002, 2006, 2007 Kai Habel

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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "f77-fcn.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dpchim, DPCHIM) (const octave_idx_type& n, double *x, double *f,
			     double *d, const octave_idx_type &incfd,
			     octave_idx_type *ierr);

  F77_RET_T
  F77_FUNC (pchim, PCHIM) (const octave_idx_type& n, float *x, float *f,
			   float *d, const octave_idx_type &incfd,
			   octave_idx_type *ierr);
}

// Wrapper for SLATEC/PCHIP function DPCHIM to calculate the derivates
// for piecewise polynomials.

DEFUN_DLD (__pchip_deriv__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __pchip_deriv__ (@var{x}, @var{y})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
  const int nargin = args.length ();

  if (nargin == 2)
    {
      if (args(0).is_single_type () || args(1).is_single_type ())
	{
	  FloatColumnVector xvec (args(0).float_vector_value ());
	  FloatMatrix ymat (args(1).float_matrix_value ());

	  octave_idx_type nx = xvec.length ();
	  octave_idx_type nyr = ymat.rows ();
	  octave_idx_type nyc = ymat.columns ();

	  if (nx != nyr)
	    {
	      error ("number of rows for x and y must match");
	      return retval;
	    }

	  FloatColumnVector dvec (nx), yvec (nx);
	  FloatMatrix dmat (nyr, nyc);

	  octave_idx_type ierr;
	  const octave_idx_type incfd = 1;
	  for (int c = 0; c < nyc; c++)
	    {
	      for (int r = 0; r < nx; r++)
		yvec(r) = ymat(r,c);

	      F77_FUNC (pchim, PCHIM) (nx, xvec.fortran_vec (), 
				       yvec.fortran_vec (), 
				       dvec.fortran_vec (), incfd, &ierr);

	      if (ierr < 0)
		{
		  error ("PCHIM error: %i\n", ierr);
		  return retval;
		}

	      for (int r=0; r<nx; r++)
		dmat(r,c) = dvec(r);
	    }

	  retval = dmat;
	}
      else
	{
	  ColumnVector xvec (args(0).vector_value ());
	  Matrix ymat (args(1).matrix_value ());

	  octave_idx_type nx = xvec.length ();
	  octave_idx_type nyr = ymat.rows ();
	  octave_idx_type nyc = ymat.columns ();

	  if (nx != nyr)
	    {
	      error ("number of rows for x and y must match");
	      return retval;
	    }

	  ColumnVector dvec (nx), yvec (nx);
	  Matrix dmat (nyr, nyc);

	  octave_idx_type ierr;
	  const octave_idx_type incfd = 1;
	  for (int c = 0; c < nyc; c++)
	    {
	      for (int r = 0; r < nx; r++)
		yvec(r) = ymat(r,c);

	      F77_FUNC (dpchim, DPCHIM) (nx, xvec.fortran_vec (), 
					 yvec.fortran_vec (), 
					 dvec.fortran_vec (), incfd, &ierr);

	      if (ierr < 0)
		{
		  error ("DPCHIM error: %i\n", ierr);
		  return retval;
		}

	      for (int r=0; r<nx; r++)
		dmat(r,c) = dvec(r);
	    }

	  retval = dmat;
	}
    }

  return retval;
}
