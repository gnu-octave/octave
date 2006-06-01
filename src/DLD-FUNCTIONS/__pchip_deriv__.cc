/* 

Copyright (C) 2002  Kai Habel

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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
  extern int F77_FUNC (dpchim, DPCHIM)
    (const int &n, double *x, double *f, double *d, const int &incfd,
     int *ierr);
}

DEFUN_DLD(__pchip_deriv__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __pchip_deriv__ (@var{x}, @var{y})\n\
Wrapper for SLATEC/PCHIP function DPCHIM to calculate the derivates for\n\
piecewise polynomials.  You should be using @code{pchip} function instead.\n\
@end deftypefn")
{
  octave_value retval;
  const int nargin = args.length ();

  if (nargin == 2)
    {
      ColumnVector xvec (args(0).vector_value ());
      Matrix ymat (args(1).matrix_value ());

      int nx = xvec.length ();
      int nyr = ymat.rows ();
      int nyc = ymat.columns ();

      if (nx != nyr)
        {
          error ("number of rows for x and y must match");
          return retval;
        }

      ColumnVector dvec (nx), yvec (nx);
      Matrix dmat (nyr, nyc);

      int ierr;
      const int incfd = 1;
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

  return retval;
}
