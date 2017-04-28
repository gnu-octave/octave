/*

Copyright (C) 2002-2017 Kai Habel
Copyright (C) 2008-2009 Jaroslav Hajek

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-slatec-proto.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"
#include "f77-fcn.h"

// Wrapper for SLATEC/PCHIP function DPCHIM to calculate the derivates
// for piecewise polynomials.

DEFUN (__pchip_deriv__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __pchip_deriv__ (@var{x}, @var{y}, @var{dim})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;
  int nargin = args.length ();

  bool rows = (nargin == 3 && args(2).uint_value () == 2);

  if (nargin >= 2)
    {
      if (args(0).is_single_type () || args(1).is_single_type ())
        {
          FloatColumnVector xvec (args(0).float_vector_value ());
          FloatMatrix ymat (args(1).float_matrix_value ());

          F77_INT nx = octave::to_f77_int (xvec.numel ());

          if (nx < 2)
            error ("__pchip_deriv__: X must be at least of length 2");

          octave_idx_type nyr = ymat.rows ();
          octave_idx_type nyc = ymat.columns ();

          if (nx != (rows ? nyc : nyr))
            error ("__pchip_deriv__: X and Y dimension mismatch");

          FloatMatrix dmat (nyr, nyc);

          F77_INT ierr;
          const F77_INT incfd = (rows ? octave::to_f77_int (nyr) : 1);
          volatile const octave_idx_type inc = (rows ? 1 : nyr);
          volatile octave_idx_type k = 0;

          for (volatile octave_idx_type i = (rows ? nyr : nyc); i > 0; i--)
            {
              F77_XFCN (pchim, PCHIM, (nx, xvec.data (),
                                       ymat.data () + k * inc,
                                       dmat.fortran_vec () + k * inc,
                                       incfd, ierr));

              k++;

              if (ierr < 0)
                error ("__pchip_deriv__: PCHIM failed with ierr = %i", ierr);
            }

          retval = dmat;
        }
      else
        {
          ColumnVector xvec (args(0).vector_value ());
          Matrix ymat (args(1).matrix_value ());

          F77_INT nx = octave::to_f77_int (xvec.numel ());

          if (nx < 2)
            error ("__pchip_deriv__: X must be at least of length 2");

          octave_idx_type nyr = ymat.rows ();
          octave_idx_type nyc = ymat.columns ();

          if (nx != (rows ? nyc : nyr))
            error ("__pchip_deriv__: X and Y dimension mismatch");

          Matrix dmat (nyr, nyc);

          F77_INT ierr;
          const F77_INT incfd = (rows ? octave::to_f77_int (nyr) : 1);
          volatile const octave_idx_type inc = (rows ? 1 : nyr);
          volatile octave_idx_type k = 0;

          for (volatile octave_idx_type i = (rows ? nyr : nyc); i > 0; i--)
            {
              F77_XFCN (dpchim, DPCHIM, (nx, xvec.data (),
                                         ymat.data () + k * inc,
                                         dmat.fortran_vec () + k * inc,
                                         incfd, ierr));
              k++;

              if (ierr < 0)
                error ("__pchip_deriv__: DPCHIM failed with ierr = %i", ierr);
            }

          retval = dmat;
        }
    }

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
