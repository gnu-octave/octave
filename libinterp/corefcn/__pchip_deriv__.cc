////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

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

OCTAVE_BEGIN_NAMESPACE(octave)

// Wrapper for SLATEC/PCHIP function DPCHIM to calculate the derivates
// for piecewise polynomials.

DEFUN (__pchip_deriv__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{d} =} __pchip_deriv__ (@var{x}, @var{y}, @var{dim})
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
          F77_INT nx = to_f77_int (xvec.numel ());
          if (nx < 2)
            error ("__pchip_deriv__: X must be at least of length 2");

          if (args(1).iscomplex ())
            {
              FloatComplexMatrix ymat (args(1).float_complex_matrix_value ());

              octave_idx_type nyr = ymat.rows ();
              octave_idx_type nyc = ymat.columns ();

              if (nx != (rows ? nyc : nyr))
                error ("__pchip_deriv__: X and Y dimension mismatch");

              FloatComplexMatrix dmat (nyr, nyc);

              F77_INT ierr;
              const F77_INT incfd = (rows ? to_f77_int (2*nyr) : 2);
              volatile const octave_idx_type inc = (rows ? 2 : 2*nyr);
              volatile octave_idx_type k = 0;

              for (volatile octave_idx_type i = (rows ? nyr : nyc); i > 0; i--)
                {
                  F77_XFCN (pchim, PCHIM, (nx, xvec.data (),
                                           reinterpret_cast<float const *>(ymat.data ()) + k * inc,
                                           reinterpret_cast<float *>(dmat.fortran_vec ()) + k * inc,
                                           incfd, ierr));

                  if (ierr < 0)
                    error ("__pchip_deriv__: PCHIM failed for real part with ierr = %"
                           OCTAVE_F77_INT_TYPE_FORMAT, ierr);

                  F77_XFCN (pchim, PCHIM, (nx, xvec.data (),
                                           reinterpret_cast<float const *>(ymat.data ()) + 1 + k * inc,
                                           reinterpret_cast<float *>(dmat.fortran_vec ()) + 1 + k * inc,
                                           incfd, ierr));

                  if (ierr < 0)
                    error ("__pchip_deriv__: PCHIM failed for imaginary part with ierr = %"
                           OCTAVE_F77_INT_TYPE_FORMAT, ierr);

                  k++;
                }

              retval = dmat;
            }
          else
            {
              FloatMatrix ymat (args(1).float_matrix_value ());

              octave_idx_type nyr = ymat.rows ();
              octave_idx_type nyc = ymat.columns ();

              if (nx != (rows ? nyc : nyr))
                error ("__pchip_deriv__: X and Y dimension mismatch");

              FloatMatrix dmat (nyr, nyc);

              F77_INT ierr;
              const F77_INT incfd = (rows ? to_f77_int (nyr) : 1);
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
                    error ("__pchip_deriv__: PCHIM failed with ierr = %"
                           OCTAVE_F77_INT_TYPE_FORMAT, ierr);
                }

              retval = dmat;
            }
        }
      else
        {
          ColumnVector xvec (args(0).vector_value ());
          F77_INT nx = to_f77_int (xvec.numel ());
          if (nx < 2)
            error ("__pchip_deriv__: X must be at least of length 2");

          if (args(1).iscomplex ())
            {
              ComplexMatrix ymat (args(1).complex_matrix_value ());

              octave_idx_type nyr = ymat.rows ();
              octave_idx_type nyc = ymat.columns ();

              if (nx != (rows ? nyc : nyr))
                error ("__pchip_deriv__: X and Y dimension mismatch");

              ComplexMatrix dmat (nyr, nyc);

              F77_INT ierr;
              const F77_INT incfd = (rows ? to_f77_int (2*nyr) : 2);
              volatile const octave_idx_type inc = (rows ? 2 : 2*nyr);
              volatile octave_idx_type k = 0;

              for (volatile octave_idx_type i = (rows ? nyr : nyc); i > 0; i--)
                {
                  F77_XFCN (dpchim, DPCHIM, (nx, xvec.data (),
                                             reinterpret_cast<double const *>(ymat.data ()) + k * inc,
                                             reinterpret_cast<double *>(dmat.fortran_vec ()) + k * inc,
                                             incfd, ierr));

                  if (ierr < 0)
                    error ("__pchip_deriv__: DPCHIM failed for real part with ierr = %"
                           OCTAVE_F77_INT_TYPE_FORMAT, ierr);

                  F77_XFCN (dpchim, DPCHIM, (nx, xvec.data (),
                                             reinterpret_cast<double const *>(ymat.data ()) + 1 + k * inc,
                                             reinterpret_cast<double *>(dmat.fortran_vec ()) + 1 + k * inc,
                                             incfd, ierr));

                  if (ierr < 0)
                    error ("__pchip_deriv__: DPCHIM failed for imaginary part with ierr = %"
                           OCTAVE_F77_INT_TYPE_FORMAT, ierr);

                  k++;
                }

              retval = dmat;
            }
          else
            {
              Matrix ymat (args(1).matrix_value ());

              octave_idx_type nyr = ymat.rows ();
              octave_idx_type nyc = ymat.columns ();

              if (nx != (rows ? nyc : nyr))
                error ("__pchip_deriv__: X and Y dimension mismatch");

              Matrix dmat (nyr, nyc);

              F77_INT ierr;
              const F77_INT incfd = (rows ? to_f77_int (nyr) : 1);
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
                    error ("__pchip_deriv__: DPCHIM failed with ierr = %"
                           OCTAVE_F77_INT_TYPE_FORMAT, ierr);
                }

              retval = dmat;
            }
        }
    }

  return retval;
}

/*
%!shared x, y
%! x = 0:3;
%! y = x.^2 + 1i * x.^3;

%!test
%! d_complex = __pchip_deriv__ (x, y, 2);
%! d_real = __pchip_deriv__ (x, real (y), 2);
%! d_imag = __pchip_deriv__ (x, imag (y), 2);
%! assert (real (d_complex), d_real);
%! assert (imag (d_complex), d_imag);

%!test
%! d_complex = __pchip_deriv__ (x.', y.');
%! d_real = __pchip_deriv__ (x.', real (y.'));
%! d_imag = __pchip_deriv__ (x.', imag (y.'));
%! assert (real (d_complex), d_real);
%! assert (imag (d_complex), d_imag);

%!test
%! d_complex = __pchip_deriv__ (single (x), single (y), 2);
%! d_real = __pchip_deriv__ (single (x), real (single (y)), 2);
%! d_imag = __pchip_deriv__ (single (x), imag (single (y)), 2);
%! assert (real (d_complex), d_real);
%! assert (imag (d_complex), d_imag);

%!test
%! d_complex = __pchip_deriv__ (single (x'), single (y'));
%! d_real = __pchip_deriv__ (single (x'), real (single (y')));
%! d_imag = __pchip_deriv__ (single (x'), imag (single (y')));
%! assert (real (d_complex), d_real);
%! assert (imag (d_complex), d_imag);
*/

OCTAVE_END_NAMESPACE(octave)
