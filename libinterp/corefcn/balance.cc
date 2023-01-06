////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <string>

#include "CMatrix.h"
#include "aepbalance.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "gepbalance.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "f77-fcn.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (balance, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{AA} =} balance (@var{A})
@deftypefnx {} {@var{AA} =} balance (@var{A}, @var{opt})
@deftypefnx {} {[@var{DD}, @var{AA}] =} balance (@var{A}, @var{opt})
@deftypefnx {} {[@var{D}, @var{P}, @var{AA}] =} balance (@var{A}, @var{opt})
@deftypefnx {} {[@var{CC}, @var{DD}, @var{AA}, @var{BB}] =} balance (@var{A}, @var{B}, @var{opt})

Balance the matrix @var{A} to reduce numerical errors in future
calculations.

Compute @code{@var{AA} = @var{DD} \ @var{A} * @var{DD}} in which @var{AA}
is a matrix whose row and column norms are roughly equal in magnitude, and
@code{@var{DD} = @var{P} * @var{D}}, in which @var{P} is a permutation
matrix and @var{D} is a diagonal matrix of powers of two.  This allows the
equilibration to be computed without round-off.  Results of eigenvalue
calculation are typically improved by balancing first.

If two output values are requested, @code{balance} returns
the diagonal @var{D} and the permutation @var{P} separately as vectors.
In this case, @code{@var{DD} = eye(n)(:,@var{P}) * diag (@var{D})}, where
@math{n} is the matrix size.

If four output values are requested, compute @code{@var{AA} =
@var{CC}*@var{A}*@var{DD}} and @code{@var{BB} = @var{CC}*@var{B}*@var{DD}},
in which @var{AA} and @var{BB} have nonzero elements of approximately the
same magnitude and @var{CC} and @var{DD} are permuted diagonal matrices as
in @var{DD} for the algebraic eigenvalue problem.

The eigenvalue balancing option @var{opt} may be one of:

@table @asis
@item @qcode{"noperm"}, @qcode{"S"}
Scale only; do not permute.

@item @qcode{"noscal"}, @qcode{"P"}
Permute only; do not scale.
@end table

Algebraic eigenvalue balancing uses standard @sc{lapack} routines.

Generalized eigenvalue problem balancing uses Ward's algorithm
(SIAM Journal on Scientific and Statistical Computing, 1981).
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3 || nargout < 0)
    print_usage ();

  octave_value_list retval;

  // determine if it's AEP or GEP
  bool AEPcase = nargin == 1 || args(1).is_string ();

  // problem dimension
  octave_idx_type nn = args(0).rows ();

  if (nn != args(0).columns ())
    err_square_matrix_required ("balance", "A");

  bool isfloat = args(0).is_single_type ()
                 || (! AEPcase && args(1).is_single_type ());

  bool complex_case = args(0).iscomplex ()
                      || (! AEPcase && args(1).iscomplex ());

  // Extract argument 1 parameter for both AEP and GEP.
  Matrix aa;
  ComplexMatrix caa;
  FloatMatrix faa;
  FloatComplexMatrix fcaa;

  if (isfloat)
    {
      if (complex_case)
        fcaa = args(0).float_complex_matrix_value ();
      else
        faa = args(0).float_matrix_value ();
    }
  else
    {
      if (complex_case)
        caa = args(0).complex_matrix_value ();
      else
        aa = args(0).matrix_value ();
    }

  // Treat AEP/GEP cases.
  if (AEPcase)
    {
      // Algebraic eigenvalue problem.
      bool noperm = false;
      bool noscal = false;
      if (nargin > 1)
        {
          std::string a1s = args(1).string_value ();
          noperm = a1s == "noperm" || a1s == "S";
          noscal = a1s == "noscal" || a1s == "P";
        }

      // balance the AEP
      if (isfloat)
        {
          if (complex_case)
            {
              math::aepbalance<FloatComplexMatrix> result (fcaa, noperm, noscal);

              if (nargout == 0 || nargout == 1)
                retval = ovl (result.balanced_matrix ());
              else if (nargout == 2)
                retval = ovl (result.balancing_matrix (),
                              result.balanced_matrix ());
              else
                retval = ovl (result.scaling_vector (),
                              result.permuting_vector (),
                              result.balanced_matrix ());
            }
          else
            {
              math::aepbalance<FloatMatrix> result (faa, noperm, noscal);

              if (nargout == 0 || nargout == 1)
                retval = ovl (result.balanced_matrix ());
              else if (nargout == 2)
                retval = ovl (result.balancing_matrix (),
                              result.balanced_matrix ());
              else
                retval = ovl (result.scaling_vector (),
                              result.permuting_vector (),
                              result.balanced_matrix ());
            }
        }
      else
        {
          if (complex_case)
            {
              math::aepbalance<ComplexMatrix> result (caa, noperm, noscal);

              if (nargout == 0 || nargout == 1)
                retval = ovl (result.balanced_matrix ());
              else if (nargout == 2)
                retval = ovl (result.balancing_matrix (),
                              result.balanced_matrix ());
              else
                retval = ovl (result.scaling_vector (),
                              result.permuting_vector (),
                              result.balanced_matrix ());
            }
          else
            {
              math::aepbalance<Matrix> result (aa, noperm, noscal);

              if (nargout == 0 || nargout == 1)
                retval = ovl (result.balanced_matrix ());
              else if (nargout == 2)
                retval = ovl (result.balancing_matrix (),
                              result.balanced_matrix ());
              else
                retval = ovl (result.scaling_vector (),
                              result.permuting_vector (),
                              result.balanced_matrix ());
            }
        }
    }
  else
    {
      std::string bal_job;
      if (nargout == 1)
        warning ("balance: used GEP, should have two output arguments");

      // Generalized eigenvalue problem.
      if (nargin == 2)
        bal_job = 'B';
      else
        bal_job = args(2).xstring_value ("balance: OPT argument must be a string");

      if ((nn != args(1).columns ()) || (nn != args(1).rows ()))
        ::err_nonconformant ();

      Matrix bb;
      ComplexMatrix cbb;
      FloatMatrix fbb;
      FloatComplexMatrix fcbb;

      if (isfloat)
        {
          if (complex_case)
            fcbb = args(1).float_complex_matrix_value ();
          else
            fbb = args(1).float_matrix_value ();
        }
      else
        {
          if (complex_case)
            cbb = args(1).complex_matrix_value ();
          else
            bb = args(1).matrix_value ();
        }

      // balance the GEP
      if (isfloat)
        {
          if (complex_case)
            {
              math::gepbalance<FloatComplexMatrix> result (fcaa, fcbb, bal_job);

              switch (nargout)
                {
                case 4:
                  retval(3) = result.balanced_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 3:
                  retval(2) = result.balanced_matrix ();
                  retval(1) = result.balancing_matrix2 ();
                  retval(0) = result.balancing_matrix ();
                  break;

                case 2:
                  retval(1) = result.balancing_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 1:
                  retval(0) = result.balancing_matrix ();
                  break;

                default:
                  error ("balance: invalid number of output arguments");
                  break;
                }
            }
          else
            {
              math::gepbalance<FloatMatrix> result (faa, fbb, bal_job);

              switch (nargout)
                {
                case 4:
                  retval(3) = result.balanced_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 3:
                  retval(2) = result.balanced_matrix ();
                  retval(1) = result.balancing_matrix2 ();
                  retval(0) = result.balancing_matrix ();
                  break;

                case 2:
                  retval(1) = result.balancing_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 1:
                  retval(0) = result.balancing_matrix ();
                  break;

                default:
                  error ("balance: invalid number of output arguments");
                  break;
                }
            }
        }
      else
        {
          if (complex_case)
            {
              math::gepbalance<ComplexMatrix> result (caa, cbb, bal_job);

              switch (nargout)
                {
                case 4:
                  retval(3) = result.balanced_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 3:
                  retval(2) = result.balanced_matrix ();
                  retval(1) = result.balancing_matrix2 ();
                  retval(0) = result.balancing_matrix ();
                  break;

                case 2:
                  retval(1) = result.balancing_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 1:
                  retval(0) = result.balancing_matrix ();
                  break;

                default:
                  error ("balance: invalid number of output arguments");
                  break;
                }
            }
          else
            {
              math::gepbalance<Matrix> result (aa, bb, bal_job);

              switch (nargout)
                {
                case 4:
                  retval(3) = result.balanced_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 3:
                  retval(2) = result.balanced_matrix ();
                  retval(1) = result.balancing_matrix2 ();
                  retval(0) = result.balancing_matrix ();
                  break;

                case 2:
                  retval(1) = result.balancing_matrix2 ();
                  OCTAVE_FALLTHROUGH;

                case 1:
                  retval(0) = result.balancing_matrix ();
                  break;

                default:
                  error ("balance: invalid number of output arguments");
                  break;
                }
            }
        }
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
