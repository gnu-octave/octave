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

#include "DET.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "ops.h"

#include "ov-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-perm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

#define MAYBE_CAST(VAR, CLASS)                                          \
  const CLASS *VAR = (arg.type_id () == CLASS::static_type_id ()        \
                      ? dynamic_cast<const CLASS *> (&arg.get_rep ())   \
                      : nullptr)

DEFUN (det, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{d} =} det (@var{A})
@deftypefnx {} {[@var{d}, @var{rcond}] =} det (@var{A})
Compute the determinant of @var{A}.

Return an estimate of the reciprocal condition number if requested.

Programming Notes: Routines from @sc{lapack} are used for full matrices and
code from @sc{umfpack} is used for sparse matrices.

The determinant should not be used to check a matrix for singularity.
For that, use any of the condition number functions: @code{cond},
@code{condest}, @code{rcond}.
@seealso{cond, condest, rcond}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value arg = args(0);

  if (arg.isempty ())
    return ovl (1.0);

  if (arg.rows () != arg.columns ())
    err_square_matrix_required ("det", "A");

  octave_value_list retval (2);

  bool isfloat = arg.is_single_type ();

  if (arg.is_diag_matrix ())
    {
      if (nargout <= 1)
        retval.resize (1);

      if (arg.iscomplex ())
        {
          if (isfloat)
            {
              retval(0) = arg.float_complex_diag_matrix_value ()
                          .determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.float_complex_diag_matrix_value ().rcond ();
            }
          else
            {
              retval(0) = arg.complex_diag_matrix_value ()
                          .determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.complex_diag_matrix_value ().rcond ();
            }
        }
      else
        {
          if (isfloat)
            {
              retval(0) = arg.float_diag_matrix_value ()
                          .determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.float_diag_matrix_value ().rcond ();
            }
          else
            {
              retval(0) = arg.diag_matrix_value ().determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.diag_matrix_value ().rcond ();
            }
        }
    }
  else if (arg.is_perm_matrix ())
    {
      if (nargout <= 1)
        retval.resize (1);

      retval(0) = static_cast<double> (arg.perm_matrix_value ().determinant ());
      if (nargout > 1)
        retval(1) = 1.0;
    }
  else if (arg.is_single_type ())
    {
      if (arg.isreal ())
        {
          octave_idx_type info;
          float rcond = 0.0;
          // Always compute rcond, so we can detect singular matrices.
          FloatMatrix m = arg.float_matrix_value ();

          MAYBE_CAST (rep, octave_float_matrix);
          MatrixType mtype = (rep ? rep -> matrix_type () : MatrixType ());
          FloatDET det = m.determinant (mtype, info, rcond);
          retval(0) = (info == -1 ? 0.0f : det.value ());
          retval(1) = rcond;
          if (rep)
            rep->matrix_type (mtype);
        }
      else if (arg.iscomplex ())
        {
          octave_idx_type info;
          float rcond = 0.0;
          // Always compute rcond, so we can detect singular matrices.
          FloatComplexMatrix m = arg.float_complex_matrix_value ();

          MAYBE_CAST (rep, octave_float_complex_matrix);
          MatrixType mtype = (rep ? rep -> matrix_type () : MatrixType ());
          FloatComplexDET det = m.determinant (mtype, info, rcond);
          retval(0) = (info == -1 ? FloatComplex (0.0) : det.value ());
          retval(1) = rcond;
          if (rep)
            rep->matrix_type (mtype);
        }
    }
  else
    {
      if (arg.isreal ())
        {
          octave_idx_type info;
          double rcond = 0.0;
          // Always compute rcond, so we can detect singular matrices.
          if (arg.issparse ())
            {
              SparseMatrix m = arg.sparse_matrix_value ();

              DET det = m.determinant (info, rcond);
              retval(0) = (info == -1 ? 0.0 : det.value ());
              retval(1) = rcond;
            }
          else
            {
              Matrix m = arg.matrix_value ();

              MAYBE_CAST (rep, octave_matrix);
              MatrixType mtype = (rep ? rep -> matrix_type ()
                                  : MatrixType ());
              DET det = m.determinant (mtype, info, rcond);
              retval(0) = (info == -1 ? 0.0 : det.value ());
              retval(1) = rcond;
              if (rep)
                rep->matrix_type (mtype);
            }
        }
      else if (arg.iscomplex ())
        {
          octave_idx_type info;
          double rcond = 0.0;
          // Always compute rcond, so we can detect singular matrices.
          if (arg.issparse ())
            {
              SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

              ComplexDET det = m.determinant (info, rcond);
              retval(0) = (info == -1 ? Complex (0.0) : det.value ());
              retval(1) = rcond;
            }
          else
            {
              ComplexMatrix m = arg.complex_matrix_value ();

              MAYBE_CAST (rep, octave_complex_matrix);
              MatrixType mtype = (rep ? rep -> matrix_type ()
                                  : MatrixType ());
              ComplexDET det = m.determinant (mtype, info, rcond);
              retval(0) = (info == -1 ? Complex (0.0) : det.value ());
              retval(1) = rcond;
              if (rep)
                rep->matrix_type (mtype);
            }
        }
      else
        err_wrong_type_arg ("det", arg);
    }

  return retval;
}

/*
%!assert (det ([1, 2; 3, 4]), -2, 10*eps)
%!assert (det (single ([1, 2; 3, 4])), single (-2), 10* eps ("single"))
%!assert (det (eye (2000)), 1)
%!error det ()
%!error det (1, 2)
%!error <must be a square matrix> det ([1, 2; 3, 4; 5, 6])
*/

OCTAVE_END_NAMESPACE(octave)
