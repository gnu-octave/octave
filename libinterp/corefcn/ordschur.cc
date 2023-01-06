////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#include "defun.h"
#include "error.h"
#include "lo-lapack-proto.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (ordschur, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{UR}, @var{SR}] =} ordschur (@var{U}, @var{S}, @var{select})
Reorder the real Schur factorization (@var{U},@var{S}) obtained with the
@code{schur} function, so that selected eigenvalues appear in the upper left
diagonal blocks of the quasi triangular Schur matrix.

The logical vector @var{select} specifies the selected eigenvalues as they
appear along @var{S}'s diagonal.

For example, given the matrix @code{@var{A} = [1, 2; 3, 4]}, and its Schur
decomposition

@example
[@var{U}, @var{S}] = schur (@var{A})
@end example

@noindent
which returns

@example
@group
@var{U} =

  -0.82456  -0.56577
   0.56577  -0.82456

@var{S} =

  -0.37228  -1.00000
   0.00000   5.37228

@end group
@end example

It is possible to reorder the decomposition so that the positive eigenvalue
is in the upper left corner, by doing:

@example
[@var{U}, @var{S}] = ordschur (@var{U}, @var{S}, [0,1])
@end example

@seealso{schur, ordeig, ordqz}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  const Array<octave_idx_type> sel_arg = args(
      2).xoctave_idx_type_vector_value ("ordschur: SELECT must be an array of integers");

  const octave_idx_type sel_n = sel_arg.numel ();

  const dim_vector dimU = args(0).dims ();
  const dim_vector dimS = args(1).dims ();

  if (sel_n != dimU(0))
    error ("ordschur: SELECT must have same length as the sides of U and S");
  else if (sel_n != dimU(0) || sel_n != dimS(0) || sel_n != dimU(1)
           || sel_n != dimS(1))
    error ("ordschur: U and S must be square and of equal sizes");

  octave_value_list retval;

  const bool double_type  = args(0).is_double_type ()
                            || args(1).is_double_type ();
  const bool complex_type = args(0).iscomplex ()
                            || args(1).iscomplex ();

#define PREPARE_ARGS(TYPE, TYPE_M, TYPE_COND)                           \
  TYPE ## Matrix U = args(0).x ## TYPE_M ## _value                      \
    ("ordschur: U and S must be real or complex floating point matrices"); \
  TYPE ## Matrix S = args(1).x ## TYPE_M ## _value                      \
    ("ordschur: U and S must be real or complex floating point matrices"); \
  TYPE ## Matrix w (dim_vector (n, 1));                                 \
  TYPE ## Matrix work (dim_vector (n, 1));                              \
  F77_INT m;                                                            \
  F77_INT info;                                                         \
  TYPE_COND cond1, cond2;

#define PREPARE_OUTPUT()                        \
  if (info != 0)                                \
    error ("ordschur: trsen failed");           \
                                                \
  retval = ovl (U, S);

  F77_INT n = to_f77_int (sel_n);
  Array<F77_INT> sel (dim_vector (n, 1));
  for (F77_INT i = 0; i < n; i++)
    sel.xelem (i) = to_f77_int (sel_arg.xelem (i));

  if (double_type)
    {
      if (complex_type)
        {
          PREPARE_ARGS (Complex, complex_matrix, double)

          F77_XFCN (ztrsen, ztrsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, F77_DBLE_CMPLX_ARG (S.fortran_vec ()), n,
                     F77_DBLE_CMPLX_ARG (U.fortran_vec ()), n,
                     F77_DBLE_CMPLX_ARG (w.fortran_vec ()), m, cond1, cond2,
                     F77_DBLE_CMPLX_ARG (work.fortran_vec ()), n,
                     info));

          PREPARE_OUTPUT()
        }
      else
        {
          PREPARE_ARGS (, matrix, double)
          Matrix wi (dim_vector (n, 1));
          Array<F77_INT> iwork (dim_vector (n, 1));

          F77_XFCN (dtrsen, dtrsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, S.fortran_vec (), n, U.fortran_vec (), n,
                     w.fortran_vec (), wi.fortran_vec (), m, cond1, cond2,
                     work.fortran_vec (), n, iwork.fortran_vec (), n, info));

          PREPARE_OUTPUT ()
        }
    }
  else
    {
      if (complex_type)
        {
          PREPARE_ARGS (FloatComplex, float_complex_matrix, float)

          F77_XFCN (ctrsen, ctrsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, F77_CMPLX_ARG (S.fortran_vec ()), n,
                     F77_CMPLX_ARG (U.fortran_vec ()), n,
                     F77_CMPLX_ARG (w.fortran_vec ()), m, cond1, cond2,
                     F77_CMPLX_ARG (work.fortran_vec ()), n,
                     info));

          PREPARE_OUTPUT ()
        }
      else
        {
          PREPARE_ARGS (Float, float_matrix, float)
          FloatMatrix wi (dim_vector (n, 1));
          Array<F77_INT> iwork (dim_vector (n, 1));

          F77_XFCN (strsen, strsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, S.fortran_vec (), n, U.fortran_vec (), n,
                     w.fortran_vec (), wi.fortran_vec (), m, cond1, cond2,
                     work.fortran_vec (), n, iwork.fortran_vec (), n, info));

          PREPARE_OUTPUT ()
        }
    }

#undef PREPARE_ARGS
#undef PREPARE_OUTPUT

  return retval;
}

/*

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4 ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (U, T, [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps));
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps));

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4 ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (single (U), single (T), [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps ("single")));
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps ("single")));

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4+3i ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (U, T, [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps));
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps));

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4+3i ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (single (U), single (T), [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps ("single")));
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps ("single")));

*/

OCTAVE_END_NAMESPACE(octave)
