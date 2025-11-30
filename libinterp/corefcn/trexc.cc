////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2025 The Octave Project Developers
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

#include "lapack-proto.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (trexc, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{U}, @var{T}] =} trexc (@var{U}, @var{T}, @var{M})
Reorder the Schur factorization using LAPACK's ZTREXC.

Given a unitary matrix @var{U} and an upper triangular Schur form @var{T}
from the Schur decomposition @code{A = U*T*U'}, this function reorders
the diagonal elements of @var{T} according to the swap specifications
in @var{M}.

Each row of @var{M} contains two indices @code{[IFST, ILST]} specifying
that the diagonal element at position @code{IFST} should be moved to
position @code{ILST}.  The swaps are performed sequentially.

@seealso{schur, ordschur}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  ComplexMatrix U = args(0).complex_matrix_value ();
  ComplexMatrix T = args(1).complex_matrix_value ();
  Matrix M = args(2).matrix_value ();

  F77_INT n = octave::to_f77_int (T.rows ());

  if (T.rows () != T.cols ())
    error ("trexc: T must be a square matrix");

  if (U.rows () != n || U.cols () != n)
    error ("trexc: U must have the same dimensions as T");

  if (M.cols () != 2)
    error ("trexc: M must have exactly 2 columns [IFST, ILST]");

  F77_INT ldt = n;
  F77_INT ldq = n;
  F77_INT info = 0;

  // Process each swap specified in M
  octave_idx_type n_swaps = M.rows ();

  for (octave_idx_type k = 0; k < n_swaps; k++)
    {
      F77_INT ifst = octave::to_f77_int (static_cast<int> (M(k, 0)));
      F77_INT ilst = octave::to_f77_int (static_cast<int> (M(k, 1)));

      // Validate indices
      if (ifst < 1 || ifst > n || ilst < 1 || ilst > n)
        error ("trexc: indices in M must be between 1 and %d", n);

      F77_XFCN (ztrexc, ZTREXC,
                (F77_CONST_CHAR_ARG ("V"),
                 n,
                 F77_DBLE_CMPLX_ARG (T.fortran_vec ()),
                 ldt,
                 F77_DBLE_CMPLX_ARG (U.fortran_vec ()),
                 ldq,
                 ifst,
                 ilst,
                 info
                 F77_CHAR_ARG_LEN (1)));

      if (info != 0)
        error ("trexc: ZTREXC failed with info = %d", info);
    }

  return ovl (U, T);
}

OCTAVE_END_NAMESPACE(octave)

/*
%!test
%! A = [1 2 3; 0 4 5; 0 0 6];
%! [U, T] = schur (A, "complex");
%! M = [1, 2];
%! [U2, T2] = trexc (U, T, M);
%! assert (U2 * T2 * U2', A, 4*eps);

%!test
%! v = randn ("state");
%! randn ("state", 42);
%! A = randn (4) + 1i * randn (4);
%! randn ("state", v);
%! [U, T] = schur (A, "complex");
%! M = [1, 3; 2, 4];
%! [U2, T2] = trexc (U, T, M);
%! assert (norm(U2 * T2 * U2' - A, "fro"), 0, 13 * eps(norm(A, "fro")));

%!error <must be a square matrix> trexc (eye (2), [1 2; 3 4; 5 6], [1 2])
%!error <must have exactly 2 columns> trexc (eye (2), eye (2), [1 2 3])
*/
