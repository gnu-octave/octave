////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

// This is the octave interface to amd, which bore the copyright given
// in the help of the functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>

#include "CSparse.h"
#include "Sparse.h"
#include "dMatrix.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"
#include "parse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (amd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} amd (@var{S})
@deftypefnx {} {@var{p} =} amd (@var{S}, @var{opts})

Return the approximate minimum degree permutation of a matrix.

This is a permutation such that the Cholesky@tie{}factorization of
@code{@var{S} (@var{p}, @var{p})} tends to be sparser than the
Cholesky@tie{}factorization of @var{S} itself.  @code{amd} is typically
faster than @code{symamd} but serves a similar purpose.

The optional parameter @var{opts} is a structure that controls the behavior
of @code{amd}.  The fields of the structure are

@table @asis
@item @var{opts}.dense
Determines what @code{amd} considers to be a dense row or column of the
input matrix.  Rows or columns with more than @code{max (16, (dense *
sqrt (@var{n})))} entries, where @var{n} is the order of the matrix @var{S},
are ignored by @code{amd} during the calculation of the permutation.
The value of dense must be a positive scalar and the default value is 10.0

@item @var{opts}.aggressive
If this value is a nonzero scalar, then @code{amd} performs aggressive
absorption.  The default is not to perform aggressive absorption.
@end table

The author of the code itself is Timothy A. Davis
(see @url{http://faculty.cse.tamu.edu/davis/suitesparse.html}).
@seealso{symamd, colamd}
@end deftypefn */)
{
#if defined (HAVE_AMD)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_idx_type n_row, n_col;
  const suitesparse_integer *ridx, *cidx;
  SparseMatrix sm;
  SparseComplexMatrix scm;

  if (args(0).issparse ())
    {
      if (args(0).iscomplex ())
        {
          scm = args(0).sparse_complex_matrix_value ();
          n_row = scm.rows ();
          n_col = scm.cols ();
          ridx = to_suitesparse_intptr (scm.xridx ());
          cidx = to_suitesparse_intptr (scm.xcidx ());
        }
      else
        {
          sm = args(0).sparse_matrix_value ();
          n_row = sm.rows ();
          n_col = sm.cols ();
          ridx = to_suitesparse_intptr (sm.xridx ());
          cidx = to_suitesparse_intptr (sm.xcidx ());
        }
    }
  else
    {
      if (args(0).iscomplex ())
        sm = SparseMatrix (real (args(0).complex_matrix_value ()));
      else
        sm = SparseMatrix (args(0).matrix_value ());

      n_row = sm.rows ();
      n_col = sm.cols ();
      ridx = to_suitesparse_intptr (sm.xridx ());
      cidx = to_suitesparse_intptr (sm.xcidx ());
    }

  if (n_row != n_col)
    err_square_matrix_required ("amd", "S");

  OCTAVE_LOCAL_BUFFER (double, Control, AMD_CONTROL);
  AMD_NAME (_defaults) (Control);
  if (nargin > 1)
    {
      octave_scalar_map arg1 = args(
                                 1).xscalar_map_value ("amd: OPTS argument must be a scalar structure");

      octave_value tmp;

      tmp = arg1.getfield ("dense");
      if (tmp.is_defined ())
        Control[AMD_DENSE] = tmp.double_value ();

      tmp = arg1.getfield ("aggressive");
      if (tmp.is_defined ())
        Control[AMD_AGGRESSIVE] = tmp.double_value ();
    }

  OCTAVE_LOCAL_BUFFER (suitesparse_integer, P, n_col);
  Matrix xinfo (AMD_INFO, 1);
  double *Info = xinfo.fortran_vec ();

  // FIXME: how can we manage the memory allocation of amd
  //        in a cleaner manner?
  SUITESPARSE_ASSIGN_FPTR (malloc_func, amd_malloc, malloc);
  SUITESPARSE_ASSIGN_FPTR (free_func, amd_free, free);
  SUITESPARSE_ASSIGN_FPTR (calloc_func, amd_calloc, calloc);
  SUITESPARSE_ASSIGN_FPTR (realloc_func, amd_realloc, realloc);
  SUITESPARSE_ASSIGN_FPTR (printf_func, amd_printf, printf);

  octave_idx_type result = AMD_NAME (_order) (n_col, cidx, ridx, P, Control,
                           Info);

  if (result == AMD_OUT_OF_MEMORY)
    error ("amd: out of memory");
  else if (result == AMD_INVALID)
    error ("amd: matrix S is corrupted");

  Matrix Pout (1, n_col);
  for (octave_idx_type i = 0; i < n_col; i++)
    Pout.xelem (i) = P[i] + 1;

  if (nargout > 1)
    return ovl (Pout, xinfo);
  else
    return ovl (Pout);

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("amd", "AMD");

#endif
}

/*
%!shared A, A2, opts
%! A = ones (20, 30);
%! A2 = ones (30, 30);

%!testif HAVE_AMD
%! assert(amd (A2), [1:30]);
%! opts.dense = 25;
%! assert(amd (A2, opts), [1:30]);
%! opts.aggressive = 1;
%! assert(amd (A2, opts), [1:30]);

%!testif HAVE_AMD
%! assert (amd ([]), zeros (1,0))

%!error <S must be a square matrix|was unavailable or disabled> amd (A)
%!error amd (A2, 2)
*/

OCTAVE_END_NAMESPACE(octave)
