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

#include "schur.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename Matrix>
static octave_value
mark_upper_triangular (const Matrix& a)
{
  octave_value retval = a;

  octave_idx_type n = a.rows ();
  error_unless (a.columns () == n);

  const typename Matrix::element_type zero = typename Matrix::element_type ();

  for (octave_idx_type i = 0; i < n; i++)
    if (a(i, i) == zero)
      return retval;

  retval.matrix_type (MatrixType::Upper);

  return retval;
}

DEFUN (schur, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{S} =} schur (@var{A})
@deftypefnx {} {@var{S} =} schur (@var{A}, "real")
@deftypefnx {} {@var{S} =} schur (@var{A}, "complex")
@deftypefnx {} {@var{S} =} schur (@var{A}, @var{opt})
@deftypefnx {} {[@var{U}, @var{S}] =} schur (@dots{})
@cindex Schur decomposition
Compute the Schur@tie{}decomposition of @var{A}.

The Schur@tie{}decomposition of a square matrix @var{A} is defined as
@tex
$$
 S = U^T A U
$$
@end tex
@ifnottex

@example
@code{@var{S} = @var{U}' * @var{A} * @var{U}}
@end example

@end ifnottex
where @var{U} is a unitary matrix
@tex
($U^T U$ is identity)
@end tex
@ifnottex
(@code{@var{U}'* @var{U}} is identity)
@end ifnottex
and @var{S} is upper triangular.  The eigenvalues of @var{A} (and @var{S})
are the diagonal elements of @var{S}.  If the matrix @var{A} is real, then
the real Schur@tie{}decomposition is computed, in which the matrix @var{U}
is orthogonal and @var{S} is block upper triangular with blocks of size at
most
@tex
$2 \times 2$
@end tex
@ifnottex
@code{2 x 2}
@end ifnottex
along the diagonal.

The default for real matrices is a real Schur@tie{}decomposition.  A complex
decomposition may be forced by passing the flag @qcode{"complex"}.

The eigenvalues are optionally ordered along the diagonal according to the
value of @var{opt}:

@table @asis
@item @qcode{@var{opt} = "a"}
Move eigenvalues with negative real parts to the leading block of @var{S}.
Mnemonic: @qcode{"a"} for Algebraic @nospell{Riccati} Equations, where this
ordering is useful.

@item @qcode{@var{opt} = "d"}
Move eigenvalues with magnitude less than one to the leading block of @var{S}.
Mnemonic: @qcode{"d"} for Discrete Algebraic @nospell{Riccati} Equations,
where this ordering is useful.

@item @qcode{@var{opt} = "u"}
Unordered.  No particular ordering of eigenvalues (default).
@end table

The leading @var{k} columns of @var{U} always span the @var{A}-invariant
subspace corresponding to the @var{k} leading eigenvalues of @var{S}.
@seealso{rsf2csf, ordschur, ordeig, lu, chol, hess, qr, qz, svd, eig}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    print_usage ();

  octave_value arg = args(0);

  std::string ord;
  if (nargin == 2)
    ord = args(1).xstring_value ("schur: second argument must be a string");

  bool force_complex = false;

  if (ord == "real")
    {
      ord = "";
    }
  else if (ord == "complex")
    {
      force_complex = true;
      ord = "";
    }
  else
    {
      char ord_char = (ord.empty () ? 'U' : ord[0]);

      if (ord_char != 'U' && ord_char != 'A' && ord_char != 'D'
          && ord_char != 'u' && ord_char != 'a' && ord_char != 'd')
        {
          warning ("schur: incorrect ordered schur argument '%s'",
                   ord.c_str ());
          return ovl ();
        }
    }

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr != nc)
    err_square_matrix_required ("schur", "A");

  if (! arg.isnumeric ())
    err_wrong_type_arg ("schur", arg);

  octave_value_list retval;

  if (arg.is_single_type ())
    {
      if (! force_complex && arg.isreal ())
        {
          FloatMatrix tmp = arg.float_matrix_value ();

          if (nargout <= 1)
            {
              math::schur<FloatMatrix> result (tmp, ord, false);
              retval = ovl (result.schur_matrix ());
            }
          else
            {
              math::schur<FloatMatrix> result (tmp, ord, true);
              retval = ovl (result.unitary_schur_matrix (),
                            result.schur_matrix ());
            }
        }
      else
        {
          FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

          if (nargout <= 1)
            {
              math::schur<FloatComplexMatrix> result (ctmp, ord, false);
              retval = ovl (mark_upper_triangular (result.schur_matrix ()));
            }
          else
            {
              math::schur<FloatComplexMatrix> result (ctmp, ord, true);
              retval = ovl (result.unitary_schur_matrix (),
                            mark_upper_triangular (result.schur_matrix ()));
            }
        }
    }
  else
    {
      if (! force_complex && arg.isreal ())
        {
          Matrix tmp = arg.matrix_value ();

          if (nargout <= 1)
            {
              math::schur<Matrix> result (tmp, ord, false);
              retval = ovl (result.schur_matrix ());
            }
          else
            {
              math::schur<Matrix> result (tmp, ord, true);
              retval = ovl (result.unitary_schur_matrix (),
                            result.schur_matrix ());
            }
        }
      else
        {
          ComplexMatrix ctmp = arg.complex_matrix_value ();

          if (nargout <= 1)
            {
              math::schur<ComplexMatrix> result (ctmp, ord, false);
              retval = ovl (mark_upper_triangular (result.schur_matrix ()));
            }
          else
            {
              math::schur<ComplexMatrix> result (ctmp, ord, true);
              retval = ovl (result.unitary_schur_matrix (),
                            mark_upper_triangular (result.schur_matrix ()));
            }
        }
    }

  return retval;
}

/*
%!test
%! a = [1, 2, 3; 4, 5, 9; 7, 8, 6];
%! [u, s] = schur (a);
%! assert (u' * a * u, s, sqrt (eps));

%!test
%! a = single ([1, 2, 3; 4, 5, 9; 7, 8, 6]);
%! [u, s] = schur (a);
%! assert (u' * a * u, s, sqrt (eps ("single")));

%!error schur ()
%!error schur (1,2,3)
%!error [a,b,c] = schur (1)
%!error <must be a square matrix> schur ([1, 2, 3; 4, 5, 6])
%!error <wrong type argument 'cell'> schur ({1})
%!warning <incorrect ordered schur argument> schur ([1, 2; 3, 4], "bad_opt");

*/

DEFUN (rsf2csf, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{U}, @var{T}] =} rsf2csf (@var{UR}, @var{TR})
Convert a real, upper quasi-triangular Schur@tie{}form @var{TR} to a
complex, upper triangular Schur@tie{}form @var{T}.

Note that the following relations hold:

@tex
$UR \cdot TR \cdot {UR}^T = U T U^{\dagger}$ and
$U^{\dagger} U$ is the identity matrix I.
@end tex
@ifnottex
@tcode{@var{UR} * @var{TR} * @var{UR}' = @var{U} * @var{T} * @var{U}'} and
@code{@var{U}' * @var{U}} is the identity matrix I.
@end ifnottex

Note also that @var{U} and @var{T} are not unique.
@seealso{schur}
@end deftypefn */)
{
  if (args.length () != 2 || nargout > 2)
    print_usage ();

  if (! args(0).isnumeric ())
    err_wrong_type_arg ("rsf2csf", args(0));
  if (! args(1).isnumeric ())
    err_wrong_type_arg ("rsf2csf", args(1));
  if (args(0).iscomplex () || args(1).iscomplex ())
    error ("rsf2csf: UR and TR must be real matrices");

  if (args(0).is_single_type () || args(1).is_single_type ())
    {
      FloatMatrix u = args(0).float_matrix_value ();
      FloatMatrix t = args(1).float_matrix_value ();

      math::schur<FloatComplexMatrix> cs
        = math::rsf2csf<FloatComplexMatrix, FloatMatrix> (t, u);

      return ovl (cs.unitary_schur_matrix (), cs.schur_matrix ());
    }
  else
    {
      Matrix u = args(0).matrix_value ();
      Matrix t = args(1).matrix_value ();

      math::schur<ComplexMatrix> cs
        = math::rsf2csf<ComplexMatrix, Matrix> (t, u);

      return ovl (cs.unitary_schur_matrix (), cs.schur_matrix ());
    }
}

/*
%!test
%! A = [1, 1, 1, 2; 1, 2, 1, 1; 1, 1, 3, 1; -2, 1, 1, 1];
%! [u, t] = schur (A);
%! [U, T] = rsf2csf (u, t);
%! assert (norm (u * t * u' - U * T * U'), 0, 1e-12);
%! assert (norm (A - U * T * U'), 0, 1e-12);

%!test
%! A = rand (10);
%! [u, t] = schur (A);
%! [U, T] = rsf2csf (u, t);
%! assert (norm (tril (T, -1)), 0);
%! assert (norm (U * U'), 1, 1e-14);

%!test
%! A = [0, 1;-1, 0];
%! [u, t] = schur (A);
%! [U, T] = rsf2csf (u,t);
%! assert (U * T * U', A, 1e-14);
*/

OCTAVE_END_NAMESPACE(octave)
