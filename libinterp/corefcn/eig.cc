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

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

#include "EIG.h"
#include "fEIG.h"
#include "oct-string.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (eig, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{lambda} =} eig (@var{A})
@deftypefnx {} {@var{lambda} =} eig (@var{A}, @var{B})
@deftypefnx {} {[@var{V}, @var{lambda}] =} eig (@var{A})
@deftypefnx {} {[@var{V}, @var{lambda}] =} eig (@var{A}, @var{B})
@deftypefnx {} {[@var{V}, @var{lambda}, @var{W}] =} eig (@var{A})
@deftypefnx {} {[@var{V}, @var{lambda}, @var{W}] =} eig (@var{A}, @var{B})
@deftypefnx {} {[@dots{}] =} eig (@var{A}, @var{balanceOption})
@deftypefnx {} {[@dots{}] =} eig (@var{A}, @var{B}, @var{algorithm})
@deftypefnx {} {[@dots{}] =} eig (@dots{}, @var{eigvalOption})
Compute the eigenvalues (@var{lambda}) and optionally the right eigenvectors
(@var{V}) and the left eigenvectors (@var{W}) of a matrix or pair of matrices.

The flag @var{balanceOption} can be one of:

@table @asis
@item @qcode{"balance"} (default)
Preliminary balancing is on.

@item @qcode{"nobalance"}
Disables preliminary balancing.
@end table

The flag @var{eigvalOption} can be one of:

@table @asis
@item @qcode{"matrix"}
Return the eigenvalues in a diagonal matrix.  (default if 2 or 3 outputs
are requested)

@item @qcode{"vector"}
Return the eigenvalues in a column vector.  (default if only 1 output is
requested, e.g., @var{lambda} = eig (@var{A}))
@end table

The flag @var{algorithm} can be one of:

@table @asis
@item @qcode{"chol"}
Use the Cholesky factorization of B.  (default if @var{A} is symmetric
(Hermitian) and @var{B} is symmetric (Hermitian) positive definite)

@item @qcode{"qz"}
Use the QZ algorithm.  (used whenever @var{A} or @var{B} are not symmetric)
@end table

@multitable @columnfractions .31 .23 .23 .23
@headitem @tab no flag @tab chol @tab qz
@item both are symmetric
@tab @qcode{"chol"}
@tab @qcode{"chol"}
@tab @qcode{"qz"}
@item at least one is not symmetric
@tab @qcode{"qz"}
@tab @qcode{"qz"}
@tab @qcode{"qz"}
@end multitable

The eigenvalues returned by @code{eig} are not ordered.
@seealso{eigs, svd}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 4 || nargin == 0)
    print_usage ();

  octave_value_list retval;

  octave_value arg_a, arg_b;

  arg_a = args(0);

  if (arg_a.isempty ())
    return octave_value_list (2, Matrix ());

  if (! arg_a.isfloat ())
    err_wrong_type_arg ("eig", arg_a);

  if (arg_a.rows () != arg_a.columns ())
    err_square_matrix_required ("eig", "A");

  // determine if it's AEP or GEP
  bool AEPcase = nargin == 1 || args(1).is_string ();

  if (! AEPcase)
    {
      arg_b = args(1);

      if (arg_b.isempty ())
        return octave_value_list (2, Matrix ());

      if (! arg_b.isfloat ())
        err_wrong_type_arg ("eig", arg_b);

      if (arg_b.rows () != arg_b.columns ())
        err_square_matrix_required ("eig", "B");
    }

  bool qz_flag = false;
  bool chol_flag = false;
  bool balance_flag = false;
  bool no_balance_flag = false;
  bool matrix_flag = false;
  bool vector_flag = false;

  for (int i = (AEPcase ? 1 : 2); i < args.length (); ++i)
    {
      if (! args(i).is_string ())
        err_wrong_type_arg ("eig", args(i));

      std::string arg_i = args(i).string_value ();
      if (string::strcmpi (arg_i, "qz"))
        qz_flag = true;
      else if (string::strcmpi (arg_i, "chol"))
        chol_flag = true;
      else if (string::strcmpi (arg_i, "balance"))
        balance_flag = true;
      else if (string::strcmpi (arg_i, "nobalance"))
        no_balance_flag = true;
      else if (string::strcmpi (arg_i, "matrix"))
        matrix_flag = true;
      else if (string::strcmpi (arg_i, "vector"))
        vector_flag = true;
      else
        error (R"(eig: invalid option "%s")", arg_i.c_str ());
    }

  if (balance_flag && no_balance_flag)
    error (R"(eig: "balance" and "nobalance" options are mutually exclusive)");
  if (vector_flag && matrix_flag)
    error (R"(eig: "vector" and "matrix" options are mutually exclusive)");
  if (qz_flag && chol_flag)
    error (R"(eig: "qz" and "chol" options are mutually exclusive)");

  if (AEPcase)
    {
      if (qz_flag)
        error (R"(eig: invalid "qz" option for algebraic eigenvalue problem)");
      if (chol_flag)
        error (R"(eig: invalid "chol" option for algebraic eigenvalue problem)");
    }
  else
    {
      if (balance_flag)
        error (R"(eig: invalid "balance" option for generalized eigenvalue problem)");
      if (no_balance_flag)
        error (R"(eig: invalid "nobalance" option for generalized eigenvalue problem)");
    }

  // Default is to balance
  const bool balance = (no_balance_flag ? false : true);
  const bool force_qz = qz_flag;


  Matrix tmp_a, tmp_b;
  ComplexMatrix ctmp_a, ctmp_b;
  FloatMatrix ftmp_a, ftmp_b;
  FloatComplexMatrix fctmp_a, fctmp_b;

  if (arg_a.is_single_type ())
    {
      FloatEIG result;
      if (AEPcase)
        {
          if (arg_a.isreal ())
            {
              ftmp_a = arg_a.float_matrix_value ();

              result = FloatEIG (ftmp_a, nargout > 1, nargout > 2, balance);
            }
          else
            {
              fctmp_a = arg_a.float_complex_matrix_value ();

              result = FloatEIG (fctmp_a, nargout > 1, nargout > 2, balance);
            }
        }
      else
        {
          if (arg_a.isreal () && arg_b.isreal ())
            {
              ftmp_a = arg_a.float_matrix_value ();
              ftmp_b = arg_b.float_matrix_value ();

              result = FloatEIG (ftmp_a, ftmp_b, nargout > 1, nargout > 2,
                                 force_qz);
            }
          else
            {
              fctmp_a = arg_a.float_complex_matrix_value ();
              fctmp_b = arg_b.float_complex_matrix_value ();

              result = FloatEIG (fctmp_a, fctmp_b, nargout > 1, nargout > 2,
                                 force_qz);
            }
        }

      if (nargout == 0 || nargout == 1)
        {
          if (matrix_flag)
            retval = ovl (FloatComplexDiagMatrix (result.eigenvalues ()));
          else
            retval = ovl (result.eigenvalues ());
        }
      else if (nargout == 2)
        {
          if (vector_flag)
            retval = ovl (result.right_eigenvectors (), result.eigenvalues ());
          else
            retval = ovl (result.right_eigenvectors (),
                          FloatComplexDiagMatrix (result.eigenvalues ()));
        }
      else
        {
          if (vector_flag)
            retval = ovl (result.right_eigenvectors (),
                          result.eigenvalues (),
                          result.left_eigenvectors ());
          else
            retval = ovl (result.right_eigenvectors (),
                          FloatComplexDiagMatrix (result.eigenvalues ()),
                          result.left_eigenvectors ());
        }
    }
  else
    {
      EIG result;

      if (AEPcase)
        {
          if (arg_a.isreal ())
            {
              tmp_a = arg_a.matrix_value ();

              result = EIG (tmp_a, nargout > 1, nargout > 2, balance);
            }
          else
            {
              ctmp_a = arg_a.complex_matrix_value ();

              result = EIG (ctmp_a, nargout > 1, nargout > 2, balance);
            }
        }
      else
        {
          if (arg_a.isreal () && arg_b.isreal ())
            {
              tmp_a = arg_a.matrix_value ();
              tmp_b = arg_b.matrix_value ();

              result = EIG (tmp_a, tmp_b, nargout > 1, nargout > 2, force_qz);
            }
          else
            {
              ctmp_a = arg_a.complex_matrix_value ();
              ctmp_b = arg_b.complex_matrix_value ();

              result = EIG (ctmp_a, ctmp_b, nargout > 1, nargout > 2, force_qz);
            }
        }

      if (nargout == 0 || nargout == 1)
        {
          if (matrix_flag)
            retval = ovl (ComplexDiagMatrix (result.eigenvalues ()));
          else
            retval = ovl (result.eigenvalues ());
        }
      else if (nargout == 2)
        {
          if (vector_flag)
            retval = ovl (result.right_eigenvectors (), result.eigenvalues ());
          else
            retval = ovl (result.right_eigenvectors (),
                          ComplexDiagMatrix (result.eigenvalues ()));
        }
      else
        {
          if (vector_flag)
            retval = ovl (result.right_eigenvectors (),
                          result.eigenvalues (),
                          result.left_eigenvectors ());
          else
            retval = ovl (result.right_eigenvectors (),
                          ComplexDiagMatrix (result.eigenvalues ()),
                          result.left_eigenvectors ());
        }
    }

  return retval;
}

/*
%!assert (eig ([1, 2; 2, 1]), [-1; 3], sqrt (eps))

%!test
%! [v, d] = eig ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert (d, [-1, 0; 0, 3], sqrt (eps))
%! assert (v, [-x, x; x, x], sqrt (eps))

%!test
%! [v, d, w] = eig ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert (w, [-x, x; x, x], sqrt (eps))

%!test
%! [v, d] = eig ([1, 2; 2, 1], "balance");
%! x = 1 / sqrt (2);
%! assert (d, [-1, 0; 0, 3], sqrt (eps))
%! assert (v, [-x, x; x, x], sqrt (eps))

%!test
%! [v, d, w] = eig ([1, 2; 2, 1], "balance");
%! x = 1 / sqrt (2);
%! assert (w, [-x, x; x, x], sqrt (eps));

%!assert (eig (single ([1, 2; 2, 1])), single ([-1; 3]), sqrt (eps ("single")))

%!assert (eig (single ([1, 2; 2, 1]), "balance"),
%!        single ([-1; 3]), sqrt (eps ("single")))

%!test
%! [v, d] = eig (single ([1, 2; 2, 1]));
%! x = single (1 / sqrt (2));
%! assert (d, single ([-1, 0; 0, 3]), sqrt (eps ("single")))
%! assert (v, [-x, x; x, x], sqrt (eps ("single")))

%!test
%! [v, d, w] = eig (single ([1, 2; 2, 1]));
%! x = single (1 / sqrt (2));
%! assert (w, [-x, x; x, x], sqrt (eps ("single")))

%!test
%! [v, d] = eig (single ([1, 2; 2, 1]), "balance");
%! x = single (1 / sqrt (2));
%! assert (d, single ([-1, 0; 0, 3]), sqrt (eps ("single")));
%! assert (v, [-x, x; x, x], sqrt (eps ("single")))

%!test
%! [v, d, w] = eig (single ([1, 2; 2, 1]), "balance");
%! x = single (1 / sqrt (2));
%! assert (w, [-x, x; x, x], sqrt (eps ("single")))


## If (at least one of) the matrices are non-symmetric,
## regardless the algorithm flag the qz algorithm should be used.
## So the results without algorithm flag, with "qz" and with "chol"
## should be the same.
%!function nonsym_chol_2_output (A, B, res = sqrt (eps))
%!  [v, d] = eig (A, B);
%!  [v2, d2] = eig (A, B, "qz");
%!  [v3, d3] = eig (A, B, "chol");
%!  assert (A * v(:, 1), d(1, 1) * B * v(:, 1), res)
%!  assert (A * v(:, 2), d(2, 2) * B * v(:, 2), res)
%!  assert (v, v2)
%!  assert (v, v3)
%!  assert (d, d2)
%!  assert (d, d3)
%!endfunction

%!test nonsym_chol_2_output ([1, 2; -1, 1], [3, 3; 1, 2])
%!test nonsym_chol_2_output ([1+3i, 2+3i; 3-8i, 8+3i], [8+i, 3+i; 4-9i, 3+i])
%!test nonsym_chol_2_output ([1, 2; 3, 8], [8, 3; 4, 3])

%!test nonsym_chol_2_output (single ([1, 2; -1, 1]),
%!                           single ([3, 3; 1, 2]), sqrt (eps ("single")))
%!test nonsym_chol_2_output (single ([1+3i, 2+3i; 3-8i, 8+3i]),
%!                           single ([8+i, 3+i; 4-9i, 3+i]),
%!                           sqrt (eps ("single")))

%!function nonsym_chol_3_output (A, B, res = sqrt (eps))
%!  [v, d, w] = eig (A, B);
%!  [v2, d2, w2] = eig (A, B, "qz");
%!  [v3, d3, w3] = eig (A, B, "chol");
%!  wt = w';
%!  assert (wt(1, :)* A, d(1, 1) * wt(1, :) * B, res)
%!  assert (wt(2, :)* A, d(2, 2) * wt(2, :) * B, res)
%!  assert (v, v2)
%!  assert (v, v3)
%!  assert (d, d2)
%!  assert (d, d3)
%!  assert (w, w2)
%!  assert (w, w3)
%!endfunction

%!test nonsym_chol_3_output ([1, 2; -1, 1], [3, 3; 1, 2])
%!test nonsym_chol_3_output ([1+3i, 2+3i; 3-8i, 8+3i], [8+i, 3+i; 4-9i, 3+i])
%!test nonsym_chol_3_output ([1, 2; 3, 8], [8, 3; 4, 3])

%!test nonsym_chol_3_output (single ([1, 2; -1, 1]),
%!                           single ([3, 3; 1, 2]), sqrt (eps ("single")))
%!test nonsym_chol_3_output (single ([1+3i, 2+3i; 3-8i, 8+3i]),
%!                           single ([8+i, 3+i; 4-9i, 3+i]),
%!                           sqrt (eps ("single")))

## If the matrices are symmetric,
## then the chol method is default.
## So the results without algorithm flag and with "chol" should be the same.
%!function sym_chol_2_input (A, B, res = sqrt (eps))
%!  [v, d] = eig (A, B);
%!  [v2, d2] = eig (A, B, "chol");
%!  assert (A * v(:, 1), d(1, 1) * B * v(:, 1), res)
%!  assert (A * v(:, 2), d(2, 2) * B * v(:, 2), res)
%!  assert (v, v2)
%!  assert (d, d2)
%!endfunction

%!test sym_chol_2_input ([1, 2; 2, 1], [3, -2; -2, 3])
%!test sym_chol_2_input ([1+3i, 2+i; 2-i, 1+3i], [5+9i, 2+i; 2-i, 5+9i])
%!test sym_chol_2_input ([1, 1+i; 1-i, 1], [2, 0; 0, 2])

%!test sym_chol_2_input (single ([1, 2; 2, 1]), single ([3, -2; -2, 3]),
%!                       sqrt (eps ("single")))
%!test sym_chol_2_input (single ([1+3i, 2+i; 2-i, 1+3i]),
%!                       single ([5+9i, 2+i; 2-i, 5+9i]), sqrt (eps ("single")))
%!test sym_chol_2_input (single ([1, 1+i; 1-i, 1]), single ([2, 0; 0, 2]),
%!                       sqrt (eps ("single")))

%!function sym_chol_3_input (A, B, res = sqrt (eps))
%!  [v, d, w] = eig (A, B);
%!  [v2, d2, w2] = eig (A, B, "chol");
%!  wt = w';
%!  assert (wt(1, :)* A, d(1, 1) * wt(1, :) * B, res)
%!  assert (wt(2, :)* A, d(2, 2) * wt(2, :) * B, res)
%!  assert (v, v2)
%!  assert (d, d2)
%!  assert (w, w2)
%!endfunction

%!test sym_chol_3_input ([1, 2; 2, 1], [3, -2; -2, 3])
%!test sym_chol_3_input ([1+3i, 2+i; 2-i, 1+3i], [5+9i, 2+i; 2-i, 5+9i])
%!test sym_chol_3_input ([1, 1+i; 1-i, 1], [2, 0; 0, 2])

%!test sym_chol_3_input (single ([1, 2; 2, 1]), single ([3, -2; -2, 3]),
%!                       sqrt (eps ("single")))
%!test sym_chol_3_input (single ([1+3i, 2+i; 2-i, 1+3i]),
%!                       single ([5+9i, 2+i; 2-i, 5+9i]), sqrt (eps ("single")))
%!test sym_chol_3_input (single ([1, 1+i; 1-i, 1]), single ([2, 0; 0, 2]),
%!                       sqrt (eps ("single")))

## "balance" is always default
## so the results with and without "balance" should be the same
## while in this case "nobalance" should produce different result
%!test
%! A = [3 -2 -0.9 0; -2 4 1 -0; -0 0 -1 0; -0.5 -0.5 0.1 1];
%! [V1, D1] = eig (A);
%! [V2, D2] = eig (A, "balance");
%! [V3, D3] = eig (A, "nobalance");
%! assert (V1, V2)
%! assert (D1, D2)
%! assert (isequal (V2, V3), false)

## Testing the flags in all combination.
## If 2 flags are on, than the result should be the same regardless
## of the flags order.
## option1 represents the first order while option2 represents the other order.
## d and d2 should be a diagonal matrix if "matrix" flag is on while
## these should be column vectors if the "vector" flag is on.
%!function test_eig_args (args, options1, options2, testd = @() true)
%!  [v, d, w] = eig (args{:}, options1{:});
%!  [v2, d2, w2] = eig (args{:}, options2{:});
%!  assert (testd (d))
%!  assert (testd (d2))
%!  assert (v, v2)
%!  assert (d, d2)
%!  assert (w, w2)
%!endfunction

%!function qz_chol_with_shapes (A, B)
%!  for shapes = struct ("name", {"vector", "matrix"},
%!                       "test", {@isvector, @isdiag})
%!    test_eig_args ({A, B}, {"qz", shapes.name},
%!                   {shapes.name, "qz"}, shapes.test);
%!    test_eig_args ({A, B}, {"chol", shapes.name},
%!                   {shapes.name, "chol"}, shapes.test);
%!  endfor
%!endfunction

%!function balance_nobalance_with_shapes (A)
%!  for shapes = struct ("name", {"vector", "matrix"},
%!                       "test", {@isvector, @isdiag})
%!    test_eig_args ({A}, {"balance", shapes.name},
%!                   {shapes.name, "balance"}, shapes.test);
%!    test_eig_args ({A}, {"nobalance", shapes.name},
%!                   {shapes.name, "nobalance"}, shapes.test);
%!  endfor
%!endfunction

## Default return format:
## diagonal matrix if 2 or 3 outputs are specified
## column vector if 1 output is specified
%!function test_shapes (args)
%!  d = eig (args{:});
%!  assert (isvector (d))
%!  d2 = eig (args{:}, "vector");
%!  assert (isvector (d2))
%!  [v, d3] = eig (args{:});
%!  assert (isdiag (d3))
%!  d4 = eig (args{:}, "matrix");
%!  assert (isdiag (d4))
%!  [v, d5, w] = eig (args{:});
%!  assert (isdiag (d5))
%!  d6 = eig (args{:}, "matrix");
%!  assert (isdiag (d6))
%!  assert (d, d2)
%!  assert (d3, d4)
%!  assert (d5, d6)
%!  assert (d, diag (d3))
%!  assert (d, diag (d5))
%!endfunction

%!function shapes_AEP (A)
%!  test_shapes({A});
%!endfunction

%!function shapes_GEP (A, B)
%!  test_shapes({A, B});
%!endfunction

%!test balance_nobalance_with_shapes ([1, 2; 2, 1]);
%!test balance_nobalance_with_shapes (single ([1, 2; 2, 1]));

%!test shapes_AEP ([1, 2; 2, 1]);
%!test shapes_AEP (single ([1, 2; 2, 1]));

%!test qz_chol_with_shapes ([1, 1+i; 1-i, 1], [2, 0; 0, 2]);
%!test qz_chol_with_shapes ([1, 2; 3, 8], [8, 3; 4, 3]);
%!test qz_chol_with_shapes ([1, 2; -1, 1], [3, 3; 1, 2]);

%!test qz_chol_with_shapes (single ([1, 1+i; 1-i, 1]),  single ([2, 0; 0, 2]));
%!test qz_chol_with_shapes (single ([1, 2; 3, 8]),  single ([8, 3; 4, 3]));
%!test qz_chol_with_shapes (single ([1, 2; -1, 1]),  single ([3, 3; 1, 2]));

%!test shapes_GEP ([1, 1+i; 1-i, 1], [2, 0; 0, 2]);
%!test shapes_GEP ([1, 2; 3, 8], [8, 3; 4, 3]);
%!test shapes_GEP ([1, 2; -1, 1], [3, 3; 1, 2]);

%!test shapes_GEP (single ([1, 1+i; 1-i, 1]),  single ([2, 0; 0, 2]));
%!test shapes_GEP (single ([1, 2; 3, 8]),  single ([8, 3; 4, 3]));
%!test shapes_GEP (single ([1, 2; -1, 1]),  single ([3, 3; 1, 2]));

## Check if correct default method is used for symmetric input
%!function chol_qz_accuracy (A, B, is_qz_accurate, is_chol_accurate)
%!  [V1, D1] = eig (A, B, 'qz');
%!  [V2, D2] = eig (A, B); #default is chol
%!  assert (isequal (A*V1, A*V1*D1), is_qz_accurate)
%!  assert (isequal (A*V2, A*V2*D2), is_chol_accurate)
%!endfunction
%!test
%! minij_100 = gallery ('minij', 100);
%! chol_qz_accuracy (minij_100, minij_100, false, true);
%! moler_100 = gallery ('moler', 100);
%! chol_qz_accuracy (moler_100, moler_100, false, true);
%! A = diag([1e-16, 1e-15]);
%! chol_qz_accuracy (A, A, true, false);

%!error eig ()
%!error eig (false)
%!error eig ([1, 2; 3, 4], [4, 3; 2, 1], 1)

%!error <EIG requires same size matrices>
%!  eig ([1, 2; 3, 4], 2)
%!error <must be a square matrix>
%! eig ([1, 2; 3, 4; 5, 6])
%!error <wrong type argument>
%!  eig ("abcd")
%!error <invalid option "abcd">
%!  eig ([1 2 ; 2 3], "abcd")
%!error <invalid "chol" option for algebraic eigenvalue problem>
%!  eig ([1 2 ; 2 3], "chol")
%!error <invalid "qz" option for algebraic eigenvalue problem>
%!  eig ([1 2 ; 2 3], "qz")
%!error <wrong type argument>
%!  eig (false, [1 2 ; 2 3])
%!error <invalid option "abcd">
%!  eig ([1 2 ; 2 3], [1 2 ; 2 3], "abcd")
%!error <invalid "qz" option for algebraic eigenvalue problem>
%!  eig ([1 2 ; 2 3], "balance", "qz")
%!error <invalid option "abcd">
%!  eig ([1 2 ; 2 3], [1 2 ; 2 3], "vector", "abcd")
%!error <invalid option "abcd">
%!  eig ([1 2 ; 2 3], "balance", "matrix", "abcd")
%!error <"balance" and "nobalance" options are mutually exclusive>
%!  eig ([1 2 ; 2 3], "balance", "nobalance")
%!error <"balance" and "nobalance" options are mutually exclusive>
%!  eig ([1 2 ; 2 3], "nobalance", "balance")
%!error <"vector" and "matrix" options are mutually exclusive>
%!  eig ([1 2 ; 2 3], "matrix", "vector")
%!error <"vector" and "matrix" options are mutually exclusive>
%!  eig ([1 2 ; 2 3], "vector", "matrix")
%!error <"vector" and "matrix" options are mutually exclusive>
%!  eig ([1 2 ; 2 3], [1 2 ; 2 3], "matrix", "vector")
%!error <"vector" and "matrix" options are mutually exclusive>
%!  eig ([1 2 ; 2 3], [1 2 ; 2 3], "vector", "matrix")
%!error <wrong type argument>
%!  eig ([1 2 ; 2 3], [1 2 ; 2 3], false)
%!error <wrong type argument>
%!  eig ([1 2 ; 2 3], [1 2 ; 2 3], [1 2 ; 2 3])
*/

OCTAVE_END_NAMESPACE(octave)
