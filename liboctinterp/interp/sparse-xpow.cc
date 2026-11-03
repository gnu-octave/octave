////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2026 The Octave Project Developers
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

#include <limits>

#include "Array-util.h"
#include "oct-cmplx.h"
#include "quit.h"

#include "error.h"
#include "ovl.h"
#include "utils.h"

#include "dSparse.h"
#include "CSparse.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "sparse-xpow.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static inline bool
xisint (double x)
{
  // FIXME: If we return *only* octave::math::is_integer (x)
  // and delete the lines with numeric_limits<int>,
  // then that works on MacOS but not on Linux.
  // It fails a test in sparse.tst that requires this calculation:
  // Complex (realmin, realmin) .^ realmax.
  // On MacOS it correctly gives zero, matching Matlab.
  // On Linux it gives Inf + Nani.
  // If we can make both platforms work without the extra check with
  // numeric_limits<int>, then we should do so.
  return (octave::math::is_integer (x)
          && x <= std::numeric_limits<int>::max ()
          && x >= std::numeric_limits<int>::min ());
}

/*
## The int limits are integers, as for full matrices
%!assert (full (sparse ([-2, 0, 1]) .^ 2147483647), [-2, 0, 1] .^ 2147483647)
%!assert (full (sparse ([-2, 0, 1]) .^ -2147483648), [-2, 0, 1] .^ -2147483648)
*/

// Safer pow functions.  Only two make sense for sparse matrices, the
// others should all promote to full matrices.

template<typename T>
static octave_value
xpow_private (const T& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return T ();

  // If we are here, A is not empty ==> A needs to be square.
  if (nr != nc)
    error ("for A^b, A must be a square matrix.  Use .^ for elementwise power.");

  if (! xisint (b))
    error ("use full(a) ^ full(b)");

  int btmp = static_cast<int> (b);
  if (btmp == 0)
    {
      // Return sparse identity matrix, so it's always type SparseMatrix
      // and does not have to be templated by typename T.
      SparseMatrix tmp = SparseMatrix (nr, nr, nr);
      for (octave_idx_type i = 0; i < nr; i++)
        {
          tmp.data (i) = 1.0;
          tmp.ridx (i) = i;
        }
      for (octave_idx_type i = 0; i < nr + 1; i++)
        tmp.cidx (i) = i;

      retval = tmp;
    }
  else
    {
      T atmp;
      if (btmp < 0)
        {
          btmp = -btmp;

          octave_idx_type info;
          double rcond = 0.0;
          MatrixType mattyp (a);

          // FIXME: This causes an error if the input sparse matrix is all-zeros.
          // That behavior is inconsistent with A ^ b when A is a full all-zeros
          // matrix, which just returns Inf of the same size with a warning.
          atmp = a.inverse (mattyp, info, rcond, 1);

          if (info == -1)
            warning ("inverse: matrix singular to machine precision, rcond = %g", rcond);
        }
      else
        atmp = a;

      if (atmp.nnz () == 0)  // Fast return for all-zeros matrix
        return atmp;

      T result (atmp);

      btmp--;

      // There are two approaches to the actual exponentiation.
      // Exponentiation by squaring uses only a logarithmic number
      // of multiplications but the matrices it multiplies tend to be dense
      // towards the end.
      // Linear multiplication uses a linear number of multiplications
      // but one of the matrices it uses will be as sparse as the original
      // matrix.
      //
      // The time to multiply fixed-size matrices is strongly affected by their
      // sparsity.  Denser matrices take much longer to multiply together.
      // See this URL for a worked-through example:
      // https://octave.discourse.group/t/3216/4
      //
      // The tradeoff is between many fast multiplications or a few slow ones.
      //
      // Large exponents favor the squaring technique, and sparse matrices
      // favor linear multiplication.
      //
      // We calculate a threshold based on the sparsity of the input
      // and use squaring for exponents larger than that.
      //
      // FIXME: Improve this threshold calculation.

      uint64_t sparsity = atmp.numel () / atmp.nnz (); // reciprocal of density
      int threshold = (sparsity >= 1000) ? 40
                      : (sparsity >=  100) ? 20
                      : 3;

      if (btmp > threshold) // use squaring technique
        {
          while (btmp > 0)
            {
              if (btmp & 1)
                result = result * atmp;

              btmp >>= 1;

              if (btmp > 0)
                atmp = atmp * atmp;
            }
        }
      else // use linear multiplication
        {
          for (int i = 0; i < btmp; i++)
            result = result * atmp;
        }

      retval = result;
    }

  return retval;
}

octave_value
xpow (const SparseMatrix& a, double b)
{
  return xpow_private<SparseMatrix> (a, b);
}

octave_value
xpow (const SparseComplexMatrix& a, double b)
{
  return xpow_private<SparseComplexMatrix> (a, b);
}

// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things
// like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// Handle special case of scalar-sparse-matrix .^ sparse-matrix.
// Forwarding to the scalar elem_xpow function and then converting the
// result back to a sparse matrix is a bit wasteful but it does not
// seem worth the effort to optimize -- how often does this case come up
// in practice?

template <typename S, typename SM>
inline octave_value
scalar_xpow (const S& a, const SM& b)
{
  octave_value val = elem_xpow (a, b);

  if (val.iscomplex ())
    return SparseComplexMatrix (val.complex_matrix_value ());
  else
    return SparseMatrix (val.matrix_value ());
}

/*
%!assert (sparse (2) .^ [3, 4], sparse ([8, 16]))
%!assert <47775> (sparse (2i) .^ [3, 4], sparse ([-0-8i, 16]))

%!test <*63080>
%! Z = sparse ([]);
%! A = sparse (zeros (0, 2));
%! B = sparse (zeros (2, 0));
%! assert (Z ^  1, Z);
%! assert (Z ^  0, Z);
%! assert (Z ^ -1, Z);
%! assert (A ^  1, Z);
%! assert (A ^  0, Z);
%! assert (A ^ -1, Z);
%! assert (B ^  1, Z);
%! assert (B ^  0, Z);
%! assert (B ^ -1, Z);

%!test <*63080>
%! A = sparse (zeros (2, 2));
%! assert (A ^  1, A);
%! assert (A ^  0, sparse (eye (2, 2)));

%!test <63080>
%! A = sparse (zeros (2, 2));
%! assert (A ^ -1, sparse (inf (2, 2)));

*/

// Raise each element of the sparse matrix A to a scalar power.  FCN computes
// one element exactly as the full operator does, so that the result equals
// that of full (A).  The implicit zeros of A all give FCN (0): if that is zero,
// the result keeps the pattern of A, otherwise it is a full matrix of type FRT.

template <typename SRT, typename FRT, typename ST, typename F>
static octave_value
sparse_scalar_pow (const ST& a, F fcn)
{
  typedef typename ST::element_type A_T;

  const auto z = fcn (A_T ());

  if (z == 0.0)
    {
      SRT result (a);

      octave_idx_type nz = a.nnz ();
      auto *rd = result.rwdata ();

      for (octave_idx_type i = 0; i < nz; i++)
        {
          octave_quit ();
          rd[i] = fcn (a.data (i));
        }

      result.maybe_compress (true);

      return result;
    }
  else
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.cols ();

      FRT result (nr, nc, z);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
          {
            octave_quit ();
            result.xelem (a.ridx (i), j) = fcn (a.data (i));
          }

      return result;
    }
}

// Raise each element of the sparse matrix A to the power of the corresponding
// element of the sparse matrix B, of the same size.  FCN computes one element
// exactly as the full operator does.  Every element where A and B both hold an
// implicit zero gives FCN (0, 0), which is 1 or NaN, never zero, so the result
// starts filled with it and element (i, j) is at i + j*nr of its data.  The
// nonzeros of A and B are then walked together, since an element held by only
// one of them gives FCN (a, 0) or FCN (0, b).

template <typename RT, typename TA, typename TB, typename F>
static RT
sparse_sparse_pow (const TA& a, const TB& b, F fcn)
{
  typedef typename TA::element_type A_T;
  typedef typename TB::element_type B_T;

  const A_T za = A_T ();
  const B_T zb = B_T ();

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  RT result (nr, nc, fcn (za, zb));
  auto *rd = result.rwdata ();

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_idx_type ia = a.cidx (j);
      octave_idx_type ea = a.cidx (j+1);
      octave_idx_type ib = b.cidx (j);
      octave_idx_type eb = b.cidx (j+1);

      while (ia < ea || ib < eb)
        {
          octave_quit ();

          if (ib == eb || (ia < ea && a.ridx (ia) < b.ridx (ib)))
            {
              rd[a.ridx (ia) + j*nr] = fcn (a.data (ia), zb);
              ia++;
            }
          else if (ia == ea || b.ridx (ib) < a.ridx (ia))
            {
              rd[b.ridx (ib) + j*nr] = fcn (za, b.data (ib));
              ib++;
            }
          else
            {
              rd[a.ridx (ia) + j*nr] = fcn (a.data (ia), b.data (ib));
              ia++;
              ib++;
            }
        }
    }

  result.maybe_compress (true);

  return result;
}

// -*- 1 -*-
octave_value
elem_xpow (double a, const SparseMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  double d1, d2;

  if (a < 0.0 && ! b.all_integers (d1, d2))
    {
      Complex atmp (a);
      ComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = std::pow (atmp, b(i, j));
            }
        }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = std::pow (a, b(i, j));
            }
        }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  // Each element as in elem_xpow (double, const ComplexNDArray&).

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          result(i, j) = std::pow (a, b(i, j));
        }
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const SparseMatrix& a, double b)
{
  // Each element as in elem_xpow (const NDArray&, double).

  if (xisint (b))
    {
      int bint = static_cast<int> (b);

      if (bint == 2)
        return sparse_scalar_pow<SparseMatrix, Matrix>
                 (a, [] (double x) { return x * x; });
      else if (bint == 3)
        return sparse_scalar_pow<SparseMatrix, Matrix>
                 (a, [] (double x) { return x * x * x; });
      else if (bint == -1)
        return sparse_scalar_pow<SparseMatrix, Matrix>
                 (a, [] (double x) { return 1.0 / x; });
      else
        return sparse_scalar_pow<SparseMatrix, Matrix>
                 (a, [bint] (double x) { return std::pow (x, bint); });
    }
  else if (a.any_element_is_negative ())
    return sparse_scalar_pow<SparseComplexMatrix, ComplexMatrix>
             (a, [b] (double x) { return std::pow (Complex (x), b); });
  else
    return sparse_scalar_pow<SparseMatrix, Matrix>
             (a, [b] (double x) { return std::pow (x, b); });
}

// -*- 4 -*-
octave_value
elem_xpow (const SparseMatrix& a, const SparseMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (b.numel () == 1 && a.numel () > 1)
    return elem_xpow (a, b(0));

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  int convert_to_complex = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
      {
        if (a.data(i) < 0.0)
          {
            double btmp = b (a.ridx (i), j);
            if (! xisint (btmp))
              {
                convert_to_complex = 1;
                goto done;
              }
          }
      }

done:

  // Each element as in elem_xpow (const NDArray&, const NDArray&).

  if (convert_to_complex)
    return sparse_sparse_pow<SparseComplexMatrix>
             (a, b, [] (double x, double y)
                    { return std::pow (Complex (x), y); });
  else
    return sparse_sparse_pow<SparseMatrix>
             (a, b, [] (double x, double y) { return std::pow (x, y); });
}

// -*- 5 -*-
octave_value
elem_xpow (const SparseMatrix& a, const Complex& b)
{
  // Each element as in elem_xpow (const NDArray&, const Complex&).

  return sparse_scalar_pow<SparseComplexMatrix, ComplexMatrix>
           (a, [&b] (double x) { return std::pow (x, b); });
}

// -*- 6 -*-
octave_value
elem_xpow (const SparseMatrix& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (b.numel () == 1 && a.numel () > 1)
    return elem_xpow (a, b(0));

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  // Each element as in elem_xpow (const NDArray&, const ComplexNDArray&).

  return sparse_sparse_pow<SparseComplexMatrix>
           (a, b, [] (double x, const Complex& y) { return std::pow (x, y); });
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          double btmp = b (i, j);
          if (xisint (btmp))
            result (i, j) = std::pow (a, static_cast<int> (btmp));
          else
            result (i, j) = std::pow (a, btmp);
        }
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a, b (i, j));
      }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, double b)
{
  // Each element as in elem_xpow (const ComplexNDArray&, double).

  if (xisint (b))
    {
      int bint = static_cast<int> (b);

      if (bint == -1)
        return sparse_scalar_pow<SparseComplexMatrix, ComplexMatrix>
                 (a, [] (const Complex& x) { return 1.0 / x; });
      else
        return sparse_scalar_pow<SparseComplexMatrix, ComplexMatrix>
                 (a, [bint] (const Complex& x) { return std::pow (x, bint); });
    }
  else
    return sparse_scalar_pow<SparseComplexMatrix, ComplexMatrix>
             (a, [b] (const Complex& x) { return std::pow (x, b); });
}

// -*- 10 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (b.numel () == 1 && a.numel () > 1)
    return elem_xpow (a, b(0));

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  // Each element as in elem_xpow (const ComplexNDArray&, const NDArray&).

  return sparse_sparse_pow<SparseComplexMatrix>
           (a, b, [] (const Complex& x, double y)
                  {
                    if (xisint (y))
                      return std::pow (x, static_cast<int> (y));
                    else
                      return std::pow (x, y);
                  });
}

// -*- 11 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const Complex& b)
{
  // Each element as in elem_xpow (const ComplexNDArray&, const Complex&).

  return sparse_scalar_pow<SparseComplexMatrix, ComplexMatrix>
           (a, [&b] (const Complex& x) { return std::pow (x, b); });
}

// -*- 12 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (b.numel () == 1 && a.numel () > 1)
    return elem_xpow (a, b(0));

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  // Each element as in elem_xpow (const ComplexNDArray&,
  // const ComplexNDArray&).

  return sparse_sparse_pow<SparseComplexMatrix>
           (a, b, [] (const Complex& x, const Complex& y)
                  { return std::pow (x, y); });
}

/*
## Elementwise power of sparse operands equals that of full operands
%!test <*57909>
%! a = sparse (4, 6);
%! b = sparse (4, 6);
%! a(2,2) = 1;
%! b(3,3) = 2;
%! a(3,4) = 3;
%! b(2,4) = 4;
%! a(2,5) = 5;
%! b(3,5) = 6;
%! a(4,6) = 7;
%! b(4,6) = 8;
%! assert (full (a .^ b), full (a) .^ full (b));

%!assert <*60846> (full (sparse ([0, 1; 0, 2]) .^ sparse ([0, 0; 1, 2])),
%!                 [0, 1; 0, 2] .^ [0, 0; 1, 2])
%!assert <*60846> (full (sparse (0) .^ sparse (1)), 0)

## A 1x1 sparse exponent acts as a scalar
%!assert (sparse ([1, 2, 0]) .^ sparse (2), sparse ([1, 4, 0]))
%!assert (full (sparse ([1, 2, 0]) .^ sparse (-1)), [1, 2, 0] .^ -1)
%!assert (full (sparse ([1, 2, 0]) .^ sparse (2i)), [1, 2, 0] .^ 2i)
%!assert (full (sparse ([1i, 2, 0]) .^ sparse (0.5)), [1i, 2, 0] .^ 0.5)
%!assert (full (sparse ([1i, 2, 0]) .^ sparse (1+1i)), [1i, 2, 0] .^ (1+1i))

%!shared ar, ac, er, ec
%! ar = [0, 2, -2, 0.5; 0, 0, 1, 0];
%! ac = [0, 1i, -1+1i, 2-1i; 0, 0, 1i, 0];
%! er = [2, -1, 0.5, -0.5; NaN, Inf, -Inf, 0];
%! ec = [2+1i, -1+1i, 1i, -1i; 0.5i, -0.5-2i, 0, 1];
%!assert <*57909> (full (sparse (ar) .^ sparse (er)), ar .^ er)
%!assert <*57909> (full (sparse (abs (ar)) .^ sparse (er)), abs (ar) .^ er)
%!assert <*57909> (full (sparse (ar) .^ sparse (ec)), ar .^ ec)
%!assert <*57909> (full (sparse (ac) .^ sparse (er)), ac .^ er)
%!assert <*57909> (full (sparse (ac) .^ sparse (ec)), ac .^ ec)
%!assert <*57909> (full (sparse (ar) .^ er), ar .^ er)
%!assert <*57909> (full (ar .^ sparse (er)), ar .^ er)
%!assert <*57909> (full (sparse (ar) .^ 0.5), ar .^ 0.5)
%!assert <*57909> (full (sparse (ar) .^ -0.5), ar .^ -0.5)
%!assert <*57909> (full (sparse (ar) .^ Inf), ar .^ Inf)
%!assert <*57909> (full (sparse (ar) .^ NaN), ar .^ NaN)
%!assert <*57909> (full (sparse (abs (ar)) .^ NaN), abs (ar) .^ NaN)
%!assert <*57909> (full (sparse (ar) .^ 1i), ar .^ 1i)
%!assert <*57909> (full (sparse (ar) .^ (-1+1i)), ar .^ (-1+1i))
%!assert <*57909> (full (sparse (ac) .^ -1), ac .^ -1)
%!assert <*57909> (full (sparse (ac) .^ -0.5), ac .^ -0.5)
%!assert <*57909> (full (sparse (ac) .^ NaN), ac .^ NaN)
%!assert <*57909> (full (sparse (ac) .^ 1i), ac .^ 1i)
%!assert <*57909> (full (0.5 .^ sparse (ec)), 0.5 .^ ec)

## The result keeps the pattern of A only where 0 .^ B is zero
%!assert (sparse ([0, 2]) .^ 2, sparse ([0, 4]))
%!assert (sparse ([0, 2]) .^ (2+1i), sparse ([0, 2] .^ (2+1i)))
%!assert <*57909> (sparse ([0, 2]) .^ NaN, [NaN, NaN])
%!assert (sparse ([0, 2]) .^ -1, [Inf, 0.5])
*/


OCTAVE_END_NAMESPACE(octave)
