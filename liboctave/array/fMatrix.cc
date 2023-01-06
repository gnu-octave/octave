////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#include <algorithm>
#include <istream>
#include <limits>
#include <ostream>

#include "Array-util.h"
#include "DET.h"
#include "PermMatrix.h"
#include "boolMatrix.h"
#include "byte-swap.h"
#include "chMatrix.h"
#include "chol.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fColVector.h"
#include "fDiagMatrix.h"
#include "fMatrix.h"
#include "fMatrix.h"
#include "fNDArray.h"
#include "fRowVector.h"
#include "lo-blas-proto.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-lapack-proto.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-fdm-fm.h"
#include "mx-fm-fdm.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"
#include "oct-cmplx.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"
#include "oct-norm.h"
#include "quit.h"
#include "schur.h"
#include "svd.h"

// Matrix class.

FloatMatrix::FloatMatrix (const FloatRowVector& rv)
  : FloatNDArray (rv)
{ }

FloatMatrix::FloatMatrix (const FloatColumnVector& cv)
  : FloatNDArray (cv)
{ }

FloatMatrix::FloatMatrix (const FloatDiagMatrix& a)
  : FloatNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatMatrix::FloatMatrix (const MDiagArray2<float>& a)
  : FloatNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatMatrix::FloatMatrix (const DiagArray2<float>& a)
  : FloatNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatMatrix::FloatMatrix (const PermMatrix& a)
  : FloatNDArray (a.dims (), 0.0)
{
  const Array<octave_idx_type> ia (a.col_perm_vec ());
  octave_idx_type len = a.rows ();
  for (octave_idx_type i = 0; i < len; i++)
    elem (ia(i), i) = 1.0;
}

// FIXME: could we use a templated mixed-type copy function here?

FloatMatrix::FloatMatrix (const boolMatrix& a)
  : FloatNDArray (a)
{ }

FloatMatrix::FloatMatrix (const charMatrix& a)
  : FloatNDArray (a.dims ())
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    for (octave_idx_type j = 0; j < a.cols (); j++)
      elem (i, j) = static_cast<unsigned char> (a.elem (i, j));
}

bool
FloatMatrix::operator == (const FloatMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return false;

  return mx_inline_equal (numel (), data (), a.data ());
}

bool
FloatMatrix::operator != (const FloatMatrix& a) const
{
  return !(*this == a);
}

bool
FloatMatrix::issymmetric (void) const
{
  if (issquare () && rows () > 0)
    {
      for (octave_idx_type i = 0; i < rows (); i++)
        for (octave_idx_type j = i+1; j < cols (); j++)
          if (elem (i, j) != elem (j, i))
            return false;

      return true;
    }

  return false;
}

FloatMatrix&
FloatMatrix::insert (const FloatMatrix& a,
                     octave_idx_type r, octave_idx_type c)
{
  FloatNDArray::insert (a, r, c);
  return *this;
}

FloatMatrix&
FloatMatrix::insert (const FloatRowVector& a,
                     octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.numel ();

  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    (*current_liboctave_error_handler) ("range error for insert");

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r, c+i) = a.elem (i);
    }

  return *this;
}

FloatMatrix&
FloatMatrix::insert (const FloatColumnVector& a,
                     octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.numel ();

  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
    (*current_liboctave_error_handler) ("range error for insert");

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c) = a.elem (i);
    }

  return *this;
}

FloatMatrix&
FloatMatrix::insert (const FloatDiagMatrix& a,
                     octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    (*current_liboctave_error_handler) ("range error for insert");

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

  octave_idx_type a_len = a.length ();

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c+i) = a.elem (i, i);
    }

  return *this;
}

FloatMatrix&
FloatMatrix::fill (float val)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          xelem (i, j) = val;
    }

  return *this;
}

FloatMatrix&
FloatMatrix::fill (float val, octave_idx_type r1, octave_idx_type c1,
                   octave_idx_type r2, octave_idx_type c2)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    (*current_liboctave_error_handler) ("range error for fill");

  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  if (r2 >= r1 && c2 >= c1)
    {
      make_unique ();

      for (octave_idx_type j = c1; j <= c2; j++)
        for (octave_idx_type i = r1; i <= r2; i++)
          xelem (i, j) = val;
    }

  return *this;
}

FloatMatrix
FloatMatrix::append (const FloatMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  FloatMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatMatrix
FloatMatrix::append (const FloatRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  FloatMatrix retval (nr, nc + a.numel ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatMatrix
FloatMatrix::append (const FloatColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.numel ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  FloatMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatMatrix
FloatMatrix::append (const FloatDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  FloatMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatMatrix
FloatMatrix::stack (const FloatMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  FloatMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatMatrix
FloatMatrix::stack (const FloatRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.numel ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  FloatMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatMatrix
FloatMatrix::stack (const FloatColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  FloatMatrix retval (nr + a.numel (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatMatrix
FloatMatrix::stack (const FloatDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  FloatMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatMatrix
real (const FloatComplexMatrix& a)
{
  return do_mx_unary_op<float, FloatComplex> (a, mx_inline_real);
}

FloatMatrix
imag (const FloatComplexMatrix& a)
{
  return do_mx_unary_op<float, FloatComplex> (a, mx_inline_imag);
}

FloatMatrix
FloatMatrix::extract (octave_idx_type r1, octave_idx_type c1,
                      octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  return index (octave::idx_vector (r1, r2+1), octave::idx_vector (c1, c2+1));
}

FloatMatrix
FloatMatrix::extract_n (octave_idx_type r1, octave_idx_type c1,
                        octave_idx_type nr, octave_idx_type nc) const
{
  return index (octave::idx_vector (r1, r1 + nr), octave::idx_vector (c1, c1 + nc));
}

// extract row or column i.

FloatRowVector
FloatMatrix::row (octave_idx_type i) const
{
  return index (octave::idx_vector (i), octave::idx_vector::colon);
}

FloatColumnVector
FloatMatrix::column (octave_idx_type i) const
{
  return index (octave::idx_vector::colon, octave::idx_vector (i));
}

// Local function to calculate the 1-norm.
static
float
norm1 (const FloatMatrix& a)
{
  float anorm = 0.0;
  FloatRowVector colsum = a.abs ().sum ().row (0);

  for (octave_idx_type i = 0; i < colsum.numel (); i++)
    {
      float sum = colsum.xelem (i);
      if (octave::math::isinf (sum) || octave::math::isnan (sum))
        {
          anorm = sum;  // Pass Inf or NaN to output
          break;
        }
      else
        anorm = std::max (anorm, sum);
    }

  return anorm;
}

FloatMatrix
FloatMatrix::inverse (void) const
{
  octave_idx_type info;
  float rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

FloatMatrix
FloatMatrix::inverse (octave_idx_type& info) const
{
  float rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

FloatMatrix
FloatMatrix::inverse (octave_idx_type& info, float& rcon, bool force,
                      bool calc_cond) const
{
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, force, calc_cond);
}

FloatMatrix
FloatMatrix::inverse (MatrixType& mattype) const
{
  octave_idx_type info;
  float rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

FloatMatrix
FloatMatrix::inverse (MatrixType& mattype, octave_idx_type& info) const
{
  float rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

FloatMatrix
FloatMatrix::tinverse (MatrixType& mattype, octave_idx_type& info, float& rcon,
                       bool force, bool calc_cond) const
{
  FloatMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");

  int typ = mattype.type ();
  char uplo = (typ == MatrixType::Lower ? 'L' : 'U');
  char udiag = 'N';
  retval = *this;
  float *tmp_data = retval.fortran_vec ();

  F77_INT tmp_info = 0;

  F77_XFCN (strtri, STRTRI, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                             F77_CONST_CHAR_ARG2 (&udiag, 1),
                             nr, tmp_data, nr, tmp_info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  info = tmp_info;

  // Throw away extra info LAPACK gives so as to not change output.
  rcon = 0.0;
  if (info != 0)
    info = -1;
  else if (calc_cond)
    {
      F77_INT dtrcon_info = 0;
      char job = '1';

      OCTAVE_LOCAL_BUFFER (float, work, 3 * nr);
      OCTAVE_LOCAL_BUFFER (F77_INT, iwork, nr);

      F77_XFCN (strcon, STRCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                 F77_CONST_CHAR_ARG2 (&uplo, 1),
                                 F77_CONST_CHAR_ARG2 (&udiag, 1),
                                 nr, tmp_data, nr, rcon,
                                 work, iwork, dtrcon_info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      if (dtrcon_info != 0)
        info = -1;
    }

  if (info == -1 && ! force)
    retval = *this; // Restore matrix contents.

  return retval;
}

FloatMatrix
FloatMatrix::finverse (MatrixType& mattype, octave_idx_type& info, float& rcon,
                       bool force, bool calc_cond) const
{
  FloatMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");

  Array<F77_INT> ipvt (dim_vector (nr, 1));
  F77_INT *pipvt = ipvt.fortran_vec ();

  retval = *this;
  float *tmp_data = retval.fortran_vec ();

  Array<float> z (dim_vector (1, 1));
  F77_INT lwork = -1;

  F77_INT tmp_info = 0;

  // Query the optimum work array size.
  F77_XFCN (sgetri, SGETRI, (nc, tmp_data, nr, pipvt,
                             z.fortran_vec (), lwork, tmp_info));

  lwork = static_cast<F77_INT> (z(0));
  lwork = (lwork < 4 * nc ? 4 * nc : lwork);
  z.resize (dim_vector (lwork, 1));
  float *pz = z.fortran_vec ();

  info = 0;
  tmp_info = 0;

  // Calculate the norm of the matrix for later use when determining rcon.
  float anorm;
  if (calc_cond)
    anorm = norm1 (retval);

  F77_XFCN (sgetrf, SGETRF, (nc, nc, tmp_data, nr, pipvt, tmp_info));

  info = tmp_info;

  // Throw away extra info LAPACK gives so as to not change output.
  rcon = 0.0;
  if (info != 0)
    info = -1;
  else if (calc_cond)
    {
      if (octave::math::isnan (anorm))
        rcon = octave::numeric_limits<float>::NaN ();
      else
        {
          F77_INT sgecon_info = 0;

          // Now calculate the condition number for non-singular matrix.
          char job = '1';
          Array<F77_INT> iz (dim_vector (nc, 1));
          F77_INT *piz = iz.fortran_vec ();
          F77_XFCN (sgecon, SGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                     nc, tmp_data, nr, anorm,
                                     rcon, pz, piz, sgecon_info
                                     F77_CHAR_ARG_LEN (1)));

          if (sgecon_info != 0)
            info = -1;
        }
    }

  if (info == -1 && ! force)
    retval = *this; // Restore matrix contents.
  else
    {
      F77_INT dgetri_info = 0;

      F77_XFCN (sgetri, SGETRI, (nc, tmp_data, nr, pipvt,
                                 pz, lwork, dgetri_info));

      if (dgetri_info != 0)
        info = -1;
    }

  if (info != 0)
    mattype.mark_as_rectangular ();

  return retval;
}

FloatMatrix
FloatMatrix::inverse (MatrixType& mattype, octave_idx_type& info, float& rcon,
                      bool force, bool calc_cond) const
{
  int typ = mattype.type (false);
  FloatMatrix ret;

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  if (typ == MatrixType::Diagonal)  // a scalar is classified as Diagonal.
    {
      ret = 1 / (*this);
      if (calc_cond)
        {
          float scalar = this->elem (0);
          if (octave::math::isfinite (scalar) && scalar != 0)
            rcon = 1.0f;
          else if (octave::math::isinf (scalar) || scalar == 0)
            rcon = 0.0f;
          else
            rcon = octave::numeric_limits<float>::NaN ();
        }
    }
  else if (typ == MatrixType::Upper || typ == MatrixType::Lower)
    ret = tinverse (mattype, info, rcon, force, calc_cond);
  else
    {
      if (mattype.ishermitian ())
        {
          octave::math::chol<FloatMatrix> chol (*this, info, true, calc_cond);
          if (info == 0)
            {
              if (calc_cond)
                rcon = chol.rcond ();
              else
                rcon = 1.0;
              ret = chol.inverse ();
            }
          else
            mattype.mark_as_unsymmetric ();
        }

      if (! mattype.ishermitian ())
        ret = finverse (mattype, info, rcon, force, calc_cond);

      if ((calc_cond || mattype.ishermitian ()) && rcon == 0.0)
        ret = FloatMatrix (rows (), columns (),
                           octave::numeric_limits<float>::Inf ());
    }

  return ret;
}

FloatMatrix
FloatMatrix::pseudo_inverse (float tol) const
{
  octave::math::svd<FloatMatrix> result (*this,
                                         octave::math::svd<FloatMatrix>::Type::economy);

  FloatDiagMatrix S = result.singular_values ();
  FloatMatrix U = result.left_singular_matrix ();
  FloatMatrix V = result.right_singular_matrix ();

  FloatColumnVector sigma = S.extract_diag ();

  octave_idx_type r = sigma.numel () - 1;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (tol <= 0.0)
    {
      tol = std::max (nr, nc) * sigma.elem (0)
            * std::numeric_limits<float>::epsilon ();

      if (tol == 0)
        tol = std::numeric_limits<float>::min ();
    }

  while (r >= 0 && sigma.elem (r) < tol)
    r--;

  if (r < 0)
    return FloatMatrix (nc, nr, 0.0);
  else
    {
      FloatMatrix Ur = U.extract (0, 0, nr-1, r);
      FloatDiagMatrix D = FloatDiagMatrix (sigma.extract (0, r)).inverse ();
      FloatMatrix Vr = V.extract (0, 0, nc-1, r);
      return Vr * D * Ur.transpose ();
    }
}

#if defined (HAVE_FFTW)

FloatComplexMatrix
FloatMatrix::fourier (void) const
{
  std::size_t nr = rows ();
  std::size_t nc = cols ();

  FloatComplexMatrix retval (nr, nc);

  std::size_t npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = (nr > nc ? nr : nc);
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  const float *in (data ());
  FloatComplex *out (retval.fortran_vec ());

  octave::fftw::fft (in, out, npts, nsamples);

  return retval;
}

FloatComplexMatrix
FloatMatrix::ifourier (void) const
{
  std::size_t nr = rows ();
  std::size_t nc = cols ();

  FloatComplexMatrix retval (nr, nc);

  std::size_t npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = (nr > nc ? nr : nc);
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  FloatComplexMatrix tmp (*this);
  FloatComplex *in (tmp.fortran_vec ());
  FloatComplex *out (retval.fortran_vec ());

  octave::fftw::ifft (in, out, npts, nsamples);

  return retval;
}

FloatComplexMatrix
FloatMatrix::fourier2d (void) const
{
  dim_vector dv (rows (), cols ());

  const float *in = data ();
  FloatComplexMatrix retval (rows (), cols ());
  octave::fftw::fftNd (in, retval.fortran_vec (), 2, dv);

  return retval;
}

FloatComplexMatrix
FloatMatrix::ifourier2d (void) const
{
  dim_vector dv (rows (), cols ());

  FloatComplexMatrix retval (*this);
  FloatComplex *out (retval.fortran_vec ());

  octave::fftw::ifftNd (out, out, 2, dv);

  return retval;
}

#else

FloatComplexMatrix
FloatMatrix::fourier (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexMatrix ();
}

FloatComplexMatrix
FloatMatrix::ifourier (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexMatrix ();
}

FloatComplexMatrix
FloatMatrix::fourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexMatrix ();
}

FloatComplexMatrix
FloatMatrix::ifourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexMatrix ();
}

#endif

FloatDET
FloatMatrix::determinant (void) const
{
  octave_idx_type info;
  float rcon;
  return determinant (info, rcon, 0);
}

FloatDET
FloatMatrix::determinant (octave_idx_type& info) const
{
  float rcon;
  return determinant (info, rcon, 0);
}

FloatDET
FloatMatrix::determinant (octave_idx_type& info, float& rcon,
                          bool calc_cond) const
{
  MatrixType mattype (*this);
  return determinant (mattype, info, rcon, calc_cond);
}

FloatDET
FloatMatrix::determinant (MatrixType& mattype,
                          octave_idx_type& info, float& rcon,
                          bool calc_cond) const
{
  FloatDET retval (1.0);

  info = 0;
  rcon = 0.0;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");

  volatile int typ = mattype.type ();

  // Even though the matrix is marked as singular (Rectangular), we may
  // still get a useful number from the LU factorization, because it always
  // completes.

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);
  else if (typ == MatrixType::Rectangular)
    typ = MatrixType::Full;

  if (typ == MatrixType::Lower || typ == MatrixType::Upper)
    {
      for (F77_INT i = 0; i < nc; i++)
        retval *= elem (i, i);
    }
  else if (typ == MatrixType::Hermitian)
    {
      FloatMatrix atmp = *this;
      float *tmp_data = atmp.fortran_vec ();

      // Calculate the norm of the matrix for later use when determining rcon.
      float anorm;
      if (calc_cond)
        anorm = norm1 (*this);

      F77_INT tmp_info = 0;

      char job = 'L';
      F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                 tmp_data, nr, tmp_info
                                 F77_CHAR_ARG_LEN (1)));

      info = tmp_info;

      if (info != 0)
        {
          rcon = 0.0;
          mattype.mark_as_unsymmetric ();
          typ = MatrixType::Full;
        }
      else
        {
          if (calc_cond)
            {
              Array<float> z (dim_vector (3 * nc, 1));
              float *pz = z.fortran_vec ();
              Array<F77_INT> iz (dim_vector (nc, 1));
              F77_INT *piz = iz.fortran_vec ();

              F77_XFCN (spocon, SPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                         nr, tmp_data, nr, anorm,
                                         rcon, pz, piz, tmp_info
                                         F77_CHAR_ARG_LEN (1)));

              info = tmp_info;

              if (info != 0)
                rcon = 0.0;
            }

          for (F77_INT i = 0; i < nc; i++)
            retval *= atmp(i, i);

          retval = retval.square ();
        }
    }
  else if (typ != MatrixType::Full)
    (*current_liboctave_error_handler) ("det: invalid dense matrix type");

  if (typ == MatrixType::Full)
    {
      Array<F77_INT> ipvt (dim_vector (nr, 1));
      F77_INT *pipvt = ipvt.fortran_vec ();

      FloatMatrix atmp = *this;
      float *tmp_data = atmp.fortran_vec ();

      info = 0;
      F77_INT tmp_info = 0;

      // Calculate the norm of the matrix for later use when determining rcon.
      float anorm;
      if (calc_cond)
        anorm = norm1 (*this);

      F77_XFCN (sgetrf, SGETRF, (nr, nr, tmp_data, nr, pipvt, tmp_info));

      info = tmp_info;

      // Throw away extra info LAPACK gives so as to not change output.
      rcon = 0.0;
      if (info != 0)
        {
          info = -1;
          retval = FloatDET ();
        }
      else
        {
          if (calc_cond)
            {
              // Now calc the condition number for non-singular matrix.
              char job = '1';
              Array<float> z (dim_vector (4 * nc, 1));
              float *pz = z.fortran_vec ();
              Array<F77_INT> iz (dim_vector (nc, 1));
              F77_INT *piz = iz.fortran_vec ();

              F77_XFCN (sgecon, SGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                         nc, tmp_data, nr, anorm,
                                         rcon, pz, piz, tmp_info
                                         F77_CHAR_ARG_LEN (1)));

              info = tmp_info;
            }

          if (info != 0)
            {
              info = -1;
              retval = FloatDET ();
            }
          else
            {
              for (F77_INT i = 0; i < nc; i++)
                {
                  float c = atmp(i, i);
                  retval *= (ipvt(i) != (i+1)) ? -c : c;
                }
            }
        }
    }

  return retval;
}

float
FloatMatrix::rcond (void) const
{
  MatrixType mattype (*this);
  return rcond (mattype);
}

float
FloatMatrix::rcond (MatrixType& mattype) const
{
  float rcon = octave::numeric_limits<float>::NaN ();
  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");

  if (nr == 0 || nc == 0)
    rcon = octave::numeric_limits<float>::Inf ();
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Unknown)
        typ = mattype.type (*this);

      // Only calculate the condition number for LU/Cholesky
      if (typ == MatrixType::Upper)
        {
          const float *tmp_data = data ();
          F77_INT info = 0;
          char norm = '1';
          char uplo = 'U';
          char dia = 'N';

          Array<float> z (dim_vector (3 * nc, 1));
          float *pz = z.fortran_vec ();
          Array<F77_INT> iz (dim_vector (nc, 1));
          F77_INT *piz = iz.fortran_vec ();

          F77_XFCN (strcon, STRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, tmp_data, nr, rcon,
                                     pz, piz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0.0;
        }
      else if (typ == MatrixType::Permuted_Upper)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Lower)
        {
          const float *tmp_data = data ();
          F77_INT info = 0;
          char norm = '1';
          char uplo = 'L';
          char dia = 'N';

          Array<float> z (dim_vector (3 * nc, 1));
          float *pz = z.fortran_vec ();
          Array<F77_INT> iz (dim_vector (nc, 1));
          F77_INT *piz = iz.fortran_vec ();

          F77_XFCN (strcon, STRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, tmp_data, nr, rcon,
                                     pz, piz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0.0;
        }
      else if (typ == MatrixType::Permuted_Lower)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
        {
          float anorm = -1.0;

          if (typ == MatrixType::Hermitian)
            {
              F77_INT info = 0;
              char job = 'L';

              FloatMatrix atmp = *this;
              float *tmp_data = atmp.fortran_vec ();

              anorm = norm1 (atmp);

              F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                         tmp_data, nr, info
                                         F77_CHAR_ARG_LEN (1)));

              if (info != 0)
                {
                  rcon = 0.0;
                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
              else
                {
                  Array<float> z (dim_vector (3 * nc, 1));
                  float *pz = z.fortran_vec ();
                  Array<F77_INT> iz (dim_vector (nc, 1));
                  F77_INT *piz = iz.fortran_vec ();

                  F77_XFCN (spocon, SPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }

          if (typ == MatrixType::Full)
            {
              F77_INT info = 0;

              FloatMatrix atmp = *this;
              float *tmp_data = atmp.fortran_vec ();

              Array<F77_INT> ipvt (dim_vector (nr, 1));
              F77_INT *pipvt = ipvt.fortran_vec ();

              if (anorm < 0.0)
                anorm = norm1 (atmp);

              Array<float> z (dim_vector (4 * nc, 1));
              float *pz = z.fortran_vec ();
              Array<F77_INT> iz (dim_vector (nc, 1));
              F77_INT *piz = iz.fortran_vec ();

              F77_XFCN (sgetrf, SGETRF, (nr, nr, tmp_data, nr, pipvt, info));

              if (info != 0)
                {
                  rcon = 0.0;
                  mattype.mark_as_rectangular ();
                }
              else
                {
                  char job = '1';
                  F77_XFCN (sgecon, SGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }
        }
      else
        rcon = 0.0;
    }

  return rcon;
}

FloatMatrix
FloatMatrix::utsolve (MatrixType& mattype, const FloatMatrix& b,
                      octave_idx_type& info,
                      float& rcon, solve_singularity_handler sing_handler,
                      bool calc_cond, blas_trans_type transt) const
{
  FloatMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (nr == 0 || nc == 0 || b_nc == 0)
    retval = FloatMatrix (nc, b_nc, 0.0);
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Upper || typ == MatrixType::Upper)
        {
          rcon = 1.0;
          info = 0;

          if (typ == MatrixType::Permuted_Upper)
            (*current_liboctave_error_handler)
              ("permuted triangular matrix not implemented");
          else
            {
              const float *tmp_data = data ();

              retval = b;
              float *result = retval.fortran_vec ();

              char uplo = 'U';
              char trans = get_blas_char (transt);
              char dia = 'N';

              F77_INT tmp_info = 0;

              F77_XFCN (strtrs, STRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                         F77_CONST_CHAR_ARG2 (&trans, 1),
                                         F77_CONST_CHAR_ARG2 (&dia, 1),
                                         nr, b_nc, tmp_data, nr,
                                         result, nr, tmp_info
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)));

              info = tmp_info;

              if (calc_cond)
                {
                  char norm = '1';
                  uplo = 'U';
                  dia = 'N';

                  Array<float> z (dim_vector (3 * nc, 1));
                  float *pz = z.fortran_vec ();
                  Array<F77_INT> iz (dim_vector (nc, 1));
                  F77_INT *piz = iz.fortran_vec ();

                  F77_XFCN (strcon, STRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                             F77_CONST_CHAR_ARG2 (&uplo, 1),
                                             F77_CONST_CHAR_ARG2 (&dia, 1),
                                             nr, tmp_data, nr, rcon,
                                             pz, piz, tmp_info
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || octave::math::isnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        octave::warn_singular_matrix (rcon);
                    }
                }

            }
        }
      else
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

FloatMatrix
FloatMatrix::ltsolve (MatrixType& mattype, const FloatMatrix& b,
                      octave_idx_type& info,
                      float& rcon, solve_singularity_handler sing_handler,
                      bool calc_cond, blas_trans_type transt) const
{
  FloatMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (nr == 0 || nc == 0 || b_nc == 0)
    retval = FloatMatrix (nc, b_nc, 0.0);
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Lower || typ == MatrixType::Lower)
        {
          rcon = 1.0;
          info = 0;

          if (typ == MatrixType::Permuted_Lower)
            (*current_liboctave_error_handler)
              ("permuted triangular matrix not implemented");
          else
            {
              const float *tmp_data = data ();

              retval = b;
              float *result = retval.fortran_vec ();

              char uplo = 'L';
              char trans = get_blas_char (transt);
              char dia = 'N';

              F77_INT tmp_info = 0;

              F77_XFCN (strtrs, STRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                         F77_CONST_CHAR_ARG2 (&trans, 1),
                                         F77_CONST_CHAR_ARG2 (&dia, 1),
                                         nr, b_nc, tmp_data, nr,
                                         result, nr, tmp_info
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)));

              info = tmp_info;

              if (calc_cond)
                {
                  char norm = '1';
                  uplo = 'L';
                  dia = 'N';

                  Array<float> z (dim_vector (3 * nc, 1));
                  float *pz = z.fortran_vec ();
                  Array<F77_INT> iz (dim_vector (nc, 1));
                  F77_INT *piz = iz.fortran_vec ();

                  F77_XFCN (strcon, STRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                             F77_CONST_CHAR_ARG2 (&uplo, 1),
                                             F77_CONST_CHAR_ARG2 (&dia, 1),
                                             nr, tmp_data, nr, rcon,
                                             pz, piz, tmp_info
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || octave::math::isnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        octave::warn_singular_matrix (rcon);
                    }
                }
            }
        }
      else
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

FloatMatrix
FloatMatrix::fsolve (MatrixType& mattype, const FloatMatrix& b,
                     octave_idx_type& info,
                     float& rcon, solve_singularity_handler sing_handler,
                     bool calc_cond) const
{
  FloatMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (nr != nc || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (nr == 0 || b_nc == 0)
    retval = FloatMatrix (nc, b_nc, 0.0);
  else
    {
      volatile int typ = mattype.type ();

      // Calculate the norm of the matrix for later use when determining rcon.
      float anorm = -1.0;

      if (typ == MatrixType::Hermitian)
        {
          info = 0;
          char job = 'L';

          FloatMatrix atmp = *this;
          float *tmp_data = atmp.fortran_vec ();

          // The norm of the matrix for later use when determining rcon.
          if (calc_cond)
            anorm = norm1 (atmp);

          F77_INT tmp_info = 0;

          F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                     tmp_data, nr, tmp_info
                                     F77_CHAR_ARG_LEN (1)));

          info = tmp_info;

          // Throw away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -2;

              mattype.mark_as_unsymmetric ();
              typ = MatrixType::Full;
            }
          else
            {
              if (calc_cond)
                {
                  Array<float> z (dim_vector (3 * nc, 1));
                  float *pz = z.fortran_vec ();
                  Array<F77_INT> iz (dim_vector (nc, 1));
                  F77_INT *piz = iz.fortran_vec ();

                  F77_XFCN (spocon, SPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, tmp_data, nr, anorm,
                                             rcon, pz, piz, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || octave::math::isnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        octave::warn_singular_matrix (rcon);
                    }
                }

              if (info == 0)
                {
                  retval = b;
                  float *result = retval.fortran_vec ();

                  F77_XFCN (spotrs, SPOTRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, tmp_data, nr,
                                             result, b_nr, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;
                }
              else
                {
                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
            }
        }

      if (typ == MatrixType::Full)
        {
          info = 0;

          Array<F77_INT> ipvt (dim_vector (nr, 1));
          F77_INT *pipvt = ipvt.fortran_vec ();

          FloatMatrix atmp = *this;
          float *tmp_data = atmp.fortran_vec ();

          if (calc_cond && anorm < 0.0)
            anorm = norm1 (atmp);

          Array<float> z (dim_vector (4 * nc, 1));
          float *pz = z.fortran_vec ();
          Array<F77_INT> iz (dim_vector (nc, 1));
          F77_INT *piz = iz.fortran_vec ();

          F77_INT tmp_info = 0;

          F77_XFCN (sgetrf, SGETRF, (nr, nr, tmp_data, nr, pipvt, tmp_info));

          info = tmp_info;

          // Throw away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -2;

              if (sing_handler)
                sing_handler (rcon);
              else
                octave::warn_singular_matrix ();

              mattype.mark_as_rectangular ();
            }
          else
            {
              if (calc_cond)
                {
                  // Calculate the condition number for non-singular matrix.
                  char job = '1';
                  F77_XFCN (sgecon, SGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, piz, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || octave::math::isnan (rcon))
                    {
                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        octave::warn_singular_matrix (rcon);
                    }
                }

              if (info == 0)
                {
                  retval = b;
                  float *result = retval.fortran_vec ();

                  char job = 'N';
                  F77_XFCN (sgetrs, SGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, tmp_data, nr,
                                             pipvt, result, b_nr, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;
                }
              else
                mattype.mark_as_rectangular ();
            }
        }
      else if (typ != MatrixType::Hermitian)
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

FloatMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

FloatMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatMatrix& b,
                    octave_idx_type& info) const
{
  float rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

FloatMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatMatrix& b,
                    octave_idx_type& info, float& rcon) const
{
  return solve (mattype, b, info, rcon, nullptr);
}

FloatMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatMatrix& b,
                    octave_idx_type& info,
                    float& rcon, solve_singularity_handler sing_handler,
                    bool singular_fallback, blas_trans_type transt) const
{
  FloatMatrix retval;
  int typ = mattype.type ();

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  // Only calculate the condition number for LU/Cholesky
  if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    retval = utsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    retval = ltsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (transt == blas_trans || transt == blas_conj_trans)
    return transpose ().solve (mattype, b, info, rcon, sing_handler,
                               singular_fallback);
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
    retval = fsolve (mattype, b, info, rcon, sing_handler, true);
  else if (typ != MatrixType::Rectangular)
    (*current_liboctave_error_handler) ("unknown matrix type");

  // Rectangular or one of the above solvers flags a singular matrix
  if (singular_fallback && mattype.type () == MatrixType::Rectangular)
    {
      octave_idx_type rank;
      retval = lssolve (b, info, rank, rcon);
    }

  return retval;
}

FloatComplexMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatComplexMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

FloatComplexMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatComplexMatrix& b,
                    octave_idx_type& info) const
{
  float rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

FloatComplexMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatComplexMatrix& b,
                    octave_idx_type& info,
                    float& rcon) const
{
  return solve (mattype, b, info, rcon, nullptr);
}

static FloatMatrix
stack_complex_matrix (const FloatComplexMatrix& cm)
{
  octave_idx_type m = cm.rows ();
  octave_idx_type n = cm.cols ();
  octave_idx_type nel = m*n;
  FloatMatrix retval (m, 2*n);
  const FloatComplex *cmd = cm.data ();
  float *rd = retval.fortran_vec ();
  for (octave_idx_type i = 0; i < nel; i++)
    {
      rd[i] = std::real (cmd[i]);
      rd[nel+i] = std::imag (cmd[i]);
    }
  return retval;
}

static FloatComplexMatrix
unstack_complex_matrix (const FloatMatrix& sm)
{
  octave_idx_type m = sm.rows ();
  octave_idx_type n = sm.cols () / 2;
  octave_idx_type nel = m*n;
  FloatComplexMatrix retval (m, n);
  const float *smd = sm.data ();
  FloatComplex *rd = retval.fortran_vec ();
  for (octave_idx_type i = 0; i < nel; i++)
    rd[i] = FloatComplex (smd[i], smd[nel+i]);
  return retval;
}

FloatComplexMatrix
FloatMatrix::solve (MatrixType& mattype, const FloatComplexMatrix& b,
                    octave_idx_type& info,
                    float& rcon, solve_singularity_handler sing_handler,
                    bool singular_fallback, blas_trans_type transt) const
{
  FloatMatrix tmp = stack_complex_matrix (b);
  tmp = solve (mattype, tmp, info, rcon, sing_handler, singular_fallback,
               transt);
  return unstack_complex_matrix (tmp);
}

FloatColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatColumnVector& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (mattype, b, info, rcon);
}

FloatColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatColumnVector& b,
                    octave_idx_type& info) const
{
  float rcon;
  return solve (mattype, b, info, rcon);
}

FloatColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatColumnVector& b,
                    octave_idx_type& info,
                    float& rcon) const
{
  return solve (mattype, b, info, rcon, nullptr);
}

FloatColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatColumnVector& b,
                    octave_idx_type& info,
                    float& rcon, solve_singularity_handler sing_handler,
                    blas_trans_type transt) const
{
  FloatMatrix tmp (b);
  tmp = solve (mattype, tmp, info, rcon, sing_handler, true, transt);
  return tmp.column (static_cast<octave_idx_type> (0));
}

FloatComplexColumnVector
FloatMatrix::solve (MatrixType& mattype,
                    const FloatComplexColumnVector& b) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (mattype, b);
}

FloatComplexColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatComplexColumnVector& b,
                    octave_idx_type& info) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (mattype, b, info);
}

FloatComplexColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatComplexColumnVector& b,
                    octave_idx_type& info, float& rcon) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (mattype, b, info, rcon);
}

FloatComplexColumnVector
FloatMatrix::solve (MatrixType& mattype, const FloatComplexColumnVector& b,
                    octave_idx_type& info, float& rcon,
                    solve_singularity_handler sing_handler,
                    blas_trans_type transt) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (mattype, b, info, rcon, sing_handler, transt);
}

FloatMatrix
FloatMatrix::solve (const FloatMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (b, info, rcon, nullptr);
}

FloatMatrix
FloatMatrix::solve (const FloatMatrix& b, octave_idx_type& info) const
{
  float rcon;
  return solve (b, info, rcon, nullptr);
}

FloatMatrix
FloatMatrix::solve (const FloatMatrix& b, octave_idx_type& info,
                    float& rcon) const
{
  return solve (b, info, rcon, nullptr);
}

FloatMatrix
FloatMatrix::solve (const FloatMatrix& b, octave_idx_type& info,
                    float& rcon, solve_singularity_handler sing_handler,
                    blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, true, transt);
}

FloatComplexMatrix
FloatMatrix::solve (const FloatComplexMatrix& b) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b);
}

FloatComplexMatrix
FloatMatrix::solve (const FloatComplexMatrix& b, octave_idx_type& info) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

FloatComplexMatrix
FloatMatrix::solve (const FloatComplexMatrix& b, octave_idx_type& info,
                    float& rcon) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon);
}

FloatComplexMatrix
FloatMatrix::solve (const FloatComplexMatrix& b, octave_idx_type& info,
                    float& rcon,
                    solve_singularity_handler sing_handler,
                    blas_trans_type transt) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon, sing_handler, transt);
}

FloatColumnVector
FloatMatrix::solve (const FloatColumnVector& b) const
{
  octave_idx_type info; float rcon;
  return solve (b, info, rcon);
}

FloatColumnVector
FloatMatrix::solve (const FloatColumnVector& b, octave_idx_type& info) const
{
  float rcon;
  return solve (b, info, rcon);
}

FloatColumnVector
FloatMatrix::solve (const FloatColumnVector& b, octave_idx_type& info,
                    float& rcon) const
{
  return solve (b, info, rcon, nullptr);
}

FloatColumnVector
FloatMatrix::solve (const FloatColumnVector& b, octave_idx_type& info,
                    float& rcon, solve_singularity_handler sing_handler,
                    blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, transt);
}

FloatComplexColumnVector
FloatMatrix::solve (const FloatComplexColumnVector& b) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b);
}

FloatComplexColumnVector
FloatMatrix::solve (const FloatComplexColumnVector& b,
                    octave_idx_type& info) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

FloatComplexColumnVector
FloatMatrix::solve (const FloatComplexColumnVector& b, octave_idx_type& info,
                    float& rcon) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon);
}

FloatComplexColumnVector
FloatMatrix::solve (const FloatComplexColumnVector& b, octave_idx_type& info,
                    float& rcon, solve_singularity_handler sing_handler,
                    blas_trans_type transt) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon, sing_handler, transt);
}

FloatMatrix
FloatMatrix::lssolve (const FloatMatrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatMatrix
FloatMatrix::lssolve (const FloatMatrix& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatMatrix
FloatMatrix::lssolve (const FloatMatrix& b, octave_idx_type& info,
                      octave_idx_type& rank) const
{
  float rcon;
  return lssolve (b, info, rank, rcon);
}


FloatMatrix
FloatMatrix::lssolve (const FloatMatrix& b, octave_idx_type& info,
                      octave_idx_type& rank, float& rcon) const
{
  FloatMatrix retval;

  F77_INT m = octave::to_f77_int (rows ());
  F77_INT n = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());
  F77_INT nrhs = b_nc;  // alias for code readability

  if (m != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (m == 0 || n == 0 || b_nc == 0)
    retval = FloatMatrix (n, b_nc, 0.0);
  else
    {
      volatile F77_INT minmn = (m < n ? m : n);
      F77_INT maxmn = (m > n ? m : n);
      rcon = -1.0;
      if (m != n)
        {
          retval = FloatMatrix (maxmn, nrhs, 0.0);

          for (F77_INT j = 0; j < nrhs; j++)
            for (F77_INT i = 0; i < m; i++)
              retval.elem (i, j) = b.elem (i, j);
        }
      else
        retval = b;

      FloatMatrix atmp = *this;
      float *tmp_data = atmp.fortran_vec ();

      float *pretval = retval.fortran_vec ();
      Array<float> s (dim_vector (minmn, 1));
      float *ps = s.fortran_vec ();

      // Ask DGELSD what the dimension of WORK should be.
      F77_INT lwork = -1;

      Array<float> work (dim_vector (1, 1));

      F77_INT smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("SGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      F77_INT mnthr;
      F77_FUNC (xilaenv, XILAENV) (6, F77_CONST_CHAR_ARG2 ("SGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   m, n, nrhs, -1, mnthr
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of iwork because DGELSD in older versions
      // of LAPACK does not return it on a query call.
      float dminmn = static_cast<float> (minmn);
      float dsmlsizp1 = static_cast<float> (smlsiz+1);
      float tmp = octave::math::log2 (dminmn / dsmlsizp1);

      F77_INT nlvl = static_cast<F77_INT> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      F77_INT liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<F77_INT> iwork (dim_vector (liwork, 1));
      F77_INT *piwork = iwork.fortran_vec ();

      F77_INT tmp_info = 0;
      F77_INT tmp_rank = 0;

      F77_XFCN (sgelsd, SGELSD, (m, n, nrhs, tmp_data, m, pretval, maxmn,
                                 ps, rcon, tmp_rank, work.fortran_vec (),
                                 lwork, piwork, tmp_info));

      info = tmp_info;
      rank = tmp_rank;

      // The workspace query is broken in at least LAPACK 3.0.0
      // through 3.1.1 when n >= mnthr.  The obtuse formula below
      // should provide sufficient workspace for DGELSD to operate
      // efficiently.
      if (n > m && n >= mnthr)
        {
          const F77_INT wlalsd
            = 9*m + 2*m*smlsiz + 8*m*nlvl + m*nrhs + (smlsiz+1)*(smlsiz+1);

          F77_INT addend = m;

          if (2*m-4 > addend)
            addend = 2*m-4;

          if (nrhs > addend)
            addend = nrhs;

          if (n-3*m > addend)
            addend = n-3*m;

          if (wlalsd > addend)
            addend = wlalsd;

          const F77_INT lworkaround = 4*m + m*m + addend;

          if (work(0) < lworkaround)
            work(0) = lworkaround;
        }
      else if (m >= n)
        {
          F77_INT lworkaround
            = 12*n + 2*n*smlsiz + 8*n*nlvl + n*nrhs + (smlsiz+1)*(smlsiz+1);

          if (work(0) < lworkaround)
            work(0) = lworkaround;
        }

      lwork = static_cast<F77_INT> (work(0));
      work.resize (dim_vector (lwork, 1));

      float anorm = norm1 (*this);

      if (octave::math::isinf (anorm))
        {
          rcon = 0.0;
          retval = FloatMatrix (n, b_nc, 0.0);
        }
      else if (octave::math::isnan (anorm))
        {
          rcon = octave::numeric_limits<float>::NaN ();
          retval = FloatMatrix (n, b_nc,
                                octave::numeric_limits<float>::NaN ());
        }
      else
        {
          F77_XFCN (sgelsd, SGELSD, (m, n, nrhs, tmp_data, m, pretval,
                                     maxmn, ps, rcon, tmp_rank,
                                     work.fortran_vec (), lwork,
                                     piwork, tmp_info));

          info = tmp_info;
          rank = tmp_rank;

          if (s.elem (0) == 0.0)
            rcon = 0.0;
          else
            rcon = s.elem (minmn - 1) / s.elem (0);

          retval.resize (n, nrhs);
        }
    }

  return retval;
}

FloatComplexMatrix
FloatMatrix::lssolve (const FloatComplexMatrix& b) const
{
  FloatComplexMatrix tmp (*this);
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

FloatComplexMatrix
FloatMatrix::lssolve (const FloatComplexMatrix& b, octave_idx_type& info) const
{
  FloatComplexMatrix tmp (*this);
  octave_idx_type rank;
  float rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

FloatComplexMatrix
FloatMatrix::lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
                      octave_idx_type& rank) const
{
  FloatComplexMatrix tmp (*this);
  float rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

FloatComplexMatrix
FloatMatrix::lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
                      octave_idx_type& rank, float& rcon) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank, rcon);
}

FloatColumnVector
FloatMatrix::lssolve (const FloatColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatColumnVector
FloatMatrix::lssolve (const FloatColumnVector& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatColumnVector
FloatMatrix::lssolve (const FloatColumnVector& b, octave_idx_type& info,
                      octave_idx_type& rank) const
{
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatColumnVector
FloatMatrix::lssolve (const FloatColumnVector& b, octave_idx_type& info,
                      octave_idx_type& rank, float& rcon) const
{
  FloatColumnVector retval;

  F77_INT nrhs = 1;

  F77_INT m = octave::to_f77_int (rows ());
  F77_INT n = octave::to_f77_int (cols ());

  if (m != b.numel ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (m == 0 || n == 0)
    retval = FloatColumnVector (n, 0.0);
  else
    {
      volatile F77_INT minmn = (m < n ? m : n);
      F77_INT maxmn = (m > n ? m : n);
      rcon = -1.0;

      if (m != n)
        {
          retval = FloatColumnVector (maxmn, 0.0);

          for (F77_INT i = 0; i < m; i++)
            retval.elem (i) = b.elem (i);
        }
      else
        retval = b;

      FloatMatrix atmp = *this;
      float *tmp_data = atmp.fortran_vec ();

      float *pretval = retval.fortran_vec ();
      Array<float> s (dim_vector (minmn, 1));
      float *ps = s.fortran_vec ();

      // Ask DGELSD what the dimension of WORK should be.
      F77_INT lwork = -1;

      Array<float> work (dim_vector (1, 1));

      F77_INT smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("SGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of iwork because DGELSD in older versions
      // of LAPACK does not return it on a query call.
      float dminmn = static_cast<float> (minmn);
      float dsmlsizp1 = static_cast<float> (smlsiz+1);
      float tmp = octave::math::log2 (dminmn / dsmlsizp1);

      F77_INT nlvl = static_cast<F77_INT> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      F77_INT liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<F77_INT> iwork (dim_vector (liwork, 1));
      F77_INT *piwork = iwork.fortran_vec ();

      F77_INT tmp_info = 0;
      F77_INT tmp_rank = 0;

      F77_XFCN (sgelsd, SGELSD, (m, n, nrhs, tmp_data, m, pretval, maxmn,
                                 ps, rcon, tmp_rank, work.fortran_vec (),
                                 lwork, piwork, tmp_info));

      info = tmp_info;
      rank = tmp_rank;

      lwork = static_cast<F77_INT> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (sgelsd, SGELSD, (m, n, nrhs, tmp_data, m, pretval,
                                 maxmn, ps, rcon, tmp_rank,
                                 work.fortran_vec (), lwork,
                                 piwork, tmp_info));

      info = tmp_info;
      rank = tmp_rank;

      if (rank < minmn)
        {
          if (s.elem (0) == 0.0)
            rcon = 0.0;
          else
            rcon = s.elem (minmn - 1) / s.elem (0);
        }

      retval.resize (n);
    }

  return retval;
}

FloatComplexColumnVector
FloatMatrix::lssolve (const FloatComplexColumnVector& b) const
{
  FloatComplexMatrix tmp (*this);
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

FloatComplexColumnVector
FloatMatrix::lssolve (const FloatComplexColumnVector& b,
                      octave_idx_type& info) const
{
  FloatComplexMatrix tmp (*this);
  octave_idx_type rank;
  float rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

FloatComplexColumnVector
FloatMatrix::lssolve (const FloatComplexColumnVector& b, octave_idx_type& info,
                      octave_idx_type& rank) const
{
  FloatComplexMatrix tmp (*this);
  float rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

FloatComplexColumnVector
FloatMatrix::lssolve (const FloatComplexColumnVector& b, octave_idx_type& info,
                      octave_idx_type& rank, float& rcon) const
{
  FloatComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank, rcon);
}

FloatMatrix&
FloatMatrix::operator += (const FloatDiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    octave::err_nonconformant ("operator +=", nr, nc, a_nr, a_nc);

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

FloatMatrix&
FloatMatrix::operator -= (const FloatDiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    octave::err_nonconformant ("operator -=", nr, nc, a_nr, a_nc);

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// column vector by row vector -> matrix operations

FloatMatrix
operator * (const FloatColumnVector& v, const FloatRowVector& a)
{
  FloatMatrix retval;

  F77_INT len = octave::to_f77_int (v.numel ());

  if (len != 0)
    {
      F77_INT a_len = octave::to_f77_int (a.numel ());

      retval = FloatMatrix (len, a_len);
      float *c = retval.fortran_vec ();

      F77_XFCN (sgemm, SGEMM, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 ("N", 1),
                               len, a_len, 1, 1.0, v.data (), len,
                               a.data (), 1, 0.0, c, len
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));
    }

  return retval;
}

// FIXME: Do these really belong here?  Maybe they should be in a base class?

FloatMatrix
FloatMatrix::cumprod (int dim) const
{
  return FloatNDArray::cumprod (dim);
}

FloatMatrix
FloatMatrix::cumsum (int dim) const
{
  return FloatNDArray::cumsum (dim);
}

FloatMatrix
FloatMatrix::prod (int dim) const
{
  return FloatNDArray::prod (dim);
}

FloatMatrix
FloatMatrix::sum (int dim) const
{
  return FloatNDArray::sum (dim);
}

FloatMatrix
FloatMatrix::sumsq (int dim) const
{
  return FloatNDArray::sumsq (dim);
}

FloatMatrix
FloatMatrix::abs (void) const
{
  return FloatNDArray::abs ();
}

FloatMatrix
FloatMatrix::diag (octave_idx_type k) const
{
  return FloatNDArray::diag (k);
}

FloatDiagMatrix
FloatMatrix::diag (octave_idx_type m, octave_idx_type n) const
{
  FloatDiagMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 1 || nc == 1)
    retval = FloatDiagMatrix (*this, m, n);
  else
    (*current_liboctave_error_handler) ("diag: expecting vector argument");

  return retval;
}

FloatColumnVector
FloatMatrix::row_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_min (dummy_idx);
}

FloatColumnVector
FloatMatrix::row_min (Array<octave_idx_type>& idx_arg) const
{
  FloatColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_idx_type idx_j;

          float tmp_min = octave::numeric_limits<float>::NaN ();

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_min = elem (i, idx_j);

              if (! octave::math::isnan (tmp_min))
                break;
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              float tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;
              else if (tmp < tmp_min)
                {
                  idx_j = j;
                  tmp_min = tmp;
                }
            }

          result.elem (i) = tmp_min;
          idx_arg.elem (i) = (octave::math::isnan (tmp_min) ? 0 : idx_j);
        }
    }

  return result;
}

FloatColumnVector
FloatMatrix::row_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_max (dummy_idx);
}

FloatColumnVector
FloatMatrix::row_max (Array<octave_idx_type>& idx_arg) const
{
  FloatColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_idx_type idx_j;

          float tmp_max = octave::numeric_limits<float>::NaN ();

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_max = elem (i, idx_j);

              if (! octave::math::isnan (tmp_max))
                break;
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              float tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;
              else if (tmp > tmp_max)
                {
                  idx_j = j;
                  tmp_max = tmp;
                }
            }

          result.elem (i) = tmp_max;
          idx_arg.elem (i) = (octave::math::isnan (tmp_max) ? 0 : idx_j);
        }
    }

  return result;
}

FloatRowVector
FloatMatrix::column_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_min (dummy_idx);
}

FloatRowVector
FloatMatrix::column_min (Array<octave_idx_type>& idx_arg) const
{
  FloatRowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type idx_i;

          float tmp_min = octave::numeric_limits<float>::NaN ();

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_min = elem (idx_i, j);

              if (! octave::math::isnan (tmp_min))
                break;
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              float tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;
              else if (tmp < tmp_min)
                {
                  idx_i = i;
                  tmp_min = tmp;
                }
            }

          result.elem (j) = tmp_min;
          idx_arg.elem (j) = (octave::math::isnan (tmp_min) ? 0 : idx_i);
        }
    }

  return result;
}

FloatRowVector
FloatMatrix::column_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_max (dummy_idx);
}

FloatRowVector
FloatMatrix::column_max (Array<octave_idx_type>& idx_arg) const
{
  FloatRowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type idx_i;

          float tmp_max = octave::numeric_limits<float>::NaN ();

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_max = elem (idx_i, j);

              if (! octave::math::isnan (tmp_max))
                break;
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              float tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;
              else if (tmp > tmp_max)
                {
                  idx_i = i;
                  tmp_max = tmp;
                }
            }

          result.elem (j) = tmp_max;
          idx_arg.elem (j) = (octave::math::isnan (tmp_max) ? 0 : idx_i);
        }
    }

  return result;
}

std::ostream&
operator << (std::ostream& os, const FloatMatrix& a)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    {
      for (octave_idx_type j = 0; j < a.cols (); j++)
        {
          os << ' ';
          octave::write_value<float> (os, a.elem (i, j));
        }
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, FloatMatrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr > 0 && nc > 0)
    {
      float tmp;
      for (octave_idx_type i = 0; i < nr; i++)
        for (octave_idx_type j = 0; j < nc; j++)
          {
            tmp = octave::read_value<float> (is);
            if (is)
              a.elem (i, j) = tmp;
            else
              return is;
          }
    }

  return is;
}

FloatMatrix
Givens (float x, float y)
{
  float cc, s, temp_r;

  F77_FUNC (slartg, SLARTG) (x, y, cc, s, temp_r);

  FloatMatrix g (2, 2);

  g.elem (0, 0) = cc;
  g.elem (1, 1) = cc;
  g.elem (0, 1) = s;
  g.elem (1, 0) = -s;

  return g;
}

FloatMatrix
Sylvester (const FloatMatrix& a, const FloatMatrix& b, const FloatMatrix& c)
{
  FloatMatrix retval;

  // FIXME: need to check that a, b, and c are all the same size.

  // Compute Schur decompositions.

  octave::math::schur<FloatMatrix> as (a, "U");
  octave::math::schur<FloatMatrix> bs (b, "U");

  // Transform c to new coordinates.

  FloatMatrix ua = as.unitary_schur_matrix ();
  FloatMatrix sch_a = as.schur_matrix ();

  FloatMatrix ub = bs.unitary_schur_matrix ();
  FloatMatrix sch_b = bs.schur_matrix ();

  FloatMatrix cx = ua.transpose () * c * ub;

  // Solve the sylvester equation, back-transform, and return the
  // solution.

  F77_INT a_nr = octave::to_f77_int (a.rows ());
  F77_INT b_nr = octave::to_f77_int (b.rows ());

  float scale;
  F77_INT info;

  float *pa = sch_a.fortran_vec ();
  float *pb = sch_b.fortran_vec ();
  float *px = cx.fortran_vec ();

  F77_XFCN (strsyl, STRSYL, (F77_CONST_CHAR_ARG2 ("N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             1, a_nr, b_nr, pa, a_nr, pb,
                             b_nr, px, a_nr, scale, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // FIXME: check info?

  retval = ua*cx*ub.transpose ();

  return retval;
}

// matrix by matrix -> matrix operations

/*
## Simple Dot Product, Matrix-Vector, and Matrix-Matrix Unit tests
%!assert (single ([1 2 3]) * single ([ 4 ; 5 ; 6]), single (32), 5e-7)
%!assert (single ([1 2 ; 3 4]) * single ([5 ; 6]), single ([17 ; 39]), 5e-7)
%!assert (single ([1 2 ; 3 4]) * single ([5 6 ; 7 8]), single ([19 22; 43 50]), 5e-7)

## Test some simple identities
%!shared M, cv, rv
%! M = single (randn (10,10));
%! cv = single (randn (10,1));
%! rv = single (randn (1,10));
%!assert ([M*cv,M*cv], M*[cv,cv], 5e-6)
%!assert ([M'*cv,M'*cv], M'*[cv,cv], 5e-6)
%!assert ([rv*M;rv*M], [rv;rv]*M, 5e-6)
%!assert ([rv*M';rv*M'], [rv;rv]*M', 5e-6)
%!assert (2*rv*cv, [rv,rv]*[cv;cv], 5e-6)

*/

static char
get_blas_trans_arg (bool trans)
{
  return trans ? 'T' : 'N';
}

// the general GEMM operation

FloatMatrix
xgemm (const FloatMatrix& a, const FloatMatrix& b,
       blas_trans_type transa, blas_trans_type transb)
{
  FloatMatrix retval;

  bool tra = transa != blas_no_trans;
  bool trb = transb != blas_no_trans;

  F77_INT a_nr = octave::to_f77_int (tra ? a.cols () : a.rows ());
  F77_INT a_nc = octave::to_f77_int (tra ? a.rows () : a.cols ());

  F77_INT b_nr = octave::to_f77_int (trb ? b.cols () : b.rows ());
  F77_INT b_nc = octave::to_f77_int (trb ? b.rows () : b.cols ());

  if (a_nc != b_nr)
    octave::err_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);

  if (a_nr == 0 || a_nc == 0 || b_nc == 0)
    retval = FloatMatrix (a_nr, b_nc, 0.0);
  else if (a.data () == b.data () && a_nr == b_nc && tra != trb)
    {
      F77_INT lda = octave::to_f77_int (a.rows ());

      retval = FloatMatrix (a_nr, b_nc);
      float *c = retval.fortran_vec ();

      const char ctra = get_blas_trans_arg (tra);
      F77_XFCN (ssyrk, SSYRK, (F77_CONST_CHAR_ARG2 ("U", 1),
                               F77_CONST_CHAR_ARG2 (&ctra, 1),
                               a_nr, a_nc, 1.0,
                               a.data (), lda, 0.0, c, a_nr
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));
      for (int j = 0; j < a_nr; j++)
        for (int i = 0; i < j; i++)
          retval.xelem (j, i) = retval.xelem (i, j);

    }
  else
    {
      F77_INT lda = octave::to_f77_int (a.rows ());
      F77_INT tda = octave::to_f77_int (a.cols ());
      F77_INT ldb = octave::to_f77_int (b.rows ());
      F77_INT tdb = octave::to_f77_int (b.cols ());

      retval = FloatMatrix (a_nr, b_nc);
      float *c = retval.fortran_vec ();

      if (b_nc == 1)
        {
          if (a_nr == 1)
            F77_FUNC (xsdot, XSDOT) (a_nc, a.data (), 1, b.data (), 1, *c);
          else
            {
              const char ctra = get_blas_trans_arg (tra);
              F77_XFCN (sgemv, SGEMV, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                       lda, tda, 1.0,  a.data (), lda,
                                       b.data (), 1, 0.0, c, 1
                                       F77_CHAR_ARG_LEN (1)));
            }
        }
      else if (a_nr == 1)
        {
          const char crevtrb = get_blas_trans_arg (! trb);
          F77_XFCN (sgemv, SGEMV, (F77_CONST_CHAR_ARG2 (&crevtrb, 1),
                                   ldb, tdb, 1.0,  b.data (), ldb,
                                   a.data (), 1, 0.0, c, 1
                                   F77_CHAR_ARG_LEN (1)));
        }
      else
        {
          const char ctra = get_blas_trans_arg (tra);
          const char ctrb = get_blas_trans_arg (trb);
          F77_XFCN (sgemm, SGEMM, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                   F77_CONST_CHAR_ARG2 (&ctrb, 1),
                                   a_nr, b_nc, a_nc, 1.0, a.data (),
                                   lda, b.data (), ldb, 0.0, c, a_nr
                                   F77_CHAR_ARG_LEN (1)
                                   F77_CHAR_ARG_LEN (1)));
        }
    }

  return retval;
}

FloatMatrix
operator * (const FloatMatrix& a, const FloatMatrix& b)
{
  return xgemm (a, b);
}

// FIXME: it would be nice to share code among the min/max functions below.

#define EMPTY_RETURN_CHECK(T)                   \
  if (nr == 0 || nc == 0)                       \
    return T (nr, nc);

FloatMatrix
min (float d, const FloatMatrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatMatrix);

  FloatMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::min (d, m(i, j));
      }

  return result;
}

FloatMatrix
min (const FloatMatrix& m, float d)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatMatrix);

  FloatMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::min (m(i, j), d);
      }

  return result;
}

FloatMatrix
min (const FloatMatrix& a, const FloatMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    (*current_liboctave_error_handler)
      ("two-arg min requires same size arguments");

  EMPTY_RETURN_CHECK (FloatMatrix);

  FloatMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::min (a(i, j), b(i, j));
      }

  return result;
}

FloatMatrix
max (float d, const FloatMatrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatMatrix);

  FloatMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::max (d, m(i, j));
      }

  return result;
}

FloatMatrix
max (const FloatMatrix& m, float d)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatMatrix);

  FloatMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::max (m(i, j), d);
      }

  return result;
}

FloatMatrix
max (const FloatMatrix& a, const FloatMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    (*current_liboctave_error_handler)
      ("two-arg max requires same size arguments");

  EMPTY_RETURN_CHECK (FloatMatrix);

  FloatMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::max (a(i, j), b(i, j));
      }

  return result;
}

FloatMatrix linspace (const FloatColumnVector& x1,
                      const FloatColumnVector& x2,
                      octave_idx_type n)

{
  octave_idx_type m = x1.numel ();

  if (x2.numel () != m)
    (*current_liboctave_error_handler)
      ("linspace: vectors must be of equal length");

  FloatMatrix retval;

  if (n < 1)
    {
      retval.clear (m, 0);
      return retval;
    }

  retval.clear (m, n);
  for (octave_idx_type i = 0; i < m; i++)
    retval.xelem (i, 0) = x1(i);

  // The last column is unused so temporarily store delta there
  float *delta = &retval.xelem (0, n-1);
  for (octave_idx_type i = 0; i < m; i++)
    delta[i] = (x1(i) == x2(i)) ? 0 : (x2(i) - x1(i)) / (n - 1);

  for (octave_idx_type j = 1; j < n-1; j++)
    for (octave_idx_type i = 0; i < m; i++)
      retval.xelem (i, j) = x1(i) + j*delta[i];

  for (octave_idx_type i = 0; i < m; i++)
    retval.xelem (i, n-1) = x2(i);

  return retval;
}

MS_CMP_OPS (FloatMatrix, float)
MS_BOOL_OPS (FloatMatrix, float)

SM_CMP_OPS (float, FloatMatrix)
SM_BOOL_OPS (float, FloatMatrix)

MM_CMP_OPS (FloatMatrix, FloatMatrix)
MM_BOOL_OPS (FloatMatrix, FloatMatrix)
