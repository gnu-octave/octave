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
#include <complex>
#include <istream>
#include <limits>
#include <ostream>

#include "Array-util.h"
#include "CDiagMatrix.h"
#include "CMatrix.h"
#include "CNDArray.h"
#include "CRowVector.h"
#include "DET.h"
#include "boolMatrix.h"
#include "chMatrix.h"
#include "chol.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "lo-blas-proto.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-lapack-proto.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-cm-dm.h"
#include "mx-cm-s.h"
#include "mx-dm-cm.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"
#include "oct-cmplx.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"
#include "oct-norm.h"
#include "schur.h"
#include "svd.h"

static const Complex Complex_NaN_result (octave::numeric_limits<double>::NaN (),
                                         octave::numeric_limits<double>::NaN ());

// Complex Matrix class

ComplexMatrix::ComplexMatrix (const Matrix& a)
  : ComplexNDArray (a)
{ }

ComplexMatrix::ComplexMatrix (const RowVector& rv)
  : ComplexNDArray (rv)
{ }

ComplexMatrix::ComplexMatrix (const ColumnVector& cv)
  : ComplexNDArray (cv)
{ }

ComplexMatrix::ComplexMatrix (const DiagMatrix& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const MDiagArray2<double>& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const DiagArray2<double>& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const ComplexRowVector& rv)
  : ComplexNDArray (rv)
{ }

ComplexMatrix::ComplexMatrix (const ComplexColumnVector& cv)
  : ComplexNDArray (cv)
{ }

ComplexMatrix::ComplexMatrix (const ComplexDiagMatrix& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const MDiagArray2<Complex>& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const DiagArray2<Complex>& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// FIXME: could we use a templated mixed-type copy function here?

ComplexMatrix::ComplexMatrix (const boolMatrix& a)
  : ComplexNDArray (a)
{ }

ComplexMatrix::ComplexMatrix (const charMatrix& a)
  : ComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    for (octave_idx_type j = 0; j < a.cols (); j++)
      elem (i, j) = static_cast<unsigned char> (a.elem (i, j));
}

ComplexMatrix::ComplexMatrix (const Matrix& re, const Matrix& im)
  : ComplexNDArray (re.dims ())
{
  if (im.rows () != rows () || im.cols () != cols ())
    (*current_liboctave_error_handler) ("complex: internal error");

  octave_idx_type nel = numel ();
  for (octave_idx_type i = 0; i < nel; i++)
    xelem (i) = Complex (re(i), im(i));
}

bool
ComplexMatrix::operator == (const ComplexMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return false;

  return mx_inline_equal (numel (), data (), a.data ());
}

bool
ComplexMatrix::operator != (const ComplexMatrix& a) const
{
  return !(*this == a);
}

bool
ComplexMatrix::ishermitian (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (issquare () && nr > 0)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        for (octave_idx_type j = i; j < nc; j++)
          if (elem (i, j) != conj (elem (j, i)))
            return false;

      return true;
    }

  return false;
}

// destructive insert/delete/reorder operations

ComplexMatrix&
ComplexMatrix::insert (const Matrix& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    (*current_liboctave_error_handler) ("range error for insert");

  if (a_nr >0 && a_nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < a_nc; j++)
        for (octave_idx_type i = 0; i < a_nr; i++)
          xelem (r+i, c+j) = a.elem (i, j);
    }

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const RowVector& a, octave_idx_type r, octave_idx_type c)
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

ComplexMatrix&
ComplexMatrix::insert (const ColumnVector& a,
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

ComplexMatrix&
ComplexMatrix::insert (const DiagMatrix& a,
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

ComplexMatrix&
ComplexMatrix::insert (const ComplexMatrix& a,
                       octave_idx_type r, octave_idx_type c)
{
  ComplexNDArray::insert (a, r, c);
  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexRowVector& a,
                       octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.numel ();
  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    (*current_liboctave_error_handler) ("range error for insert");

  for (octave_idx_type i = 0; i < a_len; i++)
    elem (r, c+i) = a.elem (i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexColumnVector& a,
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

ComplexMatrix&
ComplexMatrix::insert (const ComplexDiagMatrix& a,
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

ComplexMatrix&
ComplexMatrix::fill (double val)
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

ComplexMatrix&
ComplexMatrix::fill (const Complex& val)
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

ComplexMatrix&
ComplexMatrix::fill (double val, octave_idx_type r1, octave_idx_type c1,
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

ComplexMatrix&
ComplexMatrix::fill (const Complex& val, octave_idx_type r1, octave_idx_type c1,
                     octave_idx_type r2, octave_idx_type c2)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    (*current_liboctave_error_handler) ("range error for fill");

  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  if (r2 >= r1 && c2 >=c1)
    {
      make_unique ();

      for (octave_idx_type j = c1; j <= c2; j++)
        for (octave_idx_type i = r1; i <= r2; i++)
          xelem (i, j) = val;
    }

  return *this;
}

ComplexMatrix
ComplexMatrix::append (const Matrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const RowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.numel ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.numel ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const DiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.numel ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.numel ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    (*current_liboctave_error_handler) ("row dimension mismatch for append");

  octave_idx_type nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const Matrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const RowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.numel ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + a.numel (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const DiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.numel ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + a.numel (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    (*current_liboctave_error_handler) ("column dimension mismatch for stack");

  octave_idx_type nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
conj (const ComplexMatrix& a)
{
  return do_mx_unary_map<Complex, Complex, std::conj<double>> (a);
}

// resize is the destructive equivalent for this one

ComplexMatrix
ComplexMatrix::extract (octave_idx_type r1, octave_idx_type c1,
                        octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  return index (octave::idx_vector (r1, r2+1), octave::idx_vector (c1, c2+1));
}

ComplexMatrix
ComplexMatrix::extract_n (octave_idx_type r1, octave_idx_type c1,
                          octave_idx_type nr, octave_idx_type nc) const
{
  return index (octave::idx_vector (r1, r1 + nr), octave::idx_vector (c1, c1 + nc));
}

// extract row or column i.

ComplexRowVector
ComplexMatrix::row (octave_idx_type i) const
{
  return index (octave::idx_vector (i), octave::idx_vector::colon);
}

ComplexColumnVector
ComplexMatrix::column (octave_idx_type i) const
{
  return index (octave::idx_vector::colon, octave::idx_vector (i));
}

// Local function to calculate the 1-norm.
static
double
norm1 (const ComplexMatrix& a)
{
  double anorm = 0.0;
  RowVector colsum = a.abs ().sum ().row (0);

  for (octave_idx_type i = 0; i < colsum.numel (); i++)
    {
      double sum = colsum.xelem (i);
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

ComplexMatrix
ComplexMatrix::inverse (void) const
{
  octave_idx_type info;
  double rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

ComplexMatrix
ComplexMatrix::inverse (octave_idx_type& info) const
{
  double rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

ComplexMatrix
ComplexMatrix::inverse (octave_idx_type& info, double& rcon, bool force,
                        bool calc_cond) const
{
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, force, calc_cond);
}

ComplexMatrix
ComplexMatrix::inverse (MatrixType& mattype) const
{
  octave_idx_type info;
  double rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

ComplexMatrix
ComplexMatrix::inverse (MatrixType& mattype, octave_idx_type& info) const
{
  double rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

ComplexMatrix
ComplexMatrix::tinverse (MatrixType& mattype, octave_idx_type& info,
                         double& rcon, bool force, bool calc_cond) const
{
  ComplexMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");

  int typ = mattype.type ();
  char uplo = (typ == MatrixType::Lower ? 'L' : 'U');
  char udiag = 'N';
  retval = *this;
  Complex *tmp_data = retval.fortran_vec ();

  F77_INT tmp_info = 0;

  F77_XFCN (ztrtri, ZTRTRI, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                             F77_CONST_CHAR_ARG2 (&udiag, 1),
                             nr, F77_DBLE_CMPLX_ARG (tmp_data), nr, tmp_info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  info = tmp_info;

  // Throw away extra info LAPACK gives so as to not change output.
  rcon = 0.0;
  if (info != 0)
    info = -1;
  else if (calc_cond)
    {
      F77_INT ztrcon_info = 0;
      char job = '1';

      OCTAVE_LOCAL_BUFFER (Complex, cwork, 2*nr);
      OCTAVE_LOCAL_BUFFER (double, rwork, nr);

      F77_XFCN (ztrcon, ZTRCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                 F77_CONST_CHAR_ARG2 (&uplo, 1),
                                 F77_CONST_CHAR_ARG2 (&udiag, 1),
                                 nr, F77_DBLE_CMPLX_ARG (tmp_data), nr, rcon,
                                 F77_DBLE_CMPLX_ARG (cwork), rwork, ztrcon_info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      if (ztrcon_info != 0)
        info = -1;
    }

  if (info == -1 && ! force)
    retval = *this; // Restore matrix contents.

  return retval;
}

ComplexMatrix
ComplexMatrix::finverse (MatrixType& mattype, octave_idx_type& info,
                         double& rcon, bool force, bool calc_cond) const
{
  ComplexMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc)
    (*current_liboctave_error_handler) ("inverse requires square matrix");

  Array<F77_INT> ipvt (dim_vector (nr, 1));
  F77_INT *pipvt = ipvt.fortran_vec ();

  retval = *this;
  Complex *tmp_data = retval.fortran_vec ();

  Array<Complex> z (dim_vector (1, 1));
  F77_INT lwork = -1;

  // Query the optimum work array size.

  F77_INT tmp_info = 0;

  F77_XFCN (zgetri, ZGETRI, (nc, F77_DBLE_CMPLX_ARG (tmp_data), nr, pipvt,
                             F77_DBLE_CMPLX_ARG (z.fortran_vec ()), lwork,
                             tmp_info));

  lwork = static_cast<F77_INT> (std::real (z(0)));
  lwork = (lwork < 2 * nc ? 2 * nc : lwork);
  z.resize (dim_vector (lwork, 1));
  Complex *pz = z.fortran_vec ();

  info = 0;
  tmp_info = 0;

  // Calculate norm of the matrix (always, see bug #45577) for later use.
  double anorm = norm1 (retval);

  // Work around bug #45577, LAPACK crashes Octave if norm is NaN
  // and bug #46330, segfault with matrices containing Inf & NaN
  if (octave::math::isnan (anorm) || octave::math::isinf (anorm))
    info = -1;
  else
    {
      F77_XFCN (zgetrf, ZGETRF, (nc, nc, F77_DBLE_CMPLX_ARG (tmp_data), nr,
                                 pipvt, tmp_info));

      info = tmp_info;
    }

  // Throw away extra info LAPACK gives so as to not change output.
  rcon = 0.0;
  if (info != 0)
    info = -1;
  else if (calc_cond)
    {
      if (octave::math::isnan (anorm))
        rcon = octave::numeric_limits<double>::NaN ();
      else
        {
          F77_INT zgecon_info = 0;

          // Now calculate the condition number for non-singular matrix.
          char job = '1';
          Array<double> rz (dim_vector (2 * nc, 1));
          double *prz = rz.fortran_vec ();
          F77_XFCN (zgecon, ZGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                     nc, F77_DBLE_CMPLX_ARG (tmp_data), nr,
                                     anorm, rcon, F77_DBLE_CMPLX_ARG (pz), prz,
                                     zgecon_info F77_CHAR_ARG_LEN (1)));

          if (zgecon_info != 0)
            info = -1;
        }
    }

  if ((info == -1 && ! force)
      || octave::math::isnan (anorm) || octave::math::isinf (anorm))
    retval = *this;  // Restore contents.
  else
    {
      F77_INT zgetri_info = 0;

      F77_XFCN (zgetri, ZGETRI, (nc, F77_DBLE_CMPLX_ARG (tmp_data), nr, pipvt,
                                 F77_DBLE_CMPLX_ARG (pz), lwork, zgetri_info));

      if (zgetri_info != 0)
        info = -1;
    }

  if (info != 0)
    mattype.mark_as_rectangular ();

  return retval;
}

ComplexMatrix
ComplexMatrix::inverse (MatrixType& mattype, octave_idx_type& info,
                        double& rcon, bool force, bool calc_cond) const
{
  int typ = mattype.type (false);
  ComplexMatrix ret;

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  if (typ == MatrixType::Diagonal)  // a scalar is classified as Diagonal.
    {
      Complex scalar = this->elem (0);
      double real = std::real (scalar);
      double imag = std::imag (scalar);

      if (real == 0 && imag == 0)
        ret = ComplexMatrix (1, 1,
                             Complex (octave::numeric_limits<double>::Inf (), 0.0));
      else
        ret = Complex (1, 0) / (*this);

      if (calc_cond)
        {
          if (octave::math::isfinite (real) && octave::math::isfinite (imag)
              && (real != 0 || imag != 0))
            rcon = 1.0;
          else if (octave::math::isinf (real) || octave::math::isinf (imag)
                   || (real == 0 && imag == 0))
            rcon = 0.0;
          else
            rcon = octave::numeric_limits<double>::NaN ();
        }
    }
  else if (typ == MatrixType::Upper || typ == MatrixType::Lower)
    ret = tinverse (mattype, info, rcon, force, calc_cond);
  else
    {
      if (mattype.ishermitian ())
        {
          octave::math::chol<ComplexMatrix> chol (*this, info, true, calc_cond);
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
        {
          ret = ComplexMatrix (rows (), columns (),
                               Complex (octave::numeric_limits<double>::Inf (), 0.0));
        }
    }

  return ret;
}

ComplexMatrix
ComplexMatrix::pseudo_inverse (double tol) const
{
  ComplexMatrix retval;

  octave::math::svd<ComplexMatrix> result (*this,
      octave::math::svd<ComplexMatrix>::Type::economy);

  DiagMatrix S = result.singular_values ();
  ComplexMatrix U = result.left_singular_matrix ();
  ComplexMatrix V = result.right_singular_matrix ();

  ColumnVector sigma = S.extract_diag ();

  octave_idx_type r = sigma.numel () - 1;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (tol <= 0.0)
    {
      tol = std::max (nr, nc) * sigma.elem (0)
            * std::numeric_limits<double>::epsilon ();

      if (tol == 0)
        tol = std::numeric_limits<double>::min ();
    }

  while (r >= 0 && sigma.elem (r) < tol)
    r--;

  if (r < 0)
    retval = ComplexMatrix (nc, nr, 0.0);
  else
    {
      ComplexMatrix Ur = U.extract (0, 0, nr-1, r);
      DiagMatrix D = DiagMatrix (sigma.extract (0, r)).inverse ();
      ComplexMatrix Vr = V.extract (0, 0, nc-1, r);
      retval = Vr * D * Ur.hermitian ();
    }

  return retval;
}

#if defined (HAVE_FFTW)

ComplexMatrix
ComplexMatrix::fourier (void) const
{
  std::size_t nr = rows ();
  std::size_t nc = cols ();

  ComplexMatrix retval (nr, nc);

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

  const Complex *in (data ());
  Complex *out (retval.fortran_vec ());

  octave::fftw::fft (in, out, npts, nsamples);

  return retval;
}

ComplexMatrix
ComplexMatrix::ifourier (void) const
{
  std::size_t nr = rows ();
  std::size_t nc = cols ();

  ComplexMatrix retval (nr, nc);

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

  const Complex *in (data ());
  Complex *out (retval.fortran_vec ());

  octave::fftw::ifft (in, out, npts, nsamples);

  return retval;
}

ComplexMatrix
ComplexMatrix::fourier2d (void) const
{
  dim_vector dv (rows (), cols ());

  ComplexMatrix retval (rows (), cols ());
  const Complex *in (data ());
  Complex *out (retval.fortran_vec ());

  octave::fftw::fftNd (in, out, 2, dv);

  return retval;
}

ComplexMatrix
ComplexMatrix::ifourier2d (void) const
{
  dim_vector dv (rows (), cols ());

  ComplexMatrix retval (rows (), cols ());
  const Complex *in (data ());
  Complex *out (retval.fortran_vec ());

  octave::fftw::ifftNd (in, out, 2, dv);

  return retval;
}

#else

ComplexMatrix
ComplexMatrix::fourier (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexMatrix ();
}

ComplexMatrix
ComplexMatrix::ifourier (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexMatrix ();
}

ComplexMatrix
ComplexMatrix::fourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexMatrix ();
}

ComplexMatrix
ComplexMatrix::ifourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexMatrix ();
}

#endif

ComplexDET
ComplexMatrix::determinant (void) const
{
  octave_idx_type info;
  double rcon;
  return determinant (info, rcon, 0);
}

ComplexDET
ComplexMatrix::determinant (octave_idx_type& info) const
{
  double rcon;
  return determinant (info, rcon, 0);
}

ComplexDET
ComplexMatrix::determinant (octave_idx_type& info, double& rcon,
                            bool calc_cond) const
{
  MatrixType mattype (*this);
  return determinant (mattype, info, rcon, calc_cond);
}

ComplexDET
ComplexMatrix::determinant (MatrixType& mattype,
                            octave_idx_type& info, double& rcon,
                            bool calc_cond) const
{
  ComplexDET retval (1.0);

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
      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      double anorm;
      if (calc_cond)
        anorm = norm1 (*this);

      F77_INT tmp_info = 0;

      char job = 'L';
      F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                 F77_DBLE_CMPLX_ARG (tmp_data), nr, tmp_info
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
              Array<Complex> z (dim_vector (2 * nc, 1));
              Complex *pz = z.fortran_vec ();
              Array<double> rz (dim_vector (nc, 1));
              double *prz = rz.fortran_vec ();

              F77_XFCN (zpocon, ZPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                         nr, F77_DBLE_CMPLX_ARG (tmp_data), nr, anorm,
                                         rcon, F77_DBLE_CMPLX_ARG (pz), prz, tmp_info
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

      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      info = 0;

      // Calculate norm of the matrix (always, see bug #45577) for later use.
      double anorm = norm1 (*this);

      F77_INT tmp_info = 0;

      // Work around bug #45577, LAPACK crashes Octave if norm is NaN
      if (octave::math::isnan (anorm))
        info = -1;
      else
        {
          F77_XFCN (zgetrf, ZGETRF, (nr, nr, F77_DBLE_CMPLX_ARG (tmp_data), nr, pipvt,
                                     tmp_info));

          info = tmp_info;
        }

      // Throw away extra info LAPACK gives so as to not change output.
      rcon = 0.0;
      if (info != 0)
        {
          info = -1;
          retval = ComplexDET ();
        }
      else
        {
          if (calc_cond)
            {
              // Now calc the condition number for non-singular matrix.
              char job = '1';
              Array<Complex> z (dim_vector (2 * nc, 1));
              Complex *pz = z.fortran_vec ();
              Array<double> rz (dim_vector (2 * nc, 1));
              double *prz = rz.fortran_vec ();

              F77_XFCN (zgecon, ZGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                         nc, F77_DBLE_CMPLX_ARG (tmp_data), nr, anorm,
                                         rcon, F77_DBLE_CMPLX_ARG (pz), prz, tmp_info
                                         F77_CHAR_ARG_LEN (1)));

              info = tmp_info;
            }

          if (info != 0)
            {
              info = -1;
              retval = ComplexDET ();
            }
          else
            {
              for (F77_INT i = 0; i < nc; i++)
                {
                  Complex c = atmp(i, i);
                  retval *= (ipvt(i) != (i+1)) ? -c : c;
                }
            }
        }
    }

  return retval;
}

double
ComplexMatrix::rcond (void) const
{
  MatrixType mattype (*this);
  return rcond (mattype);
}

double
ComplexMatrix::rcond (MatrixType& mattype) const
{
  double rcon = octave::numeric_limits<double>::NaN ();
  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");

  if (nr == 0 || nc == 0)
    rcon = octave::numeric_limits<double>::Inf ();
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Unknown)
        typ = mattype.type (*this);

      // Only calculate the condition number for LU/Cholesky
      if (typ == MatrixType::Upper)
        {
          const Complex *tmp_data = data ();
          F77_INT info = 0;
          char norm = '1';
          char uplo = 'U';
          char dia = 'N';

          Array<Complex> z (dim_vector (2 * nc, 1));
          Complex *pz = z.fortran_vec ();
          Array<double> rz (dim_vector (nc, 1));
          double *prz = rz.fortran_vec ();

          F77_XFCN (ztrcon, ZTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, F77_CONST_DBLE_CMPLX_ARG (tmp_data), nr, rcon,
                                     F77_DBLE_CMPLX_ARG (pz), prz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0;
        }
      else if (typ == MatrixType::Permuted_Upper)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Lower)
        {
          const Complex *tmp_data = data ();
          F77_INT info = 0;
          char norm = '1';
          char uplo = 'L';
          char dia = 'N';

          Array<Complex> z (dim_vector (2 * nc, 1));
          Complex *pz = z.fortran_vec ();
          Array<double> rz (dim_vector (nc, 1));
          double *prz = rz.fortran_vec ();

          F77_XFCN (ztrcon, ZTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, F77_CONST_DBLE_CMPLX_ARG (tmp_data), nr, rcon,
                                     F77_DBLE_CMPLX_ARG (pz), prz, info
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
          double anorm = -1.0;

          if (typ == MatrixType::Hermitian)
            {
              F77_INT info = 0;
              char job = 'L';

              ComplexMatrix atmp = *this;
              Complex *tmp_data = atmp.fortran_vec ();

              anorm = norm1 (atmp);

              F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                         F77_DBLE_CMPLX_ARG (tmp_data), nr, info
                                         F77_CHAR_ARG_LEN (1)));

              if (info != 0)
                {
                  rcon = 0.0;

                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
              else
                {
                  Array<Complex> z (dim_vector (2 * nc, 1));
                  Complex *pz = z.fortran_vec ();
                  Array<double> rz (dim_vector (nc, 1));
                  double *prz = rz.fortran_vec ();

                  F77_XFCN (zpocon, ZPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, F77_DBLE_CMPLX_ARG (tmp_data), nr, anorm,
                                             rcon, F77_DBLE_CMPLX_ARG (pz), prz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }

          if (typ == MatrixType::Full)
            {
              F77_INT info = 0;

              ComplexMatrix atmp = *this;
              Complex *tmp_data = atmp.fortran_vec ();

              Array<F77_INT> ipvt (dim_vector (nr, 1));
              F77_INT *pipvt = ipvt.fortran_vec ();

              if (anorm < 0.0)
                anorm = norm1 (atmp);

              Array<Complex> z (dim_vector (2 * nc, 1));
              Complex *pz = z.fortran_vec ();
              Array<double> rz (dim_vector (2 * nc, 1));
              double *prz = rz.fortran_vec ();

              // Work around bug #45577, LAPACK crashes Octave if norm is NaN
              if (octave::math::isnan (anorm))
                info = -1;
              else
                F77_XFCN (zgetrf, ZGETRF, (nr, nr,
                                           F77_DBLE_CMPLX_ARG (tmp_data),
                                           nr, pipvt, info));

              if (info != 0)
                {
                  rcon = 0.0;
                  mattype.mark_as_rectangular ();
                }
              else
                {
                  char job = '1';
                  F77_XFCN (zgecon, ZGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, F77_DBLE_CMPLX_ARG (tmp_data), nr, anorm,
                                             rcon, F77_DBLE_CMPLX_ARG (pz), prz, info
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

ComplexMatrix
ComplexMatrix::utsolve (MatrixType& mattype, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcon,
                        solve_singularity_handler sing_handler,
                        bool calc_cond, blas_trans_type transt) const
{
  ComplexMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (nr == 0 || nc == 0 || b_nc == 0)
    retval = ComplexMatrix (nc, b_nc, Complex (0.0, 0.0));
  else
    {
      volatile int typ = mattype.type ();

      if (typ != MatrixType::Permuted_Upper && typ != MatrixType::Upper)
        (*current_liboctave_error_handler) ("incorrect matrix type");

      rcon = 1.0;
      info = 0;

      if (typ == MatrixType::Permuted_Upper)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");

      const Complex *tmp_data = data ();

      retval = b;
      Complex *result = retval.fortran_vec ();

      char uplo = 'U';
      char trans = get_blas_char (transt);
      char dia = 'N';

      F77_INT tmp_info = 0;

      F77_XFCN (ztrtrs, ZTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                 F77_CONST_CHAR_ARG2 (&trans, 1),
                                 F77_CONST_CHAR_ARG2 (&dia, 1),
                                 nr, b_nc, F77_CONST_DBLE_CMPLX_ARG (tmp_data), nr,
                                 F77_DBLE_CMPLX_ARG (result), nr, tmp_info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      info = tmp_info;

      if (calc_cond)
        {
          char norm = '1';
          uplo = 'U';
          dia = 'N';

          Array<Complex> z (dim_vector (2 * nc, 1));
          Complex *pz = z.fortran_vec ();
          Array<double> rz (dim_vector (nc, 1));
          double *prz = rz.fortran_vec ();

          F77_XFCN (ztrcon, ZTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, F77_CONST_DBLE_CMPLX_ARG (tmp_data), nr, rcon,
                                     F77_DBLE_CMPLX_ARG (pz), prz, tmp_info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          info = tmp_info;

          if (info != 0)
            info = -2;

          volatile double rcond_plus_one = rcon + 1.0;

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

  return retval;
}

ComplexMatrix
ComplexMatrix::ltsolve (MatrixType& mattype, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcon,
                        solve_singularity_handler sing_handler,
                        bool calc_cond, blas_trans_type transt) const
{
  ComplexMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (nr == 0 || nc == 0 || b_nc == 0)
    retval = ComplexMatrix (nc, b_nc, Complex (0.0, 0.0));
  else
    {
      volatile int typ = mattype.type ();

      if (typ != MatrixType::Permuted_Lower && typ != MatrixType::Lower)
        (*current_liboctave_error_handler) ("incorrect matrix type");

      rcon = 1.0;
      info = 0;

      if (typ == MatrixType::Permuted_Lower)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");

      const Complex *tmp_data = data ();

      retval = b;
      Complex *result = retval.fortran_vec ();

      char uplo = 'L';
      char trans = get_blas_char (transt);
      char dia = 'N';

      F77_INT tmp_info = 0;

      F77_XFCN (ztrtrs, ZTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                 F77_CONST_CHAR_ARG2 (&trans, 1),
                                 F77_CONST_CHAR_ARG2 (&dia, 1),
                                 nr, b_nc, F77_CONST_DBLE_CMPLX_ARG (tmp_data), nr,
                                 F77_DBLE_CMPLX_ARG (result), nr, tmp_info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      info = tmp_info;

      if (calc_cond)
        {
          char norm = '1';
          uplo = 'L';
          dia = 'N';

          Array<Complex> z (dim_vector (2 * nc, 1));
          Complex *pz = z.fortran_vec ();
          Array<double> rz (dim_vector (nc, 1));
          double *prz = rz.fortran_vec ();

          F77_XFCN (ztrcon, ZTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, F77_CONST_DBLE_CMPLX_ARG (tmp_data), nr, rcon,
                                     F77_DBLE_CMPLX_ARG (pz), prz, tmp_info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          info = tmp_info;

          if (info != 0)
            info = -2;

          volatile double rcond_plus_one = rcon + 1.0;

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

  return retval;
}

ComplexMatrix
ComplexMatrix::fsolve (MatrixType& mattype, const ComplexMatrix& b,
                       octave_idx_type& info, double& rcon,
                       solve_singularity_handler sing_handler,
                       bool calc_cond) const
{
  ComplexMatrix retval;

  F77_INT nr = octave::to_f77_int (rows ());
  F77_INT nc = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (nr != nc || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (nr == 0 || b_nc == 0)
    retval = ComplexMatrix (nc, b_nc, Complex (0.0, 0.0));
  else
    {
      volatile int typ = mattype.type ();

      // Calculate the norm of the matrix for later use when determining rcon.
      double anorm = -1.0;

      if (typ == MatrixType::Hermitian)
        {
          info = 0;
          char job = 'L';

          ComplexMatrix atmp = *this;
          Complex *tmp_data = atmp.fortran_vec ();

          // The norm of the matrix for later use when determining rcon.
          if (calc_cond)
            anorm = norm1 (atmp);

          F77_INT tmp_info = 0;

          F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                     F77_DBLE_CMPLX_ARG (tmp_data), nr, tmp_info
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
                  Array<Complex> z (dim_vector (2 * nc, 1));
                  Complex *pz = z.fortran_vec ();
                  Array<double> rz (dim_vector (nc, 1));
                  double *prz = rz.fortran_vec ();

                  F77_XFCN (zpocon, ZPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, F77_DBLE_CMPLX_ARG (tmp_data), nr, anorm,
                                             rcon, F77_DBLE_CMPLX_ARG (pz), prz, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;

                  if (info != 0)
                    info = -2;

                  volatile double rcond_plus_one = rcon + 1.0;

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
                  Complex *result = retval.fortran_vec ();

                  F77_XFCN (zpotrs, ZPOTRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, F77_DBLE_CMPLX_ARG (tmp_data), nr,
                                             F77_DBLE_CMPLX_ARG (result), b_nr, tmp_info
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

          ComplexMatrix atmp = *this;
          Complex *tmp_data = atmp.fortran_vec ();

          Array<Complex> z (dim_vector (2 * nc, 1));
          Complex *pz = z.fortran_vec ();
          Array<double> rz (dim_vector (2 * nc, 1));
          double *prz = rz.fortran_vec ();

          // Calculate the norm of the matrix, for later use.
          if (calc_cond && anorm < 0.0)
            anorm = norm1 (atmp);

          F77_INT tmp_info = 0;

          // Work around bug #45577, LAPACK crashes Octave if norm is NaN
          // and bug #46330, segfault with matrices containing Inf & NaN
          if (octave::math::isnan (anorm) || octave::math::isinf (anorm))
            info = -2;
          else
            {
              F77_XFCN (zgetrf, ZGETRF, (nr, nr, F77_DBLE_CMPLX_ARG (tmp_data),
                                         nr, pipvt, tmp_info));

              info = tmp_info;
            }

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
                  F77_XFCN (zgecon, ZGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, F77_DBLE_CMPLX_ARG (tmp_data), nr, anorm,
                                             rcon, F77_DBLE_CMPLX_ARG (pz), prz, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;

                  if (info != 0)
                    info = -2;

                  volatile double rcond_plus_one = rcon + 1.0;

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
                  Complex *result = retval.fortran_vec ();

                  char job = 'N';
                  F77_XFCN (zgetrs, ZGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, F77_DBLE_CMPLX_ARG (tmp_data), nr,
                                             pipvt, F77_DBLE_CMPLX_ARG (result), b_nr, tmp_info
                                             F77_CHAR_ARG_LEN (1)));

                  info = tmp_info;
                }
              else
                mattype.mark_as_rectangular ();
            }
        }

      if (octave::math::isinf (anorm))
        {
          retval = ComplexMatrix (b_nr, b_nc, Complex (0, 0));
          mattype.mark_as_full ();
        }
    }

  return retval;
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const Matrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const Matrix& b,
                      octave_idx_type& info) const
{
  double rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const Matrix& b,
                      octave_idx_type& info, double& rcon) const
{
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const Matrix& b,
                      octave_idx_type& info, double& rcon,
                      solve_singularity_handler sing_handler,
                      bool singular_fallback, blas_trans_type transt) const
{
  ComplexMatrix tmp (b);
  return solve (mattype, tmp, info, rcon, sing_handler, singular_fallback,
                transt);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const ComplexMatrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const ComplexMatrix& b,
                      octave_idx_type& info) const
{
  double rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const ComplexMatrix& b,
                      octave_idx_type& info, double& rcon) const
{
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (MatrixType& mattype, const ComplexMatrix& b,
                      octave_idx_type& info, double& rcon,
                      solve_singularity_handler sing_handler,
                      bool singular_fallback, blas_trans_type transt) const
{
  ComplexMatrix retval;
  int typ = mattype.type ();

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  // Only calculate the condition number for LU/Cholesky
  if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    retval = utsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    retval = ltsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (transt == blas_trans)
    return transpose ().solve (mattype, b, info, rcon, sing_handler,
                               singular_fallback);
  else if (transt == blas_conj_trans)
    retval = hermitian ().solve (mattype, b, info, rcon, sing_handler,
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

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ColumnVector& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (mattype, ComplexColumnVector (b), info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ColumnVector& b,
                      octave_idx_type& info) const
{
  double rcon;
  return solve (mattype, ComplexColumnVector (b), info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ColumnVector& b,
                      octave_idx_type& info, double& rcon) const
{
  return solve (mattype, ComplexColumnVector (b), info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ColumnVector& b,
                      octave_idx_type& info, double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt) const
{
  return solve (mattype, ComplexColumnVector (b), info, rcon, sing_handler,
                transt);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ComplexColumnVector& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ComplexColumnVector& b,
                      octave_idx_type& info) const
{
  double rcon;
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ComplexColumnVector& b,
                      octave_idx_type& info, double& rcon) const
{
  return solve (mattype, b, info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (MatrixType& mattype, const ComplexColumnVector& b,
                      octave_idx_type& info, double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt) const
{

  ComplexMatrix tmp (b);
  tmp = solve (mattype, tmp, info, rcon, sing_handler, true, transt);
  return tmp.column (static_cast<octave_idx_type> (0));
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, octave_idx_type& info) const
{
  double rcon;
  return solve (b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, octave_idx_type& info,
                      double& rcon) const
{
  return solve (b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, octave_idx_type& info, double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt) const
{
  ComplexMatrix tmp (b);
  return solve (tmp, info, rcon, sing_handler, transt);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, octave_idx_type& info) const
{
  double rcon;
  return solve (b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, octave_idx_type& info,
                      double& rcon) const
{
  return solve (b, info, rcon, nullptr);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, octave_idx_type& info,
                      double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, true, transt);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (ComplexColumnVector (b), info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, octave_idx_type& info) const
{
  double rcon;
  return solve (ComplexColumnVector (b), info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, octave_idx_type& info,
                      double& rcon) const
{
  return solve (ComplexColumnVector (b), info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, octave_idx_type& info,
                      double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt) const
{
  return solve (ComplexColumnVector (b), info, rcon, sing_handler, transt);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (b, info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, octave_idx_type& info) const
{
  double rcon;
  return solve (b, info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, octave_idx_type& info,
                      double& rcon) const
{
  return solve (b, info, rcon, nullptr);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, octave_idx_type& info,
                      double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, transt);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return lssolve (ComplexMatrix (b), info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  double rcon;
  return lssolve (ComplexMatrix (b), info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, octave_idx_type& info,
                        octave_idx_type& rank) const
{
  double rcon;
  return lssolve (ComplexMatrix (b), info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, octave_idx_type& info,
                        octave_idx_type& rank, double& rcon) const
{
  return lssolve (ComplexMatrix (b), info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b, octave_idx_type& info,
                        octave_idx_type& rank) const
{
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b, octave_idx_type& info,
                        octave_idx_type& rank, double& rcon) const
{
  ComplexMatrix retval;

  F77_INT m = octave::to_f77_int (rows ());
  F77_INT n = octave::to_f77_int (cols ());

  F77_INT b_nr = octave::to_f77_int (b.rows ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());
  F77_INT nrhs = b_nc;  // alias for code readability

  if (m != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (m == 0 || n == 0 || b_nc == 0)
    retval = ComplexMatrix (n, b_nc, Complex (0.0, 0.0));
  else
    {
      volatile F77_INT minmn = (m < n ? m : n);
      F77_INT maxmn = (m > n ? m : n);
      rcon = -1.0;

      if (m != n)
        {
          retval = ComplexMatrix (maxmn, nrhs);

          for (F77_INT j = 0; j < nrhs; j++)
            for (F77_INT i = 0; i < m; i++)
              retval.elem (i, j) = b.elem (i, j);
        }
      else
        retval = b;

      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      Complex *pretval = retval.fortran_vec ();
      Array<double> s (dim_vector (minmn, 1));
      double *ps = s.fortran_vec ();

      // Ask ZGELSD what the dimension of WORK should be.
      F77_INT lwork = -1;

      Array<Complex> work (dim_vector (1, 1));

      F77_INT smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("ZGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      F77_INT mnthr;
      F77_FUNC (xilaenv, XILAENV) (6, F77_CONST_CHAR_ARG2 ("ZGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   m, n, nrhs, -1, mnthr
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of rwork and iwork because ZGELSD in
      // older versions of LAPACK does not return them on a query
      // call.
      double dminmn = static_cast<double> (minmn);
      double dsmlsizp1 = static_cast<double> (smlsiz+1);
      double tmp = octave::math::log2 (dminmn / dsmlsizp1);

      F77_INT nlvl = static_cast<F77_INT> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      F77_INT lrwork = minmn*(10 + 2*smlsiz + 8*nlvl)
                       + 3*smlsiz*nrhs
                       + std::max ((smlsiz+1)*(smlsiz+1),
                                   n*(1+nrhs) + 2*nrhs);
      if (lrwork < 1)
        lrwork = 1;
      Array<double> rwork (dim_vector (lrwork, 1));
      double *prwork = rwork.fortran_vec ();

      F77_INT liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<F77_INT> iwork (dim_vector (liwork, 1));
      F77_INT *piwork = iwork.fortran_vec ();

      F77_INT tmp_info = 0;
      F77_INT tmp_rank = 0;

      F77_XFCN (zgelsd, ZGELSD, (m, n, nrhs, F77_DBLE_CMPLX_ARG (tmp_data), m,
                                 F77_DBLE_CMPLX_ARG (pretval), maxmn,
                                 ps, rcon, tmp_rank, F77_DBLE_CMPLX_ARG (work.fortran_vec ()),
                                 lwork, prwork, piwork, tmp_info));

      info = tmp_info;
      rank = tmp_rank;

      // The workspace query is broken in at least LAPACK 3.0.0
      // through 3.1.1 when n >= mnthr.  The obtuse formula below
      // should provide sufficient workspace for ZGELSD to operate
      // efficiently.
      if (n > m && n >= mnthr)
        {
          F77_INT addend = m;

          if (2*m-4 > addend)
            addend = 2*m-4;

          if (nrhs > addend)
            addend = nrhs;

          if (n-3*m > addend)
            addend = n-3*m;

          const F77_INT lworkaround = 4*m + m*m + addend;

          if (std::real (work(0)) < lworkaround)
            work(0) = lworkaround;
        }
      else if (m >= n)
        {
          F77_INT lworkaround = 2*m + m*nrhs;

          if (std::real (work(0)) < lworkaround)
            work(0) = lworkaround;
        }

      lwork = static_cast<F77_INT> (std::real (work(0)));
      work.resize (dim_vector (lwork, 1));

      double anorm = norm1 (*this);

      if (octave::math::isinf (anorm))
        {
          rcon = 0.0;
          retval = ComplexMatrix (n, b_nc, 0.0);
        }
      else if (octave::math::isnan (anorm))
        {
          rcon = octave::numeric_limits<double>::NaN ();
          retval = ComplexMatrix (n, b_nc,
                                  octave::numeric_limits<double>::NaN ());
        }
      else
        {
          F77_XFCN (zgelsd, ZGELSD, (m, n, nrhs, F77_DBLE_CMPLX_ARG (tmp_data),
                                     m, F77_DBLE_CMPLX_ARG (pretval),
                                     maxmn, ps, rcon, tmp_rank,
                                     F77_DBLE_CMPLX_ARG (work.fortran_vec ()),
                                     lwork, prwork, piwork, tmp_info));

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

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return lssolve (ComplexColumnVector (b), info, rank, rcon);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  double rcon;
  return lssolve (ComplexColumnVector (b), info, rank, rcon);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, octave_idx_type& info,
                        octave_idx_type& rank) const
{
  double rcon;
  return lssolve (ComplexColumnVector (b), info, rank, rcon);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, octave_idx_type& info,
                        octave_idx_type& rank, double& rcon) const
{
  return lssolve (ComplexColumnVector (b), info, rank, rcon);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b,
                        octave_idx_type& info) const
{
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info,
                        octave_idx_type& rank) const
{
  double rcon;
  return lssolve (b, info, rank, rcon);

}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info,
                        octave_idx_type& rank, double& rcon) const
{
  ComplexColumnVector retval;

  F77_INT nrhs = 1;

  F77_INT m = octave::to_f77_int (rows ());
  F77_INT n = octave::to_f77_int (cols ());

  F77_INT b_nel = octave::to_f77_int (b.numel ());

  if (m != b_nel)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");

  if (m == 0 || n == 0)
    retval = ComplexColumnVector (n, Complex (0.0, 0.0));
  else
    {
      volatile F77_INT minmn = (m < n ? m : n);
      F77_INT maxmn = (m > n ? m : n);
      rcon = -1.0;

      if (m != n)
        {
          retval = ComplexColumnVector (maxmn);

          for (F77_INT i = 0; i < m; i++)
            retval.elem (i) = b.elem (i);
        }
      else
        retval = b;

      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      Complex *pretval = retval.fortran_vec ();
      Array<double> s (dim_vector (minmn, 1));
      double *ps = s.fortran_vec ();

      // Ask ZGELSD what the dimension of WORK should be.
      F77_INT lwork = -1;

      Array<Complex> work (dim_vector (1, 1));

      F77_INT smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("ZGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of rwork and iwork because ZGELSD in
      // older versions of LAPACK does not return them on a query
      // call.
      double dminmn = static_cast<double> (minmn);
      double dsmlsizp1 = static_cast<double> (smlsiz+1);
      double tmp = octave::math::log2 (dminmn / dsmlsizp1);

      F77_INT nlvl = static_cast<F77_INT> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      F77_INT lrwork = minmn*(10 + 2*smlsiz + 8*nlvl)
                       + 3*smlsiz*nrhs + (smlsiz+1)*(smlsiz+1);
      if (lrwork < 1)
        lrwork = 1;
      Array<double> rwork (dim_vector (lrwork, 1));
      double *prwork = rwork.fortran_vec ();

      F77_INT liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<F77_INT> iwork (dim_vector (liwork, 1));
      F77_INT *piwork = iwork.fortran_vec ();

      F77_INT tmp_info = 0;
      F77_INT tmp_rank = 0;

      F77_XFCN (zgelsd, ZGELSD, (m, n, nrhs, F77_DBLE_CMPLX_ARG (tmp_data), m,
                                 F77_DBLE_CMPLX_ARG (pretval), maxmn,
                                 ps, rcon, tmp_rank, F77_DBLE_CMPLX_ARG (work.fortran_vec ()),
                                 lwork, prwork, piwork, tmp_info));

      info = tmp_info;
      rank = tmp_rank;

      lwork = static_cast<F77_INT> (std::real (work(0)));
      work.resize (dim_vector (lwork, 1));
      rwork.resize (dim_vector (static_cast<F77_INT> (rwork(0)), 1));
      iwork.resize (dim_vector (iwork(0), 1));

      F77_XFCN (zgelsd, ZGELSD, (m, n, nrhs, F77_DBLE_CMPLX_ARG (tmp_data), m,
                                 F77_DBLE_CMPLX_ARG (pretval),
                                 maxmn, ps, rcon, tmp_rank,
                                 F77_DBLE_CMPLX_ARG (work.fortran_vec ()), lwork,
                                 prwork, piwork, tmp_info));

      info = tmp_info;
      rank = tmp_rank;

      if (rank < minmn)
        {
          if (s.elem (0) == 0.0)
            rcon = 0.0;
          else
            rcon = s.elem (minmn - 1) / s.elem (0);

          retval.resize (n);
        }
    }

  return retval;
}

// column vector by row vector -> matrix operations

ComplexMatrix
operator * (const ColumnVector& v, const ComplexRowVector& a)
{
  ComplexColumnVector tmp (v);
  return tmp * a;
}

ComplexMatrix
operator * (const ComplexColumnVector& a, const RowVector& b)
{
  ComplexRowVector tmp (b);
  return a * tmp;
}

ComplexMatrix
operator * (const ComplexColumnVector& v, const ComplexRowVector& a)
{
  ComplexMatrix retval;

  F77_INT len = octave::to_f77_int (v.numel ());

  if (len != 0)
    {
      F77_INT a_len = octave::to_f77_int (a.numel ());

      retval = ComplexMatrix (len, a_len);
      Complex *c = retval.fortran_vec ();

      F77_XFCN (zgemm, ZGEMM, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 ("N", 1),
                               len, a_len, 1, 1.0, F77_CONST_DBLE_CMPLX_ARG (v.data ()), len,
                               F77_CONST_DBLE_CMPLX_ARG (a.data ()), 1, 0.0, F77_DBLE_CMPLX_ARG (c), len
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));
    }

  return retval;
}

// matrix by diagonal matrix -> matrix operations

ComplexMatrix&
ComplexMatrix::operator += (const DiagMatrix& a)
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

ComplexMatrix&
ComplexMatrix::operator -= (const DiagMatrix& a)
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

ComplexMatrix&
ComplexMatrix::operator += (const ComplexDiagMatrix& a)
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

ComplexMatrix&
ComplexMatrix::operator -= (const ComplexDiagMatrix& a)
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

// matrix by matrix -> matrix operations

ComplexMatrix&
ComplexMatrix::operator += (const Matrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    octave::err_nonconformant ("operator +=", nr, nc, a_nr, a_nc);

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  mx_inline_add2 (numel (), d, a.data ());
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const Matrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    octave::err_nonconformant ("operator -=", nr, nc, a_nr, a_nc);

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  mx_inline_sub2 (numel (), d, a.data ());
  return *this;
}

// other operations

boolMatrix
ComplexMatrix::all (int dim) const
{
  return ComplexNDArray::all (dim);
}

boolMatrix
ComplexMatrix::any (int dim) const
{
  return ComplexNDArray::any (dim);
}

ComplexMatrix
ComplexMatrix::cumprod (int dim) const
{
  return ComplexNDArray::cumprod (dim);
}

ComplexMatrix
ComplexMatrix::cumsum (int dim) const
{
  return ComplexNDArray::cumsum (dim);
}

ComplexMatrix
ComplexMatrix::prod (int dim) const
{
  return ComplexNDArray::prod (dim);
}

ComplexMatrix
ComplexMatrix::sum (int dim) const
{
  return ComplexNDArray::sum (dim);
}

ComplexMatrix
ComplexMatrix::sumsq (int dim) const
{
  return ComplexNDArray::sumsq (dim);
}

Matrix
ComplexMatrix::abs (void) const
{
  return ComplexNDArray::abs ();
}

ComplexMatrix
ComplexMatrix::diag (octave_idx_type k) const
{
  return ComplexNDArray::diag (k);
}

ComplexDiagMatrix
ComplexMatrix::diag (octave_idx_type m, octave_idx_type n) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != 1 && nc != 1)
    (*current_liboctave_error_handler) ("diag: expecting vector argument");

  return ComplexDiagMatrix (*this, m, n);
}

bool
ComplexMatrix::row_is_real_only (octave_idx_type i) const
{
  bool retval = true;

  octave_idx_type nc = columns ();

  for (octave_idx_type j = 0; j < nc; j++)
    {
      if (std::imag (elem (i, j)) != 0.0)
        {
          retval = false;
          break;
        }
    }

  return retval;
}

bool
ComplexMatrix::column_is_real_only (octave_idx_type j) const
{
  bool retval = true;

  octave_idx_type nr = rows ();

  for (octave_idx_type i = 0; i < nr; i++)
    {
      if (std::imag (elem (i, j)) != 0.0)
        {
          retval = false;
          break;
        }
    }

  return retval;
}

ComplexColumnVector
ComplexMatrix::row_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_min (dummy_idx);
}

ComplexColumnVector
ComplexMatrix::row_min (Array<octave_idx_type>& idx_arg) const
{
  ComplexColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          bool real_only = row_is_real_only (i);

          octave_idx_type idx_j;

          Complex tmp_min;

          double abs_min = octave::numeric_limits<double>::NaN ();

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_min = elem (i, idx_j);

              if (! octave::math::isnan (tmp_min))
                {
                  abs_min = (real_only ? tmp_min.real ()
                                       : std::abs (tmp_min));
                  break;
                }
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              Complex tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;

              double abs_tmp = (real_only ? tmp.real () : std::abs (tmp));

              if (abs_tmp < abs_min)
                {
                  idx_j = j;
                  tmp_min = tmp;
                  abs_min = abs_tmp;
                }
            }

          if (octave::math::isnan (tmp_min))
            {
              result.elem (i) = Complex_NaN_result;
              idx_arg.elem (i) = 0;
            }
          else
            {
              result.elem (i) = tmp_min;
              idx_arg.elem (i) = idx_j;
            }
        }
    }

  return result;
}

ComplexColumnVector
ComplexMatrix::row_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_max (dummy_idx);
}

ComplexColumnVector
ComplexMatrix::row_max (Array<octave_idx_type>& idx_arg) const
{
  ComplexColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          bool real_only = row_is_real_only (i);

          octave_idx_type idx_j;

          Complex tmp_max;

          double abs_max = octave::numeric_limits<double>::NaN ();

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_max = elem (i, idx_j);

              if (! octave::math::isnan (tmp_max))
                {
                  abs_max = (real_only ? tmp_max.real ()
                                       : std::abs (tmp_max));
                  break;
                }
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              Complex tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;

              double abs_tmp = (real_only ? tmp.real () : std::abs (tmp));

              if (abs_tmp > abs_max)
                {
                  idx_j = j;
                  tmp_max = tmp;
                  abs_max = abs_tmp;
                }
            }

          if (octave::math::isnan (tmp_max))
            {
              result.elem (i) = Complex_NaN_result;
              idx_arg.elem (i) = 0;
            }
          else
            {
              result.elem (i) = tmp_max;
              idx_arg.elem (i) = idx_j;
            }
        }
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_min (dummy_idx);
}

ComplexRowVector
ComplexMatrix::column_min (Array<octave_idx_type>& idx_arg) const
{
  ComplexRowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          bool real_only = column_is_real_only (j);

          octave_idx_type idx_i;

          Complex tmp_min;

          double abs_min = octave::numeric_limits<double>::NaN ();

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_min = elem (idx_i, j);

              if (! octave::math::isnan (tmp_min))
                {
                  abs_min = (real_only ? tmp_min.real ()
                                       : std::abs (tmp_min));
                  break;
                }
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              Complex tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;

              double abs_tmp = (real_only ? tmp.real () : std::abs (tmp));

              if (abs_tmp < abs_min)
                {
                  idx_i = i;
                  tmp_min = tmp;
                  abs_min = abs_tmp;
                }
            }

          if (octave::math::isnan (tmp_min))
            {
              result.elem (j) = Complex_NaN_result;
              idx_arg.elem (j) = 0;
            }
          else
            {
              result.elem (j) = tmp_min;
              idx_arg.elem (j) = idx_i;
            }
        }
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_max (dummy_idx);
}

ComplexRowVector
ComplexMatrix::column_max (Array<octave_idx_type>& idx_arg) const
{
  ComplexRowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          bool real_only = column_is_real_only (j);

          octave_idx_type idx_i;

          Complex tmp_max;

          double abs_max = octave::numeric_limits<double>::NaN ();

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_max = elem (idx_i, j);

              if (! octave::math::isnan (tmp_max))
                {
                  abs_max = (real_only ? tmp_max.real ()
                                       : std::abs (tmp_max));
                  break;
                }
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              Complex tmp = elem (i, j);

              if (octave::math::isnan (tmp))
                continue;

              double abs_tmp = (real_only ? tmp.real () : std::abs (tmp));

              if (abs_tmp > abs_max)
                {
                  idx_i = i;
                  tmp_max = tmp;
                  abs_max = abs_tmp;
                }
            }

          if (octave::math::isnan (tmp_max))
            {
              result.elem (j) = Complex_NaN_result;
              idx_arg.elem (j) = 0;
            }
          else
            {
              result.elem (j) = tmp_max;
              idx_arg.elem (j) = idx_i;
            }
        }
    }

  return result;
}

// i/o

std::ostream&
operator << (std::ostream& os, const ComplexMatrix& a)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    {
      for (octave_idx_type j = 0; j < a.cols (); j++)
        {
          os << ' ';
          octave::write_value<Complex> (os, a.elem (i, j));
        }
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, ComplexMatrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr > 0 && nc > 0)
    {
      Complex tmp;
      for (octave_idx_type i = 0; i < nr; i++)
        for (octave_idx_type j = 0; j < nc; j++)
          {
            tmp = octave::read_value<Complex> (is);
            if (is)
              a.elem (i, j) = tmp;
            else
              return is;
          }
    }

  return is;
}

ComplexMatrix
Givens (const Complex& x, const Complex& y)
{
  double cc;
  Complex cs, temp_r;

  F77_FUNC (zlartg, ZLARTG) (F77_CONST_DBLE_CMPLX_ARG (&x),
                             F77_CONST_DBLE_CMPLX_ARG (&y),
                             cc,
                             F77_DBLE_CMPLX_ARG (&cs),
                             F77_DBLE_CMPLX_ARG (&temp_r));

  ComplexMatrix g (2, 2);

  g.elem (0, 0) = cc;
  g.elem (1, 1) = cc;
  g.elem (0, 1) = cs;
  g.elem (1, 0) = -conj (cs);

  return g;
}

ComplexMatrix
Sylvester (const ComplexMatrix& a, const ComplexMatrix& b,
           const ComplexMatrix& c)
{
  ComplexMatrix retval;

  // FIXME: need to check that a, b, and c are all the same size.

  // Compute Schur decompositions

  octave::math::schur<ComplexMatrix> as (a, "U");
  octave::math::schur<ComplexMatrix> bs (b, "U");

  // Transform c to new coordinates.

  ComplexMatrix ua = as.unitary_schur_matrix ();
  ComplexMatrix sch_a = as.schur_matrix ();

  ComplexMatrix ub = bs.unitary_schur_matrix ();
  ComplexMatrix sch_b = bs.schur_matrix ();

  ComplexMatrix cx = ua.hermitian () * c * ub;

  // Solve the sylvester equation, back-transform, and return the solution.

  F77_INT a_nr = octave::to_f77_int (a.rows ());
  F77_INT b_nr = octave::to_f77_int (b.rows ());

  double scale;
  F77_INT info;

  Complex *pa = sch_a.fortran_vec ();
  Complex *pb = sch_b.fortran_vec ();
  Complex *px = cx.fortran_vec ();

  F77_XFCN (ztrsyl, ZTRSYL, (F77_CONST_CHAR_ARG2 ("N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             1, a_nr, b_nr, F77_DBLE_CMPLX_ARG (pa), a_nr, F77_DBLE_CMPLX_ARG (pb),
                             b_nr, F77_DBLE_CMPLX_ARG (px), a_nr, scale, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // FIXME: check info?

  retval = ua * cx * ub.hermitian ();

  return retval;
}

ComplexMatrix
operator * (const ComplexMatrix& m, const Matrix& a)
{
  if (m.columns () > std::min (m.rows (), a.columns ()) / 10)
    return ComplexMatrix (real (m) * a, imag (m) * a);
  else
    return m * ComplexMatrix (a);
}

ComplexMatrix
operator * (const Matrix& m, const ComplexMatrix& a)
{
  if (a.rows () > std::min (m.rows (), a.columns ()) / 10)
    return ComplexMatrix (m * real (a), m * imag (a));
  else
    return ComplexMatrix (m) * a;
}

/*

## Simple Dot Product, Matrix-Vector, and Matrix-Matrix Unit tests
%!assert ([1+i 2+i 3+i] * [ 4+i ; 5+i ; 6+i], 29+21i, 1e-14)
%!assert ([1+i 2+i ; 3+i 4+i ] * [5+i ; 6+i], [15 + 14i ; 37 + 18i], 1e-14)
%!assert ([1+i 2+i ; 3+i 4+i ] * [5+i 6+i ; 7+i 8+i], [17 + 15i 20 + 17i; 41 + 19i 48 + 21i], 1e-14)
%!assert ([1 i]*[i 0]', -i)

## Test some simple identities
%!shared M, cv, rv
%! M = randn (10,10) + i*rand (10,10);
%! cv = randn (10,1) + i*rand (10,1);
%! rv = randn (1,10) + i*rand (1,10);
%!assert ([M*cv,M*cv], M*[cv,cv], 1e-14)
%!assert ([M.'*cv,M.'*cv], M.'*[cv,cv], 1e-14)
%!assert ([M'*cv,M'*cv], M'*[cv,cv], 1e-14)
%!assert ([rv*M;rv*M], [rv;rv]*M, 1e-14)
%!assert ([rv*M.';rv*M.'], [rv;rv]*M.', 1e-14)
%!assert ([rv*M';rv*M'], [rv;rv]*M', 1e-14)
%!assert (2*rv*cv, [rv,rv]*[cv;cv], 2e-14)

*/

static inline char
get_blas_trans_arg (bool trans, bool conj)
{
  return trans ? (conj ? 'C' : 'T') : 'N';
}

// the general GEMM operation

ComplexMatrix
xgemm (const ComplexMatrix& a, const ComplexMatrix& b,
       blas_trans_type transa, blas_trans_type transb)
{
  ComplexMatrix retval;

  bool tra = transa != blas_no_trans;
  bool trb = transb != blas_no_trans;
  bool cja = transa == blas_conj_trans;
  bool cjb = transb == blas_conj_trans;

  F77_INT a_nr = octave::to_f77_int (tra ? a.cols () : a.rows ());
  F77_INT a_nc = octave::to_f77_int (tra ? a.rows () : a.cols ());

  F77_INT b_nr = octave::to_f77_int (trb ? b.cols () : b.rows ());
  F77_INT b_nc = octave::to_f77_int (trb ? b.rows () : b.cols ());

  if (a_nc != b_nr)
    octave::err_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);

  if (a_nr == 0 || a_nc == 0 || b_nc == 0)
    retval = ComplexMatrix (a_nr, b_nc, 0.0);
  else if (a.data () == b.data () && a_nr == b_nc && tra != trb)
    {
      F77_INT lda = octave::to_f77_int (a.rows ());

      // FIXME: looking at the reference BLAS, it appears that it
      // should not be necessary to initialize the output matrix if
      // BETA is 0 in the call to ZHERK, but ATLAS appears to
      // use the result matrix before zeroing the elements.

      retval = ComplexMatrix (a_nr, b_nc, 0.0);
      Complex *c = retval.fortran_vec ();

      const char ctra = get_blas_trans_arg (tra, cja);
      if (cja || cjb)
        {
          F77_XFCN (zherk, ZHERK, (F77_CONST_CHAR_ARG2 ("U", 1),
                                   F77_CONST_CHAR_ARG2 (&ctra, 1),
                                   a_nr, a_nc, 1.0,
                                   F77_CONST_DBLE_CMPLX_ARG (a.data ()), lda, 0.0, F77_DBLE_CMPLX_ARG (c), a_nr
                                   F77_CHAR_ARG_LEN (1)
                                   F77_CHAR_ARG_LEN (1)));
          for (F77_INT j = 0; j < a_nr; j++)
            for (F77_INT i = 0; i < j; i++)
              retval.xelem (j, i) = octave::math::conj (retval.xelem (i, j));
        }
      else
        {
          F77_XFCN (zsyrk, ZSYRK, (F77_CONST_CHAR_ARG2 ("U", 1),
                                   F77_CONST_CHAR_ARG2 (&ctra, 1),
                                   a_nr, a_nc, 1.0,
                                   F77_CONST_DBLE_CMPLX_ARG (a.data ()), lda, 0.0, F77_DBLE_CMPLX_ARG (c), a_nr
                                   F77_CHAR_ARG_LEN (1)
                                   F77_CHAR_ARG_LEN (1)));
          for (F77_INT j = 0; j < a_nr; j++)
            for (F77_INT i = 0; i < j; i++)
              retval.xelem (j, i) = retval.xelem (i, j);

        }

    }
  else
    {
      F77_INT lda = octave::to_f77_int (a.rows ());
      F77_INT tda = octave::to_f77_int (a.cols ());
      F77_INT ldb = octave::to_f77_int (b.rows ());
      F77_INT tdb = octave::to_f77_int (b.cols ());

      retval = ComplexMatrix (a_nr, b_nc, 0.0);
      Complex *c = retval.fortran_vec ();

      if (b_nc == 1 && a_nr == 1)
        {
          if (cja == cjb)
            {
              F77_FUNC (xzdotu, XZDOTU) (a_nc, F77_CONST_DBLE_CMPLX_ARG (a.data ()), 1,
                                         F77_CONST_DBLE_CMPLX_ARG (b.data ()), 1,
                                         F77_DBLE_CMPLX_ARG (c));
              if (cja) *c = octave::math::conj (*c);
            }
          else if (cja)
            F77_FUNC (xzdotc, XZDOTC) (a_nc, F77_CONST_DBLE_CMPLX_ARG (a.data ()), 1,
                                       F77_CONST_DBLE_CMPLX_ARG (b.data ()), 1,
                                       F77_DBLE_CMPLX_ARG (c));
          else
            F77_FUNC (xzdotc, XZDOTC) (a_nc, F77_CONST_DBLE_CMPLX_ARG (b.data ()), 1,
                                       F77_CONST_DBLE_CMPLX_ARG (a.data ()), 1,
                                       F77_DBLE_CMPLX_ARG (c));
        }
      else if (b_nc == 1 && ! cjb)
        {
          const char ctra = get_blas_trans_arg (tra, cja);
          F77_XFCN (zgemv, ZGEMV, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                   lda, tda, 1.0,  F77_CONST_DBLE_CMPLX_ARG (a.data ()), lda,
                                   F77_CONST_DBLE_CMPLX_ARG (b.data ()), 1, 0.0, F77_DBLE_CMPLX_ARG (c), 1
                                   F77_CHAR_ARG_LEN (1)));
        }
      else if (a_nr == 1 && ! cja && ! cjb)
        {
          const char crevtrb = get_blas_trans_arg (! trb, cjb);
          F77_XFCN (zgemv, ZGEMV, (F77_CONST_CHAR_ARG2 (&crevtrb, 1),
                                   ldb, tdb, 1.0,  F77_CONST_DBLE_CMPLX_ARG (b.data ()), ldb,
                                   F77_CONST_DBLE_CMPLX_ARG (a.data ()), 1, 0.0, F77_DBLE_CMPLX_ARG (c), 1
                                   F77_CHAR_ARG_LEN (1)));
        }
      else
        {
          const char ctra = get_blas_trans_arg (tra, cja);
          const char ctrb = get_blas_trans_arg (trb, cjb);
          F77_XFCN (zgemm, ZGEMM, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                   F77_CONST_CHAR_ARG2 (&ctrb, 1),
                                   a_nr, b_nc, a_nc, 1.0, F77_CONST_DBLE_CMPLX_ARG (a.data ()),
                                   lda, F77_CONST_DBLE_CMPLX_ARG (b.data ()), ldb, 0.0, F77_DBLE_CMPLX_ARG (c),
                                   a_nr
                                   F77_CHAR_ARG_LEN (1)
                                   F77_CHAR_ARG_LEN (1)));
        }
    }

  return retval;
}

ComplexMatrix
operator * (const ComplexMatrix& a, const ComplexMatrix& b)
{
  return xgemm (a, b);
}

// FIXME: it would be nice to share code among the min/max functions below.

#define EMPTY_RETURN_CHECK(T)                   \
  if (nr == 0 || nc == 0)                       \
    return T (nr, nc);

ComplexMatrix
min (const Complex& c, const ComplexMatrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (ComplexMatrix);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::min (c, m(i, j));
      }

  return result;
}

ComplexMatrix
min (const ComplexMatrix& m, const Complex& c)
{
  return min (c, m);
}

ComplexMatrix
min (const ComplexMatrix& a, const ComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    (*current_liboctave_error_handler)
      ("two-arg min requires same size arguments");

  EMPTY_RETURN_CHECK (ComplexMatrix);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      bool columns_are_real_only = true;
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          if (std::imag (a(i, j)) != 0.0 || std::imag (b(i, j)) != 0.0)
            {
              columns_are_real_only = false;
              break;
            }
        }

      if (columns_are_real_only)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            result(i, j) = octave::math::min (std::real (a(i, j)),
                                              std::real (b(i, j)));
        }
      else
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = octave::math::min (a(i, j), b(i, j));
            }
        }
    }

  return result;
}

ComplexMatrix
max (const Complex& c, const ComplexMatrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (ComplexMatrix);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = octave::math::max (c, m(i, j));
      }

  return result;
}

ComplexMatrix
max (const ComplexMatrix& m, const Complex& c)
{
  return max (c, m);
}

ComplexMatrix
max (const ComplexMatrix& a, const ComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    (*current_liboctave_error_handler)
      ("two-arg max requires same size arguments");

  EMPTY_RETURN_CHECK (ComplexMatrix);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      bool columns_are_real_only = true;
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          if (std::imag (a(i, j)) != 0.0 || std::imag (b(i, j)) != 0.0)
            {
              columns_are_real_only = false;
              break;
            }
        }

      // FIXME: is it so much faster?
      if (columns_are_real_only)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = octave::math::max (std::real (a(i, j)),
                                                std::real (b(i, j)));
            }
        }
      else
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = octave::math::max (a(i, j), b(i, j));
            }
        }
    }

  return result;
}

ComplexMatrix linspace (const ComplexColumnVector& x1,
                        const ComplexColumnVector& x2,
                        octave_idx_type n)
{
  octave_idx_type m = x1.numel ();

  if (x2.numel () != m)
    (*current_liboctave_error_handler)
      ("linspace: vectors must be of equal length");

  ComplexMatrix retval;

  if (n < 1)
    {
      retval.clear (m, 0);
      return retval;
    }

  retval.clear (m, n);
  for (octave_idx_type i = 0; i < m; i++)
    retval.xelem (i, 0) = x1(i);

  // The last column is unused so temporarily store delta there
  Complex *delta = &retval.xelem (0, n-1);
  for (octave_idx_type i = 0; i < m; i++)
    delta[i] = (x1(i) == x2(i)) ? 0 : (x2(i) - x1(i)) / (n - 1.0);

  for (octave_idx_type j = 1; j < n-1; j++)
    for (octave_idx_type i = 0; i < m; i++)
      retval.xelem (i, j) = x1(i) + static_cast<double> (j)*delta[i];

  for (octave_idx_type i = 0; i < m; i++)
    retval.xelem (i, n-1) = x2(i);

  return retval;
}

MS_CMP_OPS (ComplexMatrix, Complex)
MS_BOOL_OPS (ComplexMatrix, Complex)

SM_CMP_OPS (Complex, ComplexMatrix)
SM_BOOL_OPS (Complex, ComplexMatrix)

MM_CMP_OPS (ComplexMatrix, ComplexMatrix)
MM_BOOL_OPS (ComplexMatrix, ComplexMatrix)
