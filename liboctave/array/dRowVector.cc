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

#include <istream>
#include <ostream>
#include <type_traits>

#include "Array-util.h"
#include "lo-blas-proto.h"
#include "lo-error.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"

// Row Vector class.

bool
RowVector::operator == (const RowVector& a) const
{
  octave_idx_type len = numel ();
  if (len != a.numel ())
    return 0;
  return mx_inline_equal (len, data (), a.data ());
}

bool
RowVector::operator != (const RowVector& a) const
{
  return !(*this == a);
}

RowVector&
RowVector::insert (const RowVector& a, octave_idx_type c)
{
  octave_idx_type a_len = a.numel ();

  if (c < 0 || c + a_len > numel ())
    (*current_liboctave_error_handler) ("range error for insert");

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (c+i) = a.elem (i);
    }

  return *this;
}

RowVector&
RowVector::fill (double val)
{
  octave_idx_type len = numel ();

  if (len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < len; i++)
        xelem (i) = val;
    }

  return *this;
}

RowVector&
RowVector::fill (double val, octave_idx_type c1, octave_idx_type c2)
{
  octave_idx_type len = numel ();

  if (c1 < 0 || c2 < 0 || c1 >= len || c2 >= len)
    (*current_liboctave_error_handler) ("range error for fill");

  if (c1 > c2) { std::swap (c1, c2); }

  if (c2 >= c1)
    {
      make_unique ();

      for (octave_idx_type i = c1; i <= c2; i++)
        xelem (i) = val;
    }

  return *this;
}

RowVector
RowVector::append (const RowVector& a) const
{
  octave_idx_type len = numel ();
  octave_idx_type nc_insert = len;
  RowVector retval (len + a.numel ());
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ColumnVector
RowVector::transpose (void) const
{
  return MArray<double>::transpose ();
}

RowVector
real (const ComplexRowVector& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_real);
}

RowVector
imag (const ComplexRowVector& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_imag);
}

RowVector
RowVector::extract (octave_idx_type c1, octave_idx_type c2) const
{
  if (c1 > c2) { std::swap (c1, c2); }

  octave_idx_type new_c = c2 - c1 + 1;

  RowVector result (new_c);

  for (octave_idx_type i = 0; i < new_c; i++)
    result.xelem (i) = elem (c1+i);

  return result;
}

RowVector
RowVector::extract_n (octave_idx_type r1, octave_idx_type n) const
{
  RowVector result (n);

  for (octave_idx_type i = 0; i < n; i++)
    result.xelem (i) = elem (r1+i);

  return result;
}

// row vector by matrix -> row vector

RowVector
operator * (const RowVector& v, const Matrix& a)
{
  RowVector retval;

  F77_INT len = octave::to_f77_int (v.numel ());

  F77_INT a_nr = octave::to_f77_int (a.rows ());
  F77_INT a_nc = octave::to_f77_int (a.cols ());

  if (a_nr != len)
    octave::err_nonconformant ("operator *", 1, len, a_nr, a_nc);

  if (len == 0)
    retval.resize (a_nc, 0.0);
  else
    {
      // Transpose A to form A'*x == (x'*A)'

      F77_INT ld = a_nr;

      retval.resize (a_nc);
      double *y = retval.fortran_vec ();

      F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("T", 1),
                               a_nr, a_nc, 1.0, a.data (),
                               ld, v.data (), 1, 0.0, y, 1
                               F77_CHAR_ARG_LEN (1)));
    }

  return retval;
}

// other operations

double
RowVector::min (void) const
{
  octave_idx_type len = numel ();
  if (len == 0)
    return 0;

  double res = elem (0);

  for (octave_idx_type i = 1; i < len; i++)
    if (elem (i) < res)
      res = elem (i);

  return res;
}

double
RowVector::max (void) const
{
  octave_idx_type len = numel ();
  if (len == 0)
    return 0;

  double res = elem (0);

  for (octave_idx_type i = 1; i < len; i++)
    if (elem (i) > res)
      res = elem (i);

  return res;
}

std::ostream&
operator << (std::ostream& os, const RowVector& a)
{
//  int field_width = os.precision () + 7;

  for (octave_idx_type i = 0; i < a.numel (); i++)
    os << ' ' /* setw (field_width) */ << a.elem (i);
  return os;
}

std::istream&
operator >> (std::istream& is, RowVector& a)
{
  octave_idx_type len = a.numel ();

  if (len > 0)
    {
      double tmp;
      for (octave_idx_type i = 0; i < len; i++)
        {
          is >> tmp;
          if (is)
            a.elem (i) = tmp;
          else
            break;
        }
    }
  return is;
}

// other operations

RowVector
linspace (double x1, double x2, octave_idx_type n_in)
{
  RowVector retval;

  if (n_in < 1)
    return retval;
  else if (n_in == 1)
    {
      retval.resize (1, x2);
      return retval;
    }

  // Use unsigned type (guaranteed n_in > 1 at this point) so that divisions
  // by 2 can be replaced by compiler with shift right instructions.
  typedef std::make_unsigned<octave_idx_type>::type unsigned_octave_idx_type;

  unsigned_octave_idx_type n = n_in;

  // Set endpoints, rather than calculate, for maximum accuracy.
  retval.clear (n);
  retval.xelem (0) = x1;
  retval.xelem (n-1) = x2;

  // Construct linspace symmetrically from both ends.
  double delta = (x2 - x1) / (n - 1);
  unsigned_octave_idx_type n2 = n/2;
  for (unsigned_octave_idx_type i = 1; i < n2; i++)
    {
      retval.xelem (i) = x1 + i*delta;
      retval.xelem (n-1-i) = x2 - i*delta;
    }
  if (n % 2 == 1)  // Middle element if number of elements is odd.
    retval.xelem (n2) = (x1 == -x2 ? 0 : (x1 + x2) / 2);

  return retval;
}

// row vector by column vector -> scalar

double
operator * (const RowVector& v, const ColumnVector& a)
{
  double retval = 0.0;

  F77_INT len = octave::to_f77_int (v.numel ());

  F77_INT a_len = octave::to_f77_int (a.numel ());

  if (len != a_len)
    octave::err_nonconformant ("operator *", len, a_len);

  if (len != 0)
    F77_FUNC (xddot, XDDOT) (len, v.data (), 1, a.data (), 1, retval);

  return retval;
}

Complex
operator * (const RowVector& v, const ComplexColumnVector& a)
{
  ComplexRowVector tmp (v);
  return tmp * a;
}
