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

// Complex Row Vector class

bool
ComplexRowVector::operator == (const ComplexRowVector& a) const
{
  octave_idx_type len = numel ();
  if (len != a.numel ())
    return 0;
  return mx_inline_equal (len, data (), a.data ());
}

bool
ComplexRowVector::operator != (const ComplexRowVector& a) const
{
  return !(*this == a);
}

// destructive insert/delete/reorder operations

ComplexRowVector&
ComplexRowVector::insert (const RowVector& a, octave_idx_type c)
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

ComplexRowVector&
ComplexRowVector::insert (const ComplexRowVector& a, octave_idx_type c)
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

ComplexRowVector&
ComplexRowVector::fill (double val)
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

ComplexRowVector&
ComplexRowVector::fill (const Complex& val)
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

ComplexRowVector&
ComplexRowVector::fill (double val, octave_idx_type c1, octave_idx_type c2)
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

ComplexRowVector&
ComplexRowVector::fill (const Complex& val,
                        octave_idx_type c1, octave_idx_type c2)
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

ComplexRowVector
ComplexRowVector::append (const RowVector& a) const
{
  octave_idx_type len = numel ();
  octave_idx_type nc_insert = len;
  ComplexRowVector retval (len + a.numel ());
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ComplexRowVector
ComplexRowVector::append (const ComplexRowVector& a) const
{
  octave_idx_type len = numel ();
  octave_idx_type nc_insert = len;
  ComplexRowVector retval (len + a.numel ());
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

ComplexColumnVector
ComplexRowVector::hermitian (void) const
{
  return MArray<Complex>::hermitian (std::conj);
}

ComplexColumnVector
ComplexRowVector::transpose (void) const
{
  return MArray<Complex>::transpose ();
}

ComplexRowVector
conj (const ComplexRowVector& a)
{
  return do_mx_unary_map<Complex, Complex, std::conj<double>> (a);
}

// resize is the destructive equivalent for this one

ComplexRowVector
ComplexRowVector::extract (octave_idx_type c1, octave_idx_type c2) const
{
  if (c1 > c2) { std::swap (c1, c2); }

  octave_idx_type new_c = c2 - c1 + 1;

  ComplexRowVector result (new_c);

  for (octave_idx_type i = 0; i < new_c; i++)
    result.elem (i) = elem (c1+i);

  return result;
}

ComplexRowVector
ComplexRowVector::extract_n (octave_idx_type r1, octave_idx_type n) const
{
  ComplexRowVector result (n);

  for (octave_idx_type i = 0; i < n; i++)
    result.elem (i) = elem (r1+i);

  return result;
}

// row vector by row vector -> row vector operations

ComplexRowVector&
ComplexRowVector::operator += (const RowVector& a)
{
  octave_idx_type len = numel ();

  octave_idx_type a_len = a.numel ();

  if (len != a_len)
    octave::err_nonconformant ("operator +=", len, a_len);

  if (len == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  mx_inline_add2 (len, d, a.data ());
  return *this;
}

ComplexRowVector&
ComplexRowVector::operator -= (const RowVector& a)
{
  octave_idx_type len = numel ();

  octave_idx_type a_len = a.numel ();

  if (len != a_len)
    octave::err_nonconformant ("operator -=", len, a_len);

  if (len == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  mx_inline_sub2 (len, d, a.data ());
  return *this;
}

// row vector by matrix -> row vector

ComplexRowVector
operator * (const ComplexRowVector& v, const ComplexMatrix& a)
{
  ComplexRowVector retval;

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
      Complex *y = retval.fortran_vec ();

      F77_XFCN (zgemv, ZGEMV, (F77_CONST_CHAR_ARG2 ("T", 1),
                               a_nr, a_nc, 1.0, F77_CONST_DBLE_CMPLX_ARG (a.data ()),
                               ld, F77_CONST_DBLE_CMPLX_ARG (v.data ()), 1, 0.0, F77_DBLE_CMPLX_ARG (y), 1
                               F77_CHAR_ARG_LEN (1)));
    }

  return retval;
}

ComplexRowVector
operator * (const RowVector& v, const ComplexMatrix& a)
{
  ComplexRowVector tmp (v);
  return tmp * a;
}

// other operations

Complex
ComplexRowVector::min (void) const
{
  octave_idx_type len = numel ();
  if (len == 0)
    return Complex (0.0);

  Complex res = elem (0);
  double absres = std::abs (res);

  for (octave_idx_type i = 1; i < len; i++)
    if (std::abs (elem (i)) < absres)
      {
        res = elem (i);
        absres = std::abs (res);
      }

  return res;
}

Complex
ComplexRowVector::max (void) const
{
  octave_idx_type len = numel ();
  if (len == 0)
    return Complex (0.0);

  Complex res = elem (0);
  double absres = std::abs (res);

  for (octave_idx_type i = 1; i < len; i++)
    if (std::abs (elem (i)) > absres)
      {
        res = elem (i);
        absres = std::abs (res);
      }

  return res;
}

// i/o

std::ostream&
operator << (std::ostream& os, const ComplexRowVector& a)
{
//  int field_width = os.precision () + 7;
  for (octave_idx_type i = 0; i < a.numel (); i++)
    os << ' ' /* setw (field_width) */ << a.elem (i);
  return os;
}

std::istream&
operator >> (std::istream& is, ComplexRowVector& a)
{
  octave_idx_type len = a.numel ();

  if (len > 0)
    {
      Complex tmp;
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

// row vector by column vector -> scalar

// row vector by column vector -> scalar

Complex
operator * (const ComplexRowVector& v, const ColumnVector& a)
{
  ComplexColumnVector tmp (a);
  return v * tmp;
}

Complex
operator * (const ComplexRowVector& v, const ComplexColumnVector& a)
{
  Complex retval (0.0, 0.0);

  F77_INT len = octave::to_f77_int (v.numel ());

  F77_INT a_len = octave::to_f77_int (a.numel ());

  if (len != a_len)
    octave::err_nonconformant ("operator *", len, a_len);
  if (len != 0)
    F77_FUNC (xzdotu, XZDOTU) (len, F77_CONST_DBLE_CMPLX_ARG (v.data ()), 1,
                               F77_CONST_DBLE_CMPLX_ARG (a.data ()), 1, F77_DBLE_CMPLX_ARG (&retval));

  return retval;
}

// other operations

ComplexRowVector
linspace (const Complex& x1, const Complex& x2, octave_idx_type n_in)
{
  ComplexRowVector retval;

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
  Complex delta = (x2 - x1) / (n - 1.0);
  unsigned_octave_idx_type n2 = n/2;
  for (unsigned_octave_idx_type i = 1; i < n2; i++)
    {
      retval.xelem (i) = x1 + static_cast<double> (i)*delta;
      retval.xelem (n-1-i) = x2 - static_cast<double> (i)*delta;
    }
  if (n % 2 == 1)  // Middle element if number of elements is odd.
    retval.xelem (n2) = (x1 == -x2 ? 0 : (x1 + x2) / 2.0);

  return retval;
}
