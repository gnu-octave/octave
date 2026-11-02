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

#include <algorithm>
#include <functional>
#include <iterator>
#include <type_traits>
#include <vector>

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename RT, typename A, typename B, typename F>
Sparse<RT>
sparse_bcast_op (const A& a, const B& b, F fcn, const char *name);

OCTAVE_END_NAMESPACE(octave)

// sparse array with math ops.
// Element by element MSparse by MSparse ops.

template <typename T, typename OP>
MSparse<T>&
plus_or_minus (MSparse<T>& a, const MSparse<T>& b, OP op, const char *op_name)
{
  MSparse<T> r;

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 0 && (b_nr == 0 || b_nr == 1))
    {
      if (a_nc == 1 || b_nc == 1 || a_nc == b_nc)
        r.resize (a_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nc == 0 && (b_nc == 0 || b_nc == 1))
    {
      if (a_nr == 1 || b_nr == 1 || a_nr == b_nr)
        r.resize (std::max (a_nr, b_nr), a_nc);
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nr == 0 && (a_nr == 0 || a_nr == 1))
    {
      if (b_nc == 1 || a_nc == 1 || b_nc == a_nc)
        r.resize (b_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nc == 0 && (a_nc == 0 || a_nc == 1))
    {
      if (b_nr == 1 || a_nr == 1 || b_nr == a_nr)
        r.resize (std::max (a_nr, b_nr), b_nc);
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nr != b_nr || a_nc != b_nc)
    octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
  else
    {
      r = MSparse<T> (a_nr, a_nc, (a.nnz () + b.nnz ()));

      octave_idx_type jx = 0;
      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type ja = a.cidx (i);
          octave_idx_type ja_max = a.cidx (i+1);
          bool ja_lt_max = ja < ja_max;

          octave_idx_type jb = b.cidx (i);
          octave_idx_type jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.ridx (jx) = a.ridx (ja);
                  r.data (jx) = op (a.data (ja), 0.);
                  jx++;
                  ja++;
                  ja_lt_max= ja < ja_max;
                }
              else if ((! ja_lt_max)
                       || (b.ridx (jb) < a.ridx (ja)))
                {
                  r.ridx (jx) = b.ridx (jb);
                  r.data (jx) = op (0., b.data (jb));
                  jx++;
                  jb++;
                  jb_lt_max= jb < jb_max;
                }
              else
                {
                  if (op (a.data (ja), b.data (jb)) != 0.)
                    {
                      r.data (jx) = op (a.data (ja), b.data (jb));
                      r.ridx (jx) = a.ridx (ja);
                      jx++;
                    }
                  ja++;
                  ja_lt_max= ja < ja_max;
                  jb++;
                  jb_lt_max= jb < jb_max;
                }
            }
          r.cidx (i+1) = jx;
        }
    }
  a = r.maybe_compress ();
  return a;
}

template <typename T>
MSparse<T>&
operator += (MSparse<T>& a, const MSparse<T>& b)
{
  return plus_or_minus (a, b, std::plus<T> (), "operator +=");
}

template <typename T>
MSparse<T>&
operator -= (MSparse<T>& a, const MSparse<T>& b)
{
  return plus_or_minus (a, b, std::minus<T> (), "operator -=");
}

// Element by element MSparse by scalar ops.

template <typename T, typename OP>
MArray<T>
plus_or_minus (const MSparse<T>& a, const T& s, OP op)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  MArray<T> r (dim_vector (nr, nc), op (0.0, s));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
      r.xelem (a.ridx (i), j) = op (a.data (i), s);

  return r;
}

template <typename T>
MArray<T>
operator + (const MSparse<T>& a, const T& s)
{
  return plus_or_minus (a, s, std::plus<T> ());
}

template <typename T>
MArray<T>
operator - (const MSparse<T>& a, const T& s)
{
  return plus_or_minus (a, s, std::minus<T> ());
}

template <typename T, typename OP>
MSparse<T>
times_or_divide (const MSparse<T>& a, const T& s, OP op, const char *op_name)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nnz ();

  MSparse<T> r;

  // Handle scalars that affect zero elements in sparse array
  bool non_zero = op (0.0, s) != 0.0;

  if (non_zero)
    r = octave::sparse_bcast_op<T> (a, Array<T> (dim_vector (1, 1), s),
                                    op, op_name);
  else
    {
      r = MSparse<T> (nr, nc, nz);

      for (octave_idx_type i = 0; i < nz; i++)
        {
          r.data (i) = op (a.data (i), s);
          r.ridx (i) = a.ridx (i);
        }
      for (octave_idx_type i = 0; i < nc + 1; i++)
        r.cidx (i) = a.cidx (i);
      r.maybe_compress (true);
    }
  return r;
}

template <typename T>
MSparse<T>
operator * (const MSparse<T>& a, const T& s)
{
  return times_or_divide (a, s, std::multiplies<T> (), "operator *");
}

template <typename T>
MSparse<T>
operator / (const MSparse<T>& a, const T& s)
{
  return times_or_divide (a, s, std::divides<T> (), "operator /");
}

// Element by element scalar by MSparse ops.

template <typename T, typename OP>
MArray<T>
plus_or_minus (const T& s, const MSparse<T>& a, OP op)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  MArray<T> r (dim_vector (nr, nc), op (s, 0.0));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
      r.xelem (a.ridx (i), j) = op (s, a.data (i));

  return r;
}

template <typename T>
MArray<T>
operator + (const T& s, const MSparse<T>& a)
{
  return plus_or_minus (s, a, std::plus<T> ());
}

template <typename T>
MArray<T>
operator - (const T& s, const MSparse<T>& a)
{
  return plus_or_minus (s, a, std::minus<T> ());
}

template <typename T, typename OP>
MSparse<T>
times_or_divide (const T& s, const MSparse<T>& a, OP op, const char *op_name)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nnz ();

  MSparse<T> r;

  // Handle scalars that affect zero elements in sparse array
  bool non_zero = op (s, 0.0) != 0.0;

  if (non_zero)
    r = octave::sparse_bcast_op<T> (Array<T> (dim_vector (1, 1), s), a, op,
                                    op_name);
  else
    {
      r = MSparse<T> (nr, nc, nz);

      for (octave_idx_type i = 0; i < nz; i++)
        {
          r.data (i) = op (s, a.data (i));
          r.ridx (i) = a.ridx (i);
        }
      for (octave_idx_type i = 0; i < nc + 1; i++)
        r.cidx (i) = a.cidx (i);
      r.maybe_compress (true);
    }
  return r;
}

template <typename T>
MSparse<T>
operator * (const T& s, const MSparse<T>& a)
{
  return times_or_divide (s, a, std::multiplies<T> (), "operator *");
}

template <typename T>
MSparse<T>
operator / (const T& s, const MSparse<T>& a)
{
  return times_or_divide (s, a, std::divides<T> (), "operator /");
}

// Element by element MSparse by MSparse ops.

template <typename T, typename OP>
MSparse<T>
plus_or_minus (const MSparse<T>& a, const MSparse<T>& b, OP op,
               const char *op_name, bool negate)
{
  MSparse<T> r;

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 1 && a_nc == 1)
    {
      if (a.elem (0, 0) == 0.)
        if (negate)
          r = -MSparse<T> (b);
        else
          r = MSparse<T> (b);
      else
        {
          r = MSparse<T> (b_nr, b_nc, op (a.data (0), 0.));

          for (octave_idx_type j = 0 ; j < b_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * b_nr;
              for (octave_idx_type i = b.cidx (j) ; i < b.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + b.ridx (i)) = op (a.data (0), b.data (i));
                }
            }
          r.maybe_compress ();
        }
    }
  else if (b_nr == 1 && b_nc == 1)
    {
      if (b.elem (0, 0) == 0.)
        r = MSparse<T> (a);
      else
        {
          r = MSparse<T> (a_nr, a_nc, op (0.0, b.data (0)));

          for (octave_idx_type j = 0 ; j < a_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * a_nr;
              for (octave_idx_type i = a.cidx (j) ; i < a.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + a.ridx (i)) = op (a.data (i), b.data (0));
                }
            }
          r.maybe_compress ();
        }
    }
  else if (a_nr == 0 && (b_nr == 0 || b_nr == 1))
    {
      if (a_nc == 1 || b_nc == 1 || a_nc == b_nc)
        r.resize (a_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nc == 0 && (b_nc == 0 || b_nc == 1))
    {
      if (a_nr == 1 || b_nr == 1 || a_nr == b_nr)
        r.resize (std::max (a_nr, b_nr), a_nc);
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nr == 0 && (a_nr == 0 || a_nr == 1))
    {
      if (b_nc == 1 || a_nc == 1 || b_nc == a_nc)
        r.resize (b_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nc == 0 && (a_nc == 0 || a_nc == 1))
    {
      if (b_nr == 1 || a_nr == 1 || b_nr == a_nr)
        r.resize (std::max (a_nr, b_nr), b_nc);
      else
        octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nr == b_nr && a_nc == b_nc)
    {
      r = MSparse<T> (a_nr, a_nc, (a.nnz () + b.nnz ()));

      octave_idx_type jx = 0;
      r.cidx (0) = 0;
      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type ja = a.cidx (i);
          octave_idx_type ja_max = a.cidx (i+1);
          bool ja_lt_max = ja < ja_max;

          octave_idx_type jb = b.cidx (i);
          octave_idx_type jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.ridx (jx) = a.ridx (ja);
                  r.data (jx) = op (a.data (ja), 0.);
                  jx++;
                  ja++;
                  ja_lt_max = ja < ja_max;
                }
              else if ((! ja_lt_max) || (b.ridx (jb) < a.ridx (ja)))
                {
                  r.ridx (jx) = b.ridx (jb);
                  r.data (jx) = op (0., b.data (jb));
                  jx++;
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
              else
                {
                  if (op (a.data (ja), b.data (jb)) != 0.)
                    {
                      r.data (jx) = op (a.data (ja), b.data (jb));
                      r.ridx (jx) = a.ridx (ja);
                      jx++;
                    }
                  ja++;
                  ja_lt_max = ja < ja_max;
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
            }
          r.cidx (i+1) = jx;
        }
      r.maybe_compress ();
    }
  else if (a_nr == b_nr && (a_nc == 1 || b_nc == 1))
    // (a_nc == b_nc && (a_nr == 1 || b_nr == 1)) is handled
    // by double transpose in the caller functions
    {
      octave_idx_type r_nc = (a_nc < b_nc ? b_nc : a_nc);
      octave_idx_type rnnz = (a_nc < b_nc ? a.nnz () * b_nc + b.nnz () :
                                            a.nnz () + a_nc * b.nnz ());
      r = MSparse<T> (a_nr, r_nc, rnnz);

      octave_idx_type jx = 0;
      r.cidx (0) = 0;
      for (octave_idx_type i = 0 ; i < r_nc ; i++)
        {
          octave_idx_type ja;
          octave_idx_type ja_max;
          octave_idx_type jb;
          octave_idx_type jb_max;
          if (a_nc == 1)
            {
              ja = a.cidx(0);
              ja_max = a.cidx(1);
              jb = b.cidx (i);
              jb_max = b.cidx (i+1);
            }
          else
            {
              ja = a.cidx(i);
              ja_max = a.cidx(i+1);
              jb = b.cidx (0);
              jb_max = b.cidx (1);
            }
          bool ja_lt_max = ja < ja_max;
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.ridx (jx) = a.ridx (ja);
                  r.data (jx) = op (a.data (ja), 0.);
                  jx++;
                  ja++;
                  ja_lt_max = ja < ja_max;
                }
              else if ((! ja_lt_max) || (b.ridx (jb) < a.ridx (ja)))
                {
                  r.ridx (jx) = b.ridx (jb);
                  r.data (jx) = op (0., b.data (jb));
                  jx++;
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
              else
                {
                  if (op (a.data (ja), b.data (jb)) != 0.)
                    {
                      r.data (jx) = op (a.data (ja), b.data (jb));
                      r.ridx (jx) = a.ridx (ja);
                      jx++;
                    }
                  ja++;
                  ja_lt_max = ja < ja_max;
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
            }
          r.cidx (i+1) = jx;
        }
      r.maybe_compress ();
    }
  else if (a_nr == 1 && b_nc == 1)
    // (a_nc == 1 && b_nr == 1) is handled
    // by double transpose in the caller functions
    r = octave::sparse_bcast_op<T> (a, b, op, op_name);
  else
    octave::err_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);

  return r;
}

template <typename T>
MSparse<T>
operator + (const MSparse<T>& a, const MSparse<T>& b)
{
  if ((a.cols () != 1 && a.cols () == b.cols ()
       && (a.rows () == 1 || b.rows () == 1))
      || (a.cols () == 1 && b.rows () == 1 && a.rows () > 1 && b.cols () > 1))
    return plus_or_minus (a.transpose (), b.transpose (), std::plus<T> (),
                          "operator +", false).transpose ();
  else
    return plus_or_minus (a, b, std::plus<T> (), "operator +", false);
}

template <typename T>
MSparse<T>
operator - (const MSparse<T>& a, const MSparse<T>& b)
{
  if ((a.cols () != 1 && a.cols () == b.cols ()
       && (a.rows () == 1 || b.rows () == 1))
      || (a.cols () == 1 && b.rows () == 1 && a.rows () > 1 && b.cols () > 1))
    return plus_or_minus (a.transpose (), b.transpose (), std::minus<T> (),
                          "operator -", true).transpose ();
  else
    return plus_or_minus (a, b, std::minus<T> (), "operator -", true);
}

OCTAVE_BEGIN_NAMESPACE(octave)

// Element-wise operation FCN on A and B with broadcasting.  A and B are
// each sparse (a Sparse) or full (an Array).  The result is formed only
// where an operand has an entry, so no operand is expanded to a full
// matrix.  An entry against a zero of the other operand is kept wherever
// that gives a nonzero, such as the NaN of 0*Inf, 0*NaN, 0/0 or 0/NaN.
// Where FCN gives a nonzero for two zeros, as 0/0 or 0 == 0 do, and both
// operands are sparse, every element of the result is formed.

template <typename RT, typename A, typename B, typename F>
Sparse<RT>
sparse_bcast_op (const A& a, const B& b, F fcn, const char *name)
{
  typedef typename A::element_type TA;
  typedef typename B::element_type TB;

  // The entries of a sparse operand are its stored ones, those of a full
  // operand all of its elements.  The entries of column J of X are
  // K = beg (X, J), ..., beg (X, J+1) - 1.

  auto beg = [] (const auto& x, octave_idx_type j)
  {
    typedef std::decay_t<decltype (x)> X;
    if constexpr (std::is_base_of_v<Sparse<typename X::element_type>, X>)
      return x.cidx (j);
    else
      return j * x.rows ();
  };

  auto row = [] (const auto& x, octave_idx_type k, octave_idx_type j)
  {
    typedef std::decay_t<decltype (x)> X;
    if constexpr (std::is_base_of_v<Sparse<typename X::element_type>, X>)
      return x.ridx (k);
    else
      return k - j * x.rows ();
  };

  auto value = [] (const auto& x, octave_idx_type k)
  {
    typedef std::decay_t<decltype (x)> X;
    if constexpr (std::is_base_of_v<Sparse<typename X::element_type>, X>)
      return x.data (k);
    else
      return x.xelem (k);
  };

  auto elem = [] (const auto& x, octave_idx_type i, octave_idx_type j)
  {
    typedef std::decay_t<decltype (x)> X;
    if constexpr (std::is_base_of_v<Sparse<typename X::element_type>, X>)
      {
        const octave_idx_type *ri = x.ridx ();
        const octave_idx_type *p_beg = ri + x.cidx (j);
        const octave_idx_type *p_end = ri + x.cidx (j+1);
        const octave_idx_type *p = std::lower_bound (p_beg, p_end, i);

        return ((p != p_end && *p == i)
                ? x.data (p - ri) : typename X::element_type ());
      }
    else
      return x.xelem (i, j);
  };

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (! ((a_nr == b_nr || a_nr == 1 || b_nr == 1)
         && (a_nc == b_nc || a_nc == 1 || b_nc == 1)))
    err_nonconformant (name, a_nr, a_nc, b_nr, b_nc);

  octave_idx_type nr = (a_nr == 1 ? b_nr : a_nr);
  octave_idx_type nc = (a_nc == 1 ? b_nc : a_nc);

  // An operand with one row repeats it down every row of the result, one
  // with one column repeats it across every column.
  bool a_row = (a_nr == 1 && nr != 1);
  bool b_row = (b_nr == 1 && nr != 1);
  bool a_col = (a_nc == 1 && nc != 1 && ! a_row);
  bool b_col = (b_nc == 1 && nc != 1 && ! b_row);

  auto nonzero = [] (const RT& v) { return v != RT (); };

  // Two sparse operands with FCN nonzero for two zeros fill every column.
  bool zero_fill = (std::is_base_of_v<Sparse<TA>, A>
                    && std::is_base_of_v<Sparse<TB>, B>
                    && nonzero (fcn (TA (), TB ())));

  auto stored = [&] (const auto& x, octave_idx_type jx,
                     std::vector<octave_idx_type>& out)
  {
    for (octave_idx_type k = beg (x, jx); k < beg (x, jx+1); k++)
      out.push_back (row (x, k, jx));
  };

  // Rows of a repeated column that give a nonzero against a zero of the
  // other operand, found once rather than for every column.
  std::vector<octave_idx_type> a_fill;
  std::vector<octave_idx_type> b_fill;

  if (a_col)
    for (octave_idx_type k = beg (a, 0); k < beg (a, 1); k++)
      if (nonzero (fcn (value (a, k), TB ())))
        a_fill.push_back (row (a, k, 0));

  if (b_col)
    for (octave_idx_type k = beg (b, 0); k < beg (b, 1); k++)
      if (nonzero (fcn (TA (), value (b, k))))
        b_fill.push_back (row (b, k, 0));

  std::vector<octave_idx_type> r_cidx (nc + 1, 0);
  std::vector<octave_idx_type> r_ridx;
  std::vector<RT> r_data;

  std::vector<octave_idx_type> a_rows;
  std::vector<octave_idx_type> b_rows;
  std::vector<octave_idx_type> rows;

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_quit ();

      octave_idx_type ja = (a_nc == 1 ? 0 : j);
      octave_idx_type jb = (b_nc == 1 ? 0 : j);

      TA ca = (a_row ? elem (a, 0, ja) : TA ());
      TB cb = (b_row ? elem (b, 0, jb) : TB ());

      auto a_at = [&] (octave_idx_type i)
      { return (a_row ? ca : elem (a, i, ja)); };
      auto b_at = [&] (octave_idx_type i)
      { return (b_row ? cb : elem (b, i, jb)); };

      auto keep = [&] (octave_idx_type i)
      {
        RT v = fcn (a_at (i), b_at (i));
        if (nonzero (v))
          {
            r_ridx.push_back (i);
            r_data.push_back (v);
          }
      };

      if (zero_fill || (a_row && nonzero (fcn (ca, TB ())))
          || (b_row && nonzero (fcn (TA (), cb))))
        {
          // A repeated row that is nonzero against zero fills the column.
          for (octave_idx_type i = 0; i < nr; i++)
            keep (i);
        }
      else
        {
          a_rows.clear ();
          b_rows.clear ();
          rows.clear ();

          if (a_col)
            {
              if (b_row && cb != TB ())
                stored (a, 0, a_rows);
              else
                a_rows = a_fill;
            }
          else if (! a_row)
            stored (a, ja, a_rows);

          if (b_col)
            {
              if (a_row && ca != TA ())
                stored (b, 0, b_rows);
              else
                b_rows = b_fill;
            }
          else if (! b_row)
            stored (b, jb, b_rows);

          std::set_union (a_rows.begin (), a_rows.end (),
                          b_rows.begin (), b_rows.end (),
                          std::back_inserter (rows));

          for (octave_idx_type i : rows)
            keep (i);
        }

      r_cidx[j+1] = r_ridx.size ();
    }

  Sparse<RT> r (dim_vector (nr, nc), r_ridx.size ());

  std::copy (r_cidx.begin (), r_cidx.end (), r.cidx ());
  std::copy (r_ridx.begin (), r_ridx.end (), r.ridx ());
  std::copy (r_data.begin (), r_data.end (), r.data ());

  return r;
}

OCTAVE_END_NAMESPACE(octave)

template <typename T>
MSparse<T>
product (const MSparse<T>& a, const MSparse<T>& b)
{
  MSparse<T> r;

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 1 && a_nc == 1)
    r = a.xelem (0, 0) * b;
  else if (b_nr == 1 && b_nc == 1)
    r = a * b.xelem (0, 0);
  else if (a_nr == 0 && (b_nr == 0 || b_nr == 1))
    {
      if (a_nc == 1 || b_nc == 1 || a_nc == b_nc)
        r.resize (a_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant ("product", a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nc == 0 && (b_nc == 0 || b_nc == 1))
    {
      if (a_nr == 1 || b_nr == 1 || a_nr == b_nr)
        r.resize (std::max (a_nr, b_nr), a_nc);
      else
        octave::err_nonconformant ("product", a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nr == 0 && (a_nr == 0 || a_nr == 1))
    {
      if (b_nc == 1 || a_nc == 1 || b_nc == a_nc)
        r.resize (b_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant ("product", a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nc == 0 && (a_nc == 0 || a_nc == 1))
    {
      if (b_nr == 1 || a_nr == 1 || b_nr == a_nr)
        r.resize (std::max (a_nr, b_nr), b_nc);
      else
        octave::err_nonconformant ("product", a_nr, a_nc, b_nr, b_nc);
    }
  else
    {
      r = octave::sparse_bcast_op<T> (a, b,
                                      [] (const T& x, const T& y)
                                      { return x * y; },
                                      "product");
    }

  return r;
}

template <typename T>
MSparse<T>
quotient (const MSparse<T>& a, const MSparse<T>& b)
{
  MSparse<T> r;
  T Zero = T ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 1 && a_nc == 1)
    {
      T val = a.elem (0, 0);
      T fill = val / T ();
      if (fill == T ())
        {
          octave_idx_type b_nnz = b.nnz ();
          r = MSparse<T> (b);
          for (octave_idx_type i = 0 ; i < b_nnz ; i++)
            r.data (i) = val / r.data (i);
          r.maybe_compress (true);
        }
      else
        {
          r = MSparse<T> (b_nr, b_nc, fill);
          for (octave_idx_type j = 0 ; j < b_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * b_nr;
              for (octave_idx_type i = b.cidx (j) ; i < b.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + b.ridx (i)) = val / b.data (i);
                }
            }
          r.maybe_compress (true);
        }
    }
  else if (b_nr == 1 && b_nc == 1)
    {
      T val = b.elem (0, 0);
      T fill = T () / val;
      if (fill == T ())
        {
          octave_idx_type a_nnz = a.nnz ();
          r = MSparse<T> (a);
          for (octave_idx_type i = 0 ; i < a_nnz ; i++)
            r.data (i) = r.data (i) / val;
          r.maybe_compress (true);
        }
      else
        {
          r = MSparse<T> (a_nr, a_nc, fill);
          for (octave_idx_type j = 0 ; j < a_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * a_nr;
              for (octave_idx_type i = a.cidx (j) ; i < a.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + a.ridx (i)) = a.data (i) / val;
                }
            }
          r.maybe_compress (true);
        }
    }
  else if (a_nr == 0 && (b_nr == 0 || b_nr == 1))
    {
      if (a_nc == 1 || b_nc == 1 || a_nc == b_nc)
        r.resize (a_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant ("quotient", a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nc == 0 && (b_nc == 0 || b_nc == 1))
    {
      if (a_nr == 1 || b_nr == 1 || a_nr == b_nr)
        r.resize (std::max (a_nr, b_nr), a_nc);
      else
        octave::err_nonconformant ("quotient", a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nr == 0 && (a_nr == 0 || a_nr == 1))
    {
      if (b_nc == 1 || a_nc == 1 || b_nc == a_nc)
        r.resize (b_nr, std::max (a_nc, b_nc));
      else
        octave::err_nonconformant ("quotient", a_nr, a_nc, b_nr, b_nc);
    }
  else if (b_nc == 0 && (a_nc == 0 || a_nc == 1))
    {
      if (b_nr == 1 || a_nr == 1 || b_nr == a_nr)
        r.resize (std::max (a_nr, b_nr), b_nc);
      else
        octave::err_nonconformant ("quotient", a_nr, a_nc, b_nr, b_nc);
    }
  else if (a_nr == b_nr && a_nc == b_nc)
    {
      r = MSparse<T> (a_nr, a_nc, (Zero / Zero));

      // R stores every element, so write each one by its position.

      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type ja = a.cidx (i);
          octave_idx_type ja_max = a.cidx (i+1);
          bool ja_lt_max = ja < ja_max;

          octave_idx_type jb = b.cidx (i);
          octave_idx_type jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.xdata (i * a_nr + a.ridx (ja)) = a.data (ja) / Zero;
                  ja++;
                  ja_lt_max = ja < ja_max;
                }
              else if ((! ja_lt_max) || (b.ridx (jb) < a.ridx (ja)))
                {
                  r.xdata (i * a_nr + b.ridx (jb)) = Zero / b.data (jb);
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
              else
                {
                  r.xdata (i * a_nr + a.ridx (ja)) = a.data (ja) / b.data (jb);
                  ja++;
                  ja_lt_max = ja < ja_max;
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
            }
        }
      r.maybe_compress (true);
    }
  else if (a_nr == b_nr && (a_nc == 1 || b_nc == 1))
    {
      octave_idx_type r_nc = (a_nc < b_nc ? b_nc : a_nc);
      r = MSparse<T> (a_nr, r_nc, (Zero / Zero));

      // R stores every element, so write each one by its position.

      for (octave_idx_type i = 0 ; i < r_nc ; i++)
        {
          octave_idx_type ja;
          octave_idx_type ja_max;
          octave_idx_type jb;
          octave_idx_type jb_max;
          if (a_nc == 1)
            {
              ja = a.cidx(0);
              ja_max = a.cidx(1);
              jb = b.cidx (i);
              jb_max = b.cidx (i+1);
            }
          else
            {
              ja = a.cidx(i);
              ja_max = a.cidx(i+1);
              jb = b.cidx (0);
              jb_max = b.cidx (1);
            }
          bool ja_lt_max = ja < ja_max;
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.xdata (i * a_nr + a.ridx (ja)) = a.data (ja) / Zero;
                  ja++;
                  ja_lt_max = ja < ja_max;
                }
              else if ((! ja_lt_max) || (b.ridx (jb) < a.ridx (ja)))
                {
                  r.xdata (i * a_nr + b.ridx (jb)) = Zero / b.data (jb);
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
              else
                {
                  r.xdata (i * a_nr + a.ridx (ja)) = a.data (ja) / b.data (jb);
                  ja++;
                  ja_lt_max = ja < ja_max;
                  jb++;
                  jb_lt_max = jb < jb_max;
                }
            }
        }
      r.maybe_compress (true);
    }
  else if (a_nc == b_nc && (a_nr == 1 || b_nr == 1))
    r = quotient (a.transpose (), b.transpose ()).transpose ();
  else if (a_nr == 1 && b_nc == 1)
    r = octave::sparse_bcast_op<T> (a, b, std::divides<T> (), "quotient");
  else if (a_nc == 1 && b_nr == 1)
    r = quotient (a.transpose (), b.transpose ()).transpose ();
  else
    octave::err_nonconformant ("quotient", a_nr, a_nc, b_nr, b_nc);

  return r;
}

// Unary MSparse ops.

template <typename T>
MSparse<T>
operator + (const MSparse<T>& a)
{
  return a;
}

template <typename T>
MSparse<T>
operator - (const MSparse<T>& a)
{
  MSparse<T> retval (a);
  octave_idx_type nz = a.nnz ();
  for (octave_idx_type i = 0; i < nz; i++)
    retval.data (i) = - retval.data (i);
  return retval;
}
