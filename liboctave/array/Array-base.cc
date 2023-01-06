////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

// Do not include this file directly to instantiate the Array<T> template
// class in code outside liboctave.  Include "Array-oct.cc" or "Array.cc"
// instead.

// This file should not include config.h.  It is only included in other
// C++ source files that should have included config.h before including
// this file.

#include <ostream>

#include "Array-util.h"
#include "Array.h"
#include "lo-mappers.h"
#include "oct-locbuf.h"

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <typename T, typename Alloc>
typename Array<T, Alloc>::ArrayRep *
Array<T, Alloc>::nil_rep (void)
{
  static ArrayRep nr;
  return &nr;
}

// This is similar to the template for containers but specialized for Array.
// Note we can't specialize a member without also specializing the class.
template <typename T, typename Alloc>
Array<T, Alloc>::Array (const Array<T, Alloc>& a, const dim_vector& dv)
  : m_dimensions (dv), m_rep (a.m_rep),
    m_slice_data (a.m_slice_data), m_slice_len (a.m_slice_len)
{
  bool invalid_size = false;

  octave_idx_type new_numel = 0;

  try
    {
      // Safe numel may throw a bad_alloc error because the new
      // dimensions are too large to allocate.  Trap that here so
      // we can issue a more helpful diagnostic that is consistent
      // with other size mismatch failures.

      new_numel = m_dimensions.safe_numel ();
    }
  catch (const std::bad_alloc&)
    {
      invalid_size = true;
    }

  if (invalid_size || new_numel != a.numel ())
    {
      std::string dimensions_str = a.m_dimensions.str ();
      std::string new_dims_str = m_dimensions.str ();

      (*current_liboctave_error_handler)
        ("reshape: can't reshape %s array to %s array",
         dimensions_str.c_str (), new_dims_str.c_str ());
    }

  // This goes here because if an exception is thrown by the above,
  // destructor will be never called.
  m_rep->m_count++;
  m_dimensions.chop_trailing_singletons ();
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::fill (const T& val)
{
  if (m_rep->m_count > 1)
    {
      --m_rep->m_count;
      m_rep = new ArrayRep (numel (), val);
      m_slice_data = m_rep->m_data;
    }
  else
    std::fill_n (m_slice_data, m_slice_len, val);
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::clear (void)
{
  if (--m_rep->m_count == 0)
    delete m_rep;

  m_rep = nil_rep ();
  m_rep->m_count++;
  m_slice_data = m_rep->m_data;
  m_slice_len = m_rep->m_len;

  m_dimensions = dim_vector ();
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::clear (const dim_vector& dv)
{
  if (--m_rep->m_count == 0)
    delete m_rep;

  m_rep = new ArrayRep (dv.safe_numel ());
  m_slice_data = m_rep->m_data;
  m_slice_len = m_rep->m_len;

  m_dimensions = dv;
  m_dimensions.chop_trailing_singletons ();
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::squeeze (void) const
{
  Array<T, Alloc> retval = *this;

  if (ndims () > 2)
    {
      bool dims_changed = false;

      dim_vector new_dimensions = m_dimensions;

      int k = 0;

      for (int i = 0; i < ndims (); i++)
        {
          if (m_dimensions(i) == 1)
            dims_changed = true;
          else
            new_dimensions(k++) = m_dimensions(i);
        }

      if (dims_changed)
        {
          switch (k)
            {
            case 0:
              new_dimensions = dim_vector (1, 1);
              break;

            case 1:
              {
                octave_idx_type tmp = new_dimensions(0);

                new_dimensions.resize (2);

                new_dimensions(0) = tmp;
                new_dimensions(1) = 1;
              }
              break;

            default:
              new_dimensions.resize (k);
              break;
            }
        }

      retval = Array<T, Alloc> (*this, new_dimensions);
    }

  return retval;
}

template <typename T, typename Alloc>
octave_idx_type
Array<T, Alloc>::compute_index (octave_idx_type i, octave_idx_type j) const
{
  return ::compute_index (i, j, m_dimensions);
}

template <typename T, typename Alloc>
octave_idx_type
Array<T, Alloc>::compute_index (octave_idx_type i, octave_idx_type j,
                         octave_idx_type k) const
{
  return ::compute_index (i, j, k, m_dimensions);
}

template <typename T, typename Alloc>
octave_idx_type
Array<T, Alloc>::compute_index (const Array<octave_idx_type>& ra_idx) const
{
  return ::compute_index (ra_idx, m_dimensions);
}

template <typename T, typename Alloc>
T&
Array<T, Alloc>::checkelem (octave_idx_type n)
{
  // Do checks directly to avoid recomputing m_slice_len.
  if (n < 0)
    octave::err_invalid_index (n);
  if (n >= m_slice_len)
    octave::err_index_out_of_range (1, 1, n+1, m_slice_len, m_dimensions);

  return elem (n);
}

template <typename T, typename Alloc>
T&
Array<T, Alloc>::checkelem (octave_idx_type i, octave_idx_type j)
{
  return elem (compute_index (i, j));
}

template <typename T, typename Alloc>
T&
Array<T, Alloc>::checkelem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
{
  return elem (compute_index (i, j, k));
}

template <typename T, typename Alloc>
T&
Array<T, Alloc>::checkelem (const Array<octave_idx_type>& ra_idx)
{
  return elem (compute_index (ra_idx));
}

template <typename T, typename Alloc>
typename Array<T, Alloc>::crefT
Array<T, Alloc>::checkelem (octave_idx_type n) const
{
  // Do checks directly to avoid recomputing m_slice_len.
  if (n < 0)
    octave::err_invalid_index (n);
  if (n >= m_slice_len)
    octave::err_index_out_of_range (1, 1, n+1, m_slice_len, m_dimensions);

  return elem (n);
}

template <typename T, typename Alloc>
typename Array<T, Alloc>::crefT
Array<T, Alloc>::checkelem (octave_idx_type i, octave_idx_type j) const
{
  return elem (compute_index (i, j));
}

template <typename T, typename Alloc>
typename Array<T, Alloc>::crefT
Array<T, Alloc>::checkelem (octave_idx_type i, octave_idx_type j,
                     octave_idx_type k) const
{
  return elem (compute_index (i, j, k));
}

template <typename T, typename Alloc>
typename Array<T, Alloc>::crefT
Array<T, Alloc>::checkelem (const Array<octave_idx_type>& ra_idx) const
{
  return elem (compute_index (ra_idx));
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::column (octave_idx_type k) const
{
  octave_idx_type r = m_dimensions(0);

  return Array<T, Alloc> (*this, dim_vector (r, 1), k*r, k*r + r);
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::page (octave_idx_type k) const
{
  octave_idx_type r = m_dimensions(0);
  octave_idx_type c = m_dimensions(1);
  octave_idx_type p = r*c;

  return Array<T, Alloc> (*this, dim_vector (r, c), k*p, k*p + p);
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::linear_slice (octave_idx_type lo, octave_idx_type up) const
{
  if (up < lo)
    up = lo;

  return Array<T, Alloc> (*this, dim_vector (up - lo, 1), lo, up);
}

// Helper class for multi-d dimension permuting (generalized transpose).
class rec_permute_helper
{
public:
  rec_permute_helper (const dim_vector& dv, const Array<octave_idx_type>& perm)

    : m_n (dv.ndims ()), m_top (0), m_dim (new octave_idx_type [2*m_n]),
      m_stride (m_dim + m_n), m_use_blk (false)
  {
    assert (m_n == perm.numel ());

    // Get cumulative dimensions.
    OCTAVE_LOCAL_BUFFER (octave_idx_type, cdim, m_n+1);
    cdim[0] = 1;
    for (int i = 1; i < m_n+1; i++) cdim[i] = cdim[i-1] * dv(i-1);

    // Setup the permuted strides.
    for (int k = 0; k < m_n; k++)
      {
        int kk = perm(k);
        m_dim[k] = dv(kk);
        m_stride[k] = cdim[kk];
      }

    // Reduce contiguous runs.
    for (int k = 1; k < m_n; k++)
      {
        if (m_stride[k] == m_stride[m_top]*m_dim[m_top])
          m_dim[m_top] *= m_dim[k];
        else
          {
            m_top++;
            m_dim[m_top] = m_dim[k];
            m_stride[m_top] = m_stride[k];
          }
      }

    // Determine whether we can use block transposes.
    m_use_blk = m_top >= 1 && m_stride[1] == 1 && m_stride[0] == m_dim[1];

  }

  // No copying!

  rec_permute_helper (const rec_permute_helper&) = delete;

  rec_permute_helper& operator = (const rec_permute_helper&) = delete;

  ~rec_permute_helper (void) { delete [] m_dim; }

  template <typename T>
  void permute (const T *src, T *dest) const { do_permute (src, dest, m_top); }

  // Helper method for fast blocked transpose.
  template <typename T>
  static T *
  blk_trans (const T *src, T *dest, octave_idx_type nr, octave_idx_type nc)
  {
    static const octave_idx_type m = 8;
    OCTAVE_LOCAL_BUFFER (T, blk, m*m);
    for (octave_idx_type kr = 0; kr < nr; kr += m)
      for (octave_idx_type kc = 0; kc < nc; kc += m)
        {
          octave_idx_type lr = std::min (m, nr - kr);
          octave_idx_type lc = std::min (m, nc - kc);
          if (lr == m && lc == m)
            {
              const T *ss = src + kc * nr + kr;
              for (octave_idx_type j = 0; j < m; j++)
                for (octave_idx_type i = 0; i < m; i++)
                  blk[j*m+i] = ss[j*nr + i];
              T *dd = dest + kr * nc + kc;
              for (octave_idx_type j = 0; j < m; j++)
                for (octave_idx_type i = 0; i < m; i++)
                  dd[j*nc+i] = blk[i*m+j];
            }
          else
            {
              const T *ss = src + kc * nr + kr;
              for (octave_idx_type j = 0; j < lc; j++)
                for (octave_idx_type i = 0; i < lr; i++)
                  blk[j*m+i] = ss[j*nr + i];
              T *dd = dest + kr * nc + kc;
              for (octave_idx_type j = 0; j < lr; j++)
                for (octave_idx_type i = 0; i < lc; i++)
                  dd[j*nc+i] = blk[i*m+j];
            }
        }

    return dest + nr*nc;
  }

private:

  // Recursive N-D generalized transpose
  template <typename T>
  T * do_permute (const T *src, T *dest, int lev) const
  {
    if (lev == 0)
      {
        octave_idx_type step = m_stride[0];
        octave_idx_type len = m_dim[0];
        if (step == 1)
          {
            std::copy_n (src, len, dest);
            dest += len;
          }
        else
          {
            for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
              dest[i] = src[j];

            dest += len;
          }
      }
    else if (m_use_blk && lev == 1)
      dest = blk_trans (src, dest, m_dim[1], m_dim[0]);
    else
      {
        octave_idx_type step = m_stride[lev];
        octave_idx_type len = m_dim[lev];
        for (octave_idx_type i = 0, j = 0; i < len; i++, j+= step)
          dest = do_permute (src + i * step, dest, lev-1);
      }

    return dest;
  }

  //--------

  // STRIDE occupies the last half of the space allocated for dim to
  // avoid a double allocation.

  int m_n;
  int m_top;
  octave_idx_type *m_dim;
  octave_idx_type *m_stride;
  bool m_use_blk;
};

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::permute (const Array<octave_idx_type>& perm_vec_arg, bool inv) const
{
  Array<T, Alloc> retval;

  Array<octave_idx_type> perm_vec = perm_vec_arg;

  dim_vector dv = dims ();

  int perm_vec_len = perm_vec_arg.numel ();

  if (perm_vec_len < dv.ndims ())
    (*current_liboctave_error_handler)
      ("%s: invalid permutation vector", inv ? "ipermute" : "permute");

  dim_vector dv_new = dim_vector::alloc (perm_vec_len);

  // Append singleton dimensions as needed.
  dv.resize (perm_vec_len, 1);

  // Need this array to check for identical elements in permutation array.
  OCTAVE_LOCAL_BUFFER_INIT (bool, checked, perm_vec_len, false);

  bool identity = true;

  // Find dimension vector of permuted array.
  for (int i = 0; i < perm_vec_len; i++)
    {
      octave_idx_type perm_elt = perm_vec.elem (i);
      if (perm_elt >= perm_vec_len || perm_elt < 0)
        (*current_liboctave_error_handler)
          ("%s: permutation vector contains an invalid element",
           inv ? "ipermute" : "permute");

      if (checked[perm_elt])
        (*current_liboctave_error_handler)
          ("%s: permutation vector cannot contain identical elements",
           inv ? "ipermute" : "permute");
      else
        {
          checked[perm_elt] = true;
          identity = identity && perm_elt == i;
        }
    }

  if (identity)
    return *this;

  if (inv)
    {
      for (int i = 0; i < perm_vec_len; i++)
        perm_vec(perm_vec_arg(i)) = i;
    }

  for (int i = 0; i < perm_vec_len; i++)
    dv_new(i) = dv(perm_vec(i));

  retval = Array<T, Alloc> (dv_new);

  if (numel () > 0)
    {
      rec_permute_helper rh (dv, perm_vec);
      rh.permute (data (), retval.fortran_vec ());
    }

  return retval;
}

// Helper class for multi-d index reduction and recursive
// indexing/indexed assignment.  Rationale: we could avoid recursion
// using a state machine instead.  However, using recursion is much
// more amenable to possible parallelization in the future.
// Also, the recursion solution is cleaner and more understandable.

class rec_index_helper
{
public:
  rec_index_helper (const dim_vector& dv, const Array<octave::idx_vector>& ia)
    : m_n (ia.numel ()), m_top (0), m_dim (new octave_idx_type [2*m_n]),
      m_cdim (m_dim + m_n), m_idx (new octave::idx_vector [m_n])
  {
    assert (m_n > 0 && (dv.ndims () == std::max (m_n, 2)));

    m_dim[0] = dv(0);
    m_cdim[0] = 1;
    m_idx[0] = ia(0);

    for (int i = 1; i < m_n; i++)
      {
        // Try reduction...
        if (m_idx[m_top].maybe_reduce (m_dim[m_top], ia(i), dv(i)))
          {
            // Reduction successful, fold dimensions.
            m_dim[m_top] *= dv(i);
          }
        else
          {
            // Unsuccessful, store index & cumulative m_dim.
            m_top++;
            m_idx[m_top] = ia(i);
            m_dim[m_top] = dv(i);
            m_cdim[m_top] = m_cdim[m_top-1] * m_dim[m_top-1];
          }
      }
  }

  // No copying!

  rec_index_helper (const rec_index_helper&) = delete;

  rec_index_helper& operator = (const rec_index_helper&) = delete;

  ~rec_index_helper (void) { delete [] m_idx; delete [] m_dim; }

  template <typename T>
  void index (const T *src, T *dest) const { do_index (src, dest, m_top); }

  template <typename T>
  void assign (const T *src, T *dest) const { do_assign (src, dest, m_top); }

  template <typename T>
  void fill (const T& val, T *dest) const { do_fill (val, dest, m_top); }

  bool is_cont_range (octave_idx_type& l, octave_idx_type& u) const
  {
    return m_top == 0 && m_idx[0].is_cont_range (m_dim[0], l, u);
  }

private:

  // Recursive N-D indexing
  template <typename T>
  T * do_index (const T *src, T *dest, int lev) const
  {
    if (lev == 0)
      dest += m_idx[0].index (src, m_dim[0], dest);
    else
      {
        octave_idx_type nn = m_idx[lev].length (m_dim[lev]);
        octave_idx_type d = m_cdim[lev];
        for (octave_idx_type i = 0; i < nn; i++)
          dest = do_index (src + d*m_idx[lev].xelem (i), dest, lev-1);
      }

    return dest;
  }

  // Recursive N-D indexed assignment
  template <typename T>
  const T * do_assign (const T *src, T *dest, int lev) const
  {
    if (lev == 0)
      src += m_idx[0].assign (src, m_dim[0], dest);
    else
      {
        octave_idx_type nn = m_idx[lev].length (m_dim[lev]);
        octave_idx_type d = m_cdim[lev];
        for (octave_idx_type i = 0; i < nn; i++)
          src = do_assign (src, dest + d*m_idx[lev].xelem (i), lev-1);
      }

    return src;
  }

  // Recursive N-D indexed assignment
  template <typename T>
  void do_fill (const T& val, T *dest, int lev) const
  {
    if (lev == 0)
      m_idx[0].fill (val, m_dim[0], dest);
    else
      {
        octave_idx_type nn = m_idx[lev].length (m_dim[lev]);
        octave_idx_type d = m_cdim[lev];
        for (octave_idx_type i = 0; i < nn; i++)
          do_fill (val, dest + d*m_idx[lev].xelem (i), lev-1);
      }
  }

  //--------

  // CDIM occupies the last half of the space allocated for dim to
  // avoid a double allocation.

  int m_n;
  int m_top;
  octave_idx_type *m_dim;
  octave_idx_type *m_cdim;
  octave::idx_vector *m_idx;
};

// Helper class for multi-d recursive resizing
// This handles resize () in an efficient manner, touching memory only
// once (apart from reinitialization)
class rec_resize_helper
{
public:
  rec_resize_helper (const dim_vector& ndv, const dim_vector& odv)
    : m_cext (nullptr), m_sext (nullptr), m_dext (nullptr), m_n (0)
  {
    int l = ndv.ndims ();
    assert (odv.ndims () == l);
    octave_idx_type ld = 1;
    int i = 0;
    for (; i < l-1 && ndv(i) == odv(i); i++) ld *= ndv(i);
    m_n = l - i;
    m_cext = new octave_idx_type [3*m_n];
    // Trick to avoid three allocations
    m_sext = m_cext + m_n;
    m_dext = m_sext + m_n;

    octave_idx_type sld = ld;
    octave_idx_type dld = ld;
    for (int j = 0; j < m_n; j++)
      {
        m_cext[j] = std::min (ndv(i+j), odv(i+j));
        m_sext[j] = sld *= odv(i+j);
        m_dext[j] = dld *= ndv(i+j);
      }
    m_cext[0] *= ld;
  }

  // No copying!

  rec_resize_helper (const rec_resize_helper&) = delete;

  rec_resize_helper& operator = (const rec_resize_helper&) = delete;

  ~rec_resize_helper (void) { delete [] m_cext; }

  template <typename T>
  void resize_fill (const T *src, T *dest, const T& rfv) const
  { do_resize_fill (src, dest, rfv, m_n-1); }

private:

  // recursive resizing
  template <typename T>
  void do_resize_fill (const T *src, T *dest, const T& rfv, int lev) const
  {
    if (lev == 0)
      {
        std::copy_n (src, m_cext[0], dest);
        std::fill_n (dest + m_cext[0], m_dext[0] - m_cext[0], rfv);
      }
    else
      {
        octave_idx_type sd, dd, k;
        sd = m_sext[lev-1];
        dd = m_dext[lev-1];
        for (k = 0; k < m_cext[lev]; k++)
          do_resize_fill (src + k * sd, dest + k * dd, rfv, lev - 1);

        std::fill_n (dest + k * dd, m_dext[lev] - k * dd, rfv);
      }
  }

  //--------

  octave_idx_type *m_cext;
  octave_idx_type *m_sext;
  octave_idx_type *m_dext;
  int m_n;
};

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::index (const octave::idx_vector& i) const
{
  // Colon:
  //
  //   object   | index    | result orientation
  //   ---------+----------+-------------------
  //   anything | colon    | column vector
  //
  // Numeric array or logical mask:
  //
  //   Note that logical mask indices are always transformed to vectors
  //   before we see them.
  //
  //   object   | index    | result orientation
  //   ---------+----------+-------------------
  //   vector   | vector   | indexed object
  //            | other    | same size as index
  //   ---------+----------+-------------------
  //   array    | anything | same size as index

  octave_idx_type n = numel ();
  Array<T, Alloc> retval;

  if (i.is_colon ())
    {
      // A(:) produces a shallow copy as a column vector.
      retval = Array<T, Alloc> (*this, dim_vector (n, 1));
    }
  else
    {
      if (i.extent (n) != n)
        octave::err_index_out_of_range (1, 1, i.extent (n), n, m_dimensions);

      dim_vector result_dims = i.orig_dimensions ();
      octave_idx_type idx_len = i.length ();

      if (n != 1 && is_nd_vector () && idx_len != 1
          && result_dims.is_nd_vector ())
        {
          // Indexed object and index are both vectors.  Set result size
          // and orientation as above.

          dim_vector dv = dims ();

          result_dims = dv.make_nd_vector (idx_len);
        }

      octave_idx_type l, u;
      if (idx_len != 0 && i.is_cont_range (n, l, u))
        // If suitable, produce a shallow slice.
        retval = Array<T, Alloc> (*this, result_dims, l, u);
      else
        {
          // Don't use resize here to avoid useless initialization for POD
          // types.
          retval = Array<T, Alloc> (result_dims);

          if (idx_len != 0)
            i.index (data (), n, retval.fortran_vec ());
        }
    }

  return retval;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::index (const octave::idx_vector& i, const octave::idx_vector& j) const
{
  // Get dimensions, allowing Fortran indexing in the 2nd dim.
  dim_vector dv = m_dimensions.redim (2);
  octave_idx_type r = dv(0);
  octave_idx_type c = dv(1);
  Array<T, Alloc> retval;

  if (i.is_colon () && j.is_colon ())
    {
      // A(:,:) produces a shallow copy.
      retval = Array<T, Alloc> (*this, dv);
    }
  else
    {
      if (i.extent (r) != r)
        octave::err_index_out_of_range (2, 1, i.extent (r), r, m_dimensions); // throws
      if (j.extent (c) != c)
        octave::err_index_out_of_range (2, 2, j.extent (c), c, m_dimensions); // throws

      octave_idx_type n = numel ();
      octave_idx_type il = i.length (r);
      octave_idx_type jl = j.length (c);

      octave::idx_vector ii (i);

      if (ii.maybe_reduce (r, j, c))
        {
          octave_idx_type l, u;
          if (ii.length () > 0 && ii.is_cont_range (n, l, u))
            // If suitable, produce a shallow slice.
            retval = Array<T, Alloc> (*this, dim_vector (il, jl), l, u);
          else
            {
              // Don't use resize to avoid useless initialization for POD types.
              retval = Array<T, Alloc> (dim_vector (il, jl));

              ii.index (data (), n, retval.fortran_vec ());
            }
        }
      else
        {
          // Don't use resize to avoid useless initialization for POD types.
          retval = Array<T, Alloc> (dim_vector (il, jl));

          const T *src = data ();
          T *dest = retval.fortran_vec ();

          for (octave_idx_type k = 0; k < jl; k++)
            dest += i.index (src + r * j.xelem (k), r, dest);
        }
    }

  return retval;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::index (const Array<octave::idx_vector>& ia) const
{
  int ial = ia.numel ();
  Array<T, Alloc> retval;

  // FIXME: is this dispatching necessary?
  if (ial == 1)
    retval = index (ia(0));
  else if (ial == 2)
    retval = index (ia(0), ia(1));
  else if (ial > 0)
    {
      // Get dimensions, allowing Fortran indexing in the last dim.
      dim_vector dv = m_dimensions.redim (ial);

      // Check for out of bounds conditions.
      bool all_colons = true;
      for (int i = 0; i < ial; i++)
        {
          if (ia(i).extent (dv(i)) != dv(i))
            octave::err_index_out_of_range (ial, i+1, ia(i).extent (dv(i)), dv(i),
                                            m_dimensions); // throws

          all_colons = all_colons && ia(i).is_colon ();
        }

      if (all_colons)
        {
          // A(:,:,...,:) produces a shallow copy.
          dv.chop_trailing_singletons ();
          retval = Array<T, Alloc> (*this, dv);
        }
      else
        {
          // Form result dimensions.
          dim_vector rdv = dim_vector::alloc (ial);
          for (int i = 0; i < ial; i++) rdv(i) = ia(i).length (dv(i));
          rdv.chop_trailing_singletons ();

          // Prepare for recursive indexing
          rec_index_helper rh (dv, ia);

          octave_idx_type l, u;
          if (rh.is_cont_range (l, u))
            // If suitable, produce a shallow slice.
            retval = Array<T, Alloc> (*this, rdv, l, u);
          else
            {
              // Don't use resize to avoid useless initialization for POD types.
              retval = Array<T, Alloc> (rdv);

              // Do it.
              rh.index (data (), retval.fortran_vec ());
            }
        }
    }

  return retval;
}

// The default fill value.  Override if you want a different one.

template <typename T, typename Alloc>
T
Array<T, Alloc>::resize_fill_value (void) const
{
  static T zero = T ();
  return zero;
}

// Yes, we could do resize using index & assign.  However, that would
// possibly involve a lot more memory traffic than we actually need.

template <typename T, typename Alloc>
void
Array<T, Alloc>::resize1 (octave_idx_type n, const T& rfv)
{
  if (n < 0 || ndims () != 2)
    octave::err_invalid_resize ();

  dim_vector dv;
  // This is driven by Matlab's behavior of giving a *row* vector
  // on some out-of-bounds assignments.  Specifically, Matlab
  // allows a(i) with out-of-bouds i when a is either of 0x0, 1x0,
  // 1x1, 0xN, and gives a row vector in all cases (yes, even the
  // last one, search me why).  Giving a column vector would make
  // much more sense (given the way trailing singleton dims are
  // treated).
  bool invalid = false;
  if (rows () == 0 || rows () == 1)
    dv = dim_vector (1, n);
  else if (columns () == 1)
    dv = dim_vector (n, 1);
  else
    invalid = true;

  if (invalid)
    octave::err_invalid_resize ();

  octave_idx_type nx = numel ();
  if (n == nx - 1 && n > 0)
    {
      // Stack "pop" operation.
      if (m_rep->m_count == 1)
        m_slice_data[m_slice_len-1] = T ();
      m_slice_len--;
      m_dimensions = dv;
    }
  else if (n == nx + 1 && nx > 0)
    {
      // Stack "push" operation.
      if (m_rep->m_count == 1
          && m_slice_data + m_slice_len < m_rep->m_data + m_rep->m_len)
        {
          m_slice_data[m_slice_len++] = rfv;
          m_dimensions = dv;
        }
      else
        {
          static const octave_idx_type max_stack_chunk = 1024;
          octave_idx_type nn = n + std::min (nx, max_stack_chunk);
          Array<T, Alloc> tmp (Array<T, Alloc> (dim_vector (nn, 1)), dv, 0, n);
          T *dest = tmp.fortran_vec ();

          std::copy_n (data (), nx, dest);
          dest[nx] = rfv;

          *this = tmp;
        }
    }
  else if (n != nx)
    {
      Array<T, Alloc> tmp = Array<T, Alloc> (dv);
      T *dest = tmp.fortran_vec ();

      octave_idx_type n0 = std::min (n, nx);
      octave_idx_type n1 = n - n0;
      std::copy_n (data (), n0, dest);
      std::fill_n (dest + n0, n1, rfv);

      *this = tmp;
    }
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::resize2 (octave_idx_type r, octave_idx_type c, const T& rfv)
{
  if (r < 0 || c < 0 || ndims () != 2)
    octave::err_invalid_resize ();

  octave_idx_type rx = rows ();
  octave_idx_type cx = columns ();
  if (r != rx || c != cx)
    {
      Array<T, Alloc> tmp = Array<T, Alloc> (dim_vector (r, c));
      T *dest = tmp.fortran_vec ();

      octave_idx_type r0 = std::min (r, rx);
      octave_idx_type r1 = r - r0;
      octave_idx_type c0 = std::min (c, cx);
      octave_idx_type c1 = c - c0;
      const T *src = data ();
      if (r == rx)
        {
          std::copy_n (src, r * c0, dest);
          dest += r * c0;
        }
      else
        {
          for (octave_idx_type k = 0; k < c0; k++)
            {
              std::copy_n (src, r0, dest);
              src += rx;
              dest += r0;
              std::fill_n (dest, r1, rfv);
              dest += r1;
            }
        }

      std::fill_n (dest, r * c1, rfv);

      *this = tmp;
    }
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::resize (const dim_vector& dv, const T& rfv)
{
  int dvl = dv.ndims ();
  if (dvl == 2)
    resize2 (dv(0), dv(1), rfv);
  else if (m_dimensions != dv)
    {
      if (m_dimensions.ndims () > dvl || dv.any_neg ())
        octave::err_invalid_resize ();

      Array<T, Alloc> tmp (dv);
      // Prepare for recursive resizing.
      rec_resize_helper rh (dv, m_dimensions.redim (dvl));

      // Do it.
      rh.resize_fill (data (), tmp.fortran_vec (), rfv);
      *this = tmp;
    }
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::index (const octave::idx_vector& i, bool resize_ok, const T& rfv) const
{
  Array<T, Alloc> tmp = *this;
  if (resize_ok)
    {
      octave_idx_type n = numel ();
      octave_idx_type nx = i.extent (n);
      if (n != nx)
        {
          if (i.is_scalar ())
            return Array<T, Alloc> (dim_vector (1, 1), rfv);
          else
            tmp.resize1 (nx, rfv);
        }

      if (tmp.numel () != nx)
        return Array<T, Alloc> ();
    }

  return tmp.index (i);
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::index (const octave::idx_vector& i, const octave::idx_vector& j,
                 bool resize_ok, const T& rfv) const
{
  Array<T, Alloc> tmp = *this;
  if (resize_ok)
    {
      dim_vector dv = m_dimensions.redim (2);
      octave_idx_type r = dv(0);
      octave_idx_type c = dv(1);
      octave_idx_type rx = i.extent (r);
      octave_idx_type cx = j.extent (c);
      if (r != rx || c != cx)
        {
          if (i.is_scalar () && j.is_scalar ())
            return Array<T, Alloc> (dim_vector (1, 1), rfv);
          else
            tmp.resize2 (rx, cx, rfv);
        }

      if (tmp.rows () != rx || tmp.columns () != cx)
        return Array<T, Alloc> ();
    }

  return tmp.index (i, j);
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::index (const Array<octave::idx_vector>& ia,
                 bool resize_ok, const T& rfv) const
{
  Array<T, Alloc> tmp = *this;
  if (resize_ok)
    {
      int ial = ia.numel ();
      dim_vector dv = m_dimensions.redim (ial);
      dim_vector dvx = dim_vector::alloc (ial);
      for (int i = 0; i < ial; i++)
        dvx(i) = ia(i).extent (dv(i));
      if (! (dvx == dv))
        {
          bool all_scalars = true;
          for (int i = 0; i < ial; i++)
            all_scalars = all_scalars && ia(i).is_scalar ();
          if (all_scalars)
            return Array<T, Alloc> (dim_vector (1, 1), rfv);
          else
            tmp.resize (dvx, rfv);

          if (tmp.m_dimensions != dvx)
            return Array<T, Alloc> ();
        }
    }

  return tmp.index (ia);
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::assign (const octave::idx_vector& i, const Array<T, Alloc>& rhs, const T& rfv)
{
  octave_idx_type n = numel ();
  octave_idx_type rhl = rhs.numel ();

  if (rhl != 1 && i.length (n) != rhl)
    octave::err_nonconformant ("=", dim_vector(i.length(n), 1), rhs.dims());

  octave_idx_type nx = i.extent (n);
  bool colon = i.is_colon_equiv (nx);
  // Try to resize first if necessary.
  if (nx != n)
    {
      // Optimize case A = []; A(1:n) = X with A empty.
      if (m_dimensions.zero_by_zero () && colon)
        {
          if (rhl == 1)
            *this = Array<T, Alloc> (dim_vector (1, nx), rhs(0));
          else
            *this = Array<T, Alloc> (rhs, dim_vector (1, nx));
          return;
        }

      resize1 (nx, rfv);
      n = numel ();
    }

  if (colon)
    {
      // A(:) = X makes a full fill or a shallow copy.
      if (rhl == 1)
        fill (rhs(0));
      else
        *this = rhs.reshape (m_dimensions);
    }
  else
    {
      if (rhl == 1)
        i.fill (rhs(0), n, fortran_vec ());
      else
        i.assign (rhs.data (), n, fortran_vec ());
    }
}

// Assignment to a 2-dimensional array
template <typename T, typename Alloc>
void
Array<T, Alloc>::assign (const octave::idx_vector& i, const octave::idx_vector& j,
                  const Array<T, Alloc>& rhs, const T& rfv)
{
  bool initial_dims_all_zero = m_dimensions.all_zero ();

  // Get RHS extents, discarding singletons.
  dim_vector rhdv = rhs.dims ();

  // Get LHS extents, allowing Fortran indexing in the second dim.
  dim_vector dv = m_dimensions.redim (2);

  // Check for out-of-bounds and form resizing m_dimensions.
  dim_vector rdv;

  // In the special when all dimensions are zero, colons are allowed
  // to inquire the shape of RHS.  The rules are more obscure, so we
  // solve that elsewhere.
  if (initial_dims_all_zero)
    rdv = zero_dims_inquire (i, j, rhdv);
  else
    {
      rdv(0) = i.extent (dv(0));
      rdv(1) = j.extent (dv(1));
    }

  bool isfill = rhs.numel () == 1;
  octave_idx_type il = i.length (rdv(0));
  octave_idx_type jl = j.length (rdv(1));
  rhdv.chop_all_singletons ();
  bool match = (isfill
                || (rhdv.ndims () == 2 && il == rhdv(0) && jl == rhdv(1)));
  match = match || (il == 1 && jl == rhdv(0) && rhdv(1) == 1);

  if (match)
    {
      bool all_colons = (i.is_colon_equiv (rdv(0))
                         && j.is_colon_equiv (rdv(1)));
      // Resize if requested.
      if (rdv != dv)
        {
          // Optimize case A = []; A(1:m, 1:n) = X
          if (dv.zero_by_zero () && all_colons)
            {
              if (isfill)
                *this = Array<T, Alloc> (rdv, rhs(0));
              else
                *this = Array<T, Alloc> (rhs, rdv);
              return;
            }

          resize (rdv, rfv);
          dv = m_dimensions;
        }

      if (all_colons)
        {
          // A(:,:) = X makes a full fill or a shallow copy
          if (isfill)
            fill (rhs(0));
          else
            *this = rhs.reshape (m_dimensions);
        }
      else
        {
          // The actual work.
          octave_idx_type n = numel ();
          octave_idx_type r = dv(0);
          octave_idx_type c = dv(1);
          octave::idx_vector ii (i);

          const T *src = rhs.data ();
          T *dest = fortran_vec ();

          // Try reduction first.
          if (ii.maybe_reduce (r, j, c))
            {
              if (isfill)
                ii.fill (*src, n, dest);
              else
                ii.assign (src, n, dest);
            }
          else
            {
              if (isfill)
                {
                  for (octave_idx_type k = 0; k < jl; k++)
                    i.fill (*src, r, dest + r * j.xelem (k));
                }
              else
                {
                  for (octave_idx_type k = 0; k < jl; k++)
                    src += i.assign (src, r, dest + r * j.xelem (k));
                }
            }
        }
    }
  // any empty RHS can be assigned to an empty LHS
  else if ((il != 0 && jl != 0) || (rhdv(0) != 0 && rhdv(1) != 0))
    octave::err_nonconformant ("=", il, jl, rhs.dim1 (), rhs.dim2 ());
}

// Assignment to a multi-dimensional array
template <typename T, typename Alloc>
void
Array<T, Alloc>::assign (const Array<octave::idx_vector>& ia,
                  const Array<T, Alloc>& rhs, const T& rfv)
{
  int ial = ia.numel ();

  // FIXME: is this dispatching necessary / desirable?
  if (ial == 1)
    assign (ia(0), rhs, rfv);
  else if (ial == 2)
    assign (ia(0), ia(1), rhs, rfv);
  else if (ial > 0)
    {
      bool initial_dims_all_zero = m_dimensions.all_zero ();

      // Get RHS extents, discarding singletons.
      dim_vector rhdv = rhs.dims ();

      // Get LHS extents, allowing Fortran indexing in the second dim.
      dim_vector dv = m_dimensions.redim (ial);

      // Get the extents forced by indexing.
      dim_vector rdv;

      // In the special when all dimensions are zero, colons are
      // allowed to inquire the shape of RHS.  The rules are more
      // obscure, so we solve that elsewhere.
      if (initial_dims_all_zero)
        rdv = zero_dims_inquire (ia, rhdv);
      else
        {
          rdv = dim_vector::alloc (ial);
          for (int i = 0; i < ial; i++)
            rdv(i) = ia(i).extent (dv(i));
        }

      // Check whether LHS and RHS match, up to singleton dims.
      bool match = true;
      bool all_colons = true;
      bool isfill = rhs.numel () == 1;

      rhdv.chop_all_singletons ();
      int j = 0;
      int rhdvl = rhdv.ndims ();
      for (int i = 0; i < ial; i++)
        {
          all_colons = all_colons && ia(i).is_colon_equiv (rdv(i));
          octave_idx_type l = ia(i).length (rdv(i));
          if (l == 1) continue;
          match = match && j < rhdvl && l == rhdv(j++);
        }

      match = match && (j == rhdvl || rhdv(j) == 1);
      match = match || isfill;

      if (match)
        {
          // Resize first if necessary.
          if (rdv != dv)
            {
              // Optimize case A = []; A(1:m, 1:n) = X
              if (dv.zero_by_zero () && all_colons)
                {
                  rdv.chop_trailing_singletons ();
                  if (isfill)
                    *this = Array<T, Alloc> (rdv, rhs(0));
                  else
                    *this = Array<T, Alloc> (rhs, rdv);
                  return;
                }

              resize (rdv, rfv);
              dv = rdv;
            }

          if (all_colons)
            {
              // A(:,:,...,:) = X makes a full fill or a shallow copy.
              if (isfill)
                fill (rhs(0));
              else
                *this = rhs.reshape (m_dimensions);
            }
          else
            {
              // Do the actual work.

              // Prepare for recursive indexing
              rec_index_helper rh (dv, ia);

              // Do it.
              if (isfill)
                rh.fill (rhs(0), fortran_vec ());
              else
                rh.assign (rhs.data (), fortran_vec ());
            }
        }
      else
        {
          // dimension mismatch, unless LHS and RHS both empty
          bool lhsempty, rhsempty;
          lhsempty = rhsempty = false;
          dim_vector lhs_dv = dim_vector::alloc (ial);
          for (int i = 0; i < ial; i++)
            {
              octave_idx_type l = ia(i).length (rdv(i));
              lhs_dv(i) = l;
              lhsempty = lhsempty || (l == 0);
              rhsempty = rhsempty || (rhdv(j++) == 0);
            }
          if (! lhsempty || ! rhsempty)
            {
              lhs_dv.chop_trailing_singletons ();
              octave::err_nonconformant ("=", lhs_dv, rhdv);
            }
        }
    }
}

/*
%!shared a
%! a = [1 2; 3 4];
%!error <op1 is 1x2, op2 is 1x3> a(1,:) = [1 2 3]
%!error <op1 is 2x1, op2 is 1x3> a(:,1) = [1 2 3]
%!error <op1 is 2x2, op2 is 2x1> a(1:2,2:3) = [1;2]
*/

template <typename T, typename Alloc>
void
Array<T, Alloc>::delete_elements (const octave::idx_vector& i)
{
  octave_idx_type n = numel ();
  if (i.is_colon ())
    {
      *this = Array<T, Alloc> ();
    }
  else if (i.length (n) != 0)
    {
      if (i.extent (n) != n)
        octave::err_del_index_out_of_range (true, i.extent (n), n);

      octave_idx_type l, u;
      bool col_vec = ndims () == 2 && columns () == 1 && rows () != 1;
      if (i.is_scalar () && i(0) == n-1 && m_dimensions.isvector ())
        {
          // Stack "pop" operation.
          resize1 (n-1);
        }
      else if (i.is_cont_range (n, l, u))
        {
          // Special case deleting a contiguous range.
          octave_idx_type m = n + l - u;
          Array<T, Alloc> tmp (dim_vector (col_vec ? m : 1, ! col_vec ? m : 1));
          const T *src = data ();
          T *dest = tmp.fortran_vec ();
          std::copy_n (src, l, dest);
          std::copy (src + u, src + n, dest + l);
          *this = tmp;
        }
      else
        {
          // Use index.
          *this = index (i.complement (n));
        }
    }
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::delete_elements (int dim, const octave::idx_vector& i)
{
  if (dim < 0 || dim >= ndims ())
    (*current_liboctave_error_handler) ("invalid dimension in delete_elements");

  octave_idx_type n = m_dimensions(dim);
  if (i.is_colon ())
    {
      *this = Array<T, Alloc> ();
    }
  else if (i.length (n) != 0)
    {
      if (i.extent (n) != n)
        octave::err_del_index_out_of_range (false, i.extent (n), n);

      octave_idx_type l, u;

      if (i.is_cont_range (n, l, u))
        {
          // Special case deleting a contiguous range.
          octave_idx_type nd = n + l - u;
          octave_idx_type dl = 1;
          octave_idx_type du = 1;
          dim_vector rdv = m_dimensions;
          rdv(dim) = nd;
          for (int k = 0; k < dim; k++) dl *= m_dimensions(k);
          for (int k = dim + 1; k < ndims (); k++) du *= m_dimensions(k);

          // Special case deleting a contiguous range.
          Array<T, Alloc> tmp = Array<T, Alloc> (rdv);
          const T *src = data ();
          T *dest = tmp.fortran_vec ();
          l *= dl; u *= dl; n *= dl;
          for (octave_idx_type k = 0; k < du; k++)
            {
              std::copy_n (src, l, dest);
              dest += l;
              std::copy (src + u, src + n, dest);
              dest += n - u;
              src += n;
            }

          *this = tmp;
        }
      else
        {
          // Use index.
          Array<octave::idx_vector> ia (dim_vector (ndims (), 1), octave::idx_vector::colon);
          ia (dim) = i.complement (n);
          *this = index (ia);
        }
    }
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::delete_elements (const Array<octave::idx_vector>& ia)
{
  int ial = ia.numel ();

  if (ial == 1)
    delete_elements (ia(0));
  else
    {
      int k, dim = -1;
      for (k = 0; k < ial; k++)
        {
          if (! ia(k).is_colon ())
            {
              if (dim < 0)
                dim = k;
              else
                break;
            }
        }
      if (dim < 0)
        {
          dim_vector dv = m_dimensions;
          dv(0) = 0;
          *this = Array<T, Alloc> (dv);
        }
      else if (k == ial)
        {
          delete_elements (dim, ia(dim));
        }
      else
        {
          // Allow the null assignment to succeed if it won't change
          // anything because the indices reference an empty slice,
          // provided that there is at most one non-colon (or
          // equivalent) index.  So, we still have the requirement of
          // deleting a slice, but it is OK if the slice is empty.

          // For compatibility with Matlab, stop checking once we see
          // more than one non-colon index or an empty index.  Matlab
          // considers "[]" to be an empty index but not "false".  We
          // accept both.

          bool empty_assignment = false;

          int num_non_colon_indices = 0;

          int nd = ndims ();

          for (int i = 0; i < ial; i++)
            {
              octave_idx_type dim_len = (i >= nd ? 1 : m_dimensions(i));

              if (ia(i).length (dim_len) == 0)
                {
                  empty_assignment = true;
                  break;
                }

              if (! ia(i).is_colon_equiv (dim_len))
                {
                  num_non_colon_indices++;

                  if (num_non_colon_indices == 2)
                    break;
                }
            }

          if (! empty_assignment)
            (*current_liboctave_error_handler)
              ("a null assignment can only have one non-colon index");
        }
    }

}

template <typename T, typename Alloc>
Array<T, Alloc>&
Array<T, Alloc>::insert (const Array<T, Alloc>& a, octave_idx_type r, octave_idx_type c)
{
  octave::idx_vector i (r, r + a.rows ());
  octave::idx_vector j (c, c + a.columns ());
  if (ndims () == 2 && a.ndims () == 2)
    assign (i, j, a);
  else
    {
      Array<octave::idx_vector> idx (dim_vector (a.ndims (), 1));
      idx(0) = i;
      idx(1) = j;
      for (int k = 2; k < a.ndims (); k++)
        idx(k) = octave::idx_vector (0, a.m_dimensions(k));
      assign (idx, a);
    }

  return *this;
}

template <typename T, typename Alloc>
Array<T, Alloc>&
Array<T, Alloc>::insert (const Array<T, Alloc>& a, const Array<octave_idx_type>& ra_idx)
{
  octave_idx_type n = ra_idx.numel ();
  Array<octave::idx_vector> idx (dim_vector (n, 1));
  const dim_vector dva = a.dims ().redim (n);
  for (octave_idx_type k = 0; k < n; k++)
    idx(k) = octave::idx_vector (ra_idx(k), ra_idx(k) + dva(k));

  assign (idx, a);

  return *this;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::transpose (void) const
{
  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr >= 8 && nc >= 8)
    {
      Array<T, Alloc> result (dim_vector (nc, nr));

      // Reuse the implementation used for permuting.

      rec_permute_helper::blk_trans (data (), result.fortran_vec (), nr, nc);

      return result;
    }
  else if (nr > 1 && nc > 1)
    {
      Array<T, Alloc> result (dim_vector (nc, nr));

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          result.xelem (j, i) = xelem (i, j);

      return result;
    }
  else
    {
      // Fast transpose for vectors and empty matrices.
      return Array<T, Alloc> (*this, dim_vector (nc, nr));
    }
}

template <typename T>
static T
no_op_fcn (const T& x)
{
  return x;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::hermitian (T (*fcn) (const T&)) const
{
  assert (ndims () == 2);

  if (! fcn)
    fcn = no_op_fcn<T>;

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr >= 8 && nc >= 8)
    {
      Array<T, Alloc> result (dim_vector (nc, nr));

      // Blocked transpose to attempt to avoid cache misses.

      T buf[64];

      octave_idx_type jj;
      for (jj = 0; jj < (nc - 8 + 1); jj += 8)
        {
          octave_idx_type ii;
          for (ii = 0; ii < (nr - 8 + 1); ii += 8)
            {
              // Copy to buffer
              for (octave_idx_type j = jj, k = 0, idxj = jj * nr;
                   j < jj + 8; j++, idxj += nr)
                for (octave_idx_type i = ii; i < ii + 8; i++)
                  buf[k++] = xelem (i + idxj);

              // Copy from buffer
              for (octave_idx_type i = ii, idxi = ii * nc; i < ii + 8;
                   i++, idxi += nc)
                for (octave_idx_type j = jj, k = i - ii; j < jj + 8;
                     j++, k+=8)
                  result.xelem (j + idxi) = fcn (buf[k]);
            }

          if (ii < nr)
            for (octave_idx_type j = jj; j < jj + 8; j++)
              for (octave_idx_type i = ii; i < nr; i++)
                result.xelem (j, i) = fcn (xelem (i, j));
        }

      for (octave_idx_type j = jj; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          result.xelem (j, i) = fcn (xelem (i, j));

      return result;
    }
  else
    {
      Array<T, Alloc> result (dim_vector (nc, nr));

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          result.xelem (j, i) = fcn (xelem (i, j));

      return result;
    }
}

/*

## Transpose tests for matrices of the tile size and plus or minus a row
## and with four tiles.

%!shared m7, mt7, m8, mt8, m9, mt9
%! m7 = reshape (1 : 7*8, 8, 7);
%! mt7 = [1:8; 9:16; 17:24; 25:32; 33:40; 41:48; 49:56];
%! m8 = reshape (1 : 8*8, 8, 8);
%! mt8 = mt8 = [mt7; 57:64];
%! m9 = reshape (1 : 9*8, 8, 9);
%! mt9 = [mt8; 65:72];

%!assert (m7', mt7)
%!assert ((1i*m7).', 1i * mt7)
%!assert ((1i*m7)', conj (1i * mt7))
%!assert (m8', mt8)
%!assert ((1i*m8).', 1i * mt8)
%!assert ((1i*m8)', conj (1i * mt8))
%!assert (m9', mt9)
%!assert ((1i*m9).', 1i * mt9)
%!assert ((1i*m9)', conj (1i * mt9))
%!assert ([m7, m8; m7, m8]', [mt7, mt7; mt8, mt8])
%!assert ((1i*[m7, m8; m7, m8]).', 1i * [mt7, mt7; mt8, mt8])
%!assert ((1i*[m7, m8; m7, m8])', conj (1i * [mt7, mt7; mt8, mt8]))
%!assert ([m8, m8; m8, m8]', [mt8, mt8; mt8, mt8])
%!assert ((1i*[m8, m8; m8, m8]).', 1i * [mt8, mt8; mt8, mt8])
%!assert ((1i*[m8, m8; m8, m8])', conj (1i * [mt8, mt8; mt8, mt8]))
%!assert ([m9, m8; m9, m8]', [mt9, mt9; mt8, mt8])
%!assert ((1i*[m9, m8; m9, m8]).', 1i * [mt9, mt9; mt8, mt8])
%!assert ((1i*[m9, m8; m9, m8])', conj (1i * [mt9, mt9; mt8, mt8]))

*/

template <typename T, typename Alloc>
T *
Array<T, Alloc>::fortran_vec (void)
{
  make_unique ();

  return m_slice_data;
}

// Non-real types don't have NaNs.
template <typename T>
inline bool
sort_isnan (typename ref_param<T>::type)
{
  return false;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::sort (int dim, sortmode mode) const
{
  if (dim < 0)
    (*current_liboctave_error_handler) ("sort: invalid dimension");

  Array<T, Alloc> m (dims ());

  dim_vector dv = m.dims ();

  if (m.numel () < 1)
    return m;

  if (dim >= dv.ndims ())
    dv.resize (dim+1, 1);

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  octave_sort<T> lsort;

  if (mode != UNSORTED)
    lsort.set_compare (mode);
  else
    return m;

  if (stride == 1)
    {
      // Special case along first dimension avoids gather/scatter AND directly
      // sorts into destination buffer for an 11% performance boost.
      for (octave_idx_type j = 0; j < iter; j++)
        {
          // Copy and partition out NaNs.
          // No need to special case integer types <T> from floating point
          // types <T> to avoid sort_isnan() test as it makes no discernible
          // performance impact.
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i];
              if (sort_isnan<T> (tmp))
                v[--ku] = tmp;
              else
                v[kl++] = tmp;
            }

          // sort.
          lsort.sort (v, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (v + ku, v + ns);
              if (mode == DESCENDING)
                std::rotate (v, v + ku, v + ns);
            }

          v += ns;
          ov += ns;
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (T, buf, ns);

      for (octave_idx_type j = 0; j < iter; j++)
        {
          octave_idx_type offset = j;
          octave_idx_type n_strides = j / stride;
          offset += n_strides * stride * (ns - 1);

          // gather and partition out NaNs.
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i*stride + offset];
              if (sort_isnan<T> (tmp))
                buf[--ku] = tmp;
              else
                buf[kl++] = tmp;
            }

          // sort.
          lsort.sort (buf, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (buf + ku, buf + ns);
              if (mode == DESCENDING)
                std::rotate (buf, buf + ku, buf + ns);
            }

          // scatter.
          for (octave_idx_type i = 0; i < ns; i++)
            v[i*stride + offset] = buf[i];
        }
    }

  return m;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::sort (Array<octave_idx_type>& sidx, int dim,
                       sortmode mode) const
{
  if (dim < 0 || dim >= ndims ())
    (*current_liboctave_error_handler) ("sort: invalid dimension");

  Array<T, Alloc> m (dims ());

  dim_vector dv = m.dims ();

  if (m.numel () < 1)
    {
      sidx = Array<octave_idx_type> (dv);
      return m;
    }

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  octave_sort<T> lsort;

  sidx = Array<octave_idx_type> (dv);
  octave_idx_type *vi = sidx.fortran_vec ();

  if (mode != UNSORTED)
    lsort.set_compare (mode);
  else
    return m;

  if (stride == 1)
    {
      // Special case for dim 1 avoids gather/scatter for performance boost.
      // See comments in Array::sort (dim, mode).
      for (octave_idx_type j = 0; j < iter; j++)
        {
          // copy and partition out NaNs.
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i];
              if (sort_isnan<T> (tmp))
                {
                  --ku;
                  v[ku] = tmp;
                  vi[ku] = i;
                }
              else
                {
                  v[kl] = tmp;
                  vi[kl] = i;
                  kl++;
                }
            }

          // sort.
          lsort.sort (v, vi, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (v + ku, v + ns);
              std::reverse (vi + ku, vi + ns);
              if (mode == DESCENDING)
                {
                  std::rotate (v, v + ku, v + ns);
                  std::rotate (vi, vi + ku, vi + ns);
                }
            }

          v += ns;
          vi += ns;
          ov += ns;
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (T, buf, ns);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, bufi, ns);

      for (octave_idx_type j = 0; j < iter; j++)
        {
          octave_idx_type offset = j;
          octave_idx_type n_strides = j / stride;
          offset += n_strides * stride * (ns - 1);

          // gather and partition out NaNs.
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i*stride + offset];
              if (sort_isnan<T> (tmp))
                {
                  --ku;
                  buf[ku] = tmp;
                  bufi[ku] = i;
                }
              else
                {
                  buf[kl] = tmp;
                  bufi[kl] = i;
                  kl++;
                }
            }

          // sort.
          lsort.sort (buf, bufi, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (buf + ku, buf + ns);
              std::reverse (bufi + ku, bufi + ns);
              if (mode == DESCENDING)
                {
                  std::rotate (buf, buf + ku, buf + ns);
                  std::rotate (bufi, bufi + ku, bufi + ns);
                }
            }

          // scatter.
          for (octave_idx_type i = 0; i < ns; i++)
            v[i*stride + offset] = buf[i];
          for (octave_idx_type i = 0; i < ns; i++)
            vi[i*stride + offset] = bufi[i];
        }
    }

  return m;
}

template <typename T, typename Alloc>
typename Array<T, Alloc>::compare_fcn_type
safe_comparator (sortmode mode, const Array<T, Alloc>& /* a */,
                 bool /* allow_chk */)
{
  if (mode == ASCENDING)
    return octave_sort<T>::ascending_compare;
  else if (mode == DESCENDING)
    return octave_sort<T>::descending_compare;
  else
    return nullptr;
}

template <typename T, typename Alloc>
sortmode
Array<T, Alloc>::issorted (sortmode mode) const
{
  octave_sort<T> lsort;

  octave_idx_type n = numel ();

  if (n <= 1)
    return (mode == UNSORTED) ? ASCENDING : mode;

  if (mode == UNSORTED)
    {
      // Auto-detect mode.
      compare_fcn_type compare
        = safe_comparator (ASCENDING, *this, false);

      if (compare (elem (n-1), elem (0)))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  lsort.set_compare (safe_comparator (mode, *this, false));

  if (! lsort.issorted (data (), n))
    mode = UNSORTED;

  return mode;

}

template <typename T, typename Alloc>
Array<octave_idx_type>
Array<T, Alloc>::sort_rows_idx (sortmode mode) const
{
  Array<octave_idx_type> idx;

  octave_sort<T> lsort (safe_comparator (mode, *this, true));

  octave_idx_type r = rows ();
  octave_idx_type c = cols ();

  idx = Array<octave_idx_type> (dim_vector (r, 1));

  lsort.sort_rows (data (), idx.fortran_vec (), r, c);

  return idx;
}

template <typename T, typename Alloc>
sortmode
Array<T, Alloc>::is_sorted_rows (sortmode mode) const
{
  octave_sort<T> lsort;

  octave_idx_type r = rows ();
  octave_idx_type c = cols ();

  if (r <= 1 || c == 0)
    return (mode == UNSORTED) ? ASCENDING : mode;

  if (mode == UNSORTED)
    {
      // Auto-detect mode.
      compare_fcn_type compare
        = safe_comparator (ASCENDING, *this, false);

      octave_idx_type i;
      for (i = 0; i < cols (); i++)
        {
          T l = elem (0, i);
          T u = elem (rows () - 1, i);
          if (compare (l, u))
            {
              if (mode == DESCENDING)
                {
                  mode = UNSORTED;
                  break;
                }
              else
                mode = ASCENDING;
            }
          else if (compare (u, l))
            {
              if (mode == ASCENDING)
                {
                  mode = UNSORTED;
                  break;
                }
              else
                mode = DESCENDING;
            }
        }
      if (mode == UNSORTED && i == cols ())
        mode = ASCENDING;
    }

  if (mode != UNSORTED)
    {
      lsort.set_compare (safe_comparator (mode, *this, false));

      if (! lsort.is_sorted_rows (data (), r, c))
        mode = UNSORTED;
    }

  return mode;

}

// Do a binary lookup in a sorted array.
template <typename T, typename Alloc>
octave_idx_type
Array<T, Alloc>::lookup (const T& value, sortmode mode) const
{
  octave_idx_type n = numel ();
  octave_sort<T> lsort;

  if (mode == UNSORTED)
    {
      // auto-detect mode
      if (n > 1 && lsort.descending_compare (elem (0), elem (n-1)))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  lsort.set_compare (mode);

  return lsort.lookup (data (), n, value);
}

template <typename T, typename Alloc>
Array<octave_idx_type>
Array<T, Alloc>::lookup (const Array<T, Alloc>& values, sortmode mode) const
{
  octave_idx_type n = numel ();
  octave_idx_type nval = values.numel ();
  octave_sort<T> lsort;
  Array<octave_idx_type> idx (values.dims ());

  if (mode == UNSORTED)
    {
      // auto-detect mode
      if (n > 1 && lsort.descending_compare (elem (0), elem (n-1)))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  lsort.set_compare (mode);

  // This determines the split ratio between the O(M*log2(N)) and O(M+N)
  // algorithms.
  static const double ratio = 1.0;
  sortmode vmode = UNSORTED;

  // Attempt the O(M+N) algorithm if M is large enough.
  if (nval > ratio * n / octave::math::log2 (n + 1.0))
    {
      vmode = values.issorted ();
      // The table must not contain a NaN.
      if ((vmode == ASCENDING && sort_isnan<T> (values(nval-1)))
          || (vmode == DESCENDING && sort_isnan<T> (values(0))))
        vmode = UNSORTED;
    }

  if (vmode != UNSORTED)
    lsort.lookup_sorted (data (), n, values.data (), nval,
                         idx.fortran_vec (), vmode != mode);
  else
    lsort.lookup (data (), n, values.data (), nval, idx.fortran_vec ());

  return idx;
}

template <typename T, typename Alloc>
octave_idx_type
Array<T, Alloc>::nnz (void) const
{
  const T *src = data ();
  octave_idx_type nel = numel ();
  octave_idx_type retval = 0;
  const T zero = T ();
  for (octave_idx_type i = 0; i < nel; i++)
    if (src[i] != zero)
      retval++;

  return retval;
}

template <typename T, typename Alloc>
Array<octave_idx_type>
Array<T, Alloc>::find (octave_idx_type n, bool backward) const
{
  Array<octave_idx_type> retval;
  const T *src = data ();
  octave_idx_type nel = numel ();
  const T zero = T ();
  if (n < 0 || n >= nel)
    {
      // We want all elements, which means we'll almost surely need
      // to resize.  So count first, then allocate array of exact size.
      octave_idx_type cnt = 0;
      for (octave_idx_type i = 0; i < nel; i++)
        cnt += src[i] != zero;

      retval.clear (cnt, 1);
      octave_idx_type *dest = retval.fortran_vec ();
      for (octave_idx_type i = 0; i < nel; i++)
        if (src[i] != zero) *dest++ = i;
    }
  else
    {
      // We want a fixed max number of elements, usually small.  So be
      // optimistic, alloc the array in advance, and then resize if
      // needed.
      retval.clear (n, 1);
      if (backward)
        {
          // Do the search as a series of successive single-element searches.
          octave_idx_type k = 0;
          octave_idx_type l = nel - 1;
          for (; k < n; k++)
            {
              for (; l >= 0 && src[l] == zero; l--) ;
              if (l >= 0)
                retval(k) = l--;
              else
                break;
            }
          if (k < n)
            retval.resize2 (k, 1);
          octave_idx_type *rdata = retval.fortran_vec ();
          std::reverse (rdata, rdata + k);
        }
      else
        {
          // Do the search as a series of successive single-element searches.
          octave_idx_type k = 0;
          octave_idx_type l = 0;
          for (; k < n; k++)
            {
              for (; l != nel && src[l] == zero; l++) ;
              if (l != nel)
                retval(k) = l++;
              else
                break;
            }
          if (k < n)
            retval.resize2 (k, 1);
        }
    }

  // Fixup return dimensions, for Matlab compatibility.
  // find (zeros (0,0)) -> zeros (0,0)
  // find (zeros (1,0)) -> zeros (1,0)
  // find (zeros (0,1)) -> zeros (0,1)
  // find (zeros (0,X)) -> zeros (0,1)
  // find (zeros (1,1)) -> zeros (0,0) !!!! WHY?
  // find (zeros (0,1,0)) -> zeros (0,0)
  // find (zeros (0,1,0,1)) -> zeros (0,0) etc

  if ((numel () == 1 && retval.isempty ())
      || (rows () == 0 && dims ().numel (1) == 0))
    retval.m_dimensions = dim_vector ();
  else if (rows () == 1 && ndims () == 2)
    retval.m_dimensions = dim_vector (1, retval.numel ());

  return retval;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::nth_element (const octave::idx_vector& n, int dim) const
{
  if (dim < 0)
    (*current_liboctave_error_handler) ("nth_element: invalid dimension");

  dim_vector dv = dims ();
  if (dim >= dv.ndims ())
    dv.resize (dim+1, 1);

  octave_idx_type ns = dv(dim);

  octave_idx_type nn = n.length (ns);

  dv(dim) = std::min (nn, ns);
  dv.chop_trailing_singletons ();
  dim = std::min (dv.ndims (), static_cast<octave_idx_type> (dim));

  Array<T, Alloc> m (dv);

  if (m.isempty ())
    return m;

  sortmode mode = UNSORTED;
  octave_idx_type lo = 0;

  switch (n.idx_class ())
    {
    case octave::idx_vector::class_scalar:
      mode = ASCENDING;
      lo = n(0);
      break;
    case octave::idx_vector::class_range:
      {
        octave_idx_type inc = n.increment ();
        if (inc == 1)
          {
            mode = ASCENDING;
            lo = n(0);
          }
        else if (inc == -1)
          {
            mode = DESCENDING;
            lo = ns - 1 - n(0);
          }
      }
      break;
    case octave::idx_vector::class_vector:
      // This case resolves bug #51329, a fallback to allow the given index
      // to be a sequential vector instead of the typical scalar or range
      if (n(1) - n(0) == 1)
        {
          mode = ASCENDING;
          lo = n(0);
        }
      else if (n(1) - n(0) == -1)
        {
          mode = DESCENDING;
          lo = ns - 1 - n(0);
        }
      // Ensure that the vector is actually an arithmetic contiguous sequence
      for (octave_idx_type i = 2; i < n.length () && mode != UNSORTED; i++)
        if ((mode == ASCENDING && n(i) - n(i-1) != 1)
            || (mode == DESCENDING && n(i) - n(i-1) != -1))
          mode = UNSORTED;
      break;
    default:
      break;
    }

  if (mode == UNSORTED)
    (*current_liboctave_error_handler)
      ("nth_element: n must be a scalar or a contiguous range");

  octave_idx_type up = lo + nn;

  if (lo < 0 || up > ns)
    (*current_liboctave_error_handler) ("nth_element: invalid element index");

  octave_idx_type iter = numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  OCTAVE_LOCAL_BUFFER (T, buf, ns);

  octave_sort<T> lsort;
  lsort.set_compare (mode);

  for (octave_idx_type j = 0; j < iter; j++)
    {
      octave_idx_type kl = 0;
      octave_idx_type ku = ns;

      if (stride == 1)
        {
          // copy without NaNs.
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i];
              if (sort_isnan<T> (tmp))
                buf[--ku] = tmp;
              else
                buf[kl++] = tmp;
            }

          ov += ns;
        }
      else
        {
          octave_idx_type offset = j % stride;
          // copy without NaNs.
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[offset + i*stride];
              if (sort_isnan<T> (tmp))
                buf[--ku] = tmp;
              else
                buf[kl++] = tmp;
            }

          if (offset == stride-1)
            ov += ns*stride;
        }

      if (ku == ns)
        lsort.nth_element (buf, ns, lo, up);
      else if (mode == ASCENDING)
        lsort.nth_element (buf, ku, lo, std::min (ku, up));
      else
        {
          octave_idx_type nnan = ns - ku;
          octave_idx_type zero = 0;
          lsort.nth_element (buf, ku, std::max (lo - nnan, zero),
                             std::max (up - nnan, zero));
          std::rotate (buf, buf + ku, buf + ns);
        }

      if (stride == 1)
        {
          for (octave_idx_type i = 0; i < nn; i++)
            v[i] = buf[lo + i];

          v += nn;
        }
      else
        {
          octave_idx_type offset = j % stride;
          for (octave_idx_type i = 0; i < nn; i++)
            v[offset + stride * i] = buf[lo + i];
          if (offset == stride-1)
            v += nn*stride;
        }
    }

  return m;
}

#define NO_INSTANTIATE_ARRAY_SORT_API(T, API)                           \
  template <> API Array<T>                                              \
  Array<T>::sort (int, sortmode) const                                  \
  {                                                                     \
    return *this;                                                       \
  }                                                                     \
  template <> API Array<T>                                              \
  Array<T>::sort (Array<octave_idx_type> &sidx, int, sortmode) const  \
  {                                                                     \
    sidx = Array<octave_idx_type> ();                                   \
    return *this;                                                       \
  }                                                                     \
  template <> API sortmode                                              \
  Array<T>::issorted (sortmode) const                                   \
  {                                                                     \
    return UNSORTED;                                                    \
  }                                                                     \
  API Array<T>::compare_fcn_type                                        \
  safe_comparator (sortmode, const Array<T>&, bool)                     \
  {                                                                     \
    return nullptr;                                                     \
  }                                                                     \
  template <> API Array<octave_idx_type>                                \
  Array<T>::sort_rows_idx (sortmode) const                              \
  {                                                                     \
    return Array<octave_idx_type> ();                                   \
  }                                                                     \
  template <> API sortmode                                              \
  Array<T>::is_sorted_rows (sortmode) const                             \
  {                                                                     \
    return UNSORTED;                                                    \
  }                                                                     \
  template <> API octave_idx_type                                       \
  Array<T>::lookup (T const &, sortmode) const                          \
  {                                                                     \
    return 0;                                                           \
  }                                                                     \
  template <> API Array<octave_idx_type>                                \
  Array<T>::lookup (const Array<T>&, sortmode) const                    \
  {                                                                     \
    return Array<octave_idx_type> ();                                   \
  }                                                                     \
  template <> API octave_idx_type                                       \
  Array<T>::nnz (void) const                                            \
  {                                                                     \
    return 0;                                                           \
  }                                                                     \
  template <> API Array<octave_idx_type>                                \
  Array<T>::find (octave_idx_type, bool) const                          \
  {                                                                     \
    return Array<octave_idx_type> ();                                   \
  }                                                                     \
  template <> API Array<T>                                              \
  Array<T>::nth_element (const octave::idx_vector&, int) const {        \
    return Array<T> ();                                                 \
  }

#define NO_INSTANTIATE_ARRAY_SORT(T) NO_INSTANTIATE_ARRAY_SORT_API (T,)

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::diag (octave_idx_type k) const
{
  dim_vector dv = dims ();
  octave_idx_type nd = dv.ndims ();
  Array<T, Alloc> d;

  if (nd > 2)
    (*current_liboctave_error_handler) ("Matrix must be 2-dimensional");

  octave_idx_type nnr = dv(0);
  octave_idx_type nnc = dv(1);

  if (nnr == 0 && nnc == 0)
    ; // do nothing for empty matrix
  else if (nnr != 1 && nnc != 1)
    {
      // Extract diag from matrix
      if (k > 0)
        nnc -= k;
      else if (k < 0)
        nnr += k;

      if (nnr > 0 && nnc > 0)
        {
          octave_idx_type ndiag = (nnr < nnc) ? nnr : nnc;

          d.resize (dim_vector (ndiag, 1));

          if (k > 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                d.xelem (i) = elem (i, i+k);
            }
          else if (k < 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                d.xelem (i) = elem (i-k, i);
            }
          else
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                d.xelem (i) = elem (i, i);
            }
        }
      else  // Matlab returns [] 0x1 for out-of-range diagonal
        d.resize (dim_vector (0, 1));
    }
  else
    {
      // Create diag matrix from vector
      octave_idx_type roff = 0;
      octave_idx_type coff = 0;
      if (k > 0)
        {
          roff = 0;
          coff = k;
        }
      else if (k < 0)
        {
          roff = -k;
          coff = 0;
        }

      if (nnr == 1)
        {
          octave_idx_type n = nnc + std::abs (k);
          d = Array<T, Alloc> (dim_vector (n, n), resize_fill_value ());

          for (octave_idx_type i = 0; i < nnc; i++)
            d.xelem (i+roff, i+coff) = elem (0, i);
        }
      else
        {
          octave_idx_type n = nnr + std::abs (k);
          d = Array<T, Alloc> (dim_vector (n, n), resize_fill_value ());

          for (octave_idx_type i = 0; i < nnr; i++)
            d.xelem (i+roff, i+coff) = elem (i, 0);
        }
    }

  return d;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::diag (octave_idx_type m, octave_idx_type n) const
{
  if (ndims () != 2 || (rows () != 1 && cols () != 1))
    (*current_liboctave_error_handler) ("cat: invalid dimension");

  Array<T, Alloc> retval (dim_vector (m, n), resize_fill_value ());

  octave_idx_type nel = std::min (numel (), std::min (m, n));
  for (octave_idx_type i = 0; i < nel; i++)
    retval.xelem (i, i) = xelem (i);

  return retval;
}

template <typename T, typename Alloc>
Array<T, Alloc>
Array<T, Alloc>::cat (int dim, octave_idx_type n, const Array<T, Alloc> *array_list)
{
  // Default concatenation.
  bool (dim_vector::*concat_rule) (const dim_vector&, int) = &dim_vector::concat;

  if (dim == -1 || dim == -2)
    {
      concat_rule = &dim_vector::hvcat;
      dim = -dim - 1;
    }
  else if (dim < 0)
    (*current_liboctave_error_handler) ("cat: invalid dimension");

  if (n == 1)
    return array_list[0];
  else if (n == 0)
    return Array<T, Alloc> ();

  // Special case:
  //
  //   cat (dim, [], ..., [], A, ...)
  //
  // with dim > 2, A not 0x0, and at least three arguments to
  // concatenate is equivalent to
  //
  //   cat (dim, A, ...)
  //
  // Note that this check must be performed here because for full-on
  // braindead Matlab compatibility, we need to have things like
  //
  //   cat (3, [], [], A)
  //
  // succeed, but to have things like
  //
  //   cat (3, cat (3, [], []), A)
  //   cat (3, zeros (0, 0, 2), A)
  //
  // fail.  See also bug report #31615.

  octave_idx_type istart = 0;

  if (n > 2 && dim > 1)
    {
      for (octave_idx_type i = 0; i < n; i++)
        {
          dim_vector dv = array_list[i].dims ();

          if (dv.zero_by_zero ())
            istart++;
          else
            break;
        }

      // Don't skip any initial arguments if they are all empty.
      if (istart >= n)
        istart = 0;
    }

  dim_vector dv = array_list[istart++].dims ();

  for (octave_idx_type i = istart; i < n; i++)
    if (! (dv.*concat_rule) (array_list[i].dims (), dim))
      (*current_liboctave_error_handler) ("cat: dimension mismatch");

  Array<T, Alloc> retval (dv);

  if (retval.isempty ())
    return retval;

  int nidx = std::max (dv.ndims (), static_cast<octave_idx_type> (dim + 1));
  Array<octave::idx_vector> idxa (dim_vector (nidx, 1), octave::idx_vector::colon);
  octave_idx_type l = 0;

  for (octave_idx_type i = 0; i < n; i++)
    {
      // NOTE: This takes some thinking, but no matter what the above rules
      // are, an empty array can always be skipped at this point, because
      // the result dimensions are already determined, and there is no way
      // an empty array may contribute a nonzero piece along the dimension
      // at this point, unless an empty array can be promoted to a non-empty
      // one (which makes no sense).  I repeat, *no way*, think about it.
      if (array_list[i].isempty ())
        continue;

      octave_quit ();

      octave_idx_type u;
      if (dim < array_list[i].ndims ())
        u = l + array_list[i].dims ()(dim);
      else
        u = l + 1;

      idxa(dim) = octave::idx_vector (l, u);

      retval.assign (idxa, array_list[i]);

      l = u;
    }

  return retval;
}

template <typename T, typename Alloc>
void
Array<T, Alloc>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "m_rep address:   " << m_rep << '\n'
     << prefix << "m_rep->m_len:    " << m_rep->m_len << '\n'
     << prefix << "m_rep->m_data:   " << static_cast<void *> (m_rep->m_data) << '\n'
     << prefix << "m_rep->m_count:  " << m_rep->m_count << '\n'
     << prefix << "m_slice_data:    " << static_cast<void *> (m_slice_data) << '\n'
     << prefix << "m_slice_len:     " << m_slice_len << '\n';

  // 2D info:
  //
  //     << pefix << "rows: " << rows () << "\n"
  //     << prefix << "cols: " << cols () << "\n";
}

template <typename T, typename Alloc>
bool Array<T, Alloc>::optimize_dimensions (const dim_vector& dv)
{
  bool retval = m_dimensions == dv;
  if (retval)
    m_dimensions = dv;

  return retval;
}

template <typename T, typename Alloc>
void Array<T, Alloc>::instantiation_guard ()
{
  // This guards against accidental implicit instantiations.
  // Array<T, Alloc> instances should always be explicit and use INSTANTIATE_ARRAY.
  T::__xXxXx__ ();
}

#define INSTANTIATE_ARRAY(T, API)               \
  template <> API void                          \
  Array<T>::instantiation_guard () { }          \
                                                \
  template class API Array<T>

// FIXME: is this used?

template <typename T, typename Alloc>
std::ostream&
operator << (std::ostream& os, const Array<T, Alloc>& a)
{
  dim_vector a_dims = a.dims ();

  int n_dims = a_dims.ndims ();

  os << n_dims << "-dimensional array";

  if (n_dims)
    os << " (" << a_dims.str () << ')';

  os <<"\n\n";

  if (n_dims)
    {
      os << "data:";

      Array<octave_idx_type> ra_idx (dim_vector (n_dims, 1), 0);

      // Number of times the first 2d-array is to be displayed.

      octave_idx_type m = 1;
      for (int i = 2; i < n_dims; i++)
        m *= a_dims(i);

      if (m == 1)
        {
          octave_idx_type rows = 0;
          octave_idx_type cols = 0;

          switch (n_dims)
            {
            case 2:
              rows = a_dims(0);
              cols = a_dims(1);

              for (octave_idx_type j = 0; j < rows; j++)
                {
                  ra_idx(0) = j;
                  for (octave_idx_type k = 0; k < cols; k++)
                    {
                      ra_idx(1) = k;
                      os << ' ' << a.elem (ra_idx);
                    }
                  os << "\n";
                }
              break;

            default:
              rows = a_dims(0);

              for (octave_idx_type k = 0; k < rows; k++)
                {
                  ra_idx(0) = k;
                  os << ' ' << a.elem (ra_idx);
                }
              break;
            }

          os << "\n";
        }
      else
        {
          octave_idx_type rows = a_dims(0);
          octave_idx_type cols = a_dims(1);

          for (int i = 0; i < m; i++)
            {
              os << "\n(:,:,";

              for (int j = 2; j < n_dims - 1; j++)
                os << ra_idx(j) + 1 << ',';

              os << ra_idx(n_dims - 1) + 1 << ") = \n";

              for (octave_idx_type j = 0; j < rows; j++)
                {
                  ra_idx(0) = j;

                  for (octave_idx_type k = 0; k < cols; k++)
                    {
                      ra_idx(1) = k;
                      os << ' ' << a.elem (ra_idx);
                    }

                  os << "\n";
                }

              os << "\n";

              if (i != m - 1)
                increment_index (ra_idx, a_dims, 2);
            }
        }
    }

  return os;
}
