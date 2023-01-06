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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cinttypes>
#include <cstdlib>

#include <ostream>

#include "idx-vector.h"
#include "Array.h"
#include "Array-util.h"
#include "Sparse.h"
#include "Range.h"

#include "oct-locbuf.h"
#include "lo-error.h"
#include "lo-mappers.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_NORETURN static void err_invalid_range (void)
{
  (*current_liboctave_error_handler) ("invalid range used as index");
}

OCTAVE_NORETURN static void err_index_out_of_range (void)
{
  (*current_liboctave_error_handler)
    ("internal error: idx_vector index out of range");
}

idx_vector::idx_vector_rep *idx_vector::nil_rep (void)
{
  static idx_vector_rep ivr;
  return &ivr;
}

Array<octave_idx_type> idx_vector::idx_base_rep::as_array (void)
{
  (*current_liboctave_error_handler)
    ("internal error: as_array not allowed for this index class");

  // Never actually executed, but required to silence compiler warning
  return Array<octave_idx_type> ();
}

idx_vector::idx_colon_rep::idx_colon_rep (char c)
  : idx_base_rep ()
{
  if (c != ':')
    (*current_liboctave_error_handler)
      ("internal error: invalid character converted to idx_vector; must be ':'");
}

octave_idx_type
idx_vector::idx_colon_rep::checkelem (octave_idx_type i) const
{
  if (i < 0)
    err_index_out_of_range ();

  return i;
}

idx_vector::idx_base_rep *
idx_vector::idx_colon_rep::sort_idx (Array<octave_idx_type>&)
{
  (*current_liboctave_error_handler)
    ("internal error: idx_colon_rep::sort_idx");
}

std::ostream& idx_vector::idx_colon_rep::print (std::ostream& os) const
{
  return os << ':';
}

idx_vector::idx_range_rep::idx_range_rep (octave_idx_type start,
    octave_idx_type limit,
    octave_idx_type step)
  : idx_base_rep (), m_start(start),
    // Round length away from zero to catch incomplete intervals
    m_len (step
           ? std::max ((limit - start + step - (step > 0 ? 1 : -1)) / step,
                       static_cast<octave_idx_type> (0))
           : -1),
    m_step (step)
{
  if (m_len < 0)
    err_invalid_range ();
  if (m_start < 0)
    err_invalid_index (m_start);
  if (m_step < 0 && m_start + (m_len-1)*m_step < 0)
    err_invalid_index (m_start + (m_len-1)*m_step);
}

idx_vector::idx_range_rep::idx_range_rep (const range<double>& r)
  : idx_base_rep (), m_start (0), m_len (r.numel ()), m_step (1)
{
  if (m_len < 0)
    err_invalid_range ();

  if (m_len > 0)
    {
      if (r.all_elements_are_ints ())
        {
          m_start = static_cast<octave_idx_type> (r.base ()) - 1;
          m_step = static_cast<octave_idx_type> (r.increment ());
          if (m_start < 0)
            err_invalid_index (m_start);
          if (m_step < 0 && m_start + (m_len - 1)*m_step < 0)
            err_invalid_index (m_start + (m_len - 1)*m_step);
        }
      else
        {
          // find first non-integer, then gripe about it
          double b = r.base ();
          double inc = r.increment ();
          err_invalid_index (b != std::trunc (b) ? b : b + inc);
        }
    }
}

octave_idx_type
idx_vector::idx_range_rep::checkelem (octave_idx_type i) const
{
  if (i < 0 || i >= m_len)
    err_index_out_of_range ();

  return m_start + i*m_step;
}

idx_vector::idx_base_rep *idx_vector::idx_range_rep::sort_uniq_clone (bool)
{
  if (m_step < 0)
    return new idx_range_rep (m_start + (m_len - 1)*m_step, m_len, -m_step, DIRECT);
  else
    {
      m_count++;
      return this;
    }
}

idx_vector::idx_base_rep *
idx_vector::idx_range_rep::sort_idx (Array<octave_idx_type>& idx)
{
  if (m_step < 0 && m_len > 0)
    {
      idx.clear (1, m_len);
      for (octave_idx_type i = 0; i < m_len; i++)
        idx.xelem (i) = m_len - 1 - i;
      return new idx_range_rep (m_start + (m_len - 1)*m_step, m_len, -m_step, DIRECT);
    }
  else
    {
      idx.clear (1, m_len);
      for (octave_idx_type i = 0; i < m_len; i++)
        idx.xelem (i) = i;
      m_count++;
      return this;
    }
}

std::ostream& idx_vector::idx_range_rep::print (std::ostream& os) const
{
  os << m_start << ':' << m_step << ':' << m_start + m_len *m_step;
  return os;
}

range<double> idx_vector::idx_range_rep::unconvert (void) const
{
  return range<double>::make_n_element_range
         (static_cast<double> (m_start+1), static_cast<double> (m_step), m_len);
}

Array<octave_idx_type> idx_vector::idx_range_rep::as_array (void)
{
  Array<octave_idx_type> retval (dim_vector (1, m_len));
  for (octave_idx_type i = 0; i < m_len; i++)
    retval.xelem (i) = m_start + i*m_step;

  return retval;
}

inline octave_idx_type convert_index (octave_idx_type i, octave_idx_type& ext)
{
  if (i <= 0)
    err_invalid_index (i-1);

  if (ext < i)
    ext = i;

  return i - 1;
}

inline octave_idx_type convert_index (double x, octave_idx_type& ext)
{
  octave_idx_type i = static_cast<octave_idx_type> (x);

  if (static_cast<double> (i) != x)
    err_invalid_index (x-1);

  return convert_index (i, ext);
}

inline octave_idx_type convert_index (float x, octave_idx_type& ext)
{
  return convert_index (static_cast<double> (x), ext);
}

template <typename T>
inline octave_idx_type convert_index (octave_int<T> x, octave_idx_type& ext)
{
  octave_idx_type i = octave_int<octave_idx_type> (x).value ();

  return convert_index (i, ext);
}

template <typename T>
idx_vector::idx_scalar_rep::idx_scalar_rep (T x)
  : idx_base_rep (), m_data (0)
{
  octave_idx_type dummy = 0;

  m_data = convert_index (x, dummy);
}

idx_vector::idx_scalar_rep::idx_scalar_rep (octave_idx_type i)
  : idx_base_rep (), m_data (i)
{
  if (m_data < 0)
    err_invalid_index (m_data);
}

octave_idx_type
idx_vector::idx_scalar_rep::checkelem (octave_idx_type i) const
{
  if (i != 0)
    err_index_out_of_range ();

  return m_data;
}

idx_vector::idx_base_rep *
idx_vector::idx_scalar_rep::sort_idx (Array<octave_idx_type>& idx)
{
  idx.clear (1, 1);
  idx.fill (0);
  m_count++;
  return this;
}

std::ostream& idx_vector::idx_scalar_rep::print (std::ostream& os) const
{
  return os << m_data;
}

double idx_vector::idx_scalar_rep::unconvert (void) const
{
  return m_data + 1;
}

Array<octave_idx_type> idx_vector::idx_scalar_rep::as_array (void)
{
  return Array<octave_idx_type> (dim_vector (1, 1), m_data);
}

template <typename T>
idx_vector::idx_vector_rep::idx_vector_rep (const Array<T>& nda)
  : idx_base_rep (), m_data (nullptr), m_len (nda.numel ()), m_ext (0),
    m_aowner (nullptr), m_orig_dims (nda.dims ())
{
  if (m_len != 0)
    {
      std::unique_ptr<octave_idx_type []> d (new octave_idx_type [m_len]);

      for (octave_idx_type i = 0; i < m_len; i++)
        d[i] = convert_index (nda.xelem (i), m_ext);

      m_data = d.release ();
    }
}

// Note that this makes a shallow copy of the index array.

idx_vector::idx_vector_rep::idx_vector_rep (const Array<octave_idx_type>& inda)
  : idx_base_rep (), m_data (inda.data ()), m_len (inda.numel ()), m_ext (0),
    m_aowner (new Array<octave_idx_type> (inda)), m_orig_dims (inda.dims ())
{
  if (m_len != 0)
    {
      octave_idx_type max = -1;
      for (octave_idx_type i = 0; i < m_len; i++)
        {
          octave_idx_type k = inda.xelem (i);
          if (k < 0)
            err_invalid_index (k);
          else if (k > max)
            max = k;
        }

      m_ext = max + 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (const Array<octave_idx_type>& inda,
    octave_idx_type ext, direct)
  : idx_base_rep (), m_data (inda.data ()), m_len (inda.numel ()),
    m_ext (ext), m_aowner (new Array<octave_idx_type> (inda)),
    m_orig_dims (inda.dims ())
{
  // No checking.
  if (m_ext < 0)
    {
      octave_idx_type max = -1;
      for (octave_idx_type i = 0; i < m_len; i++)
        if (m_data[i] > max)
          max = m_data[i];

      m_ext = max + 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (bool b)
  : idx_base_rep (), m_data (nullptr), m_len (b ? 1 : 0), m_ext (0),
    m_aowner (nullptr), m_orig_dims (m_len, m_len)
{
  if (m_len != 0)
    {
      octave_idx_type *d = new octave_idx_type [1];
      d[0] = 0;
      m_data = d;
      m_ext = 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (const Array<bool>& bnda,
    octave_idx_type nnz)
  : idx_base_rep (), m_data (nullptr), m_len (nnz), m_ext (0),
    m_aowner (nullptr), m_orig_dims ()
{
  if (nnz < 0)
    m_len = bnda.nnz ();

  const dim_vector dv = bnda.dims ();

  m_orig_dims = dv.make_nd_vector (m_len);

  if (m_len != 0)
    {
      octave_idx_type *d = new octave_idx_type [m_len];

      octave_idx_type ntot = bnda.numel ();

      octave_idx_type k = 0;
      for (octave_idx_type i = 0; i < ntot; i++)
        if (bnda.xelem (i))
          d[k++] = i;

      m_data = d;

      m_ext = d[k-1] + 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (const Sparse<bool>& bnda)
  : idx_base_rep (), m_data (nullptr), m_len (bnda.nnz ()), m_ext (0),
    m_aowner (nullptr), m_orig_dims ()
{
  const dim_vector dv = bnda.dims ();

  m_orig_dims = dv.make_nd_vector (m_len);

  if (m_len != 0)
    {
      octave_idx_type *d = new octave_idx_type [m_len];

      octave_idx_type k = 0;
      octave_idx_type nc = bnda.cols ();
      octave_idx_type nr = bnda.rows ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = bnda.cidx (j); i < bnda.cidx (j+1); i++)
          if (bnda.data (i))
            d[k++] = j * nr + bnda.ridx (i);

      m_data = d;

      m_ext = d[k-1] + 1;
    }
}

idx_vector::idx_vector_rep::~idx_vector_rep (void)
{
  if (m_aowner)
    delete m_aowner;
  else
    delete [] m_data;
}

octave_idx_type
idx_vector::idx_vector_rep::checkelem (octave_idx_type n) const
{
  if (n < 0 || n >= m_len)
    err_invalid_index (n);

  return xelem (n);
}

idx_vector::idx_base_rep *
idx_vector::idx_vector_rep::sort_uniq_clone (bool uniq)
{
  if (m_len == 0)
    {
      m_count++;
      return this;
    }

  // This is wrapped in unique_ptr so that we don't leak on out-of-memory.
  std::unique_ptr<idx_vector_rep> new_rep
  (new idx_vector_rep (nullptr, m_len, m_ext, m_orig_dims, DIRECT));

  if (m_ext > m_len*math::log2 (1.0 + m_len))
    {
      // Use standard sort via octave_sort.
      octave_idx_type *new_data = new octave_idx_type [m_len];
      new_rep->m_data = new_data;

      std::copy_n (m_data, m_len, new_data);
      octave_sort<octave_idx_type> lsort;
      lsort.set_compare (ASCENDING);
      lsort.sort (new_data, m_len);

      if (uniq)
        {
          octave_idx_type new_len = std::unique (new_data, new_data + m_len)
                                    - new_data;
          new_rep->m_len = new_len;
          if (new_rep->m_orig_dims.ndims () == 2 && new_rep->m_orig_dims(0) == 1)
            new_rep->m_orig_dims = dim_vector (1, new_len);
          else
            new_rep->m_orig_dims = dim_vector (new_len, 1);
        }
    }
  else if (uniq)
    {
      // Use two-pass bucket sort (only a mask array needed).
      OCTAVE_LOCAL_BUFFER_INIT (bool, has, m_ext, false);
      for (octave_idx_type i = 0; i < m_len; i++)
        has[m_data[i]] = true;

      octave_idx_type new_len = 0;
      for (octave_idx_type i = 0; i < m_ext; i++)
        new_len += has[i];

      new_rep->m_len = new_len;
      if (new_rep->m_orig_dims.ndims () == 2 && new_rep->m_orig_dims(0) == 1)
        new_rep->m_orig_dims = dim_vector (1, new_len);
      else
        new_rep->m_orig_dims = dim_vector (new_len, 1);

      octave_idx_type *new_data = new octave_idx_type [new_len];
      new_rep->m_data = new_data;

      for (octave_idx_type i = 0, j = 0; i < m_ext; i++)
        if (has[i])
          new_data[j++] = i;
    }
  else
    {
      // Use two-pass bucket sort.
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, cnt, m_ext, 0);
      for (octave_idx_type i = 0; i < m_len; i++)
        cnt[m_data[i]]++;

      octave_idx_type *new_data = new octave_idx_type [m_len];
      new_rep->m_data = new_data;

      for (octave_idx_type i = 0, j = 0; i < m_ext; i++)
        {
          for (octave_idx_type k = 0; k < cnt[i]; k++)
            new_data[j++] = i;
        }
    }

  return new_rep.release ();
}

idx_vector::idx_base_rep *
idx_vector::idx_vector_rep::sort_idx (Array<octave_idx_type>& idx)
{
  // This is wrapped in unique_ptr so that we don't leak on out-of-memory.
  std::unique_ptr<idx_vector_rep> new_rep
  (new idx_vector_rep (nullptr, m_len, m_ext, m_orig_dims, DIRECT));

  if (m_ext > m_len*math::log2 (1.0 + m_len))
    {
      // Use standard sort via octave_sort.
      idx.clear (m_orig_dims);
      octave_idx_type *idx_data = idx.fortran_vec ();
      for (octave_idx_type i = 0; i < m_len; i++)
        idx_data[i] = i;

      octave_idx_type *new_data = new octave_idx_type [m_len];
      new_rep->m_data = new_data;
      std::copy_n (m_data, m_len, new_data);

      octave_sort<octave_idx_type> lsort;
      lsort.set_compare (ASCENDING);
      lsort.sort (new_data, idx_data, m_len);
    }
  else
    {
      // Use two-pass bucket sort.
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, cnt, m_ext, 0);

      for (octave_idx_type i = 0; i < m_len; i++)
        cnt[m_data[i]]++;

      idx.clear (m_orig_dims);
      octave_idx_type *idx_data = idx.fortran_vec ();

      octave_idx_type *new_data = new octave_idx_type [m_len];
      new_rep->m_data = new_data;

      for (octave_idx_type i = 0, k = 0; i < m_ext; i++)
        {
          octave_idx_type j = cnt[i];
          cnt[i] = k;
          k += j;
        }

      for (octave_idx_type i = 0; i < m_len; i++)
        {
          octave_idx_type j = m_data[i];
          octave_idx_type k = cnt[j]++;
          new_data[k] = j;
          idx_data[k] = i;
        }
    }

  return new_rep.release ();
}

std::ostream& idx_vector::idx_vector_rep::print (std::ostream& os) const
{
  os << '[';

  for (octave_idx_type i = 0; i < m_len - 1; i++)
    os << m_data[i] << ',' << ' ';

  if (m_len > 0)
    os << m_data[m_len-1];

  os << ']';

  return os;
}

Array<double> idx_vector::idx_vector_rep::unconvert (void) const
{
  Array<double> retval (m_orig_dims);
  for (octave_idx_type i = 0; i < m_len; i++)
    retval.xelem (i) = m_data[i] + 1;
  return retval;
}

Array<octave_idx_type> idx_vector::idx_vector_rep::as_array (void)
{
  if (m_aowner)
    return *m_aowner;
  else
    {
      Array<octave_idx_type> retval (m_orig_dims);

      if (m_data)
        {
          std::memcpy (retval.fortran_vec (), m_data, m_len* sizeof (octave_idx_type));
          // Delete the old copy and share the m_data instead to save memory.
          delete [] m_data;
        }

      m_data = retval.fortran_vec ();
      m_aowner = new Array<octave_idx_type> (retval);

      return retval;
    }
}

idx_vector::idx_mask_rep::idx_mask_rep (bool b)
  : idx_base_rep (), m_data (nullptr), m_len (b ? 1 : 0), m_ext (0),
    m_lsti (-1), m_lste (-1), m_aowner (nullptr), m_orig_dims (m_len, m_len)
{
  if (m_len != 0)
    {
      bool *d = new bool [1];
      d[0] = true;
      m_data = d;
      m_ext = 1;
    }
}

idx_vector::idx_mask_rep::idx_mask_rep (const Array<bool>& bnda,
                                        octave_idx_type nnz)
  : idx_base_rep (), m_data (nullptr), m_len (nnz), m_ext (bnda.numel ()),
    m_lsti (-1), m_lste (-1), m_aowner (nullptr), m_orig_dims ()
{
  if (nnz < 0)
    m_len = bnda.nnz ();

  // We truncate the extent as much as possible.  For Matlab
  // compatibility, but maybe it's not a bad idea anyway.
  while (m_ext > 0 && ! bnda(m_ext-1))
    m_ext--;

  const dim_vector dv = bnda.dims ();

  m_orig_dims = dv.make_nd_vector (m_len);

  m_aowner = new Array<bool> (bnda);
  m_data = bnda.data ();
}

idx_vector::idx_mask_rep::~idx_mask_rep (void)
{
  if (m_aowner)
    delete m_aowner;
  else
    delete [] m_data;
}

octave_idx_type idx_vector::idx_mask_rep::xelem (octave_idx_type n) const
{
  if (n == m_lsti + 1)
    {
      m_lsti = n;
      while (! m_data[++m_lste]) ;
    }
  else
    {
      m_lsti = n++;
      m_lste = -1;
      while (n > 0)
        if (m_data[++m_lste]) --n;
    }
  return m_lste;
}

octave_idx_type idx_vector::idx_mask_rep::checkelem (octave_idx_type n) const
{
  if (n < 0 || n >= m_len)
    err_invalid_index (n);

  return xelem (n);
}

std::ostream& idx_vector::idx_mask_rep::print (std::ostream& os) const
{
  os << '[';

  for (octave_idx_type i = 0; i < m_ext - 1; i++)
    os << m_data[i] << ',' << ' ';

  if (m_ext > 0)
    os << m_data[m_ext-1];

  os << ']';

  return os;
}

Array<bool> idx_vector::idx_mask_rep::unconvert (void) const
{
  if (m_aowner)
    return *m_aowner;
  else
    {
      Array<bool> retval (dim_vector (m_ext, 1));
      for (octave_idx_type i = 0; i < m_ext; i++)
        retval.xelem (i) = m_data[i];
      return retval;
    }
}

Array<octave_idx_type> idx_vector::idx_mask_rep::as_array (void)
{
  if (m_aowner)
    return m_aowner->find ().reshape (m_orig_dims);
  else
    {
      Array<bool> retval (m_orig_dims);
      for (octave_idx_type i = 0, j = 0; i < m_ext; i++)
        if (m_data[i])
          retval.xelem (j++) = i;

      return retval;
    }
}

idx_vector::idx_base_rep *
idx_vector::idx_mask_rep::sort_idx (Array<octave_idx_type>& idx)
{
  idx.clear (m_len, 1);
  for (octave_idx_type i = 0; i < m_len; i++)
    idx.xelem (i) = i;

  m_count++;
  return this;
}

const idx_vector idx_vector::colon (new idx_vector::idx_colon_rep ());

idx_vector::idx_vector (const Array<bool>& bnda)
  : m_rep (nullptr)
{
  // Convert only if it means saving at least half the memory.
  static const int factor = (2 * sizeof (octave_idx_type));
  octave_idx_type nnz = bnda.nnz ();
  if (nnz <= bnda.numel () / factor)
    m_rep = new idx_vector_rep (bnda, nnz);
  else
    m_rep = new idx_mask_rep (bnda, nnz);
}

bool idx_vector::maybe_reduce (octave_idx_type n, const idx_vector& j,
                               octave_idx_type nj)
{
  bool reduced = false;

  // Empty index always reduces.
  if (m_rep->length (n) == 0)
    {
      *this = idx_vector ();
      return true;
    }

  // Possibly skip singleton dims.
  if (n == 1 && m_rep->is_colon_equiv (n))
    {
      *this = j;
      return true;
    }

  if (nj == 1 && j.is_colon_equiv (nj))
    return true;

  switch (j.idx_class ())
    {
    case class_colon:
      switch (m_rep->idx_class ())
        {
        case class_colon:
          // (:,:) reduces to (:)
          reduced = true;
          break;

        case class_scalar:
          {
            // (i,:) reduces to a range.
            idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
            octave_idx_type k = r->get_data ();
            *this = new idx_range_rep (k, nj, n, DIRECT);
            reduced = true;
          }
          break;

        case class_range:
          {
            // (i:k:end,:) reduces to a range if i <= k and k divides n.
            idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
            octave_idx_type s = r->get_start ();
            octave_idx_type l = r->length (n);
            octave_idx_type t = r->get_step ();
            if (l*t == n)
              {
                *this = new idx_range_rep (s, l * nj, t, DIRECT);
                reduced = true;
              }
          }
          break;

        default:
          break;
        }
      break;

    case class_range:
      switch (m_rep->idx_class ())
        {
        case class_colon:
          {
            // (:,i:j) reduces to a range (the m_step must be 1)
            idx_range_rep *rj = dynamic_cast<idx_range_rep *> (j.m_rep);
            if (rj->get_step () == 1)
              {
                octave_idx_type sj = rj->get_start ();
                octave_idx_type lj = rj->length (nj);
                *this = new idx_range_rep (sj * n, lj * n, 1, DIRECT);
                reduced = true;
              }
          }
          break;

        case class_scalar:
          {
            // (k,i:d:j) reduces to a range.
            idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
            idx_range_rep *rj = dynamic_cast<idx_range_rep *> (j.m_rep);
            octave_idx_type k = r->get_data ();
            octave_idx_type sj = rj->get_start ();
            octave_idx_type lj = rj->length (nj);
            octave_idx_type tj = rj->get_step ();
            *this = new idx_range_rep (n * sj + k, lj, n * tj, DIRECT);
            reduced = true;
          }
          break;

        case class_range:
          {
            // (i:k:end,p:q) reduces to a range if i <= k and k divides n.
            // (ones (1, m), ones (1, n)) reduces to (ones (1, m*n))
            idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
            octave_idx_type s = r->get_start ();
            octave_idx_type l = r->length (n);
            octave_idx_type t = r->get_step ();
            idx_range_rep *rj = dynamic_cast<idx_range_rep *> (j.m_rep);
            octave_idx_type sj = rj->get_start ();
            octave_idx_type lj = rj->length (nj);
            octave_idx_type tj = rj->get_step ();
            if ((l*t == n && tj == 1) || (t == 0 && tj == 0))
              {
                *this = new idx_range_rep (s + n * sj, l * lj, t, DIRECT);
                reduced = true;
              }
          }
          break;

        default:
          break;
        }
      break;

    case class_scalar:
      switch (m_rep->idx_class ())
        {
        case class_scalar:
          {
            // (i,j) reduces to a single index.
            idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
            idx_scalar_rep *rj = dynamic_cast<idx_scalar_rep *> (j.m_rep);
            octave_idx_type k = r->get_data () + n * rj->get_data ();
            *this = new idx_scalar_rep (k, DIRECT);
            reduced = true;
          }
          break;

        case class_range:
          {
            // (i:d:j,k) reduces to a range.
            idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
            idx_scalar_rep *rj = dynamic_cast<idx_scalar_rep *> (j.m_rep);
            octave_idx_type s = r->get_start ();
            octave_idx_type l = r->length (nj);
            octave_idx_type t = r->get_step ();
            octave_idx_type k = rj->get_data ();
            *this = new idx_range_rep (n * k + s, l, t, DIRECT);
            reduced = true;
          }
          break;

        case class_colon:
          {
            // (:,k) reduces to a range.
            idx_scalar_rep *rj = dynamic_cast<idx_scalar_rep *> (j.m_rep);
            octave_idx_type k = rj->get_data ();
            *this = new idx_range_rep (n * k, n, 1, DIRECT);
            reduced = true;
          }
          break;

        default:
          break;
        }
      break;

    default:
      break;
    }

  return reduced;
}

bool idx_vector::is_cont_range (octave_idx_type n, octave_idx_type& l,
                                octave_idx_type& u) const
{
  bool res = false;

  switch (m_rep->idx_class ())
    {
    case class_colon:
      l = 0; u = n;
      res = true;
      break;

    case class_range:
      {
        idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
        if (r->get_step () == 1)
          {
            l = r->get_start ();
            u = l + r->length (n);
            res = true;
          }
      }
      break;

    case class_scalar:
      {
        idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
        l = r->get_data ();
        u = l + 1;
        res = true;
      }
      break;

    case class_mask:
      {
        idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
        octave_idx_type m_ext = r->extent (0);
        octave_idx_type m_len = r->length (0);
        if (m_ext == m_len)
          {
            l = 0;
            u = m_len;
            res = true;
          }
      }

    default:
      break;
    }

  return res;
}

octave_idx_type idx_vector::increment (void) const
{
  octave_idx_type retval = 0;

  switch (m_rep->idx_class ())
    {
    case class_colon:
      retval = 1;
      break;

    case class_range:
      retval = dynamic_cast<idx_range_rep *> (m_rep) -> get_step ();
      break;

    case class_vector:
    case class_mask:
      {
        if (length (0) > 1)
          retval = elem (1) - elem (0);
      }
      break;

    default:
      break;
    }

  return retval;
}

const octave_idx_type *idx_vector::raw (void)
{
  if (m_rep->idx_class () != class_vector)
    *this = idx_vector (as_array (), extent (0));

  idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);

  assert (r != nullptr);

  return r->get_data ();
}

void idx_vector::copy_data (octave_idx_type *m_data) const
{
  octave_idx_type m_len = m_rep->length (0);

  switch (m_rep->idx_class ())
    {
    case class_colon:
      (*current_liboctave_error_handler) ("colon not allowed");
      break;

    case class_range:
      {
        idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
        octave_idx_type m_start = r->get_start ();
        octave_idx_type m_step = r->get_step ();
        octave_idx_type i, j;
        if (m_step == 1)
          for (i = m_start, j = m_start + m_len; i < j; i++) *m_data++ = i;
        else if (m_step == -1)
          for (i = m_start, j = m_start - m_len; i > j; i--) *m_data++ = i;
        else
          for (i = 0, j = m_start; i < m_len; i++, j += m_step) *m_data++ = j;
      }
      break;

    case class_scalar:
      {
        idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
        *m_data = r->get_data ();
      }
      break;

    case class_vector:
      {
        idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
        const octave_idx_type *rdata = r->get_data ();
        std::copy_n (rdata, m_len, m_data);
      }
      break;

    case class_mask:
      {
        idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
        const bool *mask = r->get_data ();
        octave_idx_type m_ext = r->extent (0);
        for (octave_idx_type i = 0, j = 0; i < m_ext; i++)
          if (mask[i])
            m_data[j++] = i;
      }
      break;

    default:
      assert (false);
      break;
    }
}

idx_vector idx_vector::complement (octave_idx_type n) const
{
  idx_vector retval;
  if (extent (n) > n)
    (*current_liboctave_error_handler)
      ("internal error: out of range complement index requested");

  if (idx_class () == class_mask)
    {
      idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
      octave_idx_type nz = r->length (0);
      octave_idx_type m_ext = r->extent (0);
      Array<bool> mask (dim_vector (n, 1));
      const bool *m_data = r->get_data ();
      bool *ndata = mask.fortran_vec ();
      for (octave_idx_type i = 0; i < m_ext; i++)
        ndata[i] = ! m_data[i];
      std::fill_n (ndata + m_ext, n - m_ext, true);
      retval = new idx_mask_rep (mask, n - nz);
    }
  else
    {
      Array<bool> mask (dim_vector (n, 1), true);
      fill (false, length (n), mask.fortran_vec ());
      retval = idx_vector (mask);
    }

  return retval;
}

bool idx_vector::is_permutation (octave_idx_type n) const
{
  bool retval = false;

  if (is_colon_equiv (n))
    retval = true;
  else if (length(n) == n && extent(n) == n)
    {
      OCTAVE_LOCAL_BUFFER_INIT (bool, left, n, true);

      retval = true;

      for (octave_idx_type i = 0, m_len = length (); i < m_len; i++)
        {
          octave_idx_type k = xelem (i);
          if (left[k])
            left[k] = false;
          else
            {
              retval = false;
              break;
            }
        }
    }

  return retval;
}

idx_vector idx_vector::inverse_permutation (octave_idx_type n) const
{
  assert (n == length (n));

  idx_vector retval;

  switch (idx_class ())
    {
    case class_range:
      {
        if (increment () == -1)
          retval = sorted ();
        else
          retval = *this;
        break;
      }
    case class_vector:
      {
        idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
        const octave_idx_type *ri = r->get_data ();
        Array<octave_idx_type> idx (orig_dimensions ());
        for (octave_idx_type i = 0; i < n; i++)
          idx.xelem (ri[i]) = i;
        retval = new idx_vector_rep (idx, r->extent (0), DIRECT);
        break;
      }
    default:
      retval = *this;
      break;
    }

  return retval;
}

idx_vector idx_vector::unmask (void) const
{
  if (idx_class () == class_mask)
    {
      idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
      const bool *m_data = r->get_data ();
      octave_idx_type m_ext = r->extent (0);
      octave_idx_type m_len = r->length (0);
      octave_idx_type *idata = new octave_idx_type [m_len];

      for (octave_idx_type i = 0, j = 0; i < m_ext; i++)
        if (m_data[i])
          idata[j++] = i;

      m_ext = (m_len > 0 ? idata[m_len - 1] + 1 : 0);

      return new idx_vector_rep (idata, m_len, m_ext, r->orig_dimensions (),
                                 DIRECT);
    }
  else
    return *this;
}

void idx_vector::unconvert (idx_class_type& iclass,
                            double& scalar, range<double>& range,
                            Array<double>& array, Array<bool>& mask) const
{
  iclass = idx_class ();
  switch (iclass)
    {
    case class_colon:
      break;

    case class_range:
      {
        idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
        range = r->unconvert ();
      }
      break;

    case class_scalar:
      {
        idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
        scalar = r->unconvert ();
      }
      break;

    case class_vector:
      {
        idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
        array = r->unconvert ();
      }
      break;

    case class_mask:
      {
        idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
        mask = r->unconvert ();
      }
      break;

    default:
      assert (false);
      break;
    }
}

Array<octave_idx_type> idx_vector::as_array (void) const
{
  return m_rep->as_array ();
}

bool idx_vector::isvector (void) const
{
  return idx_class () != class_vector || orig_dimensions ().isvector ();
}

octave_idx_type
idx_vector::freeze (octave_idx_type z_len, const char *, bool resize_ok)
{
  if (! resize_ok && extent (z_len) > z_len)
    (*current_liboctave_error_handler)
      ("invalid matrix index = %" OCTAVE_IDX_TYPE_FORMAT, extent (z_len));

  return length (z_len);
}

octave_idx_type idx_vector::ones_count () const
{
  octave_idx_type n = 0;

  if (is_colon ())
    n = 1;
  else
    {
      for (octave_idx_type i = 0; i < length (1); i++)
        if (xelem (i) == 0)
          n++;
    }

  return n;
}

// Instantiate the octave_int constructors we want.
#define INSTANTIATE_SCALAR_VECTOR_REP_CONST(T)                          \
  template OCTAVE_API idx_vector::idx_scalar_rep::idx_scalar_rep (T);   \
  template OCTAVE_API idx_vector::idx_vector_rep::idx_vector_rep (const Array<T>&);

INSTANTIATE_SCALAR_VECTOR_REP_CONST (float)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (double)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int8)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int16)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int32)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int64)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint8)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint16)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint32)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint64)

OCTAVE_END_NAMESPACE(octave)

/*

%!error id=Octave:index-out-of-bounds 1(find ([1,1] != 0))
%!assert ((1:3)(find ([1,0,1] != 0)), [1,3])

*/
