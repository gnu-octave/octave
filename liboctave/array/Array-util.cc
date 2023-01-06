////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
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

#include "Array-util.h"
#include "lo-error.h"
#include "oct-locbuf.h"

bool
index_in_bounds (const Array<octave_idx_type>& ra_idx,
                 const dim_vector& dimensions)
{
  bool retval = true;

  int n = ra_idx.numel ();

  if (n == dimensions.ndims ())
    {
      for (int i = 0; i < n; i++)
        {
          if (ra_idx(i) < 0 || ra_idx(i) >= dimensions(i))
            {
              retval = false;
              break;
            }
        }
    }
  else
    retval = false;

  return retval;
}

void
increment_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions,
                 int start_dimension)
{
  ra_idx(start_dimension)++;

  int n = ra_idx.numel () - 1;
  int nda = dimensions.ndims ();

  for (int i = start_dimension; i < n; i++)
    {
      if (ra_idx(i) < (i < nda ? dimensions(i) : 1))
        break;
      else
        {
          ra_idx(i) = 0;
          ra_idx(i+1)++;
        }
    }
}

octave_idx_type
get_scalar_idx (Array<octave_idx_type>& idx, dim_vector& dims)
{
  octave_idx_type retval (-1);

  int n = idx.numel ();

  if (n > 0)
    {
      retval = idx(--n);

      while (--n >= 0)
        {
          retval *= dims(n);

          retval += idx(n);
        }
    }
  return retval;
}

octave_idx_type
num_ones (const Array<octave_idx_type>& ra_idx)
{
  octave_idx_type retval = 0;

  for (octave_idx_type i = 0; i < ra_idx.numel (); i++)
    {
      if (ra_idx(i) == 1)
        retval++;
    }

  return retval;
}

bool
is_scalar (const dim_vector& dim)
{
  bool retval = true;

  int n = dim.ndims ();

  if (n == 0)
    retval = false;
  else
    {
      for (int i = 0; i < n; i++)
        {
          if (dim(i) != 1)
            {
              retval = false;

              break;
            }
        }
    }
  return retval;
}

bool
isvector (const dim_vector& dim)
{
  int m = 0;
  int n = dim.ndims ();

  if (n == 0)
    m = 2;
  else
    {
      for (int i = 0; i < n; i++)
        if (dim(i) > 1)
          m++;
        else if (dim(i) < 1)
          m += 2;
    }

  return (m < 2);
}

bool
any_ones (const Array<octave_idx_type>& arr)
{
  bool retval = false;

  for (octave_idx_type i = 0; i < arr.numel (); i++)
    {
      if (arr (i) == 1)
        {
          retval = true;

          break;
        }
    }
  return retval;
}

octave_idx_type
compute_index (octave_idx_type n, const dim_vector& dims)
{
  if (n < 0)
    octave::err_invalid_index (n, 1, 1);
  if (n >= dims.numel ())
    octave::err_index_out_of_range (1, 1, n+1, dims.numel (), dims);

  return n;
}

octave_idx_type
compute_index (octave_idx_type i, octave_idx_type j, const dim_vector& dims)
{
  if (i < 0)
    octave::err_invalid_index (i, 2, 1);
  if (j < 0)
    octave::err_invalid_index (j, 2, 2);
  if (i >= dims(0))
    octave::err_index_out_of_range (2, 1, i+1, dims(0), dims);
  if (j >= dims.numel (1))
    octave::err_index_out_of_range (2, 2, j+1, dims.numel (1), dims);

  return j*dims(0) + i;
}

octave_idx_type
compute_index (octave_idx_type i, octave_idx_type j, octave_idx_type k,
               const dim_vector& dims)
{
  if (i < 0)
    octave::err_invalid_index (i, 3, 1);
  if (j < 0)
    octave::err_invalid_index (j, 3, 2);
  if (k < 0)
    octave::err_invalid_index (k, 3, 3);
  if (i >= dims(0))
    octave::err_index_out_of_range (3, 1, i+1, dims(0), dims);
  if (j >= dims(1))
    octave::err_index_out_of_range (3, 2, j+1, dims(1), dims);
  if (k >= dims.numel (2))
    octave::err_index_out_of_range (3, 3, k+1, dims.numel (2), dims);

  return (k*dims(1) + j)*dims(0) + i;
}

octave_idx_type
compute_index (const Array<octave_idx_type>& ra_idx, const dim_vector& dims)
{
  int nd = ra_idx.numel ();
  const dim_vector dv = dims.redim (nd);
  for (int d = 0; d < nd; d++)
    {
      if (ra_idx(d) < 0)
        octave::err_invalid_index (ra_idx(d), nd, d+1);
      if (ra_idx(d) >= dv(d))
        octave::err_index_out_of_range (nd, d+1, ra_idx(d)+1, dv(d), dims);
    }

  return dv.compute_index (ra_idx.data ());
}

Array<octave_idx_type>
conv_to_int_array (const Array<octave::idx_vector>& a)
{
  Array<octave_idx_type> retval (a.dims ());

  for (octave_idx_type i = 0; i < a.numel (); i++)
    retval(i) = a(i).elem (0);

  return retval;
}

Array<octave::idx_vector>
conv_to_array (const octave::idx_vector *tmp, const octave_idx_type len)
{
  Array<octave::idx_vector> retval (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    retval(i) = tmp[i];

  return retval;
}

dim_vector
freeze (Array<octave::idx_vector>& ra_idx, const dim_vector& dimensions, int resize_ok)
{
  dim_vector retval;

  int n = ra_idx.numel ();

  assert (n == dimensions.ndims ());

  retval.resize (n);

  static const char *tag[3] = { "row", "column", nullptr };

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).freeze (dimensions(i), tag[i < 2 ? i : 2],
                                  resize_ok);

  return retval;
}

bool
vector_equivalent (const dim_vector& dv)
{
  int n = dv.ndims ();

  bool found_first = false;

  for (int i = 0; i < n; i++)
    {
      if (dv(i) != 1)
        {
          if (! found_first)
            found_first = true;
          else
            return false;
        }
    }

  return true;
}

bool
all_ok (const Array<octave::idx_vector>& ra_idx)
{
  bool retval = true;

  octave_idx_type n = ra_idx.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (! ra_idx(i))
        {
          retval = false;
          break;
        }
    }

  return retval;
}

bool
any_orig_empty (const Array<octave::idx_vector>& ra_idx)
{
  bool retval = false;

  octave_idx_type n = ra_idx.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (ra_idx(i).orig_empty ())
        {
          retval = true;
          break;
        }
    }

  return retval;
}

bool
all_colon_equiv (const Array<octave::idx_vector>& ra_idx,
                 const dim_vector& frozen_lengths)
{
  bool retval = true;

  octave_idx_type idx_n = ra_idx.numel ();

  int n = frozen_lengths.ndims ();

  assert (idx_n == n);

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (! ra_idx(i).is_colon_equiv (frozen_lengths(i)))
        {
          retval = false;
          break;
        }
    }

  return retval;
}

bool
all_ones (const Array<octave_idx_type>& arr)
{
  bool retval = true;

  for (octave_idx_type i = 0; i < arr.numel (); i++)
    {
      if (arr(i) != 1)
        {
          retval = false;
          break;
        }
    }

  return retval;
}

Array<octave_idx_type>
get_elt_idx (const Array<octave::idx_vector>& ra_idx,
             const Array<octave_idx_type>& result_idx)
{
  octave_idx_type n = ra_idx.numel ();

  Array<octave_idx_type> retval (dim_vector (n, 1));

  for (octave_idx_type i = 0; i < n; i++)
    retval(i) = ra_idx(i).elem (result_idx(i));

  return retval;
}

Array<octave_idx_type>
get_ra_idx (octave_idx_type idx, const dim_vector& dims)
{
  Array<octave_idx_type> retval;

  int n_dims = dims.ndims ();

  retval.resize (dim_vector (n_dims, 1));

  for (int i = 0; i < n_dims; i++)
    retval(i) = 0;

  assert (idx > 0 || idx < dims.numel ());

  for (octave_idx_type i = 0; i < idx; i++)
    increment_index (retval, dims);

  // FIXME: the solution using increment_index is not efficient.

#if 0
  octave_idx_type var = 1;
  for (int i = 0; i < n_dims; i++)
    {
      std::cout << "idx: " << idx << ", var: " << var
                << ", dims(" << i << "): " << dims(i) <<"\n";
      retval(i) = ((int)floor(((idx) / (double)var))) % dims(i);
      idx -= var * retval(i);
      var = dims(i);
    }
#endif

  return retval;
}

dim_vector
zero_dims_inquire (const Array<octave::idx_vector>& ia, const dim_vector& rhdv)
{
  int ial = ia.numel ();
  int rhdvl = rhdv.ndims ();
  dim_vector rdv = dim_vector::alloc (ial);
  bool *scalar = new bool [ial];
  bool *colon = new bool [ial];
  // Mark scalars and colons, count non-scalar indices.
  int nonsc = 0;
  bool all_colons = true;
  for (int i = 0; i < ial; i++)
    {
      // FIXME: should we check for length() instead?
      scalar[i] = ia(i).is_scalar ();
      colon[i] = ia(i).is_colon ();
      if (! scalar[i]) nonsc++;
      if (! colon[i]) rdv(i) = ia(i).extent (0);
      all_colons = all_colons && colon[i];
    }

  // If the number of nonscalar indices matches the dimensionality of
  // RHS, we try an exact match, inquiring even singleton dimensions.
  if (all_colons)
    {
      rdv = rhdv;
      rdv.resize (ial, 1);
    }
  else if (nonsc == rhdvl)
    {
      for (int i = 0, j = 0; i < ial; i++)
        {
          if (scalar[i]) continue;
          if (colon[i])
            rdv(i) = rhdv(j);
          j++;
        }
    }
  else
    {
      dim_vector rhdv0 = rhdv;
      rhdv0.chop_all_singletons ();
      int rhdv0l = rhdv0.ndims ();
      for (int i = 0, j = 0; i < ial; i++)
        {
          if (scalar[i]) continue;
          if (colon[i])
            rdv(i) = (j < rhdv0l) ? rhdv0(j++) : 1;
        }
    }

  delete [] scalar;
  delete [] colon;

  return rdv;
}

dim_vector
zero_dims_inquire (const octave::idx_vector& i, const octave::idx_vector& j,
                   const dim_vector& rhdv)
{
  bool icol = i.is_colon ();
  bool jcol = j.is_colon ();
  dim_vector rdv;
  if (icol && jcol && rhdv.ndims () == 2)
    {
      rdv(0) = rhdv(0);
      rdv(1) = rhdv(1);
    }
  else if (rhdv.ndims () == 2
           && ! i.is_scalar () && ! j.is_scalar ())
    {
      rdv(0) = (icol ? rhdv(0) : i.extent (0));
      rdv(1) = (jcol ? rhdv(1) : j.extent (0));
    }
  else
    {
      dim_vector rhdv0 = rhdv;
      rhdv0.chop_all_singletons ();
      int k = 0;
      rdv(0) = i.extent (0);
      if (icol)
        rdv(0) = rhdv0(k++);
      else if (! i.is_scalar ())
        k++;
      rdv(1) = j.extent (0);
      if (jcol)
        rdv(1) = rhdv0(k++);
      else if (! j.is_scalar ())
        k++;
    }

  return rdv;
}

octave::idx_vector
sub2ind (const dim_vector& dv, const Array<octave::idx_vector>& idxa)
{
  octave::idx_vector retval;
  octave_idx_type len = idxa.numel ();

  if (len == 0)
    (*current_liboctave_error_handler) ("sub2ind: needs at least 2 indices");

  const dim_vector dvx = dv.redim (len);
  bool all_ranges = true;
  octave_idx_type clen = -1;

  for (octave_idx_type i = 0; i < len; i++)
    {
      try
        {
          octave::idx_vector idx = idxa(i);
          octave_idx_type n = dvx(i);

          all_ranges = all_ranges && idx.is_range ();
          if (clen < 0)
            clen = idx.length (n);
          else if (clen != idx.length (n))
            (*current_liboctave_error_handler)
              ("sub2ind: lengths of indices must match");

          if (idx.extent (n) > n)
            octave::err_index_out_of_range (len, i+1, idx.extent (n), n, dv);
        }
      catch (octave::index_exception& ie)
        {
          ie.set_pos_if_unset (len, i+1);
          ie.set_var ();
          std::string msg = ie.message ();
          (*current_liboctave_error_with_id_handler)
            (ie.err_id (), "%s", msg.c_str ());
        }
    }
  // idxa known to be valid.
  // Shouldn't need to catch index_exception below here.

  if (len == 1)
    retval = idxa(0);
  else if (clen == 1)
    {
      // All scalars case - the result is a scalar.
      octave_idx_type idx = idxa(len-1)(0);
      for (octave_idx_type i = len - 2; i >= 0; i--)
        idx = dvx(i) * idx + idxa(i)(0);
      retval = octave::idx_vector (idx);
    }
  else if (all_ranges && clen != 0)
    {
      // All ranges case - the result is a range.
      octave_idx_type start = 0;
      octave_idx_type step = 0;
      for (octave_idx_type i = len - 1; i >= 0; i--)
        {
          octave_idx_type xstart = idxa(i)(0);
          octave_idx_type xstep = idxa(i)(1) - xstart;
          start = dvx(i) * start + xstart;
          step = dvx(i) * step + xstep;
        }
      retval = octave::idx_vector::make_range (start, step, clen);
    }
  else
    {
      Array<octave_idx_type> idx (idxa(0).orig_dimensions ());

      for (octave_idx_type i = len - 1; i >= 0; i--)
        {
          // Initialized inside the loop so that each call to
          // idx_vector::loop operates from the beginning of IDX_VEC.

          octave_idx_type *idx_vec = idx.fortran_vec ();

          if (i < len - 1)
            {
              octave_idx_type n = dvx(i);

              idxa(i).loop (clen, [=, &idx_vec] (octave_idx_type k) {
                (*idx_vec++ *= n) += k;
              });
            }
          else
            idxa(i).copy_data (idx_vec);
        }

      retval = octave::idx_vector (idx);
    }

  return retval;
}

Array<octave::idx_vector>
ind2sub (const dim_vector& dv, const octave::idx_vector& idx)
{
  octave_idx_type len = idx.length (0);
  octave_idx_type n = dv.ndims ();
  Array<octave::idx_vector> retval (dim_vector (n, 1));
  octave_idx_type numel = dv.numel ();

  if (idx.is_scalar ())
    {
      octave_idx_type k = idx(0);
      for (octave_idx_type j = 0; j < n-1; j++)
        {
          retval(j) = k % dv(j);
          k /= dv(j);
        }

      retval(n-1) = idx(0) < numel ? k % dv(n-1) : k;
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (Array<octave_idx_type>, rdata, n);

      dim_vector odv = idx.orig_dimensions ();
      for (octave_idx_type j = 0; j < n; j++)
        rdata[j] = Array<octave_idx_type> (odv);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_idx_type k = idx(i);
          for (octave_idx_type j = 0; j < n-1; j++)
            {
              rdata[j](i) = k % dv(j);
              k /= dv(j);
            }

          rdata[n-1](i) = idx(i) < numel ? k % dv(n-1) : k;
        }

      for (octave_idx_type j = 0; j < n; j++)
        retval(j) = rdata[j];
    }

  return retval;
}

int
permute_vector_compare (const void *a, const void *b)
{
  const permute_vector *pva = static_cast<const permute_vector *> (a);
  const permute_vector *pvb = static_cast<const permute_vector *> (b);

  return pva->pidx > pvb->pidx;
}
