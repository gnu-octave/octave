// Template sparse array class
/*

Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <climits>

#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

#include "Array.h"
#include "MArray.h"
#include "Array-util.h"
#include "Range.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "quit.h"
#include "oct-locbuf.h"

#include "Sparse.h"
#include "sparse-sort.h"
#include "sparse-util.h"
#include "oct-spparms.h"
#include "mx-inlines.cc"

template <class T>
T&
Sparse<T>::SparseRep::elem (octave_idx_type _r, octave_idx_type _c)
{
  octave_idx_type i;

  if (nzmx > 0)
    {
      for (i = c[_c]; i < c[_c + 1]; i++)
        if (r[i] == _r)
          return d[i];
        else if (r[i] > _r)
          break;

      // Ok, If we've gotten here, we're in trouble.. Have to create a 
      // new element in the sparse array. This' gonna be slow!!!
      if (c[ncols] == nzmx)
        {
          (*current_liboctave_error_handler)
            ("Sparse::SparseRep::elem (octave_idx_type, octave_idx_type): sparse matrix filled");
          return *d;
        }

      octave_idx_type to_move = c[ncols] - i;
      if (to_move != 0)
        {
          for (octave_idx_type j = c[ncols]; j > i; j--)
            {
              d[j] = d[j-1];
              r[j] = r[j-1];
            }
        }

      for (octave_idx_type j = _c + 1; j < ncols + 1; j++)
        c[j] = c[j] + 1;
      
      d[i] = 0.;
      r[i] = _r;

      return d[i];
    }
  else
    {
      (*current_liboctave_error_handler)
        ("Sparse::SparseRep::elem (octave_idx_type, octave_idx_type): sparse matrix filled");
      return *d;
    }
}

template <class T>
T
Sparse<T>::SparseRep::celem (octave_idx_type _r, octave_idx_type _c) const
{
  if (nzmx > 0)
    for (octave_idx_type i = c[_c]; i < c[_c + 1]; i++)
      if (r[i] == _r)
        return d[i];
  return T ();
}

template <class T>
void
Sparse<T>::SparseRep::maybe_compress (bool remove_zeros)
{
  if (remove_zeros)
    {
      octave_idx_type i = 0, k = 0;
      for (octave_idx_type j = 1; j <= ncols; j++)
        {
          octave_idx_type u = c[j];
          for (i = i; i < u; i++)
            if (d[i] != T())
              {
                d[k] = d[i];
                r[k++] = r[i];
              }
          c[j] = k;
        }
    }

  change_length (c[ncols]);
}

template <class T>
void
Sparse<T>::SparseRep::change_length (octave_idx_type nz)
{
  for (octave_idx_type j = ncols; j > 0 && c[j] > nz; j--)
    c[j] = nz;

  // We shall skip reallocation if we have less than 1/frac extra elements to
  // discard.
  static const int frac = 5;
  if (nz > nzmx || nz < nzmx - nzmx/frac)
    {
      // Reallocate.
      octave_idx_type min_nzmx = std::min (nz, nzmx);

      octave_idx_type * new_ridx = new octave_idx_type [nz];
      copy_or_memcpy (min_nzmx, r, new_ridx);

      delete [] r;
      r = new_ridx;

      T * new_data = new T [nz];
      copy_or_memcpy (min_nzmx, d, new_data);

      delete [] d;
      d = new_data;

      nzmx = nz;
    }
}

template <class T>
bool
Sparse<T>::SparseRep::indices_ok (void) const
{
  return sparse_indices_ok (r, c, nrows, ncols, nnz ());
}

template <class T>
template <class U>
Sparse<T>::Sparse (const Sparse<U>& a)
  : dimensions (a.dimensions), idx (0), idx_count (0)
{
  if (a.nnz () == 0)
    rep = new typename Sparse<T>::SparseRep (rows (), cols());
  else
    {
      rep = new typename Sparse<T>::SparseRep (rows (), cols (), a.nnz ());
      
      octave_idx_type nz = a.nnz ();
      octave_idx_type nc = cols ();
      for (octave_idx_type i = 0; i < nz; i++)
        {
          xdata (i) = T (a.data (i));
          xridx (i) = a.ridx (i);
        }
      for (octave_idx_type i = 0; i < nc + 1; i++)
        xcidx (i) = a.cidx (i);
    }
}

template <class T>
Sparse<T>::Sparse (octave_idx_type nr, octave_idx_type nc, T val)
  : dimensions (dim_vector (nr, nc)), idx (0), idx_count (0)
{ 
  if (val != T ())
    {
      rep = new typename Sparse<T>::SparseRep (nr, nc, nr*nc);

      octave_idx_type ii = 0;
      xcidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              xdata (ii) = val;
              xridx (ii++) = i;
            } 
          xcidx (j+1) = ii;
        }
    }
  else
    {
      rep = new typename Sparse<T>::SparseRep (nr, nc, 0);
      for (octave_idx_type j = 0; j < nc+1; j++)
        xcidx(j) = 0;
    }
}

template <class T>
Sparse<T>::Sparse (const dim_vector& dv)
  : dimensions (dv), idx (0), idx_count (0)
{ 
  if (dv.length() != 2)
    (*current_liboctave_error_handler)
      ("Sparse::Sparse (const dim_vector&): dimension mismatch");
  else
    rep = new typename Sparse<T>::SparseRep (dv(0), dv(1));
}

template <class T>
Sparse<T>::Sparse (const Sparse<T>& a, const dim_vector& dv)
  : dimensions (dv), idx (0), idx_count (0)
{

  // Work in unsigned long long to avoid overflow issues with numel
  unsigned long long a_nel = static_cast<unsigned long long>(a.rows ()) *
    static_cast<unsigned long long>(a.cols ());
  unsigned long long dv_nel = static_cast<unsigned long long>(dv (0)) *
    static_cast<unsigned long long>(dv (1));

  if (a_nel != dv_nel)
    (*current_liboctave_error_handler)
      ("Sparse::Sparse (const Sparse&, const dim_vector&): dimension mismatch");
  else
    {
      dim_vector old_dims = a.dims();
      octave_idx_type new_nzmx = a.nnz ();
      octave_idx_type new_nr = dv (0);
      octave_idx_type new_nc = dv (1);
      octave_idx_type old_nr = old_dims (0);
      octave_idx_type old_nc = old_dims (1);

      rep = new typename Sparse<T>::SparseRep (new_nr, new_nc, new_nzmx);

      octave_idx_type kk = 0;
      xcidx(0) = 0;
      for (octave_idx_type i = 0; i < old_nc; i++)
        for (octave_idx_type j = a.cidx(i); j < a.cidx(i+1); j++)
          {
            octave_idx_type tmp = i * old_nr + a.ridx(j);
            octave_idx_type ii = tmp % new_nr;
            octave_idx_type jj = (tmp - ii) / new_nr; 
            for (octave_idx_type k = kk; k < jj; k++)
              xcidx(k+1) = j;
            kk = jj;
            xdata(j) = a.data(j);
            xridx(j) = ii;
          }
      for (octave_idx_type k = kk; k < new_nc; k++)
        xcidx(k+1) = new_nzmx;
    }
}

template <class T>
Sparse<T>::Sparse (const Array<T>& a, const idx_vector& r, 
                   const idx_vector& c, octave_idx_type nr,
                   octave_idx_type nc, bool sum_terms)
  : rep (nil_rep ()), dimensions (), idx (0), idx_count (0)
{
  if (nr < 0)
      nr = r.extent (0);
  else if (r.extent (nr) > nr)
    (*current_liboctave_error_handler) ("sparse: row index %d out of bound %d",
                                        r.extent (nr), nr);

  if (nc < 0)
      nc = c.extent (0);
  else if (c.extent (nc) > nc)
    (*current_liboctave_error_handler) ("sparse: column index %d out of bound %d",
                                        r.extent (nc), nc);

  if (--rep->count == 0)
    delete rep;
  rep = new SparseRep (nr, nc);

  dimensions = dim_vector (nr, nc);


  octave_idx_type n = a.numel (), rl = r.length (nr), cl = c.length (nc);
  bool a_scalar = n == 1;
  if (a_scalar)
    {
      if (rl != 1)
        n = rl;
      else if (cl != 1)
        n = cl;
    }

  if ((rl != 1 && rl != n) || (cl != 1 && cl != n))
    (*current_liboctave_error_handler) ("sparse: dimension mismatch");
    
  if (rl <= 1 && cl <= 1)
    {
      if (n == 1 && a(0) != T ())
        {
          change_capacity (1);
          xridx(0) = r(0);
          xdata(0) = a(0);
          for (octave_idx_type j = 0; j < nc; j++)
            xcidx(j+1) = j >= c(0);
        }
    }
  else if (a_scalar)
    {
      // This is completely specialized, because the sorts can be simplified.
      T a0 = a(0);
      if (cl == 1)
        {
          // Sparse column vector. Sort row indices.
          idx_vector rs = r.sorted ();

          octave_quit ();

          const octave_idx_type *rd = rs.raw ();
          // Count unique indices.
          octave_idx_type new_nz = 1;
          for (octave_idx_type i = 1; i < n; i++)
            new_nz += rd[i-1] != rd[i];
          // Allocate result.
          change_capacity (new_nz);
          xcidx (1) = new_nz;
          octave_idx_type *rri = ridx ();
          T *rrd = data ();

          octave_quit ();

          octave_idx_type k = -1, l = -1;

          if (sum_terms)
            {
              // Sum repeated indices.
              for (octave_idx_type i = 0; i < n; i++)
                {
                  if (rd[i] != l)
                    {
                      l = rd[i];
                      rri[++k] = rd[i];
                      rrd[k] = a0;
                    }
                  else
                    rrd[k] += a0;
                }
            }
          else
            {
              // Pick the last one.
              for (octave_idx_type i = 1; i < n; i++)
                {
                  if (rd[i] != l)
                    {
                      l = rd[i];
                      rrd[++k] = a0;
                      rri[k] = rd[i];
                    }
                }
            }

        }
      else
        {
          idx_vector rr = r, cc = c;
          const octave_idx_type *rd = rr.raw (), *cd = cc.raw ();
          OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, ci, nc+1, 0);
          ci[0] = 0;
          // Bin counts of column indices.
          for (octave_idx_type i = 0; i < n; i++)
            ci[cd[i]+1]++;
          // Make them cumulative, shifted one to right.
          for (octave_idx_type i = 1, s = 0; i <= nc; i++)
            {
              octave_idx_type s1 = s + ci[i];
              ci[i] = s;
              s = s1;
            }

          octave_quit ();

          // Bucket sort.
          OCTAVE_LOCAL_BUFFER (octave_idx_type, sidx, n);
          for (octave_idx_type i = 0; i < n; i++)
            sidx[ci[cd[i]+1]++] = rd[i];

          // Subsorts. We don't need a stable sort, all values are equal.
          xcidx(0) = 0;
          for (octave_idx_type j = 0; j < nc; j++)
            {
              std::sort (sidx + ci[j], sidx + ci[j+1]);
              octave_idx_type l = -1, nzj = 0;
              // Count.
              for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                {
                  octave_idx_type k = sidx[i];
                  if (k != l)
                    {
                      l = k;
                      nzj++;
                    }
                }
              // Set column pointer.
              xcidx(j+1) = xcidx(j) + nzj;
            }

          change_capacity (xcidx (nc));
          octave_idx_type *rri = ridx ();
          T *rrd = data ();

          // Fill-in data.
          for (octave_idx_type j = 0, jj = -1; j < nc; j++)
            {
              octave_quit ();
              octave_idx_type l = -1;
              if (sum_terms)
                {
                  // Sum adjacent terms.
                  for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                    {
                      octave_idx_type k = sidx[i];
                      if (k != l)
                        {
                          l = k;
                          rrd[++jj] = a0;
                          rri[jj] = k;
                        }
                      else
                        rrd[jj] += a0;
                    }
                }
              else
                {
                  // Use the last one.
                  for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                    {
                      octave_idx_type k = sidx[i];
                      if (k != l)
                        {
                          l = k;
                          rrd[++jj] = a0;
                          rri[jj] = k;
                        }
                    }
                }
            }
        }
    }
  else if (cl == 1)
    {
      // Sparse column vector. Sort row indices.
      Array<octave_idx_type> rsi;
      idx_vector rs = r.sorted (rsi);
      
      octave_quit ();

      const octave_idx_type *rd = rs.raw (), *rdi = rsi.data ();
      // Count unique indices.
      octave_idx_type new_nz = 1;
      for (octave_idx_type i = 1; i < n; i++)
        new_nz += rd[i-1] != rd[i];
      // Allocate result.
      change_capacity (new_nz);
      xcidx(1) = new_nz;
      octave_idx_type *rri = ridx ();
      T *rrd = data ();

      octave_quit ();

      octave_idx_type k = 0;
      rri[k] = rd[0];
      rrd[k] = a(rdi[0]);

      if (sum_terms)
        {
          // Sum repeated indices.
          for (octave_idx_type i = 1; i < n; i++)
            {
              if (rd[i] != rd[i-1])
                {
                  rri[++k] = rd[i];
                  rrd[k] = a(rdi[i]);
                }
              else
                rrd[k] += a(rdi[i]);
            }
        }
      else
        {
          // Pick the last one.
          for (octave_idx_type i = 1; i < n; i++)
            {
              if (rd[i] != rd[i-1])
                rri[++k] = rd[i];
              rrd[k] = a(rdi[i]);
            }
        }
    }
  else
    {
      idx_vector rr = r, cc = c;
      const octave_idx_type *rd = rr.raw (), *cd = cc.raw ();
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, ci, nc+1, 0);
      ci[0] = 0;
      // Bin counts of column indices.
      for (octave_idx_type i = 0; i < n; i++)
        ci[cd[i]+1]++;
      // Make them cumulative, shifted one to right.
      for (octave_idx_type i = 1, s = 0; i <= nc; i++)
        {
          octave_idx_type s1 = s + ci[i];
          ci[i] = s;
          s = s1;
        }

      octave_quit ();

      typedef std::pair<octave_idx_type, octave_idx_type> idx_pair;
      // Bucket sort.
      OCTAVE_LOCAL_BUFFER (idx_pair, spairs, n);
      for (octave_idx_type i = 0; i < n; i++)
        {
          idx_pair& p = spairs[ci[cd[i]+1]++];
          p.first = rd[i];
          p.second = i;
        }

      // Subsorts. We don't need a stable sort, the second index stabilizes it.
      xcidx(0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
        {
          std::sort (spairs + ci[j], spairs + ci[j+1]);
          octave_idx_type l = -1, nzj = 0;
          // Count.
          for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
            {
              octave_idx_type k = spairs[i].first;
              if (k != l)
                {
                  l = k;
                  nzj++;
                }
            }
          // Set column pointer.
          xcidx(j+1) = xcidx(j) + nzj;
        }

      change_capacity (xcidx (nc));
      octave_idx_type *rri = ridx ();
      T *rrd = data ();

      // Fill-in data.
      for (octave_idx_type j = 0, jj = -1; j < nc; j++)
        {
          octave_quit ();
          octave_idx_type l = -1;
          if (sum_terms)
            {
              // Sum adjacent terms.
              for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                {
                  octave_idx_type k = spairs[i].first;
                  if (k != l)
                    {
                      l = k;
                      rrd[++jj] = a(spairs[i].second);
                      rri[jj] = k;
                    }
                  else
                    rrd[jj] += a(spairs[i].second);
                }
            }
          else
            {
              // Use the last one.
              for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                {
                  octave_idx_type k = spairs[i].first;
                  if (k != l)
                    {
                      l = k;
                      rri[++jj] = k;
                    }
                  rrd[jj] = a(spairs[i].second);
                }
            }
        }
    }
}

template <class T>
Sparse<T>::Sparse (const Array<T>& a)
  : dimensions (a.dims ()), idx (0), idx_count (0)
{
  if (dimensions.length () > 2)
    (*current_liboctave_error_handler)
      ("Sparse::Sparse (const Array<T>&): dimension mismatch");
  else
    {
      octave_idx_type nr = rows ();
      octave_idx_type nc = cols ();
      octave_idx_type len = a.length ();
      octave_idx_type new_nzmx = 0;

      // First count the number of non-zero terms
      for (octave_idx_type i = 0; i < len; i++)
        if (a(i) != T ())
          new_nzmx++;

      rep = new typename Sparse<T>::SparseRep (nr, nc, new_nzmx);

      octave_idx_type ii = 0;
      xcidx(0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            if (a.elem (i,j) != T ())
              {
                xdata(ii) = a.elem (i,j);
                xridx(ii++) = i;
              }
          xcidx(j+1) = ii;
        }
    }
}

template <class T>
Sparse<T>::~Sparse (void)
{
  if (--rep->count <= 0)
    delete rep;

  delete [] idx;
}

template <class T>
Sparse<T>&
Sparse<T>::operator = (const Sparse<T>& a)
{
  if (this != &a)
    {
      if (--rep->count <= 0)
        delete rep;

      rep = a.rep;
      rep->count++;

      dimensions = a.dimensions;

      delete [] idx;
      idx_count = 0;
      idx = 0;
    }

  return *this;
}

template <class T>
octave_idx_type
Sparse<T>::compute_index (const Array<octave_idx_type>& ra_idx) const
{
  octave_idx_type retval = -1;

  octave_idx_type n = dimensions.length ();

  if (n > 0 && n == ra_idx.length ())
    {
      retval = ra_idx(--n);

      while (--n >= 0)
        {
          retval *= dimensions(n);
          retval += ra_idx(n);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("Sparse<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, octave_idx_type n) const
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, octave_idx_type n)
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  static T foo;
  return foo;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  static T foo;
  return foo;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, const Array<octave_idx_type>& ra_idx) const
{
  std::ostringstream buf;

  buf << fcn << " (";

  octave_idx_type n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (octave_idx_type i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";
  
  std::string buf_str = buf.str ();

  (*current_liboctave_error_handler) (buf_str.c_str ());

  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, const Array<octave_idx_type>& ra_idx)
{
  std::ostringstream buf;

  buf << fcn << " (";

  octave_idx_type n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (octave_idx_type i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  std::string buf_str = buf.str ();

  (*current_liboctave_error_handler) (buf_str.c_str ());

  static T foo;
  return foo;
}

template <class T>
Sparse<T>
Sparse<T>::reshape (const dim_vector& new_dims) const
{
  Sparse<T> retval;
  dim_vector dims2 = new_dims;

  if (dims2.length () > 2)
    {
      (*current_liboctave_warning_handler)
        ("reshape: sparse reshape to N-d array smashes dims");

      for (octave_idx_type i = 2; i < dims2.length(); i++)
        dims2(1) *= dims2(i);

      dims2.resize (2);
    }

  if (dimensions != dims2)
    {
      if (dimensions.numel () == dims2.numel ())
        {
          octave_idx_type new_nnz = nnz ();
          octave_idx_type new_nr = dims2 (0);
          octave_idx_type new_nc = dims2 (1);
          octave_idx_type old_nr = rows ();
          octave_idx_type old_nc = cols ();
          retval = Sparse<T> (new_nr, new_nc, new_nnz);

          octave_idx_type kk = 0;
          retval.xcidx(0) = 0;
          for (octave_idx_type i = 0; i < old_nc; i++)
            for (octave_idx_type j = cidx(i); j < cidx(i+1); j++)
              {
                octave_idx_type tmp = i * old_nr + ridx(j);
                octave_idx_type ii = tmp % new_nr;
                octave_idx_type jj = (tmp - ii) / new_nr; 
                for (octave_idx_type k = kk; k < jj; k++)
                  retval.xcidx(k+1) = j;
                kk = jj;
                retval.xdata(j) = data(j);
                retval.xridx(j) = ii;
              }
          for (octave_idx_type k = kk; k < new_nc; k++)
            retval.xcidx(k+1) = new_nnz;
        }
      else
        {
          std::string dimensions_str = dimensions.str ();
          std::string new_dims_str = new_dims.str ();

          (*current_liboctave_error_handler)
            ("reshape: can't reshape %s array to %s array",
             dimensions_str.c_str (), new_dims_str.c_str ());
        }
    }
  else
    retval = *this;

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::permute (const Array<octave_idx_type>& perm_vec, bool) const
{
  // The only valid permutations of a sparse array are [1, 2] and [2, 1].

  bool fail = false;
  bool trans = false;

  if (perm_vec.length () == 2)
    {
      if (perm_vec(0) == 0 && perm_vec(1) == 1)
        /* do nothing */;
      else if (perm_vec(0) == 1 && perm_vec(1) == 0)
        trans = true;
      else
        fail = true;
    }
  else
    fail = true;

  if (fail)
    (*current_liboctave_error_handler)
      ("permutation vector contains an invalid element");

  return trans ? this->transpose () : *this;
}

template <class T>
void
Sparse<T>::resize1 (octave_idx_type n)
{
  octave_idx_type nr = rows (), nc = cols ();

  if (nr == 1 || nr == 0)
    resize (1, n);
  else if (nc == 1)
    resize (n, 1);
  else
    gripe_invalid_resize ();
}

template <class T>
void
Sparse<T>::resize (const dim_vector& dv)
{
  octave_idx_type n = dv.length ();

  if (n != 2)
    {
      (*current_liboctave_error_handler) ("sparse array must be 2-D");
      return;
    }

  resize (dv(0), dv(1));
}

template <class T>
void
Sparse<T>::resize (octave_idx_type r, octave_idx_type c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
        ("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  // This wouldn't be necessary for r >= rows () if nrows wasn't part of the
  // Sparse rep. It is not good for anything in there.
  make_unique ();

  if (r < rows ())
    {
      octave_idx_type i = 0, k = 0;
      for (octave_idx_type j = 1; j <= rep->ncols; j++)
        {
          octave_idx_type u = xcidx(j);
          for (i = i; i < u; i++)
            if (xridx(i) < r)
              {
                xdata(k) = xdata(i);
                xridx(k++) = xridx(i);
              }
          xcidx(j) = k;
        }
    }

  rep->nrows = dimensions(0) = r;

  if (c != rep->ncols)
    {
      octave_idx_type *new_cidx = new octave_idx_type [c+1];
      copy_or_memcpy (std::min (c, rep->ncols)+1, rep->c, new_cidx);
      delete [] rep->c;
      rep->c = new_cidx;

      if (c > rep->ncols)
        fill_or_memset (c - rep->ncols, rep->c[rep->ncols], rep->c + rep->ncols + 1);
    }

  rep->ncols = dimensions(1) = c;

  rep->change_length (rep->nnz ());
}

template <class T>
Sparse<T>&
Sparse<T>::insert (const Sparse<T>& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_rows = a.rows ();
  octave_idx_type a_cols = a.cols ();
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r < 0 || r + a_rows > rows () || c < 0 || c + a_cols > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  // First count the number of elements in the final array
  octave_idx_type nel = cidx(c) + a.nnz ();

  if (c + a_cols < nc)
    nel += cidx(nc) - cidx(c + a_cols);

  for (octave_idx_type i = c; i < c + a_cols; i++)
    for (octave_idx_type j = cidx(i); j < cidx(i+1); j++)
      if (ridx(j) < r || ridx(j) >= r + a_rows)
        nel++;

  Sparse<T> tmp (*this);
  --rep->count;
  rep = new typename Sparse<T>::SparseRep (nr, nc, nel);

  for (octave_idx_type i = 0; i < tmp.cidx(c); i++)
    {
      data(i) = tmp.data(i);
      ridx(i) = tmp.ridx(i);
    }
  for (octave_idx_type i = 0; i < c + 1; i++)
    cidx(i) = tmp.cidx(i);

  octave_idx_type ii = cidx(c);

  for (octave_idx_type i = c; i < c + a_cols; i++)
    {
      octave_quit ();

      for (octave_idx_type j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
        if (tmp.ridx(j) < r)
          {
            data(ii) = tmp.data(j);
            ridx(ii++) = tmp.ridx(j);
          }

      octave_quit ();

      for (octave_idx_type j = a.cidx(i-c); j < a.cidx(i-c+1); j++)
        {
          data(ii) = a.data(j);
          ridx(ii++) = r + a.ridx(j);
        }

      octave_quit ();

      for (octave_idx_type j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
        if (tmp.ridx(j) >= r + a_rows)
          {
            data(ii) = tmp.data(j);
            ridx(ii++) = tmp.ridx(j);
          }

      cidx(i+1) = ii;
    }

  for (octave_idx_type i = c + a_cols; i < nc; i++)
    {
      for (octave_idx_type j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
        {
          data(ii) = tmp.data(j);
          ridx(ii++) = tmp.ridx(j);
        }
      cidx(i+1) = ii;
    }

  return *this;
}

template <class T>
Sparse<T>&
Sparse<T>::insert (const Sparse<T>& a, const Array<octave_idx_type>& ra_idx)
{

  if (ra_idx.length () != 2)
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  return insert (a, ra_idx (0), ra_idx (1));
}

template <class T>
Sparse<T>
Sparse<T>::transpose (void) const
{
  assert (ndims () == 2);

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz = nnz ();
  Sparse<T> retval (nc, nr, nz);

  for (octave_idx_type i = 0; i < nz; i++)
    retval.xcidx (ridx (i) + 1)++;
  // retval.xcidx[1:nr] holds the row degrees for rows 0:(nr-1)
  nz = 0;
  for (octave_idx_type i = 1; i <= nr; i++)
    {
      const octave_idx_type tmp = retval.xcidx (i);
      retval.xcidx (i) = nz;
      nz += tmp;
    }
  // retval.xcidx[1:nr] holds row entry *start* offsets for rows 0:(nr-1)

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type k = cidx(j); k < cidx(j+1); k++)
      {
        octave_idx_type q = retval.xcidx (ridx (k) + 1)++;
        retval.xridx (q) = j;
        retval.xdata (q) = data (k);
      }
  assert (nnz () == retval.xcidx (nr));
  // retval.xcidx[1:nr] holds row entry *end* offsets for rows 0:(nr-1)
  // and retval.xcidx[0:(nr-1)] holds their row entry *start* offsets

  return retval;
}

template <class T>
void
Sparse<T>::clear_index (void)
{
  delete [] idx;
  idx = 0;
  idx_count = 0;
}

template <class T>
void
Sparse<T>::set_index (const idx_vector& idx_arg)
{
  octave_idx_type nd = ndims ();

  if (! idx && nd > 0)
    idx = new idx_vector [nd];

  if (idx_count < nd)
    {
      idx[idx_count++] = idx_arg;
    }
  else
    {
      idx_vector *new_idx = new idx_vector [idx_count+1];

      for (octave_idx_type i = 0; i < idx_count; i++)
        new_idx[i] = idx[i];

      new_idx[idx_count++] = idx_arg;

      delete [] idx;

      idx = new_idx;
    }
}

// Lower bound lookup. Could also use octave_sort, but that has upper bound
// semantics, so requires some manipulation to set right. Uses a plain loop for
// small columns.
static octave_idx_type
lblookup (const octave_idx_type *ridx, octave_idx_type nr,
          octave_idx_type ri)
{
  if (nr <= 8)
    {
      octave_idx_type l;
      for (l = 0; l < nr; l++)
        if (ridx[l] >= ri)
          break;
      return l;
    }
  else
    return std::lower_bound (ridx, ridx + nr, ri) - ridx;
}

template <class T>
void
Sparse<T>::delete_elements (const idx_vector& idx)
{
  Sparse<T> retval;

  assert (ndims () == 2);

  // FIXME: please don't fix the shadowed member warning yet because
  // Sparse<T>::idx will eventually go away.

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type nel = numel (); // Can throw.

  const dim_vector idx_dims = idx.orig_dimensions ();

  if (idx.extent (nel) > nel)
    gripe_del_index_out_of_range (true, idx.extent (nel), nel);
  else if (nc == 1)
    {
      // Sparse column vector.
      const Sparse<T> tmp = *this; // constant copy to prevent COW.

      octave_idx_type lb, ub;

      if (idx.is_cont_range (nel, lb, ub))
        {
          // Special-case a contiguous range.
          // Look-up indices first.
          octave_idx_type li = lblookup (tmp.ridx (), nz, lb);
          octave_idx_type ui = lblookup (tmp.ridx (), nz, ub);
          // Copy data and adjust indices.
          octave_idx_type nz_new = nz - (ui - li);
          *this = Sparse<T> (nr - (ub - lb), 1, nz_new);
          copy_or_memcpy (li, tmp.data (), data ());
          copy_or_memcpy (li, tmp.ridx (), xridx ());
          copy_or_memcpy (nz - ui, tmp.data () + ui, xdata () + li);
          mx_inline_sub (nz - ui, xridx () + li, tmp.ridx () + ui, ub - lb);
          xcidx(1) = nz_new;
        }
      else
        {
          OCTAVE_LOCAL_BUFFER (octave_idx_type, ridx_new, nz);
          OCTAVE_LOCAL_BUFFER (T, data_new, nz);
          idx_vector sidx = idx.sorted (true);
          const octave_idx_type *sj = sidx.raw ();
          octave_idx_type sl = sidx.length (nel), nz_new = 0, j = 0;
          for (octave_idx_type i = 0; i < nz; i++)
            {
              octave_idx_type r = tmp.ridx(i);
              for (;j < sl && sj[j] < r; j++) ;
              if (j == sl || sj[j] > r)
                {
                  data_new[nz_new] = tmp.data(i);
                  ridx_new[nz_new++] = r - j;
                }
            }

          *this = Sparse<T> (nr - sl, 1, nz_new);
          copy_or_memcpy (nz_new, ridx_new, ridx ());
          copy_or_memcpy (nz_new, data_new, xdata ());
          xcidx(1) = nz_new;
        }
    }
  else if (nr == 1)
    {
      // Sparse row vector.
      octave_idx_type lb, ub;
      if (idx.is_cont_range (nc, lb, ub))
        {
          const Sparse<T> tmp = *this;
          octave_idx_type lbi = tmp.cidx(lb), ubi = tmp.cidx(ub), new_nz = nz - (ubi - lbi);
          *this = Sparse<T> (1, nc - (ub - lb), new_nz);
          copy_or_memcpy (lbi, tmp.data (), data ());
          copy_or_memcpy (nz - ubi, tmp.data () + ubi, xdata () + lbi);
          fill_or_memset (new_nz, static_cast<octave_idx_type> (0), ridx ());
          copy_or_memcpy (lb, tmp.cidx () + 1, cidx () + 1);
          mx_inline_sub (nc - ub, xcidx () + 1, tmp.cidx () + ub + 1, ubi - lbi);
        }
      else
        *this = index (idx.complement (nc));
    }
  else
    {
      *this = index (idx_vector::colon);
      delete_elements (idx);
      *this = transpose (); // We want a row vector.
    }
}

template <class T>
void
Sparse<T>::delete_elements (const idx_vector& idx_i, const idx_vector& idx_j)
{
  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  if (idx_i.is_colon ())
    {
      // Deleting columns.
      octave_idx_type lb, ub;
      if (idx_j.extent (nc) > nc)
        gripe_del_index_out_of_range (false, idx_j.extent (nc), nc);
      else if (idx_j.is_cont_range (nc, lb, ub))
        {
          const Sparse<T> tmp = *this;
          octave_idx_type lbi = tmp.cidx(lb), ubi = tmp.cidx(ub), new_nz = nz - (ubi - lbi);
          *this = Sparse<T> (nr, nc - (ub - lb), new_nz);
          copy_or_memcpy (lbi, tmp.data (), data ());
          copy_or_memcpy (lbi, tmp.ridx (), ridx ());
          copy_or_memcpy (nz - ubi, tmp.data () + ubi, xdata () + lbi);
          copy_or_memcpy (nz - ubi, tmp.ridx () + ubi, xridx () + lbi);
          copy_or_memcpy (lb, tmp.cidx () + 1, cidx () + 1);
          mx_inline_sub (nc - ub, xcidx () + 1, tmp.cidx () + ub + 1, ubi - lbi);
        }
      else
        *this = index (idx_i, idx_j.complement (nc));
    }
  else if (idx_j.is_colon ())
    {
      // Deleting rows.
      octave_idx_type lb, ub;
      if (idx_i.extent (nr) > nr)
        gripe_del_index_out_of_range (false, idx_i.extent (nr), nr);
      else if (idx_i.is_cont_range (nr, lb, ub))
        {
          // This is more memory-efficient than the approach below.
          const Sparse<T> tmpl = index (idx_vector (0, lb), idx_j);
          const Sparse<T> tmpu = index (idx_vector (ub, nr), idx_j);
          *this = Sparse<T> (nr - (ub - lb), nc, tmpl.nnz () + tmpu.nnz ());
          for (octave_idx_type j = 0, k = 0; j < nc; j++)
            {
              for (octave_idx_type i = tmpl.cidx(j); i < tmpl.cidx(j+1); i++)
                {
                  xdata(k) = tmpl.data(i);
                  xridx(k++) = tmpl.ridx(i);
                }
              for (octave_idx_type i = tmpu.cidx(j); i < tmpu.cidx(j+1); i++)
                {
                  xdata(k) = tmpu.data(i);
                  xridx(k++) = tmpu.ridx(i) + lb;
                }

              xcidx(j+1) = k;
            }
        }
      else
        {
          // This is done by transposing, deleting columns, then transposing
          // again.
          Sparse<T> tmp = transpose ();
          tmp.delete_elements (idx_j, idx_i);
          *this = tmp.transpose ();
        }
    }
  else
    (*current_liboctave_error_handler)
      ("A null assignment can only have one non-colon index.");
}

template <class T>
void
Sparse<T>::delete_elements (int dim, const idx_vector& idx)
{
  if (dim == 0)
    delete_elements (idx, idx_vector::colon);
  else if (dim == 1)
    delete_elements (idx_vector::colon, idx);
  else
    {
      (*current_liboctave_error_handler)
        ("invalid dimension in delete_elements");
      return;
    }
}

template <class T>
Sparse<T>
Sparse<T>::value (void)
{
  Sparse<T> retval;

  int n_idx = index_count ();

  if (n_idx == 2)
    {
      idx_vector *tmp = get_idx ();

      idx_vector idx_i = tmp[0];
      idx_vector idx_j = tmp[1];

      retval = index (idx_i, idx_j);
    }
  else if (n_idx == 1)
    {
      retval = index (idx[0]);
    }
  else
    (*current_liboctave_error_handler)
      ("Sparse<T>::value: invalid number of indices specified");

  clear_index ();

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::index (const idx_vector& idx, bool resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  // FIXME: please don't fix the shadowed member warning yet because
  // Sparse<T>::idx will eventually go away.

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type nel = numel (); // Can throw.

  const dim_vector idx_dims = idx.orig_dimensions ();

  if (idx_dims.length () > 2)
    (*current_liboctave_error_handler)
      ("cannot index sparse matrix with an N-D Array");
  else if (idx.is_colon ())
    {
      if (nc == 1)
        retval = *this;
      else
        {
          // Fast magic colon processing.
          retval = Sparse<T> (nel, 1, nz);

          for (octave_idx_type i = 0; i < nc; i++)
            {
              for (octave_idx_type j = cidx(i); j < cidx(i+1); j++)
                {
                  retval.xdata(j) = data(j); 
                  retval.xridx(j) = ridx(j) + i * nr;
                }
            }

          retval.xcidx(0) = 0;
          retval.xcidx(1) = nz;
        }
    }
  else if (idx.extent (nel) > nel)
    {
      // resize_ok is completely handled here.
      if (resize_ok)
        {
          octave_idx_type ext = idx.extent (nel);
          Sparse<T> tmp = *this;
          tmp.resize1 (ext);
          retval = tmp.index (idx);
        }
      else
        gripe_index_out_of_range (1, 1, idx.extent (nel), nel);
    }
  else if (nr == 1 && nc == 1)
    {
      // You have to be pretty sick to get to this bit of code,
      // since you have a scalar stored as a sparse matrix, and
      // then want to make a dense matrix with sparse 
      // representation. Ok, we'll do it, but you deserve what 
      // you get!!
      retval = Sparse<T> (idx_dims(0), idx_dims(1), nz ? data(0) : T ());
    }
  else if (nc == 1)
    {
      // Sparse column vector.
      octave_idx_type lb, ub;

      if (idx.is_scalar ())
        {
          // Scalar index - just a binary lookup.
          octave_idx_type i = lblookup (ridx (), nz, idx(0));
          if (i < nz && ridx(i) == idx(0))
            retval = Sparse (1, 1, data(i));
          else
            retval = Sparse (1, 1);
        }
      else if (idx.is_cont_range (nel, lb, ub))
        {
          // Special-case a contiguous range.
          // Look-up indices first.
          octave_idx_type li = lblookup (ridx (), nz, lb);
          octave_idx_type ui = lblookup (ridx (), nz, ub);
          // Copy data and adjust indices.
          octave_idx_type nz_new = ui - li;
          retval = Sparse<T> (ub - lb, 1, nz_new);
          copy_or_memcpy (nz_new, data () + li, retval.data ());
          mx_inline_sub (nz_new, retval.xridx (), ridx () + li, lb);
          retval.xcidx(1) = nz_new;
        }
      else
        {
          // If indexing a sparse column vector by a vector, the result is a
          // sparse column vector, otherwise it inherits the shape of index.
          // Vector transpose is cheap, so do it right here.
          const Array<octave_idx_type> idxa = (idx_dims(0) == 1 
                                               ? idx.as_array ().transpose ()
                                               : idx.as_array ());

          octave_idx_type new_nr = idxa.rows (), new_nc = idxa.cols ();

          // Lookup.
          // FIXME: Could specialize for sorted idx?
          NoAlias< Array<octave_idx_type> > lidx (new_nr, new_nc);
          for (octave_idx_type i = 0; i < new_nr*new_nc; i++)
            lidx(i) = lblookup (ridx (), nz, idxa(i));

          // Count matches.
          retval = Sparse<T> (idxa.rows (), idxa.cols ());
          for (octave_idx_type j = 0; j < new_nc; j++)
            {
              octave_idx_type nzj = 0;
              for (octave_idx_type i = 0; i < new_nr; i++)
                {
                  octave_idx_type l = lidx(i, j);
                  if (l < nz && ridx(l) == idxa(i, j))
                    nzj++;
                  else
                    lidx(i, j) = nz;
                }
              retval.xcidx(j+1) = retval.xcidx(j) + nzj;
            }

          retval.change_capacity (retval.xcidx(new_nc));
          
          // Copy data and set row indices.
          octave_idx_type k = 0;
          for (octave_idx_type j = 0; j < new_nc; j++)
            for (octave_idx_type i = 0; i < new_nr; i++)
              {
                octave_idx_type l = lidx(i, j);
                if (l < nz)
                  {
                    retval.data(k) = data(l);
                    retval.xridx(k++) = i;
                  }
              }
        }
    }
  else if (nr == 1)
    {
      octave_idx_type lb, ub;
      if (idx.is_scalar ())
        retval = Sparse<T> (1, 1, elem(0, idx(0)));
      else if (idx.is_cont_range (nel, lb, ub))
        {
          // Special-case a contiguous range.
          octave_idx_type lbi = cidx(lb), ubi = cidx(ub), new_nz = ubi - lbi;
          retval = Sparse<T> (1, ub - lb, new_nz);
          copy_or_memcpy (new_nz, data () + lbi, retval.data ());
          fill_or_memset (new_nz, static_cast<octave_idx_type> (0), retval.ridx ());
          mx_inline_sub (ub - lb + 1, retval.cidx (), cidx () + lb, lbi);
        }
      else
        {
          // Sparse row vectors occupy O(nr) storage anyway, so let's just
          // convert the matrix to full, index, and sparsify the result.
          retval = Sparse<T> (array_value ().index (idx));
        }
    }
  else
    {
      (*current_liboctave_warning_with_id_handler) 
        ("Octave:fortran-indexing", "single index used for sparse matrix");

      if (nr != 0 && idx.is_scalar ())
        retval = Sparse<T> (1, 1, elem (idx(0) % nr, idx(0) / nr));
      else
        {
          // Indexing a non-vector sparse matrix by linear indexing. 
          // I suppose this is rare (and it may easily overflow), so let's take the easy way,
          // and reshape first to column vector, which is already handled above.
          retval = index (idx_vector::colon).index (idx);
          // In this case we're supposed to always inherit the shape, but column(row) doesn't do
          // it, so we'll do it instead.
          if (idx_dims(0) == 1 && idx_dims(1) != 1)
            retval = retval.transpose ();
        }
    }

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::index (const idx_vector& idx_i, const idx_vector& idx_j, bool resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  octave_idx_type n = idx_i.length (nr);
  octave_idx_type m = idx_j.length (nc);

  octave_idx_type lb, ub;

  if (idx_i.extent (nr) > nr || idx_j.extent (nc) > nc)
    {
      // resize_ok is completely handled here.
      if (resize_ok)
        {
          octave_idx_type ext_i = idx_i.extent (nr);
          octave_idx_type ext_j = idx_j.extent (nc);
          Sparse<T> tmp = *this;
          tmp.resize (ext_i, ext_j);
          retval = tmp.index (idx_i, idx_j);
        }
      else if (idx_i.extent (nr) > nr)
        gripe_index_out_of_range (2, 1, idx_i.extent (nr), nr);
      else
        gripe_index_out_of_range (2, 2, idx_j.extent (nc), nc);
    }
  else if (idx_i.is_colon ())
    {
      // Great, we're just manipulating columns. This is going to be quite
      // efficient, because the columns can stay compressed as they are.
      if (idx_j.is_colon ())
        retval = *this; // Shallow copy.
      else if (idx_j.is_cont_range (nc, lb, ub))
        {
          // Special-case a contiguous range.
          octave_idx_type lbi = cidx(lb), ubi = cidx(ub), new_nz = ubi - lbi;
          retval = Sparse<T> (nr, ub - lb, new_nz);
          copy_or_memcpy (new_nz, data () + lbi, retval.data ());
          copy_or_memcpy (new_nz, ridx () + lbi, retval.ridx ());
          mx_inline_sub (ub - lb + 1, retval.cidx (), cidx () + lb, lbi);
        }
      else
        {
          // Count new nonzero elements.
          retval = Sparse<T> (nr, m);
          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_idx_type jj = idx_j(j);
              retval.xcidx(j+1) = retval.xcidx(j) + (cidx(jj+1) - cidx(jj));
            }

          retval.change_capacity (retval.xcidx (m));

          // Copy data & indices.
          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_idx_type ljj = cidx(idx_j(j));
              octave_idx_type lj = retval.xcidx(j), nzj = retval.xcidx(j+1) - lj;
              copy_or_memcpy (nzj, data () + ljj, retval.data () + lj);
              copy_or_memcpy (nzj, ridx () + ljj, retval.ridx () + lj);
            }
        }
    }
  else if (idx_i.is_scalar ())
    {
      octave_idx_type ii = idx_i(0);
      retval = Sparse<T> (1, m);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ij, m);
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_quit ();
          octave_idx_type jj = idx_j(j), lj = cidx(jj), nzj = cidx(jj+1) - cidx(jj);
          // Scalar index - just a binary lookup.
          octave_idx_type i = lblookup (ridx () + lj, nzj, ii);
          if (i < nzj && ridx(i+lj) == ii)
            {
              ij[j] = i + lj;
              retval.xcidx(j+1) = retval.xcidx(j) + 1;
            }
          else
            retval.xcidx(j+1) = retval.xcidx(j);
        }

      retval.change_capacity (retval.xcidx(m));

      // Copy data, adjust row indices.
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_idx_type i = retval.xcidx(j);
          if (retval.xcidx(j+1) > i)
            {
              retval.xridx(i) = 0;
              retval.xdata(i) = data(ij[j]);
            }
        }
    }
  else if (idx_i.is_cont_range (nr, lb, ub))
    {
      retval = Sparse<T> (n, m);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, li, m);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ui, m);
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_quit ();
          octave_idx_type jj = idx_j(j), lj = cidx(jj), nzj = cidx(jj+1) - cidx(jj);
          octave_idx_type lij, uij;
          // Lookup indices.
          li[j] = lij = lblookup (ridx () + lj, nzj, lb) + lj;
          ui[j] = uij = lblookup (ridx () + lj, nzj, ub) + lj;
          retval.xcidx(j+1) = retval.xcidx(j) + ui[j] - li[j];
        }

      retval.change_capacity (retval.xcidx(m));

      // Copy data, adjust row indices.
      for (octave_idx_type j = 0, k = 0; j < m; j++)
        {
          octave_quit ();
          for (octave_idx_type i = li[j]; i < ui[j]; i++)
            {
              retval.xdata(k) = data(i);
              retval.xridx(k++) = ridx(i) - lb;
            }
        }
    }
  else if (idx_i.is_permutation (nr))
    {
      // The columns preserve their length, we just need to renumber and sort them.
      // Count new nonzero elements.
      retval = Sparse<T> (nr, m);
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_idx_type jj = idx_j(j);
          retval.xcidx(j+1) = retval.xcidx(j) + (cidx(jj+1) - cidx(jj));
        }

      retval.change_capacity (retval.xcidx (m));

      octave_quit ();

      if (idx_i.is_range () && idx_i.increment () == -1)
        {
          // It's nr:-1:1. Just flip all columns.
          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_quit ();
              octave_idx_type jj = idx_j(j), lj = cidx(jj), nzj = cidx(jj+1) - cidx(jj);
              octave_idx_type li = retval.xcidx(j), uj = lj + nzj - 1;
              for (octave_idx_type i = 0; i < nzj; i++)
                {
                  retval.xdata(li + i) = data(uj - i); // Copy in reverse order.
                  retval.xridx(li + i) = nr - 1 - ridx(uj - i); // Ditto with transform.
                }
            }
        }
      else
        {
          // Get inverse permutation.
          OCTAVE_LOCAL_BUFFER (octave_idx_type, iinv, nr);
          const Array<octave_idx_type> ia = idx_i.as_array ();
          for (octave_idx_type i = 0; i < nr; i++)
            iinv[ia(i)] = i;

          // Scatter buffer.
          OCTAVE_LOCAL_BUFFER (T, scb, nr);
          octave_idx_type *rri = retval.ridx ();

          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_quit ();
              octave_idx_type jj = idx_j(j), lj = cidx(jj), nzj = cidx(jj+1) - cidx(jj);
              octave_idx_type li = retval.xcidx(j);
              // Scatter the column, transform indices.
              for (octave_idx_type i = 0; i < nzj; i++)
                scb[rri[li + i] = iinv[ridx(lj + i)]] = data(lj + i);

              octave_quit ();

              // Sort them.
              std::sort (rri + li, rri + li + nzj);

              // Gather.
              for (octave_idx_type i = 0; i < nzj; i++)
                retval.xdata(li + i) = scb[rri[li + i]];
            }
        }

    }
  else
    {
      // This is the most general case, where all optimizations failed.
      // I suppose this is a relatively rare case, so it will be done
      // as s(i,j) = ((s(:,j).')(:,i)).'
      // Note that if j is :, the first indexing expr. is a shallow copy.
      retval = index (idx_vector::colon, idx_j).transpose ();
      retval = retval.index (idx_vector::colon, idx_i).transpose ();
    }

  return retval;
}

// Can't use versions of these in Array.cc due to duplication of the 
// instantiations for Array<double and Sparse<double>, etc
template <class T>
bool 
sparse_ascending_compare (typename ref_param<T>::type a, 
                          typename ref_param<T>::type b)
{
  return (a < b);
}

template <class T>
bool 
sparse_descending_compare (typename ref_param<T>::type a, 
                           typename ref_param<T>::type b)
{
  return (a > b);
}

template <class T>
Sparse<T>
Sparse<T>::sort (octave_idx_type dim, sortmode mode) const
{
  Sparse<T> m = *this;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (m.length () < 1)
    return m;

  if (dim > 0)
    {
      m = m.transpose ();
      nr = m.rows ();
      nc = m.columns ();
    }

  octave_sort<T> lsort;

  if (mode == ASCENDING) 
    lsort.set_compare (sparse_ascending_compare<T>);
  else if (mode == DESCENDING)
    lsort.set_compare (sparse_descending_compare<T>);
  else
    abort ();

  T *v = m.data ();
  octave_idx_type *mcidx = m.cidx ();
  octave_idx_type *mridx = m.ridx ();

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_idx_type ns = mcidx [j + 1] - mcidx [j];
      lsort.sort (v, ns);

      octave_idx_type i;
      if (mode == ASCENDING) 
        {
          for (i = 0; i < ns; i++)
            if (sparse_ascending_compare<T> (static_cast<T> (0), v [i]))
              break;
        }
      else
        {
          for (i = 0; i < ns; i++)
            if (sparse_descending_compare<T> (static_cast<T> (0), v [i]))
              break;
        }
      for (octave_idx_type k = 0; k < i; k++)
        mridx [k] = k;
      for (octave_idx_type k = i; k < ns; k++)
        mridx [k] = k - ns + nr; 

      v += ns;
      mridx += ns;
    }

  if (dim > 0)
      m = m.transpose ();

  return m;
}

template <class T>
Sparse<T>
Sparse<T>::sort (Array<octave_idx_type> &sidx, octave_idx_type dim, 
                 sortmode mode) const
{
  Sparse<T> m = *this;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (m.length () < 1)
    {
      sidx = Array<octave_idx_type> (dim_vector (nr, nc));
      return m;
    }

  if (dim > 0)
    {
      m = m.transpose ();
      nr = m.rows ();
      nc = m.columns ();
    }

  octave_sort<T> indexed_sort;

  if (mode == ASCENDING) 
    indexed_sort.set_compare (sparse_ascending_compare<T>);
  else if (mode == DESCENDING)
    indexed_sort.set_compare (sparse_descending_compare<T>);
  else
    abort ();

  T *v = m.data ();
  octave_idx_type *mcidx = m.cidx ();
  octave_idx_type *mridx = m.ridx ();

  sidx = Array<octave_idx_type> (dim_vector (nr, nc));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, vi, nr);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_idx_type ns = mcidx [j + 1] - mcidx [j];
      octave_idx_type offset = j * nr;

      if (ns == 0)
        {
          for (octave_idx_type k = 0; k < nr; k++)
            sidx (offset + k) = k;
        }
      else
        {
          for (octave_idx_type i = 0; i < ns; i++)
            vi[i] = mridx[i];

          indexed_sort.sort (v, vi, ns);

          octave_idx_type i;
          if (mode == ASCENDING) 
            {
              for (i = 0; i < ns; i++)
                if (sparse_ascending_compare<T> (static_cast<T> (0), v[i]))
                  break;
            }
          else
            {
              for (i = 0; i < ns; i++)
                if (sparse_descending_compare<T> (static_cast<T> (0), v[i]))
                  break;
            }

          octave_idx_type ii = 0;
          octave_idx_type jj = i;
          for (octave_idx_type k = 0; k < nr; k++)
            {
              if (ii < ns && mridx[ii] == k)
                ii++;
              else
                sidx (offset + jj++) = k;
            }

          for (octave_idx_type k = 0; k < i; k++)
            {
              sidx (k + offset) = vi [k];
              mridx [k] = k;
            }

          for (octave_idx_type k = i; k < ns; k++)
            {
              sidx (k - ns + nr + offset) = vi [k];
              mridx [k] = k - ns + nr; 
            }

          v += ns;
          mridx += ns;
        }
    }

  if (dim > 0)
    {
      m = m.transpose ();
      sidx = sidx.transpose ();
    }

  return m;
}

template <class T>
Sparse<T>
Sparse<T>::diag (octave_idx_type k) const
{
  octave_idx_type nnr = rows ();
  octave_idx_type nnc = cols ();
  Sparse<T> d;

  if (nnr == 0 || nnc == 0)
    ; // do nothing
  else if (nnr != 1 && nnc != 1)
    {
      if (k > 0)
        nnc -= k;
      else if (k < 0)
        nnr += k;

      if (nnr > 0 && nnc > 0)
        {
          octave_idx_type ndiag = (nnr < nnc) ? nnr : nnc;

          // Count the number of non-zero elements
          octave_idx_type nel = 0;
          if (k > 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                if (elem (i, i+k) != 0.)
                  nel++;
            }
          else if ( k < 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                if (elem (i-k, i) != 0.)
                  nel++;
            }
          else
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                if (elem (i, i) != 0.)
                  nel++;
            }
      
          d = Sparse<T> (ndiag, 1, nel);
          d.xcidx (0) = 0;
          d.xcidx (1) = nel;

          octave_idx_type ii = 0;
          if (k > 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                {
                  T tmp = elem (i, i+k);
                  if (tmp != 0.)
                    {
                      d.xdata (ii) = tmp;
                      d.xridx (ii++) = i;
                    }
                }
            }
          else if ( k < 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                {
                  T tmp = elem (i-k, i);
                  if (tmp != 0.)
                    {
                      d.xdata (ii) = tmp;
                      d.xridx (ii++) = i;
                    }
                }
            }
          else
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                {
                  T tmp = elem (i, i);
                  if (tmp != 0.)
                    {
                      d.xdata (ii) = tmp;
                      d.xridx (ii++) = i;
                    }
                }
            }
        }
      else
        (*current_liboctave_error_handler) 
          ("diag: requested diagonal out of range");
    }
  else if (nnr != 0 && nnc != 0)
    {
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
          octave_idx_type nz = nnz ();

          d = Sparse<T> (n, n, nz);

          if (nnz () > 0)
            {
              for (octave_idx_type i = 0; i < coff+1; i++)
                d.xcidx (i) = 0;

              for (octave_idx_type j = 0; j < nnc; j++)
                {
                  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
                    {
                      d.xdata (i) = data (i);
                      d.xridx (i) = j + roff;
                    }
                  d.xcidx (j + coff + 1) = cidx(j+1);
                }

              for (octave_idx_type i = nnc + coff + 1; i < n + 1; i++)
                d.xcidx (i) = nz;
            }
        } 
      else 
        {
          octave_idx_type n = nnr + std::abs (k);
          octave_idx_type nz = nnz ();

          d = Sparse<T> (n, n, nz);

          if (nnz () > 0)
            {
              octave_idx_type ii = 0;
              octave_idx_type ir = ridx(0);

              for (octave_idx_type i = 0; i < coff+1; i++)
                d.xcidx (i) = 0;

              for (octave_idx_type i = 0; i < nnr; i++)
                {
                  if (ir == i)
                    {
                      d.xdata (ii) = data (ii);
                      d.xridx (ii++) = ir + roff;

                      if (ii != nz)
                        ir = ridx (ii);
                    }
                  d.xcidx (i + coff + 1) = ii;
                }

              for (octave_idx_type i = nnr + coff + 1; i < n+1; i++)
                d.xcidx (i) = nz;
            }
        }
    }

  return d;
}

template <class T>
Array<T>
Sparse<T>::array_value () const
{
  NoAlias< Array<T> > retval (dims (), T());
  if (rows () == 1)
    {
      octave_idx_type i = 0;
      for (octave_idx_type j = 0, nc = cols (); j < nc; j++)
        {
          if (cidx(j+1) > i)
            retval(j) = data (i++);
        }
    }
  else
    {
      for (octave_idx_type j = 0, nc = cols (); j < nc; j++)
        for (octave_idx_type i = cidx(j), iu = cidx(j+1); i < iu; i++)
          retval (ridx(i), j) = data (i);
    }

  return retval;
}

// FIXME
// Unfortunately numel can overflow for very large but very sparse matrices.
// For now just flag an error when this happens.
template <class LT, class RT>
int
assign1 (Sparse<LT>& lhs, const Sparse<RT>& rhs)
{
  int retval = 1;

  idx_vector *idx_tmp = lhs.get_idx ();

  idx_vector lhs_idx = idx_tmp[0];

  octave_idx_type lhs_len = lhs.numel ();
  octave_idx_type rhs_len = rhs.numel ();

  uint64_t long_lhs_len = 
    static_cast<uint64_t> (lhs.rows ()) *
    static_cast<uint64_t> (lhs.cols ());

  uint64_t long_rhs_len = 
    static_cast<uint64_t> (rhs.rows ()) *
    static_cast<uint64_t> (rhs.cols ());

  if (long_rhs_len != static_cast<uint64_t>(rhs_len) ||
      long_lhs_len != static_cast<uint64_t>(lhs_len))
    {
      (*current_liboctave_error_handler)
        ("A(I) = X: Matrix dimensions too large to ensure correct\n",
         "operation. This is an limitation that should be removed\n",
         "in the future.");

      lhs.clear_index ();
      return 0;
    }

  octave_idx_type nr = lhs.rows ();
  octave_idx_type nc = lhs.cols ();
  octave_idx_type nz = lhs.nnz ();

  octave_idx_type n = lhs_idx.freeze (lhs_len, "vector", true);

  if (n != 0)
    {
      octave_idx_type max_idx = lhs_idx.max () + 1;
      max_idx = max_idx < lhs_len ? lhs_len : max_idx;

      // Take a constant copy of lhs. This means that elem won't 
      // create missing elements.
      const Sparse<LT> c_lhs (lhs);

      if (rhs_len == n)
        {
          octave_idx_type new_nzmx = lhs.nnz ();

          OCTAVE_LOCAL_BUFFER (octave_idx_type, rhs_idx, n);
          if (! lhs_idx.is_colon ())
            {
              // Ok here we have to be careful with the indexing,
              // to treat cases like "a([3,2,1]) = b", and still 
              // handle the need for strict sorting of the sparse 
              // elements.
              OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort *, sidx, n);
              OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort, sidxX, n);

              for (octave_idx_type i = 0; i < n; i++)
                {
                  sidx[i] = &sidxX[i];
                  sidx[i]->i = lhs_idx.elem(i);
                  sidx[i]->idx = i;
                }
                          
              octave_quit ();
              octave_sort<octave_idx_vector_sort *> 
                sort (octave_idx_vector_comp);

              sort.sort (sidx, n);

              intNDArray<octave_idx_type> new_idx (dim_vector (n,1));

              for (octave_idx_type i = 0; i < n; i++)
                {
                  new_idx.xelem(i) = sidx[i]->i;
                  rhs_idx[i] = sidx[i]->idx;
                }

              lhs_idx = idx_vector (new_idx);
            }
          else
            for (octave_idx_type i = 0; i < n; i++)
              rhs_idx[i] = i;

          // First count the number of non-zero elements
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_quit ();

              octave_idx_type ii = lhs_idx.elem (i);
              if (i < n - 1 && lhs_idx.elem (i + 1) == ii)
                continue;
              if (ii < lhs_len && c_lhs.elem(ii) != LT ())
                new_nzmx--;
              if (rhs.elem(rhs_idx[i]) != RT ())
                new_nzmx++;
            }

          if (nr > 1)
            {
              Sparse<LT> tmp ((max_idx > nr ? max_idx : nr), 1, new_nzmx);
              tmp.cidx(0) = 0;
              tmp.cidx(1) = new_nzmx;

              octave_idx_type i = 0;
              octave_idx_type ii = 0;
              if (i < nz)
                ii = c_lhs.ridx(i);

              octave_idx_type j = 0;
              octave_idx_type jj = lhs_idx.elem(j);

              octave_idx_type kk = 0;

              while (j < n || i < nz)
                {
                  if (j < n - 1 && lhs_idx.elem (j + 1) == jj)
                    {
                      j++;
                      jj = lhs_idx.elem (j);
                      continue;
                    }
                  if (j == n || (i < nz && ii < jj))
                    {
                      tmp.xdata (kk) = c_lhs.data (i);
                      tmp.xridx (kk++) = ii;
                      if (++i < nz)
                        ii = c_lhs.ridx(i);
                    }
                  else
                    {
                      RT rtmp = rhs.elem (rhs_idx[j]);
                      if (rtmp != RT ())
                        {
                          tmp.xdata (kk) = rtmp;
                          tmp.xridx (kk++) = jj;
                        }

                      if (ii == jj && i < nz)
                        if (++i < nz)
                          ii = c_lhs.ridx(i);
                      if (++j < n)
                        jj = lhs_idx.elem(j);
                    }
                }

              lhs = tmp;
            }
          else
            {
              Sparse<LT> tmp (1, (max_idx > nc ? max_idx : nc), new_nzmx);

              octave_idx_type i = 0;
              octave_idx_type ii = 0;
              while (ii < nc && c_lhs.cidx(ii+1) <= i)
                ii++;

              octave_idx_type j = 0;
              octave_idx_type jj = lhs_idx.elem(j);

              octave_idx_type kk = 0;
              octave_idx_type ic = 0;

              while (j < n || i < nz)
                {
                  if (j < n - 1 && lhs_idx.elem (j + 1) == jj)
                    {
                      j++;
                      jj = lhs_idx.elem (j);
                      continue;
                    }
                  if (j == n || (i < nz && ii < jj))
                    {
                      while (ic <= ii)
                        tmp.xcidx (ic++) = kk;
                      tmp.xdata (kk) = c_lhs.data (i);
                      tmp.xridx (kk++) = 0;
                      i++;
                      while (ii < nc && c_lhs.cidx(ii+1) <= i)
                        ii++;
                    }
                  else
                    {
                      while (ic <= jj)
                        tmp.xcidx (ic++) = kk;

                      RT rtmp = rhs.elem (rhs_idx[j]);
                      if (rtmp != RT ())
                        {
                          tmp.xdata (kk) = rtmp;
                          tmp.xridx (kk++) = 0;
                        }
                      if (ii == jj)
                        {
                          i++;
                          while (ii < nc && c_lhs.cidx(ii+1) <= i)
                            ii++;
                        }
                      j++;
                      if (j < n)
                        jj = lhs_idx.elem(j);
                    }
                }

              for (octave_idx_type iidx = ic; iidx < max_idx+1; iidx++)
                tmp.xcidx(iidx) = kk;

              lhs = tmp;
            }
        }
      else if (rhs_len == 1)
        {
          octave_idx_type new_nzmx = lhs.nnz ();
          RT scalar = rhs.elem (0);
          bool scalar_non_zero = (scalar != RT ());
          lhs_idx.sort (true);
          n = lhs_idx.length (n);

          // First count the number of non-zero elements
          if (scalar != RT ())
            new_nzmx += n;
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_quit ();

              octave_idx_type ii = lhs_idx.elem (i);
              if (ii < lhs_len && c_lhs.elem(ii) != LT ())
                new_nzmx--;
            }

          if (nr > 1)
            {
              Sparse<LT> tmp ((max_idx > nr ? max_idx : nr), 1, new_nzmx);
              tmp.cidx(0) = 0;
              tmp.cidx(1) = new_nzmx;

              octave_idx_type i = 0;
              octave_idx_type ii = 0;
              if (i < nz)
                ii = c_lhs.ridx(i);

              octave_idx_type j = 0;
              octave_idx_type jj = lhs_idx.elem(j);

              octave_idx_type kk = 0;

              while (j < n || i < nz)
                {
                  if (j == n || (i < nz && ii < jj))
                    {
                      tmp.xdata (kk) = c_lhs.data (i);
                      tmp.xridx (kk++) = ii;
                      if (++i < nz)
                        ii = c_lhs.ridx(i);
                    }
                  else
                    {
                      if (scalar_non_zero)
                        {
                          tmp.xdata (kk) = scalar;
                          tmp.xridx (kk++) = jj;
                        }

                      if (ii == jj && i < nz)
                        if (++i < nz)
                          ii = c_lhs.ridx(i);
                      if (++j < n)
                        jj = lhs_idx.elem(j);
                    }
                }

              lhs = tmp;
            }
          else
            {
              Sparse<LT> tmp (1, (max_idx > nc ? max_idx : nc), new_nzmx);

              octave_idx_type i = 0;
              octave_idx_type ii = 0;
              while (ii < nc && c_lhs.cidx(ii+1) <= i)
                ii++;

              octave_idx_type j = 0;
              octave_idx_type jj = lhs_idx.elem(j);

              octave_idx_type kk = 0;
              octave_idx_type ic = 0;

              while (j < n || i < nz)
                {
                  if (j == n || (i < nz && ii < jj))
                    {
                      while (ic <= ii)
                        tmp.xcidx (ic++) = kk;
                      tmp.xdata (kk) = c_lhs.data (i);
                      i++;
                      while (ii < nc && c_lhs.cidx(ii+1) <= i)
                        ii++;
                      tmp.xridx (kk++) = 0;
                    }
                  else
                    {
                      while (ic <= jj)
                        tmp.xcidx (ic++) = kk;
                      if (scalar_non_zero)
                        {
                          tmp.xdata (kk) = scalar;
                          tmp.xridx (kk++) = 0;
                        }
                      if (ii == jj)
                        {
                          i++;
                          while (ii < nc && c_lhs.cidx(ii+1) <= i)
                            ii++;
                        }
                      j++;
                      if (j < n)
                        jj = lhs_idx.elem(j);
                    }
                }

              for (octave_idx_type iidx = ic; iidx < max_idx+1; iidx++)
                tmp.xcidx(iidx) = kk;

              lhs = tmp;
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("A(I) = X: X must be a scalar or a vector with same length as I");

          retval = 0;
        }
    }
  else if (lhs_idx.is_colon ())
    {
      if (lhs_len == 0)
        {

          octave_idx_type new_nzmx = rhs.nnz ();
          Sparse<LT> tmp (1, rhs_len, new_nzmx);

          octave_idx_type ii = 0;
          octave_idx_type jj = 0;
          for (octave_idx_type i = 0; i < rhs.cols(); i++)
            for (octave_idx_type j = rhs.cidx(i); j < rhs.cidx(i+1); j++)
              {
                octave_quit ();
                for (octave_idx_type k = jj; k <= i * rhs.rows() + rhs.ridx(j); k++)
                  tmp.cidx(jj++) = ii;

                tmp.data(ii) = rhs.data(j);
                tmp.ridx(ii++) = 0;
              }

          for (octave_idx_type i = jj; i < rhs_len + 1; i++)
            tmp.cidx(i) = ii;

          lhs = tmp;
        }
      else
        (*current_liboctave_error_handler)
          ("A(:) = X: A must be the same size as X");
    }
  else if (! (rhs_len == 1 || rhs_len == 0))
    {
      (*current_liboctave_error_handler)
        ("A([]) = X: X must also be an empty matrix or a scalar");

      retval = 0;
    }

  lhs.clear_index ();

  return retval;
}

template <class LT, class RT>
int
assign (Sparse<LT>& lhs, const Sparse<RT>& rhs)
{
  int retval = 1;

  int n_idx = lhs.index_count ();

  octave_idx_type lhs_nr = lhs.rows ();
  octave_idx_type lhs_nc = lhs.cols ();
  octave_idx_type lhs_nz = lhs.nnz ();

  octave_idx_type rhs_nr = rhs.rows ();
  octave_idx_type rhs_nc = rhs.cols ();

  idx_vector *tmp = lhs.get_idx ();

  idx_vector idx_i;
  idx_vector idx_j;

  if (n_idx > 2)
    {
      (*current_liboctave_error_handler)
        ("A(I, J) = X: can only have 1 or 2 indexes for sparse matrices");

      lhs.clear_index ();
      return 0;
    }

  if (n_idx > 1)
    idx_j = tmp[1];

  if (n_idx > 0)
    idx_i = tmp[0];

  // Take a constant copy of lhs. This means that ridx and family won't 
  // call make_unique.
  const Sparse<LT> c_lhs (lhs);

  if (n_idx == 2)
    {
      octave_idx_type n = idx_i.freeze (lhs_nr, "row", true);
      octave_idx_type m = idx_j.freeze (lhs_nc, "column", true);

      int idx_i_is_colon = idx_i.is_colon ();
      int idx_j_is_colon = idx_j.is_colon ();

      if (lhs_nr == 0 && lhs_nc == 0)
        {
          if (idx_i_is_colon)
            n = rhs_nr;

          if (idx_j_is_colon)
            m = rhs_nc;
        }

      if (idx_i && idx_j)
        {
          if (rhs_nr == 1 && rhs_nc == 1 && n >= 0 && m >= 0)
            {
              if (n > 0 && m > 0)
                {
                  idx_i.sort (true);
                  n = idx_i.length (n);
                  idx_j.sort (true);
                  m = idx_j.length (m);

                  octave_idx_type max_row_idx = idx_i_is_colon ? rhs_nr : 
                    idx_i.max () + 1;
                  octave_idx_type max_col_idx = idx_j_is_colon ? rhs_nc : 
                    idx_j.max () + 1;
                  octave_idx_type new_nr = max_row_idx > lhs_nr ? 
                    max_row_idx : lhs_nr;
                  octave_idx_type new_nc = max_col_idx > lhs_nc ? 
                    max_col_idx : lhs_nc;
                  RT scalar = rhs.elem (0, 0);

                  // Count the number of non-zero terms
                  octave_idx_type new_nzmx = lhs.nnz ();
                  for (octave_idx_type j = 0; j < m; j++)
                    {
                      octave_idx_type jj = idx_j.elem (j);
                      if (jj < lhs_nc)
                        {
                          for (octave_idx_type i = 0; i < n; i++)
                            {
                              octave_quit ();

                              octave_idx_type ii = idx_i.elem (i);

                              if (ii < lhs_nr)
                                {
                                  for (octave_idx_type k = c_lhs.cidx(jj); 
                                       k < c_lhs.cidx(jj+1); k++)
                                    {
                                      if (c_lhs.ridx(k) == ii)
                                        new_nzmx--;
                                      if (c_lhs.ridx(k) >= ii)
                                        break;
                                    }
                                }
                            }
                        }
                    }

                  if (scalar != RT())
                    new_nzmx += m * n;

                  Sparse<LT> stmp (new_nr, new_nc, new_nzmx);

                  octave_idx_type jji = 0;
                  octave_idx_type jj = idx_j.elem (jji);
                  octave_idx_type kk = 0;
                  stmp.cidx(0) = 0;
                  for (octave_idx_type j = 0; j < new_nc; j++)
                    {
                      if (jji < m && jj == j)
                        {
                          octave_idx_type iii = 0;
                          octave_idx_type ii = idx_i.elem (iii);
                          octave_idx_type ppp = 0;
                          octave_idx_type ppi = (j >= lhs_nc ? 0 : 
                                                 c_lhs.cidx(j+1) - 
                                                 c_lhs.cidx(j));
                          octave_idx_type pp = (ppp < ppi ? 
                                                c_lhs.ridx(c_lhs.cidx(j)+ppp) :
                                                new_nr);
                          while (ppp < ppi || iii < n)
                            {
                              if (iii < n && ii <= pp)
                                {
                                  if (scalar != RT ())
                                    {
                                      stmp.data(kk) = scalar;
                                      stmp.ridx(kk++) = ii;
                                    }
                                  if (ii == pp)
                                    pp = (++ppp < ppi ? c_lhs.ridx(c_lhs.cidx(j)+ppp) : new_nr);                                        
                                  if (++iii < n)
                                    ii = idx_i.elem(iii);
                                }
                              else
                                {
                                  stmp.data(kk) = 
                                    c_lhs.data(c_lhs.cidx(j)+ppp);
                                  stmp.ridx(kk++) = pp;
                                  pp = (++ppp < ppi ? c_lhs.ridx(c_lhs.cidx(j)+ppp) : new_nr);
                                }
                            }
                          if (++jji < m)
                            jj = idx_j.elem(jji);
                        }
                      else if (j < lhs_nc) 
                        {
                          for (octave_idx_type i = c_lhs.cidx(j); 
                               i < c_lhs.cidx(j+1); i++)
                            {
                              stmp.data(kk) = c_lhs.data(i);
                              stmp.ridx(kk++) = c_lhs.ridx(i);
                            }
                        }
                      stmp.cidx(j+1) = kk;
                    }

                  lhs = stmp;
                }
              else
                {
#if 0
                  // FIXME -- the following code will make this
                  // function behave the same as the full matrix
                  // case for things like
                  //
                  // x = sparse (ones (2));
                  // x([],3) = 2;
                  //
                  // x =
                  //
                  // Compressed Column Sparse (rows = 2, cols = 3, nnz = 4)
                  //
                  // (1, 1) ->  1
                  // (2, 1) ->  1
                  // (1, 2) ->  1
                  // (2, 2) ->  1
                  //
                  // However, Matlab doesn't resize in this case
                  // even though it does in the full matrix case.

                  if (n > 0)
                    {
                      octave_idx_type max_row_idx = idx_i_is_colon ? 
                        rhs_nr : idx_i.max () + 1;
                      octave_idx_type new_nr = max_row_idx > lhs_nr ? 
                        max_row_idx : lhs_nr;
                      octave_idx_type new_nc = lhs_nc;

                      lhs.resize (new_nr, new_nc);
                    }
                  else if (m > 0)
                    {
                      octave_idx_type max_col_idx = idx_j_is_colon ? 
                        rhs_nc : idx_j.max () + 1;
                      octave_idx_type new_nr = lhs_nr;
                      octave_idx_type new_nc = max_col_idx > lhs_nc ? 
                        max_col_idx : lhs_nc;

                      lhs.resize  (new_nr, new_nc);
                    }
#endif
                }
            }
          else if (n == rhs_nr && m == rhs_nc)
            {
              if (n > 0 && m > 0)
                {
                  octave_idx_type max_row_idx = idx_i_is_colon ? rhs_nr : 
                    idx_i.max () + 1;
                  octave_idx_type max_col_idx = idx_j_is_colon ? rhs_nc : 
                    idx_j.max () + 1;
                  octave_idx_type new_nr = max_row_idx > lhs_nr ?
                    max_row_idx : lhs_nr;
                  octave_idx_type new_nc = max_col_idx > lhs_nc ? 
                    max_col_idx : lhs_nc;

                  OCTAVE_LOCAL_BUFFER (octave_idx_type, rhs_idx_i, n);
                  if (! idx_i.is_colon ())
                    {
                      // Ok here we have to be careful with the indexing,
                      // to treat cases like "a([3,2,1],:) = b", and still 
                      // handle the need for strict sorting of the sparse 
                      // elements.
                      OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort *,
                                           sidx, n);
                      OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort,
                                           sidxX, n);

                      for (octave_idx_type i = 0; i < n; i++)
                        {
                          sidx[i] = &sidxX[i];
                          sidx[i]->i = idx_i.elem(i);
                          sidx[i]->idx = i;
                        }

                      octave_quit ();
                      octave_sort<octave_idx_vector_sort *> 
                        sort (octave_idx_vector_comp);

                      sort.sort (sidx, n);

                      intNDArray<octave_idx_type> new_idx (dim_vector (n,1));

                      for (octave_idx_type i = 0; i < n; i++)
                        {
                          new_idx.xelem(i) = sidx[i]->i;
                          rhs_idx_i[i] = sidx[i]->idx;
                        }

                      idx_i = idx_vector (new_idx);
                    }
                  else
                    for (octave_idx_type i = 0; i < n; i++)
                      rhs_idx_i[i] = i;

                  OCTAVE_LOCAL_BUFFER (octave_idx_type, rhs_idx_j, m);
                  if (! idx_j.is_colon ())
                    {
                      // Ok here we have to be careful with the indexing,
                      // to treat cases like "a([3,2,1],:) = b", and still 
                      // handle the need for strict sorting of the sparse 
                      // elements.
                      OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort *,
                                           sidx, m);
                      OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort,
                                           sidxX, m);

                      for (octave_idx_type i = 0; i < m; i++)
                        {
                          sidx[i] = &sidxX[i];
                          sidx[i]->i = idx_j.elem(i);
                          sidx[i]->idx = i;
                        }

                      octave_quit ();
                      octave_sort<octave_idx_vector_sort *> 
                        sort (octave_idx_vector_comp);

                      sort.sort (sidx, m);

                      intNDArray<octave_idx_type> new_idx (dim_vector (m,1));

                      for (octave_idx_type i = 0; i < m; i++)
                        {
                          new_idx.xelem(i) = sidx[i]->i;
                          rhs_idx_j[i] = sidx[i]->idx;
                        }

                      idx_j = idx_vector (new_idx);
                    }
                  else
                    for (octave_idx_type i = 0; i < m; i++)
                      rhs_idx_j[i] = i;

                  // Maximum number of non-zero elements
                  octave_idx_type new_nzmx = lhs.nnz() + rhs.nnz();

                  Sparse<LT> stmp (new_nr, new_nc, new_nzmx);

                  octave_idx_type jji = 0;
                  octave_idx_type jj = idx_j.elem (jji);
                  octave_idx_type kk = 0;
                  stmp.cidx(0) = 0;
                  for (octave_idx_type j = 0; j < new_nc; j++)
                    {
                      if (jji < m && jj == j)
                        {
                          octave_idx_type iii = 0;
                          octave_idx_type ii = idx_i.elem (iii);
                          octave_idx_type ppp = 0;
                          octave_idx_type ppi = (j >= lhs_nc ? 0 : 
                                                 c_lhs.cidx(j+1) - 
                                                 c_lhs.cidx(j));
                          octave_idx_type pp = (ppp < ppi ? 
                                                c_lhs.ridx(c_lhs.cidx(j)+ppp) :
                                                new_nr);
                          while (ppp < ppi || iii < n)
                            {
                              if (iii < n && ii <= pp)
                                {
                                  if (iii < n - 1 && 
                                      idx_i.elem (iii + 1) == ii)
                                    {
                                      iii++;
                                      ii = idx_i.elem(iii);
                                      continue;
                                    }

                                  RT rtmp = rhs.elem (rhs_idx_i[iii], 
                                                      rhs_idx_j[jji]);
                                  if (rtmp != RT ())
                                    {
                                      stmp.data(kk) = rtmp;
                                      stmp.ridx(kk++) = ii;
                                    }
                                  if (ii == pp)
                                    pp = (++ppp < ppi ? c_lhs.ridx(c_lhs.cidx(j)+ppp) : new_nr);                                        
                                  if (++iii < n)
                                    ii = idx_i.elem(iii);
                                }
                              else
                                {
                                  stmp.data(kk) = 
                                    c_lhs.data(c_lhs.cidx(j)+ppp);
                                  stmp.ridx(kk++) = pp;
                                  pp = (++ppp < ppi ? c_lhs.ridx(c_lhs.cidx(j)+ppp) : new_nr);
                                }
                            }
                          if (++jji < m)
                            jj = idx_j.elem(jji);
                        }
                      else if (j < lhs_nc) 
                        {
                          for (octave_idx_type i = c_lhs.cidx(j); 
                               i < c_lhs.cidx(j+1); i++)
                            {
                              stmp.data(kk) = c_lhs.data(i);
                              stmp.ridx(kk++) = c_lhs.ridx(i);
                            }
                        }
                      stmp.cidx(j+1) = kk;
                    }

                  stmp.maybe_compress();
                  lhs = stmp;
                }
            }
          else if (n == 0 && m == 0)
            {
              if (! ((rhs_nr == 1 && rhs_nc == 1)
                     || (rhs_nr == 0 || rhs_nc == 0)))
                {
                  (*current_liboctave_error_handler)
                    ("A([], []) = X: X must be an empty matrix or a scalar");

                  retval = 0;
                }
            }
          else
            {
              (*current_liboctave_error_handler)
                ("A(I, J) = X: X must be a scalar or the number of elements in I must");
              (*current_liboctave_error_handler)
                ("match the number of rows in X and the number of elements in J must");
              (*current_liboctave_error_handler)
                ("match the number of columns in X");

              retval = 0;
            }
        }
      // idx_vector::freeze() printed an error message for us.
    }
  else if (n_idx == 1)
    {
      int lhs_is_empty = lhs_nr == 0 || lhs_nc == 0;

      if (lhs_is_empty || (lhs_nr == 1 && lhs_nc == 1))
        {
          octave_idx_type lhs_len = lhs.length ();

          // Called for side-effects on idx_i.
          idx_i.freeze (lhs_len, 0, true);

          if (idx_i)
            {
              if (lhs_is_empty
                  && idx_i.is_colon ()
                  && ! (rhs_nr == 1 || rhs_nc == 1))
                {
                  (*current_liboctave_warning_with_id_handler)
                    ("Octave:fortran-indexing",
                     "A(:) = X: X is not a vector or scalar");
                }
              else
                {
                  octave_idx_type idx_nr = idx_i.orig_rows ();
                  octave_idx_type idx_nc = idx_i.orig_columns ();

                  if (! (rhs_nr == idx_nr && rhs_nc == idx_nc))
                    (*current_liboctave_warning_with_id_handler)
                      ("Octave:fortran-indexing",
                       "A(I) = X: X does not have same shape as I");
                }

              if (! assign1 (lhs, rhs))
                retval = 0;
            }
          // idx_vector::freeze() printed an error message for us.
        }
      else if (lhs_nr == 1)
        {
          idx_i.freeze (lhs_nc, "vector", true);

          if (idx_i)
            {
              if (! assign1 (lhs, rhs))
                retval = 0;
            }
          // idx_vector::freeze() printed an error message for us.
        }
      else if (lhs_nc == 1)
        {
          idx_i.freeze (lhs_nr, "vector", true);

          if (idx_i)
            {
              if (! assign1 (lhs, rhs))
                retval = 0;
            }
          // idx_vector::freeze() printed an error message for us.
        }
      else
        {
          if (! idx_i.is_colon ())
            (*current_liboctave_warning_with_id_handler)
              ("Octave:fortran-indexing", "single index used for matrix");

          octave_idx_type lhs_len = lhs.length ();

          octave_idx_type len = idx_i.freeze (lhs_nr * lhs_nc, "matrix");

          if (idx_i)
            {
              if (len == 0)
                {
                  if (! ((rhs_nr == 1 && rhs_nc == 1)
                         || (rhs_nr == 0 || rhs_nc == 0)))
                    (*current_liboctave_error_handler)
                      ("A([]) = X: X must be an empty matrix or scalar");
                }
              else if (len == rhs_nr * rhs_nc)
                {
                  octave_idx_type new_nzmx = lhs_nz;
                  OCTAVE_LOCAL_BUFFER (octave_idx_type, rhs_idx, len);
                  
                  if (! idx_i.is_colon ())
                    {
                      // Ok here we have to be careful with the indexing, to
                      // treat cases like "a([3,2,1]) = b", and still handle
                      // the need for strict sorting of the sparse elements.

                      OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort *, sidx, 
                                           len);
                      OCTAVE_LOCAL_BUFFER (octave_idx_vector_sort, sidxX, 
                                           len);

                      for (octave_idx_type i = 0; i < len; i++)
                        {
                          sidx[i] = &sidxX[i];
                          sidx[i]->i = idx_i.elem(i);
                          sidx[i]->idx = i;
                        }

                      octave_quit ();
                      octave_sort<octave_idx_vector_sort *> 
                        sort (octave_idx_vector_comp);

                      sort.sort (sidx, len);

                      intNDArray<octave_idx_type> new_idx (dim_vector (len,1));

                      for (octave_idx_type i = 0; i < len; i++)
                        {
                          new_idx.xelem(i) = sidx[i]->i;
                          rhs_idx[i] = sidx[i]->idx;
                        }

                      idx_i = idx_vector (new_idx);
                    }
                  else
                    for (octave_idx_type i = 0; i < len; i++)
                      rhs_idx[i] = i;

                  // First count the number of non-zero elements
                  for (octave_idx_type i = 0; i < len; i++)
                    {
                      octave_quit ();
                      
                      octave_idx_type ii = idx_i.elem (i);
                      if (i < len - 1 && idx_i.elem (i + 1) == ii)
                        continue;
                      if (ii < lhs_len && c_lhs.elem(ii) != LT ())
                        new_nzmx--;
                      if (rhs.elem(rhs_idx[i]) != RT ())
                        new_nzmx++;
                    }

                  Sparse<LT> stmp (lhs_nr, lhs_nc, new_nzmx);

                  octave_idx_type i = 0;
                  octave_idx_type ii = 0;
                  octave_idx_type ic = 0;
                  if (i < lhs_nz)
                    {
                      while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
                        ic++;
                      ii = ic * lhs_nr + c_lhs.ridx(i);
                    }

                  octave_idx_type j = 0;
                  octave_idx_type jj = idx_i.elem (j);
                  octave_idx_type jr = jj % lhs_nr;
                  octave_idx_type jc = (jj - jr) / lhs_nr;

                  octave_idx_type kk = 0;
                  octave_idx_type kc = 0;

                  while (j < len || i < lhs_nz)
                    {
                      if (j < len - 1 && idx_i.elem (j + 1) == jj)
                        {
                          j++;
                          jj = idx_i.elem (j);
                          jr = jj % lhs_nr;
                          jc = (jj - jr) / lhs_nr;
                          continue;
                        }

                      if (j == len || (i < lhs_nz && ii < jj))
                        {
                          while (kc <= ic)
                            stmp.xcidx (kc++) = kk;
                          stmp.xdata (kk) = c_lhs.data (i);
                          stmp.xridx (kk++) = c_lhs.ridx (i);
                          i++;
                          while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
                            ic++;
                          if (i < lhs_nz)
                            ii = ic * lhs_nr + c_lhs.ridx(i);
                        }
                      else
                        {
                          while (kc <= jc)
                            stmp.xcidx (kc++) = kk;
                          RT rtmp = rhs.elem (rhs_idx[j]);
                          if (rtmp != RT ())
                            {
                              stmp.xdata (kk) = rtmp;
                              stmp.xridx (kk++) = jr;
                            }
                          if (ii == jj)
                            {
                              i++;
                              while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
                                ic++;
                              if (i < lhs_nz)
                                ii = ic * lhs_nr + c_lhs.ridx(i);
                            }
                          j++;
                          if (j < len)
                            {
                              jj = idx_i.elem (j);
                              jr = jj % lhs_nr;
                              jc = (jj - jr) / lhs_nr;
                            }
                        }
                    }

                  for (octave_idx_type iidx = kc; iidx < lhs_nc+1; iidx++)
                    stmp.xcidx(iidx) = kk; 

                  lhs = stmp;
                }
              else if (rhs_nr == 1 && rhs_nc == 1)
                {
                  RT scalar = rhs.elem (0, 0);
                  octave_idx_type new_nzmx = lhs_nz;
                  idx_i.sort (true);
                  len = idx_i.length (len);

                  // First count the number of non-zero elements
                  if (scalar != RT ())
                    new_nzmx += len;
                  for (octave_idx_type i = 0; i < len; i++)
                    {
                      octave_quit ();
                      octave_idx_type ii = idx_i.elem (i);
                      if (ii < lhs_len && c_lhs.elem(ii) != LT ())
                        new_nzmx--;
                    }

                  Sparse<LT> stmp (lhs_nr, lhs_nc, new_nzmx);

                  octave_idx_type i = 0;
                  octave_idx_type ii = 0;
                  octave_idx_type ic = 0;
                  if (i < lhs_nz)
                    {
                      while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
                        ic++;
                      ii = ic * lhs_nr + c_lhs.ridx(i);
                    }

                  octave_idx_type j = 0;
                  octave_idx_type jj = idx_i.elem (j);
                  octave_idx_type jr = jj % lhs_nr;
                  octave_idx_type jc = (jj - jr) / lhs_nr;

                  octave_idx_type kk = 0;
                  octave_idx_type kc = 0;

                  while (j < len || i < lhs_nz)
                    {
                      if (j == len || (i < lhs_nz && ii < jj))
                        {
                          while (kc <= ic)
                            stmp.xcidx (kc++) = kk;
                          stmp.xdata (kk) = c_lhs.data (i);
                          stmp.xridx (kk++) = c_lhs.ridx (i);
                          i++;
                          while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
                            ic++;
                          if (i < lhs_nz)
                            ii = ic * lhs_nr + c_lhs.ridx(i);
                        }
                      else
                        {
                          while (kc <= jc)
                            stmp.xcidx (kc++) = kk;
                          if (scalar != RT ())
                            {
                              stmp.xdata (kk) = scalar;
                              stmp.xridx (kk++) = jr;
                            }
                          if (ii == jj)
                            {
                              i++;
                              while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
                                ic++;
                              if (i < lhs_nz)
                                ii = ic * lhs_nr + c_lhs.ridx(i);
                            }
                          j++;
                          if (j < len)
                            {
                              jj = idx_i.elem (j);
                              jr = jj % lhs_nr;
                              jc = (jj - jr) / lhs_nr;
                            }
                        }
                    }

                  for (octave_idx_type iidx = kc; iidx < lhs_nc+1; iidx++)
                    stmp.xcidx(iidx) = kk;
                  
                  lhs = stmp;
                }
              else
                {
                  (*current_liboctave_error_handler)
      ("A(I) = X: X must be a scalar or a matrix with the same size as I");

                  retval = 0;
                }
            }
          // idx_vector::freeze() printed an error message for us.
        }
    }
  else
    {
      (*current_liboctave_error_handler)
        ("invalid number of indices for matrix expression");

      retval = 0;
    }

  lhs.clear_index ();

  return retval;
}

/*
 * Tests
 *

%!function x = set_slice(x, dim, slice, arg)
%!  switch dim
%!    case 11
%!      x(slice) = 2;
%!    case 21
%!      x(slice, :) = 2;
%!    case 22
%!      x(:, slice) = 2;
%!    otherwise
%!      error("invalid dim, '%d'", dim);
%!  endswitch
%! endfunction

%!function x = set_slice2(x, dim, slice)
%!  switch dim
%!    case 11
%!      x(slice) = 2 * ones (size(slice));
%!    case 21
%!      x(slice, :) = 2 * ones (length(slice), columns (x));
%!    case 22
%!      x(:, slice) = 2 * ones (rows (x), length(slice));
%!    otherwise
%!      error("invalid dim, '%d'", dim);
%!  endswitch
%! endfunction

%!function test_sparse_slice(size, dim, slice)
%!  x = ones(size);
%!  s = set_slice(sparse(x), dim, slice);
%!  f = set_slice(x, dim, slice);
%!  assert (nnz(s), nnz(f));
%!  assert(full(s), f);
%!  s = set_slice2(sparse(x), dim, slice);
%!  f = set_slice2(x, dim, slice);
%!  assert (nnz(s), nnz(f));
%!  assert(full(s), f);
%! endfunction

#### 1d indexing

## size = [2 0]
%!test test_sparse_slice([2 0], 11, []);
%!assert(set_slice(sparse(ones([2 0])), 11, 1), sparse([2 0]'));  # sparse different from full
%!assert(set_slice(sparse(ones([2 0])), 11, 2), sparse([0 2]'));  # sparse different from full
%!assert(set_slice(sparse(ones([2 0])), 11, 3), sparse([0 0 2]'));  # sparse different from full
%!assert(set_slice(sparse(ones([2 0])), 11, 4), sparse([0 0 0 2]'));  # sparse different from full

## size = [0 2]
%!test test_sparse_slice([0 2], 11, []);
%!assert(set_slice(sparse(ones([0 2])), 11, 1), sparse(1,2));  # sparse different from full
%!test test_sparse_slice([0 2], 11, 2);
%!test test_sparse_slice([0 2], 11, 3);
%!test test_sparse_slice([0 2], 11, 4);
%!test test_sparse_slice([0 2], 11, [4, 4]);

## size = [2 1]
%!test test_sparse_slice([2 1], 11, []);
%!test test_sparse_slice([2 1], 11, 1);
%!test test_sparse_slice([2 1], 11, 2);
%!test test_sparse_slice([2 1], 11, 3);
%!test test_sparse_slice([2 1], 11, 4);
%!test test_sparse_slice([2 1], 11, [4, 4]);

## size = [1 2]
%!test test_sparse_slice([1 2], 11, []);
%!test test_sparse_slice([1 2], 11, 1);
%!test test_sparse_slice([1 2], 11, 2);
%!test test_sparse_slice([1 2], 11, 3);
%!test test_sparse_slice([1 2], 11, 4);
%!test test_sparse_slice([1 2], 11, [4, 4]);

## size = [2 2]
%!test test_sparse_slice([2 2], 11, []);
%!test test_sparse_slice([2 2], 11, 1);
%!test test_sparse_slice([2 2], 11, 2);
%!test test_sparse_slice([2 2], 11, 3);
%!test test_sparse_slice([2 2], 11, 4);
%!test test_sparse_slice([2 2], 11, [4, 4]);
# These 2 errors are the same as in the full case
%!error <invalid matrix index = 5> set_slice(sparse(ones([2 2])), 11, 5);
%!error <invalid matrix index = 6> set_slice(sparse(ones([2 2])), 11, 6);


#### 2d indexing

## size = [2 0]
%!test test_sparse_slice([2 0], 21, []);
%!test test_sparse_slice([2 0], 21, 1);
%!test test_sparse_slice([2 0], 21, 2);
%!test test_sparse_slice([2 0], 21, [2,2]);
%!assert(set_slice(sparse(ones([2 0])), 21, 3), sparse(2,0));  # sparse different from full
%!assert(set_slice(sparse(ones([2 0])), 21, 4), sparse(2,0));  # sparse different from full
%!test test_sparse_slice([2 0], 22, []);
%!test test_sparse_slice([2 0], 22, 1);
%!test test_sparse_slice([2 0], 22, 2);
%!test test_sparse_slice([2 0], 22, [2,2]);
%!assert(set_slice(sparse(ones([2 0])), 22, 3), sparse([0 0 2;0 0 2]));  # sparse different from full
%!assert(set_slice(sparse(ones([2 0])), 22, 4), sparse([0 0 0 2;0 0 0 2]));  # sparse different from full

## size = [0 2]
%!test test_sparse_slice([0 2], 21, []);
%!test test_sparse_slice([0 2], 21, 1);
%!test test_sparse_slice([0 2], 21, 2);
%!test test_sparse_slice([0 2], 21, [2,2]);
%!assert(set_slice(sparse(ones([0 2])), 21, 3), sparse([0 0;0 0;2 2]));  # sparse different from full
%!assert(set_slice(sparse(ones([0 2])), 21, 4), sparse([0 0;0 0;0 0;2 2]));  # sparse different from full
%!test test_sparse_slice([0 2], 22, []);
%!test test_sparse_slice([0 2], 22, 1);
%!test test_sparse_slice([0 2], 22, 2);
%!test test_sparse_slice([0 2], 22, [2,2]);
%!assert(set_slice(sparse(ones([0 2])), 22, 3), sparse(0,2));  # sparse different from full
%!assert(set_slice(sparse(ones([0 2])), 22, 4), sparse(0,2));  # sparse different from full

## size = [2 1]
%!test test_sparse_slice([2 1], 21, []);
%!test test_sparse_slice([2 1], 21, 1);
%!test test_sparse_slice([2 1], 21, 2);
%!test test_sparse_slice([2 1], 21, [2,2]);
%!test test_sparse_slice([2 1], 21, 3);
%!test test_sparse_slice([2 1], 21, 4);
%!test test_sparse_slice([2 1], 22, []);
%!test test_sparse_slice([2 1], 22, 1);
%!test test_sparse_slice([2 1], 22, 2);
%!test test_sparse_slice([2 1], 22, [2,2]);
%!test test_sparse_slice([2 1], 22, 3);
%!test test_sparse_slice([2 1], 22, 4);

## size = [1 2]
%!test test_sparse_slice([1 2], 21, []);
%!test test_sparse_slice([1 2], 21, 1);
%!test test_sparse_slice([1 2], 21, 2);
%!test test_sparse_slice([1 2], 21, [2,2]);
%!test test_sparse_slice([1 2], 21, 3);
%!test test_sparse_slice([1 2], 21, 4);
%!test test_sparse_slice([1 2], 22, []);
%!test test_sparse_slice([1 2], 22, 1);
%!test test_sparse_slice([1 2], 22, 2);
%!test test_sparse_slice([1 2], 22, [2,2]);
%!test test_sparse_slice([1 2], 22, 3);
%!test test_sparse_slice([1 2], 22, 4);

## size = [2 2]
%!test test_sparse_slice([2 2], 21, []);
%!test test_sparse_slice([2 2], 21, 1);
%!test test_sparse_slice([2 2], 21, 2);
%!test test_sparse_slice([2 2], 21, [2,2]);
%!test test_sparse_slice([2 2], 21, 3);
%!test test_sparse_slice([2 2], 21, 4);
%!test test_sparse_slice([2 2], 22, []);
%!test test_sparse_slice([2 2], 22, 1);
%!test test_sparse_slice([2 2], 22, 2);
%!test test_sparse_slice([2 2], 22, [2,2]);
%!test test_sparse_slice([2 2], 22, 3);
%!test test_sparse_slice([2 2], 22, 4);

*/

template <class T>
void
Sparse<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << "\n"
     << prefix << "rep->nzmx:   " << rep->nzmx  << "\n"
     << prefix << "rep->nrows:  " << rep->nrows << "\n"
     << prefix << "rep->ncols:  " << rep->ncols << "\n"
     << prefix << "rep->data:   " << static_cast<void *> (rep->d) << "\n"
     << prefix << "rep->ridx:   " << static_cast<void *> (rep->r) << "\n"
     << prefix << "rep->cidx:   " << static_cast<void *> (rep->c) << "\n"
     << prefix << "rep->count:  " << rep->count << "\n";
}
