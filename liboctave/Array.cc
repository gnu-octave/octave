// Template array classes
/*

Copyright (C) 2008, 2009 Jaroslav Hajek
Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2003, 2004,
              2005, 2006, 2007 John W. Eaton 

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

#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <new>

#include "Array.h"
#include "Array-util.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "oct-locbuf.h"

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
void
Array<T>::make_unique (void)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new ArrayRep (slice_data, slice_len, true);
      slice_data = rep->data;
    }
}

template <class T>
Array<T>::Array (const Array<T>& a, const dim_vector& dv)
  : rep (a.rep), dimensions (dv), 
    slice_data (a.slice_data), slice_len (a.slice_len)
{
  rep->count++;

  if (a.numel () < dv.numel ())
    (*current_liboctave_error_handler)
      ("Array::Array (const Array&, const dim_vector&): dimension mismatch");
}

template <class T>
Array<T>::~Array (void)
{
  if (--rep->count <= 0)
    delete rep;
}

template <class T>
Array<T>&
Array<T>::operator = (const Array<T>& a)
{
  if (this != &a)
    {
      if (--rep->count <= 0)
	delete rep;

      rep = a.rep;
      rep->count++;

      dimensions = a.dimensions;
      slice_data = a.slice_data;
      slice_len = a.slice_len;
    }

  return *this;
}

template <class T>
void
Array<T>::fill (const T& val)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new ArrayRep (length (), val);
      slice_data = rep->data;
    }
  else
    std::fill (slice_data, slice_data + slice_len, val);
}

template <class T>
Array<T>
Array<T>::squeeze (void) const
{
  Array<T> retval = *this;

  if (ndims () > 2)
    {
      bool dims_changed = false;

      dim_vector new_dimensions = dimensions;

      int k = 0;

      for (int i = 0; i < ndims (); i++)
	{
	  if (dimensions(i) == 1)
	    dims_changed = true;
	  else
	    new_dimensions(k++) = dimensions(i);
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

      retval = Array<T> (*this, new_dimensions);
    }

  return retval;
}

// KLUGE

// The following get_size functions will throw a std::bad_alloc ()
// exception if the requested size is larger than can be indexed by
// octave_idx_type.  This may be smaller than the actual amount of
// memory that can be safely allocated on a system.  However, if we
// don't fail here, we can end up with a mysterious crash inside a
// function that is iterating over an array using octave_idx_type
// indices.

// A guess (should be quite conservative).
#define MALLOC_OVERHEAD 1024

template <class T>
octave_idx_type
Array<T>::get_size (octave_idx_type r, octave_idx_type c)
{
  static int nl;
  static double dl
    = frexp (static_cast<double> 
	(std::numeric_limits<octave_idx_type>::max() - MALLOC_OVERHEAD) / sizeof (T), &nl);

  int nr, nc;
  double dr = frexp (static_cast<double> (r), &nr);   // r = dr * 2^nr
  double dc = frexp (static_cast<double> (c), &nc);   // c = dc * 2^nc

  int nt = nr + nc;
  double dt = dr * dc;

  if (dt < 0.5)
    {
      nt--;
      dt *= 2;
    }

  if (nt < nl || (nt == nl && dt < dl))
    return r * c;
  else
    {
      throw std::bad_alloc ();
      return 0;
    }
}

template <class T>
octave_idx_type
Array<T>::get_size (octave_idx_type r, octave_idx_type c, octave_idx_type p)
{
  static int nl;
  static double dl
    = frexp (static_cast<double>
	(std::numeric_limits<octave_idx_type>::max() - MALLOC_OVERHEAD) / sizeof (T), &nl);

  int nr, nc, np;
  double dr = frexp (static_cast<double> (r), &nr);
  double dc = frexp (static_cast<double> (c), &nc);
  double dp = frexp (static_cast<double> (p), &np);

  int nt = nr + nc + np;
  double dt = dr * dc * dp;

  if (dt < 0.5)
    {
      nt--;
      dt *= 2;

      if (dt < 0.5)
	{
	  nt--;
	  dt *= 2;
	}
    }

  if (nt < nl || (nt == nl && dt < dl))
    return r * c * p;
  else
    {
      throw std::bad_alloc ();
      return 0;
    }
}

template <class T>
octave_idx_type
Array<T>::get_size (const dim_vector& ra_idx)
{
  static int nl;
  static double dl
    = frexp (static_cast<double>
	(std::numeric_limits<octave_idx_type>::max() - MALLOC_OVERHEAD) / sizeof (T), &nl);

  int n = ra_idx.length ();

  int nt = 0;
  double dt = 1;

  for (int i = 0; i < n; i++)
    {
      int nra_idx;
      double dra_idx = frexp (static_cast<double> (ra_idx(i)), &nra_idx);

      nt += nra_idx;
      dt *= dra_idx;

      if (dt < 0.5)
	{
	  nt--;
	  dt *= 2;
	}
    }

  if (nt < nl || (nt == nl && dt < dl))
    {
      octave_idx_type retval = 1;

      for (int i = 0; i < n; i++)
	retval *= ra_idx(i);

      return retval;
    }
  else
    {
      throw std::bad_alloc ();
      return 0;
    }
}

#undef MALLOC_OVERHEAD

template <class T>
octave_idx_type
Array<T>::compute_index (const Array<octave_idx_type>& ra_idx) const
{
  octave_idx_type retval = -1;

  int n = dimensions.length ();

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
      ("Array<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

template <class T>
T
Array<T>::range_error (const char *fcn, octave_idx_type n) const
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, octave_idx_type n)
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  static T foo;
  return foo;
}

template <class T>
T
Array<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  static T foo;
  return foo;
}

template <class T>
T
Array<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j, octave_idx_type k) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d, %d): range error", fcn, i, j, k);
  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j, octave_idx_type k)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d, %d): range error", fcn, i, j, k);
  static T foo;
  return foo;
}

template <class T>
T
Array<T>::range_error (const char *fcn, const Array<octave_idx_type>& ra_idx) const
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
Array<T>::range_error (const char *fcn, const Array<octave_idx_type>& ra_idx)
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
Array<T>
Array<T>::reshape (const dim_vector& new_dims) const
{
  Array<T> retval;

  if (dimensions != new_dims)
    {
      if (dimensions.numel () == new_dims.numel ())
	retval = Array<T> (*this, new_dims);
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

// Helper class for multi-d dimension permuting (generalized transpose).
class rec_permute_helper
{
  octave_idx_type *dim, *stride;
  bool use_blk;
  int top;

public:
  rec_permute_helper (const dim_vector& dv, const Array<octave_idx_type>& perm)
    {
      int n = dv.length ();
      assert (n == perm.length ());

      dim = new octave_idx_type [2*n];
      // A hack to avoid double allocation
      stride = dim + n;

      // Get cumulative dimensions.
      OCTAVE_LOCAL_BUFFER (octave_idx_type, cdim, n+1);
      cdim[0] = 1;
      for (int i = 1; i < n+1; i++) cdim[i] = cdim[i-1] * dv(i-1);

      // Setup the permuted strides.
      for (int k = 0; k < n; k++)
        {
          int kk = perm(k);
          dim[k] = dv(kk);
          stride[k] = cdim[kk];
        }

      // Reduce contiguous runs.
      top = 0;
      for (int k = 1; k < n; k++)
        {
          if (stride[k] == stride[top]*dim[top])
            dim[top] *= dim[k];
          else
            {
              top++;
              dim[top] = dim[k];
              stride[top] = stride[k];
            }
        }

      // Determine whether we can use block transposes.
      use_blk = top >= 1 && stride[1] == 1 && stride[0] == dim[1];

    }

  ~rec_permute_helper (void) { delete [] dim; }

  // Helper method for fast blocked transpose.
  template <class T>
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

  // Recursive N-d generalized transpose
  template <class T>
  T *do_permute (const T *src, T *dest, int lev) const
    {
      if (lev == 0)
        {
          octave_idx_type step = stride[0], len = dim[0];
          if (step == 1)
            dest = std::copy (src, src + len, dest);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                dest[i] = src[j];

              dest += len;
            }
        }
      else if (use_blk && lev == 1)
        dest = blk_trans (src, dest, dim[1], dim[0]);
      else
        {
          octave_idx_type step = stride[lev], len = dim[lev];
          for (octave_idx_type i = 0, j = 0; i < len; i++, j+= step)
           dest = do_permute (src + i * step, dest, lev-1);
        }

      return dest;
    }
  
public:

  template <class T>
  void permute (const T *src, T *dest) const { do_permute (src, dest, top); }

};


template <class T>
Array<T>
Array<T>::permute (const Array<octave_idx_type>& perm_vec_arg, bool inv) const
{
  Array<T> retval;

  Array<octave_idx_type> perm_vec = perm_vec_arg;

  dim_vector dv = dims ();
  dim_vector dv_new;

  int perm_vec_len = perm_vec_arg.length ();

  if (perm_vec_len < dv.length ())
    (*current_liboctave_error_handler)
      ("%s: invalid permutation vector", inv ? "ipermute" : "permute");

  dv_new.resize (perm_vec_len);

  // Append singleton dimensions as needed.
  dv.resize (perm_vec_len, 1);

  // Need this array to check for identical elements in permutation array.
  OCTAVE_LOCAL_BUFFER_INIT (bool, checked, perm_vec_len, false);

  // Find dimension vector of permuted array.
  for (int i = 0; i < perm_vec_len; i++)
    {
      octave_idx_type perm_elt = perm_vec.elem (i);
      if (perm_elt >= perm_vec_len || perm_elt < 0)
	{
	  (*current_liboctave_error_handler)
	    ("%s: permutation vector contains an invalid element",
	     inv ? "ipermute" : "permute");

	  return retval;
	}

      if (checked[perm_elt])
	{
	  (*current_liboctave_error_handler)
	    ("%s: permutation vector cannot contain identical elements",
	     inv ? "ipermute" : "permute");

	  return retval;
	}
      else
	checked[perm_elt] = true;

      dv_new(i) = dv(perm_elt);
    }

  if (inv)
    {
      for (int i = 0; i < perm_vec_len; i++)
        perm_vec(perm_vec_arg(i)) = i;
    }

  retval = Array<T> (dv_new);

  if (numel () > 0)
    {
      rec_permute_helper rh (dv, perm_vec);
      rh.permute (data (), retval.fortran_vec ());
    }

  retval.chop_trailing_singletons ();

  return retval;
}

// Helper class for multi-d index reduction and recursive indexing/indexed assignment.
// Rationale: we could avoid recursion using a state machine instead. However, using
// recursion is much more amenable to possible parallelization in the future.
// Also, the recursion solution is cleaner and more understandable.
class rec_index_helper
{
  octave_idx_type *dim, *cdim;
  idx_vector *idx;
  int top;

public:
  rec_index_helper (const dim_vector& dv, const Array<idx_vector>& ia)
    {
      int n = ia.length ();
      assert (n > 0 && (dv.length () == std::max (n, 2)));

      dim = new octave_idx_type [2*n];
      // A hack to avoid double allocation
      cdim = dim + n;
      idx = new idx_vector [n];
      top = 0;

      dim[0] = dv(0);
      cdim[0] = 1;
      idx[0] = ia(0);

      for (int i = 1; i < n; i++)
        {
          // Try reduction...
          if (idx[top].maybe_reduce (dim[top], ia(i), dv(i)))
            {
              // Reduction successful, fold dimensions.
              dim[top] *= dv(i);
            }
          else
            {
              // Unsuccessful, store index & cumulative dim.
              top++;
              idx[top] = ia(i);
              dim[top] = dv(i);
              cdim[top] = cdim[top-1] * dim[top-1];
            } 
        }
    }

  ~rec_index_helper (void) { delete [] idx; delete [] dim; }

private:

  // Recursive N-d indexing
  template <class T>
  T *do_index (const T *src, T *dest, int lev) const
    {
      if (lev == 0)
        dest += idx[0].index (src, dim[0], dest);
      else
        {
          octave_idx_type n = idx[lev].length (dim[lev]), d = cdim[lev];
          for (octave_idx_type i = 0; i < n; i++)
            dest = do_index (src + d*idx[lev].xelem (i), dest, lev-1);
        }

      return dest;
    }
  
  // Recursive N-d indexed assignment
  template <class T>
  const T *do_assign (const T *src, T *dest, int lev) const
    {
      if (lev == 0)
        src += idx[0].assign (src, dim[0], dest);
      else
        {
          octave_idx_type n = idx[lev].length (dim[lev]), d = cdim[lev];
          for (octave_idx_type i = 0; i < n; i++)
            src = do_assign (src, dest + d*idx[lev].xelem (i), lev-1);
        }

      return src;
    }

  // Recursive N-d indexed assignment
  template <class T>
  void do_fill (const T& val, T *dest, int lev) const
    {
      if (lev == 0)
        idx[0].fill (val, dim[0], dest);
      else
        {
          octave_idx_type n = idx[lev].length (dim[lev]), d = cdim[lev];
          for (octave_idx_type i = 0; i < n; i++)
            do_fill (val, dest + d*idx[lev].xelem (i), lev-1);
        }
    }

public:

  template <class T>
  void index (const T *src, T *dest) const { do_index (src, dest, top); }

  template <class T>
  void assign (const T *src, T *dest) const { do_assign (src, dest, top); }

  template <class T>
  void fill (const T& val, T *dest) const { do_fill (val, dest, top); }

  bool is_cont_range (octave_idx_type& l, 
                            octave_idx_type& u) const
    {
      return top == 0 && idx[0].is_cont_range (dim[0], l, u);
    }
};

// Helper class for multi-d recursive resizing
// This handles resize () in an efficient manner, touching memory only
// once (apart from reinitialization)
class rec_resize_helper
{
  octave_idx_type *cext, *sext, *dext;
  int n;

public:
  rec_resize_helper (const dim_vector& ndv, const dim_vector& odv)
    {
      int l = ndv.length ();
      assert (odv.length () == l);
      octave_idx_type ld = 1;
      int i = 0;
      for (; i < l-1 && ndv(i) == odv(i); i++) ld *= ndv(i);
      n = l - i;
      cext = new octave_idx_type[3*n];
      // Trick to avoid three allocations
      sext = cext + n;
      dext = sext + n;

      octave_idx_type sld = ld, dld = ld;
      for (int j = 0; j < n; j++)
        {
          cext[j] = std::min (ndv(i+j), odv(i+j));
          sext[j] = sld *= odv(i+j);
          dext[j] = dld *= ndv(i+j);
        }
      cext[0] *= ld;
    }

  ~rec_resize_helper (void) { delete [] cext; }

private:
  // recursive resizing
  template <class T>
  void do_resize_fill (const T* src, T *dest, const T& rfv, int lev) const
    {
      if (lev == 0)
        {
          T* destc = std::copy (src, src + cext[0], dest);
          std::fill (destc, dest + dext[0], rfv);
        }
      else
        {
          octave_idx_type sd = sext[lev-1], dd = dext[lev-1], k;
          for (k = 0; k < cext[lev]; k++)
            do_resize_fill (src + k * sd, dest + k * dd, rfv, lev - 1);

          std::fill (dest + k * dd, dest + dext[lev], rfv);
        }
    }
public:
  template <class T>
  void resize_fill (const T* src, T *dest, const T& rfv) const 
    { do_resize_fill (src, dest, rfv, n-1); }

};

static void gripe_index_out_of_range (void)
{
  (*current_liboctave_error_handler)
    ("A(I): Index exceeds matrix dimension.");
}

template <class T>
Array<T>
Array<T>::index (const idx_vector& i) const
{
  octave_idx_type n = numel ();
  Array<T> retval;

  if (i.is_colon ())
    {
      // A(:) produces a shallow copy as a column vector.
      retval = Array<T> (*this, dim_vector (n, 1));
    }
  else if (i.extent (n) != n)
    {
      gripe_index_out_of_range ();
    }
  else
    {
      // FIXME -- this is the only place where orig_dimensions are used.
      dim_vector rd = i.orig_dimensions ();
      octave_idx_type il = i.length (n);

      // FIXME -- this is for Matlab compatibility.  Matlab 2007 given
      //
      //   b = ones(3,1)
      //
      // yields the following:
      //
      //   b(zeros(0,0)) gives []
      //   b(zeros(1,0)) gives zeros(0,1)
      //   b(zeros(0,1)) gives zeros(0,1)
      //   b(zeros(0,m)) gives zeros(0,m)
      //   b(zeros(m,0)) gives zeros(m,0)
      //   b(1:2) gives ones(2,1)
      //   b(ones(2)) gives ones(2) etc.
      //
      // As you can see, the behaviour is weird, but the tests end up pretty
      // simple.  Nah, I don't want to suggest that this is ad hoc :)

      if (ndims () == 2 && n != 1)
        {
          if (columns () == 1 && rd(0) == 1)
            rd = dim_vector (il, 1);
          else if (rows () == 1 && rd(1) == 1)
            rd = dim_vector (1, il);
        }

      octave_idx_type l, u;
      if (il != 0 && i.is_cont_range (n, l, u))
        // If suitable, produce a shallow slice.
        retval = Array<T> (*this, rd, l, u);
      else
        {
          // Don't use resize here to avoid useless initialization for POD
          // types.
          retval = Array<T> (rd);

          if (il != 0)
            i.index (data (), n, retval.fortran_vec ());
        }
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::index (const idx_vector& i, const idx_vector& j) const
{
  // Get dimensions, allowing Fortran indexing in the 2nd dim.
  dim_vector dv = dimensions.redim (2);
  octave_idx_type r = dv(0), c = dv(1);
  Array<T> retval;

  if (i.is_colon () && j.is_colon ())
    {
      // A(:,:) produces a shallow copy.
      retval = Array<T> (*this, dv);
    }
  else if (i.extent (r) != r || j.extent (c) != c)
    {
      gripe_index_out_of_range ();
    }
  else
    {
      octave_idx_type n = numel (), il = i.length (r), jl = j.length (c);

      idx_vector ii (i);

      if (ii.maybe_reduce (r, j, c))
        {
          octave_idx_type l, u;
          if (ii.length () > 0 && ii.is_cont_range (n, l, u))
            // If suitable, produce a shallow slice.
            retval = Array<T> (*this, dim_vector (il, jl), l, u);
          else
            {
              // Don't use resize here to avoid useless initialization for POD types.
              retval = Array<T> (dim_vector (il, jl));

              ii.index (data (), n, retval.fortran_vec ());
            }
        }
      else
        {
          // Don't use resize here to avoid useless initialization for POD types.
          retval = Array<T> (dim_vector (il, jl));

          const T* src = data ();
          T *dest = retval.fortran_vec ();

          for (octave_idx_type k = 0; k < jl; k++)
            dest += i.index (src + r * j.xelem (k), r, dest);
        }
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::index (const Array<idx_vector>& ia) const
{
  int ial = ia.length ();
  Array<T> retval;

  // FIXME: is this dispatching necessary?
  if (ial == 1)
    retval = index (ia(0));
  else if (ial == 2)
    retval = index (ia(0), ia(1));
  else if (ial > 0)
    {
      // Get dimensions, allowing Fortran indexing in the last dim.
      dim_vector dv = dimensions.redim (ial);

      // Check for out of bounds conditions.
      bool all_colons = true, mismatch = false;
      for (int i = 0; i < ial; i++)
        {
          if (ia(i).extent (dv(i)) != dv(i))
            {
              mismatch = true;
              break;
            }
          else
            all_colons = all_colons && ia(i).is_colon ();
        }


      if (mismatch)
        {
          gripe_index_out_of_range ();
        }
      else if (all_colons)
        {
          // A(:,:,...,:) produces a shallow copy.
          retval = Array<T> (*this, dv);
        }
      else 
        {
          // Form result dimensions.
          dim_vector rdv;
          rdv.resize (ial);
          for (int i = 0; i < ial; i++) rdv(i) = ia(i).length (dv(i));
          rdv.chop_trailing_singletons ();

          // Prepare for recursive indexing
          rec_index_helper rh (dv, ia);

          octave_idx_type l, u;
          if (rh.is_cont_range (l, u))
            // If suitable, produce a shallow slice.
            retval = Array<T> (*this, rdv, l, u);
          else
            {
              // Don't use resize here to avoid useless initialization for POD types.
              retval = Array<T> (rdv);

              // Do it.
              rh.index (data (), retval.fortran_vec ());
            }
        }
    }

  return retval;
}

// FIXME -- the following is a common error message to resize,
// regardless of whether it's called from assign or elsewhere.  It
// seems OK to me, but eventually the gripe can be specialized.
// Anyway, propagating various error messages into procedure is, IMHO,
// a nonsense.  If anything, we should change error handling here (and
// throughout liboctave) to allow custom handling of errors
static void gripe_invalid_resize (void)
{
  (*current_liboctave_error_handler)
    ("resize: Invalid resizing operation or ambiguous assignment to an out-of-bounds array element.");
}

// The default fill value.  Override if you want a different one.

template <class T>
T Array<T>::resize_fill_value ()
{
  return T ();
}

// Yes, we could do resize using index & assign.  However, that would
// possibly involve a lot more memory traffic than we actually need.

template <class T>
void
Array<T>::resize_fill (octave_idx_type n, const T& rfv)
{
  if (n >= 0 && ndims () == 2)
    {
      dim_vector dv;
      // This is driven by Matlab's behaviour of giving a *row* vector
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
        gripe_invalid_resize ();
      else
        {
          octave_idx_type nx = numel ();
          if (n == nx - 1 && n > 0)
            {
              // Stack "pop" operation.
              if (rep->count == 1)
                slice_data[slice_len-1] = T ();
              slice_len--;
              dimensions = dv;
            }
          else if (n == nx + 1 && nx > 0)
            {
              // Stack "push" operation.
              if (rep->count == 1 && slice_data + slice_len < rep->data + rep->len)
                {
                  slice_data[slice_len++] = rfv;
                  dimensions = dv;
                }
              else
                {
                  static const octave_idx_type max_stack_chunk = 1024;
                  octave_idx_type nn = n + std::min (nx, max_stack_chunk);
                  Array<T> tmp (Array<T> (nn), dv, 0, n);
                  T *dest = tmp.fortran_vec ();

                  std::copy (data (), data () + nx, dest);
                  dest[nx] = rfv;

                  *this = tmp;
                }
            }
          else if (n != nx)
            {
              Array<T> tmp = Array<T> (dv);
              T *dest = tmp.fortran_vec ();

              octave_idx_type n0 = std::min (n, nx), n1 = n - n0;
              dest = std::copy (data (), data () + n0, dest);
              std::fill (dest, dest + n1, rfv);

              *this = tmp;
            }
        }
    }
  else
    gripe_invalid_resize ();
}

template <class T>
void
Array<T>::resize_fill (octave_idx_type r, octave_idx_type c, const T& rfv)
{
  if (r >= 0 && c >= 0 && ndims () == 2)
    {
      octave_idx_type rx = rows (), cx = columns ();
      if (r != rx || c != cx)
        {
          Array<T> tmp = Array<T> (dim_vector (r, c));
          T *dest = tmp.fortran_vec ();

          octave_idx_type r0 = std::min (r, rx), r1 = r - r0;
          octave_idx_type c0 = std::min (c, cx), c1 = c - c0;
          const T *src = data ();
          if (r == rx)
            dest = std::copy (src, src + r * c0, dest);
          else
            {
              for (octave_idx_type k = 0; k < c0; k++)
                {
                  dest = std::copy (src, src + r0, dest);
                  src += rx;
                  std::fill (dest, dest + r1, rfv);
                  dest += r1;
                }
            }

          std::fill (dest, dest + r * c1, rfv);

          *this = tmp;
        }
    }
  else
    gripe_invalid_resize ();

}

template<class T>
void
Array<T>::resize_fill (const dim_vector& dv, const T& rfv)
{
  int dvl = dv.length ();
  if (dvl == 2)
    resize (dv(0), dv(1), rfv);
  else if (dimensions != dv)
    {
      if (dimensions.length () <= dvl && ! dv.any_neg ())
        {
          Array<T> tmp (dv);
          // Prepare for recursive resizing.
          rec_resize_helper rh (dv, dimensions.redim (dvl));

          // Do it.
          rh.resize_fill (data (), tmp.fortran_vec (), rfv);   
          *this = tmp;
        }
      else
        gripe_invalid_resize ();
    }
}

template <class T>
Array<T> 
Array<T>::index (const idx_vector& i, bool resize_ok, const T& rfv) const
{
  Array<T> tmp = *this;
  if (resize_ok)
    {
      octave_idx_type n = numel (), nx = i.extent (n);
      if (n != nx)
        {
          if (i.is_scalar ())
            return Array<T> (1, rfv);
          else
            tmp.resize_fill (nx, rfv);
        }

      if (tmp.numel () != nx)
        return Array<T> ();
    }

  return tmp.index (i);
}

template <class T>
Array<T> 
Array<T>::index (const idx_vector& i, const idx_vector& j, 
                 bool resize_ok, const T& rfv) const
{
  Array<T> tmp = *this;
  if (resize_ok)
    {
      dim_vector dv = dimensions.redim (2);
      octave_idx_type r = dv(0), c = dv(1);
      octave_idx_type rx = i.extent (r), cx = j.extent (c);
      if (r != rx || c != cx)
        {
          if (i.is_scalar () && j.is_scalar ())
            return Array<T> (1, rfv);
          else
            tmp.resize_fill (rx, cx, rfv);
        }

      if (tmp.rows () != rx || tmp.columns () != cx)
        return Array<T> ();
    }

  return tmp.index (i, j);  
}

template <class T>
Array<T> 
Array<T>::index (const Array<idx_vector>& ia,
                 bool resize_ok, const T& rfv) const
{
  Array<T> tmp = *this;
  if (resize_ok)
    {
      int ial = ia.length ();
      dim_vector dv = dimensions.redim (ial);
      dim_vector dvx; dvx.resize (ial);
      for (int i = 0; i < ial; i++) dvx(i) = ia(i).extent (dv (i));
      if (! (dvx == dv))
        {
          bool all_scalars = true;
          for (int i = 0; i < ial; i++) 
            all_scalars = all_scalars && ia(i).is_scalar ();
          if (all_scalars)
            return Array<T> (1, rfv);
          else
            tmp.resize_fill (dvx, rfv);
        }

      if (tmp.dimensions != dvx)
        return Array<T> ();
    }

  return tmp.index (ia);  
}


static void 
gripe_invalid_assignment_size (void)
{
  (*current_liboctave_error_handler)
    ("A(I) = X: X must have the same size as I");
}

static void
gripe_assignment_dimension_mismatch (void)
{
  (*current_liboctave_error_handler)
    ("A(I,J,...) = X: dimensions mismatch");
}

template <class T>
void
Array<T>::assign (const idx_vector& i, const Array<T>& rhs, const T& rfv)
{
  octave_idx_type n = numel (), rhl = rhs.numel ();

  if (rhl == 1 || i.length (n) == rhl)
    {
      octave_idx_type nx = i.extent (n);
      // Try to resize first if necessary. 
      if (nx != n)
        {
          // Optimize case A = []; A(1:n) = X with A empty. 
          if (rows () == 0 && columns () == 0 && ndims () == 2
              && i.is_colon_equiv (nx))
            {
              if (rhl == 1)
                *this = Array<T> (dim_vector (1, nx), rhs(0));
              else
                *this = Array<T> (rhs, dim_vector (1, nx));
              return;
            }

          resize_fill (nx, rfv);      
          n = numel ();
        }

      if (i.is_colon ())
        {
          // A(:) = X makes a full fill or a shallow copy.
          if (rhl == 1)
            fill (rhs(0));
          else
            *this = rhs.reshape (dimensions);
        }
      else
        {
          if (rhl == 1)
            i.fill (rhs(0), n, fortran_vec ());
          else
            i.assign (rhs.data (), n, fortran_vec ());
        }
    }
  else
    gripe_invalid_assignment_size ();
}

template <class T>
void
Array<T>::assign (const idx_vector& i, const idx_vector& j,
                  const Array<T>& rhs, const T& rfv)
{
  // Get RHS extents, discarding singletons.
  dim_vector rhdv = rhs.dims (); 
  // Get LHS extents, allowing Fortran indexing in the second dim.
  dim_vector dv = dimensions.redim (2);
  // Check for out-of-bounds and form resizing dimensions.
  dim_vector rdv; 
  // In the special when all dimensions are zero, colons are allowed
  // to inquire the shape of RHS.  The rules are more obscure, so we
  // solve that elsewhere.
  if (dv.all_zero ())
    rdv = zero_dims_inquire (i, j, rhdv);
  else
    {
      rdv(0) = i.extent (dv(0));
      rdv(1) = j.extent (dv(1));
    }

  bool isfill = rhs.numel () == 1;
  octave_idx_type il = i.length (rdv(0)), jl = j.length (rdv(1));
  rhdv.chop_all_singletons ();
  bool match = (isfill
		|| (rhdv.length () == 2 && il == rhdv(0) && jl == rhdv(1)));
  match = match || (il == 1 && jl == rhdv(0) && rhdv(1) == 1);

  if (match)
    {
      // Resize if requested.
      if (rdv != dv)
        {
          // Optimize case A = []; A(1:m, 1:n) = X
          if (dv.all_zero () && i.is_colon_equiv (rdv(0))
              && j.is_colon_equiv (rdv(1)))
            {
              if (isfill)
                *this = Array<T> (rdv, rhs(0));
              else
                *this = Array<T> (rhs, rdv);
              return;
            }

          resize (rdv, rfv);
          dv = dimensions;
        }

      if (i.is_colon () && j.is_colon ())
        {
          // A(:,:) = X makes a full fill or a shallow copy
          if (isfill)
            fill (rhs(0));
          else
            *this = rhs.reshape (dimensions);
        }
      else
        {
          // The actual work.
          octave_idx_type n = numel (), r = dv (0), c = dv (1);
          idx_vector ii (i);

          const T* src = rhs.data ();
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
  else
    gripe_assignment_dimension_mismatch ();
}

template <class T>
void
Array<T>::assign (const Array<idx_vector>& ia,
                  const Array<T>& rhs, const T& rfv)
{
  int ial = ia.length ();

  // FIXME: is this dispatching necessary / desirable?
  if (ial == 1)
    assign (ia(0), rhs, rfv);
  else if (ial == 2)
    assign (ia(0), ia(1), rhs, rfv);
  else if (ial > 0)
    {
      // Get RHS extents, discarding singletons.
      dim_vector rhdv = rhs.dims ();

      // Get LHS extents, allowing Fortran indexing in the second dim.
      dim_vector dv = dimensions.redim (ial);
      
      // Get the extents forced by indexing. 
      dim_vector rdv;

      // In the special when all dimensions are zero, colons are
      // allowed to inquire the shape of RHS.  The rules are more
      // obscure, so we solve that elsewhere.
      if (dv.all_zero ())
        rdv = zero_dims_inquire (ia, rhdv);
      else
        {
          rdv.resize (ial);
          for (int i = 0; i < ial; i++)
            rdv(i) = ia(i).extent (dv(i));
        }

      // Check whether LHS and RHS match, up to singleton dims.
      bool match = true, all_colons = true, isfill = rhs.numel () == 1;

      rhdv.chop_all_singletons ();
      int j = 0, rhdvl = rhdv.length ();
      for (int i = 0; i < ial; i++)
        {
          all_colons = all_colons && ia(i).is_colon ();
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
              resize_fill (rdv, rfv);
              dv = dimensions;
              chop_trailing_singletons ();
            }

          if (all_colons)
            {
              // A(:,:,...,:) = X makes a full fill or a shallow copy.
              if (isfill)
                fill (rhs(0));
              else
                *this = rhs.reshape (dimensions);
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
        gripe_assignment_dimension_mismatch ();
    }
}

template <class T>
void 
Array<T>::delete_elements (const idx_vector& i)
{
  octave_idx_type n = numel ();
  if (i.is_colon ())
    { 
      *this = Array<T> ();
    }
  else if (i.extent (n) != n)
    {
      gripe_index_out_of_range ();
    }
  else if (i.length (n) != 0)
    {
      octave_idx_type l, u;
      bool col_vec = ndims () == 2 && columns () == 1 && rows () != 1;
      if (i.is_scalar () && i(0) == n-1)
        {
          // Stack "pop" operation.
          resize (n-1);
        }
      else if (i.is_cont_range (n, l, u))
        {
          // Special case deleting a contiguous range.
          octave_idx_type m = n + l - u;
          Array<T> tmp (dim_vector (col_vec ? m : 1, !col_vec ? m : 1));
          const T *src = data ();
          T *dest = tmp.fortran_vec ();
          dest = std::copy (src, src + l, dest);
          dest = std::copy (src + u, src + n, dest);
          *this = tmp;
        }
      else
        {
          // Use index.
          *this = index (i.complement (n));
        }
    }
}

template <class T>
void 
Array<T>::delete_elements (int dim, const idx_vector& i)
{
  if (dim < 0 || dim >= ndims ())
    {
      (*current_liboctave_error_handler)
        ("invalid dimension in delete_elements");
      return;
    }

  octave_idx_type n = dimensions (dim);
  if (i.is_colon ())
    { 
      *this = Array<T> ();
    }
  else if (i.extent (n) != n)
    {
      gripe_index_out_of_range ();
    }
  else if (i.length (n) != 0)
    {
      octave_idx_type l, u;

      if (i.is_cont_range (n, l, u))
        {
          // Special case deleting a contiguous range.
          octave_idx_type nd = n + l - u, dl = 1, du = 1;
          dim_vector rdv = dimensions;
          rdv(dim) = nd;
          for (int k = 0; k < dim; k++) dl *= dimensions(k);
          for (int k = dim + 1; k < ndims (); k++) du *= dimensions(k);

          // Special case deleting a contiguous range.
          Array<T> tmp = Array<T> (rdv);
          const T *src = data ();
          T *dest = tmp.fortran_vec ();
          l *= dl; u *= dl; n *= dl;
          for (octave_idx_type k = 0; k < du; k++)
            {
              dest = std::copy (src, src + l, dest);
              dest = std::copy (src + u, src + n, dest);
              src += n;
            }

          *this = tmp;
        }
      else
        {
          // Use index.
          Array<idx_vector> ia (ndims (), idx_vector::colon);
          ia (dim) = i.complement (n);
          *this = index (ia);
        }
    }
}

template <class T>
void 
Array<T>::delete_elements (const Array<idx_vector>& ia)
{
  if (ia.length () == 1)
    delete_elements (ia(0));
  else
    {
      int len = ia.length (), k, dim = -1;
      for (k = 0; k < len; k++)
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
          dim_vector dv = dimensions;
          dv(0) = 0;
          *this = Array<T> (dv);
        }
      else if (k == len)
        {
          delete_elements (dim, ia(dim));
        }
      else
        {
          (*current_liboctave_error_handler)
            ("A null assignment can only have one non-colon index.");
        }
    }

}

// FIXME: Remove these methods or implement them using assign.

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, octave_idx_type r, octave_idx_type c)
{
  if (ndims () == 2 && a.ndims () == 2)
    insert2 (a, r, c);
  else
    insertN (a, r, c);

  return *this;
}


template <class T>
Array<T>&
Array<T>::insert2 (const Array<T>& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_rows = a.rows ();
  octave_idx_type a_cols = a.cols ();

  if (r < 0 || r + a_rows > rows () || c < 0 || c + a_cols > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (octave_idx_type j = 0; j < a_cols; j++)
    for (octave_idx_type i = 0; i < a_rows; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

template <class T>
Array<T>&
Array<T>::insertN (const Array<T>& a, octave_idx_type r, octave_idx_type c)
{
  dim_vector dv = dims ();

  dim_vector a_dv = a.dims ();

  int n = a_dv.length ();

  if (n == dimensions.length ())
    {
      Array<octave_idx_type> a_ra_idx (a_dv.length (), 0);

      a_ra_idx.elem (0) = r;
      a_ra_idx.elem (1) = c;

      for (int i = 0; i < n; i++)
	{
	  if (a_ra_idx(i) < 0 || (a_ra_idx(i) + a_dv(i)) > dv(i))
	    {
	      (*current_liboctave_error_handler)
		("Array<T>::insert: range error for insert");
	      return *this;
	    }
	}

      octave_idx_type n_elt = a.numel ();
      
      const T *a_data = a.data ();   
   
      octave_idx_type iidx = 0;
	  
      octave_idx_type a_rows = a_dv(0);

      octave_idx_type this_rows = dv(0);
	  
      octave_idx_type numel_page = a_dv(0) * a_dv(1);	  

      octave_idx_type count_pages = 0;
	  
      for (octave_idx_type i = 0; i < n_elt; i++)
	{
	  if (i != 0 && i % a_rows == 0)
	    iidx += (this_rows - a_rows);	      
	  
	  if (i % numel_page == 0)
	    iidx = c * dv(0) + r + dv(0) * dv(1) * count_pages++;

	  elem (iidx++) = a_data[i];
	}
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::insert: invalid indexing operation");

  return *this;
}

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, const Array<octave_idx_type>& ra_idx)
{
  octave_idx_type n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      dim_vector dva = a.dims ();
      dim_vector dv = dims ();
      int len_a = dva.length ();
      int non_full_dim = 0;

      for (octave_idx_type i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || (ra_idx(i) + 
				(i < len_a ? dva(i) : 1)) > dimensions(i))
	    {
	      (*current_liboctave_error_handler)
		("Array<T>::insert: range error for insert");
	      return *this;
	    }

	  if (dv(i) != (i < len_a ? dva(i) : 1))
	    non_full_dim++;
	}

      if (dva.numel ())
        {
	  if (non_full_dim < 2)
	    {
	      // Special case for fast concatenation
	      const T *a_data = a.data ();
	      octave_idx_type numel_to_move = 1;
	      octave_idx_type skip = 0;
	      for (int i = 0; i < len_a; i++)
		if (ra_idx(i) == 0 && dva(i) == dv(i))
		  numel_to_move *= dva(i);
		else
		  {
		    skip = numel_to_move * (dv(i) - dva(i));
		    numel_to_move *= dva(i);
		    break;
		  }

	      octave_idx_type jidx = ra_idx(n-1);
	      for (int i = n-2; i >= 0; i--)
		{
		  jidx *= dv(i);
		  jidx += ra_idx(i);
		}

	      octave_idx_type iidx = 0;
	      octave_idx_type moves = dva.numel () / numel_to_move;
	      for (octave_idx_type i = 0; i < moves; i++)
		{
		  for (octave_idx_type j = 0; j < numel_to_move; j++)
		    elem (jidx++) = a_data[iidx++];
		  jidx += skip;
		}
	    }
	  else
	    {
	      // Generic code
	      const T *a_data = a.data ();
	      int nel = a.numel ();
	      Array<octave_idx_type> a_idx (n, 0);

	      for (int i = 0; i < nel; i++)
		{
		  int iidx = a_idx(n-1) + ra_idx(n-1);
		  for (int j = n-2; j >= 0; j--)
		    {
		      iidx *= dv(j);
		      iidx += a_idx(j) + ra_idx(j);
		    }

		  elem (iidx) = a_data[i];

		  increment_index (a_idx, dva);
		}
	    }
	}
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::insert: invalid indexing operation");

  return *this;
}


template <class T>
Array<T>
Array<T>::transpose (void) const
{
  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr >= 8 && nc >= 8)
    {
      Array<T> result (dim_vector (nc, nr));

      // Reuse the implementation used for permuting.

      rec_permute_helper::blk_trans (data (), result.fortran_vec (), nr, nc);

      return result;
    }
  else if (nr > 1 && nc > 1)
    {
      Array<T> result (dim_vector (nc, nr));

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  result.xelem (j, i) = xelem (i, j);

      return result;
    }
  else
    {
      // Fast transpose for vectors and empty matrices.
      return Array<T> (*this, dim_vector (nc, nr));
    }
}

template <class T>
static T
no_op_fcn (const T& x)
{
  return x;
}

template <class T>
Array<T>
Array<T>::hermitian (T (*fcn) (const T&)) const
{
  assert (ndims () == 2);

  if (! fcn)
    fcn = no_op_fcn<T>;

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr >= 8 && nc >= 8)
    {
      Array<T> result (dim_vector (nc, nr));

      // Blocked transpose to attempt to avoid cache misses.

      // Don't use OCTAVE_LOCAL_BUFFER here as it doesn't work with bool
      // on some compilers.
      T buf[64];

      octave_idx_type ii = 0, jj;
      for (jj = 0; jj < (nc - 8 + 1); jj += 8)
	{
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
      Array<T> result (dim_vector (nc, nr));

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  result.xelem (j, i) = fcn (xelem (i, j));

      return result;
    }
}

/*

%% Tranpose tests for matrices of the tile size and plus or minus a row
%% and with four tiles.

%!shared m7, mt7, m8, mt8, m9, mt9
%! m7 = reshape (1 : 7*8, 8, 7);
%! mt7 = [1:8; 9:16; 17:24; 25:32; 33:40; 41:48; 49:56];
%! m8 = reshape (1 : 8*8, 8, 8);
%! mt8 = mt8 = [mt7; 57:64];
%! m9 = reshape (1 : 9*8, 8, 9);
%! mt9 = [mt8; 65:72];

%!assert(m7', mt7)
%!assert((1i*m7).', 1i * mt7)
%!assert((1i*m7)', conj (1i * mt7))
%!assert(m8', mt8)
%!assert((1i*m8).', 1i * mt8)
%!assert((1i*m8)', conj (1i * mt8))
%!assert(m9', mt9)
%!assert((1i*m9).', 1i * mt9)
%!assert((1i*m9)', conj (1i * mt9))
%!assert([m7, m8; m7, m8]', [mt7, mt7; mt8, mt8])
%!assert((1i*[m7, m8; m7, m8]).', 1i * [mt7, mt7; mt8, mt8])
%!assert((1i*[m7, m8; m7, m8])', conj (1i * [mt7, mt7; mt8, mt8]))
%!assert([m8, m8; m8, m8]', [mt8, mt8; mt8, mt8])
%!assert((1i*[m8, m8; m8, m8]).', 1i * [mt8, mt8; mt8, mt8])
%!assert((1i*[m8, m8; m8, m8])', conj (1i * [mt8, mt8; mt8, mt8]))
%!assert([m9, m8; m9, m8]', [mt9, mt9; mt8, mt8])
%!assert((1i*[m9, m8; m9, m8]).', 1i * [mt9, mt9; mt8, mt8])
%!assert((1i*[m9, m8; m9, m8])', conj (1i * [mt9, mt9; mt8, mt8]))

*/

template <class T>
T *
Array<T>::fortran_vec (void)
{
  make_unique ();

  return slice_data;
}

template <class T>
void
Array<T>::maybe_delete_dims (void)
{
  int nd = dimensions.length ();

  dim_vector new_dims (1, 1);

  bool delete_dims = true;

  for (int i = nd - 1; i >= 0; i--)
    {
      if (delete_dims)
        {
          if (dimensions(i) != 1)
	    {
	      delete_dims = false;

	      new_dims = dim_vector (i + 1, dimensions(i));
	    }
        }
      else
	new_dims(i) = dimensions(i);
    }

  if (nd != new_dims.length ())
    dimensions = new_dims;
}

// Non-real types don't have NaNs.
template <class T>
inline bool
sort_isnan (typename ref_param<T>::type)
{
  return false;
}

template <class T>
Array<T>
Array<T>::sort (octave_idx_type dim, sortmode mode) const
{
  if (dim < 0 || dim >= ndims ())
    {
      (*current_liboctave_error_handler)
        ("sort: invalid dimension");
      return Array<T> ();
    }

  Array<T> m (dims ());

  dim_vector dv = m.dims ();

  if (m.length () < 1)
    return m;

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  octave_sort<T> lsort;
  
  if (mode) 
    lsort.set_compare (mode);
  else
    return m;

  if (stride == 1)
    {
      for (octave_idx_type j = 0; j < iter; j++)
	{
          // copy and partition out NaNs. 
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0, ku = ns;
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
	  octave_idx_type offset2 = 0;

	  while (offset >= stride)
	    {
	      offset -= stride;
	      offset2++;
	    }

	  offset += offset2 * stride * ns;
	  
          // gather and partition out NaNs. 
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0, ku = ns;
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

template <class T>
Array<T>
Array<T>::sort (Array<octave_idx_type> &sidx, octave_idx_type dim, 
		sortmode mode) const
{
  if (dim < 0 || dim >= ndims ())
    {
      (*current_liboctave_error_handler)
        ("sort: invalid dimension");
      return Array<T> ();
    }

  Array<T> m (dims ());

  dim_vector dv = m.dims ();

  if (m.length () < 1)
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
  
  if (mode) 
    lsort.set_compare (mode);
  else
    return m;

  if (stride == 1)
    {
      for (octave_idx_type j = 0; j < iter; j++)
	{
          // copy and partition out NaNs. 
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0, ku = ns;
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
	  octave_idx_type offset2 = 0;

	  while (offset >= stride)
	    {
	      offset -= stride;
	      offset2++;
	    }

	  offset += offset2 * stride * ns;
	  
          // gather and partition out NaNs. 
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0, ku = ns;
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

template <class T>
sortmode
Array<T>::is_sorted (sortmode mode) const
{
  if (nelem () <= 1)
    return ASCENDING;

  const T *lo = data (), *hi = data () + nelem () - 1;

  // Check for NaNs at the beginning and end.
  if (mode != ASCENDING && sort_isnan<T> (*lo))
    {
      mode = DESCENDING;
      do
        ++lo;
      while (lo < hi && sort_isnan<T> (*lo));
    }
  else if (mode != DESCENDING && sort_isnan<T> (*hi))
    {
      mode = ASCENDING;
      do
        --hi;
      while (lo < hi && sort_isnan<T> (*hi));
    }
  
  octave_sort<T> lsort;

  // If mode is still unknown, compare lo and hi
  if (! mode)
    {
      if (lsort.descending_compare (*lo, *hi))
        mode = DESCENDING;
      else if (lsort.ascending_compare (*lo, *hi))
        mode = ASCENDING;
      else
        mode = ASCENDING;
    }

  lsort.set_compare (mode);

  if (! lsort.is_sorted (lo, hi - lo + 1))
    mode = UNSORTED;

  return mode;
}

template <class T>
typename Array<T>::compare_fcn_type
sortrows_comparator (sortmode mode, const Array<T>& /* a */,
		     bool /* allow_chk */)
{
  if (mode == ASCENDING)
    return octave_sort<T>::ascending_compare;
  else if (mode == DESCENDING)
    return octave_sort<T>::descending_compare;
  else
    return 0;
}

template <class T>
Array<octave_idx_type>
Array<T>::sort_rows_idx (sortmode mode) const
{
  Array<octave_idx_type> idx;

  octave_sort<T> lsort;

  lsort.set_compare (sortrows_comparator (mode, *this, true));

  octave_idx_type r = rows (), c = cols ();

  idx = Array<octave_idx_type> (r);

  lsort.sort_rows (data (), idx.fortran_vec (), r, c);

  return idx;
}


template <class T>
sortmode 
Array<T>::is_sorted_rows (sortmode mode) const
{
  octave_sort<T> lsort;

  octave_idx_type r = rows (), c = cols ();

  if (r <= 1 || c == 0)
    return mode ? mode : ASCENDING;

  if (! mode)
    {
      // Auto-detect mode.
      compare_fcn_type compare
	= sortrows_comparator (ASCENDING, *this, false);

      octave_idx_type i;
      for (i = 0; i < cols (); i++)
        {
          T l = elem (0, i), u = elem (rows () - 1, i);
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
      if (! mode && i == cols ())
        mode = ASCENDING;
    }

  if (mode)
    {
      lsort.set_compare (sortrows_comparator (mode, *this, false));

      if (! lsort.is_sorted_rows (data (), r, c))
        mode = UNSORTED;
    }

  return mode;

}

// Do a binary lookup in a sorted array.
template <class T>
octave_idx_type 
Array<T>::lookup (const T& value, sortmode mode) const
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

// Ditto, but for an array of values, specializing on long runs.
// Adds optional offset to all indices.
template <class T>
Array<octave_idx_type> 
Array<T>::lookup (const Array<T>& values, sortmode mode, 
                  bool linf, bool rinf) const
{
  octave_idx_type n = numel ();
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

  // set offset and shift size.
  octave_idx_type offset = 0;

  if (linf && n > 0)
    {
      offset++;
      n--;
    }
  if (rinf && n > 0)
    n--;

  lsort.lookup (data () + offset, n, values.data (), values.numel (),
                idx.fortran_vec (), offset);

  return idx;
}

template <class T>
Array<octave_idx_type> 
Array<T>::find (octave_idx_type n, bool backward) const
{
  Array<octave_idx_type> retval;
  const T *src = data ();
  octave_idx_type nel = nelem ();
  const T zero = T ();
  if (n < 0 || n >= nel)
    {
      // We want all elements, which means we'll almost surely need
      // to resize. So count first, then allocate array of exact size.
      octave_idx_type cnt = 0;
      for (octave_idx_type i = 0; i < nel; i++)
        cnt += src[i] != zero;

      retval = Array<octave_idx_type> (cnt);
      octave_idx_type *dest = retval.fortran_vec ();
      for (octave_idx_type i = 0; i < nel; i++)
        if (src[i] != zero) *dest++ = i;
    }
  else
    {
      // We want a fixed max number of elements, usually small. So be
      // optimistic, alloc the array in advance, and then resize if
      // needed.
      retval = Array<octave_idx_type> (n);
      if (backward)
        {
          // Do the search as a series of successive single-element searches.
          octave_idx_type k = 0, l = nel - 1;
          for (; k < n; k++)
            {
              for (;l >= 0 && src[l] == zero; l--) ;
              if (l >= 0)
                retval(k) = l--;
              else
                break;
            }
          if (k < n)
            retval.resize (k);
          octave_idx_type *rdata = retval.fortran_vec ();
          std::reverse (rdata, rdata + k);
        }
      else
        {
          // Do the search as a series of successive single-element searches.
          octave_idx_type k = 0, l = 0;
          for (; k < n; k++)
            {
              for (;l != nel && src[l] == zero; l++) ;
              if (l != nel)
                retval(k) = l++;
              else
                break;
            }
          if (k < n)
            retval.resize (k);
        }
    }

  // Fixup return dimensions, for Matlab compatibility.
  // find(zeros(0,0)) -> zeros(0,0)
  // find(zeros(1,0)) -> zeros(1,0)
  // find(zeros(0,1)) -> zeros(0,1)
  // find(zeros(0,X)) -> zeros(0,1)
  // find(zeros(1,1)) -> zeros(0,0) !!!! WHY?
  // find(zeros(0,1,0)) -> zeros(0,0)
  // find(zeros(0,1,0,1)) -> zeros(0,0) etc

  if ((numel () == 1 && retval.is_empty ())
      || (rows () == 0 && dims ().numel (1) == 0))
    retval.dimensions = dim_vector ();
  else if (rows () == 1 && ndims () == 2)
    retval.dimensions = dim_vector (1, retval.length ());

  return retval;
}


#define INSTANTIATE_ARRAY_SORT(T) template class OCTAVE_API octave_sort<T>;

#define NO_INSTANTIATE_ARRAY_SORT(T) \
 \
template <> Array<T>  \
Array<T>::sort (octave_idx_type, sortmode) const { return *this; } \
 \
template <> Array<T>  \
Array<T>::sort (Array<octave_idx_type> &sidx, octave_idx_type, sortmode) const \
{ sidx = Array<octave_idx_type> (); return *this; } \
 \
template <> sortmode  \
Array<T>::is_sorted (sortmode) const  \
{ return UNSORTED; } \
 \
Array<T>::compare_fcn_type \
sortrows_comparator (sortmode, const Array<T>&, bool) \
{ return 0; } \
 \
template <> Array<octave_idx_type>  \
Array<T>::sort_rows_idx (sortmode) const  \
{ return Array<octave_idx_type> (); } \
 \
template <> sortmode  \
Array<T>::is_sorted_rows (sortmode) const \
{ return UNSORTED; } \
 \
template <> octave_idx_type  \
Array<T>::lookup (T const &, sortmode) const \
{ return 0; } \
template <> Array<octave_idx_type>  \
Array<T>::lookup (const Array<T>&, sortmode, bool, bool) const \
{ return Array<octave_idx_type> (); } \
template <> Array<octave_idx_type> \
Array<T>::find (octave_idx_type, bool) const\
{ return Array<octave_idx_type> (); } \


template <class T>
Array<T>
Array<T>::diag (octave_idx_type k) const
{
  dim_vector dv = dims ();
  octave_idx_type nd = dv.length ();
  Array<T> d;

  if (nd > 2)
    (*current_liboctave_error_handler) ("Matrix must be 2-dimensional");    
  else
    {
      octave_idx_type nnr = dv (0);
      octave_idx_type nnc = dv (1);

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
	      d = Array<T> (dim_vector (n, n), resize_fill_value ());

	      for (octave_idx_type i = 0; i < nnc; i++)
		d.xelem (i+roff, i+coff) = elem (0, i);
	    }
	  else
	    {
	      octave_idx_type n = nnr + std::abs (k);
	      d = Array<T> (dim_vector (n, n), resize_fill_value ());

	      for (octave_idx_type i = 0; i < nnr; i++)
		d.xelem (i+roff, i+coff) = elem (i, 0);
	    }
	}
    }

  return d;
}

template <class T>
void
Array<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << '\n'
     << prefix << "rep->len:    " << rep->len << '\n'
     << prefix << "rep->data:   " << static_cast<void *> (rep->data) << '\n'
     << prefix << "rep->count:  " << rep->count << '\n'
     << prefix << "slice_data:  " << static_cast<void *> (slice_data) << '\n'
     << prefix << "slice_len:   " << slice_len << '\n';

  // 2D info:
  //
  //     << pefix << "rows: " << rows () << "\n"
  //     << prefix << "cols: " << cols () << "\n";
}

template <class T>
void Array<T>::instantiation_guard ()
{
  // This guards against accidental implicit instantiations.
  // Array<T> instances should always be explicit and use INSTANTIATE_ARRAY.
  T::__xXxXx__();
}

#define INSTANTIATE_ARRAY(T, API) \
  template <> void Array<T>::instantiation_guard () { } \
  template class API Array<T>

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
