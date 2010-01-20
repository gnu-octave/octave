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

#include <iostream>
#include <sstream>
#include <vector>

#include "Array.h"
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
  octave_idx_type ndel = nzmx - c[ncols];
  octave_idx_type nzero = 0;

  if (remove_zeros)
    for (octave_idx_type i = 0; i < nzmx - ndel; i++)
      if (d[i] == T ())
	nzero++;

  if (!ndel && !nzero)
    return;

  if (!nzero)
    {
      octave_idx_type new_nzmx = nzmx - ndel;

      T *new_data = new T [new_nzmx];
      for (octave_idx_type i = 0; i < new_nzmx; i++)
	new_data[i] = d[i];
      delete [] d;
      d = new_data;

      octave_idx_type *new_ridx = new octave_idx_type [new_nzmx];
      for (octave_idx_type i = 0; i < new_nzmx; i++)
	new_ridx[i] = r[i];
      delete [] r;
      r = new_ridx;
    }
  else
    {
      octave_idx_type new_nzmx = nzmx - ndel - nzero;

      T *new_data = new T [new_nzmx];
      octave_idx_type *new_ridx = new octave_idx_type [new_nzmx];

      octave_idx_type ii = 0;
      octave_idx_type ic = 0;
      for (octave_idx_type j = 0; j < ncols; j++)
	{
	  for (octave_idx_type k = ic; k < c[j+1]; k++)
	    if (d[k] != T ())
	      {
		new_data [ii] = d[k];
		new_ridx [ii++] = r[k];
	      }
	  ic = c[j+1];
	  c[j+1] = ii;
	}

      delete [] d;
      d = new_data;

      delete [] r;
      r = new_ridx;
    }

  nzmx -= ndel + nzero;
}

template <class T>
void
Sparse<T>::SparseRep::change_length (octave_idx_type nz)
{
  if (nz != nzmx)
    {
      octave_idx_type min_nzmx = (nz < nzmx ? nz : nzmx);

      octave_idx_type * new_ridx = new octave_idx_type [nz];
      for (octave_idx_type i = 0; i < min_nzmx; i++)
	new_ridx[i] = r[i];

      delete [] r;
      r = new_ridx;

      T * new_data = new T [nz];
      for (octave_idx_type i = 0; i < min_nzmx; i++)
	new_data[i] = d[i];

      delete [] d;
      d = new_data;

      if (nz < nzmx)
	for (octave_idx_type i = 0; i <= ncols; i++)
	  if (c[i] > nz)
	    c[i] = nz;

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
Sparse<T>::Sparse (const Array<T>& a, const Array<octave_idx_type>& r, 
		   const Array<octave_idx_type>& c, octave_idx_type nr,
		   octave_idx_type nc, bool sum_terms)
  : dimensions (dim_vector (nr, nc)), idx (0), idx_count (0)
{
  octave_idx_type a_len = a.length ();
  octave_idx_type r_len = r.length ();
  octave_idx_type c_len = c.length ();
  bool ri_scalar = (r_len == 1); 
  bool ci_scalar = (c_len == 1);
  bool cf_scalar = (a_len == 1);
  
  if ((a_len != r_len && !cf_scalar && !ri_scalar) ||
      (a_len != c_len && !cf_scalar && !ci_scalar) ||
      (r_len != c_len && !ri_scalar && !ci_scalar) || nr < 0 || nc < 0)
    {
      (*current_liboctave_error_handler)
	("Sparse::Sparse (const Array<T>&, const Array<octave_idx_type>&, ...): dimension mismatch");
      rep = nil_rep ();
      dimensions = dim_vector (0, 0);
    }
  else
    {
      octave_idx_type max_nzmx = (r_len > c_len ? r_len : c_len);

      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl *, sidx, max_nzmx);
      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl, sidxX, max_nzmx);

      for (octave_idx_type i = 0; i < max_nzmx; i++)
	sidx[i] = &sidxX[i];

      octave_idx_type actual_nzmx = 0;
      octave_quit ();
      for (octave_idx_type i = 0; i < max_nzmx; i++) 
	{
	  octave_idx_type rowidx =  (ri_scalar ? r(0) : r(i));
	  octave_idx_type colidx = (ci_scalar ? c(0) : c(i));
	  if (rowidx < nr && rowidx >= 0 &&
	      colidx < nc && colidx >= 0 ) 
	    {
	      if ( a (cf_scalar ? 0 : i ) != T ()) 
		{
		  sidx[actual_nzmx]->r = rowidx;
		  sidx[actual_nzmx]->c = colidx;
		  sidx[actual_nzmx]->idx = i;
		  actual_nzmx++;	
		}
	    }
	  else 
	    {
	      (*current_liboctave_error_handler)
		("Sparse::Sparse : index (%d,%d) out of range", 
		 rowidx + 1, colidx + 1);
	      rep = nil_rep ();
	      dimensions = dim_vector (0, 0);
	      return;
	    }
	}
  
      if (actual_nzmx == 0)
	rep = new typename Sparse<T>::SparseRep (nr, nc);
      else
	{
	  octave_quit ();
	  octave_sort<octave_sparse_sort_idxl *> 
	    lsort (octave_sparse_sidxl_comp);

	  lsort.sort (sidx, actual_nzmx);
	  octave_quit ();

	  // Now count the unique non-zero values
	  octave_idx_type real_nzmx = 1;
	  for (octave_idx_type i = 1; i < actual_nzmx; i++) 
	    if (sidx[i-1]->r != sidx[i]->r || sidx[i-1]->c != sidx[i]->c) 
	      real_nzmx++;

	  rep = new typename Sparse<T>::SparseRep (nr, nc, real_nzmx);

	  octave_idx_type cx = 0;
	  octave_idx_type prev_rval = -1;
	  octave_idx_type prev_cval = -1;
	  octave_idx_type ii = -1;
	  xcidx (0) = 0;
	  for (octave_idx_type i = 0; i < actual_nzmx; i++) 
	    {
	      octave_quit ();
	      octave_idx_type iidx = sidx[i]->idx;
	      octave_idx_type rval = sidx[i]->r;
	      octave_idx_type cval = sidx[i]->c;

	      if (prev_cval < cval || (prev_rval < rval && prev_cval == cval)) 
		{
		  octave_idx_type ci = static_cast<octave_idx_type> (c (ci_scalar ? 0 : iidx));
		  ii++;
		  while (cx < ci) 
		    xcidx (++cx) = ii;
		  xdata(ii) = a (cf_scalar ? 0 : iidx);
		  xridx(ii) = static_cast<octave_idx_type> (r (ri_scalar ? 0 : iidx));
		} 
	      else 
		{
		  if (sum_terms)
		    xdata(ii) += a (cf_scalar ? 0 : iidx);
		  else
		    xdata(ii) =  a (cf_scalar ? 0 : iidx);
		}
	      prev_rval = rval;
	      prev_cval = cval;
	    } 

	  while (cx < nc) 
	    xcidx (++cx) = ii + 1;
	}
    }
}

template <class T>
Sparse<T>::Sparse (const Array<T>& a, const Array<double>& r, 
		   const Array<double>& c, octave_idx_type nr,
		   octave_idx_type nc, bool sum_terms)
  : dimensions (dim_vector (nr, nc)), idx (0), idx_count (0)
{
  octave_idx_type a_len = a.length ();
  octave_idx_type r_len = r.length ();
  octave_idx_type c_len = c.length ();
  bool ri_scalar = (r_len == 1); 
  bool ci_scalar = (c_len == 1);
  bool cf_scalar = (a_len == 1);

  if ((a_len != r_len && !cf_scalar && !ri_scalar) ||
      (a_len != c_len && !cf_scalar && !ci_scalar) ||
      (r_len != c_len && !ri_scalar && !ci_scalar) || nr < 0 || nc < 0)
    {
      (*current_liboctave_error_handler)
	("Sparse::Sparse (const Array<T>&, const Array<double>&, ...): dimension mismatch");
      rep = nil_rep ();
      dimensions = dim_vector (0, 0);
    }
  else
    {
      octave_idx_type max_nzmx = (r_len > c_len ? r_len : c_len);
  
      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl *, sidx, max_nzmx);
      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl, sidxX, max_nzmx);

      for (octave_idx_type i = 0; i < max_nzmx; i++)
	sidx[i] = &sidxX[i];

      octave_idx_type actual_nzmx = 0;
      octave_quit ();

      for (octave_idx_type i = 0; i < max_nzmx; i++) 
	{
	  octave_idx_type rowidx = static_cast<octave_idx_type> (ri_scalar ? r(0) : r(i));
	  octave_idx_type colidx = static_cast<octave_idx_type> (ci_scalar ? c(0) : c(i));
	  if (rowidx < nr && rowidx >= 0 &&
	      colidx < nc && colidx >= 0 ) 
	    {
	      if ( a (cf_scalar ? 0 : i ) != T ()) 
		{
		  sidx[actual_nzmx]->r = rowidx;
		  sidx[actual_nzmx]->c = colidx;
		  sidx[actual_nzmx]->idx = i;
		  actual_nzmx++;	
		}
	    }
	  else 
	    {
	      (*current_liboctave_error_handler)
		("Sparse::Sparse : index (%d,%d) out of range", 
		 rowidx + 1, colidx + 1);
	      rep = nil_rep ();
	      dimensions = dim_vector (0, 0);
	      return;
	    }
	}

      if (actual_nzmx == 0)
	rep = new typename Sparse<T>::SparseRep (nr, nc);
      else
	{
	  octave_quit ();
	  octave_sort<octave_sparse_sort_idxl *> 
	    lsort (octave_sparse_sidxl_comp);

	  lsort.sort (sidx, actual_nzmx);
	  octave_quit ();

	  // Now count the unique non-zero values
	  octave_idx_type real_nzmx = 1;
	  for (octave_idx_type i = 1; i < actual_nzmx; i++) 
	    if (sidx[i-1]->r != sidx[i]->r || sidx[i-1]->c != sidx[i]->c) 
	      real_nzmx++;

	  rep = new typename Sparse<T>::SparseRep (nr, nc, real_nzmx);

	  octave_idx_type cx = 0;
	  octave_idx_type prev_rval = -1;
	  octave_idx_type prev_cval = -1;
	  octave_idx_type ii = -1;
	  xcidx (0) = 0;
	  for (octave_idx_type i = 0; i < actual_nzmx; i++) 
	    {
	      octave_quit ();
	      octave_idx_type iidx = sidx[i]->idx;
	      octave_idx_type rval = sidx[i]->r;
	      octave_idx_type cval = sidx[i]->c;

	      if (prev_cval < cval || (prev_rval < rval && prev_cval == cval)) 
		{
		  octave_idx_type ci = static_cast<octave_idx_type> (c (ci_scalar ? 0 : iidx));
		  ii++;

		  while (cx < ci) 
		    xcidx (++cx) = ii;
		  xdata(ii) = a (cf_scalar ? 0 : iidx);
		  xridx(ii) = static_cast<octave_idx_type> (r (ri_scalar ? 0 : iidx));
		} 
	      else 
		{
		  if (sum_terms)
		    xdata(ii) += a (cf_scalar ? 0 : iidx);
		  else
		    xdata(ii) =  a (cf_scalar ? 0 : iidx);
		}
	      prev_rval = rval;
	      prev_cval = cval;
	    } 

	  while (cx < nc) 
	    xcidx (++cx) = ii + 1;
	}
    }
}

template <class T>
Sparse<T>::Sparse (const Array2<T>& a)
  : dimensions (a.dims ()), idx (0), idx_count (0)
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
Sparse<T>::resize_no_fill (const dim_vector& dv)
{
  octave_idx_type n = dv.length ();

  if (n != 2)
    {
      (*current_liboctave_error_handler) ("sparse array must be 2-D");
      return;
    }

  resize_no_fill (dv(0), dv(1));
}

template <class T>
void
Sparse<T>::resize_no_fill (octave_idx_type r, octave_idx_type c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (ndims () == 0)
    dimensions = dim_vector (0, 0);

  if (r == dim1 () && c == dim2 ())
    return;

  typename Sparse<T>::SparseRep *old_rep = rep;

  octave_idx_type nc = cols ();
  octave_idx_type nr = rows ();

  if (nnz () == 0 || r == 0 || c == 0)
    // Special case of redimensioning to/from a sparse matrix with 
    // no elements
    rep = new typename Sparse<T>::SparseRep (r, c);
  else
    {
      octave_idx_type n = 0;
      Sparse<T> tmpval;
      if (r >= nr)
	{
	  if (c > nc)
	    n = xcidx(nc);
	  else
	    n = xcidx(c);

	  tmpval = Sparse<T> (r, c, n);

	  if (c > nc)
	    {
	      for (octave_idx_type i = 0; i < nc + 1; i++)
		tmpval.cidx(i) = xcidx(i);
	      for (octave_idx_type i = nc + 1; i < c + 1; i++)
		tmpval.cidx(i) = tmpval.cidx(i-1);
	    }
	  else if (c <= nc)
	    for (octave_idx_type i = 0; i < c + 1; i++)
	      tmpval.cidx(i) = xcidx(i);
	  
	  for (octave_idx_type i = 0; i < n; i++)
	    {
	      tmpval.data(i) = xdata(i);
	      tmpval.ridx(i) = xridx(i);
	    }
	}
      else
	{
	  // Count how many non zero terms before we do anything
	  octave_idx_type min_nc = (c < nc ? c : nc);
	  for (octave_idx_type i = 0; i < min_nc; i++)
	    for (octave_idx_type j = xcidx(i); j < xcidx(i+1); j++)
	      if (xridx(j) < r)
		n++;

	  if (n)
	    {
	      // Now that we know the size we can do something
	      tmpval = Sparse<T> (r, c, n);

	      tmpval.cidx(0);
	      for (octave_idx_type i = 0, ii = 0; i < min_nc; i++)
		{
		  for (octave_idx_type j = xcidx(i); j < xcidx(i+1); j++)
		    if (xridx(j) < r)
		      {
			tmpval.data(ii) = xdata(j);
			tmpval.ridx(ii++) = xridx(j);
		      }
		  tmpval.cidx(i+1) = ii;
		}
	      if (c > min_nc)
		for (octave_idx_type i = nc; i < c; i++)
		  tmpval.cidx(i+1) = tmpval.cidx(i);
	    }
	  else
	    tmpval = Sparse<T> (r, c);
	}

      rep = tmpval.rep;
      rep->count++;
    }

  dimensions = dim_vector (r, c);

  if (--old_rep->count <= 0)
    delete old_rep;
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

template <class T>
void
Sparse<T>::maybe_delete_elements (idx_vector& idx_arg)
{
  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr == 0 && nc == 0)
    return;

  octave_idx_type n;
  if (nr == 1)
    n = nc;
  else if (nc == 1)
    n = nr;
  else
    {
      // Reshape to row vector for Matlab compatibility.

      n = nr * nc;
      nr = 1;
      nc = n;
    }

  if (idx_arg.is_colon_equiv (n, 1))
    {
      // Either A(:) = [] or A(idx) = [] with idx enumerating all
      // elements, so we delete all elements and return [](0x0).  To
      // preserve the orientation of the vector, you have to use
      // A(idx,:) = [] (delete rows) or A(:,idx) (delete columns).

      resize_no_fill (0, 0);
      return;
    }

  idx_arg.sort (true);

  octave_idx_type num_to_delete = idx_arg.length (n);

  if (num_to_delete != 0)
    {
      octave_idx_type new_n = n;
      octave_idx_type new_nnz = nnz ();

      octave_idx_type iidx = 0;

      const Sparse<T> tmp (*this);

      for (octave_idx_type i = 0; i < n; i++)
	{
	  octave_quit ();

	  if (i == idx_arg.elem (iidx))
	    {
	      iidx++;
	      new_n--;

	      if (tmp.elem (i) != T ())
		new_nnz--;

	      if (iidx == num_to_delete)
		break;
	    }
	}

      if (new_n > 0)
	{
	  rep->count--;

	  if (nr == 1)
	    rep = new typename Sparse<T>::SparseRep (1, new_n, new_nnz);
	  else
	    rep = new typename Sparse<T>::SparseRep (new_n, 1, new_nnz);

	  octave_idx_type ii = 0;
	  octave_idx_type jj = 0;
	  iidx = 0;
	  for (octave_idx_type i = 0; i < n; i++)
	    {
	      octave_quit ();

	      if (iidx < num_to_delete && i == idx_arg.elem (iidx))
		iidx++;
	      else
		{
		  T el = tmp.elem (i);
		  if (el != T ())
		    {
		      data(ii) = el;
		      ridx(ii++) = jj;
		    }
		  jj++;
		}
	    }

	  dimensions.resize (2);

	  if (nr == 1)
	    {
	      ii = 0;
	      cidx(0) = 0;
	      for (octave_idx_type i = 0; i < new_n; i++)
		{
		  octave_quit ();
		  if (ridx(ii) == i)
		    ridx(ii++) = 0;
		  cidx(i+1) = ii;
		}

	      dimensions(0) = 1;
	      dimensions(1) = new_n;
	    }
	  else
	    {
	      cidx(0) = 0;
	      cidx(1) = new_nnz;
	      dimensions(0) = new_n;
	      dimensions(1) = 1;
	    }
	}
      else
	(*current_liboctave_error_handler)
	  ("A(idx) = []: index out of range");
    }
}

template <class T>
void
Sparse<T>::maybe_delete_elements (idx_vector& idx_i, idx_vector& idx_j)
{
  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr == 0 && nc == 0)
    return;

  if (idx_i.is_colon ())
    {
      if (idx_j.is_colon ())
	{
	  // A(:,:) -- We are deleting columns and rows, so the result
	  // is [](0x0).

	  resize_no_fill (0, 0);
	  return;
	}

      if (idx_j.is_colon_equiv (nc, 1))
	{
	  // A(:,j) -- We are deleting columns by enumerating them,
	  // If we enumerate all of them, we should have zero columns
	  // with the same number of rows that we started with.

	  resize_no_fill (nr, 0);
	  return;
	}
    }

  if (idx_j.is_colon () && idx_i.is_colon_equiv (nr, 1))
    {
      // A(i,:) -- We are deleting rows by enumerating them.  If we
      // enumerate all of them, we should have zero rows with the
      // same number of columns that we started with.

      resize_no_fill (0, nc);
      return;
    }

  if (idx_i.is_colon_equiv (nr, 1))
    {
      if (idx_j.is_colon_equiv (nc, 1))
	resize_no_fill (0, 0);
      else
	{
	  idx_j.sort (true);

	  octave_idx_type num_to_delete = idx_j.length (nc);

	  if (num_to_delete != 0)
	    {
	      if (nr == 1 && num_to_delete == nc)
		resize_no_fill (0, 0);
	      else
		{
		  octave_idx_type new_nc = nc;
		  octave_idx_type new_nnz = nnz ();

		  octave_idx_type iidx = 0;

		  for (octave_idx_type j = 0; j < nc; j++)
		    {
		      octave_quit ();

		      if (j == idx_j.elem (iidx))
			{
			  iidx++;
			  new_nc--;
			  
			  new_nnz -= cidx(j+1) - cidx(j);

			  if (iidx == num_to_delete)
			    break;
			}
		    }

		  if (new_nc > 0)
		    {
		      const Sparse<T> tmp (*this);
		      --rep->count;
		      rep = new typename Sparse<T>::SparseRep (nr, new_nc, 
							       new_nnz);
		      octave_idx_type ii = 0;
		      octave_idx_type jj = 0;
		      iidx = 0;
		      cidx(0) = 0;
		      for (octave_idx_type j = 0; j < nc; j++)
			{
			  octave_quit ();

			  if (iidx < num_to_delete && j == idx_j.elem (iidx))
			    iidx++;
			  else
			    {
			      for (octave_idx_type i = tmp.cidx(j); 
				   i < tmp.cidx(j+1); i++)
				{
				  data(jj) = tmp.data(i);
				  ridx(jj++) = tmp.ridx(i);
				}
			      cidx(++ii) = jj;
			    }
			}

		      dimensions.resize (2);
		      dimensions(1) = new_nc;
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
  else if (idx_j.is_colon_equiv (nc, 1))
    {
      if (idx_i.is_colon_equiv (nr, 1))
	resize_no_fill (0, 0);
      else
	{
	  idx_i.sort (true);

	  octave_idx_type num_to_delete = idx_i.length (nr);

	  if (num_to_delete != 0)
	    {
	      if (nc == 1 && num_to_delete == nr)
		resize_no_fill (0, 0);
	      else
		{
		  octave_idx_type new_nr = nr;
		  octave_idx_type new_nnz = nnz ();

		  octave_idx_type iidx = 0;

		  for (octave_idx_type i = 0; i < nr; i++)
		    {
		      octave_quit ();

		      if (i == idx_i.elem (iidx))
			{
			  iidx++;
			  new_nr--;
			  
			  for (octave_idx_type j = 0; j < nnz (); j++)
			    if (ridx(j) == i)
			      new_nnz--;

			  if (iidx == num_to_delete)
			    break;
			}
		    }

		  if (new_nr > 0)
		    {
		      const Sparse<T> tmp (*this);
		      --rep->count;
		      rep = new typename Sparse<T>::SparseRep (new_nr, nc, 
							       new_nnz);

		      octave_idx_type jj = 0;
		      cidx(0) = 0;
		      for (octave_idx_type i = 0; i < nc; i++)
			{
			  iidx = 0;
			  for (octave_idx_type j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
			    {
			      octave_quit ();

			      octave_idx_type ri = tmp.ridx(j);

			      while (iidx < num_to_delete && 
				     ri > idx_i.elem (iidx))
				{
				  iidx++;
				}

			      if (iidx == num_to_delete ||
				  ri != idx_i.elem(iidx))
				{
				  data(jj) = tmp.data(j);
				  ridx(jj++) = ri - iidx;
				}
			    }
			  cidx(i+1) = jj;
			}

		      dimensions.resize (2);
		      dimensions(0) = new_nr;
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
}

template <class T>
void
Sparse<T>::maybe_delete_elements (Array<idx_vector>& ra_idx)
{
  if (ra_idx.length () == 1)
    maybe_delete_elements (ra_idx(0));
  else if (ra_idx.length () == 2)
    maybe_delete_elements (ra_idx(0), ra_idx(1));
  else
    (*current_liboctave_error_handler) 
      ("range error for maybe_delete_elements");
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
Sparse<T>::index (idx_vector& idx_arg, int resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type orig_len = nr * nc;

  dim_vector idx_orig_dims = idx_arg.orig_dimensions ();

  octave_idx_type idx_orig_rows = idx_arg.orig_rows ();
  octave_idx_type idx_orig_columns = idx_arg.orig_columns ();

  if (idx_orig_dims.length () > 2)
    (*current_liboctave_error_handler)
      ("Sparse<T>::index: Can not index Sparse<T> with an N-D Array");
  else if (idx_arg.is_colon ())
    {
      // Fast magic colon processing.
      retval = Sparse<T> (nr * nc, 1, nz);

      for (octave_idx_type i = 0; i < nc; i++)
	for (octave_idx_type j = cidx(i); j < cidx(i+1); j++)
	  {
	    octave_quit ();
	    retval.xdata(j) = data(j); 
	    retval.xridx(j) = ridx(j) + i * nr;
	  }
      retval.xcidx(0) = 0;
      retval.xcidx(1) = nz;
    }
  else if (nr == 1 && nc == 1)
    {
      // You have to be pretty sick to get to this bit of code,
      // since you have a scalar stored as a sparse matrix, and
      // then want to make a dense matrix with sparse 
      // representation. Ok, we'll do it, but you deserve what 
      // you get!!
      octave_idx_type n = idx_arg.freeze (length (), "sparse vector", resize_ok);
      if (n == 0)

	  retval = Sparse<T> (idx_orig_dims);
      else if (nz < 1)
	if (n >= idx_orig_dims.numel ())
	  retval = Sparse<T> (idx_orig_dims);
	else
	  retval = Sparse<T> (dim_vector (n, 1));
      else if (n >= idx_orig_dims.numel ())
	{
	  T el = elem (0);
	  octave_idx_type new_nr = idx_orig_rows;
	  octave_idx_type new_nc = idx_orig_columns;
	  for (octave_idx_type i = 2; i < idx_orig_dims.length (); i++)
	    new_nc *= idx_orig_dims (i);
		
	  retval = Sparse<T> (new_nr, new_nc, idx_arg.ones_count ());

	  octave_idx_type ic = 0;
	  for (octave_idx_type i = 0; i < n; i++)
	    {
	      if (i % new_nr == 0)
		retval.xcidx(i / new_nr) = ic;

	      octave_idx_type ii = idx_arg.elem (i);
	      if (ii == 0)
		{
		  octave_quit ();
		  retval.xdata(ic) = el;
		  retval.xridx(ic++) = i % new_nr;
		}
	    }
	  retval.xcidx (new_nc) = ic;
	}
      else
	{
	  T el = elem (0);
	  retval = Sparse<T> (n, 1, nz);
  	 
	  for (octave_idx_type i = 0; i < nz; i++) 
	    {
	      octave_quit ();
	      retval.xdata(i) = el;
	      retval.xridx(i) = i;
	    }
	  retval.xcidx(0) = 0; 	 
	  retval.xcidx(1) = n; 	 
	}
    }
  else if (nr == 1 || nc == 1)
    {
      // If indexing a vector with a matrix, return value has same
      // shape as the index.  Otherwise, it has same orientation as
      // indexed object.
      octave_idx_type len = length ();
      octave_idx_type n = idx_arg.freeze (len, "sparse vector", resize_ok);

      if (n == 0)
	if (nr == 1)
	  retval = Sparse<T> (dim_vector (1, 0));
	else
	  retval = Sparse<T> (dim_vector (0, 1));
      else if (nz < 1)
	if (idx_orig_rows == 1 || idx_orig_columns == 1)
	  retval = Sparse<T> ((nr == 1 ? 1 : n), (nr == 1 ? n : 1));
	else
	  retval = Sparse<T> (idx_orig_dims);
      else
	{

	  octave_idx_type new_nzmx = 0;
	  if (nr == 1)
	    for (octave_idx_type i = 0; i < n; i++)
	      {
		octave_quit ();

		octave_idx_type ii = idx_arg.elem (i);
		if (ii < len)
		  if (cidx(ii) != cidx(ii+1))
		    new_nzmx++;
	      }
	  else
	    for (octave_idx_type i = 0; i < n; i++)
	      {
		octave_idx_type ii = idx_arg.elem (i);
		if (ii < len)
		  for (octave_idx_type j = 0; j < nz; j++)
		    {
		      octave_quit ();

		      if (ridx(j) == ii)
			new_nzmx++;
		      if (ridx(j) >= ii)
			break;
		    }
	      }

	  if (idx_orig_rows == 1 || idx_orig_columns == 1)
	    {
	      if (nr == 1)
		{
		  retval = Sparse<T> (1, n, new_nzmx);
		  octave_idx_type jj = 0;
		  retval.xcidx(0) = 0;
		  for (octave_idx_type i = 0; i < n; i++)
		    {
		      octave_quit ();

		      octave_idx_type ii = idx_arg.elem (i);
		      if (ii < len)
			if (cidx(ii) != cidx(ii+1))
			  {
			    retval.xdata(jj) = data(cidx(ii));
			    retval.xridx(jj++) = 0;
			  }
		      retval.xcidx(i+1) = jj;
		    }
		}
	      else
		{
		  retval = Sparse<T> (n, 1, new_nzmx);
		  retval.xcidx(0) = 0;
		  retval.xcidx(1) = new_nzmx;
		  octave_idx_type jj = 0;
		  for (octave_idx_type i = 0; i < n; i++)
		    {
		      octave_idx_type ii = idx_arg.elem (i);
		      if (ii < len)
			for (octave_idx_type j = 0; j < nz; j++)
			  {
			    octave_quit ();

			    if (ridx(j) == ii)
			      {
				retval.xdata(jj) = data(j);
				retval.xridx(jj++) = i;
			      }
			    if (ridx(j) >= ii)
			      break;
			  }
		    }
		}
	    }
	  else 
	    {
	      octave_idx_type new_nr;
	      octave_idx_type new_nc;
	      if (n >= idx_orig_dims.numel ())
		{
		  new_nr = idx_orig_rows;
		  new_nc = idx_orig_columns;
		}
	      else
		{
		  new_nr = n;
		  new_nc = 1;
		}

	      retval = Sparse<T> (new_nr, new_nc, new_nzmx);

	      if (nr == 1)
		{
		  octave_idx_type jj = 0;
		  retval.xcidx(0) = 0;
		  for (octave_idx_type i = 0; i < n; i++)
		    {
		      octave_quit ();

		      octave_idx_type ii = idx_arg.elem (i);
		      if (ii < len)
			if (cidx(ii) != cidx(ii+1))
			  {
			    retval.xdata(jj) = data(cidx(ii));
			    retval.xridx(jj++) = 0;
			  }
		      retval.xcidx(i/new_nr+1) = jj;
		    }
		}
	      else
		{
		  octave_idx_type jj = 0;
		  retval.xcidx(0) = 0;
		  for (octave_idx_type i = 0; i < n; i++)
		    {
		      octave_idx_type ii = idx_arg.elem (i);
		      if (ii < len)
			for (octave_idx_type j = 0; j < nz; j++)
			  {
			    octave_quit ();

			    if (ridx(j) == ii)
			      {
				retval.xdata(jj) = data(j);
				retval.xridx(jj++) = i;
			      }
			    if (ridx(j) >= ii)
			      break;
			  }
		      retval.xcidx(i/new_nr+1) = jj;
		    }
		}
	    }
	}
    }
  else
    {
      (*current_liboctave_warning_with_id_handler) 
	("Octave:fortran-indexing", "single index used for sparse matrix");

      // This code is only for indexing matrices.  The vector
      // cases are handled above.

      idx_arg.freeze (nr * nc, "matrix", resize_ok);

      if (idx_arg)
	{
	  octave_idx_type result_nr = idx_orig_rows;
	  octave_idx_type result_nc = idx_orig_columns;

	  if (nz < 1)
	    retval = Sparse<T> (result_nr, result_nc);
	  else
	    {
	      // Count number of non-zero elements
	      octave_idx_type new_nzmx = 0;
	      octave_idx_type kk = 0;
	      for (octave_idx_type j = 0; j < result_nc; j++)
		{
		  for (octave_idx_type i = 0; i < result_nr; i++)
		    {
		      octave_quit ();
		      
		      octave_idx_type ii = idx_arg.elem (kk++);
		      if (ii < orig_len)
			{
			  octave_idx_type fr = ii % nr;
			  octave_idx_type fc = (ii - fr) / nr;
			  for (octave_idx_type k = cidx(fc); k < cidx(fc+1); k++)
			    {
			      if (ridx(k) == fr)
				new_nzmx++;
			      if (ridx(k) >= fr)
				break;
			    }
			}
		    }
		}
	      
	      retval = Sparse<T> (result_nr, result_nc, new_nzmx);

	      kk = 0;
	      octave_idx_type jj = 0;
	      retval.xcidx(0) = 0;
	      for (octave_idx_type j = 0; j < result_nc; j++)
		{
		  for (octave_idx_type i = 0; i < result_nr; i++)
		    {
		      octave_quit ();

		      octave_idx_type ii = idx_arg.elem (kk++);
		      if (ii < orig_len)
			{
			  octave_idx_type fr = ii % nr;
			  octave_idx_type fc = (ii - fr) / nr;
			  for (octave_idx_type k = cidx(fc); k < cidx(fc+1); k++)
			    {
			      if (ridx(k) == fr)
				{
				  retval.xdata(jj) = data(k);
				  retval.xridx(jj++) = i;
				}
			      if (ridx(k) >= fr)
				break;
			    }
			}
		    }
		  retval.xcidx(j+1) = jj;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
    }

  return retval;
}

struct 
idx_node 
{
  octave_idx_type i;
  struct idx_node *next;
};		    

template <class T>
Sparse<T>
Sparse<T>::index (idx_vector& idx_i, idx_vector& idx_j, int resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  octave_idx_type n = idx_i.freeze (nr, "row", resize_ok);
  octave_idx_type m = idx_j.freeze (nc, "column", resize_ok);

  if (idx_i && idx_j)
    {
      if (idx_i.orig_empty () || idx_j.orig_empty () || n == 0 || m == 0)
	{
	  retval.resize_no_fill (n, m);
	}
      else 
	{
	  int idx_i_colon = idx_i.is_colon_equiv (nr);
	  int idx_j_colon = idx_j.is_colon_equiv (nc);

	  if (idx_i_colon && idx_j_colon)
	    {
	      retval = *this;
	    }
	  else
	    {
	      // Identify if the indices have any repeated values
	      bool permutation = true;

	      OCTAVE_LOCAL_BUFFER (octave_idx_type, itmp, 
				   (nr > nc ? nr : nc));
	      octave_sort<octave_idx_type> lsort;

	      if (n > nr || m > nc)
		permutation = false;

	      if (permutation && ! idx_i_colon)
		{
		  // Can't use something like
		  //   idx_vector tmp_idx = idx_i;
		  //   tmp_idx.sort (true);
		  //   if (tmp_idx.length(nr) != n)
		  //       permutation = false;
		  // here as there is no make_unique function 
		  // for idx_vector type.
		  for (octave_idx_type i = 0; i < n; i++)
		    itmp [i] = idx_i.elem (i);
		  lsort.sort (itmp, n);
		  for (octave_idx_type i = 1; i < n; i++)
		    if (itmp[i-1] == itmp[i])
		      {
			permutation = false;
			break;
		      }
		}
	      if (permutation && ! idx_j_colon)
		{
		  for (octave_idx_type i = 0; i < m; i++)
		    itmp [i] = idx_j.elem (i);
		  lsort.sort (itmp, m);
		  for (octave_idx_type i = 1; i < m; i++)
		    if (itmp[i-1] == itmp[i])
		      {
			permutation = false;
			break;
		      }
		}

	      if (permutation)
		{
		  // Special case permutation like indexing for speed
		  retval = Sparse<T> (n, m, nnz ());
		  octave_idx_type *ri = retval.xridx ();
	      
		  std::vector<T> X (n);
		  for (octave_idx_type i = 0; i < nr; i++)
		    itmp [i] = -1;
		  for (octave_idx_type i = 0; i < n; i++)
		    itmp[idx_i.elem(i)] = i;

		  octave_idx_type kk = 0;
		  retval.xcidx(0) = 0;
		  for (octave_idx_type j = 0; j < m; j++)
		    {
		      octave_idx_type jj = idx_j.elem (j);
		      for (octave_idx_type i = cidx(jj); i < cidx(jj+1); i++)
			{
			  octave_quit ();

			  octave_idx_type ii = itmp [ridx(i)];
			  if (ii >= 0)
			    {
			      X [ii] = data (i);
			      retval.xridx (kk++) = ii;
			    }
			}
		      lsort.sort (ri + retval.xcidx (j), kk - retval.xcidx (j));
		      for (octave_idx_type p = retval.xcidx (j); p < kk; p++)
			retval.xdata (p) = X [retval.xridx (p)]; 
		      retval.xcidx(j+1) = kk;
		    }
		  retval.maybe_compress ();
		}
	      else
		{
		  OCTAVE_LOCAL_BUFFER (struct idx_node, nodes, n); 
		  OCTAVE_LOCAL_BUFFER (octave_idx_type, start_nodes, nr); 

		  for (octave_idx_type i = 0; i < nr; i++)
		    start_nodes[i] = -1;

		  for (octave_idx_type i = 0; i < n; i++)
		    {
		      octave_idx_type ii = idx_i.elem (i);
		      nodes[i].i = i;
		      nodes[i].next = 0;

		      octave_idx_type node = start_nodes[ii];
		      if (node == -1)
			start_nodes[ii] = i;
		      else
			{
			  while (nodes[node].next)
			    node = nodes[node].next->i;
			  nodes[node].next = nodes + i;
			}
		    }

		  // First count the number of non-zero elements
		  octave_idx_type new_nzmx = 0;
		  for (octave_idx_type j = 0; j < m; j++)
		    {
		      octave_idx_type jj = idx_j.elem (j);

		      if (jj < nc)
			{
			  for (octave_idx_type i = cidx(jj); 
			       i < cidx(jj+1); i++)
			    {
			      octave_quit ();

			      octave_idx_type ii = start_nodes [ridx(i)];

			      if (ii >= 0)
				{
				  struct idx_node inode = nodes[ii];
			      
				  while (true)
				    {
				      if (idx_i.elem (inode.i) < nr)
					new_nzmx ++;
				      if (inode.next == 0)
					break;
				      else
					inode = *inode.next;
				    }
				}
			    }
			}
		    }

		  std::vector<T> X (n);
		  retval = Sparse<T> (n, m, new_nzmx);
		  octave_idx_type *ri = retval.xridx ();

		  octave_idx_type kk = 0;
		  retval.xcidx(0) = 0;
		  for (octave_idx_type j = 0; j < m; j++)
		    {
		      octave_idx_type jj = idx_j.elem (j);
		      if (jj < nc)
			{
			  for (octave_idx_type i = cidx(jj); 
			       i < cidx(jj+1); i++)
			    {
			      octave_quit ();

			      octave_idx_type ii = start_nodes [ridx(i)];

			      if (ii >= 0)
				{
				  struct idx_node inode = nodes[ii];
			      
				  while (true)
				    {
				      if (idx_i.elem (inode.i) < nr)
					{
					  X [inode.i] = data (i);
					  retval.xridx (kk++) = inode.i;
					}

				      if (inode.next == 0)
					break;
				      else
					inode = *inode.next;
				    }
				}
			    }
			  lsort.sort (ri + retval.xcidx (j), 
				     kk - retval.xcidx (j));
			  for (octave_idx_type p = retval.xcidx (j); 
			       p < kk; p++)
			    retval.xdata (p) = X [retval.xridx (p)]; 
			  retval.xcidx(j+1) = kk;
			}
		    }
		}
	    }
	}
    }
  // idx_vector::freeze() printed an error message for us.

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::index (Array<idx_vector>& ra_idx, int resize_ok) const
{

  if (ra_idx.length () != 2)
    {
      (*current_liboctave_error_handler) ("range error for index");
      return *this;
    }

  return index (ra_idx (0), ra_idx (1), resize_ok);
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
	  octave_idx_type nz = nzmax ();
	  d = Sparse<T> (n, n, nz);
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
      else 
	{
	  octave_idx_type n = nnr + std::abs (k);
	  octave_idx_type nz = nzmax ();
	  octave_idx_type ii = 0;
	  octave_idx_type ir = ridx(0);
	  d = Sparse<T> (n, n, nz);
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

  return d;
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
