// Template sparse array class
/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <climits>

#include <iostream>
#include <vector>

#include "Array.h"
#include "Array-flags.h"
#include "Array-util.h"
#include "Range.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "lo-sstream.h"
#include "quit.h"

#include "Sparse.h"
#include "sparse-sort.h"
#include "oct-spparms.h"

template <class T>
T&
Sparse<T>::SparseRep::elem (int _r, int _c)
{
  int i;

  if (nnz > 0)
    {
      for (i = c[_c]; i < c[_c + 1]; i++)
	if (r[i] == _r)
	  return d[i];
	else if (r[i] > _r)
	  break;

      // Ok, If we've gotten here, we're in trouble.. Have to create a 
      // new element in the sparse array. This' gonna be slow!!!
      if (c[ncols+1] == nnz)
	{
	  (*current_liboctave_error_handler)
	    ("Sparse::SparseRep::elem (int, int): sparse matrix filled");
	  return *d;
	}

      int to_move = c[ncols] - i;
      if (to_move != 0)
	{
	  for (int j = c[ncols]; j > i; j--)
	    {
	      d[j] = d[j-1];
	      r[j] = r[j-1];
	    }
	}

      for (int j = _c + 1; j < ncols + 1; j++)
	c[j] = c[j] + 1;
      
      d[i] = 0.;
      r[i] = _r;

      return d[i];
    }
  else
    {
      (*current_liboctave_error_handler)
	("Sparse::SparseRep::elem (int, int): sparse matrix filled");
      return *d;
    }
}

template <class T>
T
Sparse<T>::SparseRep::celem (int _r, int _c) const
{
  if (nnz > 0)
    for (int i = c[_c]; i < c[_c + 1]; i++)
      if (r[i] == _r)
	return d[i];
  return T ();
}

template <class T>
void
Sparse<T>::SparseRep::maybe_compress (bool remove_zeros)
{
  int ndel = nnz - c[ncols];
  int nzero = 0;

  if (remove_zeros)
    for (int i = 0; i < nnz - ndel; i++)
      if (d[i] == T ())
	nzero++;

  if (!ndel && !nzero)
    return;

  if (!nzero)
    {
      int new_nnz = nnz - ndel;

      T *new_data = new T [new_nnz];
      for (int i = 0; i < new_nnz; i++)
	new_data[i] = d[i];
      delete [] d;
      d = new_data;

      int *new_ridx = new int [new_nnz];
      for (int i = 0; i < new_nnz; i++)
	new_ridx[i] = r[i];
      delete [] r;
      r = new_ridx;
    }
  else
    {
      int new_nnz = nnz - ndel - nzero;

      T *new_data = new T [new_nnz];
      int *new_ridx = new int [new_nnz];

      int ii = 0;
      int ic = 0;
      for (int j = 0; j < ncols; j++)
	{
	  for (int k = ic; k < c[j+1]; k++)
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

  nnz -= ndel + nzero;
}

template <class T>
void
Sparse<T>::SparseRep::change_length (int nz)
{
  if (nz != nnz)
    {
      int min_nnz = (nz < nnz ? nz : nnz);

      int * new_ridx = new int [nz];
      for (int i = 0; i < min_nnz; i++)
	new_ridx[i] = r[i];

      delete [] r;
      r = new_ridx;

      T * new_data = new T [nz];
      for (int i = 0; i < min_nnz; i++)
	new_data[i] = d[i];

      delete [] d;
      d = new_data;

      if (nz < nnz)
	for (int i = 0; i <= ncols; i++)
	  if (c[i] > nz)
	    c[i] = nz;

      nnz = nz;
    }
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
      
      int nz = nnz ();
      int nc = cols ();
      for (int i = 0; i < nz; i++)
	{
	  xdata (i) = T (a.data (i));
	  xridx (i) = a.ridx (i);
	}
      for (int i = 0; i < nc + 1; i++)
	xcidx (i) = a.cidx (i);
    }
}

template <class T>
Sparse<T>::Sparse (int nr, int nc, T val)
  : rep (new typename Sparse<T>::SparseRep (nr, nc, nr*nc)),
    dimensions (dim_vector (nr, nc)), idx (0), idx_count (0)
{ 

  int ii = 0;
  xcidx (0) = 0;
  for (int j = 0; j < nc; j++)
    {
      for (int i = 0; i < nr; i++)
	{
	  xdata (ii) = val;
	  xridx (ii++) = i;
	} 
      xcidx (j+1) = ii;
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
      int new_nnz = a.nnz ();
      int new_nr = dv (0);
      int new_nc = dv (1);
      int old_nr = old_dims (0);
      int old_nc = old_dims (1);

      rep = new typename Sparse<T>::SparseRep (new_nr, new_nc, new_nnz);

      int kk = 0;
      xcidx(0) = 0;
      for (int i = 0; i < old_nc; i++)
	for (int j = a.cidx(i); j < a.cidx(i+1); j++)
	  {
	    int tmp = i * old_nr + a.ridx(j);
	    int ii = tmp % new_nr;
	    int jj = (tmp - ii) / new_nr; 
	    for (int k = kk; k < jj; k++)
	      xcidx(k+1) = j;
	    kk = jj;
	    xdata(j) = a.data(j);
	    xridx(j) = ii;
	  }
      for (int k = kk; k < new_nc; k++)
	xcidx(k+1) = new_nnz;
    }
}

template <class T>
Sparse<T>::Sparse (const Array<T>& a, const Array<int>& r, 
		   const Array<int>& c, int nr,
		   int nc, bool sum_terms)
  : dimensions (dim_vector (nr, nc)), idx (0), idx_count (0)
{
  int a_len = a.length ();
  int r_len = r.length ();
  int c_len = c.length ();
  bool ri_scalar = (r_len == 1); 
  bool ci_scalar = (c_len == 1);
  bool cf_scalar = (a_len == 1);
  
  if ((a_len != r_len && !cf_scalar && !ri_scalar) ||
      (a_len != c_len && !cf_scalar && !ci_scalar) ||
      (r_len != c_len && !ri_scalar && !ci_scalar) || nr < 0 || nc < 0)
    {
      (*current_liboctave_error_handler)
	("Sparse::Sparse (const Array<T>&, const Array<int>&, ...): dimension mismatch");
      rep = nil_rep ();
      dimensions = dim_vector (0, 0);
    }
  else
    {
      int max_nnz = (r_len > c_len ? r_len : c_len);

      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl *, sidx, max_nnz);
      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl, sidxX, max_nnz);

      for (int i = 0; i < max_nnz; i++)
	sidx[i] = &sidxX[i];

      int actual_nnz = 0;
      OCTAVE_QUIT;
      for (int i = 0; i < max_nnz; i++) 
	{
	  int rowidx =  (ri_scalar ? r(0) : r(i));
	  int colidx = (ci_scalar ? c(0) : c(i));
	  if (rowidx < nr && rowidx >= 0 &&
	      colidx < nc && colidx >= 0 ) 
	    {
	      if ( a (cf_scalar ? 0 : i ) != T ()) 
		{
		  sidx[actual_nnz]->r = rowidx;
		  sidx[actual_nnz]->c = colidx;
		  sidx[actual_nnz]->idx = i;
		  actual_nnz++;	
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
  
      if (actual_nnz == 0)
	rep = new typename Sparse<T>::SparseRep (nr, nc);
      else
	{
	  OCTAVE_QUIT;
	  octave_sort<octave_sparse_sort_idxl *> 
	    sort (octave_sparse_sidxl_comp);

	  sort.sort (sidx, actual_nnz);
	  OCTAVE_QUIT;

	  // Now count the unique non-zero values
	  int real_nnz = 1;
	  for (int i = 1; i < actual_nnz; i++) 
	    if (sidx[i-1]->r != sidx[i]->r || sidx[i-1]->c != sidx[i]->c) 
	      real_nnz++;

	  rep = new typename Sparse<T>::SparseRep (nr, nc, real_nnz);

	  int cx = 0;
	  int prev_rval = -1;
	  int prev_cval = -1;
	  int ii = -1;
	  xcidx (0) = 0;
	  for (int i = 0; i < actual_nnz; i++) 
	    {
	      OCTAVE_QUIT;
	      int iidx = sidx[i]->idx;
	      int rval = sidx[i]->r;
	      int cval = sidx[i]->c;

	      if (prev_cval < cval || (prev_rval < rval && prev_cval == cval)) 
		{
		  int ci = static_cast<int> (c (ci_scalar ? 0 : iidx));
		  ii++;
		  while (cx < ci) 
		    xcidx (++cx) = ii;
		  xdata(ii) = a (cf_scalar ? 0 : iidx);
		  xridx(ii) = static_cast<int> (r (ri_scalar ? 0 : iidx));
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
		   const Array<double>& c, int nr,
		   int nc, bool sum_terms)
  : dimensions (dim_vector (nr, nc)), idx (0), idx_count (0)
{
  int a_len = a.length ();
  int r_len = r.length ();
  int c_len = c.length ();
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
      int max_nnz = (r_len > c_len ? r_len : c_len);
  
      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl *, sidx, max_nnz);
      OCTAVE_LOCAL_BUFFER (octave_sparse_sort_idxl, sidxX, max_nnz);

      for (int i = 0; i < max_nnz; i++)
	sidx[i] = &sidxX[i];

      int actual_nnz = 0;
      OCTAVE_QUIT;

      for (int i = 0; i < max_nnz; i++) 
	{
	  int rowidx = static_cast<int> (ri_scalar ? r(0) : r(i));
	  int colidx = static_cast<int> (ci_scalar ? c(0) : c(i));
	  if (rowidx < nr && rowidx >= 0 &&
	      colidx < nc && colidx >= 0 ) 
	    {
	      if ( a (cf_scalar ? 0 : i ) != T ()) 
		{
		  sidx[actual_nnz]->r = rowidx;
		  sidx[actual_nnz]->c = colidx;
		  sidx[actual_nnz]->idx = i;
		  actual_nnz++;	
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

      if (actual_nnz == 0)
	rep = new typename Sparse<T>::SparseRep (nr, nc);
      else
	{
	  OCTAVE_QUIT;
	  octave_sort<octave_sparse_sort_idxl *> 
	    sort (octave_sparse_sidxl_comp);

	  sort.sort (sidx, actual_nnz);
	  OCTAVE_QUIT;

	  // Now count the unique non-zero values
	  int real_nnz = 1;
	  for (int i = 1; i < actual_nnz; i++) 
	    if (sidx[i-1]->r != sidx[i]->r || sidx[i-1]->c != sidx[i]->c) 
	      real_nnz++;

	  rep = new typename Sparse<T>::SparseRep (nr, nc, real_nnz);

	  int cx = 0;
	  int prev_rval = -1;
	  int prev_cval = -1;
	  int ii = -1;
	  xcidx (0) = 0;
	  for (int i = 0; i < actual_nnz; i++) 
	    {
	      OCTAVE_QUIT;
	      int iidx = sidx[i]->idx;
	      int rval = sidx[i]->r;
	      int cval = sidx[i]->c;

	      if (prev_cval < cval || (prev_rval < rval && prev_cval == cval)) 
		{
		  int ci = static_cast<int> (c (ci_scalar ? 0 : iidx));
		  ii++;

		  while (cx < ci) 
		    xcidx (++cx) = ii;
		  xdata(ii) = a (cf_scalar ? 0 : iidx);
		  xridx(ii) = static_cast<int> (r (ri_scalar ? 0 : iidx));
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
  int nr = rows ();
  int nc = cols ();
  int len = a.length ();
  int new_nnz = 0;

  // First count the number of non-zero terms
  for (int i = 0; i < len; i++)
    if (a(i) != T ())
      new_nnz++;

  rep = new typename Sparse<T>::SparseRep (nr, nc, new_nnz);

  int ii = 0;
  xcidx(0) = 0;
  for (int j = 0; j < nc; j++)
    {
      for (int i = 0; i < nr; i++)
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
      int nr = rows ();
      int nc = cols ();
      int len = a.length ();
      int new_nnz = 0;

      // First count the number of non-zero terms
      for (int i = 0; i < len; i++)
	if (a(i) != T ())
	  new_nnz++;

      rep = new typename Sparse<T>::SparseRep (nr, nc, new_nnz);

      int ii = 0;
      xcidx(0) = 0;
      for (int j = 0; j < nc; j++)
	{
	  for (int i = 0; i < nr; i++)
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
int
Sparse<T>::compute_index (const Array<int>& ra_idx) const
{
  int retval = -1;

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
      ("Sparse<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, int n) const
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, int n)
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  static T foo;
  return foo;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, int i, int j) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, int i, int j)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  static T foo;
  return foo;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, const Array<int>& ra_idx) const
{
  OSSTREAM buf;

  buf << fcn << " (";

  int n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (int i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  buf << OSSTREAM_ENDS;

  (*current_liboctave_error_handler) (OSSTREAM_C_STR (buf));

  OSSTREAM_FREEZE (buf);

  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, const Array<int>& ra_idx)
{
  OSSTREAM buf;

  buf << fcn << " (";

  int n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (int i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  buf << OSSTREAM_ENDS;

  (*current_liboctave_error_handler) (OSSTREAM_C_STR (buf));

  OSSTREAM_FREEZE (buf);

  static T foo;
  return foo;
}

template <class T>
Sparse<T>
Sparse<T>::reshape (const dim_vector& new_dims) const
{
  Sparse<T> retval;

  if (dimensions != new_dims)
    {
      if (dimensions.numel () == new_dims.numel ())
	{
	  int new_nnz = nnz ();
	  int new_nr = new_dims (0);
	  int new_nc = new_dims (1);
	  int old_nr = rows ();
	  int old_nc = cols ();
	  retval = Sparse<T> (new_nr, new_nc, new_nnz);

	  int kk = 0;
	  retval.xcidx(0) = 0;
	  for (int i = 0; i < old_nc; i++)
	    for (int j = cidx(i); j < cidx(i+1); j++)
	      {
		int tmp = i * old_nr + ridx(j);
		int ii = tmp % new_nr;
		int jj = (tmp - ii) / new_nr; 
		for (int k = kk; k < jj; k++)
		  retval.xcidx(k+1) = j;
		kk = jj;
		retval.xdata(j) = data(j);
		retval.xridx(j) = ii;
	      }
	  retval.xcidx(new_nc) = new_nnz;
	}
      else
	(*current_liboctave_error_handler) ("reshape: size mismatch");
    }
  else
    retval = *this;

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::permute (const Array<int>& perm_vec, bool) const
{
  dim_vector dv = dims ();
  dim_vector dv_new;

  int nd = dv.length ();

  dv_new.resize (nd);

  // Need this array to check for identical elements in permutation array.
  Array<bool> checked (nd, false);

  // Find dimension vector of permuted array.
  for (int i = 0; i < nd; i++)
    {
      int perm_el = perm_vec.elem (i);

      if (perm_el > dv.length () || perm_el < 1)
	{
	  (*current_liboctave_error_handler)
	    ("permutation vector contains an invalid element");

	  return Sparse<T> ();
	}

      if (checked.elem(perm_el - 1))
	{
	  (*current_liboctave_error_handler)
	    ("PERM cannot contain identical elements");

	  return Sparse<T> ();
	}
      else
	checked.elem(perm_el - 1) = true;

      dv_new (i) = dv (perm_el - 1);
    }

  if (dv_new == dv)
    return *this;
  else
    return transpose ();
}

template <class T>
void
Sparse<T>::resize_no_fill (const dim_vector& dv)
{
  int n = dv.length ();

  if (n != 2)
    {
      (*current_liboctave_error_handler) ("sparse array must be 2-D");
      return;
    }

  resize_no_fill (dv(0), dv(1));
}

template <class T>
void
Sparse<T>::resize_no_fill (int r, int c)
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

  typename Sparse<T>::SparseRep *old_rep = Sparse<T>::rep;
  int nc = cols ();
  int nr = rows ();

  if (nnz () == 0 || r == 0 || c == 0)
    // Special case of redimensioning to/from a sparse matrix with 
    // no elements
    rep = new typename Sparse<T>::SparseRep (r, c);
  else
    {
      int n = 0;
      Sparse<T> tmpval;
      if (r >= nr)
	{
	  if (c > nc)
	    n = cidx(nc);
	  else
	    n = cidx(c);

	  tmpval = Sparse<T> (r, c, n);

	  if (c > nc)
	    {
	      for (int i = 0; i < nc; i++)
		tmpval.cidx(i) = cidx(i);
	      for (int i = nc+2; i < c; i++)
		tmpval.cidx(i) = tmpval.cidx(i-1);
	    }
	  else if (c <= nc)
	    for (int i = 0; i < c; i++)
	      tmpval.cidx(i) = cidx(i);
	  
	  for (int i = 0; i < n; i++)
	    {
	      tmpval.data(i) = data(i);
	      tmpval.ridx(i) = ridx(i);
	    }
	}
      else
	{
	  // Count how many non zero terms before we do anything
	  for (int i = 0; i < c; i++)
	    for (int j = cidx(i); j < cidx(i+1); j++)
	      if (ridx(j) < r)
		n++;

	  if (n)
	    {
	      // Now that we know the size we can do something
	      tmpval = Sparse<T> (r, c, n);

	      tmpval.cidx(0);
	      for (int i = 0, ii = 0; i < c; i++)
		{
		  for (int j = cidx(i); j < cidx(i+1); j++)
		    if (ridx(j) < r)
		      {
			tmpval.data(ii) = data(j);
			tmpval.ridx(ii++) = ridx(j);
		      }
		  tmpval.cidx(i+1) = ii;
		}
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
Sparse<T>::insert (const Sparse<T>& a, int r, int c)
{
  int a_rows = a.rows ();
  int a_cols = a.cols ();
  int nr = rows ();
  int nc = cols ();

  if (r < 0 || r + a_rows > rows () || c < 0 || c + a_cols > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  // First count the number of elements in the final array
  int nel = cidx(c) + a.nnz ();

  if (c + a_cols < nc)
    nel += cidx(nc) - cidx(c + a_cols);

  for (int i = c; i < c + a_cols; i++)
    for (int j = cidx(i); j < cidx(i+1); j++)
      if (ridx(j) < r || ridx(j) >= r + a_rows)
	nel++;

  Sparse<T> tmp (*this);
  --rep->count;
  rep = new typename Sparse<T>::SparseRep (nr, nc, nel);

  for (int i = 0; i < tmp.cidx(c); i++)
    {
      data(i) = tmp.data(i);
      ridx(i) = tmp.ridx(i);
    }
  for (int i = 0; i < c + 1; i++)
    cidx(i) = tmp.cidx(i);

  int ii = cidx(c);

  for (int i = c; i < c + a_cols; i++)
    {
      OCTAVE_QUIT;

      for (int j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
	if (tmp.ridx(j) < r)
	  {
	    data(ii) = tmp.data(j);
	    ridx(ii++) = tmp.ridx(j);
	  }

      OCTAVE_QUIT;

      for (int j = a.cidx(i-c); j < a.cidx(i-c+1); j++)
	{
	  data(ii) = a.data(j);
	  ridx(ii++) = r + a.ridx(j);
	}

      OCTAVE_QUIT;

      for (int j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
	if (tmp.ridx(j) >= r + a_rows)
	  {
	    data(ii) = tmp.data(j);
	    ridx(ii++) = tmp.ridx(j);
	  }

      cidx(i+1) = ii;
    }

  for (int i = c + a_cols; i < nc; i++)
    {
      for (int j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
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
Sparse<T>::insert (const Sparse<T>& a, const Array<int>& ra_idx)
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

  int nr = rows ();
  int nc = cols ();
  int nz = nnz ();
  Sparse<T> retval (nc, nr, nz);

  retval.cidx(0) = 0;
  for (int i = 0, iidx = 0; i < nr; i++)
    {
      for (int j = 0; j < nc; j++)
	for (int k = cidx(j); k < cidx(j+1); k++)
	  if (ridx(k) == i)
	    {
	      retval.data(iidx) = data(k);
	      retval.ridx(iidx++) = j;
	    }
      retval.cidx(i+1) = iidx;
    }

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
  int nd = ndims ();

  if (! idx && nd > 0)
    idx = new idx_vector [nd];

  if (idx_count < nd)
    {
      idx[idx_count++] = idx_arg;
    }
  else
    {
      idx_vector *new_idx = new idx_vector [idx_count+1];

      for (int i = 0; i < idx_count; i++)
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
  int nr = dim1 ();
  int nc = dim2 ();

  if (nr == 0 && nc == 0)
    return;

  int n;
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

  int num_to_delete = idx_arg.length (n);

  if (num_to_delete != 0)
    {
      int new_n = n;
      int new_nnz = nnz ();

      int iidx = 0;

      const Sparse<T> tmp (*this);

      for (int i = 0; i < n; i++)
	{
	  OCTAVE_QUIT;

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

	  int ii = 0;
	  int jj = 0;
	  iidx = 0;
	  for (int i = 0; i < n; i++)
	    {
	      OCTAVE_QUIT;

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
	      for (int i = 0; i < new_n; i++)
		{
		  OCTAVE_QUIT;
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

  int nr = dim1 ();
  int nc = dim2 ();

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

	  int num_to_delete = idx_j.length (nc);

	  if (num_to_delete != 0)
	    {
	      if (nr == 1 && num_to_delete == nc)
		resize_no_fill (0, 0);
	      else
		{
		  int new_nc = nc;
		  int new_nnz = nnz ();

		  int iidx = 0;

		  for (int j = 0; j < nc; j++)
		    {
		      OCTAVE_QUIT;

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
		      int ii = 0;
		      int jj = 0;
		      iidx = 0;
		      cidx(0) = 0;
		      for (int j = 0; j < nc; j++)
			{
			  OCTAVE_QUIT;

			  if (iidx < num_to_delete && j == idx_j.elem (iidx))
			    iidx++;
			  else
			    {
			      for (int i = tmp.cidx(j); 
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

	  int num_to_delete = idx_i.length (nr);

	  if (num_to_delete != 0)
	    {
	      if (nc == 1 && num_to_delete == nr)
		resize_no_fill (0, 0);
	      else
		{
		  int new_nr = nr;
		  int new_nnz = nnz ();

		  int iidx = 0;

		  for (int i = 0; i < nr; i++)
		    {
		      OCTAVE_QUIT;

		      if (i == idx_i.elem (iidx))
			{
			  iidx++;
			  new_nr--;
			  
			  for (int j = 0; j < nnz (); j++)
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

		      int jj = 0;
		      cidx(0) = 0;
		      for (int i = 0; i < nc; i++)
			{
			  iidx = 0;
			  for (int j = tmp.cidx(i); j < tmp.cidx(i+1); j++)
			    {
			      OCTAVE_QUIT;

			      int ri = tmp.ridx(j);

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

  int nr = dim1 ();
  int nc = dim2 ();
  int nz = nnz ();

  int orig_len = nr * nc;

  dim_vector idx_orig_dims = idx_arg.orig_dimensions ();

  int idx_orig_rows = idx_arg.orig_rows ();
  int idx_orig_columns = idx_arg.orig_columns ();

  if (idx_orig_dims.length () > 2)
    (*current_liboctave_error_handler)
      ("Sparse<T>::index: Can not index Sparse<T> with an N-D Array");
  else if (idx_arg.is_colon ())
    {
      // Fast magic colon processing.
      retval = Sparse<T> (nr * nc, 1, nz);

      for (int i = 0; i < nc; i++)
	for (int j = cidx(i); j < cidx(i+1); j++)
	  {
	    OCTAVE_QUIT;
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
      int n = idx_arg.freeze (length (), "sparse vector", resize_ok);
      if (n == 0)
	if (idx_arg.one_zero_only ())
	  retval = Sparse<T> (dim_vector (0, 0));
	else
	  retval = Sparse<T> (dim_vector (0, 1));
      else if (nz < 1)
	if (n >= idx_orig_dims.numel ())
	  retval = Sparse<T> (idx_orig_dims);
	else
	  retval = Sparse<T> (dim_vector (n, 1));
      else if (n >= idx_orig_dims.numel ())
	{
	  T el = elem (0);
	  int new_nr = idx_orig_rows;
	  int new_nc = idx_orig_columns;
	  for (int i = 2; i < idx_orig_dims.length (); i++)
	    new_nc *= idx_orig_dims (i);
		
	  retval = Sparse<T> (new_nr, new_nc, idx_arg.ones_count ());

	  int ic = 0;
	  for (int i = 0; i < n; i++)
	    {
	      if (i % new_nr == 0)
		retval.xcidx(i % new_nr) = ic;

	      int ii = idx_arg.elem (i);
	      if (ii == 0)
		{
		  OCTAVE_QUIT;
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
  	 
	  for (int i = 0; i < nz; i++) 
	    {
	      OCTAVE_QUIT;
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
      int len = length ();
      int n = idx_arg.freeze (len, "sparse vector", resize_ok);

      if (n == 0)
	if (nr == 1)
	  retval = Sparse<T> (dim_vector (1, 0));
	else
	  retval = Sparse<T> (dim_vector (0, 1));
      else if (nz < 1)
	if ((n != 0 && idx_arg.one_zero_only ())
	    || idx_orig_rows == 1 || idx_orig_columns == 1)
	  retval = Sparse<T> ((nr == 1 ? 1 : n), (nr == 1 ? n : 1));
	else
	  retval = Sparse<T> (idx_orig_dims);
      else
	{

	  int new_nnz = 0;
	  if (nr == 1)
	    for (int i = 0; i < n; i++)
	      {
		OCTAVE_QUIT;

		int ii = idx_arg.elem (i);
		if (ii < len)
		  if (cidx(ii) != cidx(ii+1))
		    new_nnz++;
	      }
	  else
	    for (int i = 0; i < n; i++)
	      {
		int ii = idx_arg.elem (i);
		if (ii < len)
		  for (int j = 0; j < nz; j++)
		    {
		      OCTAVE_QUIT;

		      if (ridx(j) == ii)
			new_nnz++;
		      if (ridx(j) >= ii)
			break;
		    }
	      }

	  if (idx_arg.one_zero_only () || idx_orig_rows == 1 || 
	      idx_orig_columns == 1)
	    {
	      if (nr == 1)
		{
		  retval = Sparse<T> (1, n, new_nnz);
		  int jj = 0;
		  retval.xcidx(0) = 0;
		  for (int i = 0; i < n; i++)
		    {
		      OCTAVE_QUIT;

		      int ii = idx_arg.elem (i);
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
		  retval = Sparse<T> (n, 1, new_nnz);
		  retval.xcidx(0) = 0;
		  retval.xcidx(1) = new_nnz;
		  int jj = 0;
		  for (int i = 0; i < n; i++)
		    {
		      int ii = idx_arg.elem (i);
		      if (ii < len)
			for (int j = 0; j < nz; j++)
			  {
			    OCTAVE_QUIT;

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
	      int new_nr;
	      int new_nc;
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

	      retval = Sparse<T> (new_nr, new_nc, new_nnz);

	      if (nr == 1)
		{
		  int jj = 0;
		  retval.xcidx(0) = 0;
		  for (int i = 0; i < n; i++)
		    {
		      OCTAVE_QUIT;

		      int ii = idx_arg.elem (i);
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
		  int jj = 0;
		  retval.xcidx(0) = 0;
		  for (int i = 0; i < n; i++)
		    {
		      int ii = idx_arg.elem (i);
		      if (ii < len)
			for (int j = 0; j < nz; j++)
			  {
			    OCTAVE_QUIT;

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
      if (liboctave_wfi_flag
	  && ! (idx_arg.one_zero_only ()
		&& idx_orig_rows == nr
		&& idx_orig_columns == nc))
	(*current_liboctave_warning_handler) 
	  ("single index used for sparse matrix");

      // This code is only for indexing matrices.  The vector
      // cases are handled above.

      idx_arg.freeze (nr * nc, "matrix", resize_ok);

      if (idx_arg)
	{
	  int result_nr = idx_orig_rows;
	  int result_nc = idx_orig_columns;

	  if (idx_arg.one_zero_only ())
	    {
	      result_nr = idx_arg.ones_count ();
	      result_nc = (result_nr > 0 ? 1 : 0);
	    }

	  if (nz < 1)
	      retval = Sparse<T> (result_nr, result_nc);
	  else
	    {
	      // Count number of non-zero elements
	      int new_nnz = 0;
	      int kk = 0;
	      for (int j = 0; j < result_nc; j++)
		{
		  for (int i = 0; i < result_nr; i++)
		    {
		      OCTAVE_QUIT;
		      
		      int ii = idx_arg.elem (kk++);
		      if (ii < orig_len)
			{
			  int fr = ii % nr;
			  int fc = (ii - fr) / nr;
			  for (int k = cidx(fc); k < cidx(fc+1); k++)
			    {
			      if (ridx(k) == fr)
				new_nnz++;
			      if (ridx(k) >= fr)
				break;
			    }
			}
		    }
		}
	      
	      retval = Sparse<T> (result_nr, result_nc, new_nnz);

	      kk = 0;
	      int jj = 0;
	      retval.xcidx(0) = 0;
	      for (int j = 0; j < result_nc; j++)
		{
		  for (int i = 0; i < result_nr; i++)
		    {
		      OCTAVE_QUIT;

		      int ii = idx_arg.elem (kk++);
		      if (ii < orig_len)
			{
			  int fr = ii % nr;
			  int fc = (ii - fr) / nr;
			  for (int k = cidx(fc); k < cidx(fc+1); k++)
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

template <class T>
Sparse<T>
Sparse<T>::index (idx_vector& idx_i, idx_vector& idx_j, int resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  int nr = dim1 ();
  int nc = dim2 ();

  int n = idx_i.freeze (nr, "row", resize_ok);
  int m = idx_j.freeze (nc, "column", resize_ok);

  if (idx_i && idx_j)
    {
      if (idx_i.orig_empty () || idx_j.orig_empty () || n == 0 || m == 0)
	{
	  retval.resize_no_fill (n, m);
	}
      else if (idx_i.is_colon_equiv (nr) && idx_j.is_colon_equiv (nc))
	{
	  retval = *this;
	}
      else
	{
	  // First count the number of non-zero elements
	  int new_nnz = 0;
	  for (int j = 0; j < m; j++)
	    {
	      int jj = idx_j.elem (j);
	      for (int i = 0; i < n; i++)
		{
		  OCTAVE_QUIT;

		  int ii = idx_i.elem (i);
		  if (ii < nr && jj < nc)
		    {
		      for (int k = cidx(jj); k < cidx(jj+1); k++)
			{
			  if (ridx(k) == ii)
			    new_nnz++;
			  if (ridx(k) >= ii)
			    break;
			}
		    }
		}
	    }

	  retval = Sparse<T> (n, m, new_nnz);

	  int kk = 0;
	  retval.xcidx(0) = 0;
	  for (int j = 0; j < m; j++)
	    {
	      int jj = idx_j.elem (j);
	      for (int i = 0; i < n; i++)
		{
		  OCTAVE_QUIT;

		  int ii = idx_i.elem (i);
		  if (ii < nr && jj < nc)
		    {
		      for (int k = cidx(jj); k < cidx(jj+1); k++)
			{
			  if (ridx(k) == ii)
			    {
			      retval.xdata(kk) = data(k);
			      retval.xridx(kk++) = i;
			    }
			  if (ridx(k) >= ii)
			    break;
			}
		    }
		}
	      retval.xcidx(j+1) = kk;
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

// XXX FIXME XXX
// Unfortunately numel can overflow for very large but very sparse matrices.
// For now just flag an error when this happens.
template <class LT, class RT>
int
assign1 (Sparse<LT>& lhs, const Sparse<RT>& rhs)
{
  int retval = 1;

  idx_vector *idx_tmp = lhs.get_idx ();

  idx_vector lhs_idx = idx_tmp[0];

  int lhs_len = lhs.numel ();
  int rhs_len = rhs.numel ();

  unsigned EIGHT_BYTE_INT long_lhs_len = 
    static_cast<unsigned EIGHT_BYTE_INT> (lhs.rows ()) *
    static_cast<unsigned EIGHT_BYTE_INT> (lhs.cols ());

  unsigned EIGHT_BYTE_INT long_rhs_len = 
    static_cast<unsigned EIGHT_BYTE_INT> (rhs.rows ()) *
    static_cast<unsigned EIGHT_BYTE_INT> (rhs.cols ());

  if (long_rhs_len != static_cast<unsigned EIGHT_BYTE_INT>(rhs_len) ||
      long_lhs_len != static_cast<unsigned EIGHT_BYTE_INT>(lhs_len))
    {
      (*current_liboctave_error_handler)
	("A(I) = X: Matrix dimensions too large to ensure correct\n",
	 "operation. This is an limitation that should be removed\n",
	 "in the future.");

      lhs.clear_index ();
      return 0;
    }

  int nr = lhs.rows ();
  int nc = lhs.cols ();
  int nz = lhs.nnz ();

  int n = lhs_idx.freeze (lhs_len, "vector", true, liboctave_wrore_flag);

  if (n != 0)
    {
      int max_idx = lhs_idx.max () + 1;
      max_idx = max_idx < lhs_len ? lhs_len : max_idx;

      // Take a constant copy of lhs. This means that elem won't 
      // create missing elements.
      const Sparse<LT> c_lhs (lhs);

      if (rhs_len == n)
	{
	  int new_nnz = lhs.nnz ();

	  // First count the number of non-zero elements
	  for (int i = 0; i < n; i++)
	    {
	      OCTAVE_QUIT;

	      int ii = lhs_idx.elem (i);
	      if (ii < lhs_len && c_lhs.elem(ii) != LT ())
		new_nnz--;
	      if (rhs.elem(i) != RT ())
		new_nnz++;
	    }

	  if (nr > 1)
	    {
	      Sparse<LT> tmp (max_idx, 1, new_nnz);
	      tmp.cidx(0) = 0;
	      tmp.cidx(1) = tmp.nnz ();

	      int i = 0;
	      int ii = 0;
	      if (i < nz)
		ii = c_lhs.ridx(i);

	      int j = 0;
	      int jj = lhs_idx.elem(j);

	      int kk = 0;

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
		      RT rtmp = rhs.elem (j);
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
	      Sparse<LT> tmp (1, max_idx, new_nnz);

	      int i = 0;
	      int ii = 0;
	      while (ii < nc && c_lhs.cidx(ii+1) <= i)
		ii++;

	      int j = 0;
	      int jj = lhs_idx.elem(j);

	      int kk = 0;
	      int ic = 0;

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
		    }
		  else
		    {
		      while (ic <= jj)
			tmp.xcidx (ic++) = kk;

		      RT rtmp = rhs.elem (j);
		      if (rtmp != RT ())
			tmp.xdata (kk) = rtmp;
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
		  tmp.xridx (kk++) = 0;
		}

	      for (int iidx = ic; iidx < max_idx+1; iidx++)
		tmp.xcidx(iidx) = kk;

	      lhs = tmp;
	    }
	}
      else if (rhs_len == 1)
	{
	  int new_nnz = lhs.nnz ();
	  RT scalar = rhs.elem (0);
	  bool scalar_non_zero = (scalar != RT ());

	  // First count the number of non-zero elements
	  if (scalar != RT ())
	    new_nnz += n;
	  for (int i = 0; i < n; i++)
	    {
	      OCTAVE_QUIT;

	      int ii = lhs_idx.elem (i);
	      if (ii < lhs_len && c_lhs.elem(ii) != LT ())
		new_nnz--;
	    }

	  if (nr > 1)
	    {
	      Sparse<LT> tmp (max_idx, 1, new_nnz);
	      tmp.cidx(0) = 0;
	      tmp.cidx(1) = tmp.nnz ();

	      int i = 0;
	      int ii = 0;
	      if (i < nz)
		ii = c_lhs.ridx(i);

	      int j = 0;
	      int jj = lhs_idx.elem(j);

	      int kk = 0;

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
	      Sparse<LT> tmp (1, max_idx, new_nnz);

	      int i = 0;
	      int ii = 0;
	      while (ii < nc && c_lhs.cidx(ii+1) <= i)
		ii++;

	      int j = 0;
	      int jj = lhs_idx.elem(j);

	      int kk = 0;
	      int ic = 0;

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
		    }
		  else
		    {
		      while (ic <= jj)
			tmp.xcidx (ic++) = kk;
		      if (scalar_non_zero)
			tmp.xdata (kk) = scalar;
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
		  tmp.xridx (kk++) = 0;
		}

	      for (int iidx = ic; iidx < max_idx+1; iidx++)
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

	  int new_nnz = rhs.nnz ();
	  Sparse<LT> tmp (1, rhs_len, new_nnz);

	  int ii = 0;
	  int jj = 0;
	  for (int i = 0; i < rhs.cols(); i++)
	    for (int j = rhs.cidx(i); j < rhs.cidx(i+1); j++)
	      {
		OCTAVE_QUIT;
		for (int k = jj; k <= i * rhs.rows() + rhs.ridx(j); k++)
		  tmp.cidx(jj++) = ii;

		tmp.data(ii) = rhs.data(j);
		tmp.ridx(ii++) = 0;
	      }

	  for (int i = jj; i < rhs_len + 1; i++)
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

  int lhs_nr = lhs.rows ();
  int lhs_nc = lhs.cols ();
  int lhs_nz = lhs.nnz ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.cols ();

  idx_vector *tmp = lhs.get_idx ();

  idx_vector idx_i;
  idx_vector idx_j;

  if (n_idx > 2)
    {
      (*current_liboctave_error_handler)
        ("A(I, J) = X: can only have 1 or 2 indexes for sparse matrices");
      return 0;
    }

  if (n_idx > 1)
    idx_j = tmp[1];

  if (n_idx > 0)
    idx_i = tmp[0];

  if (n_idx == 2)
    {
      int n = idx_i.freeze (lhs_nr, "row", true, liboctave_wrore_flag);
      idx_i.sort (true);

      int m = idx_j.freeze (lhs_nc, "column", true, liboctave_wrore_flag);
      idx_j.sort (true);


      int idx_i_is_colon = idx_i.is_colon ();
      int idx_j_is_colon = idx_j.is_colon ();

      if (idx_i_is_colon)
	n = lhs_nr > 0 ? lhs_nr : rhs_nr;

      if (idx_j_is_colon)
	m = lhs_nc > 0 ? lhs_nc : rhs_nc;

      if (idx_i && idx_j)
	{
	  if (rhs_nr == 0 && rhs_nc == 0)
	    {
	      lhs.maybe_delete_elements (idx_i, idx_j);
	    }
	  else
	    {
	      if (rhs_nr == 1 && rhs_nc == 1 && n >= 0 && m >= 0)
		{
		  // No need to do anything if either of the indices
		  // are empty.

		  if (n > 0 && m > 0)
		    {
		      int max_row_idx = idx_i_is_colon ? rhs_nr : 
			idx_i.max () + 1;
		      int max_col_idx = idx_j_is_colon ? rhs_nc : 
			idx_j.max () + 1;
		      int new_nr = max_row_idx > lhs_nr ? max_row_idx : 
			lhs_nr;
		      int new_nc = max_col_idx > lhs_nc ? max_col_idx : 
			lhs_nc;
		      RT scalar = rhs.elem (0, 0);

		      // Count the number of non-zero terms
		      int new_nnz = lhs.nnz ();
		      for (int j = 0; j < m; j++)
			{
			  int jj = idx_j.elem (j);
			  if (jj < lhs_nc)
			    {
			      for (int i = 0; i < n; i++)
				{
				  OCTAVE_QUIT;

				  int ii = idx_i.elem (i);
			      
				  if (ii < lhs_nr)
				    {
				      for (int k = lhs.cidx(jj); 
					   k < lhs.cidx(jj+1); k++)
					{
					  if (lhs.ridx(k) == ii)
					    new_nnz--;
					  if (lhs.ridx(k) >= ii)
					    break;
					}
				    }
				}
			    }
			}

		      if (scalar != RT())
			new_nnz += m * n;

		      Sparse<LT> stmp (new_nr, new_nc, new_nnz);

		      int jji = 0;
		      int jj = idx_j.elem (jji);
		      int kk = 0;
		      stmp.cidx(0) = 0;
		      for (int j = 0; j < new_nc; j++)
			{
			  if (jji < m && jj == j)
			    {
			      int iii = 0;
			      int ii = idx_i.elem (iii);
			      for (int i = 0; i < new_nr; i++)
				{
				  OCTAVE_QUIT;

				  if (iii < n && ii == i)
				    {
				      if (scalar != RT ())
					{
					  stmp.data(kk) = scalar;
					  stmp.ridx(kk++) = i;
					}
				      if (++iii < n)
					ii = idx_i.elem(iii);
				    }
				  else if (j < lhs.cols()) 
				    {
				      for (int k = lhs.cidx(j); 
					   k < lhs.cidx(j+1); k++)
					{
					  if (lhs.ridx(k) == i)
					    {
					      stmp.data(kk) = lhs.data(k);
					      stmp.ridx(kk++) = i;
					    }
					  if (lhs.ridx(k) >= i)
					    break;
					}
				    }
				}
			      if (++jji < m)
				jj = idx_j.elem(jji);
			    }
			  else if (j < lhs.cols()) 
			    {
			      for (int i = lhs.cidx(j); 
				   i < lhs.cidx(j+1); i++)
				{
				  stmp.data(kk) = lhs.data(i);
				  stmp.ridx(kk++) = lhs.ridx(i);
				}
			    }
			  stmp.cidx(j+1) = kk;
			}
		      
		      lhs = stmp;
		    }
		}
	      else if (n == rhs_nr && m == rhs_nc)
		{
		  if (n > 0 && m > 0)
		    {
		      int max_row_idx = idx_i_is_colon ? rhs_nr : 
			idx_i.max () + 1;
		      int max_col_idx = idx_j_is_colon ? rhs_nc : 
			idx_j.max () + 1;
		      int new_nr = max_row_idx > lhs_nr ? max_row_idx : 
			lhs_nr;
		      int new_nc = max_col_idx > lhs_nc ? max_col_idx : 
			lhs_nc;

		      // Count the number of non-zero terms
		      int new_nnz = lhs.nnz ();
		      for (int j = 0; j < m; j++)
			{
			  int jj = idx_j.elem (j);
			  for (int i = 0; i < n; i++)
			    {
			      OCTAVE_QUIT;

			      if (jj < lhs_nc)
				{
				  int ii = idx_i.elem (i);
			      
				  if (ii < lhs_nr)
				    {
				      for (int k = lhs.cidx(jj); 
					   k < lhs.cidx(jj+1); k++)
					{
					  if (lhs.ridx(k) == ii)
					    new_nnz--;
					  if (lhs.ridx(k) >= ii)
					    break;
					}
				    }
				}
			      
			      if (rhs.elem(i,j) != RT ())
				new_nnz++;
			    }
			}

		      Sparse<LT> stmp (new_nr, new_nc, new_nnz);

		      int jji = 0;
		      int jj = idx_j.elem (jji);
		      int kk = 0;
		      stmp.cidx(0) = 0;
		      for (int j = 0; j < new_nc; j++)
			{
			  if (jji < m && jj == j)
			    {
			      int iii = 0;
			      int ii = idx_i.elem (iii);
			      for (int i = 0; i < new_nr; i++)
				{
				  OCTAVE_QUIT;

				  if (iii < n && ii == i)
				    {
				      RT rtmp = rhs.elem (iii, jji);
				      if (rtmp != RT ())
					{
					  stmp.data(kk) = rtmp;
					  stmp.ridx(kk++) = i;
					}
				      if (++iii < n)
					ii = idx_i.elem(iii);
				    }
				  else if (j < lhs.cols()) 
				    {
				      for (int k = lhs.cidx(j); 
					   k < lhs.cidx(j+1); k++)
					{
					  if (lhs.ridx(k) == i)
					    {
					      stmp.data(kk) = lhs.data(k);
					      stmp.ridx(kk++) = i;
					    }
					  if (lhs.ridx(k) >= i)
					    break;
					}
				    }
				}
			      if (++jji < m)
				jj = idx_j.elem(jji);
			    }
			  else if (j < lhs.cols()) 
			    {
			      for (int i = lhs.cidx(j); 
				   i < lhs.cidx(j+1); i++)
				{
				  stmp.data(kk) = lhs.data(i);
				  stmp.ridx(kk++) = lhs.ridx(i);
				}
			    }
			  stmp.cidx(j+1) = kk;
			}

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
	}
      // idx_vector::freeze() printed an error message for us.
    }
  else if (n_idx == 1)
    {
      int lhs_is_empty = lhs_nr == 0 || lhs_nc == 0;

      if (lhs_is_empty || (lhs_nr == 1 && lhs_nc == 1))
	{
	  int lhs_len = lhs.length ();

	  int n = idx_i.freeze (lhs_len, 0, true, liboctave_wrore_flag);
	  idx_i.sort (true);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		{
		  if (n != 0 && (lhs_nr != 0 || lhs_nc != 0))
		    lhs.maybe_delete_elements (idx_i);
		}
	      else
		{
		  if (liboctave_wfi_flag)
		    {
		      if (lhs_is_empty
			  && idx_i.is_colon ()
			  && ! (rhs_nr == 1 || rhs_nc == 1))
			{
			  (*current_liboctave_warning_handler)
			    ("A(:) = X: X is not a vector or scalar");
			}
		      else
			{
			  int idx_nr = idx_i.orig_rows ();
			  int idx_nc = idx_i.orig_columns ();

			  if (! (rhs_nr == idx_nr && rhs_nc == idx_nc))
			    (*current_liboctave_warning_handler)
			      ("A(I) = X: X does not have same shape as I");
			}
		    }

		  if (! assign1 ((Sparse<LT>&) lhs, (Sparse<RT>&) rhs))
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nr == 1)
	{
	  idx_i.freeze (lhs_nc, "vector", true, liboctave_wrore_flag);
	  idx_i.sort (true);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else if (! assign1 ((Sparse<LT>&) lhs, (Sparse<RT>&) rhs))
		retval = 0;
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nc == 1)
	{
	  idx_i.freeze (lhs_nr, "vector", true, liboctave_wrore_flag);
	  idx_i.sort (true);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else if (! assign1 ((Sparse<LT>&) lhs, (Sparse<RT>&) rhs))
		retval = 0;
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else
	{
	  if (liboctave_wfi_flag
	      && ! (idx_i.is_colon ()
		    || (idx_i.one_zero_only ()
			&& idx_i.orig_rows () == lhs_nr
			&& idx_i.orig_columns () == lhs_nc)))
	    (*current_liboctave_warning_handler)
	      ("single index used for matrix");

	  int lhs_len = lhs.length ();

	  int len = idx_i.freeze (lhs_nr * lhs_nc, "matrix");
	  idx_i.sort (true);

	  if (idx_i)
	    {
	      // Take a constant copy of lhs. This means that elem won't 
	      // create missing elements.
	      const Sparse<LT> c_lhs (lhs);

	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else if (len == 0)
		{
		  if (! ((rhs_nr == 1 && rhs_nc == 1)
			 || (rhs_nr == 0 || rhs_nc == 0)))
		    (*current_liboctave_error_handler)
		      ("A([]) = X: X must be an empty matrix or scalar");
		}
	      else if (len == rhs_nr * rhs_nc)
		{
		  int new_nnz = lhs_nz;

		  // First count the number of non-zero elements
		  for (int i = 0; i < len; i++)
		    {
		      OCTAVE_QUIT;
		      
		      int ii = idx_i.elem (i);
		      if (ii < lhs_len && c_lhs.elem(ii) != LT ())
			new_nnz--;
		      if (rhs.elem(i) != RT ())
			new_nnz++;
		    }

		  Sparse<LT> stmp (lhs_nr, lhs_nc, new_nnz);

		  int i = 0;
		  int ii = 0;
		  int ic = 0;
		  if (i < lhs_nz)
		    {
		      while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
			ic++;
		      ii = ic * lhs_nr + c_lhs.ridx(i);
		    }

		  int j = 0;
		  int jj = idx_i.elem (j);
		  int jr = jj % lhs_nr;
		  int jc = (jj - jr) / lhs_nr;

		  int kk = 0;
		  int kc = 0;

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
			  RT rtmp = rhs.elem (j);
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

		  for (int iidx = kc; iidx < lhs_nc+1; iidx++)
		    stmp.xcidx(iidx) = kk;
		  

		  lhs = stmp;
		}
	      else if (rhs_nr == 1 && rhs_nc == 1)
		{
		  RT scalar = rhs.elem (0, 0);
		  int new_nnz = lhs_nz;

		  // First count the number of non-zero elements
		  if (scalar != RT ())
		    new_nnz += len;
		  for (int i = 0; i < len; i++)
		    {
		      OCTAVE_QUIT;
		      int ii = idx_i.elem (i);
		      if (ii < lhs_len && c_lhs.elem(ii) != LT ())
			new_nnz--;
		    }

		  Sparse<LT> stmp (lhs_nr, lhs_nc, new_nnz);

		  int i = 0;
		  int ii = 0;
		  int ic = 0;
		  if (i < lhs_nz)
		    {
		      while (ic < lhs_nc && i >= c_lhs.cidx(ic+1))
			ic++;
		      ii = ic * lhs_nr + c_lhs.ridx(i);
		    }

		  int j = 0;
		  int jj = idx_i.elem (j);
		  int jr = jj % lhs_nr;
		  int jc = (jj - jr) / lhs_nr;

		  int kk = 0;
		  int kc = 0;

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

		  for (int iidx = kc; iidx < lhs_nc+1; iidx++)
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

template <class T>
void
Sparse<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << "\n"
     << prefix << "rep->nnz:    " << rep->nnz << "\n"
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
