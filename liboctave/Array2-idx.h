// Template array classes
/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include "Array-flags.h"
#include "idx-vector.h"
#include "lo-error.h"

template <class T>
Array2<T>
Array2<T>::value (void)
{
  Array2<T> retval;

  int n_idx = index_count ();

  if (n_idx == 2)
    {
      idx_vector *tmp = get_idx ();
      idx_vector idx_i = tmp[0];
      idx_vector idx_j = tmp[1];

      return index (idx_i, idx_j);
    }
  else if (n_idx == 1)
    {
      return index (idx[0]);
    }
  else
    (*current_liboctave_error_handler)
      ("invalid number of indices for matrix expression");

  clear_index ();

  return retval;
}

template <class T>
Array2<T>
Array2<T>::index (idx_vector& idx_arg, int resize_ok,
		  const T& resize_fill_value) const
{
  Array2<T> retval;

  int nr = d1;
  int nc = d2;

  int orig_len = nr * nc;

  int idx_orig_rows = idx_arg.orig_rows ();
  int idx_orig_columns = idx_arg.orig_columns ();

  if (idx_arg.is_colon ())
    {
      // Fast magic colon processing.

      int result_nr = nr * nc;
      int result_nc = result_nr ? 1 : 0;

      retval = Array2<T> (*this, result_nr, result_nc);
    }
  else if (nr == 1 && nc == 1)
    {
      Array<T> tmp = Array<T>::index (idx_arg, resize_ok);

      if (tmp.length () != 0)
	retval = Array2<T> (tmp, idx_orig_rows, idx_orig_columns);
      else
	retval = Array2<T> (tmp, 0, 0);
    }
  else if (nr == 1 || nc == 1)
    {
      int result_is_column_vector = (nc == 1);

      Array<T> tmp = Array<T>::index (idx_arg, resize_ok);

      int len = tmp.length ();

      if (len == 0)
	retval = Array2<T> (0, 0);
      else
	{
	  if (result_is_column_vector)
	    retval = Array2<T> (tmp, len, 1);
	  else
	    retval = Array2<T> (tmp, 1, len);
	}
    }
  else if (liboctave_dfi_flag
	   || (idx_arg.one_zero_only ()
	       && idx_orig_rows == nr
	       && idx_orig_columns == nc))
    {
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

	  retval.resize (result_nr, result_nc);



	  int k = 0;
	  for (int j = 0; j < result_nc; j++)
	    {
	      for (int i = 0; i < result_nr; i++)
		{
		  int ii = idx_arg.elem (k++);
		  if (ii > orig_len)
		    retval.elem (i, j) = resize_fill_value;
		  else
		    {
		      int fr = ii % nr;
		      int fc = (ii - fr) / nr;
		      retval.elem (i, j) = elem (fr, fc);
		    }
		}
	    }
	}
      // idx_vector::freeze() printed an error message for us.
    }
  else
    (*current_liboctave_error_handler)
      ("single index only valid for row or column vector");

  return retval;
}

template <class T>
Array2<T>
Array2<T>::index (idx_vector& idx_i, idx_vector& idx_j, int resize_ok,
		  const T& resize_fill_value) const
{
  Array2<T> retval;

  int nr = d1;
  int nc = d2;

  int n = idx_i.freeze (nr, "row", resize_ok);
  int m = idx_j.freeze (nc, "column", resize_ok);

  if (idx_i && idx_j)
    {
      if (idx_i.orig_empty () || idx_j.orig_empty () || n == 0 || m == 0)
	{
	  retval.resize (n, m);
	}
      else if (idx_i.is_colon_equiv (nr) && idx_j.is_colon_equiv (nc))
	{
	  retval = *this;
	}
      else
	{
	  retval.resize (n, m);

	  for (int j = 0; j < m; j++)
	    {
	      int jj = idx_j.elem (j);
	      for (int i = 0; i < n; i++)
		{
		  int ii = idx_i.elem (i);
		  if (ii > nr || jj > nc)
		    retval.elem (i, j) = resize_fill_value;
		  else
		    retval.elem (i, j) = elem (ii, jj);
		}
	    }
	}
    }

  // idx_vector::freeze() printed an error message for us.

  return retval;
}

template <class T>
void
Array2<T>::maybe_delete_elements (idx_vector& idx_arg)
{
  int nr = d1;
  int nc = d2;

  if (nr == 0 && nc == 0)
    return;

  int n;
  if (nr == 1)
    n = nc;
  else if (nc == 1)
    n = nr;
  else
    {
      (*current_liboctave_error_handler)
	("A(idx) = []: expecting A to be row or column vector or scalar");

      return;
    }

  if (idx_arg.is_colon_equiv (n, 1))
    {
      // Either A(:) = [] or A(idx) = [] with idx enumerating all
      // elements, so we delete all elements and return [](0x0).  To
      // preserve the orientation of the vector, you have to use
      // A(idx,:) = [] (delete rows) or A(:,idx) (delete columns).

      resize (0, 0);
      return;
    }

  idx_arg.sort (true);

  int num_to_delete = idx_arg.length (n);

  if (num_to_delete != 0)
    {
      int new_n = n;

      int iidx = 0;

      for (int i = 0; i < n; i++)
	if (i == idx_arg.elem (iidx))
	  {
	    iidx++;
	    new_n--;

	    if (iidx == num_to_delete)
	      break;
	  }

      if (new_n > 0)
	{
	  T *new_data = new T [new_n];

	  int ii = 0;
	  iidx = 0;
	  for (int i = 0; i < n; i++)
	    {
	      if (iidx < num_to_delete && i == idx_arg.elem (iidx))
		iidx++;
	      else
		{
		  if (nr == 1)
		    new_data[ii] = elem (0, i);
		  else
		    new_data[ii] = elem (i, 0);

		  ii++;
		}
	    }

	  if (--rep->count <= 0)
	    delete rep;

	  rep = new typename Array<T>::ArrayRep (new_data, new_n);

	  if (nr == 1)
	    {
	      d1 = 1;
	      d2 = new_n;
	    }
	  else
	    {
	      d1 = new_n;
	      d2 = 1;
	    }

	  set_max_indices (2);
	}
      else
	(*current_liboctave_error_handler)
	  ("A(idx) = []: index out of range");
    }
}

template <class T>
void
Array2<T>::maybe_delete_elements (idx_vector& idx_i, idx_vector& idx_j)
{
  int nr = d1;
  int nc = d2;

  if (nr == 0 && nc == 0)
    return;

  if (idx_i.is_colon ())
    {
      if (idx_j.is_colon ())
	{
	  // A(:,:) -- We are deleting columns and rows, so the result
	  // is [](0x0).

	  resize (0, 0);
	  return;
	}

      if (idx_j.is_colon_equiv (nc, 1))
	{
	  // A(:,j) -- We are deleting columns by enumerating them,
	  // If we enumerate all of them, we should have zero columns
	  // with the same number of rows that we started with.

	  resize (nr, 0);
	  return;
	}
    }

  if (idx_j.is_colon () && idx_i.is_colon_equiv (nr, 1))
    {
      // A(i,:) -- We are deleting rows by enumerating them.  If we
      // enumerate all of them, we should have zero rows with the
      // same number of columns that we started with.

      resize (0, nc);
      return;
    }

  if (idx_i.is_colon_equiv (nr, 1))
    {
      if (idx_j.is_colon_equiv (nc, 1))
	resize (0, 0);
      else
	{
	  idx_j.sort (true);

	  int num_to_delete = idx_j.length (nc);

	  if (num_to_delete != 0)
	    {
	      if (nr == 1 && num_to_delete == nc)
		resize (0, 0);
	      else
		{
		  int new_nc = nc;

		  int iidx = 0;

		  for (int j = 0; j < nc; j++)
		    if (j == idx_j.elem (iidx))
		      {
			iidx++;
			new_nc--;

			if (iidx == num_to_delete)
			  break;
		      }

		  if (new_nc > 0)
		    {
		      T *new_data = new T [nr * new_nc];

		      int jj = 0;
		      iidx = 0;
		      for (int j = 0; j < nc; j++)
			{
			  if (iidx < num_to_delete && j == idx_j.elem (iidx))
			    iidx++;
			  else
			    {
			      for (int i = 0; i < nr; i++)
				new_data[nr*jj+i] = elem (i, j);
			      jj++;
			    }
			}

		      if (--rep->count <= 0)
			delete rep;

		      rep = new typename Array<T>::ArrayRep (new_data, nr * new_nc);

		      d2 = new_nc;

		      set_max_indices (2);
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
	resize (0, 0);
      else
	{
	  idx_i.sort (true);

	  int num_to_delete = idx_i.length (nr);

	  if (num_to_delete != 0)
	    {
	      if (nc == 1 && num_to_delete == nr)
		resize (0, 0);
	      else 
		{
		  int new_nr = nr;

		  int iidx = 0;

		  for (int i = 0; i < nr; i++)
		    if (i == idx_i.elem (iidx))
		      {
			iidx++;
			new_nr--;

			if (iidx == num_to_delete)
			  break;
		      }

		  if (new_nr > 0)
		    {
		      T *new_data = new T [new_nr * nc];

		      int ii = 0;
		      iidx = 0;
		      for (int i = 0; i < nr; i++)
			{
			  if (iidx < num_to_delete && i == idx_i.elem (iidx))
			    iidx++;
			  else
			    {
			      for (int j = 0; j < nc; j++)
				new_data[new_nr*j+ii] = elem (i, j);
			      ii++;
			    }
			}

		      if (--rep->count <= 0)
			delete rep;

		      rep = new typename Array<T>::ArrayRep (new_data, new_nr * nc);

		      d1 = new_nr;

		      set_max_indices (2);
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
}

#define MAYBE_RESIZE_LHS \
  do \
    { \
      if (liboctave_rre_flag) \
	{ \
	  int max_row_idx = idx_i_is_colon ? rhs_nr : idx_i.max () + 1; \
	  int max_col_idx = idx_j_is_colon ? rhs_nc : idx_j.max () + 1; \
 \
	  int new_nr = max_row_idx > lhs_nr ? max_row_idx : lhs_nr; \
	  int new_nc = max_col_idx > lhs_nc ? max_col_idx : lhs_nc; \
 \
	  lhs.resize (new_nr, new_nc, resize_fill_value); \
	} \
    } \
  while (0)

template <class LT, class RT>
int
assign (Array2<LT>& lhs, const Array2<RT>& rhs, const LT& resize_fill_value)
{
  int retval = 1;

  int n_idx = lhs.index_count ();

  int lhs_nr = lhs.rows ();
  int lhs_nc = lhs.cols ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.cols ();

  idx_vector *tmp = lhs.get_idx ();

  idx_vector idx_i;
  idx_vector idx_j;

  if (n_idx > 1)
    idx_j = tmp[1];

  if (n_idx > 0)
    idx_i = tmp[0];

  if (n_idx == 2)
    {
      int n = idx_i.freeze (lhs_nr, "row", liboctave_rre_flag);

      int m = idx_j.freeze (lhs_nc, "column", liboctave_rre_flag);

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
	      if (rhs_nr == 1 && rhs_nc == 1 && n > 0 && m > 0)
		{
		  MAYBE_RESIZE_LHS;

		  RT scalar = rhs.elem (0, 0);

		  for (int j = 0; j < m; j++)
		    {
		      int jj = idx_j.elem (j);
		      for (int i = 0; i < n; i++)
			{
			  int ii = idx_i.elem (i);
			  lhs.elem (ii, jj) = scalar;
			}
		    }
		}
	      else if (n == rhs_nr && m == rhs_nc)
		{
		  MAYBE_RESIZE_LHS;

		  for (int j = 0; j < m; j++)
		    {
		      int jj = idx_j.elem (j);
		      for (int i = 0; i < n; i++)
			{
			  int ii = idx_i.elem (i);
			  lhs.elem (ii, jj) = rhs.elem (i, j);
			}
		    }
		}
	      else if (n == 0 && m == 0)
		{
		  if (! ((rhs_nr == 1 && rhs_nc == 1)
			 || (rhs_nr == 0 && rhs_nc == 0)))
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

	  int n = idx_i.freeze (lhs_len, 0, liboctave_rre_flag);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		{
		  if (n != 0 && (lhs_nr != 0 || lhs_nc != 0))
		    lhs.maybe_delete_elements (idx_i);
		}
	      else if (! liboctave_dfi_flag && lhs_is_empty
		       && idx_i.is_colon ()
		       && ! (rhs_nr == 1 || rhs_nc == 1))
		{
		  (*current_liboctave_error_handler)
		    ("A(:) = X: X must be a vector");
		}
	      else
		{
		  if (assign ((Array<LT>&) lhs, (Array<RT>&) rhs))
		    {
		      int len = lhs.length ();

		      if (len > 0)
			{
			  int idx_nr = idx_i.orig_rows ();
			  int idx_nc = idx_i.orig_columns ();

			  // lhs_is_empty now means that lhs was
			  // *originally* empty, and lhs_len is the
			  // *original* length of the lhs.

			  if (liboctave_dfi_flag
			      || (idx_nr == 1 && idx_nc == 1)
			      || (rhs_nr == 1 && rhs_nc == 1 && lhs_len == 1))
			    {
			      if (liboctave_pcv_flag)
				{
				  lhs.d1 = lhs.length ();
				  lhs.d2 = 1;
				}
			      else
				{
				  lhs.d1 = 1;
				  lhs.d2 = lhs.length ();
				}
			    }
			  else if (lhs_is_empty && idx_i.is_colon ())
			    {
			      lhs.d1 = rhs.d1;
			      lhs.d2 = rhs.d2;
			    }
			  else if (lhs_is_empty && idx_i.one_zero_only ())
			    {
			      lhs.d1 = idx_nr;
			      lhs.d2 = idx_nc;
			    }
			  else if (rhs_nr == 1
				   && (idx_nr == 1 || lhs_len == 1))
			    {
			      lhs.d1 = 1;
			      lhs.d2 = lhs.length ();
			    }
			  else if (rhs_nc == 1
				   && (idx_nc == 1 || lhs_len == 1))
			    {
			      lhs.d1 = lhs.length ();
			      lhs.d2 = 1;
			    }
			  else if (idx_nr == 0 && idx_nc == 0)
			    {
			      if (! ((rhs.d1 == 1 && rhs.d2 == 1)
				     || (rhs.d1 == 0 && rhs.d2 == 0)))
				(*current_liboctave_error_handler)
			  ("A([]) = X: X must be an empty matrix or scalar");
			    }
			  else
			    (*current_liboctave_error_handler)
      ("A(I) = X: X must be a scalar or a matrix with the same size as I");
			}
		      else
			{
			  lhs.d1 = 0;
			  lhs.d2 = 0;
			}
		    }
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nr == 1)
	{
	  idx_i.freeze (lhs_nc, "vector", liboctave_rre_flag);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else
		{
		  if (assign ((Array<LT>&) lhs, (Array<RT>&) rhs))
		    lhs.d2 = lhs.length ();
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nc == 1)
	{
	  idx_i.freeze (lhs_nr, "vector", liboctave_rre_flag);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else
		{
		  if (assign ((Array<LT>&) lhs, (Array<RT>&) rhs))
		    lhs.d1 = lhs.length ();
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (liboctave_dfi_flag
	       || idx_i.is_colon ()
	       || (idx_i.one_zero_only ()
		   && idx_i.orig_rows () == lhs_nr
		   && idx_i.orig_columns () == lhs_nc))
	{
	  int len = idx_i.freeze (lhs_nr * lhs_nc, "matrix");

	  if (idx_i)
	    {
	      if (len == 0)
		{
		  if (! ((rhs_nr == 1 && rhs_nc == 1)
			 || (rhs_nr == 0 && rhs_nc == 0)))
		    (*current_liboctave_error_handler)
		      ("A([]) = X: X must be an empty matrix or scalar");
		}
	      else if (len == rhs_nr * rhs_nc)
		{
		  int k = 0;
		  for (int j = 0; j < rhs_nc; j++)
		    {
		      for (int i = 0; i < rhs_nr; i++)
			{
			  int ii = idx_i.elem (k++);
			  int fr = ii % lhs_nr;
			  int fc = (ii - fr) / lhs_nr;
			  lhs.elem (fr, fc) = rhs.elem (i, j);
			}
		    }
		}
	      else if (rhs_nr == 1 && rhs_nc == 1 && len <= lhs_nr * lhs_nc)
		{
		  RT scalar = rhs.elem (0, 0);

		  for (int i = 0; i < len; i++)
		    {
		      int ii = idx_i.elem (i);
		      int fr = ii % lhs_nr;
		      int fc = (ii - fr) / lhs_nr;
		      lhs.elem (fr, fc) = scalar;
		    }
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
      else
	{
	  (*current_liboctave_error_handler)
	    ("single index only valid for row or column vector");

	  retval = 0;
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
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
