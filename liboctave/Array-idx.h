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
void
Array<T>::clear_index (void)
{
  delete [] idx;
  idx = 0;
  idx_count = 0;
}

template <class T>
void
Array<T>::set_index (const idx_vector& i)
{
  if (! idx)
    idx = new idx_vector [max_indices];

  if (idx_count < max_indices)
    {
      idx[idx_count++] = i;
    }
  else
    {
      (*current_liboctave_error_handler)
	("invalid number of indices specified");

      clear_index ();
    }
}

template <class T>
Array<T>
Array<T>::value (void)
{
  Array<T> retval;

  idx_vector *tmp = get_idx ();
  idx_vector idx = tmp[0];

  retval = index (idx);

  clear_index ();

  return retval;
}

template <class T>
Array<T>
Array<T>::index (idx_vector& idx) const
{
  Array<T> retval;

  int len = length ();

  int n = idx.freeze (len, "vector");

  if (idx)
    {
      if (idx.is_colon_equiv (len))
	{
	  retval = *this;
	}
      else if (n == 0)
	{
	  retval.resize (0);
	}
      else if (len == 1 && n > 1
	       && idx.one_zero_only ()
	       && idx.ones_count () == n)
	{
	  retval.resize (n, elem (0));
	}
      else
	{
	  retval.resize (n);

	  for (int i = 0; i < n; i++)
	    {
	      int ii = idx.elem (i);
	      retval.elem (i) = elem (ii);
	    }
	}
    }

  // idx_vector::freeze() printed an error message for us.

  return retval;
}

template <class T>
void
Array<T>::maybe_delete_elements (idx_vector& idx)
{
  int len = length ();

  if (len == 0)
    return;

  if (idx.is_colon_equiv (len, 1))
    resize (0);
  else
    {
      int num_to_delete = idx.length (len);

      if (num_to_delete != 0)
	{
	  int new_len = len;

	  int iidx = 0;

	  for (int i = 0; i < len; i++)
	    if (i == idx.elem (iidx))
	      {
		iidx++;
		new_len--;
	      }

	  if (new_len > 0)
	    {
	      T *new_data = new T [new_len];

	      int ii = 0;
	      iidx = 0;
	      for (int i = 0; i < len; i++)
		{
		  if (i == idx.elem (iidx))
		    iidx++;
		  else
		    {
		      new_data[ii] = elem (i);
		      ii++;
		    }
		}

	      if (--rep->count <= 0)
		delete rep;

	      rep = new ArrayRep (new_data, new_len);

	      set_max_indices (1);
	    }
	  else
	    (*current_liboctave_error_handler)
	      ("A(idx) = []: index out of range");
	}
    }
}

// ??? FIXME ??? -- this does not handle assignment of empty vectors
// to delete elements.  Should it?

template <class LT, class RT>
int
assign (Array<LT>& lhs, const Array<RT>& rhs)
{
  int retval = 1;

  idx_vector *tmp = lhs.get_idx ();

  idx_vector idx = tmp[0];

  int lhs_len = lhs.length ();
  int rhs_len = rhs.length ();

  int n = idx.freeze (lhs_len, "vector", liboctave_rre_flag);

  if (n != 0)
    {
      if (liboctave_rre_flag && (rhs_len == n || rhs_len == 1))
	{
	  int max_idx = idx.max () + 1;
	  if (max_idx > lhs_len)
	    lhs.resize (max_idx, 0.0);
	}

      if (rhs_len == n)
	{
	  for (int i = 0; i < n; i++)
	    {
	      int ii = idx.elem (i);
	      lhs.elem (ii) = rhs.elem (i);
	    }
	}
      else if (rhs_len == 1)
	{
	  RT scalar = rhs.elem (0);

	  for (int i = 0; i < n; i++)
	    {
	      int ii = idx.elem (i);
	      lhs.elem (ii) = scalar;
	    }
	}
      else
	{
	  (*current_liboctave_error_handler)
	    ("A(I) = X: X must be a scalar or a vector with same length as I");

	  retval = 0;
	}
    }
  else if (idx.is_colon ())
    {
      if (lhs_len == 0)
	{
	  lhs.resize (rhs_len);

	  for (int i = 0; i < rhs_len; i++)
	    lhs.elem (i) = rhs.elem (i);
	}
      else
	(*current_liboctave_error_handler)
	  ("A(:) = X: A must be the same size as X");
    }
  else
    {
      (*current_liboctave_error_handler)
	("A([]) = X: X must also be an empty matrix");

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
