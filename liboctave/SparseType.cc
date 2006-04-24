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
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>

#include "SparseType.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-spparms.h"

// FIXME There is a large code duplication here

SparseType::SparseType (void) : typ (SparseType::Unknown), nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
} 

SparseType::SparseType (const SparseType &a) : typ (a.typ), 
    sp_bandden (a.sp_bandden), bandden (a.bandden), 
    upper_band (a.upper_band), lower_band (a.lower_band), 
    dense (a.dense), nperm (a.nperm)
{ 
  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
	perm[i] = a.perm[i];
    }
}

SparseType::SparseType (const SparseMatrix &a)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nzmax ();

  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Calculating Sparse Matrix Type");

  nperm = 0;

  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  bool maybe_hermitian = false;
  typ = SparseType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = SparseType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
	{
	  if (a.cidx(i+1) != a.cidx(i) + 1)
	    {
	      tmp_typ = SparseType::Full;
	      break;
	    }
	  if (a.ridx(i) != i)
	    {
	      tmp_typ = SparseType::Permuted_Diagonal;
	      break;
	    }
	}
	  
      if (tmp_typ == SparseType::Permuted_Diagonal)
	{
	  std::vector<bool> found (nrows);

	  for (octave_idx_type j = 0; j < i; j++)
	    found [j] = true;
	  for (octave_idx_type j = i; j < nrows; j++)
	    found [j] = false;
	      
	  for (octave_idx_type j = i; j < nm; j++)
	    {
	      if ((a.cidx(j+1) > a.cidx(j) + 1)  || 
		  ((a.cidx(j+1) == a.cidx(j) + 1) && found [a.ridx(j)]))
		{
		  tmp_typ = SparseType::Full;
		  break;
		}
	      found [a.ridx(j)] = true;
	    }
	}
      typ = tmp_typ;
    }

  if (typ == SparseType::Full)
    {
      // Search for banded, upper and lower triangular matrices
      bool singular = false;
      upper_band = 0;
      lower_band = 0;
      for (octave_idx_type j = 0; j < ncols; j++)
	{
	  bool zero_on_diagonal = false;
	  if (j < nrows)
	    {
	      zero_on_diagonal = true;
	      for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
		if (a.ridx(i) == j)
		  {
		    zero_on_diagonal = false;
		    break;
		  }
	    }

	  if (zero_on_diagonal)
	    {
	      singular = true;
	      break;
	    }

	  if (a.cidx(j+1) != a.cidx(j))
	    {
	      octave_idx_type ru = a.ridx(a.cidx(j));
	      octave_idx_type rl = a.ridx(a.cidx(j+1)-1);

	      if (j - ru > upper_band)
		upper_band = j - ru;
		  
	      if (rl - j > lower_band)
		lower_band = rl - j;
	    }
	}

      if (!singular)
	{
	  bandden = double (nnz) /
	    (double (ncols) * (double (lower_band) +
			       double (upper_band)) -
	     0.5 * double (upper_band + 1) * double (upper_band) -
	     0.5 * double (lower_band + 1) * double (lower_band));

	  if (nrows == ncols && sp_bandden != 1. && bandden > sp_bandden)
	    {
	      if (upper_band == 1 && lower_band == 1)
		typ = SparseType::Tridiagonal;
	      else
		typ = SparseType::Banded;

	      octave_idx_type nnz_in_band = 
		(upper_band + lower_band + 1) * nrows -
		(1 + upper_band) * upper_band / 2 -
		(1 + lower_band) * lower_band / 2;
	      if (nnz_in_band == nnz)
		dense = true;
	      else 
		dense = false;
	    }
	  else if (upper_band == 0)
	    typ = SparseType::Lower;
	  else if (lower_band == 0)
	    typ = SparseType::Upper;

	  if (upper_band == lower_band && nrows == ncols)
	    maybe_hermitian = true;
	}

      if (typ == SparseType::Full)
	{
	  // Search for a permuted triangular matrix, and test if
	  // permutation is singular

	  // FIXME
	  // Perhaps this should be based on a dmperm algorithm
	  bool found = false;

	  nperm = ncols;
	  perm = new octave_idx_type [ncols];

	  for (octave_idx_type i = 0; i < ncols; i++)
	    perm [i] = -1;

	  for (octave_idx_type i = 0; i < nm; i++)
	    {
	      found = false;

	      for (octave_idx_type j = 0; j < ncols; j++)
		{
		  if ((a.cidx(j+1) - a.cidx(j)) > 0 && 
		      (a.ridx(a.cidx(j+1)-1) == i))
		    {
		      perm [i] = j;
		      found = true;
		      break;
		    }
		}

	      if (!found)
		break;
	    }

	  if (found)
	    {
	      typ = SparseType::Permuted_Upper;
	      if (ncols > nrows)
		{
		  octave_idx_type k = nrows;
		  for (octave_idx_type i = 0; i < ncols; i++)
		    if (perm [i] == -1)
		      perm[i] = k++;
		}
	    }
	  else if (a.cidx(nm) == a.cidx(ncols))
	    {
	      nperm = nrows;
	      delete [] perm;
	      perm = new octave_idx_type [nrows];
	      OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, nrows);

	      for (octave_idx_type i = 0; i < nrows; i++)
		{
		  perm [i] = -1;
		  tmp [i] = -1;
		}

	      for (octave_idx_type j = 0; j < ncols; j++)
		for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
		    perm [a.ridx(i)] = j;

	      found = true;
	      for (octave_idx_type i = 0; i < nm; i++)
		if (perm[i] == -1)
		  {
		    found = false;
		    break;
		  }
		else
		  {
		    tmp[perm[i]] = 1;
		  }

	      if (found)
		{
		  octave_idx_type k = ncols;
		  for (octave_idx_type i = 0; i < nrows; i++)
		    {
		      if (tmp[i] == -1)
			{
			  if (k < nrows)
			    {
			      perm[k++] = i;
			    }
			  else
			    {
			      found = false;
			      break;
			    }
			}
		    }
		}

	      if (found)
		typ = SparseType::Permuted_Lower;
	      else
		{
		  delete [] perm;
		  nperm = 0;
		}
	    }
	  else
	    {
	      delete [] perm;
	      nperm = 0;
	    }
	}

      // FIXME
      // Disable lower under-determined and upper over-determined problems
      // as being detected, and force to treat as singular. As this seems
      // to cause issues
      if (((typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
	   && nrows > ncols) ||
	  ((typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
	   && nrows < ncols))
	{
	  typ = SparseType::Rectangular;
	  if (typ == SparseType::Permuted_Upper ||
	      typ == SparseType::Permuted_Lower)
	    delete [] perm;
	  nperm = 0;
	}

      if (typ == SparseType::Full && ncols != nrows)
	typ = SparseType::Rectangular;

      if (maybe_hermitian && (typ == SparseType::Full || 
			      typ == SparseType::Tridiagonal || 
			      typ == SparseType::Banded))
	{
	  // Check for symmetry, with positive real diagonal, which
	  // has a very good chance of being symmetric positive
	  // definite..
	  bool is_herm = true;

	  for (octave_idx_type j = 0; j < ncols; j++)
	    {
	      bool diag_positive = false;

	      for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
		{
		  octave_idx_type ri = a.ridx(i);

		  if (ri == j)
		    {
		      if (a.data(i) == std::abs(a.data(i)))
			diag_positive = true;
		      else
			break;
		    }
		  else
		    {
		      bool found = false;

		      for (octave_idx_type k = a.cidx(ri); k < a.cidx(ri+1); k++)
			{
			  if (a.ridx(k) == j)
			    {
			      if (a.data(i) == a.data(k))
				found = true;
			      break;
			    }
			}

		      if (! found)
			{
			  is_herm = false;
			  break;
			}
		    }
		}

	      if (! diag_positive || ! is_herm)
		{
		  is_herm = false;
		  break;
		} 
	    }

	  if (is_herm)
	    {
	      if (typ == SparseType::Full)
		typ = SparseType::Hermitian;
	      else if (typ == SparseType::Banded)
		typ = SparseType::Banded_Hermitian;
	      else
		typ = SparseType::Tridiagonal_Hermitian;
	    }
	}
    }
}

SparseType::SparseType (const SparseComplexMatrix &a)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nzmax ();

  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Calculating Sparse Matrix Type");

  nperm = 0;

  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  bool maybe_hermitian = false;
  typ = SparseType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = SparseType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
	{
	  if (a.cidx(i+1) != a.cidx(i) + 1)
	    {
	      tmp_typ = SparseType::Full;
	      break;
	    }
	  if (a.ridx(i) != i)
	    {
	      tmp_typ = SparseType::Permuted_Diagonal;
	      break;
	    }
	}
	  
      if (tmp_typ == SparseType::Permuted_Diagonal)
	{
	  std::vector<bool> found (nrows);

	  for (octave_idx_type j = 0; j < i; j++)
	    found [j] = true;
	  for (octave_idx_type j = i; j < nrows; j++)
	    found [j] = false;
	      
	  for (octave_idx_type j = i; j < nm; j++)
	    {
	      if ((a.cidx(j+1) > a.cidx(j) + 1)  || 
		  ((a.cidx(j+1) == a.cidx(j) + 1) && found [a.ridx(j)]))
		{
		  tmp_typ = SparseType::Full;
		  break;
		}
	      found [a.ridx(j)] = true;
	    }
	}
      typ = tmp_typ;
    }

  if (typ == SparseType::Full)
    {
      // Search for banded, upper and lower triangular matrices
      bool singular = false;
      upper_band = 0;
      lower_band = 0;
      for (octave_idx_type j = 0; j < ncols; j++)
	{
	  bool zero_on_diagonal = false;
	  if (j < nrows)
	    {
	      zero_on_diagonal = true;
	      for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
		if (a.ridx(i) == j)
		  {
		    zero_on_diagonal = false;
		    break;
		  }
	    }

	  if (zero_on_diagonal)
	    {
	      singular = true;
	      break;
	    }

	  if (a.cidx(j+1) != a.cidx(j))
	    {
	      octave_idx_type ru = a.ridx(a.cidx(j));
	      octave_idx_type rl = a.ridx(a.cidx(j+1)-1);

	      if (j - ru > upper_band)
		upper_band = j - ru;
		  
	      if (rl - j > lower_band)
		lower_band = rl - j;
	    }
	}

      if (!singular)
	{
	  bandden = double (nnz) /
	    (double (ncols) * (double (lower_band) +
			       double (upper_band)) -
	     0.5 * double (upper_band + 1) * double (upper_band) -
	     0.5 * double (lower_band + 1) * double (lower_band));

	  if (nrows == ncols && sp_bandden != 1. && bandden > sp_bandden)
	    {
	      if (upper_band == 1 && lower_band == 1)
		typ = SparseType::Tridiagonal;
	      else
		typ = SparseType::Banded;

	      octave_idx_type nnz_in_band = 
		(upper_band + lower_band + 1) * nrows -
		(1 + upper_band) * upper_band / 2 -
		(1 + lower_band) * lower_band / 2;
	      if (nnz_in_band == nnz)
		dense = true;
	      else 
		dense = false;
	    }
	  else if (upper_band == 0)
	    typ = SparseType::Lower;
	  else if (lower_band == 0)
	    typ = SparseType::Upper;

	  if (upper_band == lower_band && nrows == ncols)
	    maybe_hermitian = true;
	}

      if (typ == SparseType::Full)
	{
	  // Search for a permuted triangular matrix, and test if
	  // permutation is singular

	  // FIXME
	  // Perhaps this should be based on a dmperm algorithm
	  bool found = false;

	  nperm = ncols;
	  perm = new octave_idx_type [ncols];

	  for (octave_idx_type i = 0; i < ncols; i++)
	    perm [i] = -1;

	  for (octave_idx_type i = 0; i < nm; i++)
	    {
	      found = false;

	      for (octave_idx_type j = 0; j < ncols; j++)
		{
		  if ((a.cidx(j+1) - a.cidx(j)) > 0 && 
		      (a.ridx(a.cidx(j+1)-1) == i))
		    {
		      perm [i] = j;
		      found = true;
		      break;
		    }
		}

	      if (!found)
		break;
	    }

	  if (found)
	    {
	      typ = SparseType::Permuted_Upper;
	      if (ncols > nrows)
		{
		  octave_idx_type k = nrows;
		  for (octave_idx_type i = 0; i < ncols; i++)
		    if (perm [i] == -1)
		      perm[i] = k++;
		}
	    }
	  else if (a.cidx(nm) == a.cidx(ncols))
	    {
	      nperm = nrows;
	      delete [] perm;
	      perm = new octave_idx_type [nrows];
	      OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, nrows);

	      for (octave_idx_type i = 0; i < nrows; i++)
		{
		  perm [i] = -1;
		  tmp [i] = -1;
		}

	      for (octave_idx_type j = 0; j < ncols; j++)
		for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
		    perm [a.ridx(i)] = j;

	      found = true;
	      for (octave_idx_type i = 0; i < nm; i++)
		if (perm[i] == -1)
		  {
		    found = false;
		    break;
		  }
		else
		  {
		    tmp[perm[i]] = 1;
		  }

	      if (found)
		{
		  octave_idx_type k = ncols;
		  for (octave_idx_type i = 0; i < nrows; i++)
		    {
		      if (tmp[i] == -1)
			{
			  if (k < nrows)
			    {
			      perm[k++] = i;
			    }
			  else
			    {
			      found = false;
			      break;
			    }
			}
		    }
		}

	      if (found)
		typ = SparseType::Permuted_Lower;
	      else
		{
		  delete [] perm;
		  nperm = 0;
		}
	    }
	  else
	    {
	      delete [] perm;
	      nperm = 0;
	    }
	}

      // FIXME
      // Disable lower under-determined and upper over-determined problems
      // as being detected, and force to treat as singular. As this seems
      // to cause issues
      if (((typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
	   && nrows > ncols) ||
	  ((typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
	   && nrows < ncols))
	{
	  typ = SparseType::Rectangular;
	  if (typ == SparseType::Permuted_Upper ||
	      typ == SparseType::Permuted_Lower)
	    delete [] perm;
	  nperm = 0;
	}

      if (typ == SparseType::Full && ncols != nrows)
	typ = SparseType::Rectangular;

      if (maybe_hermitian && (typ == SparseType::Full || 
			      typ == SparseType::Tridiagonal || 
			      typ == SparseType::Banded))
	{
	  // Check for symmetry, with positive real diagonal, which
	  // has a very good chance of being symmetric positive
	  // definite..
	  bool is_herm = true;

	  for (octave_idx_type j = 0; j < ncols; j++)
	    {
	      bool diag_positive = false;

	      for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
		{
		  octave_idx_type ri = a.ridx(i);

		  if (ri == j)
		    {
		      if (a.data(i) == std::abs(a.data(i)))
			diag_positive = true;
		      else
			break;
		    }
		  else
		    {
		      bool found = false;

		      for (octave_idx_type k = a.cidx(ri); k < a.cidx(ri+1); k++)
			{
			  if (a.ridx(k) == j)
			    {
			      if (a.data(i) == conj(a.data(k)))
				found = true;
			      break;
			    }
			}

		      if (! found)
			{
			  is_herm = false;
			  break;
			}
		    }
		}

	      if (! diag_positive || ! is_herm)
		{
		  is_herm = false;
		  break;
		} 
	    }

	  if (is_herm)
	    {
	      if (typ == SparseType::Full)
		typ = SparseType::Hermitian;
	      else if (typ == SparseType::Banded)
		typ = SparseType::Banded_Hermitian;
	      else
		typ = SparseType::Tridiagonal_Hermitian;
	    }
	}
    }
}

SparseType::SparseType (const matrix_type t) : typ (SparseType::Unknown), 
					       nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");

  if (t == SparseType::Full || t == SparseType::Diagonal ||
      t == SparseType::Permuted_Diagonal || t == SparseType::Upper ||
      t == SparseType::Lower || t == SparseType::Tridiagonal ||
      t == SparseType::Tridiagonal_Hermitian || t == SparseType::Rectangular)
    typ = t;
  else
    (*current_liboctave_warning_handler) ("Invalid sparse matrix type");
}

SparseType::SparseType (const matrix_type t, const octave_idx_type np,
			const octave_idx_type *p) : typ (SparseType::Unknown), 
					       nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");

  if (t == SparseType::Permuted_Upper || t == SparseType::Permuted_Lower)
    {
      typ = t;
      nperm = np;
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
	perm[i] = p[i];
    }
  else
    (*current_liboctave_warning_handler) ("Invalid sparse matrix type");
}

SparseType::SparseType (const matrix_type t, const octave_idx_type ku,
			const octave_idx_type kl) : typ (SparseType::Unknown), 
					       nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");

  if (t == SparseType::Banded || t == SparseType::Banded_Hermitian)
    {
      typ = t;
      upper_band = ku;
      lower_band = kl;
    }
  else
    (*current_liboctave_warning_handler) ("Invalid sparse matrix type"); 
}

SparseType::~SparseType (void) 
{ 
  if (nperm != 0)
    {
      delete [] perm; 
    }
}

SparseType& 
SparseType::operator = (const SparseType& a)
{
  if (this != &a)
    {
      typ = a.typ;
      sp_bandden = a.sp_bandden;
      bandden = a.bandden;
      upper_band = a.upper_band;
      lower_band = a.lower_band;
      dense = a.dense;
      nperm = a.nperm;

      if (nperm != 0)
	{
	  perm = new octave_idx_type [nperm];
	  for (octave_idx_type i = 0; i < nperm; i++)
	    perm[i] = a.perm[i];
	}
    }

  return *this;
}

int
SparseType::type (bool quiet)
{
  if (typ != SparseType::Unknown && 
      sp_bandden == Voctave_sparse_controls.get_key ("bandden"))
    {
      if (!quiet &&
	  Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Sparse Matrix Type");
      
      return typ;
    }

  if (typ != SparseType::Unknown && 
      Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Invalidating Sparse Matrix Type");

  typ = SparseType::Unknown;

  return typ;
}

int
SparseType::type (const SparseMatrix &a)
{
  if (typ != SparseType::Unknown && 
      sp_bandden == Voctave_sparse_controls.get_key ("bandden"))
    {
      if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Sparse Matrix Type");
      
      return typ;
    }

  SparseType tmp_typ (a);
  typ = tmp_typ.typ;
  sp_bandden = tmp_typ.sp_bandden;
  bandden = tmp_typ.bandden;
  upper_band = tmp_typ.upper_band;
  lower_band = tmp_typ.lower_band;
  dense = tmp_typ.dense;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
	perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

int
SparseType::type (const SparseComplexMatrix &a)
{
  if (typ != SparseType::Unknown && 
      sp_bandden == Voctave_sparse_controls.get_key ("bandden"))
    {
      if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Sparse Matrix Type");
      
      return typ;
    }

  SparseType tmp_typ (a);
  typ = tmp_typ.typ;
  sp_bandden = tmp_typ.sp_bandden;
  bandden = tmp_typ.bandden;
  upper_band = tmp_typ.upper_band;
  lower_band = tmp_typ.lower_band;
  dense = tmp_typ.dense;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
	perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

void
SparseType::info (void) const
{
  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    {
      if (typ == SparseType::Unknown)
	(*current_liboctave_warning_handler) 
	  ("Unknown Sparse Matrix Type");
      else if (typ == SparseType::Diagonal)
	(*current_liboctave_warning_handler) 
	  ("Diagonal Sparse Matrix");
      else if (typ == SparseType::Permuted_Diagonal)
	(*current_liboctave_warning_handler) 
	  ("Permuted Diagonal Sparse Matrix");
      else if (typ == SparseType::Upper)
	(*current_liboctave_warning_handler) 
	  ("Upper Triangular Sparse Matrix");
      else if (typ == SparseType::Lower)
	(*current_liboctave_warning_handler) 
	  ("Lower Triangular Sparse Matrix");
      else if (typ == SparseType::Permuted_Upper)
	(*current_liboctave_warning_handler) 
	  ("Permuted Upper Triangular Sparse Matrix");
      else if (typ == SparseType::Permuted_Lower)
	(*current_liboctave_warning_handler) 
	  ("Permuted Lower Triangular Sparse Matrix");
      else if (typ == SparseType::Banded)
	(*current_liboctave_warning_handler) 
	  ("Banded Sparse Matrix %d-1-%d (Density %f)", lower_band, 
	   upper_band, bandden);
      else if (typ == SparseType::Banded_Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Banded Hermitian/Symmetric Sparse Matrix %d-1-%d (Density %f)", 
	   lower_band, upper_band, bandden);
      else if (typ == SparseType::Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Hermitian/Symmetric Sparse Matrix");
      else if (typ == SparseType::Tridiagonal)
	(*current_liboctave_warning_handler) 
	  ("Tridiagonal Sparse Matrix");
      else if (typ == SparseType::Tridiagonal_Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Hermitian/Symmetric Tridiagonal Sparse Matrix");
      else if (typ == SparseType::Rectangular)
	(*current_liboctave_warning_handler) 
	  ("Rectangular/Singular Sparse Matrix");
      else if (typ == SparseType::Full)
	(*current_liboctave_warning_handler) 
	  ("Full Sparse Matrix");
    }
}

void
SparseType::mark_as_symmetric (void)
{
  if (typ == SparseType::Tridiagonal || 
      typ == SparseType::Tridiagonal_Hermitian)
    typ = SparseType::Tridiagonal_Hermitian;
  else if (typ == SparseType::Banded ||
	   typ == SparseType::Banded_Hermitian)
    typ = SparseType::Banded_Hermitian;
  else if (typ == SparseType::Full || typ == SparseType::Hermitian || 
	   typ == SparseType::Unknown)
    typ = SparseType::Hermitian;
  else
    (*current_liboctave_error_handler) 
      ("Can not mark current matrix type as symmetric");
}

void
SparseType::mark_as_unsymmetric (void)
{
  if (typ == SparseType::Tridiagonal || 
      typ == SparseType::Tridiagonal_Hermitian)
    typ = SparseType::Tridiagonal;
  else if (typ == SparseType::Banded ||
	   typ == SparseType::Banded_Hermitian)
    typ = SparseType::Banded;
  else if (typ == SparseType::Full || typ == SparseType::Hermitian || 
	   typ == SparseType::Unknown)
    typ = SparseType::Full;
}

void
SparseType::mark_as_permuted (const octave_idx_type np, const octave_idx_type *p)
{
  nperm = np;
  perm = new octave_idx_type [nperm];
  for (octave_idx_type i = 0; i < nperm; i++)
    perm[i] = p[i];

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    typ = SparseType::Permuted_Diagonal;
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    typ = SparseType::Permuted_Upper;
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    typ = SparseType::Permuted_Lower;
  else
    (*current_liboctave_error_handler) 
      ("Can not mark current matrix type as symmetric");
}

void
SparseType::mark_as_unpermuted (void)
{
  if (nperm)
    {
      nperm = 0;
      delete [] perm;
    }

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    typ = SparseType::Diagonal;
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    typ = SparseType::Upper;
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    typ = SparseType::Lower;
}

SparseType
SparseType::transpose (void) const
{
  SparseType retval (*this);
  if (typ == SparseType::Upper)
    retval.typ = SparseType::Lower;
  else if (typ == SparseType::Permuted_Upper)
    retval.typ = SparseType::Permuted_Lower;
  else if (typ == SparseType::Lower)
    retval.typ = SparseType::Upper;
  else if (typ == SparseType::Permuted_Lower)
    retval.typ = SparseType::Permuted_Upper;
  else if (typ == SparseType::Banded)
    {
      retval.upper_band = lower_band;
      retval.lower_band = upper_band;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

