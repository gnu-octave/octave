/*

Copyright (C) 2006 David Bateman
Copyright (C) 2006 Andy Adler

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

#include "MatrixType.h"
#include "dMatrix.h"
#include "CMatrix.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-spparms.h"

// FIXME There is a large code duplication here

MatrixType::MatrixType (void) : typ (MatrixType::Unknown), full (false),
				nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
} 

MatrixType::MatrixType (const MatrixType &a) : typ (a.typ), 
    sp_bandden (a.sp_bandden), bandden (a.bandden), 
    upper_band (a.upper_band), lower_band (a.lower_band), 
    dense (a.dense), full (a.full), nperm (a.nperm)
{ 
  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
	perm[i] = a.perm[i];
    }
}

MatrixType::MatrixType (const Matrix &a)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  nperm = 0;
  full = true;

  if (ncols == nrows)
    {
      bool upper = true;
      bool lower = true;
      bool hermitian = true;

      for (octave_idx_type j = 0; j < ncols; j++)
	{
	  if (j < nrows)
	    {
	      if (a.elem (j,j) == 0.)
		{
		  upper = false;
		  lower = false;
		  hermitian = false;
		  break;
		}
	      if (a.elem (j,j) < 0.)
		hermitian = false;
	    }      
	  for (octave_idx_type i = 0; i < j; i++)
	    if (lower && a.elem (i,j) != 0.)
	      {
		lower = false;
		break;
	      }
	  for (octave_idx_type i = j+1; i < nrows; i++)
	    {
	      if (hermitian && a.elem (i, j) != a.elem (j, i))
		hermitian = false;
	      if (upper && a.elem (i,j) != 0)
		upper = false;
	    }
	  if (!upper && !lower && !hermitian)
	    break;
	}

      if (upper)
	typ = MatrixType::Upper;
      else if (lower)
	typ = MatrixType::Lower;
      else if (hermitian)
	typ = MatrixType::Hermitian;
      else if (ncols == nrows)
	typ = MatrixType::Full;
    }
  else
    typ = MatrixType::Rectangular;
}

MatrixType::MatrixType (const ComplexMatrix &a)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  nperm = 0;
  full = true;

  if (ncols == nrows)
    {
      bool upper = true;
      bool lower = true;
      bool hermitian = true;

      for (octave_idx_type j = 0; j < ncols; j++)
	{
	  if (j < ncols)
	    {
	      if (imag(a.elem (j,j)) == 0. && 
		  real(a.elem (j,j)) == 0.)
		{
		  upper = false;
		  lower = false;
		  hermitian = false;
		  break;
		}

	      if (imag(a.elem (j,j)) != 0. || 
		  real(a.elem (j,j)) < 0.)
		    hermitian = false;
	    }
	  for (octave_idx_type i = 0; i < j; i++)
	    if (lower && (real(a.elem (i,j)) != 0 || imag(a.elem (i,j)) != 0))
	      {
		lower = false;
		break;
	      }
	  for (octave_idx_type i = j+1; i < nrows; i++)
	    {
	      if (hermitian && a.elem (i, j) != conj(a.elem (j, i)))
		hermitian = false;
	      if (upper && (real(a.elem (i,j)) != 0 || 
			    imag(a.elem (i,j)) != 0))
		upper = false;
	    }
	  if (!upper && !lower && !hermitian)
	    break;
	}

      if (upper)
	typ = MatrixType::Upper;
      else if (lower)
	typ = MatrixType::Lower;
      else if (hermitian)
	typ = MatrixType::Hermitian;
      else if (ncols == nrows)
	typ = MatrixType::Full;
    }
  else
    typ = MatrixType::Rectangular;
}

MatrixType::MatrixType (const SparseMatrix &a)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nzmax ();
  full = false;

  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Calculating Sparse Matrix Type");

  nperm = 0;

  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  bool maybe_hermitian = false;
  typ = MatrixType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = MatrixType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
	{
	  if (a.cidx(i+1) != a.cidx(i) + 1)
	    {
	      tmp_typ = MatrixType::Full;
	      break;
	    }
	  if (a.ridx(i) != i)
	    {
	      tmp_typ = MatrixType::Permuted_Diagonal;
	      break;
	    }
	}
	  
      if (tmp_typ == MatrixType::Permuted_Diagonal)
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
		  tmp_typ = MatrixType::Full;
		  break;
		}
	      found [a.ridx(j)] = true;
	    }
	}
      typ = tmp_typ;
    }

  if (typ == MatrixType::Full)
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
		typ = MatrixType::Tridiagonal;
	      else
		typ = MatrixType::Banded;

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
	    typ = MatrixType::Lower;
	  else if (lower_band == 0)
	    typ = MatrixType::Upper;

	  if (upper_band == lower_band && nrows == ncols)
	    maybe_hermitian = true;
	}

      if (typ == MatrixType::Full)
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
	      typ = MatrixType::Permuted_Upper;
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
		typ = MatrixType::Permuted_Lower;
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
      if (((typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
	   && nrows > ncols) ||
	  ((typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
	   && nrows < ncols))
	{
	  typ = MatrixType::Rectangular;
	  if (typ == MatrixType::Permuted_Upper ||
	      typ == MatrixType::Permuted_Lower)
	    delete [] perm;
	  nperm = 0;
	}

      if (typ == MatrixType::Full && ncols != nrows)
	typ = MatrixType::Rectangular;

      if (maybe_hermitian && (typ == MatrixType::Full || 
			      typ == MatrixType::Tridiagonal || 
			      typ == MatrixType::Banded))
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
	      if (typ == MatrixType::Full)
		typ = MatrixType::Hermitian;
	      else if (typ == MatrixType::Banded)
		typ = MatrixType::Banded_Hermitian;
	      else
		typ = MatrixType::Tridiagonal_Hermitian;
	    }
	}
    }
}

MatrixType::MatrixType (const SparseComplexMatrix &a)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nzmax ();
  full = false;

  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)  full = true;

    (*current_liboctave_warning_handler) 
      ("Calculating Sparse Matrix Type");

  nperm = 0;

  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  bool maybe_hermitian = false;
  typ = MatrixType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = MatrixType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
	{
	  if (a.cidx(i+1) != a.cidx(i) + 1)
	    {
	      tmp_typ = MatrixType::Full;
	      break;
	    }
	  if (a.ridx(i) != i)
	    {
	      tmp_typ = MatrixType::Permuted_Diagonal;
	      break;
	    }
	}
	  
      if (tmp_typ == MatrixType::Permuted_Diagonal)
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
		  tmp_typ = MatrixType::Full;
		  break;
		}
	      found [a.ridx(j)] = true;
	    }
	}
      typ = tmp_typ;
    }

  if (typ == MatrixType::Full)
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
		typ = MatrixType::Tridiagonal;
	      else
		typ = MatrixType::Banded;

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
	    typ = MatrixType::Lower;
	  else if (lower_band == 0)
	    typ = MatrixType::Upper;

	  if (upper_band == lower_band && nrows == ncols)
	    maybe_hermitian = true;
	}

      if (typ == MatrixType::Full)
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
	      typ = MatrixType::Permuted_Upper;
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
		typ = MatrixType::Permuted_Lower;
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
      if (((typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
	   && nrows > ncols) ||
	  ((typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
	   && nrows < ncols))
	{
	  typ = MatrixType::Rectangular;
	  if (typ == MatrixType::Permuted_Upper ||
	      typ == MatrixType::Permuted_Lower)
	    delete [] perm;
	  nperm = 0;
	}

      if (typ == MatrixType::Full && ncols != nrows)
	typ = MatrixType::Rectangular;

      if (maybe_hermitian && (typ == MatrixType::Full || 
			      typ == MatrixType::Tridiagonal || 
			      typ == MatrixType::Banded))
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
	      if (typ == MatrixType::Full)
		typ = MatrixType::Hermitian;
	      else if (typ == MatrixType::Banded)
		typ = MatrixType::Banded_Hermitian;
	      else
		typ = MatrixType::Tridiagonal_Hermitian;
	    }
	}
    }
}
MatrixType::MatrixType (const matrix_type t, bool _full) : 
  typ (MatrixType::Unknown), nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  full = _full;

  if (t == MatrixType::Full || t == MatrixType::Diagonal ||
      t == MatrixType::Permuted_Diagonal || t == MatrixType::Upper ||
      t == MatrixType::Lower || t == MatrixType::Tridiagonal ||
      t == MatrixType::Tridiagonal_Hermitian || t == MatrixType::Rectangular)
    typ = t;
  else
    (*current_liboctave_warning_handler) ("Invalid matrix type");
}

MatrixType::MatrixType (const matrix_type t, const octave_idx_type np,
			const octave_idx_type *p, bool _full) : 
  typ (MatrixType::Unknown), nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  full = _full;

  if (t == MatrixType::Permuted_Upper || t == MatrixType::Permuted_Lower)
    {
      typ = t;
      nperm = np;
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
	perm[i] = p[i];
    }
  else
    (*current_liboctave_warning_handler) ("Invalid matrix type");
}

MatrixType::MatrixType (const matrix_type t, const octave_idx_type ku,
			const octave_idx_type kl, bool _full) : 
  typ (MatrixType::Unknown), nperm (0)
{
  sp_bandden = Voctave_sparse_controls.get_key ("bandden");
  full = _full;

  if (t == MatrixType::Banded || t == MatrixType::Banded_Hermitian)
    {
      typ = t;
      upper_band = ku;
      lower_band = kl;
    }
  else
    (*current_liboctave_warning_handler) ("Invalid sparse matrix type"); 
}

MatrixType::~MatrixType (void) 
{ 
  if (nperm != 0)
    {
      delete [] perm; 
    }
}

MatrixType& 
MatrixType::operator = (const MatrixType& a)
{
  if (this != &a)
    {
      typ = a.typ;
      sp_bandden = a.sp_bandden;
      bandden = a.bandden;
      upper_band = a.upper_band;
      lower_band = a.lower_band;
      dense = a.dense;
      full = a.full;
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
MatrixType::type (bool quiet)
{
  if (typ != MatrixType::Unknown && (full ||
      sp_bandden == Voctave_sparse_controls.get_key ("bandden")))
    {
      if (!quiet &&
	  Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Matrix Type");
      
      return typ;
    }

  if (typ != MatrixType::Unknown && 
      Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Invalidating Matrix Type");

  typ = MatrixType::Unknown;

  return typ;
}

int
MatrixType::type (const SparseMatrix &a)
{
  if (typ != MatrixType::Unknown && (full ||
      sp_bandden == Voctave_sparse_controls.get_key ("bandden")))
    {
      if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Matrix Type");
      
      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  sp_bandden = tmp_typ.sp_bandden;
  bandden = tmp_typ.bandden;
  upper_band = tmp_typ.upper_band;
  lower_band = tmp_typ.lower_band;
  dense = tmp_typ.dense;
  full = tmp_typ.full;
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
MatrixType::type (const SparseComplexMatrix &a)
{
  if (typ != MatrixType::Unknown && (full || 
      sp_bandden == Voctave_sparse_controls.get_key ("bandden")))
    {
      if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Matrix Type");
      
      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  sp_bandden = tmp_typ.sp_bandden;
  bandden = tmp_typ.bandden;
  upper_band = tmp_typ.upper_band;
  lower_band = tmp_typ.lower_band;
  dense = tmp_typ.dense;
  full = tmp_typ.full;
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
MatrixType::type (const Matrix &a)
{
  if (typ != MatrixType::Unknown)
    {
      if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Matrix Type");
      
      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  full = tmp_typ.full;
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
MatrixType::type (const ComplexMatrix &a)
{
  if (typ != MatrixType::Unknown)
    {
      if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
  	(*current_liboctave_warning_handler) 
  	  ("Using Cached Matrix Type");
      
      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  full = tmp_typ.full; 
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
MatrixType::info () const
{
  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    {
      if (typ == MatrixType::Unknown)
	(*current_liboctave_warning_handler) 
	  ("Unknown Matrix Type");
      else if (typ == MatrixType::Diagonal)
	(*current_liboctave_warning_handler) 
	  ("Diagonal Sparse Matrix");
      else if (typ == MatrixType::Permuted_Diagonal)
	(*current_liboctave_warning_handler) 
	  ("Permuted Diagonal Sparse Matrix");
      else if (typ == MatrixType::Upper)
	(*current_liboctave_warning_handler) 
	  ("Upper Triangular Matrix");
      else if (typ == MatrixType::Lower)
	(*current_liboctave_warning_handler) 
	  ("Lower Triangular Matrix");
      else if (typ == MatrixType::Permuted_Upper)
	(*current_liboctave_warning_handler) 
	  ("Permuted Upper Triangular Matrix");
      else if (typ == MatrixType::Permuted_Lower)
	(*current_liboctave_warning_handler) 
	  ("Permuted Lower Triangular Matrix");
      else if (typ == MatrixType::Banded)
	(*current_liboctave_warning_handler) 
	  ("Banded Sparse Matrix %d-1-%d (Density %f)", lower_band, 
	   upper_band, bandden);
      else if (typ == MatrixType::Banded_Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Banded Hermitian/Symmetric Sparse Matrix %d-1-%d (Density %f)", 
	   lower_band, upper_band, bandden);
      else if (typ == MatrixType::Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Hermitian/Symmetric Matrix");
      else if (typ == MatrixType::Tridiagonal)
	(*current_liboctave_warning_handler) 
	  ("Tridiagonal Sparse Matrix");
      else if (typ == MatrixType::Tridiagonal_Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Hermitian/Symmetric Tridiagonal Sparse Matrix");
      else if (typ == MatrixType::Rectangular)
	(*current_liboctave_warning_handler) 
	  ("Rectangular/Singular Matrix");
      else if (typ == MatrixType::Full)
	(*current_liboctave_warning_handler) 
	  ("Full Matrix");
    }
}

void
MatrixType::mark_as_symmetric (void)
{
  if (typ == MatrixType::Tridiagonal || 
      typ == MatrixType::Tridiagonal_Hermitian)
    typ = MatrixType::Tridiagonal_Hermitian;
  else if (typ == MatrixType::Banded ||
	   typ == MatrixType::Banded_Hermitian)
    typ = MatrixType::Banded_Hermitian;
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian || 
	   typ == MatrixType::Unknown)
    typ = MatrixType::Hermitian;
  else
    (*current_liboctave_error_handler) 
      ("Can not mark current matrix type as symmetric");
}

void
MatrixType::mark_as_unsymmetric (void)
{
  if (typ == MatrixType::Tridiagonal || 
      typ == MatrixType::Tridiagonal_Hermitian)
    typ = MatrixType::Tridiagonal;
  else if (typ == MatrixType::Banded ||
	   typ == MatrixType::Banded_Hermitian)
    typ = MatrixType::Banded;
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian || 
	   typ == MatrixType::Unknown)
    typ = MatrixType::Full;
}

void
MatrixType::mark_as_permuted (const octave_idx_type np, const octave_idx_type *p)
{
  nperm = np;
  perm = new octave_idx_type [nperm];
  for (octave_idx_type i = 0; i < nperm; i++)
    perm[i] = p[i];

  if (typ == MatrixType::Diagonal || typ == MatrixType::Permuted_Diagonal)
    typ = MatrixType::Permuted_Diagonal;
  else if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    typ = MatrixType::Permuted_Upper;
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    typ = MatrixType::Permuted_Lower;
  else
    (*current_liboctave_error_handler) 
      ("Can not mark current matrix type as symmetric");
}

void
MatrixType::mark_as_unpermuted (void)
{
  if (nperm)
    {
      nperm = 0;
      delete [] perm;
    }

  if (typ == MatrixType::Diagonal || typ == MatrixType::Permuted_Diagonal)
    typ = MatrixType::Diagonal;
  else if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    typ = MatrixType::Upper;
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    typ = MatrixType::Lower;
}

MatrixType
MatrixType::transpose (void) const
{
  MatrixType retval (*this);
  if (typ == MatrixType::Upper)
    retval.typ = MatrixType::Lower;
  else if (typ == MatrixType::Permuted_Upper)
    retval.typ = MatrixType::Permuted_Lower;
  else if (typ == MatrixType::Lower)
    retval.typ = MatrixType::Upper;
  else if (typ == MatrixType::Permuted_Lower)
    retval.typ = MatrixType::Permuted_Upper;
  else if (typ == MatrixType::Banded)
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

