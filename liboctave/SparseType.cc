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

#include "SparseType.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-spparms.h"

// XXX FIXME XXX There is a large code duplication here

SparseType::SparseType (const SparseType &a) : typ (a.typ), 
    sp_bandden (a.sp_bandden), bandden (a.bandden), 
    upper_band (a.upper_band), lower_band (a.lower_band), 
    dense (a.dense), nperm (a.nperm)
{ 
  if (nperm != 0)
    {
      row_perm = new int [nperm];
      col_perm = new int [nperm];
      for (int i = 0; i < nperm; i++)
	{
	  row_perm[i] = a.row_perm[i];
	  col_perm[i] = a.col_perm[i];
	}
    }
}

SparseType::SparseType (const SparseMatrix &a)
{
  int nrows = a.rows ();
  int ncols = a.cols ();
  int nnz = a.nnz ();

  nperm = 0;

  if (nrows != ncols)
    typ = SparseType::Rectangular;
  else
    {
      sp_bandden = Voctave_sparse_controls.get_key ("bandden");
      bool maybe_hermitian = false;
      typ = SparseType::Full;

      if (nnz == ncols)
	{
	  matrix_type tmp_typ = SparseType::Diagonal;
	  int i;
	  // Maybe the matrix is diagonal
	  for (i = 0; i < ncols; i++)
	    {
	      if (a.cidx(i+1) != a.cidx(i) + 1)
		{
		  tmp_typ = Full;
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
	      bool found [ncols];

	      for (int j = 0; j < i; j++)
		found [j] = true;
	      for (int j = i; j < ncols; j++)
		found [j] = false;
	      
	      for (int j = i; j < ncols; j++)
		{
		  if ((a.cidx(j+1) != a.cidx(j) + 1) || found [a.ridx(j)])
		    {
		      tmp_typ = Full;
		      break;
		    }
		  found [a.ridx(j)] = true;
		}
	    }
	  typ = tmp_typ;
	}

      if (typ == Full)
	{
	  // Search for banded, upper and lower triangular matrices
	  bool singular = false;
	  upper_band = 0;
	  lower_band = 0;
	  for (int j = 0; j < ncols; j++)
	    {
	      bool zero_on_diagonal = true;
	      for (int i = a.cidx(j); i < a.cidx(j+1); i++)
		if (a.ridx(i) == j)
		  {
		    zero_on_diagonal = false;
		    break;
		  }

	      if (zero_on_diagonal)
		{
		  singular = true;
		  break;
		}

	      if (a.cidx(j+1) - a.cidx(j) > 0)
		{
		  int ru = a.ridx(a.cidx(j));
		  int rl = a.ridx(a.cidx(j+1)-1);

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

	      if (sp_bandden != 1. && bandden > sp_bandden)
		{
		  if (upper_band == 1 && lower_band == 1)
		    typ = SparseType::Tridiagonal;
		  else
		    typ = SparseType::Banded;

		  int nnz_in_band = (upper_band + lower_band + 1) * nrows -
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

	      if (upper_band == lower_band)
		maybe_hermitian = true;
	    }

	  if (typ == Full)
	    {
	      // Search for a permuted triangular matrix, and test if
	      // permutation is singular

	      // XXX FIXME XXX Write this test based on dmperm
	      
	    }
	}

      if (maybe_hermitian && (typ == Full || typ == Tridiagonal || 
			      typ == Banded))
	{
	  // Check for symmetry, with positive real diagonal, which
	  // has a very good chance of being symmetric positive
	  // definite..
	  bool is_herm = true;

	  for (int j = 0; j < ncols; j++)
	    {
	      bool diag_positive = false;

	      for (int i = a.cidx(j); i < a.cidx(j+1); i++)
		{
		  int ri = a.ridx(i);

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

		      for (int k = a.cidx(ri); k < a.cidx(ri+1); k++)
			{
			  if (a.ridx(k) == j)
			    {
			      if (a.data(i) == conj (a.data(k)))
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
	      if (typ == Full)
		typ = Hermitian;
	      else if (typ == Banded)
		typ = Banded_Hermitian;
	      else
		typ = Tridiagonal_Hermitian;
	    }
	}
    }
}

SparseType::SparseType (const SparseComplexMatrix &a)
{
  int nrows = a.rows ();
  int ncols = a.cols ();
  int nnz = a.nnz ();

  nperm = 0;

  if (nrows != ncols)
    typ = SparseType::Rectangular;
  else
    {
      sp_bandden = Voctave_sparse_controls.get_key ("bandden");
      bool maybe_hermitian = false;
      typ = SparseType::Full;

      if (nnz == ncols)
	{
	  matrix_type tmp_typ = SparseType::Diagonal;
	  int i;
	  // Maybe the matrix is diagonal
	  for (i = 0; i < ncols; i++)
	    {
	      if (a.cidx(i+1) != a.cidx(i) + 1)
		{
		  tmp_typ = Full;
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
	      bool found [ncols];

	      for (int j = 0; j < i; j++)
		found [j] = true;
	      for (int j = i; j < ncols; j++)
		found [j] = false;
	      
	      for (int j = i; j < ncols; j++)
		{
		  if ((a.cidx(j+1) != a.cidx(j) + 1) || found [a.ridx(j)])
		    {
		      tmp_typ = Full;
		      break;
		    }
		  found [a.ridx(j)] = true;
		}
	    }
	  typ = tmp_typ;
	}

      if (typ == Full)
	{
	  // Search for banded, upper and lower triangular matrices
	  bool singular = false;
	  upper_band = 0;
	  lower_band = 0;
	  for (int j = 0; j < ncols; j++)
	    {
	      bool zero_on_diagonal = true;
	      for (int i = a.cidx(j); i < a.cidx(j+1); i++)
		if (a.ridx(i) == j)
		  {
		    zero_on_diagonal = false;
		    break;
		  }

	      if (zero_on_diagonal)
		{
		  singular = true;
		  break;
		}

	      if (a.cidx(j+1) - a.cidx(j) > 0)
		{
		  int ru = a.ridx(a.cidx(j));
		  int rl = a.ridx(a.cidx(j+1)-1);

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

	      if (sp_bandden != 1. && bandden > sp_bandden)
		{
		  if (upper_band == 1 && lower_band == 1)
		    typ = SparseType::Tridiagonal;
		  else
		    typ = SparseType::Banded;

		  int nnz_in_band = (upper_band + lower_band + 1) * nrows -
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

	      if (upper_band == lower_band)
		maybe_hermitian = true;
	    }

	  if (typ == Full)
	    {
	      // Search for a permuted triangular matrix, and test if
	      // permutation is singular

	      // XXX FIXME XXX Write this test based on dmperm
	      
	    }
	}

      if (maybe_hermitian && (typ == Full || typ == Tridiagonal || 
			      typ == Banded))
	{
	  // Check for symmetry, with positive real diagonal, which
	  // has a very good chance of being symmetric positive
	  // definite..
	  bool is_herm = true;

	  for (int j = 0; j < ncols; j++)
	    {
	      bool diag_positive = false;

	      for (int i = a.cidx(j); i < a.cidx(j+1); i++)
		{
		  int ri = a.ridx(i);

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

		      for (int k = a.cidx(ri); k < a.cidx(ri+1); k++)
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
	      if (typ == Full)
		typ = Hermitian;
	      else if (typ == Banded)
		typ = Banded_Hermitian;
	      else
		typ = Tridiagonal_Hermitian;
	    }
	}
    }
}

SparseType::~SparseType (void) 
{ 
  if (nperm != 0)
    {
      delete [] row_perm; 
      delete [] col_perm; 
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
	  row_perm = new int [nperm];
	  col_perm = new int [nperm];
	  for (int i = 0; i < nperm; i++)
	    {
	      row_perm[i] = a.row_perm[i];
	      col_perm[i] = a.col_perm[i];
	    }
    }

    }
  return *this;
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

  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Calculating Sparse Matrix Type");


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
      row_perm = new int [nperm];
      col_perm = new int [nperm];
      for (int i = 0; i < nperm; i++)
	{
	  row_perm[i] = tmp_typ.row_perm[i];
	  col_perm[i] = tmp_typ.col_perm[i];
	}
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

  if (Voctave_sparse_controls.get_key ("spumoni") != 0.)
    (*current_liboctave_warning_handler) 
      ("Calculating Sparse Matrix Type");


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
      row_perm = new int [nperm];
      col_perm = new int [nperm];
      for (int i = 0; i < nperm; i++)
	{
	  row_perm[i] = tmp_typ.row_perm[i];
	  col_perm[i] = tmp_typ.col_perm[i];
	}
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
	  ("Banded Sparse Matrix %g-1-%g (Density %g)", lower_band, 
	   upper_band, bandden);
      else if (typ == SparseType::Banded_Hermitian)
	(*current_liboctave_warning_handler) 
	  ("Banded Hermitian/Symmetric Sparse Matrix %g-1-%g (Density %g)", 
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
	  ("Rectangular Sparse Matrix");
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
SparseType::mark_as_permuted (const int np, const int *pr, const int *pc)
{
  nperm = np;
  row_perm = new int [nperm];
  col_perm = new int [nperm];
  for (int i = 0; i < nperm; i++)
    {
      row_perm[i] = pr[i];
      col_perm[i] = pc[i];
    }

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
      delete [] row_perm;
      delete [] col_perm;
    }

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    typ = SparseType::Diagonal;
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    typ = SparseType::Upper;
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    typ = SparseType::Lower;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

