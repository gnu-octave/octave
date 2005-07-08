/*

Copyright (C) 2005 David Bateman

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

#include <algorithm>

#include "ov.h"
#include "defun-dld.h"
#include "error.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "SparseType.h"

DEFUN_DLD (matrix_type, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{type} =} matrix_type (@var{a})\n\
@deftypefnx {Loadable Function} {@var{a} =} matrix_type (@var{a}, @var{type})\n\
@deftypefnx {Loadable Function} {@var{a} =} matrix_type (@var{a}, 'upper', @var{perm})\n\
@deftypefnx {Loadable Function} {@var{a} =} matrix_type (@var{a}, 'lower', @var{perm})\n\
@deftypefnx {Loadable Function} {@var{a} =} matrix_type (@var{a}, 'banded', @var{nl}, @var{nu})\n\
Identify the matrix type or mark a matrix as a particular type. This allows rapid\n\
for solutions of linear equations involving @var{a} to be performed. Called with a\n\
single argument, @code{matrix_type} returns the type of the matrix and caches it for\n\
future use. Called with more than one argument, @code{matrix_type} allows the type\n\
of the matrix to be defined.\n\
\n\
The possible matrix types depend on whether the matrix is full or sparse, and can be\n\
one of the following\n\
\n\
@table @asis\n\
@item 'unknown'\n\
Remove any previously cached matrix type, and mark type as unknown\n\
\n\
@item 'full'\n\
Mark the matrix as full.\n\
\n\
@item 'positive definite'\n\
Full positive definite matrix.\n\
\n\
@item 'diagonal'\n\
Diagonal Matrix. (Sparse matrices only)\n\
\n\
@item 'permuted diagonal'\n\
Permuted Diagonal matrix. The permutation does not need to be specifically\n\
indicated, as the structure of the matrix explicitly gives this. (Sparse matrices\n\
only)\n\
\n\
@item 'upper'\n\
Upper triangular. If the optional third argument @var{perm} is given, the matrix is\n\
assumed to be a permuted upper triangular with the permutations defined by the\n\
vector @var{perm}.\n\
\n\
@item 'lower'\n\
Lower triangular. If the optional third argument @var{perm} is given, the matrix is\n\
assumed to be a permuted lower triangular with the permutations defined by the\n\
vector @var{perm}.\n\
\n\
@item 'banded'\n\
@itemx 'banded positive definite'\n\
Banded matrix with the band size of @var{nl} below the diagonal and @var{nu} above\n\
it. If @var{nl} and @var{nu} are 1, then the matrix is tridiagonal and treated\n\
with specialized code. In addition the matrix can be marked as positive definite\n\
(Sparse matrices only)\n\
\n\
@item 'singular'\n\
The matrix is assumed to be singular and will be treated with a minimum norm solution\n\
\n\
@end table\n\
\n\
Note that the matrix type will be discovered automatically on the first attempt to\n\
solve a linear equation involving @var{a}. Therefore @code{matrix_type} is only\n\
useful to give Octave hints of the matrix type. Incorrectly defining the\n\
matrix type will result in incorrect results from solutions of linear equations,\n\
and so it is entirely the responsibility of the user to correctly indentify the\n\
matrix type.\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin == 0)
    print_usage ("matrix_type");
  else if (nargin > 4)
    error ("matrix_type: incorrect number of arguments");
  else
    {
      if (args(0).class_name () == "sparse") 
	{
	  if (nargin == 1)
	    {
	      SparseType mattyp;
	      const octave_value& rep = args(0).get_rep ();

	      if (args(0).type_name () == "sparse complex matrix" ) 
		{
		  mattyp = 
		    ((const octave_sparse_complex_matrix &)rep).sparse_type ();

		  if (mattyp.is_unknown ())
		    {
		      mattyp = SparseType (args(0).sparse_complex_matrix_value ());
		      ((octave_sparse_complex_matrix &)rep).sparse_type (mattyp);
		    }
		}
	      else
		{
		  mattyp = ((const octave_sparse_matrix &)rep).sparse_type ();

		  if (mattyp.is_unknown ())
		    {
		      mattyp = SparseType (args(0).sparse_matrix_value ());
		      ((octave_sparse_matrix &)rep).sparse_type (mattyp);
		    }
		}

	      int typ = mattyp.type ();

	      if (typ == SparseType::Diagonal)
		retval = octave_value ("Diagonal");
	      else if (typ == SparseType::Permuted_Diagonal)
		retval = octave_value ("Permuted Diagonal");
	      else if (typ == SparseType::Upper)
		retval = octave_value ("Upper");
	      else if (typ == SparseType::Permuted_Upper)
		retval = octave_value ("Permuted Upper");
	      else if (typ == SparseType::Lower)
		retval = octave_value ("Lower");
	      else if (typ == SparseType::Permuted_Lower)
		retval = octave_value ("Permuted Lower");
	      else if (typ == SparseType::Banded)
		retval = octave_value ("Banded");
	      else if (typ == SparseType::Banded_Hermitian)
		retval = octave_value ("Banded Positive Definite");
	      else if (typ == SparseType::Tridiagonal)
		retval = octave_value ("Tridiagonal");
	      else if (typ == SparseType::Tridiagonal_Hermitian)
		retval = octave_value ("Tridiagonal Positive Definite");
	      else if (typ == SparseType::Hermitian)
		retval = octave_value ("Positive Definite");
	      else if (typ == SparseType::Full)
		retval = octave_value ("Full");
	      else
		// This should never happen!!!
		retval = octave_value ("Unknown");
	    }
	  else
	    {
	      // Ok, we're changing the matrix type
	      std::string str_typ = args(1).string_value ();

	      // XXX FIXME, why do I have to explicitly call the constructor?
	      SparseType mattyp = SparseType ();

	      int nl = 0;
	      int nu = 0;
	      
	      if (error_state)
		error ("Matrix type must be a string");
	      else
		{
		  // Use STL function to convert to lower case
		  std::transform (str_typ.begin (), str_typ.end (),
				  str_typ.begin (), tolower);

		  if (str_typ == "diagonal")
		    mattyp.mark_as_diagonal ();
		  if (str_typ == "permuted diagonal")
		    mattyp.mark_as_permuted_diagonal ();
		  else if (str_typ == "upper")
		    mattyp.mark_as_upper_triangular ();
		  else if (str_typ == "lower")
		    mattyp.mark_as_lower_triangular ();
		  else if (str_typ == "banded" || str_typ == "banded positive definite")
		    {
		      if (nargin != 4)
			error ("matrix_type: banded matrix type requires 4 arguments");
		      else
			{
			  nl = args(2).nint_value ();
			  nu = args(3).nint_value ();

			  if (error_state)
			    error ("matrix_type: band size must be integer");
			  else
			    {
			      if (nl == 1 && nu == 1)
				mattyp.mark_as_tridiagonal ();
			      else
				mattyp.mark_as_banded (nu, nl);
			      
			      if (str_typ == "banded positive definite")
				mattyp.mark_as_symmetric ();
			    }
			}
		    }
		  else if (str_typ == "positive definite")
		    {
		      mattyp.mark_as_full ();
		      mattyp.mark_as_symmetric ();
		    }
		  else if (str_typ == "singular")
		    mattyp.mark_as_rectangular ();
		  else if (str_typ == "full")
		    mattyp.mark_as_full ();
		  else if (str_typ == "unknown")
		    mattyp.invalidate_type ();
		  else
		    error ("matrix_type: Unknown matrix type %s", str_typ.c_str());

		  if (! error_state)
		    {
		      if (nargin == 3 && (str_typ == "upper" || str_typ == "lower"))
			{
			  const ColumnVector perm = 
			    ColumnVector (args (2).vector_value ());

			  if (error_state)
			    error ("matrix_type: Invalid permutation vector");
			  else
			    {
			      int len = perm.length ();
			      dim_vector dv = args(0).dims ();
			      
			      if (len != dv(0))
				error ("matrix_type: Invalid permutation vector");
			      else
				{
				  OCTAVE_LOCAL_BUFFER (octave_idx_type, p, len);

				  for (int i = 0; i < len; i++)
				    p[i] = (int) (perm (i)); 

				  if (str_typ == "upper")
				    mattyp.mark_as_permuted (len, p);
				  else
				    mattyp.mark_as_permuted (len, p);
				}
			    }
			}
		      else if (nargin != 2 && str_typ != "banded positive definite" &&
			       str_typ != "banded")
			error ("matrix_type: Invalid number of arguments");

		      if (! error_state)
			{
			  // Set the matrix type
			  if (args(0).type_name () == "sparse complex matrix" ) 
			    retval = 
			      octave_value (args(0).sparse_complex_matrix_value (), 
					    mattyp);
			  else
			    retval = octave_value (args(0).sparse_matrix_value (), 
						   mattyp);
			}
		    }
		}
	    }
	}
      else
	error ("matrix_type: Only sparse matrices treated at the moment");
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
