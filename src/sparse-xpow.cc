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

#include <cassert>
#include <climits>

#include "Array-util.h"
#include "oct-cmplx.h"
#include "quit.h"

#include "error.h"
#include "oct-obj.h"
#include "utils.h"

#include "dSparse.h"
#include "CSparse.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "sparse-xpow.h"

static inline int
xisint (double x)
{
  return (D_NINT (x) == x
	  && ((x >= 0 && x < INT_MAX)
	      || (x <= 0 && x > INT_MIN)));
}


// Safer pow functions. Only two make sense for sparse matrices, the
// others should all promote to full matrices.

octave_value
xpow (const SparseMatrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be square");
  else
    {
      if (static_cast<int> (b) == b)
	{
	  int btmp = static_cast<int> (b);
	  if (btmp == 0)
	    {
	      SparseMatrix tmp = SparseMatrix (nr, nr, nr);
	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  tmp.data (i) = 1.0;
		  tmp.ridx (i) = i;
		}
	      for (octave_idx_type i = 0; i < nr + 1; i++)
		tmp.cidx (i) = i;

	      retval = tmp;
	    }
	  else
	    {
	      SparseMatrix atmp;
	      if (btmp < 0)
		{
		  btmp = -btmp;

		  octave_idx_type info;
		  double rcond = 0.0;
		  SparseType mattyp (a);

		  atmp = a.inverse (mattyp, info, rcond, 1);

		  if (info == -1)
		    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
		}
	      else
		atmp = a;

	      SparseMatrix result (atmp);

	      btmp--;

	      while (btmp > 0)
		{
		  if (btmp & 1)
		    result = result * atmp;

		  btmp >>= 1;

		  if (btmp > 0)
		    atmp = atmp * atmp;
		}

	      retval = result;
	    }
	}
      else
	error ("use full(a) ^ full(b)");
    }

  return retval;
}

octave_value
xpow (const SparseComplexMatrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be square");
  else
    {
      if (static_cast<int> (b) == b)
	{
	  int btmp = static_cast<int> (b);
	  if (btmp == 0)
	    {
	      SparseMatrix tmp = SparseMatrix (nr, nr, nr);
	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  tmp.data (i) = 1.0;
		  tmp.ridx (i) = i;
		}
	      for (octave_idx_type i = 0; i < nr + 1; i++)
		tmp.cidx (i) = i;

	      retval = tmp;
	    }
	  else
	    {
	      SparseComplexMatrix atmp;
	      if (btmp < 0)
		{
		  btmp = -btmp;

		  octave_idx_type info;
		  double rcond = 0.0;
		  SparseType mattyp (a);

		  atmp = a.inverse (mattyp, info, rcond, 1);

		  if (info == -1)
		    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
		}
	      else
		atmp = a;

	      SparseComplexMatrix result (atmp);

	      btmp--;

	      while (btmp > 0)
		{
		  if (btmp & 1)
		    result = result * atmp;

		  btmp >>= 1;

		  if (btmp > 0)
		    atmp = atmp * atmp;
		}

	      retval = result;
	    }
	}
      else
	error ("use full(a) ^ full(b)");
    }

  return retval;
}

// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// XXX FIXME XXX -- these functions need to be fixed so that things
// like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (double a, const SparseMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  double d1, d2;

  if (a < 0.0 && ! b.all_integers (d1, d2))
    {
      Complex atmp (a);
      ComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      OCTAVE_QUIT;
	      result (i, j) = pow (atmp, b(i,j));
	    }
	}

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      OCTAVE_QUIT;
	      result (i, j) = pow (a, b(i,j));
	    }
	}

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  Complex atmp (a);
  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;
	  result (i, j) = pow (atmp, b(i,j));
	}
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const SparseMatrix& a, double b)
{
  // XXX FIXME XXX What should a .^ 0 give?? Matlab gives a 
  // sparse matrix with same structure as a, which is strictly
  // incorrect. Keep compatiability.

  octave_value retval;

  octave_idx_type nz = a.nnz ();

  if (b <= 0.0)
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.cols ();

      if (static_cast<int> (b) != b && a.any_element_is_negative ())
	{
	  ComplexMatrix result (nr, nc, Complex (pow (0.0, b)));

	  // XXX FIXME XXX -- avoid apparent GNU libm bug by
	  // converting A and B to complex instead of just A.
	  Complex btmp (b);

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
	      {
		OCTAVE_QUIT;
	      
		Complex atmp (a.data (i));
		
		result (a.ridx(i), j) = pow (atmp, btmp);
	      }

	  retval = octave_value (result);
	}
      else
	{
	  Matrix result (nr, nc, (pow (0.0, b)));

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
	      {
		OCTAVE_QUIT;
		result (a.ridx(i), j) = pow (a.data (i), b);
	      }

	  retval = octave_value (result);
	}
    }
  else if (static_cast<int> (b) != b && a.any_element_is_negative ())
    {
      SparseComplexMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
	{
	  OCTAVE_QUIT;

	  // XXX FIXME XXX -- avoid apparent GNU libm bug by
	  // converting A and B to complex instead of just A.

	  Complex atmp (a.data (i));
	  Complex btmp (b);

	  result.data (i) = pow (atmp, btmp);
	}

      result.maybe_compress (true);

      retval = result;
    }
  else
    {
      SparseMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
	{
	  OCTAVE_QUIT;
	  result.data (i) = pow (a.data (i), b);
	}

      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const SparseMatrix& a, const SparseMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  int convert_to_complex = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	double atmp = a (i, j);
	double btmp = b (i, j);
	if (atmp < 0.0 && static_cast<int> (btmp) != btmp)
	  {
	    convert_to_complex = 1;
	    goto done;
	  }
      }

done:

  octave_idx_type nel = 0;
  for (octave_idx_type j = 0; j < nc; j++) 
    for (octave_idx_type i = 0; i < nr; i++)
      if (!(a.elem (i, j) == 0. && b.elem (i, j) != 0.))
	nel++;

  if (convert_to_complex)
    {
      SparseComplexMatrix complex_result (nr, nc, nel);

      octave_idx_type ii = 0;
      complex_result.cidx(0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      OCTAVE_QUIT;
	      Complex atmp (a (i, j));
	      Complex btmp (b (i, j));
	      Complex tmp =  pow (atmp, btmp);
	      if (tmp != 0.)
		{
		  complex_result.data (ii) = tmp;
		  complex_result.ridx (ii++) = i;
		}
	    }
	  complex_result.cidx (j+1) = ii;
	}
      complex_result.maybe_compress ();

      retval = complex_result;
    }
  else
    {
      SparseMatrix result (nr, nc, nel);
      octave_idx_type ii = 0;

      result.cidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      OCTAVE_QUIT;
	      double tmp = pow (a (i, j), b (i, j));
	      if (tmp != 0.)
		{
		  result.data (ii) = tmp;
		  result.ridx (ii++) = i;
		}
	    }
	  result.cidx (j+1) = ii;
	}

      result.maybe_compress ();

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const SparseMatrix& a, const Complex& b)
{
  octave_value retval;

  if (b == 0.0)
    // Can this case ever happen, due to automatic retyping with maybe_mutate?
    retval = octave_value (NDArray (a.dims (), 1));
  else
    {
      octave_idx_type nz = a.nnz ();
      SparseComplexMatrix result (a);
      
      for (octave_idx_type i = 0; i < nz; i++)
	{
	  OCTAVE_QUIT;
	  result.data (i) = pow (Complex (a.data (i)), b);
	}
  
      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 6 -*-
octave_value
elem_xpow (const SparseMatrix& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  octave_idx_type nel = 0;
  for (octave_idx_type j = 0; j < nc; j++) 
    for (octave_idx_type i = 0; i < nr; i++)
      if (!(a.elem (i, j) == 0. && b.elem (i, j) != 0.))
	nel++;

  SparseComplexMatrix result (nr, nc, nel);
  octave_idx_type ii = 0;

  result.cidx(0) = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;
	  Complex tmp = pow (Complex (a (i, j)), b (i, j));
	  if (tmp != 0.)
	    {
	      result.data (ii) = tmp; 
	      result.ridx (ii++) = i; 
	    }
	}
      result.cidx (j+1) = ii;
    }

  result.maybe_compress ();

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;
	  double btmp = b (i, j);
	  if (xisint (btmp))
	    result (i, j) = pow (a, static_cast<int> (btmp));
	  else
	    result (i, j) = pow (a, btmp);
	}
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = pow (a, b (i, j));
      }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, double b)
{
  octave_value retval;

  if (b <= 0)
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.cols ();

      ComplexMatrix result (nr, nc, Complex (pow (0.0, b)));

      if (xisint (b))
	{
	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
	      {
		OCTAVE_QUIT;
		result (a.ridx(i), j) = 
		  pow (a.data (i), static_cast<int> (b));
	      }
	}
      else
	{
	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
	      {
		OCTAVE_QUIT;
		result (a.ridx(i), j) = pow (a.data (i), b);
	      }
	}  

      retval = result;
    }
  else
    {
      octave_idx_type nz = a.nnz ();

      SparseComplexMatrix result (a);
  
      if (xisint (b))
	{
	  for (octave_idx_type i = 0; i < nz; i++)
	    {
	      OCTAVE_QUIT;
	      result.data (i) = pow (a.data (i), static_cast<int> (b));
	    }
	}
      else
	{
	  for (octave_idx_type i = 0; i < nz; i++)
	    {
	      OCTAVE_QUIT;
	      result.data (i) = pow (a.data (i), b);
	    }
	}  

      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 10 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  octave_idx_type nel = 0;
  for (octave_idx_type j = 0; j < nc; j++) 
    for (octave_idx_type i = 0; i < nr; i++)
      if (!(a.elem (i, j) == 0. && b.elem (i, j) != 0.))
	nel++;

  SparseComplexMatrix result (nr, nc, nel);
  octave_idx_type ii = 0;

  result.cidx (0) = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;
	  double btmp = b (i, j);
	  Complex tmp;

	  if (xisint (btmp))
	    tmp = pow (a (i, j), static_cast<int> (btmp));
	  else
	    tmp = pow (a (i, j), btmp);
	  if (tmp != 0.)
	    {
	      result.data (ii) = tmp; 
	      result.ridx (ii++) = i; 
	    }
	}
      result.cidx (j+1) = ii;
    }

  result.maybe_compress ();

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const Complex& b)
{
  octave_value retval;

  if (b == 0.0)
    // Can this case ever happen, due to automatic retyping with maybe_mutate?
    retval = octave_value (NDArray (a.dims (), 1));
  else
    {

      octave_idx_type nz = a.nnz ();

      SparseComplexMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
	{
	  OCTAVE_QUIT;
	  result.data (i) = pow (a.data (i), b);
	}

      result.maybe_compress (true);
      
      retval = result;
    }

  return retval;
}

// -*- 12 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  octave_idx_type nel = 0;
  for (octave_idx_type j = 0; j < nc; j++) 
    for (octave_idx_type i = 0; i < nr; i++)
      if (!(a.elem (i, j) == 0. && b.elem (i, j) != 0.))
	nel++;

  SparseComplexMatrix result (nr, nc, nel);
  octave_idx_type ii = 0;

  result.cidx (0) = 0;
  for (octave_idx_type j = 0; j < nc; j++) 
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;
	  Complex tmp = pow (a (i, j), b (i, j));
	  if (tmp != 0.)
	    {
	      result.data (ii) = tmp;
	      result.ridx (ii++) = i;
	    }
	}
      result.cidx (j+1) = ii;
    }
  result.maybe_compress (true);

  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
