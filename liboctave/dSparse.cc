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

#include <cfloat>

#include <iostream>
#include <vector>

#include "quit.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "f77-fcn.h"
#include "dRowVector.h"

#include "CSparse.h"
#include "boolSparse.h"
#include "dSparse.h"
#include "oct-spparms.h"
#include "SparsedbleLU.h"
#include "SparseType.h"
#include "oct-sparse.h"
#include "sparse-util.h"
#include "SparsedbleCHOL.h"
#include "SparseQR.h"

#include "oct-sort.h"

// Define whether to use a basic QR solver or one that uses a Dulmange
// Mendelsohn factorization to seperate the problem into under-determined,
// well-determined and over-determined parts and solves them seperately
#ifndef USE_QRSOLVE
#include "sparse-dmsolve.cc"
#endif

// Fortran functions we call.
extern "C"
{
  F77_RET_T
  F77_FUNC (dgbtrf, DGBTRF) (const octave_idx_type&, const int&, const octave_idx_type&, 
			     const octave_idx_type&, double*, const octave_idx_type&, octave_idx_type*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgbtrs, DGBTRS) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
			     const double*, const octave_idx_type&,
			     const octave_idx_type*, double*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgbcon, DGBCON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const octave_idx_type&, const octave_idx_type&, double*, 
			     const octave_idx_type&, const octave_idx_type*, const double&, 
			     double&, double*, octave_idx_type*, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpbtrf, DPBTRF) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const octave_idx_type&, double*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpbtrs, DPBTRS) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const octave_idx_type&, const octave_idx_type&, double*, const octave_idx_type&, 
			     double*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpbcon, DPBCON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const octave_idx_type&, double*, const octave_idx_type&, 
			     const double&, double&, double*, octave_idx_type*, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (dptsv, DPTSV) (const octave_idx_type&, const octave_idx_type&, double*, double*,
			   double*, const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgtsv, DGTSV) (const octave_idx_type&, const octave_idx_type&, double*, double*,
			   double*, double*, const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgttrf, DGTTRF) (const octave_idx_type&, double*, double*, double*, double*,
			     octave_idx_type*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgttrs, DGTTRS) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     const octave_idx_type&, const double*, const double*,
			     const double*, const double*, const octave_idx_type*,
			     double *, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zptsv, ZPTSV) (const octave_idx_type&, const octave_idx_type&, double*, Complex*,
			   Complex*, const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (zgtsv, ZGTSV) (const octave_idx_type&, const octave_idx_type&, Complex*, Complex*,
			   Complex*, Complex*, const octave_idx_type&, octave_idx_type&);

}

SparseMatrix::SparseMatrix (const SparseBoolMatrix &a)
  : MSparse<double> (a.rows (), a.cols (), a.nnz ())
{
  octave_idx_type nc = cols ();
  octave_idx_type nz = a.nnz ();

  for (octave_idx_type i = 0; i < nc + 1; i++)
    cidx (i) = a.cidx (i);

  for (octave_idx_type i = 0; i < nz; i++)
    {
      data (i) = a.data (i);
      ridx (i) = a.ridx (i);
    }
}

bool
SparseMatrix::operator == (const SparseMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz = nnz ();
  octave_idx_type nr_a = a.rows ();
  octave_idx_type nc_a = a.cols ();
  octave_idx_type nz_a = a.nnz ();

  if (nr != nr_a || nc != nc_a || nz != nz_a)
    return false;

  for (octave_idx_type i = 0; i < nc + 1; i++)
    if (cidx(i) != a.cidx(i))
	return false;

  for (octave_idx_type i = 0; i < nz; i++)
    if (data(i) != a.data(i) || ridx(i) != a.ridx(i))
      return false;

  return true;
}

bool
SparseMatrix::operator != (const SparseMatrix& a) const
{
  return !(*this == a);
}

bool
SparseMatrix::is_symmetric (void) const
{
  if (is_square () && rows () > 0)
    {
      for (octave_idx_type i = 0; i < rows (); i++)
	for (octave_idx_type j = i+1; j < cols (); j++)
	  if (elem (i, j) != elem (j, i))
	    return false;

      return true;
    }

  return false;
}

SparseMatrix&
SparseMatrix::insert (const SparseMatrix& a, octave_idx_type r, octave_idx_type c)
{
  MSparse<double>::insert (a, r, c);
  return *this;
}

SparseMatrix
SparseMatrix::max (int dim) const
{
  Array2<octave_idx_type> dummy_idx;
  return max (dummy_idx, dim);
}

SparseMatrix
SparseMatrix::max (Array2<octave_idx_type>& idx_arg, int dim) const
{
  SparseMatrix result;
  dim_vector dv = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return result;
 
  octave_idx_type nr = dv(0);
  octave_idx_type nc = dv(1);

  if (dim == 0)
    {
      idx_arg.resize (1, nc);
      octave_idx_type nel = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  double tmp_max = octave_NaN;
	  octave_idx_type idx_j = 0;
	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      if (ridx(i) != idx_j)
		break;
	      else
		idx_j++;
	    }

	  if (idx_j != nr)
	    tmp_max = 0.;

	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      double tmp = data (i);

	      if (xisnan (tmp))
		continue;
	      else if (xisnan (tmp_max) || tmp > tmp_max)
		{
		  idx_j = ridx (i);
		  tmp_max = tmp;
		}

	    }

 	  idx_arg.elem (j) = xisnan (tmp_max) ? 0 : idx_j;
	  if (tmp_max != 0.)
	    nel++;
	}

      result = SparseMatrix (1, nc, nel);

      octave_idx_type ii = 0;
      result.xcidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  double tmp = elem (idx_arg(j), j);
	  if (tmp != 0.)
	    {
	      result.xdata (ii) = tmp;
	      result.xridx (ii++) = 0;
	    }
	  result.xcidx (j+1) = ii;

	}
    }
  else
    {
      idx_arg.resize (nr, 1, 0);

      for (octave_idx_type i = cidx(0); i < cidx(1); i++)
	idx_arg.elem(ridx(i)) = -1;

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  {
	    if (idx_arg.elem(i) != -1)
	      continue;
	    bool found = false;
	    for (octave_idx_type k = cidx(j); k < cidx(j+1); k++)
	      if (ridx(k) == i)
		{
		  found = true;
		  break;
		}
	    
	    if (!found)
	      idx_arg.elem(i) = j;

	  }

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      octave_idx_type ir = ridx (i);
	      octave_idx_type ix = idx_arg.elem (ir);
	      double tmp = data (i);

	      if (xisnan (tmp))
		continue;
	      else if (ix == -1 || tmp > elem (ir, ix))
		idx_arg.elem (ir) = j;
	    }
	}

      octave_idx_type nel = 0;
      for (octave_idx_type j = 0; j < nr; j++)
	if (idx_arg.elem(j) == -1 || elem (j, idx_arg.elem (j)) != 0.)
	  nel++;

      result = SparseMatrix (nr, 1, nel);

      octave_idx_type ii = 0;
      result.xcidx (0) = 0;
      result.xcidx (1) = nel;
      for (octave_idx_type j = 0; j < nr; j++)
	{
	  if (idx_arg(j) == -1)
	    {
	      idx_arg(j) = 0;
	      result.xdata (ii) = octave_NaN;
	      result.xridx (ii++) = j;
	    }
	  else
	    {
	      double tmp = elem (j, idx_arg(j));
	      if (tmp != 0.)
		{
		  result.xdata (ii) = tmp;
		  result.xridx (ii++) = j;
		}
	    }
	}
    }

  return result;
}

SparseMatrix
SparseMatrix::min (int dim) const
{
  Array2<octave_idx_type> dummy_idx;
  return min (dummy_idx, dim);
}

SparseMatrix
SparseMatrix::min (Array2<octave_idx_type>& idx_arg, int dim) const
{
  SparseMatrix result;
  dim_vector dv = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return result;
 
  octave_idx_type nr = dv(0);
  octave_idx_type nc = dv(1);

  if (dim == 0)
    {
      idx_arg.resize (1, nc);
      octave_idx_type nel = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  double tmp_min = octave_NaN;
	  octave_idx_type idx_j = 0;
	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      if (ridx(i) != idx_j)
		break;
	      else
		idx_j++;
	    }

	  if (idx_j != nr)
	    tmp_min = 0.;

	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      double tmp = data (i);

	      if (xisnan (tmp))
		continue;
	      else if (xisnan (tmp_min) || tmp < tmp_min)
		{
		  idx_j = ridx (i);
		  tmp_min = tmp;
		}

	    }

 	  idx_arg.elem (j) = xisnan (tmp_min) ? 0 : idx_j;
	  if (tmp_min != 0.)
	    nel++;
	}

      result = SparseMatrix (1, nc, nel);

      octave_idx_type ii = 0;
      result.xcidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  double tmp = elem (idx_arg(j), j);
	  if (tmp != 0.)
	    {
	      result.xdata (ii) = tmp;
	      result.xridx (ii++) = 0;
	    }
	  result.xcidx (j+1) = ii;

	}
    }
  else
    {
      idx_arg.resize (nr, 1, 0);

      for (octave_idx_type i = cidx(0); i < cidx(1); i++)
	idx_arg.elem(ridx(i)) = -1;

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  {
	    if (idx_arg.elem(i) != -1)
	      continue;
	    bool found = false;
	    for (octave_idx_type k = cidx(j); k < cidx(j+1); k++)
	      if (ridx(k) == i)
		{
		  found = true;
		  break;
		}
	    
	    if (!found)
	      idx_arg.elem(i) = j;

	  }

      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      octave_idx_type ir = ridx (i);
	      octave_idx_type ix = idx_arg.elem (ir);
	      double tmp = data (i);

	      if (xisnan (tmp))
		continue;
	      else if (ix == -1 || tmp < elem (ir, ix))
		idx_arg.elem (ir) = j;
	    }
	}

      octave_idx_type nel = 0;
      for (octave_idx_type j = 0; j < nr; j++)
	if (idx_arg.elem(j) == -1 || elem (j, idx_arg.elem (j)) != 0.)
	  nel++;

      result = SparseMatrix (nr, 1, nel);

      octave_idx_type ii = 0;
      result.xcidx (0) = 0;
      result.xcidx (1) = nel;
      for (octave_idx_type j = 0; j < nr; j++)
	{
	  if (idx_arg(j) == -1)
	    {
	      idx_arg(j) = 0;
	      result.xdata (ii) = octave_NaN;
	      result.xridx (ii++) = j;
	    }
	  else
	    {
	      double tmp = elem (j, idx_arg(j));
	      if (tmp != 0.)
		{
		  result.xdata (ii) = tmp;
		  result.xridx (ii++) = j;
		}
	    }
	}
    }

  return result;
}

SparseMatrix
SparseMatrix::concat (const SparseMatrix& rb, const Array<octave_idx_type>& ra_idx)
{
  // Don't use numel to avoid all possiblity of an overflow
  if (rb.rows () > 0 && rb.cols () > 0)
    insert (rb, ra_idx(0), ra_idx(1));
  return *this;
}

SparseComplexMatrix
SparseMatrix::concat (const SparseComplexMatrix& rb, const Array<octave_idx_type>& ra_idx)
{
  SparseComplexMatrix retval (*this);
  if (rb.rows () > 0 && rb.cols () > 0)
    retval.insert (rb, ra_idx(0), ra_idx(1));
  return retval;
}

SparseMatrix
real (const SparseComplexMatrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nnz ();
  SparseMatrix r (nr, nc, nz);

  for (octave_idx_type i = 0; i < nc +1; i++)
    r.cidx(i) = a.cidx(i);

  for (octave_idx_type i = 0; i < nz; i++)
    {
      r.data(i) = std::real (a.data(i));
      r.ridx(i) = a.ridx(i);
    }

  return r;
}

SparseMatrix
imag (const SparseComplexMatrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nnz ();
  SparseMatrix r (nr, nc, nz);

  for (octave_idx_type i = 0; i < nc +1; i++)
    r.cidx(i) = a.cidx(i);

  for (octave_idx_type i = 0; i < nz; i++)
    {
      r.data(i) = std::imag (a.data(i));
      r.ridx(i) = a.ridx(i);
    }

  return r;
}

SparseMatrix 
atan2 (const double& x, const SparseMatrix& y)
{
  octave_idx_type nr = y.rows ();
  octave_idx_type nc = y.cols ();

  if (x == 0.)
    return SparseMatrix (nr, nc);
  else
    {
      // Its going to be basically full, so this is probably the
      // best way to handle it.
      Matrix tmp (nr, nc, atan2 (x, 0.));

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = y.cidx (j); i < y.cidx (j+1); i++)
	  tmp.elem (y.ridx(i), j) = atan2 (x, y.data(i));

      return SparseMatrix (tmp);
    }
}

SparseMatrix 
atan2 (const SparseMatrix& x, const double& y)
{
  octave_idx_type nr = x.rows ();
  octave_idx_type nc = x.cols ();
  octave_idx_type nz = x.nnz ();

  SparseMatrix retval (nr, nc, nz);

  octave_idx_type ii = 0;
  retval.xcidx(0) = 0;
  for (octave_idx_type i = 0; i < nc; i++)
    {
      for (octave_idx_type j = x.cidx(i); j < x.cidx(i+1); j++)
	{
	  double tmp = atan2 (x.data(j), y);
	  if (tmp != 0.)
	    {
	      retval.xdata (ii) = tmp;
	      retval.xridx (ii++) = x.ridx (j);
	    }
	}
      retval.xcidx (i+1) = ii;
    }

  if (ii != nz)
    {
      SparseMatrix retval2 (nr, nc, ii);
      for (octave_idx_type i = 0; i < nc+1; i++)
	retval2.xcidx (i) = retval.cidx (i);
      for (octave_idx_type i = 0; i < ii; i++)
	{
	  retval2.xdata (i) = retval.data (i);
	  retval2.xridx (i) = retval.ridx (i);
	}
      return retval2;
    }
  else
    return retval;
}

SparseMatrix 
atan2 (const SparseMatrix& x, const SparseMatrix& y)
{
  SparseMatrix r;

  if ((x.rows() == y.rows()) && (x.cols() == y.cols())) 
    {
      octave_idx_type x_nr = x.rows ();
      octave_idx_type x_nc = x.cols ();

      octave_idx_type y_nr = y.rows ();
      octave_idx_type y_nc = y.cols ();

      if (x_nr != y_nr || x_nc != y_nc)
	gripe_nonconformant ("atan2", x_nr, x_nc, y_nr, y_nc);
      else
	{
	  r = SparseMatrix (x_nr, x_nc, (x.nnz () + y.nnz ()));
       
	  octave_idx_type jx = 0;
	  r.cidx (0) = 0;
	  for (octave_idx_type i = 0 ; i < x_nc ; i++)
	    {
	      octave_idx_type  ja = x.cidx(i);
	      octave_idx_type  ja_max = x.cidx(i+1);
	      bool ja_lt_max= ja < ja_max;
           
	      octave_idx_type  jb = y.cidx(i);
	      octave_idx_type  jb_max = y.cidx(i+1);
	      bool jb_lt_max = jb < jb_max;
           
	      while (ja_lt_max || jb_lt_max )
		{
		  OCTAVE_QUIT;
		  if ((! jb_lt_max) ||
                      (ja_lt_max && (x.ridx(ja) < y.ridx(jb))))
		    {
		      r.ridx(jx) = x.ridx(ja);
		      r.data(jx) = atan2 (x.data(ja), 0.);
		      jx++;
		      ja++;
		      ja_lt_max= ja < ja_max;
		    }
		  else if (( !ja_lt_max ) ||
			   (jb_lt_max && (y.ridx(jb) < x.ridx(ja)) ) )
		    {
		      jb++;
		      jb_lt_max= jb < jb_max;
		    }
		  else
		    {
		      double tmp = atan2 (x.data(ja), y.data(jb));
		      if (tmp != 0.)
			{
                          r.data(jx) = tmp;
                          r.ridx(jx) = x.ridx(ja);
                          jx++;
			}
		      ja++;
		      ja_lt_max= ja < ja_max;
		      jb++;
		      jb_lt_max= jb < jb_max;
		    }
		}
	      r.cidx(i+1) = jx;
	    }
	  
	  r.maybe_compress ();
	}
    }
  else
    (*current_liboctave_error_handler) ("matrix size mismatch");

  return r;
}

SparseMatrix
SparseMatrix::inverse (void) const
{
  octave_idx_type info;
  double rcond;
  SparseType mattype (*this);
  return inverse (mattype, info, rcond, 0, 0);
}

SparseMatrix
SparseMatrix::inverse (SparseType& mattype) const
{
  octave_idx_type info;
  double rcond;
  return inverse (mattype, info, rcond, 0, 0);
}

SparseMatrix
SparseMatrix::inverse (SparseType& mattype, octave_idx_type& info) const
{
  double rcond;
  return inverse (mattype, info, rcond, 0, 0);
}

SparseMatrix 
SparseMatrix::dinverse (SparseType &mattyp, octave_idx_type& info, 
			double& rcond, const bool, 
			const bool calccond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  info = 0;

  if (nr == 0 || nc == 0 || nr != nc)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattyp.type ();
      mattyp.info ();

      if (typ == SparseType::Diagonal ||
	  typ == SparseType::Permuted_Diagonal)
	{
	  if (typ == SparseType::Permuted_Diagonal)
	    retval = transpose();
	  else
	    retval = *this;
	      
	  // Force make_unique to be called
	  double *v = retval.data();

	  if (calccond)
	    {
	      double dmax = 0., dmin = octave_Inf; 
	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  double tmp = fabs(v[i]);
		  if (tmp > dmax)
		    dmax = tmp;
		  if (tmp < dmin)
		    dmin = tmp;
		}
	      rcond = dmin / dmax;
	    }

	  for (octave_idx_type i = 0; i < nr; i++)
	    v[i] = 1.0 / v[i];
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix 
SparseMatrix::tinverse (SparseType &mattyp, octave_idx_type& info, 
			double& rcond, const bool, 
			const bool calccond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  info = 0;

  if (nr == 0 || nc == 0 || nr != nc)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattyp.type ();
      mattyp.info ();

      if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper || 
	  typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;

	  if (calccond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nr; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  if (typ == SparseType::Upper || typ == SparseType::Lower)
	    {
	      octave_idx_type nz = nnz ();
	      octave_idx_type cx = 0;
	      octave_idx_type nz2 = nz;
	      retval = SparseMatrix (nr, nc, nz2);

	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  OCTAVE_QUIT;
		  // place the 1 in the identity position
		  octave_idx_type cx_colstart = cx;
	  
		  if (cx == nz2)
		    {
		      nz2 *= 2;
		      retval.change_capacity (nz2);
		    }

		  retval.xcidx(i) = cx;
		  retval.xridx(cx) = i;
		  retval.xdata(cx) = 1.0;
		  cx++;

		  // iterate accross columns of input matrix
		  for (octave_idx_type j = i+1; j < nr; j++) 
		    {
		      double v = 0.;
		      // iterate to calculate sum
		      octave_idx_type colXp = retval.xcidx(i);
		      octave_idx_type colUp = cidx(j);
		      octave_idx_type rpX, rpU;
		      do
			{
			  OCTAVE_QUIT;
			  rpX = retval.xridx(colXp);
			  rpU = ridx(colUp);

			  if (rpX < rpU) 
			    colXp++;
			  else if (rpX > rpU) 
			    colUp++;
			  else 
			    {
			      v -= retval.xdata(colXp) * data(colUp);
			      colXp++;
			      colUp++;
			    }
			} while ((rpX<j) && (rpU<j) && 
				 (colXp<cx) && (colUp<nz));

		      // get A(m,m)
		      colUp = cidx(j+1) - 1;
		      double pivot = data(colUp);
		      if (pivot == 0.) 
			(*current_liboctave_error_handler) 
			  ("division by zero");

		      if (v != 0.)
			{
			  if (cx == nz2)
			    {
			      nz2 *= 2;
			      retval.change_capacity (nz2);
			    }

			  retval.xridx(cx) = j;
			  retval.xdata(cx) = v / pivot;
			  cx++;
			}
		    }

		  // get A(m,m)
		  octave_idx_type colUp = cidx(i+1) - 1;
		  double pivot = data(colUp);
		  if (pivot == 0.) 
		    (*current_liboctave_error_handler) ("division by zero");

		  if (pivot != 1.0)
		    for (octave_idx_type j = cx_colstart; j < cx; j++)
		      retval.xdata(j) /= pivot;
		}
	      retval.xcidx(nr) = cx;
	      retval.maybe_compress ();
	    }
	  else
	    {
	      octave_idx_type nz = nnz ();
	      octave_idx_type cx = 0;
	      octave_idx_type nz2 = nz;
	      retval = SparseMatrix (nr, nc, nz2);

	      OCTAVE_LOCAL_BUFFER (double, work, nr);
	      OCTAVE_LOCAL_BUFFER (octave_idx_type, rperm, nr);

	      octave_idx_type *perm = mattyp.triangular_perm();
	      if (typ == SparseType::Permuted_Upper)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    rperm[perm[i]] = i;
		}
	      else
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    rperm[i] = perm[i];
		  for (octave_idx_type i = 0; i < nr; i++)
		    perm[rperm[i]] = i;
		}

	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  OCTAVE_QUIT;
		  octave_idx_type iidx = rperm[i];

		  for (octave_idx_type j = 0; j < nr; j++)
		    work[j] = 0.;

		  // place the 1 in the identity position
		  work[iidx] = 1.0;

		  // iterate accross columns of input matrix
		  for (octave_idx_type j = iidx+1; j < nr; j++) 
		    {
		      double v = 0.;
		      octave_idx_type jidx = perm[j];
		      // iterate to calculate sum
		      for (octave_idx_type k = cidx(jidx); 
			   k < cidx(jidx+1); k++)
			{
			  OCTAVE_QUIT;
			  v -= work[ridx(k)] * data(k);
			}

		      // get A(m,m)
		      double pivot = data(cidx(jidx+1) - 1);
		      if (pivot == 0.) 
			(*current_liboctave_error_handler) 
			  ("division by zero");

		      work[j] = v / pivot;
		    }

		  // get A(m,m)
		  octave_idx_type colUp = cidx(perm[iidx]+1) - 1;
		  double pivot = data(colUp);
		  if (pivot == 0.) 
		    (*current_liboctave_error_handler) 
		      ("division by zero");

		  octave_idx_type new_cx = cx;
		  for (octave_idx_type j = iidx; j < nr; j++)
		    if (work[j] != 0.0)
		      {
			new_cx++;
			if (pivot != 1.0)
			  work[j] /= pivot;
		      }

		  if (cx < new_cx)
		    {
		      nz2 = (2*nz2 < new_cx ? new_cx : 2*nz2);
		      retval.change_capacity (nz2);
		    }

		  retval.xcidx(i) = cx;
		  for (octave_idx_type j = iidx; j < nr; j++)
		    if (work[j] != 0.)
		      {
			retval.xridx(cx) = j;
			retval.xdata(cx++) = work[j];
		      }
		}

	      retval.xcidx(nr) = cx;
	      retval.maybe_compress ();
	    }

	  if (calccond)
	    {
	      // Calculate the 1-norm of inverse matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nr; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = retval.cidx(j); 
		       i < retval.cidx(j+1); i++)
		    atmp += fabs(retval.data(i));
		  if (atmp > ainvnorm)
		    ainvnorm = atmp;
		}

	      rcond = 1. / ainvnorm / anorm;     
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix
SparseMatrix::inverse (SparseType &mattype, octave_idx_type& info, 
		       double& rcond, int, int calc_cond) const
{
  int typ = mattype.type (false);
  SparseMatrix ret;

  if (typ == SparseType::Unknown)
    typ = mattype.type (*this);

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    ret = dinverse (mattype, info, rcond, true, calc_cond);
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    ret = tinverse (mattype, info, rcond, true, calc_cond).transpose();
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    ret = transpose().tinverse (mattype, info, rcond, true, calc_cond);
  else if (typ != SparseType::Rectangular)
    {
      if (mattype.is_hermitian())
	{
	  SparseType tmp_typ (SparseType::Upper);
	  SparseCHOL fact (*this, info, false);
	  rcond = fact.rcond();
	  if (info == 0)
	    {
	      double rcond2;
	      SparseMatrix Q = fact.Q();
	      SparseMatrix InvL = fact.L().transpose().tinverse(tmp_typ,
					   info, rcond2, true, false);
	      ret = Q * InvL.transpose() * InvL * Q.transpose();
	    }
	  else
	    {
	      // Matrix is either singular or not positive definite
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Full;
	    }
	}

      if (!mattype.is_hermitian())
	{
	  octave_idx_type n = rows();
	  ColumnVector Qinit(n);
	  for (octave_idx_type i = 0; i < n; i++)
	    Qinit(i) = i;

	  SparseType tmp_typ (SparseType::Upper);
	  SparseLU fact (*this, Qinit, -1.0, false);
	  rcond = fact.rcond();
	  double rcond2;
	  SparseMatrix InvL = fact.L().transpose().tinverse(tmp_typ, 
					   info, rcond2, true, false);
	  SparseMatrix InvU = fact.U().tinverse(tmp_typ, info, rcond2,
					   true, false).transpose();
	  ret = fact.Pc().transpose() * InvU * InvL * fact.Pr();
	}
    }
  else
    (*current_liboctave_error_handler) ("inverse requires square matrix");

  return ret;
}

DET
SparseMatrix::determinant (void) const
{
  octave_idx_type info;
  double rcond;
  return determinant (info, rcond, 0);
}

DET
SparseMatrix::determinant (octave_idx_type& info) const
{
  double rcond;
  return determinant (info, rcond, 0);
}

DET
SparseMatrix::determinant (octave_idx_type& err, double& rcond, int) const
{
  DET retval;

#ifdef HAVE_UMFPACK
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    {
      double d[2];
      d[0] = 1.0;
      d[1] = 0.0;
      retval = DET (d);
    }
  else
    {
      err = 0;

      // Setup the control parameters
      Matrix Control (UMFPACK_CONTROL, 1);
      double *control = Control.fortran_vec ();
      UMFPACK_DNAME (defaults) (control);

      double tmp = Voctave_sparse_controls.get_key ("spumoni");
      if (!xisnan (tmp))
	Control (UMFPACK_PRL) = tmp;

      tmp = Voctave_sparse_controls.get_key ("piv_tol");
      if (!xisnan (tmp))
	{
	  Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
	  Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
	}

      // Set whether we are allowed to modify Q or not
      tmp = Voctave_sparse_controls.get_key ("autoamd");
      if (!xisnan (tmp))
	Control (UMFPACK_FIXQ) = tmp;

      // Turn-off UMFPACK scaling for LU 
      Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

      UMFPACK_DNAME (report_control) (control);

      const octave_idx_type *Ap = cidx ();
      const octave_idx_type *Ai = ridx ();
      const double *Ax = data ();

      UMFPACK_DNAME (report_matrix) (nr, nc, Ap, Ai, Ax, 1, control);

      void *Symbolic;
      Matrix Info (1, UMFPACK_INFO);
      double *info = Info.fortran_vec ();
      int status = UMFPACK_DNAME (qsymbolic) (nr, nc, Ap, Ai, 
					 Ax, NULL, &Symbolic, control, info);

      if (status < 0)
	{
	  (*current_liboctave_error_handler) 
	    ("SparseMatrix::determinant symbolic factorization failed");

	  UMFPACK_DNAME (report_status) (control, status);
	  UMFPACK_DNAME (report_info) (control, info);

	  UMFPACK_DNAME (free_symbolic) (&Symbolic) ;
	}
      else
	{
	  UMFPACK_DNAME (report_symbolic) (Symbolic, control);

	  void *Numeric;
	  status = UMFPACK_DNAME (numeric) (Ap, Ai, Ax, Symbolic, 
				       &Numeric, control, info) ;
	  UMFPACK_DNAME (free_symbolic) (&Symbolic) ;

	  rcond = Info (UMFPACK_RCOND);

	  if (status < 0)
	    {
	      (*current_liboctave_error_handler) 
		("SparseMatrix::determinant numeric factorization failed");

	      UMFPACK_DNAME (report_status) (control, status);
	      UMFPACK_DNAME (report_info) (control, info);

	      UMFPACK_DNAME (free_numeric) (&Numeric);
	    }
	  else
	    {
	      UMFPACK_DNAME (report_numeric) (Numeric, control);

	      double d[2];

	      status = UMFPACK_DNAME (get_determinant) (&d[0],
						   &d[1], Numeric, info);

	      if (status < 0)
		{
		  (*current_liboctave_error_handler) 
		    ("SparseMatrix::determinant error calculating determinant");
		  
		  UMFPACK_DNAME (report_status) (control, status);
		  UMFPACK_DNAME (report_info) (control, info);
		}
	      else
		retval = DET (d);

	      UMFPACK_DNAME (free_numeric) (&Numeric);
	    }
	}
    }
#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif

  return retval;
}

Matrix
SparseMatrix::dsolve (SparseType &mattype, const Matrix& b, octave_idx_type& err,
		      double& rcond, solve_singularity_handler, 
		      bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc < nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Diagonal ||
	  typ == SparseType::Permuted_Diagonal)
	{
	  retval.resize (nc, b.cols(), 0.);
	  if (typ == SparseType::Diagonal)
	    for (octave_idx_type j = 0; j < b.cols(); j++)
	      for (octave_idx_type i = 0; i < nm; i++)
		retval(i,j) = b(i,j) / data (i);
	  else
	    for (octave_idx_type j = 0; j < b.cols(); j++)
	      for (octave_idx_type k = 0; k < nc; k++)
		for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
		  retval(k,j) = b(ridx(i),j) / data (i);

	  if (calc_cond)
	    {
	      double dmax = 0., dmin = octave_Inf; 
	      for (octave_idx_type i = 0; i < nm; i++)
		{
		  double tmp = fabs(data(i));
		  if (tmp > dmax)
		    dmax = tmp;
		  if (tmp < dmin)
		    dmin = tmp;
		}
	      rcond = dmin / dmax;
	    }
	  else
	    rcond = 1.;
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix
SparseMatrix::dsolve (SparseType &mattype, const SparseMatrix& b, 
		      octave_idx_type& err, double& rcond, 
		      solve_singularity_handler, bool calc_cond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc < nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Diagonal ||
	  typ == SparseType::Permuted_Diagonal)
	{
	  octave_idx_type b_nc = b.cols ();
	  octave_idx_type b_nz = b.nnz ();
	  retval = SparseMatrix (nc, b_nc, b_nz);

	  retval.xcidx(0) = 0;
	  octave_idx_type ii = 0;
	  if (typ == SparseType::Diagonal)
	    for (octave_idx_type j = 0; j < b_nc; j++)
	      {
		for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		  {
		    if (b.ridx(i) >= nm)
		      break;
		    retval.xridx (ii) = b.ridx(i);
		    retval.xdata (ii++) = b.data(i) / data (b.ridx (i));
		  }
		retval.xcidx(j+1) = ii;
	      }
	  else
	    for (octave_idx_type j = 0; j < b_nc; j++)
	      {
		for (octave_idx_type l = 0; l < nc; l++)
		  for (octave_idx_type i = cidx(l); i < cidx(l+1); i++)
		    {
		      bool found = false;
		      octave_idx_type k;
		      for (k = b.cidx(j); k < b.cidx(j+1); k++)
			if (ridx(i) == b.ridx(k))
			  {
			    found = true;
			    break;
			  }
		      if (found)
			{
			  retval.xridx (ii) = l;
			  retval.xdata (ii++) = b.data(k) / data (i);
			}
		    }
		retval.xcidx(j+1) = ii;
	      }

	  if (calc_cond)
	    {
	      double dmax = 0., dmin = octave_Inf; 
	      for (octave_idx_type i = 0; i < nm; i++)
		{
		  double tmp = fabs(data(i));
		  if (tmp > dmax)
		    dmax = tmp;
		  if (tmp < dmin)
		    dmin = tmp;
		}
	      rcond = dmin / dmax;
	    }
	  else
	    rcond = 1.;
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

ComplexMatrix
SparseMatrix::dsolve (SparseType &mattype, const ComplexMatrix& b,
		      octave_idx_type& err, double& rcond,
		      solve_singularity_handler, bool calc_cond) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc < nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Diagonal ||
	  typ == SparseType::Permuted_Diagonal)
	{
	  retval.resize (nc, b.cols(), 0);
	  if (typ == SparseType::Diagonal)
	    for (octave_idx_type j = 0; j < b.cols(); j++)
		for (octave_idx_type i = 0; i < nm; i++)
		  retval(i,j) = b(i,j) / data (i);
	  else
	    for (octave_idx_type j = 0; j < b.cols(); j++)
	      for (octave_idx_type k = 0; k < nc; k++)
		for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
		  retval(k,j) = b(ridx(i),j) / data (i);
	    
	  if (calc_cond)
	    {
	      double dmax = 0., dmin = octave_Inf; 
	      for (octave_idx_type i = 0; i < nm; i++)
		{
		  double tmp = fabs(data(i));
		  if (tmp > dmax)
		    dmax = tmp;
		  if (tmp < dmin)
		    dmin = tmp;
		}
	      rcond = dmin / dmax;
	    }
	  else
	    rcond = 1.;
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseComplexMatrix
SparseMatrix::dsolve (SparseType &mattype, const SparseComplexMatrix& b,
		     octave_idx_type& err, double& rcond, 
		     solve_singularity_handler, bool calc_cond) const
{
  SparseComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc < nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Diagonal ||
	  typ == SparseType::Permuted_Diagonal)
	{
	  octave_idx_type b_nc = b.cols ();
	  octave_idx_type b_nz = b.nnz ();
	  retval = SparseComplexMatrix (nc, b_nc, b_nz);

	  retval.xcidx(0) = 0;
	  octave_idx_type ii = 0;
	  if (typ == SparseType::Diagonal)
	    for (octave_idx_type j = 0; j < b.cols(); j++)
	      {
		for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		  {
		    if (b.ridx(i) >= nm)
		      break;
		    retval.xridx (ii) = b.ridx(i);
		    retval.xdata (ii++) = b.data(i) / data (b.ridx (i));
		  }
		retval.xcidx(j+1) = ii;
	      }
	  else
	    for (octave_idx_type j = 0; j < b.cols(); j++)
	      {
		for (octave_idx_type l = 0; l < nc; l++)
		  for (octave_idx_type i = cidx(l); i < cidx(l+1); i++)
		    {
		      bool found = false;
		      octave_idx_type k;
		      for (k = b.cidx(j); k < b.cidx(j+1); k++)
			if (ridx(i) == b.ridx(k))
			  {
			    found = true;
			    break;
			  }
		      if (found)
			{
			  retval.xridx (ii) = l;
			  retval.xdata (ii++) = b.data(k) / data (i);
			}
		    }
		retval.xcidx(j+1) = ii;
	      }
	    
	  if (calc_cond)
	    {
	      double dmax = 0., dmin = octave_Inf; 
	      for (octave_idx_type i = 0; i < nm; i++)
		{
		  double tmp = fabs(data(i));
		  if (tmp > dmax)
		    dmax = tmp;
		  if (tmp < dmin)
		    dmin = tmp;
		}
	      rcond = dmin / dmax;
	    }
	  else
	    rcond = 1.;
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
SparseMatrix::utsolve (SparseType &mattype, const Matrix& b,
		       octave_idx_type& err, double& rcond,
		       solve_singularity_handler sing_handler, 
		       bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Permuted_Upper ||
	  typ == SparseType::Upper)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  octave_idx_type b_nc = b.cols ();
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  if (typ == SparseType::Permuted_Upper)
	    {
	      retval.resize (nc, b_nc);
	      octave_idx_type *perm = mattype.triangular_perm ();
	      OCTAVE_LOCAL_BUFFER (double, work, nm);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    work[i] = b(i,j);
		  for (octave_idx_type i = nr; i < nc; i++)
		    work[i] = 0.;

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      octave_idx_type kidx = perm[k];

		      if (work[k] != 0.)
			{
			  if (ridx(cidx(kidx+1)-1) != k ||
			      data(cidx(kidx+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(cidx(kidx+1)-1);
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(kidx); 
			       i < cidx(kidx+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval.xelem (perm[i], j) = work[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  octave_idx_type iidx = perm[k];

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(iidx+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(iidx); 
				   i < cidx(iidx+1)-1; i++)
				{
				  octave_idx_type idx2 = ridx(i);
				  work[idx2] = work[idx2] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (double, work, nm);
	      retval.resize (nc, b_nc);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    work[i] = b(i,j);
		  for (octave_idx_type i = nr; i < nc; i++)
		    work[i] = 0.;

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      if (work[k] != 0.)
			{
			  if (ridx(cidx(k+1)-1) != k ||
			      data(cidx(k+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(cidx(k+1)-1);
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval.xelem (i, j) = work[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); i < cidx(k+1)-1; i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix
SparseMatrix::utsolve (SparseType &mattype, const SparseMatrix& b,
		       octave_idx_type& err, double& rcond, 
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Permuted_Upper ||
	  typ == SparseType::Upper)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  octave_idx_type b_nc = b.cols ();
	  octave_idx_type b_nz = b.nnz ();
	  retval = SparseMatrix (nc, b_nc, b_nz);
	  retval.xcidx(0) = 0;
	  octave_idx_type ii = 0;
	  octave_idx_type x_nz = b_nz;

	  if (typ == SparseType::Permuted_Upper)
	    {
	      octave_idx_type *perm = mattype.triangular_perm ();
	      OCTAVE_LOCAL_BUFFER (double, work, nm);

	      OCTAVE_LOCAL_BUFFER (octave_idx_type, rperm, nc);
	      for (octave_idx_type i = 0; i < nc; i++)
		rperm[perm[i]] = i;

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    work[b.ridx(i)] = b.data(i);

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      octave_idx_type kidx = perm[k];

		      if (work[k] != 0.)
			{
			  if (ridx(cidx(kidx+1)-1) != k ||
			      data(cidx(kidx+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(cidx(kidx+1)-1);
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(kidx); 
			       i < cidx(kidx+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[rperm[i]] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = work[rperm[i]];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  octave_idx_type iidx = perm[k];

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(iidx+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(iidx); 
				   i < cidx(iidx+1)-1; i++)
				{
				  octave_idx_type idx2 = ridx(i);
				  work[idx2] = work[idx2] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (double, work, nm);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    work[b.ridx(i)] = b.data(i);

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      if (work[k] != 0.)
			{
			  if (ridx(cidx(k+1)-1) != k ||
			      data(cidx(k+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(cidx(k+1)-1);
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = work[i];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1)-1; i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }
  return retval;
}

ComplexMatrix
SparseMatrix::utsolve (SparseType &mattype, const ComplexMatrix& b, 
		       octave_idx_type& err, double& rcond, 
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Permuted_Upper ||
	  typ == SparseType::Upper)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  octave_idx_type b_nc = b.cols ();
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  if (typ == SparseType::Permuted_Upper)
	    {
	      retval.resize (nc, b_nc);
	      octave_idx_type *perm = mattype.triangular_perm ();
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    cwork[i] = b(i,j);
		  for (octave_idx_type i = nr; i < nc; i++)
		    cwork[i] = 0.;

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      octave_idx_type kidx = perm[k];

		      if (cwork[k] != 0.)
			{
			  if (ridx(cidx(kidx+1)-1) != k ||
			      data(cidx(kidx+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(cidx(kidx+1)-1);
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(kidx); 
			       i < cidx(kidx+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval.xelem (perm[i], j) = cwork[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  octave_idx_type iidx = perm[k];

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(iidx+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(iidx); 
				   i < cidx(iidx+1)-1; i++)
				{
				  octave_idx_type idx2 = ridx(i);
				  work[idx2] = work[idx2] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);
	      retval.resize (nc, b_nc);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    cwork[i] = b(i,j);
		  for (octave_idx_type i = nr; i < nc; i++)
		    cwork[i] = 0.;

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      if (cwork[k] != 0.)
			{
			  if (ridx(cidx(k+1)-1) != k ||
			      data(cidx(k+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(cidx(k+1)-1);
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      cwork[iidx] = cwork[iidx] - tmp  * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval.xelem (i, j) = cwork[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1)-1; i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseComplexMatrix
SparseMatrix::utsolve (SparseType &mattype, const SparseComplexMatrix& b,
		       octave_idx_type& err, double& rcond, 
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  SparseComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Permuted_Upper ||
	  typ == SparseType::Upper)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  octave_idx_type b_nc = b.cols ();
	  octave_idx_type b_nz = b.nnz ();
	  retval = SparseComplexMatrix (nc, b_nc, b_nz);
	  retval.xcidx(0) = 0;
	  octave_idx_type ii = 0;
	  octave_idx_type x_nz = b_nz;

	  if (typ == SparseType::Permuted_Upper)
	    {
	      octave_idx_type *perm = mattype.triangular_perm ();
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);

	      OCTAVE_LOCAL_BUFFER (octave_idx_type, rperm, nc);
	      for (octave_idx_type i = 0; i < nc; i++)
		rperm[perm[i]] = i;

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    cwork[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    cwork[b.ridx(i)] = b.data(i);

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      octave_idx_type kidx = perm[k];

		      if (cwork[k] != 0.)
			{
			  if (ridx(cidx(kidx+1)-1) != k ||
			      data(cidx(kidx+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(cidx(kidx+1)-1);
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(kidx); 
			       i < cidx(kidx+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[rperm[i]] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = cwork[rperm[i]];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  octave_idx_type iidx = perm[k];

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(iidx+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(iidx); 
				   i < cidx(iidx+1)-1; i++)
				{
				  octave_idx_type idx2 = ridx(i);
				  work[idx2] = work[idx2] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    cwork[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    cwork[b.ridx(i)] = b.data(i);

		  for (octave_idx_type k = nc-1; k >= 0; k--)
		    {
		      if (cwork[k] != 0.)
			{
			  if (ridx(cidx(k+1)-1) != k ||
			      data(cidx(k+1)-1) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(cidx(k+1)-1);
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1)-1; i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = cwork[i];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k >= 0; k--)
			{
			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k+1)-1);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1)-1; i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = 0; i < j+1; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
SparseMatrix::ltsolve (SparseType &mattype, const Matrix& b,
		       octave_idx_type& err, double& rcond,
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Permuted_Lower ||
	  typ == SparseType::Lower)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  octave_idx_type b_nc = b.cols ();
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  if (typ == SparseType::Permuted_Lower)
	    {
	      retval.resize (nc, b_nc);
	      OCTAVE_LOCAL_BUFFER (double, work, nm);
	      octave_idx_type *perm = mattype.triangular_perm ();

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  if (nc > nr)
		    for (octave_idx_type i = 0; i < nm; i++)
		      work[i] = 0.;
		  for (octave_idx_type i = 0; i < nr; i++)
		    work[perm[i]] = b(i,j);

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (work[k] != 0.)
			{
			  octave_idx_type minr = nr;
			  octave_idx_type mini = 0;

			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    if (perm[ridx(i)] < minr)
			      {
				minr = perm[ridx(i)];
				mini = i;
			      }

			  if (minr != k || data(mini) == 0)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(mini);
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    {
			      if (i == mini)
				continue;

			      octave_idx_type iidx = perm[ridx(i)];
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval (i, j) = work[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = 0; k < nc; k++)
			{
			  if (work[k] != 0.)
			    {
			      octave_idx_type minr = nr;
			      octave_idx_type mini = 0;

			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				if (perm[ridx(i)] < minr)
				  {
				    minr = perm[ridx(i)];
				    mini = i;
				  }

			      double tmp = work[k] / data(mini);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				{
				  if (i == mini)
				    continue;

				  octave_idx_type iidx = perm[ridx(i)];
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}

		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (double, work, nm);
	      retval.resize (nc, b_nc, 0.);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    work[i] = b(i,j);
		  for (octave_idx_type i = nr; i < nc; i++)
		    work[i] = 0.;
		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (work[k] != 0.)
			{
			  if (ridx(cidx(k)) != k ||
			      data(cidx(k)) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(cidx(k));
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(k)+1; 
			       i < cidx(k+1); i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval.xelem (i, j) = work[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k < nc; k++)
			{

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k));
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k)+1; 
				   i < cidx(k+1); i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix
SparseMatrix::ltsolve (SparseType &mattype, const SparseMatrix& b, 
		       octave_idx_type& err, double& rcond, 
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Permuted_Lower ||
	  typ == SparseType::Lower)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  octave_idx_type b_nc = b.cols ();
	  octave_idx_type b_nz = b.nnz ();
	  retval = SparseMatrix (nc, b_nc, b_nz);
	  retval.xcidx(0) = 0;
	  octave_idx_type ii = 0;
	  octave_idx_type x_nz = b_nz;

	  if (typ == SparseType::Permuted_Lower)
	    {
	      OCTAVE_LOCAL_BUFFER (double, work, nm);
	      octave_idx_type *perm = mattype.triangular_perm ();

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    work[perm[b.ridx(i)]] = b.data(i);

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (work[k] != 0.)
			{
			  octave_idx_type minr = nr;
			  octave_idx_type mini = 0;

			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    if (perm[ridx(i)] < minr)
			      {
				minr = perm[ridx(i)];
				mini = i;
			      }

			  if (minr != k || data(mini) == 0)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(mini);
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    {
			      if (i == mini)
				continue;

			      octave_idx_type iidx = perm[ridx(i)];
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = work[i];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = 0; k < nc; k++)
			{
			  if (work[k] != 0.)
			    {
			      octave_idx_type minr = nr;
			      octave_idx_type mini = 0;

			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				if (perm[ridx(i)] < minr)
				  {
				    minr = perm[ridx(i)];
				    mini = i;
				  }

			      double tmp = work[k] / data(mini);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				{
				  if (i == mini)
				    continue;

				  octave_idx_type iidx = perm[ridx(i)];
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}

		      double atmp = 0;
		      for (octave_idx_type i = j; i < nr; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (double, work, nm);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    work[b.ridx(i)] = b.data(i);

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (work[k] != 0.)
			{
			  if (ridx(cidx(k)) != k ||
			      data(cidx(k)) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  double tmp = work[k] / data(cidx(k));
			  work[k] = tmp;
			  for (octave_idx_type i = cidx(k)+1; i < cidx(k+1); i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      work[iidx] = work[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (work[i] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = work[i];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k < nc; k++)
			{

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k));
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k)+1; 
				   i < cidx(k+1); i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

ComplexMatrix
SparseMatrix::ltsolve (SparseType &mattype, const ComplexMatrix& b, 
		       octave_idx_type& err, double& rcond, 
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Permuted_Lower ||
	  typ == SparseType::Lower)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  octave_idx_type b_nc = b.cols ();
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  if (typ == SparseType::Permuted_Lower)
	    {
	      retval.resize (nc, b_nc);
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);
	      octave_idx_type *perm = mattype.triangular_perm ();

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    cwork[i] = 0.;
		  for (octave_idx_type i = 0; i < nr; i++)
		    cwork[perm[i]] = b(i,j);

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (cwork[k] != 0.)
			{
			  octave_idx_type minr = nr;
			  octave_idx_type mini = 0;

			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    if (perm[ridx(i)] < minr)
			      {
				minr = perm[ridx(i)];
				mini = i;
			      }

			  if (minr != k || data(mini) == 0)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(mini);
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    {
			      if (i == mini)
				continue;

			      octave_idx_type iidx = perm[ridx(i)];
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval (i, j) = cwork[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = 0; k < nc; k++)
			{
			  if (work[k] != 0.)
			    {
			      octave_idx_type minr = nr;
			      octave_idx_type mini = 0;

			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				if (perm[ridx(i)] < minr)
				  {
				    minr = perm[ridx(i)];
				    mini = i;
				  }

			      double tmp = work[k] / data(mini);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				{
				  if (i == mini)
				    continue;

				  octave_idx_type iidx = perm[ridx(i)];
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}

		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);
	      retval.resize (nc, b_nc, 0.);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nr; i++)
		    cwork[i] = b(i,j);
		  for (octave_idx_type i = nr; i < nc; i++)
		    cwork[i] = 0.;

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (cwork[k] != 0.)
			{
			  if (ridx(cidx(k)) != k ||
			      data(cidx(k)) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(cidx(k));
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(k)+1; i < cidx(k+1); i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    retval.xelem (i, j) = cwork[i];
		}

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k < nc; k++)
			{

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k));
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k)+1; 
				   i < cidx(k+1); i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseComplexMatrix
SparseMatrix::ltsolve (SparseType &mattype, const SparseComplexMatrix& b,
		       octave_idx_type& err, double& rcond, 
		       solve_singularity_handler sing_handler,
		       bool calc_cond) const
{
  SparseComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nm = (nc > nr ? nc : nr);
  err = 0;

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Permuted_Lower ||
	  typ == SparseType::Lower)
	{
	  double anorm = 0.;
	  double ainvnorm = 0.;
	  rcond = 1.;

	  if (calc_cond)
	    {
	      // Calculate the 1-norm of matrix for rcond calculation
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  octave_idx_type b_nc = b.cols ();
	  octave_idx_type b_nz = b.nnz ();
	  retval = SparseComplexMatrix (nc, b_nc, b_nz);
	  retval.xcidx(0) = 0;
	  octave_idx_type ii = 0;
	  octave_idx_type x_nz = b_nz;

	  if (typ == SparseType::Permuted_Lower)
	    {
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);
	      octave_idx_type *perm = mattype.triangular_perm ();

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    cwork[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    cwork[perm[b.ridx(i)]] = b.data(i);

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (cwork[k] != 0.)
			{
			  octave_idx_type minr = nr;
			  octave_idx_type mini = 0;

			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    if (perm[ridx(i)] < minr)
			      {
				minr = perm[ridx(i)];
				mini = i;
			      }

			  if (minr != k || data(mini) == 0)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(mini);
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(k); i < cidx(k+1); i++)
			    {
			      if (i == mini)
				continue;

			      octave_idx_type iidx = perm[ridx(i)];
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = cwork[i];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = 0; k < nc; k++)
			{
			  if (work[k] != 0.)
			    {
			      octave_idx_type minr = nr;
			      octave_idx_type mini = 0;

			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				if (perm[ridx(i)] < minr)
				  {
				    minr = perm[ridx(i)];
				    mini = i;
				  }

			      double tmp = work[k] / data(mini);
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k); 
				   i < cidx(k+1); i++)
				{
				  if (i == mini)
				    continue;

				  octave_idx_type iidx = perm[ridx(i)];
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}

		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (Complex, cwork, nm);

	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < nm; i++)
		    cwork[i] = 0.;
		  for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
		    cwork[b.ridx(i)] = b.data(i);

		  for (octave_idx_type k = 0; k < nc; k++)
		    {
		      if (cwork[k] != 0.)
			{
			  if (ridx(cidx(k)) != k ||
			      data(cidx(k)) == 0.)
			    {
			      err = -2;
			      goto triangular_error;
			    }			    

			  Complex tmp = cwork[k] / data(cidx(k));
			  cwork[k] = tmp;
			  for (octave_idx_type i = cidx(k)+1; i < cidx(k+1); i++)
			    {
			      octave_idx_type iidx = ridx(i);
			      cwork[iidx] = cwork[iidx] - tmp * data(i);
			    }
			}
		    }

		  // Count non-zeros in work vector and adjust space in
		  // retval if needed
		  octave_idx_type new_nnz = 0;
		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      new_nnz++;

		  if (ii + new_nnz > x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
		      retval.change_capacity (sz);
		      x_nz = sz;
		    }

		  for (octave_idx_type i = 0; i < nc; i++)
		    if (cwork[i] != 0.)
		      {
			retval.xridx(ii) = i;
			retval.xdata(ii++) = cwork[i];
		      }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      if (calc_cond)
		{
		  // Calculation of 1-norm of inv(*this)
		  OCTAVE_LOCAL_BUFFER (double, work, nm);
		  for (octave_idx_type i = 0; i < nm; i++)
		    work[i] = 0.;

		  for (octave_idx_type j = 0; j < nr; j++)
		    {
		      work[j] = 1.;

		      for (octave_idx_type k = j; k < nc; k++)
			{

			  if (work[k] != 0.)
			    {
			      double tmp = work[k] / data(cidx(k));
			      work[k] = tmp;
			      for (octave_idx_type i = cidx(k)+1; 
				   i < cidx(k+1); i++)
				{
				  octave_idx_type iidx = ridx(i);
				  work[iidx] = work[iidx] - tmp * data(i);
				}
			    }
			}
		      double atmp = 0;
		      for (octave_idx_type i = j; i < nc; i++)
			{
			  atmp += fabs(work[i]);
			  work[i] = 0.;
			}
		      if (atmp > ainvnorm)
			ainvnorm = atmp;
		    }
		  rcond = 1. / ainvnorm / anorm;
		}
	    }

	triangular_error:
	  if (err != 0)
	    {
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		   rcond);
	    }

	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0 || xisnan (rcond))
	    {
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision, rcond = %g",
		   rcond);
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
SparseMatrix::trisolve (SparseType &mattype, const Matrix& b,
			octave_idx_type& err, double& rcond,
			solve_singularity_handler sing_handler,
			bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (calc_cond)
    (*current_liboctave_error_handler) 
      ("calculation of condition number not implemented");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Tridiagonal_Hermitian)
	{
	  OCTAVE_LOCAL_BUFFER (double, D, nr);
	  OCTAVE_LOCAL_BUFFER (double, DL, nr - 1);

	  if (mattype.is_dense ())
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < nc-1; j++)
		{
		  D[j] = data(ii++);
		  DL[j] = data(ii);
		  ii += 2;
		}
	      D[nc-1] = data(ii);
	    }
	  else
	    {
	      D[0] = 0.;
	      for (octave_idx_type i = 0; i < nr - 1; i++)
		{
		  D[i+1] = 0.;
		  DL[i] = 0.;
		}

	      for (octave_idx_type j = 0; j < nc; j++)
		for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		  {
		    if (ridx(i) == j)
		      D[j] = data(i);
		    else if (ridx(i) == j + 1)
		      DL[j] = data(i);
		  }
	    }
	      
	  octave_idx_type b_nc = b.cols();
	  retval = b;
	  double *result = retval.fortran_vec ();

	  F77_XFCN (dptsv, DPTSV, (nr, b_nc, D, DL, result, 
				   b.rows(), err));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dptsv");
	  else if (err != 0)
	    {
	      err = 0;
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Tridiagonal;
	    }
	  else 
	    rcond = 1.;
	}

      if (typ == SparseType::Tridiagonal)
	{
	  OCTAVE_LOCAL_BUFFER (double, DU, nr - 1);
	  OCTAVE_LOCAL_BUFFER (double, D, nr);
	  OCTAVE_LOCAL_BUFFER (double, DL, nr - 1);

	  if (mattype.is_dense ())
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < nc-1; j++)
		{
		  D[j] = data(ii++);
		  DL[j] = data(ii++);
		  DU[j] = data(ii++);
		}
	      D[nc-1] = data(ii);
	    }
	  else
	    {
	      D[0] = 0.;
	      for (octave_idx_type i = 0; i < nr - 1; i++)
		{
		  D[i+1] = 0.;
		  DL[i] = 0.;
		  DU[i] = 0.;
		}

	      for (octave_idx_type j = 0; j < nc; j++)
		for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		  {
		    if (ridx(i) == j)
		      D[j] = data(i);
		    else if (ridx(i) == j + 1)
		      DL[j] = data(i);
		    else if (ridx(i) == j - 1)
		      DU[j-1] = data(i);
		  }
	    }

	  octave_idx_type b_nc = b.cols();
	  retval = b;
	  double *result = retval.fortran_vec ();

	  F77_XFCN (dgtsv, DGTSV, (nr, b_nc, DL, D, DU, result, 
				   b.rows(), err));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgtsv");
	  else if (err != 0)
	    {
	      rcond = 0.;
	      err = -2;

	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision");

	    } 
	  else 
	    rcond = 1.;
	}
      else if (typ != SparseType::Tridiagonal_Hermitian)
	       (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix
SparseMatrix::trisolve (SparseType &mattype, const SparseMatrix& b, 
			octave_idx_type& err, double& rcond, 
			solve_singularity_handler sing_handler,
			bool calc_cond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (calc_cond)
    (*current_liboctave_error_handler) 
      ("calculation of condition number not implemented");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      // Note can't treat symmetric case as there is no dpttrf function
      if (typ == SparseType::Tridiagonal ||
	  typ == SparseType::Tridiagonal_Hermitian)
	{
	  OCTAVE_LOCAL_BUFFER (double, DU2, nr - 2);
	  OCTAVE_LOCAL_BUFFER (double, DU, nr - 1);
	  OCTAVE_LOCAL_BUFFER (double, D, nr);
	  OCTAVE_LOCAL_BUFFER (double, DL, nr - 1);
	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  if (mattype.is_dense ())
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < nc-1; j++)
		{
		  D[j] = data(ii++);
		  DL[j] = data(ii++);
		  DU[j] = data(ii++);
		}
	      D[nc-1] = data(ii);
	    }
	  else
	    {
	      D[0] = 0.;
	      for (octave_idx_type i = 0; i < nr - 1; i++)
		{
		  D[i+1] = 0.;
		  DL[i] = 0.;
		  DU[i] = 0.;
		}

	      for (octave_idx_type j = 0; j < nc; j++)
		for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		  {
		    if (ridx(i) == j)
		      D[j] = data(i);
		    else if (ridx(i) == j + 1)
		      DL[j] = data(i);
		    else if (ridx(i) == j - 1)
		      DU[j-1] = data(i);
		  }
	    }

	  F77_XFCN (dgttrf, DGTTRF, (nr, DL, D, DU, DU2, pipvt, err));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgttrf");
	  else
	    {
	      if (err != 0) 
		{
		  rcond = 0.0;
		  err = -2;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision");

		} 
	      else 
		{
		  rcond = 1.0;
		  char job = 'N';
		  volatile octave_idx_type x_nz = b.nnz ();
		  octave_idx_type b_nc = b.cols ();
		  retval = SparseMatrix (nr, b_nc, x_nz);
		  retval.xcidx(0) = 0;
		  volatile octave_idx_type ii = 0;

		  OCTAVE_LOCAL_BUFFER (double, work, nr);

		  for (volatile octave_idx_type j = 0; j < b_nc; j++)
		    {
		      for (octave_idx_type i = 0; i < nr; i++)
			work[i] = 0.;
		      for (octave_idx_type i = b.cidx(j); i < b.cidx(j+1); i++)
			work[b.ridx(i)] = b.data(i);

		      F77_XFCN (dgttrs, DGTTRS, 
				(F77_CONST_CHAR_ARG2 (&job, 1),
				 nr, 1, DL, D, DU, DU2, pipvt, 
				 work, b.rows (), err
				 F77_CHAR_ARG_LEN (1)));
		    
		      if (f77_exception_encountered)
			{
			  (*current_liboctave_error_handler)
			    ("unrecoverable error in dgttrs");
			  break;
			}

		      // Count non-zeros in work vector and adjust 
		      // space in retval if needed
		      octave_idx_type new_nnz = 0;
		      for (octave_idx_type i = 0; i < nr; i++)
			if (work[i] != 0.)
			  new_nnz++;

		      if (ii + new_nnz > x_nz)
			{
			  // Resize the sparse matrix
			  octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
			  retval.change_capacity (sz);
			  x_nz = sz;
			}

		      for (octave_idx_type i = 0; i < nr; i++)
			if (work[i] != 0.)
			  {
			    retval.xridx(ii) = i;
			    retval.xdata(ii++) = work[i];
			  }
		      retval.xcidx(j+1) = ii;
		    }

		  retval.maybe_compress ();
		}
	    }
	}
      else if (typ != SparseType::Tridiagonal_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

ComplexMatrix
SparseMatrix::trisolve (SparseType &mattype, const ComplexMatrix& b, 
			octave_idx_type& err, double& rcond, 
			solve_singularity_handler sing_handler,
			bool calc_cond) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (calc_cond)
    (*current_liboctave_error_handler) 
      ("calculation of condition number not implemented");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();
      
      if (typ == SparseType::Tridiagonal_Hermitian)
	{
	  OCTAVE_LOCAL_BUFFER (double, D, nr);
	  OCTAVE_LOCAL_BUFFER (Complex, DL, nr - 1);

	  if (mattype.is_dense ())
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < nc-1; j++)
		{
		  D[j] = data(ii++);
		  DL[j] = data(ii);
		  ii += 2;
		}
	      D[nc-1] = data(ii);
	    }
	  else
	    {
	      D[0] = 0.;
	      for (octave_idx_type i = 0; i < nr - 1; i++)
		{
		  D[i+1] = 0.;
		  DL[i] = 0.;
		}

	      for (octave_idx_type j = 0; j < nc; j++)
		for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		  {
		    if (ridx(i) == j)
		      D[j] = data(i);
		    else if (ridx(i) == j + 1)
		      DL[j] = data(i);
		  }
	    }

	  octave_idx_type b_nr = b.rows ();
	  octave_idx_type b_nc = b.cols();
	  rcond = 1.;

	  retval = b;
	  Complex *result = retval.fortran_vec ();
		  
	  F77_XFCN (zptsv, ZPTSV, (nr, b_nc, D, DL, result, 
				   b_nr, err));

	  if (f77_exception_encountered)
	    {
	      (*current_liboctave_error_handler) 
		("unrecoverable error in zptsv");
	      err = -1;
	    }
	  else if (err != 0)
	    {
	      err = 0;
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Tridiagonal;
	    }
	}

      if (typ == SparseType::Tridiagonal)
	{
	  OCTAVE_LOCAL_BUFFER (Complex, DU, nr - 1);
	  OCTAVE_LOCAL_BUFFER (Complex, D, nr);
	  OCTAVE_LOCAL_BUFFER (Complex, DL, nr - 1);

	  if (mattype.is_dense ())
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < nc-1; j++)
		{
		  D[j] = data(ii++);
		  DL[j] = data(ii++);
		  DU[j] = data(ii++);
		}
	      D[nc-1] = data(ii);
	    }
	  else
	    {
	      D[0] = 0.;
	      for (octave_idx_type i = 0; i < nr - 1; i++)
		{
		  D[i+1] = 0.;
		  DL[i] = 0.;
		  DU[i] = 0.;
		}

	      for (octave_idx_type j = 0; j < nc; j++)
		for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		  {
		    if (ridx(i) == j)
		      D[j] = data(i);
		    else if (ridx(i) == j + 1)
		      DL[j] = data(i);
		    else if (ridx(i) == j - 1)
		      DU[j-1] = data(i);
		  }
	    }

	  octave_idx_type b_nr = b.rows();
	  octave_idx_type b_nc = b.cols();
	  rcond = 1.;

	  retval = b;
	  Complex *result = retval.fortran_vec ();
	      
	  F77_XFCN (zgtsv, ZGTSV, (nr, b_nc, DL, D, DU, result, 
				   b_nr, err));

	  if (f77_exception_encountered)
	    {
	      (*current_liboctave_error_handler) 
		("unrecoverable error in zgtsv");
	      err = -1;
	    }
	  else if (err != 0)
	    {
	      rcond = 0.;
	      err = -2;
		      
	      if (sing_handler)
		{
		  sing_handler (rcond);
		  mattype.mark_as_rectangular ();
		}
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision");
	    }
	}
      else if (typ != SparseType::Tridiagonal_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseComplexMatrix
SparseMatrix::trisolve (SparseType &mattype, const SparseComplexMatrix& b,
			octave_idx_type& err, double& rcond, 
			solve_singularity_handler sing_handler,
			bool calc_cond) const
{
  SparseComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (calc_cond)
    (*current_liboctave_error_handler) 
      ("calculation of condition number not implemented");
  else
    {
      // Print spparms("spumoni") info if requested
      int typ = mattype.type ();
      mattype.info ();
      
      // Note can't treat symmetric case as there is no dpttrf function
      if (typ == SparseType::Tridiagonal ||
	  typ == SparseType::Tridiagonal_Hermitian)
	{
	  OCTAVE_LOCAL_BUFFER (double, DU2, nr - 2);
	  OCTAVE_LOCAL_BUFFER (double, DU, nr - 1);
	  OCTAVE_LOCAL_BUFFER (double, D, nr);
	  OCTAVE_LOCAL_BUFFER (double, DL, nr - 1);
	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  if (mattype.is_dense ())
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < nc-1; j++)
		{
		  D[j] = data(ii++);
		  DL[j] = data(ii++);
		  DU[j] = data(ii++);
		}
	      D[nc-1] = data(ii);
	    }
	  else
	    {
	      D[0] = 0.;
	      for (octave_idx_type i = 0; i < nr - 1; i++)
		{
		  D[i+1] = 0.;
		  DL[i] = 0.;
		  DU[i] = 0.;
		}

	      for (octave_idx_type j = 0; j < nc; j++)
		for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		  {
		    if (ridx(i) == j)
		      D[j] = data(i);
		    else if (ridx(i) == j + 1)
		      DL[j] = data(i);
		    else if (ridx(i) == j - 1)
		      DU[j-1] = data(i);
		  }
	    }

	  F77_XFCN (dgttrf, DGTTRF, (nr, DL, D, DU, DU2, pipvt, err));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgttrf");
	  else
	    {
	      if (err != 0) 
		{
		  rcond = 0.0;
		  err = -2;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision");
		} 
	      else 
		{	
		  rcond = 1.;
		  char job = 'N';
		  octave_idx_type b_nr = b.rows ();
		  octave_idx_type b_nc = b.cols ();
		  OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);
		  OCTAVE_LOCAL_BUFFER (double, Bz, b_nr);

		  // Take a first guess that the number of non-zero terms
		  // will be as many as in b
		  volatile octave_idx_type x_nz = b.nnz ();
		  volatile octave_idx_type ii = 0;
		  retval = SparseComplexMatrix (b_nr, b_nc, x_nz);

		  retval.xcidx(0) = 0;
		  for (volatile octave_idx_type j = 0; j < b_nc; j++)
		    {

		      for (octave_idx_type i = 0; i < b_nr; i++)
			{
			  Complex c = b (i,j);
			  Bx[i] = std::real (c);
			  Bz[i] = std::imag (c);
			}

		      F77_XFCN (dgttrs, DGTTRS, 
				(F77_CONST_CHAR_ARG2 (&job, 1),
				 nr, 1, DL, D, DU, DU2, pipvt, 
				 Bx, b_nr, err
				 F77_CHAR_ARG_LEN (1)));
		    
		      if (f77_exception_encountered)
			{
			  (*current_liboctave_error_handler)
			    ("unrecoverable error in dgttrs");
			  break;
			}

		      if (err != 0)
			{
			  (*current_liboctave_error_handler)
			    ("SparseMatrix::solve solve failed");

			  err = -1;
			  break;
			}

		      F77_XFCN (dgttrs, DGTTRS, 
				(F77_CONST_CHAR_ARG2 (&job, 1),
				 nr, 1, DL, D, DU, DU2, pipvt, 
				 Bz, b_nr, err
				 F77_CHAR_ARG_LEN (1)));
		    
		      if (f77_exception_encountered)
			{
			  (*current_liboctave_error_handler)
			    ("unrecoverable error in dgttrs");
			  break;
			}

		      if (err != 0)
			{
			  (*current_liboctave_error_handler)
			    ("SparseMatrix::solve solve failed");

			  err = -1;
			  break;
			}

		      // Count non-zeros in work vector and adjust 
		      // space in retval if needed
		      octave_idx_type new_nnz = 0;
		      for (octave_idx_type i = 0; i < nr; i++)
			if (Bx[i] != 0. || Bz[i] != 0.)
			  new_nnz++;
		      
		      if (ii + new_nnz > x_nz)
			{
			  // Resize the sparse matrix
			  octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
			  retval.change_capacity (sz);
			  x_nz = sz;
			}
			  
		      for (octave_idx_type i = 0; i < nr; i++)
			if (Bx[i] != 0. || Bz[i] != 0.)
			  {
			    retval.xridx(ii) = i;
			    retval.xdata(ii++) = 
			      Complex (Bx[i], Bz[i]);
			  }

		      retval.xcidx(j+1) = ii;
		    }

		  retval.maybe_compress ();
		}
	    }
	}
      else if (typ != SparseType::Tridiagonal_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
SparseMatrix::bsolve (SparseType &mattype, const Matrix& b,
		      octave_idx_type& err, double& rcond,
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Banded_Hermitian)
	{
	  octave_idx_type n_lower = mattype.nlower ();
	  octave_idx_type ldm = n_lower + 1;
	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      {
		octave_idx_type ri = ridx (i);
		if (ri >= j)
		  m_band(ri - j, j) = data(i);
	      }

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    anorm = m_band.abs().sum().row(0).max();

	  char job = 'L';
	  F77_XFCN (dpbtrf, DPBTRF, (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, tmp_data, ldm, err
				     F77_CHAR_ARG_LEN (1)));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dpbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  // Matrix is not positive definite!! Fall through to
		  // unsymmetric banded solver.
		  mattype.mark_as_unsymmetric ();
		  typ = SparseType::Banded;
		  rcond = 0.0;
		  err = 0;
		} 
	      else 
		{
		  if (calc_cond)
		    {
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dpbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nr, n_lower, tmp_data, ldm,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		      	(*current_liboctave_error_handler) 
		      	  ("unrecoverable error in dpbcon");

		      if (err != 0) 
		      	err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      retval = b;
		      double *result = retval.fortran_vec ();

		      octave_idx_type b_nc = b.cols ();

		      F77_XFCN (dpbtrs, DPBTRS, 
				(F77_CONST_CHAR_ARG2 (&job, 1),
				 nr, n_lower, b_nc, tmp_data,
				 ldm, result, b.rows(), err
				 F77_CHAR_ARG_LEN (1)));
		    
		      if (f77_exception_encountered)
			(*current_liboctave_error_handler)
			  ("unrecoverable error in dpbtrs");

		      if (err != 0)
			{
			  (*current_liboctave_error_handler) 
			    ("SparseMatrix::solve solve failed");
			  err = -1;
			}
		    }
		}
	    }
	}

      if (typ == SparseType::Banded)
	{
	  // Create the storage for the banded form of the sparse matrix
	  int n_upper = mattype.nupper ();
	  int n_lower = mattype.nlower ();
	  int ldm = n_upper + 2 * n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      m_band(ridx(i) - j + n_lower + n_upper, j) = data(i);

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    {
	      for (octave_idx_type j = 0; j < nr; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  F77_XFCN (dgbtrf, DGBTRF, (nr, nr, n_lower, n_upper, tmp_data, 
				     ldm, pipvt, err));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgbtrf");
	  else
	    {
	      // Throw-away extra info LAPACK gives so as to not 
	      // change output.
	      if (err != 0) 
		{
		  err = -2;
		  rcond = 0.0;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision");

		} 
	      else 
		{
		  if (calc_cond)
		    {
		      char job = '1';
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dgbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nc, n_lower, n_upper, tmp_data, ldm, pipvt,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		        (*current_liboctave_error_handler) 
		          ("unrecoverable error in dgbcon");

		       if (err != 0) 
		        err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      retval = b;
		      double *result = retval.fortran_vec ();

		      octave_idx_type b_nc = b.cols ();

		      char job = 'N';
		      F77_XFCN (dgbtrs, DGBTRS, 
				(F77_CONST_CHAR_ARG2 (&job, 1),
				 nr, n_lower, n_upper, b_nc, tmp_data,
				 ldm, pipvt, result, b.rows(), err
				 F77_CHAR_ARG_LEN (1)));
		    
		      if (f77_exception_encountered)
			(*current_liboctave_error_handler)
			  ("unrecoverable error in dgbtrs");
		    }
		}
	    }
	}
      else if (typ != SparseType::Banded_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseMatrix
SparseMatrix::bsolve (SparseType &mattype, const SparseMatrix& b,
		      octave_idx_type& err, double& rcond, 
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Banded_Hermitian)
	{
	  int n_lower = mattype.nlower ();
	  int ldm = n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      {
		octave_idx_type ri = ridx (i);
		if (ri >= j)
		  m_band(ri - j, j) = data(i);
	      }

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    anorm = m_band.abs().sum().row(0).max();

	  char job = 'L';
	  F77_XFCN (dpbtrf, DPBTRF, (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, tmp_data, ldm, err
				     F77_CHAR_ARG_LEN (1)));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dpbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  mattype.mark_as_unsymmetric ();
		  typ = SparseType::Banded;
		  rcond = 0.0;
		  err = 0;
		} 
	      else 
		{
		  if (calc_cond)
		    {
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dpbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nr, n_lower, tmp_data, ldm,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		      	(*current_liboctave_error_handler) 
		      	  ("unrecoverable error in dpbcon");

		      if (err != 0) 
		      	err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      octave_idx_type b_nr = b.rows ();
		      octave_idx_type b_nc = b.cols ();
		      OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);

		      // Take a first guess that the number of non-zero terms
		      // will be as many as in b
		      volatile octave_idx_type x_nz = b.nnz ();
		      volatile octave_idx_type ii = 0;
		      retval = SparseMatrix (b_nr, b_nc, x_nz);

		      retval.xcidx(0) = 0;
		      for (volatile octave_idx_type j = 0; j < b_nc; j++)
			{
			  for (octave_idx_type i = 0; i < b_nr; i++)
			    Bx[i] = b.elem (i, j);

			  F77_XFCN (dpbtrs, DPBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, 1, tmp_data,
				     ldm, Bx, b_nr, err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dpbtrs");
			      err = -1;
			      break;
			    }

			  if (err != 0)
			    {
			      (*current_liboctave_error_handler) 
				("SparseMatrix::solve solve failed");
			      err = -1;
			      break;
			    }

			  for (octave_idx_type i = 0; i < b_nr; i++)
			    {
			      double tmp = Bx[i];
			      if (tmp != 0.0)
				{
				  if (ii == x_nz)
				    {
				      // Resize the sparse matrix
				      octave_idx_type sz = x_nz * 
					(b_nc - j) / b_nc;
				      sz = (sz > 10 ? sz : 10) + x_nz;
				      retval.change_capacity (sz);
				      x_nz = sz;
				    }
				  retval.xdata(ii) = tmp;
				  retval.xridx(ii++) = i;
				}
			    }
			  retval.xcidx(j+1) = ii;
			}

		      retval.maybe_compress ();
		    }
		}
	    }
	}

      if (typ == SparseType::Banded)
	{
	  // Create the storage for the banded form of the sparse matrix
	  octave_idx_type n_upper = mattype.nupper ();
	  octave_idx_type n_lower = mattype.nlower ();
	  octave_idx_type ldm = n_upper + 2 * n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      m_band(ridx(i) - j + n_lower + n_upper, j) = data(i);

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    {
	      for (octave_idx_type j = 0; j < nr; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  F77_XFCN (dgbtrf, DGBTRF, (nr, nr, n_lower, n_upper, tmp_data, 
				     ldm, pipvt, err));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  err = -2;
		  rcond = 0.0;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision");

		} 
	      else 
		{
		  if (calc_cond)
		    {
		      char job = '1';
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dgbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nc, n_lower, n_upper, tmp_data, ldm, pipvt,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		        (*current_liboctave_error_handler) 
		          ("unrecoverable error in dgbcon");

		       if (err != 0) 
		        err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      char job = 'N';
		      volatile octave_idx_type x_nz = b.nnz ();
		      octave_idx_type b_nc = b.cols ();
		      retval = SparseMatrix (nr, b_nc, x_nz);
		      retval.xcidx(0) = 0;
		      volatile octave_idx_type ii = 0;

		      OCTAVE_LOCAL_BUFFER (double, work, nr);

		      for (volatile octave_idx_type j = 0; j < b_nc; j++)
			{
			  for (octave_idx_type i = 0; i < nr; i++)
			    work[i] = 0.;
			  for (octave_idx_type i = b.cidx(j); 
			       i < b.cidx(j+1); i++)
			    work[b.ridx(i)] = b.data(i);

			  F77_XFCN (dgbtrs, DGBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, n_upper, 1, tmp_data,
				     ldm, pipvt, work, b.rows (), err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dgbtrs");
			      break;
			    }

			  // Count non-zeros in work vector and adjust 
			  // space in retval if needed
			  octave_idx_type new_nnz = 0;
			  for (octave_idx_type i = 0; i < nr; i++)
			    if (work[i] != 0.)
			      new_nnz++;

			  if (ii + new_nnz > x_nz)
			    {
			      // Resize the sparse matrix
			      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
			      retval.change_capacity (sz);
			      x_nz = sz;
			    }

			  for (octave_idx_type i = 0; i < nr; i++)
			    if (work[i] != 0.)
			      {
				retval.xridx(ii) = i;
				retval.xdata(ii++) = work[i];
			      }
			  retval.xcidx(j+1) = ii;
			}

		      retval.maybe_compress ();
		    }
		}
	    }
	}
      else if (typ != SparseType::Banded_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

ComplexMatrix
SparseMatrix::bsolve (SparseType &mattype, const ComplexMatrix& b, 
		      octave_idx_type& err, double& rcond, 
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Banded_Hermitian)
	{
	  octave_idx_type n_lower = mattype.nlower ();
	  octave_idx_type ldm = n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      {
		octave_idx_type ri = ridx (i);
		if (ri >= j)
		  m_band(ri - j, j) = data(i);
	      }

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    anorm = m_band.abs().sum().row(0).max();

	  char job = 'L';
	  F77_XFCN (dpbtrf, DPBTRF, (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, tmp_data, ldm, err
				     F77_CHAR_ARG_LEN (1)));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dpbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  // Matrix is not positive definite!! Fall through to
		  // unsymmetric banded solver.
		  mattype.mark_as_unsymmetric ();
		  typ = SparseType::Banded;
		  rcond = 0.0;
		  err = 0;
		} 
	      else 
		{
		  if (calc_cond)
		    {
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dpbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nr, n_lower, tmp_data, ldm,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		      	(*current_liboctave_error_handler) 
		      	  ("unrecoverable error in dpbcon");

		      if (err != 0) 
		      	err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      octave_idx_type b_nr = b.rows ();
		      octave_idx_type b_nc = b.cols ();

		      OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);
		      OCTAVE_LOCAL_BUFFER (double, Bz, b_nr);

		      retval.resize (b_nr, b_nc);
	      
		      for (volatile octave_idx_type j = 0; j < b_nc; j++)
			{
			  for (octave_idx_type i = 0; i < b_nr; i++)
			    {
			      Complex c = b (i,j);
			      Bx[i] = std::real (c);
			      Bz[i] = std::imag (c);
			    }
			  
			  F77_XFCN (dpbtrs, DPBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, 1, tmp_data,
				     ldm, Bx, b_nr, err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dpbtrs");
			      err = -1;
			      break;
			    }

			  if (err != 0)
			    {
			      (*current_liboctave_error_handler) 
				("SparseMatrix::solve solve failed");
			      err = -1;
			      break;
			    }

			  F77_XFCN (dpbtrs, DPBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, 1, tmp_data,
				     ldm, Bz, b.rows(), err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dpbtrs");
			      err = -1;
			      break;
			    }

			  if (err != 0)
			    {
			      (*current_liboctave_error_handler) 
				("SparseMatrix::solve solve failed");
			      err = -1;
			      break;
			    }

			  for (octave_idx_type i = 0; i < b_nr; i++)
			    retval (i, j) = Complex (Bx[i], Bz[i]);
			}
		    }
		}
	    }
	}

      if (typ == SparseType::Banded)
	{
	  // Create the storage for the banded form of the sparse matrix
	  int n_upper = mattype.nupper ();
	  int n_lower = mattype.nlower ();
	  int ldm = n_upper + 2 * n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      m_band(ridx(i) - j + n_lower + n_upper, j) = data(i);

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    {
	      for (octave_idx_type j = 0; j < nr; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  F77_XFCN (dgbtrf, DGBTRF, (nr, nr, n_lower, n_upper, tmp_data, 
				     ldm, pipvt, err));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  err = -2;
		  rcond = 0.0;

		  if (sing_handler)
		    {
		    sing_handler (rcond);
		    mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision");

		} 
	      else 
		{
		  if (calc_cond)
		    {
		      char job = '1';
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dpbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nr, n_lower, tmp_data, ldm,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		      	(*current_liboctave_error_handler) 
		      	  ("unrecoverable error in dpbcon");

		      if (err != 0) 
		      	err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
		            sing_handler (rcond);
			    mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      char job = 'N';
		      octave_idx_type b_nc = b.cols ();
		      retval.resize (nr,b_nc);

		      OCTAVE_LOCAL_BUFFER (double, Bz, nr);
		      OCTAVE_LOCAL_BUFFER (double, Bx, nr);

		      for (volatile octave_idx_type j = 0; j < b_nc; j++)
			{
			  for (octave_idx_type i = 0; i < nr; i++)
			    {
			      Complex c = b (i, j);
			      Bx[i] = std::real (c);
			      Bz[i] = std::imag  (c);
			    }

			  F77_XFCN (dgbtrs, DGBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, n_upper, 1, tmp_data,
				     ldm, pipvt, Bx, b.rows (), err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dgbtrs");
			      break;
			    }

			  F77_XFCN (dgbtrs, DGBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, n_upper, 1, tmp_data,
				     ldm, pipvt, Bz, b.rows (), err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dgbtrs");
			      break;
			    }

			  for (octave_idx_type i = 0; i < nr; i++)
			    retval (i, j) = Complex (Bx[i], Bz[i]);
			}
		    }
		}
	    }
	}
      else if (typ != SparseType::Banded_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

SparseComplexMatrix
SparseMatrix::bsolve (SparseType &mattype, const SparseComplexMatrix& b,
		      octave_idx_type& err, double& rcond, 
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  SparseComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Banded_Hermitian)
	{
	  int n_lower = mattype.nlower ();
	  int ldm = n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      {
		octave_idx_type ri = ridx (i);
		if (ri >= j)
		  m_band(ri - j, j) = data(i);
	      }

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    anorm = m_band.abs().sum().row(0).max();

	  char job = 'L';
	  F77_XFCN (dpbtrf, DPBTRF, (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, tmp_data, ldm, err
				     F77_CHAR_ARG_LEN (1)));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dpbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  // Matrix is not positive definite!! Fall through to
		  // unsymmetric banded solver.
		  mattype.mark_as_unsymmetric ();
		  typ = SparseType::Banded;

		  rcond = 0.0;
		  err = 0;
		} 
	      else 
		{
		  if (calc_cond)
		    {
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dpbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nr, n_lower, tmp_data, ldm,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		      	(*current_liboctave_error_handler) 
		      	  ("unrecoverable error in dpbcon");

		      if (err != 0) 
		      	err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      octave_idx_type b_nr = b.rows ();
		      octave_idx_type b_nc = b.cols ();
		      OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);
		      OCTAVE_LOCAL_BUFFER (double, Bz, b_nr);

		      // Take a first guess that the number of non-zero terms
		      // will be as many as in b
		      volatile octave_idx_type x_nz = b.nnz ();
		      volatile octave_idx_type ii = 0;
		      retval = SparseComplexMatrix (b_nr, b_nc, x_nz);

		      retval.xcidx(0) = 0;
		      for (volatile octave_idx_type j = 0; j < b_nc; j++)
			{

			  for (octave_idx_type i = 0; i < b_nr; i++)
			    {
			      Complex c = b (i,j);
			      Bx[i] = std::real (c);
			      Bz[i] = std::imag (c);
			    }

			  F77_XFCN (dpbtrs, DPBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, 1, tmp_data,
				     ldm, Bx, b_nr, err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dpbtrs");
			      err = -1;
			      break;
			    }

			  if (err != 0)
			    {
			      (*current_liboctave_error_handler) 
				("SparseMatrix::solve solve failed");
			      err = -1;
			      break;
			    }

			  F77_XFCN (dpbtrs, DPBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, 1, tmp_data,
				     ldm, Bz, b_nr, err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dpbtrs");
			      err = -1;
			      break;
			    }

			  if (err != 0)
			    {
			      (*current_liboctave_error_handler)
				("SparseMatrix::solve solve failed");

			      err = -1;
			      break;
			    }

			  // Count non-zeros in work vector and adjust 
			  // space in retval if needed
			  octave_idx_type new_nnz = 0;
			  for (octave_idx_type i = 0; i < nr; i++)
			    if (Bx[i] != 0. || Bz[i] != 0.)
			      new_nnz++;
			  
			  if (ii + new_nnz > x_nz)
			    {
			      // Resize the sparse matrix
			      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
			      retval.change_capacity (sz);
			      x_nz = sz;
			    }
			  
			  for (octave_idx_type i = 0; i < nr; i++)
			    if (Bx[i] != 0. || Bz[i] != 0.)
			      {
				retval.xridx(ii) = i;
				retval.xdata(ii++) = 
				  Complex (Bx[i], Bz[i]);
			      }

			  retval.xcidx(j+1) = ii;
			}

		      retval.maybe_compress ();
		    }
		}
	    }
	}

      if (typ == SparseType::Banded)
	{
	  // Create the storage for the banded form of the sparse matrix
	  int n_upper = mattype.nupper ();
	  int n_lower = mattype.nlower ();
	  int ldm = n_upper + 2 * n_lower + 1;

	  Matrix m_band (ldm, nc);
	  double *tmp_data = m_band.fortran_vec ();
	      
	  if (! mattype.is_dense ()) 
	    {
	      octave_idx_type ii = 0;

	      for (octave_idx_type j = 0; j < ldm; j++)
		for (octave_idx_type i = 0; i < nc; i++)
		  tmp_data[ii++] = 0.;
	    }

	  for (octave_idx_type j = 0; j < nc; j++)
	    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	      m_band(ridx(i) - j + n_lower + n_upper, j) = data(i);

	  // Calculate the norm of the matrix, for later use.
	  double anorm;
	  if (calc_cond)
	    {
	      for (octave_idx_type j = 0; j < nr; j++)
		{
		  double atmp = 0.;
		  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
		    atmp += fabs(data(i));
		  if (atmp > anorm)
		    anorm = atmp;
		}
	    }

	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  F77_XFCN (dgbtrf, DGBTRF, (nr, nr, n_lower, n_upper, tmp_data, 
				     ldm, pipvt, err));
	    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgbtrf");
	  else
	    {
	      if (err != 0) 
		{
		  err = -2;
		  rcond = 0.0;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision");

		} 
	      else 
		{
		  if (calc_cond)
		    {
		      char job = '1';
		      Array<double> z (3 * nr);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nr);
		      int *piz = iz.fortran_vec ();

		      F77_XFCN (dgbcon, DGBCON, 
		      	(F77_CONST_CHAR_ARG2 (&job, 1),
		      	 nc, n_lower, n_upper, tmp_data, ldm, pipvt,
		      	 anorm, rcond, pz, piz, err
		      	 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
		        (*current_liboctave_error_handler) 
		          ("unrecoverable error in dgbcon");

		       if (err != 0) 
		        err = -2;

		      volatile double rcond_plus_one = rcond + 1.0;

		      if (rcond_plus_one == 1.0 || xisnan (rcond))
		        {
		          err = -2;

		          if (sing_handler)
			    {
			      sing_handler (rcond);
			      mattype.mark_as_rectangular ();
			    }
		          else
		            (*current_liboctave_error_handler)
		              ("matrix singular to machine precision, rcond = %g",
		               rcond);
		        }
		    }
		  else
		    rcond = 1.;

		  if (err == 0)
		    {
		      char job = 'N';
		      volatile octave_idx_type x_nz = b.nnz ();
		      octave_idx_type b_nc = b.cols ();
		      retval = SparseComplexMatrix (nr, b_nc, x_nz);
		      retval.xcidx(0) = 0;
		      volatile octave_idx_type ii = 0;

		      OCTAVE_LOCAL_BUFFER (double, Bx, nr);
		      OCTAVE_LOCAL_BUFFER (double, Bz, nr);

		      for (volatile octave_idx_type j = 0; j < b_nc; j++)
			{
			  for (octave_idx_type i = 0; i < nr; i++)
			    {
			      Bx[i] = 0.;
			      Bz[i] = 0.;
			    }
			  for (octave_idx_type i = b.cidx(j); 
			       i < b.cidx(j+1); i++)
			    {
			      Complex c = b.data(i);
			      Bx[b.ridx(i)] = std::real (c);
			      Bz[b.ridx(i)] = std::imag (c);
			    }

			  F77_XFCN (dgbtrs, DGBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, n_upper, 1, tmp_data,
				     ldm, pipvt, Bx, b.rows (), err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dgbtrs");
			      break;
			    }

			  F77_XFCN (dgbtrs, DGBTRS, 
				    (F77_CONST_CHAR_ARG2 (&job, 1),
				     nr, n_lower, n_upper, 1, tmp_data,
				     ldm, pipvt, Bz, b.rows (), err
				     F77_CHAR_ARG_LEN (1)));
		    
			  if (f77_exception_encountered)
			    {
			      (*current_liboctave_error_handler)
				("unrecoverable error in dgbtrs");
			      break;
			    }

			  // Count non-zeros in work vector and adjust 
			  // space in retval if needed
			  octave_idx_type new_nnz = 0;
			  for (octave_idx_type i = 0; i < nr; i++)
			    if (Bx[i] != 0. || Bz[i] != 0.)
			      new_nnz++;

			  if (ii + new_nnz > x_nz)
			    {
			      // Resize the sparse matrix
			      octave_idx_type sz = new_nnz * (b_nc - j) + x_nz;
			      retval.change_capacity (sz);
			      x_nz = sz;
			    }

			  for (octave_idx_type i = 0; i < nr; i++)
			    if (Bx[i] != 0. || Bz[i] != 0.)
			      {
				retval.xridx(ii) = i;
				retval.xdata(ii++) = 
				  Complex (Bx[i], Bz[i]);
			      }
			  retval.xcidx(j+1) = ii;
			}

		      retval.maybe_compress ();
		    }
		}
	    }
	}
      else if (typ != SparseType::Banded_Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }
  
  return retval;
}

void *
SparseMatrix::factorize (octave_idx_type& err, double &rcond, Matrix &Control,
			 Matrix &Info, solve_singularity_handler sing_handler,
			 bool calc_cond) const
{
  // The return values
  void *Numeric = 0;
  err = 0;

#ifdef HAVE_UMFPACK
  // Setup the control parameters
  Control = Matrix (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  UMFPACK_DNAME (defaults) (control);

  double tmp = Voctave_sparse_controls.get_key ("spumoni");
  if (!xisnan (tmp))
    Control (UMFPACK_PRL) = tmp;
  tmp = Voctave_sparse_controls.get_key ("piv_tol");
  if (!xisnan (tmp))
    {
      Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
      Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
    }

  // Set whether we are allowed to modify Q or not
  tmp = Voctave_sparse_controls.get_key ("autoamd");
  if (!xisnan (tmp))
    Control (UMFPACK_FIXQ) = tmp;

  UMFPACK_DNAME (report_control) (control);

  const octave_idx_type *Ap = cidx ();
  const octave_idx_type *Ai = ridx ();
  const double *Ax = data ();
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  UMFPACK_DNAME (report_matrix) (nr, nc, Ap, Ai, Ax, 1, control);

  void *Symbolic;
  Info = Matrix (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status = UMFPACK_DNAME (qsymbolic) (nr, nc, Ap, Ai, Ax, NULL,
				     &Symbolic, control, info);

  if (status < 0)
    {
      (*current_liboctave_error_handler) 
	("SparseMatrix::solve symbolic factorization failed");
      err = -1;

      UMFPACK_DNAME (report_status) (control, status);
      UMFPACK_DNAME (report_info) (control, info);

      UMFPACK_DNAME (free_symbolic) (&Symbolic) ;
    }
  else
    {
      UMFPACK_DNAME (report_symbolic) (Symbolic, control);

      status = UMFPACK_DNAME (numeric) (Ap, Ai, Ax, Symbolic,
				   &Numeric, control, info) ;
      UMFPACK_DNAME (free_symbolic) (&Symbolic) ;

      if (calc_cond)
	rcond = Info (UMFPACK_RCOND);
      else
	rcond = 1.;
      volatile double rcond_plus_one = rcond + 1.0;

      if (status == UMFPACK_WARNING_singular_matrix || 
	  rcond_plus_one == 1.0 || xisnan (rcond))
	{
	  UMFPACK_DNAME (report_numeric) (Numeric, control);

	  err = -2;

	  if (sing_handler)
	    sing_handler (rcond);
	  else
	    (*current_liboctave_error_handler)
	      ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
	       rcond);

	}
      else if (status < 0)
	  {
	    (*current_liboctave_error_handler) 
	      ("SparseMatrix::solve numeric factorization failed");

	    UMFPACK_DNAME (report_status) (control, status);
	    UMFPACK_DNAME (report_info) (control, info);
	      
	    err = -1;
	  }
	else
	  {
	    UMFPACK_DNAME (report_numeric) (Numeric, control);
	  }
    }

  if (err != 0)
    UMFPACK_DNAME (free_numeric) (&Numeric);

#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif

  return Numeric;
}

Matrix
SparseMatrix::fsolve (SparseType &mattype, const Matrix& b,
		      octave_idx_type& err, double& rcond, 
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Hermitian)
	{
#ifdef HAVE_CHOLMOD
	  cholmod_common Common;
	  cholmod_common *cm = &Common;

	  // Setup initial parameters
	  CHOLMOD_NAME(start) (cm);
	  cm->prefer_zomplex = false;

	  double spu = Voctave_sparse_controls.get_key ("spumoni");
	  if (spu == 0.)
	    {
	      cm->print = -1;
	      cm->print_function = NULL;
	    }
	  else
	    {
	      cm->print = (int)spu + 2;
	      cm->print_function =&SparseCholPrint;
	    }

	  cm->error_handler = &SparseCholError;
	  cm->complex_divide = CHOLMOD_NAME(divcomplex);
	  cm->hypotenuse = CHOLMOD_NAME(hypot);

#ifdef HAVE_METIS
	  // METIS 4.0.1 uses malloc and free, and will terminate if
	  // it runs out of memory.  Use CHOLMOD's memory guard for
	  // METIS, which allocates a huge block of memory (and then
	  // immediately frees it) before calling METIS
	  cm->metis_memory = 2.0;

#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
	  // METIS 4.0.2 uses function pointers for malloc and free.
	  METIS_malloc = cm->malloc_memory;
	  METIS_free   = cm->free_memory;
	  // Turn off METIS memory guard.
	  cm->metis_memory   = 0.0;
#endif
#endif
#endif

	  cm->final_ll = true;

	  cholmod_sparse Astore;
	  cholmod_sparse *A = &Astore;
	  double dummy;
	  A->nrow = nr;
	  A->ncol = nc;

	  A->p = cidx();
	  A->i = ridx();
	  A->nzmax = nnz();
	  A->packed = true;
	  A->sorted = true;
	  A->nz = NULL;
#ifdef IDX_TYPE_LONG
	  A->itype = CHOLMOD_LONG;
#else
	  A->itype = CHOLMOD_INT;
#endif
	  A->dtype = CHOLMOD_DOUBLE;
	  A->stype = 1;
	  A->xtype = CHOLMOD_REAL;

	  if (nr < 1)
	    A->x = &dummy;
	  else
	    A->x = data();

	  cholmod_dense Bstore;
	  cholmod_dense *B = &Bstore;
	  B->nrow = b.rows();
	  B->ncol = b.cols();
	  B->d = B->nrow;
	  B->nzmax = B->nrow * B->ncol;
	  B->dtype = CHOLMOD_DOUBLE;
	  B->xtype = CHOLMOD_REAL;
	  if (nc < 1 || b.cols() < 1)
	    B->x = &dummy;
	  else
	    // We won't alter it, honest :-)
	    B->x = const_cast<double *>(b.fortran_vec());

	  cholmod_factor *L;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  L = CHOLMOD_NAME(analyze) (A, cm);
	  CHOLMOD_NAME(factorize) (A, L, cm);
	  if (calc_cond)
	    rcond = CHOLMOD_NAME(rcond)(L, cm);
	  else
	    rcond = 1.0;

	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  if (rcond == 0.0)
	    {
	      // Either its indefinite or singular. Try UMFPACK
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Full;
	    }
	  else
	    {
	      volatile double rcond_plus_one = rcond + 1.0;

	      if (rcond_plus_one == 1.0 || xisnan (rcond))
		{
		  err = -2;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		       rcond);
	      
		  return retval;
		}

	      cholmod_dense *X;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      X = CHOLMOD_NAME(solve) (CHOLMOD_A, L, B, cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	      retval.resize (b.rows (), b.cols());
	      for (octave_idx_type j = 0; j < b.cols(); j++)
		{
		  octave_idx_type jr = j * b.rows();
		  for (octave_idx_type i = 0; i < b.rows(); i++)
		    retval.xelem(i,j) = static_cast<double *>(X->x)[jr + i];
		}

	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CHOLMOD_NAME(free_dense) (&X, cm);
	      CHOLMOD_NAME(free_factor) (&L, cm);
	      CHOLMOD_NAME(finish) (cm);
	      CHOLMOD_NAME(print_common) (" ", cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
#else
	  (*current_liboctave_warning_handler)
	    ("CHOLMOD not installed");

	  mattype.mark_as_unsymmetric ();
	  typ = SparseType::Full;
#endif
	}

      if (typ == SparseType::Full)
	{
#ifdef HAVE_UMFPACK
	  Matrix Control, Info;
	  void *Numeric = 
	    factorize (err, rcond, Control, Info, sing_handler, calc_cond);

	  if (err == 0)
	    {
	      const double *Bx = b.fortran_vec ();
	      retval.resize (b.rows (), b.cols());
	      double *result = retval.fortran_vec ();
	      octave_idx_type b_nr = b.rows ();
	      octave_idx_type b_nc = b.cols ();
	      int status = 0;
	      double *control = Control.fortran_vec ();
	      double *info = Info.fortran_vec ();
	      const octave_idx_type *Ap = cidx ();
	      const octave_idx_type *Ai = ridx ();
	      const double *Ax = data ();

	      for (octave_idx_type j = 0, iidx = 0; j < b_nc; j++, iidx += b_nr)
		{
		  status = UMFPACK_DNAME (solve) (UMFPACK_A, Ap, 
					     Ai, Ax, &result[iidx], &Bx[iidx],
					     Numeric, control, info);
		  if (status < 0)
		    {
		      (*current_liboctave_error_handler) 
			("SparseMatrix::solve solve failed");

		      UMFPACK_DNAME (report_status) (control, status);
		      
		      err = -1;
		  
		      break;
		    }
		}

	      UMFPACK_DNAME (report_info) (control, info);
		
	      UMFPACK_DNAME (free_numeric) (&Numeric);
	    }
	  else
	    mattype.mark_as_rectangular ();

#else
	  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
	}
      else if (typ != SparseType::Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }
  
  return retval;
}

SparseMatrix
SparseMatrix::fsolve (SparseType &mattype, const SparseMatrix& b,
		      octave_idx_type& err, double& rcond,
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  SparseMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Hermitian)
	{
#ifdef HAVE_CHOLMOD
	  cholmod_common Common;
	  cholmod_common *cm = &Common;

	  // Setup initial parameters
	  CHOLMOD_NAME(start) (cm);
	  cm->prefer_zomplex = false;

	  double spu = Voctave_sparse_controls.get_key ("spumoni");
	  if (spu == 0.)
	    {
	      cm->print = -1;
	      cm->print_function = NULL;
	    }
	  else
	    {
	      cm->print = (int)spu + 2;
	      cm->print_function =&SparseCholPrint;
	    }

	  cm->error_handler = &SparseCholError;
	  cm->complex_divide = CHOLMOD_NAME(divcomplex);
	  cm->hypotenuse = CHOLMOD_NAME(hypot);

#ifdef HAVE_METIS
	  // METIS 4.0.1 uses malloc and free, and will terminate MATLAB if
	  // it runs out of memory.  Use CHOLMOD's memory guard for METIS, 
	  // which mxMalloc's a huge block of memory (and then immediately 
	  // mxFree's it) before calling METIS
	  cm->metis_memory = 2.0;

#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
	  // METIS 4.0.2 uses function pointers for malloc and free
	  METIS_malloc = cm->malloc_memory;
	  METIS_free   = cm->free_memory;
	  // Turn off METIS memory guard.  It is not needed, because mxMalloc
	  // will safely terminate the mexFunction and free any workspace
	  // without killing all of octave.
	  cm->metis_memory   = 0.0;
#endif
#endif
#endif

	  cm->final_ll = true;

	  cholmod_sparse Astore;
	  cholmod_sparse *A = &Astore;
	  double dummy;
	  A->nrow = nr;
	  A->ncol = nc;

	  A->p = cidx();
	  A->i = ridx();
	  A->nzmax = nnz();
	  A->packed = true;
	  A->sorted = true;
	  A->nz = NULL;
#ifdef IDX_TYPE_LONG
	  A->itype = CHOLMOD_LONG;
#else
	  A->itype = CHOLMOD_INT;
#endif
	  A->dtype = CHOLMOD_DOUBLE;
	  A->stype = 1;
	  A->xtype = CHOLMOD_REAL;

	  if (nr < 1)
	    A->x = &dummy;
	  else
	    A->x = data();

	  cholmod_sparse Bstore;
	  cholmod_sparse *B = &Bstore;
	  B->nrow = b.rows();
	  B->ncol = b.cols();
	  B->p = b.cidx();
	  B->i = b.ridx();
	  B->nzmax = b.nnz();
	  B->packed = true;
	  B->sorted = true;
	  B->nz = NULL;
#ifdef IDX_TYPE_LONG
	  B->itype = CHOLMOD_LONG;
#else
	  B->itype = CHOLMOD_INT;
#endif
	  B->dtype = CHOLMOD_DOUBLE;
	  B->stype = 0;
	  B->xtype = CHOLMOD_REAL;

	  if (b.rows() < 1 || b.cols() < 1)
	    B->x = &dummy;
	  else
	    B->x = b.data();

	  cholmod_factor *L;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  L = CHOLMOD_NAME(analyze) (A, cm);
	  CHOLMOD_NAME(factorize) (A, L, cm);
	  if (calc_cond)
	    rcond = CHOLMOD_NAME(rcond)(L, cm);
	  else
	    rcond = 1.;
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  if (rcond == 0.0)
	    {
	      // Either its indefinite or singular. Try UMFPACK
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Full;
	    }
	  else
	    {
	      volatile double rcond_plus_one = rcond + 1.0;

	      if (rcond_plus_one == 1.0 || xisnan (rcond))
		{
		  err = -2;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		       rcond);
	      
		  return retval;
		}

	      cholmod_sparse *X;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      X = CHOLMOD_NAME(spsolve) (CHOLMOD_A, L, B, cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	      retval = SparseMatrix (static_cast<octave_idx_type>(X->nrow), 
				     static_cast<octave_idx_type>(X->ncol),
				     static_cast<octave_idx_type>(X->nzmax));
	      for (octave_idx_type j = 0; 
		   j <= static_cast<octave_idx_type>(X->ncol); j++)
		retval.xcidx(j) = static_cast<octave_idx_type *>(X->p)[j];
	      for (octave_idx_type j = 0; 
		   j < static_cast<octave_idx_type>(X->nzmax); j++)
		{
		  retval.xridx(j) = static_cast<octave_idx_type *>(X->i)[j];
		  retval.xdata(j) = static_cast<double *>(X->x)[j];
		}

	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CHOLMOD_NAME(free_sparse) (&X, cm);
	      CHOLMOD_NAME(free_factor) (&L, cm);
	      CHOLMOD_NAME(finish) (cm);
	      CHOLMOD_NAME(print_common) (" ", cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
#else
	  (*current_liboctave_warning_handler)
	    ("CHOLMOD not installed");

	  mattype.mark_as_unsymmetric ();
	  typ = SparseType::Full;
#endif
	}

      if (typ == SparseType::Full)
	{
#ifdef HAVE_UMFPACK
	  Matrix Control, Info;
	  void *Numeric = factorize (err, rcond, Control, Info, 
				     sing_handler, calc_cond);

	  if (err == 0)
	    {
	      octave_idx_type b_nr = b.rows ();
	      octave_idx_type b_nc = b.cols ();
	      int status = 0;
	      double *control = Control.fortran_vec ();
	      double *info = Info.fortran_vec ();
	      const octave_idx_type *Ap = cidx ();
	      const octave_idx_type *Ai = ridx ();
	      const double *Ax = data ();

	      OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);
	      OCTAVE_LOCAL_BUFFER (double, Xx, b_nr);

	      // Take a first guess that the number of non-zero terms
	      // will be as many as in b
	      octave_idx_type x_nz = b.nnz ();
	      octave_idx_type ii = 0;
	      retval = SparseMatrix (b_nr, b_nc, x_nz);

	      retval.xcidx(0) = 0;
	      for (octave_idx_type j = 0; j < b_nc; j++)
		{

		  for (octave_idx_type i = 0; i < b_nr; i++)
		    Bx[i] = b.elem (i, j);

		  status = UMFPACK_DNAME (solve) (UMFPACK_A, Ap, 
					     Ai, Ax, Xx, Bx, Numeric, control, 
					     info);
		  if (status < 0)
		    {
		      (*current_liboctave_error_handler) 
			("SparseMatrix::solve solve failed");

		      UMFPACK_DNAME (report_status) (control, status);
		  
		      err = -1;

		      break;
		    }
	      
		  for (octave_idx_type i = 0; i < b_nr; i++)
		    {
		      double tmp = Xx[i];
		      if (tmp != 0.0)
			{
			  if (ii == x_nz)
			    {
			      // Resize the sparse matrix
			      octave_idx_type sz = x_nz * (b_nc - j) / b_nc;
			      sz = (sz > 10 ? sz : 10) + x_nz;
			      retval.change_capacity (sz);
			      x_nz = sz;
			    }
			  retval.xdata(ii) = tmp;
			  retval.xridx(ii++) = i;
			}
		    }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      UMFPACK_DNAME (report_info) (control, info);

	      UMFPACK_DNAME (free_numeric) (&Numeric);
	    }
	  else
	    mattype.mark_as_rectangular ();

#else
	  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
	}
      else if (typ != SparseType::Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }
  
  return retval;
}

ComplexMatrix
SparseMatrix::fsolve (SparseType &mattype, const ComplexMatrix& b, 
		      octave_idx_type& err, double& rcond,
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Hermitian)
	{
#ifdef HAVE_CHOLMOD
	  cholmod_common Common;
	  cholmod_common *cm = &Common;

	  // Setup initial parameters
	  CHOLMOD_NAME(start) (cm);
	  cm->prefer_zomplex = false;

	  double spu = Voctave_sparse_controls.get_key ("spumoni");
	  if (spu == 0.)
	    {
	      cm->print = -1;
	      cm->print_function = NULL;
	    }
	  else
	    {
	      cm->print = (int)spu + 2;
	      cm->print_function =&SparseCholPrint;
	    }

	  cm->error_handler = &SparseCholError;
	  cm->complex_divide = CHOLMOD_NAME(divcomplex);
	  cm->hypotenuse = CHOLMOD_NAME(hypot);

#ifdef HAVE_METIS
	  // METIS 4.0.1 uses malloc and free, and will terminate MATLAB if
	  // it runs out of memory.  Use CHOLMOD's memory guard for METIS, 
	  // which mxMalloc's a huge block of memory (and then immediately 
	  // mxFree's it) before calling METIS
	  cm->metis_memory = 2.0;

#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
	  // METIS 4.0.2 uses function pointers for malloc and free
	  METIS_malloc = cm->malloc_memory;
	  METIS_free   = cm->free_memory;
	  // Turn off METIS memory guard.  It is not needed, because mxMalloc
	  // will safely terminate the mexFunction and free any workspace
	  // without killing all of octave.
	  cm->metis_memory   = 0.0;
#endif
#endif
#endif

	  cm->final_ll = true;

	  cholmod_sparse Astore;
	  cholmod_sparse *A = &Astore;
	  double dummy;
	  A->nrow = nr;
	  A->ncol = nc;

	  A->p = cidx();
	  A->i = ridx();
	  A->nzmax = nnz();
	  A->packed = true;
	  A->sorted = true;
	  A->nz = NULL;
#ifdef IDX_TYPE_LONG
	  A->itype = CHOLMOD_LONG;
#else
	  A->itype = CHOLMOD_INT;
#endif
	  A->dtype = CHOLMOD_DOUBLE;
	  A->stype = 1;
	  A->xtype = CHOLMOD_REAL;

	  if (nr < 1)
	    A->x = &dummy;
	  else
	    A->x = data();

	  cholmod_dense Bstore;
	  cholmod_dense *B = &Bstore;
	  B->nrow = b.rows();
	  B->ncol = b.cols();
	  B->d = B->nrow;
	  B->nzmax = B->nrow * B->ncol;
	  B->dtype = CHOLMOD_DOUBLE;
	  B->xtype = CHOLMOD_COMPLEX;
	  if (nc < 1 || b.cols() < 1)
	    B->x = &dummy;
	  else
	    // We won't alter it, honest :-)
	    B->x = const_cast<Complex *>(b.fortran_vec());

	  cholmod_factor *L;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  L = CHOLMOD_NAME(analyze) (A, cm);
	  CHOLMOD_NAME(factorize) (A, L, cm);
	  if (calc_cond)
	    rcond = CHOLMOD_NAME(rcond)(L, cm);
	  else
	    rcond = 1.0;
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  if (rcond == 0.0)
	    {
	      // Either its indefinite or singular. Try UMFPACK
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Full;
	    }
	  else
	    {
	      volatile double rcond_plus_one = rcond + 1.0;

	      if (rcond_plus_one == 1.0 || xisnan (rcond))
		{
		  err = -2;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		       rcond);
	      
		  return retval;
		}

	      cholmod_dense *X;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      X = CHOLMOD_NAME(solve) (CHOLMOD_A, L, B, cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	      retval.resize (b.rows (), b.cols());
	      for (octave_idx_type j = 0; j < b.cols(); j++)
		{
		  octave_idx_type jr = j * b.rows();
		  for (octave_idx_type i = 0; i < b.rows(); i++)
		    retval.xelem(i,j) = static_cast<Complex *>(X->x)[jr + i];
		}

	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CHOLMOD_NAME(free_dense) (&X, cm);
	      CHOLMOD_NAME(free_factor) (&L, cm);
	      CHOLMOD_NAME(finish) (cm);
	      CHOLMOD_NAME(print_common) (" ", cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
#else
	  (*current_liboctave_warning_handler)
	    ("CHOLMOD not installed");

	  mattype.mark_as_unsymmetric ();
	  typ = SparseType::Full;
#endif
	}

      if (typ == SparseType::Full)
	{
#ifdef HAVE_UMFPACK
	  Matrix Control, Info;
	  void *Numeric = factorize (err, rcond, Control, Info, 
				     sing_handler, calc_cond);

	  if (err == 0)
	    {
	      octave_idx_type b_nr = b.rows ();
	      octave_idx_type b_nc = b.cols ();
	      int status = 0;
	      double *control = Control.fortran_vec ();
	      double *info = Info.fortran_vec ();
	      const octave_idx_type *Ap = cidx ();
	      const octave_idx_type *Ai = ridx ();
	      const double *Ax = data ();

	      OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);
	      OCTAVE_LOCAL_BUFFER (double, Bz, b_nr);

	      retval.resize (b_nr, b_nc);

	      OCTAVE_LOCAL_BUFFER (double, Xx, b_nr);
	      OCTAVE_LOCAL_BUFFER (double, Xz, b_nr);
	      
	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < b_nr; i++)
		    {
		      Complex c = b (i,j);
		      Bx[i] = std::real (c);
		      Bz[i] = std::imag (c);
		    }

		  status = UMFPACK_DNAME (solve) (UMFPACK_A, Ap, 
					     Ai, Ax, Xx, Bx, Numeric, control, 
					     info);
		  int status2 = UMFPACK_DNAME (solve) (UMFPACK_A,
						  Ap, Ai, Ax, Xz, Bz, Numeric, 
						  control, info) ;

		  if (status < 0 || status2 < 0)
		    {
		      (*current_liboctave_error_handler) 
			("SparseMatrix::solve solve failed");

		      UMFPACK_DNAME (report_status) (control, status);
		      
		      err = -1;

		      break;
		    }

		  for (octave_idx_type i = 0; i < b_nr; i++)
		    retval (i, j) = Complex (Xx[i], Xz[i]);
		}

	      UMFPACK_DNAME (report_info) (control, info);

	      UMFPACK_DNAME (free_numeric) (&Numeric);
	    }
	  else
	    mattype.mark_as_rectangular ();

#else
	  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
	}
      else if (typ != SparseType::Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }
  
  return retval;
}

SparseComplexMatrix
SparseMatrix::fsolve (SparseType &mattype, const SparseComplexMatrix& b, 
		      octave_idx_type& err, double& rcond,
		      solve_singularity_handler sing_handler,
		      bool calc_cond) const
{
  SparseComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  err = 0;

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      // Print spparms("spumoni") info if requested
      volatile int typ = mattype.type ();
      mattype.info ();

      if (typ == SparseType::Hermitian)
	{
#ifdef HAVE_CHOLMOD
	  cholmod_common Common;
	  cholmod_common *cm = &Common;

	  // Setup initial parameters
	  CHOLMOD_NAME(start) (cm);
	  cm->prefer_zomplex = false;

	  double spu = Voctave_sparse_controls.get_key ("spumoni");
	  if (spu == 0.)
	    {
	      cm->print = -1;
	      cm->print_function = NULL;
	    }
	  else
	    {
	      cm->print = (int)spu + 2;
	      cm->print_function =&SparseCholPrint;
	    }

	  cm->error_handler = &SparseCholError;
	  cm->complex_divide = CHOLMOD_NAME(divcomplex);
	  cm->hypotenuse = CHOLMOD_NAME(hypot);

#ifdef HAVE_METIS
	  // METIS 4.0.1 uses malloc and free, and will terminate MATLAB if
	  // it runs out of memory.  Use CHOLMOD's memory guard for METIS, 
	  // which mxMalloc's a huge block of memory (and then immediately 
	  // mxFree's it) before calling METIS
	  cm->metis_memory = 2.0;

#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
	  // METIS 4.0.2 uses function pointers for malloc and free
	  METIS_malloc = cm->malloc_memory;
	  METIS_free   = cm->free_memory;
	  // Turn off METIS memory guard.  It is not needed, because mxMalloc
	  // will safely terminate the mexFunction and free any workspace
	  // without killing all of octave.
	  cm->metis_memory   = 0.0;
#endif
#endif
#endif

	  cm->final_ll = true;

	  cholmod_sparse Astore;
	  cholmod_sparse *A = &Astore;
	  double dummy;
	  A->nrow = nr;
	  A->ncol = nc;

	  A->p = cidx();
	  A->i = ridx();
	  A->nzmax = nnz();
	  A->packed = true;
	  A->sorted = true;
	  A->nz = NULL;
#ifdef IDX_TYPE_LONG
	  A->itype = CHOLMOD_LONG;
#else
	  A->itype = CHOLMOD_INT;
#endif
	  A->dtype = CHOLMOD_DOUBLE;
	  A->stype = 1;
	  A->xtype = CHOLMOD_REAL;

	  if (nr < 1)
	    A->x = &dummy;
	  else
	    A->x = data();

	  cholmod_sparse Bstore;
	  cholmod_sparse *B = &Bstore;
	  B->nrow = b.rows();
	  B->ncol = b.cols();
	  B->p = b.cidx();
	  B->i = b.ridx();
	  B->nzmax = b.nnz();
	  B->packed = true;
	  B->sorted = true;
	  B->nz = NULL;
#ifdef IDX_TYPE_LONG
	  B->itype = CHOLMOD_LONG;
#else
	  B->itype = CHOLMOD_INT;
#endif
	  B->dtype = CHOLMOD_DOUBLE;
	  B->stype = 0;
	  B->xtype = CHOLMOD_COMPLEX;

	  if (b.rows() < 1 || b.cols() < 1)
	    B->x = &dummy;
	  else
	    B->x = b.data();

	  cholmod_factor *L;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  L = CHOLMOD_NAME(analyze) (A, cm);
	  CHOLMOD_NAME(factorize) (A, L, cm);
	  if (calc_cond)
	    rcond = CHOLMOD_NAME(rcond)(L, cm);
	  else
	    rcond = 1.0;
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  if (rcond == 0.0)
	    {
	      // Either its indefinite or singular. Try UMFPACK
	      mattype.mark_as_unsymmetric ();
	      typ = SparseType::Full;
	    }
	  else
	    {
	      volatile double rcond_plus_one = rcond + 1.0;

	      if (rcond_plus_one == 1.0 || xisnan (rcond))
		{
		  err = -2;

		  if (sing_handler)
		    {
		      sing_handler (rcond);
		      mattype.mark_as_rectangular ();
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("SparseMatrix::solve matrix singular to machine precision, rcond = %g",
		       rcond);
	      
		  return retval;
		}

	      cholmod_sparse *X;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      X = CHOLMOD_NAME(spsolve) (CHOLMOD_A, L, B, cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	      retval = SparseComplexMatrix 
		(static_cast<octave_idx_type>(X->nrow), 
		 static_cast<octave_idx_type>(X->ncol),
		 static_cast<octave_idx_type>(X->nzmax));
	      for (octave_idx_type j = 0; 
		   j <= static_cast<octave_idx_type>(X->ncol); j++)
		retval.xcidx(j) = static_cast<octave_idx_type *>(X->p)[j];
	      for (octave_idx_type j = 0; 
		   j < static_cast<octave_idx_type>(X->nzmax); j++)
		{
		  retval.xridx(j) = static_cast<octave_idx_type *>(X->i)[j];
		  retval.xdata(j) = static_cast<Complex *>(X->x)[j];
		}

	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CHOLMOD_NAME(free_sparse) (&X, cm);
	      CHOLMOD_NAME(free_factor) (&L, cm);
	      CHOLMOD_NAME(finish) (cm);
	      CHOLMOD_NAME(print_common) (" ", cm);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
#else
	  (*current_liboctave_warning_handler)
	    ("CHOLMOD not installed");

	  mattype.mark_as_unsymmetric ();
	  typ = SparseType::Full;
#endif
	}

      if (typ == SparseType::Full)
	{
#ifdef HAVE_UMFPACK
	  Matrix Control, Info;
	  void *Numeric = factorize (err, rcond, Control, Info, 
				     sing_handler, calc_cond);

	  if (err == 0)
	    {
	      octave_idx_type b_nr = b.rows ();
	      octave_idx_type b_nc = b.cols ();
	      int status = 0;
	      double *control = Control.fortran_vec ();
	      double *info = Info.fortran_vec ();
	      const octave_idx_type *Ap = cidx ();
	      const octave_idx_type *Ai = ridx ();
	      const double *Ax = data ();

	      OCTAVE_LOCAL_BUFFER (double, Bx, b_nr);
	      OCTAVE_LOCAL_BUFFER (double, Bz, b_nr);

	      // Take a first guess that the number of non-zero terms
	      // will be as many as in b
	      octave_idx_type x_nz = b.nnz ();
	      octave_idx_type ii = 0;
	      retval = SparseComplexMatrix (b_nr, b_nc, x_nz);

	      OCTAVE_LOCAL_BUFFER (double, Xx, b_nr);
	      OCTAVE_LOCAL_BUFFER (double, Xz, b_nr);
	      
	      retval.xcidx(0) = 0;
	      for (octave_idx_type j = 0; j < b_nc; j++)
		{
		  for (octave_idx_type i = 0; i < b_nr; i++)
		    {
		      Complex c = b (i,j);
		      Bx[i] = std::real (c);
		      Bz[i] = std::imag (c);
		    }

		  status = UMFPACK_DNAME (solve) (UMFPACK_A, Ap,
					     Ai, Ax, Xx, Bx, Numeric, control, 
					     info);
		  int status2 = UMFPACK_DNAME (solve) (UMFPACK_A,
						  Ap, Ai, Ax, Xz, Bz, Numeric, 
						  control, info) ;

		  if (status < 0 || status2 < 0)
		    {
		      (*current_liboctave_error_handler) 
			("SparseMatrix::solve solve failed");

		      UMFPACK_DNAME (report_status) (control, status);
		      
		      err = -1;

		      break;
		    }

		  for (octave_idx_type i = 0; i < b_nr; i++)
		    {
		      Complex tmp = Complex (Xx[i], Xz[i]);
		      if (tmp != 0.0)
			{
			  if (ii == x_nz)
			    {
			      // Resize the sparse matrix
			      octave_idx_type sz = x_nz * (b_nc - j) / b_nc;
			      sz = (sz > 10 ? sz : 10) + x_nz;
			      retval.change_capacity (sz);
			      x_nz = sz;
			    }
			  retval.xdata(ii) = tmp;
			  retval.xridx(ii++) = i;
			}
		    }
		  retval.xcidx(j+1) = ii;
		}

	      retval.maybe_compress ();

	      UMFPACK_DNAME (report_info) (control, info);

	      UMFPACK_DNAME (free_numeric) (&Numeric);
	    }
	  else
	    mattype.mark_as_rectangular ();
#else
	  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
	}
      else if (typ != SparseType::Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }
  
  return retval;
}

Matrix
SparseMatrix::solve (SparseType &mattype, const Matrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

Matrix
SparseMatrix::solve (SparseType &mattype, const Matrix& b, 
		     octave_idx_type& info) const
{
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

Matrix
SparseMatrix::solve (SparseType &mattype, const Matrix& b, octave_idx_type& info, 
		     double& rcond) const
{
  return solve (mattype, b, info, rcond, 0);
}

Matrix
SparseMatrix::solve (SparseType &mattype, const Matrix& b, octave_idx_type& err, 
		     double& rcond, solve_singularity_handler sing_handler,
		     bool singular_fallback) const
{
  Matrix retval;
  int typ = mattype.type (false);

  if (typ == SparseType::Unknown)
    typ = mattype.type (*this);

  // Only calculate the condition number for CHOLMOD/UMFPACK
  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    retval = dsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    retval = utsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    retval = ltsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Banded || typ == SparseType::Banded_Hermitian)
    retval = bsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Tridiagonal || 
	   typ == SparseType::Tridiagonal_Hermitian)
    retval = trisolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Full || typ == SparseType::Hermitian)
    retval = fsolve (mattype, b, err, rcond, sing_handler, true);
  else if (typ != SparseType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return Matrix ();
    }

  // Rectangular or one of the above solvers flags a singular matrix
  if (singular_fallback && mattype.type (false) == SparseType::Rectangular)
    {
      rcond = 1.;
#ifdef USE_QRSOLVE
      retval = qrsolve (*this, b, err);
#else
      retval = dmsolve<Matrix, SparseMatrix, Matrix> (*this, b, err);
#endif
    }

  return retval;
}

SparseMatrix
SparseMatrix::solve (SparseType &mattype, const SparseMatrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

SparseMatrix
SparseMatrix::solve (SparseType &mattype, const SparseMatrix& b, 
		     octave_idx_type& info) const
{
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

SparseMatrix
SparseMatrix::solve (SparseType &mattype, const SparseMatrix& b,
		     octave_idx_type& info, double& rcond) const
{
  return solve (mattype, b, info, rcond, 0);
}

SparseMatrix
SparseMatrix::solve (SparseType &mattype, const SparseMatrix& b, 
		     octave_idx_type& err, double& rcond,
		     solve_singularity_handler sing_handler,
		     bool singular_fallback) const
{
  SparseMatrix retval;
  int typ = mattype.type (false);

  if (typ == SparseType::Unknown)
    typ = mattype.type (*this);

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    retval = dsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    retval = utsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    retval = ltsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Banded || typ == SparseType::Banded_Hermitian)
    retval = bsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Tridiagonal || 
	   typ == SparseType::Tridiagonal_Hermitian)
    retval = trisolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Full || typ == SparseType::Hermitian)
    retval = fsolve (mattype, b, err, rcond, sing_handler, true);
  else if (typ != SparseType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return SparseMatrix ();
    }

  if (singular_fallback && mattype.type (false) == SparseType::Rectangular)
    {
      rcond = 1.;
#ifdef USE_QRSOLVE
      retval = qrsolve (*this, b, err);
#else
      retval = dmsolve<SparseMatrix, SparseMatrix, 
	SparseMatrix> (*this, b, err);
#endif
    }

  return retval;
}

ComplexMatrix
SparseMatrix::solve (SparseType &mattype, const ComplexMatrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

ComplexMatrix
SparseMatrix::solve (SparseType &mattype, const ComplexMatrix& b, 
			    octave_idx_type& info) const
{
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

ComplexMatrix
SparseMatrix::solve (SparseType &mattype, const ComplexMatrix& b, 
		     octave_idx_type& info, double& rcond) const
{
  return solve (mattype, b, info, rcond, 0);
}

ComplexMatrix
SparseMatrix::solve (SparseType &mattype, const ComplexMatrix& b, 
		     octave_idx_type& err, double& rcond, 
		     solve_singularity_handler sing_handler,
		     bool singular_fallback) const
{
  ComplexMatrix retval;
  int typ = mattype.type (false);

  if (typ == SparseType::Unknown)
    typ = mattype.type (*this);

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    retval = dsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    retval = utsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    retval = ltsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Banded || typ == SparseType::Banded_Hermitian)
    retval = bsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Tridiagonal || 
	   typ == SparseType::Tridiagonal_Hermitian)
    retval = trisolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Full || typ == SparseType::Hermitian)
    retval = fsolve (mattype, b, err, rcond, sing_handler, true);
  else if (typ != SparseType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return ComplexMatrix ();
    }

  if (singular_fallback && mattype.type(false) == SparseType::Rectangular)
    {
      rcond = 1.;
#ifdef USE_QRSOLVE
      retval = qrsolve (*this, b, err);
#else
      retval = dmsolve<ComplexMatrix, SparseMatrix, 
	ComplexMatrix> (*this, b, err);
#endif
    }

  return retval;
}

SparseComplexMatrix
SparseMatrix::solve (SparseType &mattype, const SparseComplexMatrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

SparseComplexMatrix
SparseMatrix::solve (SparseType &mattype, const SparseComplexMatrix& b, 
		     octave_idx_type& info) const
{
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

SparseComplexMatrix
SparseMatrix::solve (SparseType &mattype, const SparseComplexMatrix& b,
		     octave_idx_type& info, double& rcond) const
{
  return solve (mattype, b, info, rcond, 0);
}

SparseComplexMatrix
SparseMatrix::solve (SparseType &mattype, const SparseComplexMatrix& b, 
		     octave_idx_type& err, double& rcond,
		     solve_singularity_handler sing_handler,
		     bool singular_fallback) const
{
  SparseComplexMatrix retval;
  int typ = mattype.type (false);

  if (typ == SparseType::Unknown)
    typ = mattype.type (*this);

  if (typ == SparseType::Diagonal || typ == SparseType::Permuted_Diagonal)
    retval = dsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Upper || typ == SparseType::Permuted_Upper)
    retval = utsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Lower || typ == SparseType::Permuted_Lower)
    retval = ltsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Banded || typ == SparseType::Banded_Hermitian)
    retval = bsolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Tridiagonal || 
	   typ == SparseType::Tridiagonal_Hermitian)
    retval = trisolve (mattype, b, err, rcond, sing_handler, false);
  else if (typ == SparseType::Full || typ == SparseType::Hermitian)
    retval = fsolve (mattype, b, err, rcond, sing_handler, true);
  else if (typ != SparseType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return SparseComplexMatrix ();
    }

  if (singular_fallback && mattype.type(false) == SparseType::Rectangular)
    {
      rcond = 1.;
#ifdef USE_QRSOLVE
      retval = qrsolve (*this, b, err);
#else
      retval = dmsolve<SparseComplexMatrix, SparseMatrix, 
	SparseComplexMatrix> (*this, b, err);
#endif
    }

  return retval;
}

ColumnVector
SparseMatrix::solve (SparseType &mattype, const ColumnVector& b) const
{
  octave_idx_type info; double rcond;
  return solve (mattype, b, info, rcond);
}

ColumnVector
SparseMatrix::solve (SparseType &mattype, const ColumnVector& b, octave_idx_type& info) const
{
  double rcond;
  return solve (mattype, b, info, rcond);
}

ColumnVector
SparseMatrix::solve (SparseType &mattype, const ColumnVector& b, octave_idx_type& info, double& rcond) const
{
  return solve (mattype, b, info, rcond, 0);
}

ColumnVector
SparseMatrix::solve (SparseType &mattype, const ColumnVector& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  Matrix tmp (b);
  return solve (mattype, tmp, info, rcond, sing_handler).column (static_cast<octave_idx_type> (0));
}

ComplexColumnVector
SparseMatrix::solve (SparseType &mattype, const ComplexColumnVector& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

ComplexColumnVector
SparseMatrix::solve (SparseType &mattype, const ComplexColumnVector& b, octave_idx_type& info) const
{
  double rcond;
  return solve (mattype, b, info, rcond, 0);
}

ComplexColumnVector
SparseMatrix::solve (SparseType &mattype, const ComplexColumnVector& b, octave_idx_type& info, 
		     double& rcond) const
{
  return solve (mattype, b, info, rcond, 0);
}

ComplexColumnVector
SparseMatrix::solve (SparseType &mattype, const ComplexColumnVector& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (b);
  return solve (mattype, tmp, info, rcond, sing_handler).column (static_cast<octave_idx_type> (0));
}

Matrix
SparseMatrix::solve (const Matrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (b, info, rcond, 0);
}

Matrix
SparseMatrix::solve (const Matrix& b, octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

Matrix
SparseMatrix::solve (const Matrix& b, octave_idx_type& info, 
		     double& rcond) const
{
  return solve (b, info, rcond, 0);
}

Matrix
SparseMatrix::solve (const Matrix& b, octave_idx_type& err, 
		     double& rcond, 
		     solve_singularity_handler sing_handler) const
{
  SparseType mattype (*this);
  return solve (mattype, b, err, rcond, sing_handler);
}

SparseMatrix
SparseMatrix::solve (const SparseMatrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (b, info, rcond, 0);
}

SparseMatrix
SparseMatrix::solve (const SparseMatrix& b, 
		     octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

SparseMatrix
SparseMatrix::solve (const SparseMatrix& b,
		     octave_idx_type& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

SparseMatrix
SparseMatrix::solve (const SparseMatrix& b, 
		     octave_idx_type& err, double& rcond,
		     solve_singularity_handler sing_handler) const
{
  SparseType mattype (*this);
  return solve (mattype, b, err, rcond, sing_handler);
}

ComplexMatrix
SparseMatrix::solve (const ComplexMatrix& b, 
			    octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexMatrix
SparseMatrix::solve (const ComplexMatrix& b, 
		     octave_idx_type& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ComplexMatrix
SparseMatrix::solve (const ComplexMatrix& b, 
		     octave_idx_type& err, double& rcond, 
		     solve_singularity_handler sing_handler) const
{
  SparseType mattype (*this);
  return solve (mattype, b, err, rcond, sing_handler);
}

SparseComplexMatrix
SparseMatrix::solve (const SparseComplexMatrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (b, info, rcond, 0);
}

SparseComplexMatrix
SparseMatrix::solve (const SparseComplexMatrix& b, 
		     octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

SparseComplexMatrix
SparseMatrix::solve (const SparseComplexMatrix& b,
		     octave_idx_type& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

SparseComplexMatrix
SparseMatrix::solve (const SparseComplexMatrix& b, 
		     octave_idx_type& err, double& rcond,
		     solve_singularity_handler sing_handler) const
{
  SparseType mattype (*this);
  return solve (mattype, b, err, rcond, sing_handler);
}

ColumnVector
SparseMatrix::solve (const ColumnVector& b) const
{
  octave_idx_type info; double rcond;
  return solve (b, info, rcond);
}

ColumnVector
SparseMatrix::solve (const ColumnVector& b, octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

ColumnVector
SparseMatrix::solve (const ColumnVector& b, octave_idx_type& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ColumnVector
SparseMatrix::solve (const ColumnVector& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  Matrix tmp (b);
  return solve (tmp, info, rcond, sing_handler).column (static_cast<octave_idx_type> (0));
}

ComplexColumnVector
SparseMatrix::solve (const ComplexColumnVector& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexColumnVector
SparseMatrix::solve (const ComplexColumnVector& b, octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexColumnVector
SparseMatrix::solve (const ComplexColumnVector& b, octave_idx_type& info, 
		     double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ComplexColumnVector
SparseMatrix::solve (const ComplexColumnVector& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (b);
  return solve (tmp, info, rcond, sing_handler).column (static_cast<octave_idx_type> (0));
}

// other operations.

SparseMatrix
SparseMatrix::map (d_d_Mapper f) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz = nnz ();
  bool f_zero = (f(0.0) == 0.0);

  // Count number of non-zero elements
  octave_idx_type nel = (f_zero ? 0 : nr*nc - nz);
  for (octave_idx_type i = 0; i < nz; i++)
    if (f (data(i)) != 0.0)
      nel++;

  SparseMatrix retval (nr, nc, nel);

  if (f_zero)
    {
      octave_idx_type ii = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      double tmp = f (elem (i, j));
	      if (tmp != 0.0)
		{
		  retval.data(ii) = tmp;
		  retval.ridx(ii++) = i;
		}
	    }
	  retval.cidx(j+1) = ii;
	}
    }
  else
    {
      octave_idx_type ii = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      retval.data(ii) = f (elem(i));
	      retval.ridx(ii++) = ridx(i);
	    }
	  retval.cidx(j+1) = ii;
	}
    }

  return retval;
}

SparseBoolMatrix
SparseMatrix::map (b_d_Mapper f) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz = nnz ();
  bool f_zero = f(0.0);

  // Count number of non-zero elements
  octave_idx_type nel = (f_zero ? 0 : nr*nc - nz);
  for (octave_idx_type i = 0; i < nz; i++)
    if (f (data(i)) != 0.0)
      nel++;

  SparseBoolMatrix retval (nr, nc, nel);

  if (f_zero)
    {
      octave_idx_type ii = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      bool tmp = f (elem (i, j));
	      if (tmp)
		{
		  retval.data(ii) = tmp;
		  retval.ridx(ii++) = i;
		}
	    }
	  retval.cidx(j+1) = ii;
	}
    }
  else
    {
      octave_idx_type ii = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
	    {
	      retval.data(ii) = f (elem(i));
	      retval.ridx(ii++) = ridx(i);
	    }
	  retval.cidx(j+1) = ii;
	}
    }

  return retval;
}

SparseMatrix&
SparseMatrix::apply (d_d_Mapper f)
{
  *this = map (f);
  return *this;
}

bool
SparseMatrix::any_element_is_negative (bool neg_zero) const
{
  octave_idx_type nel = nnz ();

  if (neg_zero)
    {
      for (octave_idx_type i = 0; i < nel; i++)
	if (lo_ieee_signbit (data (i)))
	  return true;
    }
  else
    {
      for (octave_idx_type i = 0; i < nel; i++)
	if (data (i) < 0)
	  return true;
    }

  return false;
}

bool
SparseMatrix::any_element_is_inf_or_nan (void) const
{
  octave_idx_type nel = nnz ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = data (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }

  return false;
}

bool
SparseMatrix::all_elements_are_int_or_inf_or_nan (void) const
{
  octave_idx_type nel = nnz ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = data (i);
      if (xisnan (val) || D_NINT (val) == val)
	continue;
      else
	return false;
    }

  return true;
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

bool
SparseMatrix::all_integers (double& max_val, double& min_val) const
{
  octave_idx_type nel = nnz ();

  if (nel == 0)
    return false;

  max_val = data (0);
  min_val = data (0);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = data (i);

      if (val > max_val)
	max_val = val;

      if (val < min_val)
	min_val = val;

      if (D_NINT (val) != val)
	return false;
    }

  return true;
}

bool
SparseMatrix::too_large_for_float (void) const
{
  octave_idx_type nel = nnz ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = data (i);

      if (val > FLT_MAX || val < FLT_MIN)
	return true;
    }

  return false;
}

SparseBoolMatrix 
SparseMatrix::operator ! (void) const 
{ 
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz1 = nnz ();
  octave_idx_type nz2 = nr*nc - nz1;
   
  SparseBoolMatrix r (nr, nc, nz2);
   
  octave_idx_type ii = 0;
  octave_idx_type jj = 0;
  r.cidx (0) = 0;
  for (octave_idx_type i = 0; i < nc; i++)
    {
      for (octave_idx_type j = 0; j < nr; j++)
	{
	  if (jj < cidx(i+1) && ridx(jj) == j)
	    jj++;
	  else
	    {
	      r.data(ii) = true;
	      r.ridx(ii++) = j;
	    }
	}
      r.cidx (i+1) = ii;
    }

  return r;
}

// XXX FIXME XXX Do these really belong here?  Maybe they should be
// in a base class?

SparseBoolMatrix
SparseMatrix::all (int dim) const
{
  SPARSE_ALL_OP (dim);
}

SparseBoolMatrix
SparseMatrix::any (int dim) const
{
  SPARSE_ANY_OP (dim);
}

SparseMatrix
SparseMatrix::cumprod (int dim) const
{
  SPARSE_CUMPROD (SparseMatrix, double, cumprod);
}

SparseMatrix
SparseMatrix::cumsum (int dim) const
{
  SPARSE_CUMSUM (SparseMatrix, double, cumsum);
}

SparseMatrix
SparseMatrix::prod (int dim) const
{
  SPARSE_REDUCTION_OP (SparseMatrix, double, *=, 1.0, 1.0);
}

SparseMatrix
SparseMatrix::sum (int dim) const
{
  SPARSE_REDUCTION_OP (SparseMatrix, double, +=, 0.0, 0.0);
}

SparseMatrix
SparseMatrix::sumsq (int dim) const
{
#define ROW_EXPR \
  double d = elem (i, j); \
  tmp[i] += d * d

#define COL_EXPR \
  double d = elem (i, j); \
  tmp[j] += d * d

  SPARSE_BASE_REDUCTION_OP (SparseMatrix, double, ROW_EXPR, COL_EXPR, 
			    0.0, 0.0);

#undef ROW_EXPR
#undef COL_EXPR
}

SparseMatrix
SparseMatrix::abs (void) const
{
  octave_idx_type nz = nnz ();

  SparseMatrix retval (*this);

  for (octave_idx_type i = 0; i < nz; i++)
    retval.data(i) = fabs(retval.data(i));

  return retval;
}

SparseMatrix
SparseMatrix::diag (octave_idx_type k) const
{
  octave_idx_type nnr = rows ();
  octave_idx_type nnc = cols ();

  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  SparseMatrix d;

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
      
      d = SparseMatrix (ndiag, 1, nel);
      d.xcidx (0) = 0;
      d.xcidx (1) = nel;

      octave_idx_type ii = 0;
      if (k > 0)
	{
	  for (octave_idx_type i = 0; i < ndiag; i++)
	    {
	      double tmp = elem (i, i+k);
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
	      double tmp = elem (i-k, i);
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
	      double tmp = elem (i, i);
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

  return d;
}

Matrix
SparseMatrix::matrix_value (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  Matrix retval (nr, nc, 0.0);
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
      retval.elem (ridx(i), j) = data (i);

  return retval;
}

std::ostream&
operator << (std::ostream& os, const SparseMatrix& a)
{
  octave_idx_type nc = a.cols ();

   // add one to the printed indices to go from
   //  zero-based to one-based arrays
   for (octave_idx_type j = 0; j < nc; j++)  {
      OCTAVE_QUIT;
      for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++) {
	os << a.ridx(i) + 1 << " "  << j + 1 << " ";
	octave_write_double (os, a.data(i));
	os << "\n";
      }
   }

  return os;
}

std::istream&
operator >> (std::istream& is, SparseMatrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nzmax ();

  if (nr < 1 || nc < 1)
    is.clear (std::ios::badbit);
  else
    {
      octave_idx_type itmp, jtmp, jold = 0;
      double tmp;
      octave_idx_type ii = 0;
       
      a.cidx (0) = 0;
      for (octave_idx_type i = 0; i < nz; i++)
	{
	  is >> itmp;
	  itmp--;
	  is >> jtmp;
	  jtmp--;
	  tmp = octave_read_double (is);
	  
	  if (is)
	    {
	      if (jold != jtmp)
		{
		  for (octave_idx_type j = jold; j < jtmp; j++)
		    a.cidx(j+1) = ii;
		  
		  jold = jtmp;
		}
	      a.data (ii) = tmp;
	      a.ridx (ii++) = itmp;
	    }
	  else
	    goto done;
	}

      for (octave_idx_type j = jold; j < nc; j++)
	a.cidx(j+1) = ii;
    }
  
 done:

  return is;
}

SparseMatrix
SparseMatrix::squeeze (void) const 
{ 
  return MSparse<double>::squeeze (); 
}

SparseMatrix
SparseMatrix::index (idx_vector& i, int resize_ok) const 
{ 
  return MSparse<double>::index (i, resize_ok); 
}

SparseMatrix
SparseMatrix::index (idx_vector& i, idx_vector& j, int resize_ok) const 
{ 
  return MSparse<double>::index (i, j, resize_ok); 
}
  
SparseMatrix
SparseMatrix::index (Array<idx_vector>& ra_idx, int resize_ok) const 
{ 
  return MSparse<double>::index (ra_idx, resize_ok); 
}

SparseMatrix
SparseMatrix::reshape (const dim_vector& new_dims) const
{
  return MSparse<double>::reshape (new_dims);
}

SparseMatrix
SparseMatrix::permute (const Array<octave_idx_type>& vec, bool inv) const
{
  return MSparse<double>::permute (vec, inv);
}

SparseMatrix
SparseMatrix::ipermute (const Array<octave_idx_type>& vec) const
{
  return MSparse<double>::ipermute (vec);
}

// matrix by matrix -> matrix operations

SparseMatrix
operator * (const SparseMatrix& m, const SparseMatrix& a)
{
  SPARSE_SPARSE_MUL (SparseMatrix, double, double);
}

Matrix
operator * (const Matrix& m, const SparseMatrix& a)
{
  FULL_SPARSE_MUL (Matrix, double, 0.);
}

Matrix
operator * (const SparseMatrix& m, const Matrix& a)
{
  SPARSE_FULL_MUL (Matrix, double, 0.);
}

// XXX FIXME XXX -- it would be nice to share code among the min/max
// functions below.

#define EMPTY_RETURN_CHECK(T) \
  if (nr == 0 || nc == 0) \
    return T (nr, nc);

SparseMatrix
min (double d, const SparseMatrix& m)
{
  SparseMatrix result;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (SparseMatrix);

  // Count the number of non-zero elements
  if (d < 0.)
    {
      result = SparseMatrix (nr, nc, d);
      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
	  {
	    double tmp = xmin (d, m.data (i));
	    if (tmp != 0.)
	      {
		octave_idx_type idx = m.ridx(i) + j * nr;
		result.xdata(idx) = tmp;
		result.xridx(idx) = m.ridx(i);
	      }
	  }
    }
  else
    {
      octave_idx_type nel = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
	  if (xmin (d, m.data (i)) != 0.)
	    nel++;

      result = SparseMatrix (nr, nc, nel);

      octave_idx_type ii = 0;
      result.xcidx(0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
	    {
	      double tmp = xmin (d, m.data (i));

	      if (tmp != 0.)
		{
		  result.xdata(ii) = tmp;
		  result.xridx(ii++) = m.ridx(i);
		}
	    }
	  result.xcidx(j+1) = ii;
	}
    }

  return result;
}

SparseMatrix
min (const SparseMatrix& m, double d)
{
  return min (d, m);
}

SparseMatrix
min (const SparseMatrix& a, const SparseMatrix& b)
{
  SparseMatrix r;

  if ((a.rows() == b.rows()) && (a.cols() == b.cols())) 
    {
      octave_idx_type a_nr = a.rows ();
      octave_idx_type a_nc = a.cols ();

      octave_idx_type b_nr = b.rows ();
      octave_idx_type b_nc = b.cols ();

      if (a_nr != b_nr || a_nc != b_nc)
	gripe_nonconformant ("min", a_nr, a_nc, b_nr, b_nc);
      else
	{
	  r = SparseMatrix (a_nr, a_nc, (a.nnz () + b.nnz ()));
       
	  octave_idx_type jx = 0;
	  r.cidx (0) = 0;
	  for (octave_idx_type i = 0 ; i < a_nc ; i++)
	    {
	      octave_idx_type  ja = a.cidx(i);
	      octave_idx_type  ja_max = a.cidx(i+1);
	      bool ja_lt_max= ja < ja_max;
           
	      octave_idx_type  jb = b.cidx(i);
	      octave_idx_type  jb_max = b.cidx(i+1);
	      bool jb_lt_max = jb < jb_max;
           
	      while (ja_lt_max || jb_lt_max )
		{
		  OCTAVE_QUIT;
		  if ((! jb_lt_max) ||
                      (ja_lt_max && (a.ridx(ja) < b.ridx(jb))))
		    {
		      double tmp = xmin (a.data(ja), 0.);
		      if (tmp != 0.)
			{
			  r.ridx(jx) = a.ridx(ja);
			  r.data(jx) = tmp;
			  jx++;
			}
		      ja++;
		      ja_lt_max= ja < ja_max;
		    }
		  else if (( !ja_lt_max ) ||
			   (jb_lt_max && (b.ridx(jb) < a.ridx(ja)) ) )
		    {
		      double tmp = xmin (0., b.data(jb));
		      if (tmp != 0.)
			{
			  r.ridx(jx) = b.ridx(jb);
			  r.data(jx) = tmp;
			  jx++;
			}
		      jb++;
		      jb_lt_max= jb < jb_max;
		    }
		  else
		    {
		      double tmp = xmin (a.data(ja), b.data(jb));
		      if (tmp != 0.)
			{
                          r.data(jx) = tmp;
                          r.ridx(jx) = a.ridx(ja);
                          jx++;
			}
		      ja++;
		      ja_lt_max= ja < ja_max;
		      jb++;
		      jb_lt_max= jb < jb_max;
		    }
		}
	      r.cidx(i+1) = jx;
	    }
	  
	  r.maybe_compress ();
	}
    }
  else
    (*current_liboctave_error_handler) ("matrix size mismatch");

  return r;
}

SparseMatrix
max (double d, const SparseMatrix& m)
{
  SparseMatrix result;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (SparseMatrix);

  // Count the number of non-zero elements
  if (d > 0.)
    {
      result = SparseMatrix (nr, nc, d);
      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
	  {
	    double tmp = xmax (d, m.data (i));

	    if (tmp != 0.)
	      {
		octave_idx_type idx = m.ridx(i) + j * nr;
		result.xdata(idx) = tmp;
		result.xridx(idx) = m.ridx(i);
	      }
	  }
    }
  else
    {
      octave_idx_type nel = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
	  if (xmax (d, m.data (i)) != 0.)
	    nel++;

      result = SparseMatrix (nr, nc, nel);

      octave_idx_type ii = 0;
      result.xcidx(0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
	    {
	      double tmp = xmax (d, m.data (i));
	      if (tmp != 0.)
		{
		  result.xdata(ii) = tmp;
		  result.xridx(ii++) = m.ridx(i);
		}
	    }
	  result.xcidx(j+1) = ii;
	}
    }

  return result;
}

SparseMatrix
max (const SparseMatrix& m, double d)
{
  return max (d, m);
}

SparseMatrix
max (const SparseMatrix& a, const SparseMatrix& b)
{
  SparseMatrix r;

  if ((a.rows() == b.rows()) && (a.cols() == b.cols())) 
    {
      octave_idx_type a_nr = a.rows ();
      octave_idx_type a_nc = a.cols ();

      octave_idx_type b_nr = b.rows ();
      octave_idx_type b_nc = b.cols ();

      if (a_nr != b_nr || a_nc != b_nc)
	gripe_nonconformant ("min", a_nr, a_nc, b_nr, b_nc);
      else
	{
	  r = SparseMatrix (a_nr, a_nc, (a.nnz () + b.nnz ()));
       
	  octave_idx_type jx = 0;
	  r.cidx (0) = 0;
	  for (octave_idx_type i = 0 ; i < a_nc ; i++)
	    {
	      octave_idx_type  ja = a.cidx(i);
	      octave_idx_type  ja_max = a.cidx(i+1);
	      bool ja_lt_max= ja < ja_max;
           
	      octave_idx_type  jb = b.cidx(i);
	      octave_idx_type  jb_max = b.cidx(i+1);
	      bool jb_lt_max = jb < jb_max;
           
	      while (ja_lt_max || jb_lt_max )
		{
		  OCTAVE_QUIT;
		  if ((! jb_lt_max) ||
                      (ja_lt_max && (a.ridx(ja) < b.ridx(jb))))
		    {
		      double tmp = xmax (a.data(ja), 0.);
		      if (tmp != 0.)
			{
			  r.ridx(jx) = a.ridx(ja);
			  r.data(jx) = tmp;
			  jx++;
			}
		      ja++;
		      ja_lt_max= ja < ja_max;
		    }
		  else if (( !ja_lt_max ) ||
			   (jb_lt_max && (b.ridx(jb) < a.ridx(ja)) ) )
		    {
		      double tmp = xmax (0., b.data(jb));
		      if (tmp != 0.)
			{
			  r.ridx(jx) = b.ridx(jb);
			  r.data(jx) = tmp;
			  jx++;
			}
		      jb++;
		      jb_lt_max= jb < jb_max;
		    }
		  else
		    {
		      double tmp = xmax (a.data(ja), b.data(jb));
		      if (tmp != 0.)
			{
                          r.data(jx) = tmp;
                          r.ridx(jx) = a.ridx(ja);
                          jx++;
			}
		      ja++;
		      ja_lt_max= ja < ja_max;
		      jb++;
		      jb_lt_max= jb < jb_max;
		    }
		}
	      r.cidx(i+1) = jx;
	    }
	  
	  r.maybe_compress ();
	}
    }
  else
    (*current_liboctave_error_handler) ("matrix size mismatch");

  return r;
}

SPARSE_SMS_CMP_OPS (SparseMatrix, 0.0, , double, 0.0, )
SPARSE_SMS_BOOL_OPS (SparseMatrix, double, 0.0)

SPARSE_SSM_CMP_OPS (double, 0.0, , SparseMatrix, 0.0, )
SPARSE_SSM_BOOL_OPS (double, SparseMatrix, 0.0)

SPARSE_SMSM_CMP_OPS (SparseMatrix, 0.0, , SparseMatrix, 0.0, )
SPARSE_SMSM_BOOL_OPS (SparseMatrix, SparseMatrix, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
