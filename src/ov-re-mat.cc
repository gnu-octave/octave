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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"

#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "pr-output.h"
#include "variables.h"

template class octave_base_matrix<Matrix>;

DEFINE_OCTAVE_ALLOCATOR (octave_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_matrix, "matrix");

octave_matrix::octave_matrix (const RowVector& v, int pcv)
  : octave_base_matrix<Matrix> ((pcv < 0 && Vprefer_column_vectors) || pcv
				? Matrix (v.transpose ()) : Matrix (v)) { }

octave_matrix::octave_matrix (const ColumnVector& v, int pcv)
  : octave_base_matrix<Matrix> ((pcv < 0 && Vprefer_column_vectors) || pcv
				? Matrix (v) : Matrix (v.transpose ())) { }

octave_value *
octave_matrix::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  int nr = matrix.rows ();
  int nc = matrix.cols ();

  if (nr == 1 && nc == 1)
    retval = new octave_scalar (matrix (0, 0));

  return retval;
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern void assign (Array2<double>&, const Array2<double>&);
#endif

void
octave_matrix::assign (const octave_value_list& idx, const Matrix& rhs)
{
  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	matrix.set_index (i);
	matrix.set_index (j);

	::assign (matrix, rhs);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	matrix.set_index (i);

	::assign (matrix, rhs);
      }
      break;

    default:
      error ("invalid number of indices (%d) for indexed matrix assignment",
	     len);
      break;
    }
}

void
octave_matrix::assign_struct_elt (assign_op, const string& nm,
				  const octave_value& rhs)
{
  octave_value retval;

  Matrix m = rhs.matrix_value ();

  if (! error_state)
    {
      int nr = -1;
      int nc = -1;

      int dim = -1;

      if (m.rows () == 1 && m.cols () == 2)
	{
	  nr = NINT (m (0, 0));
	  nc = NINT (m (0, 1));
	}
      else if (m.rows () == 2 && m.cols () == 1)
	{
	  nr = NINT (m (0, 0));
	  nc = NINT (m (1, 0));
	}
      else if (m.rows () == 1 && m.cols () == 1)
	{
	  dim = NINT (m (0, 0));

	  nr = matrix.rows ();
	  nc = matrix.cols ();
	}

      if (nm == "size")
	{
	  if (nr >= 0 && nc >= 0)
	    matrix.resize (nr, nc, 0.0);
	  else
	    error ("invalid size specification = [%d, %d] specified",
		   nr, nc);
	}
      else if (nm == "rows")
	{
	  if (dim >= 0)
	    matrix.resize (dim, nc, 0.0);
	  else
	    error ("invalid row dimension = %d specified", dim);
	}
      else if (nm == "cols" || nm == "columns")
	{
	  if (dim >= 0)
	    matrix.resize (nr, dim, 0.0);
	  else
	    error ("invalid column dimension = %d specified", dim);
	}
    }
}

void
octave_matrix::assign_struct_elt (assign_op, const string&,
				  const octave_value_list&,
				  const octave_value&)
{
  error ("indexed assignment for matrix properties is not implemented");
}

octave_value
octave_matrix::do_struct_elt_index_op (const string& nm,
				       const octave_value_list& idx,
				       bool silent)
{
  // XXX DO_ME XXX
}

octave_value
octave_matrix::do_struct_elt_index_op (const string& nm, bool silent)
{
  octave_value retval;

  double nr = static_cast<double> (matrix.rows ());
  double nc = static_cast<double> (matrix.cols ());

  if (nm == "rows")
    retval = nr;
  else if (nm == "cols" || nm == "columns")
    retval = nc;
  else if (nm == "size")
    {
      Matrix tmp (1, 2);

      tmp.elem (0, 0) = nr;
      tmp.elem (0, 1) = nc;

      retval = tmp;
    }
  else if (! silent)
    error ("structure has no member `%s'", nm.c_str ());

  return retval;
}

octave_lvalue
octave_matrix::struct_elt_ref (octave_value *parent, const string& nm)
{
  return octave_lvalue (parent, nm);
}

bool
octave_matrix::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

double
octave_matrix::double_value (bool) const
{
  double retval = octave_NaN;

  // XXX FIXME XXX -- maybe this should be a function, valid_as_scalar()
  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("real matrix", "real scalar");

  return retval;
}

Complex
octave_matrix::complex_value (bool) const
{
  Complex retval (octave_NaN, octave_NaN);

  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("real matrix", "complex scalar");

  return retval;
}

octave_value
octave_matrix::convert_to_str (void) const
{
  octave_value retval;

  int nr = matrix.rows ();
  int nc = matrix.columns ();

  if (nr == 0 && nc == 0)
    {
      char s = '\0';
      retval = octave_value (&s);
    }
  else
    {
      if (nr == 0 || nc == 0)
	{
	  char s = '\0';
	  retval = octave_value (&s);
	}
      else
	{
	  charMatrix chm (nr, nc);

	  for (int j = 0; j < nc; j++)
	    {
	      for (int i = 0; i < nr; i++)
		{
		  double d = matrix (i, j);

		  if (xisnan (d))
		    {
		      ::error ("invalid conversion from NaN to character");
		      return retval;
		    }
		  else
		    {
		      // XXX FIXME XXX -- warn about out of range
		      // conversions?

		      int ival = NINT (d);
		      chm (i, j) = (char) ival;
		    }
		}
	    }

	  retval = octave_value (chm, 1);
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
