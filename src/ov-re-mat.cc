/*

Copyright (C) 1996 John W. Eaton

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

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"

#include "gripes.h"
#include "mappers.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "pr-output.h"

int octave_matrix::t_id = -1;

const string octave_matrix::t_name ("matrix");

octave_matrix::octave_matrix (const RowVector& v, int pcv)
  : octave_base_value (),
    matrix ((pcv < 0 && Vprefer_column_vectors) || pcv
	    ? Matrix (v.transpose ()) : Matrix (v)) { }

octave_matrix::octave_matrix (const ColumnVector& v, int pcv)
  : octave_base_value (),
    matrix ((pcv < 0 && Vprefer_column_vectors) || pcv
	    ? Matrix (v) : Matrix (v.transpose ())) { }

#include <iostream.h>

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

octave_value
octave_matrix::index (const octave_value_list& idx) const
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = Matrix (matrix.index (i, j));
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = Matrix (matrix.index (i));
      }
      break;

    default:
      error ("invalid number of indices (%d) for matrix value", len);
      break;
    }

  return retval;
}

extern void assign (Array2<double>&, const Array2<double>&);

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

bool
octave_matrix::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

bool
octave_matrix::is_true (void) const
{
  bool retval = false;

  if (rows () == 0 || columns () == 0)
    {
      int flag = Vpropagate_empty_matrices;

      if (flag < 0)
	warning ("empty matrix used in conditional expression");
      else if (flag == 0)
	error ("empty matrix used in conditional expression");
    }
  else
    {
      Matrix m = (matrix.all ()) . all ();

      retval = (m.rows () == 1 && m.columns () == 1 && m (0, 0) != 0.0);
    }

  return retval;
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

void
octave_matrix::print (ostream& os)
{
  octave_print_internal (os, matrix, false, struct_indent);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
