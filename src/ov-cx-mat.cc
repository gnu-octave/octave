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
#include "mx-base.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

octave_allocator
octave_complex_matrix::allocator (sizeof (octave_complex_matrix));

int
octave_complex_matrix::t_id (-1);

const string
octave_complex_matrix::t_name ("complex matrix");

octave_complex_matrix::octave_complex_matrix (const ComplexRowVector& v,
					      int pcv)
  : octave_base_value (),
    matrix ((pcv < 0 && Vprefer_column_vectors) || pcv
	    ? ComplexMatrix (v.transpose ()) : ComplexMatrix (v)) { }

octave_complex_matrix::octave_complex_matrix (const ComplexColumnVector& v,
					      int pcv)
  : octave_base_value (),
    matrix ((pcv < 0 && Vprefer_column_vectors) || pcv
	    ? ComplexMatrix (v) : ComplexMatrix (v.transpose ())) { }

octave_value *
octave_complex_matrix::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  int nr = matrix.rows ();
  int nc = matrix.cols ();

  if (nr == 1 && nc == 1)
    {
      Complex c = matrix (0, 0);

      if (imag (c) == 0.0)
	retval = new octave_scalar (::real (c));
      else
	retval = new octave_complex (c);
    }
  else if (nr == 0 && nc == 0)
    retval = new octave_matrix (Matrix ());
  else if (matrix.all_elements_are_real ())
    retval = new octave_matrix (::real (matrix));

  return retval;
}

octave_value
octave_complex_matrix::index (const octave_value_list& idx) const
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = ComplexMatrix (matrix.index (i, j));
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = ComplexMatrix (matrix.index (i));
      }
      break;

    default:
      error ("invalid number of indices (%d) for complex matrix value", len);
      break;
    }

  return retval;
}

extern void assign (Array2<Complex>&, const Array2<Complex>&);

void
octave_complex_matrix::assign (const octave_value_list& idx,
			       const ComplexMatrix& rhs)
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

extern void assign (Array2<Complex>&, const Array2<double>&);

void
octave_complex_matrix::assign (const octave_value_list& idx,
			       const Matrix& rhs)
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
octave_complex_matrix::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

bool
octave_complex_matrix::valid_as_zero_index (void) const
{
  // XXX FIXME XXX
  return false;
}

bool
octave_complex_matrix::is_true (void) const
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
octave_complex_matrix::double_value (bool force_conversion) const
{
  double retval = octave_NaN;

  int flag = force_conversion;

  if (! flag)
    flag = Vok_to_lose_imaginary_part;

  if (flag < 0)
    gripe_implicit_conversion ("complex matrix", "real scalar");

  if (flag)
    {
      if ((rows () == 1 && columns () == 1)
	  || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
	retval = ::real (matrix (0, 0));
      else
	gripe_invalid_conversion ("complex matrix", "real scalar");
    }
  else
    gripe_invalid_conversion ("complex matrix", "real scalar");

  return retval;
}

Matrix
octave_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  int flag = force_conversion;

  if (! flag)
    flag = Vok_to_lose_imaginary_part;

  if (flag < 0)
    gripe_implicit_conversion ("complex matrix", "real matrix");

  if (flag)
    retval = ::real (matrix);
  else
    gripe_invalid_conversion ("complex matrix", "real matrix");

  return retval;
}

Complex
octave_complex_matrix::complex_value (bool) const
{
  Complex retval (octave_NaN, octave_NaN);

  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("complex matrix", "complex scalar");

  return retval;
}

ComplexMatrix
octave_complex_matrix::complex_matrix_value (bool) const
{
  return matrix;
}

void
octave_complex_matrix::print (ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_complex_matrix::print_raw (ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

bool
octave_complex_matrix::print_name_tag (ostream& os, const string& name) const
{
  bool retval = false;

  int nr = rows ();
  int nc = columns ();

  indent (os);

  if (nr == 1 && nc == 1 || (nr == 0 || nc == 0))
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      newline (os);
      retval = true;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
