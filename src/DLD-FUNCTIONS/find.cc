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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

static octave_value_list
find_to_fortran_idx (const ColumnVector i_idx, const ColumnVector j_idx,
		     const octave_value& val, int nr, int nargout)
{
  octave_value_list retval;

  switch (nargout)
    {
    case 0:
    case 1:
      {
	int count = i_idx.length ();
	ColumnVector tmp (count);
	for (int i = 0; i < count; i++)
	  tmp (i) = nr * (j_idx (i) - 1.0) + i_idx (i);

	// If the original argument was a row vector, force a row
	// vector of indices to be returned.

	retval(0) = octave_value (tmp, (nr != 1));
      }
      break;

    case 3:
      retval(2) = val;
      // Fall through!

    case 2:
      retval(1) = octave_value (j_idx, 1);
      retval(0) = octave_value (i_idx, 1);

      // If you want this to work more like Matlab, use
      //
      //    retval(0) = octave_value (i_idx, (nr != 1));
      //
      // instead of the previous statement.

      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

static octave_value_list
find_nonzero_elem_idx (const Matrix& m, int nargout)
{
  int count = 0;
  int m_nr = m.rows ();
  int m_nc = m.columns ();

  int i, j;
  for (j = 0; j < m_nc; j++)
    for (i = 0; i < m_nr; i++)
      if (m (i, j) != 0.0)
	count++;

  octave_value_list retval (((nargout == 0) ? 1 : nargout), Matrix ());

  if (count == 0)
    return retval;

  ColumnVector i_idx (count);
  ColumnVector j_idx (count);
  ColumnVector v (count);

  count = 0;
  for (j = 0; j < m_nc; j++)
    for (i = 0; i < m_nr; i++)
      {
	double d = m (i, j);
	if (d != 0.0)
	  {
	    i_idx (count) = i + 1;
	    j_idx (count) = j + 1;
	    v (count) = d;
	    count++;
	  }
      }

  octave_value tmp (v, 1);
  return find_to_fortran_idx (i_idx, j_idx, tmp, m_nr, nargout);
}

static octave_value_list
find_nonzero_elem_idx (const ComplexMatrix& m, int nargout)
{
  int count = 0;
  int m_nr = m.rows ();
  int m_nc = m.columns ();

  int i, j;
  for (j = 0; j < m_nc; j++)
    for (i = 0; i < m_nr; i++)
      if (m (i, j) != 0.0)
	count++;

  octave_value_list retval (((nargout == 0) ? 1 : nargout), Matrix ());

  if (count == 0)
    return retval;

  ColumnVector i_idx (count);
  ColumnVector j_idx (count);
  ComplexColumnVector v (count);

  count = 0;
  for (j = 0; j < m_nc; j++)
    for (i = 0; i < m_nr; i++)
      {
	Complex c = m (i, j);
	if (c != 0.0)
	  {
	    i_idx (count) = i + 1;
	    j_idx (count) = j + 1;
	    v (count) = c;
	    count++;
	  }
      }

  octave_value tmp (v, 1);
  return find_to_fortran_idx (i_idx, j_idx, tmp, m_nr, nargout);
}

DEFUN_DLD (find, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} find (@var{x})\n\
Return a vector of indices of nonzero elements of a matrix.  To obtain a\n\
single index for each matrix element, Octave pretends that the columns\n\
of a matrix form one long vector (like Fortran arrays are stored).  For\n\
example,\n\
\n\
@example\n\
@group\n\
find (eye (2))\n\
     @result{} [ 1; 4 ]\n\
@end group\n\
@end example\n\
\n\
If two outputs are requested, @code{find} returns the row and column\n\
indices of nonzero elements of a matrix.  For example,\n\
\n\
@example\n\
@group\n\
[i, j] = find (2 * eye (2))\n\
     @result{} i = [ 1; 2 ]\n\
     @result{} j = [ 1; 2 ]\n\
@end group\n\
@end example\n\
\n\
If three outputs are requested, @code{find} also returns a vector\n\
containing the nonzero values.  For example,\n\
\n\
@example\n\
@group\n\
[i, j, v] = find (3 * eye (2))\n\
     @result{} i = [ 1; 2 ]\n\
     @result{} j = [ 1; 2 ]\n\
     @result{} v = [ 3; 3 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 3)
    {
      print_usage ("find");
      return retval;
    }

  octave_value arg = args(0);

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (m, nargout);
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	retval = find_nonzero_elem_idx (m, nargout);
    }
  else
    {
      gripe_wrong_type_arg ("find", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
