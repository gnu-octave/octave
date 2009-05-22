/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2006,
              2007, 2008 John W. Eaton
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "Cell.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "pr-output.h"

template <class MT>
octave_value
octave_base_matrix<MT>::subsref (const std::string& type,
				 const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

template <class MT>
octave_value
octave_base_matrix<MT>::subsasgn (const std::string& type,
				  const std::list<octave_value_list>& idx,
				  const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
	if (type.length () == 1)
	  retval = numeric_assign (type, idx, rhs);
	else if (is_empty ())
	  {
	    // Allow conversion of empty matrix to some other type in
	    // cases like
	    //
	    //  x = []; x(i).f = rhs

	    if (type[1] == '.')
	      {
		octave_value tmp = octave_value::empty_conv (type, rhs);

		retval = tmp.subsasgn (type, idx, rhs);
	      }
	    else
	      error ("invalid assignment expression");
	  }
	else
	  {
	    std::string nm = type_name ();
	    error ("in indexed assignment of %s, last lhs index must be ()",
		   nm.c_str ());
	  }
      }
      break;

    case '{':
    case '.':
      {
	if (is_empty ())
	  {
	    octave_value tmp = octave_value::empty_conv (type, rhs);

	    retval = tmp.subsasgn (type, idx, rhs);
	  }
	else
	  {
	    std::string nm = type_name ();
	    error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	  }
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <class MT>
octave_value
octave_base_matrix<MT>::do_index_op (const octave_value_list& idx,
				     bool resize_ok)
{
  octave_value retval;

  octave_idx_type n_idx = idx.length ();

  int nd = matrix.ndims ();

  switch (n_idx)
    {
    case 0:
      retval = matrix;
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	if (! error_state)
          {
            // optimize single scalar index.
            if (i.is_scalar () && i(0) < matrix.numel ())
              retval = const_cast<const MT&> (matrix)(i(0));
            else
              retval = MT (matrix.index (i, resize_ok));
          }
      }
      break;

    case 2:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx (1).index_vector ();

            if (! error_state)
              {
                // optimize two scalar indices.
                if (i.is_scalar () && j.is_scalar () && nd == 2
                    && i(0) < matrix.rows () && j(0) < matrix.columns ())
                  retval = const_cast<const MT&> (matrix)(i(0), j(0));
                else
                  retval = MT (matrix.index (i, j, resize_ok));
              }
          }
      }
      break;

    default:
      {
        Array<idx_vector> idx_vec (n_idx);
        bool scalar_opt = n_idx == nd;
        const dim_vector dv = matrix.dims ();

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            idx_vec(i) = idx(i).index_vector ();

            if (error_state)
              break;

            scalar_opt = (scalar_opt && idx_vec(i).is_scalar ()
                          && idx_vec(i)(0) < dv(0));
          }

        if (! error_state)
          {
            if (scalar_opt)
              {
                // optimize all scalar indices. Don't construct an index array,
                // but rather calc a scalar index directly.
                octave_idx_type k = 1, j = 0;
                for (octave_idx_type i = 0; i < n_idx; i++)
                  {
                    j += idx_vec(i)(0) * k;
                    k *= dv (i);
                  }
                retval = const_cast<const MT&> (matrix)(j);
              }
            else
              retval = MT (matrix.index (idx_vec, resize_ok));
          }
      }
      break;
    }

  return retval;
}

template <class MT>
void
octave_base_matrix<MT>::assign (const octave_value_list& idx, const MT& rhs)
{
  octave_idx_type n_idx = idx.length ();

  switch (n_idx)
    {
    case 0:
      panic_impossible ();
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	if (! error_state)
          matrix.assign (i, rhs);
      }
      break;

    case 2:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx (1).index_vector ();

            if (! error_state)
              matrix.assign (i, j, rhs);
          }
      }
      break;

    default:
      {
        Array<idx_vector> idx_vec (n_idx);

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            idx_vec(i) = idx(i).index_vector ();

            if (error_state)
              break;
          }

        if (! error_state)
          matrix.assign (idx_vec, rhs);
      }
      break;
    }

  // Invalidate the matrix type
  typ.invalidate_type ();
}

template <class MT>
void
octave_base_matrix<MT>::assign (const octave_value_list& idx,
                                typename MT::element_type rhs)
{
  octave_idx_type n_idx = idx.length ();

  int nd = matrix.ndims ();

  MT mrhs (dim_vector (1), rhs);

  switch (n_idx)
    {
    case 0:
      panic_impossible ();
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	if (! error_state)
          {
            // optimize single scalar index.
            if (i.is_scalar () && i(0) < matrix.numel ())
              matrix(i(0)) = rhs;
            else
              matrix.assign (i, mrhs);
          }
      }
      break;

    case 2:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx (1).index_vector ();

            if (! error_state)
              {
                // optimize two scalar indices.
                if (i.is_scalar () && j.is_scalar () && nd == 2
                    && i(0) < matrix.rows () && j(0) < matrix.columns ())
                  matrix(i(0), j(0)) = rhs;
                else
                  matrix.assign (i, j, mrhs);
              }
          }
      }
      break;

    default:
      {
        Array<idx_vector> idx_vec (n_idx);
        bool scalar_opt = n_idx == nd;
        const dim_vector dv = matrix.dims ().redim (n_idx);

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            idx_vec(i) = idx(i).index_vector ();

            if (error_state)
              break;

            scalar_opt = (scalar_opt && idx_vec(i).is_scalar ()
                          && idx_vec(i)(0) < dv(i));
          }

        if (! error_state)
          {
            if (scalar_opt)
              {
                // optimize all scalar indices. Don't construct an index array,
                // but rather calc a scalar index directly.
                octave_idx_type k = 1, j = 0;
                for (octave_idx_type i = 0; i < n_idx; i++)
                  {
                    j += idx_vec(i)(0) * k;
                    k *= dv (i);
                  }
                matrix(j) = rhs;
              }
            else
              matrix.assign (idx_vec, mrhs);
          }
      }
      break;
    }

  // Invalidate the matrix type
  typ.invalidate_type ();
}

template <class MT>
void
octave_base_matrix<MT>::delete_elements (const octave_value_list& idx)
{
  octave_idx_type len = idx.length ();

  Array<idx_vector> ra_idx (len);

  for (octave_idx_type i = 0; i < len; i++)
    ra_idx(i) = idx(i).index_vector ();

  matrix.delete_elements (ra_idx);

  // Invalidate the matrix type
  typ.invalidate_type ();
}

template <class MT>
octave_value
octave_base_matrix<MT>::resize (const dim_vector& dv, bool fill) const
{
  MT retval (matrix); 
  if (fill)
    retval.resize (dv, 0);
  else
    retval.resize (dv); 
  return retval;
}

template <class MT>
bool
octave_base_matrix<MT>::is_true (void) const
{
  bool retval = false;
  dim_vector dv = matrix.dims ();
  int nel = dv.numel ();

  if (nel > 0)
    {
      MT t1 (matrix.reshape (dim_vector (nel, 1)));

      if (t1.any_element_is_nan ())
	error ("invalid conversion from NaN to logical");
      else
	{
	  boolNDArray t2 = t1.all ();

	  retval = t2(0);
	}
    }

  return retval;
}

template <class MT>
bool
octave_base_matrix<MT>::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

template <class MT>
void
octave_base_matrix<MT>::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class MT>
void
octave_base_matrix<MT>::print_info (std::ostream& os,
				    const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
