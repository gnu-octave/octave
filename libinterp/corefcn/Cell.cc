////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1999-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "idx-vector.h"

#include "Cell.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

Cell::Cell (const octave_value_list& ovl)
  : Array<octave_value> (ovl.cell_value ())
{ }

Cell::Cell (const string_vector& sv, bool trim)
  : Array<octave_value> ()
{
  octave_idx_type n = sv.numel ();

  if (n > 0)
    {
      resize (dim_vector (n, 1));

      for (octave_idx_type i = 0; i < n; i++)
        {
          std::string s = sv[i];

          if (trim)
            {
              std::size_t pos = s.find_last_not_of (' ');

              s = (pos == std::string::npos) ? "" : s.substr (0, pos+1);
            }

          elem (i, 0) = s;
        }
    }
}

Cell::Cell (const std::list<std::string>& sl)
  : Array<octave_value> ()
{
  octave_idx_type n = sl.size ();

  if (n > 0)
    {
      resize (dim_vector (n, 1));

      octave_value *dst = fortran_vec ();
      auto p = sl.begin ();

      for (octave_idx_type i = 0; i < n; i++)
        dst[i] = *p++;
    }
}

Cell::Cell (const Array<std::string>& sa)
  : Array<octave_value> (sa.dims ())
{
  octave_idx_type n = sa.numel ();

  octave_value *dst = fortran_vec ();
  const std::string *src = sa.data ();

  for (octave_idx_type i = 0; i < n; i++)
    dst[i] = src[i];
}

// Set size to DV, filling with [].  Then fill with as many elements of
// SV as possible.

Cell::Cell (const dim_vector& dv, const string_vector& sv, bool trim)
  : Array<octave_value> (dv, Matrix ())
{
  octave_idx_type n = sv.numel ();

  if (n > 0)
    {
      octave_idx_type m = numel ();

      octave_idx_type len = (n > m ? m : n);

      for (octave_idx_type i = 0; i < len; i++)
        {
          std::string s = sv[i];

          if (trim)
            {
              std::size_t pos = s.find_last_not_of (' ');

              s = (pos == std::string::npos) ? "" : s.substr (0, pos+1);
            }

          elem(i) = s;
        }
    }
}

bool
Cell::iscellstr (void) const
{
  bool retval = true;

  octave_idx_type n = numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (! elem(i).is_string ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

Array<std::string>
Cell::cellstr_value (void) const
{
  Array<std::string> retval (dims ());

  octave_idx_type n = numel ();

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = elem (i).string_value ();

  return retval;
}

string_vector
Cell::string_vector_value (void) const
{
  octave_idx_type n = numel ();

  string_vector retval (n);

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = elem (i).string_value ();

  return retval;
}

Cell
Cell::index (const octave_value_list& idx_arg, bool resize_ok) const
{
  Cell retval;

  octave_idx_type n = idx_arg.length ();

  // If we catch an indexing error in index_vector, we flag an error
  // in index k.  Ensure it is the right value before each idx_vector
  // call.  Same variable as used in for loop in default case.

  octave_idx_type k = 0;

  try
    {
      switch (n)
        {
        case 0:
          warn_empty_index ("cell array");
          retval = *this;
          break;

        case 1:
          {
            octave::idx_vector i = idx_arg(0).index_vector ();

            retval = Array<octave_value>::index (i, resize_ok, Matrix ());
          }
          break;

        case 2:
          {
            octave::idx_vector i = idx_arg(0).index_vector ();

            k = 1;
            octave::idx_vector j = idx_arg(1).index_vector ();

            retval = Array<octave_value>::index (i, j, resize_ok, Matrix ());
          }
          break;

        default:
          {
            Array<octave::idx_vector> iv (dim_vector (n, 1));

            for (k = 0; k < n; k++)
              iv(k) = idx_arg(k).index_vector ();

            retval = Array<octave_value>::index (iv, resize_ok, Matrix ());
          }
          break;
        }
    }
  catch (octave::index_exception& ie)
    {
      // Rethrow to allow more info to be reported later.
      ie.set_pos_if_unset (n, k+1);
      throw;
    }

  return retval;
}

/*
%% This behavior is required for Matlab compatibility.
%!shared a
%! a = {"foo", "bar"};
%!assert (a(), a)
%!error <invalid empty index expression> a{}
*/

void
Cell::assign (const octave_value_list& idx_arg, const Cell& rhs,
              const octave_value& fill_val)

{
  octave_idx_type len = idx_arg.length ();

  Array<octave::idx_vector> ra_idx (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    {
      try
        {
          ra_idx(i) = idx_arg(i).index_vector ();
        }
      catch (octave::index_exception& ie)
        {
          // Rethrow to allow more info to be reported later.
          ie.set_pos (len, i+1);
          throw;
        }
    }

  Array<octave_value>::assign (ra_idx, rhs, fill_val);
}

void
Cell::delete_elements (const octave_value_list& idx_arg)

{
  octave_idx_type len = idx_arg.length ();

  Array<octave::idx_vector> ra_idx (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    try
      {
        ra_idx.xelem (i) = idx_arg(i).index_vector ();
      }
    catch (octave::index_exception& ie)
      {
        // Rethrow to allow more info to be reported later.
        ie.set_pos (len, i+1);
        throw;
      }

  Array<octave_value>::delete_elements (ra_idx);
}

octave_idx_type
Cell::nnz (void) const
{
  err_wrong_type_arg ("nnz", "cell array");
}

/*
%!error <wrong type argument 'cell array'> nnz ({0, 1, 2})
%!error <wrong type argument 'cell array'> nnz (cell ())
%!error <wrong type argument 'cell array'> nnz ({"foo", "bar"})
*/

Cell
Cell::column (octave_idx_type i) const
{
  Cell retval;

  if (ndims () > 2)
    error ("Cell::column: requires 2-D cell array");

  if (i < 0 || i >= cols ())
    error ("invalid column selection");

  octave_idx_type nr = rows ();

  retval.resize (dim_vector (nr, 1));

  for (octave_idx_type j = 0; j < nr; j++)
    retval.xelem (j) = elem (j, i);

  return retval;
}

Cell
Cell::concat (const Cell& rb, const Array<octave_idx_type>& ra_idx)
{
  return insert (rb, ra_idx);
}

Cell&
Cell::insert (const Cell& a, octave_idx_type r, octave_idx_type c)
{
  Array<octave_value>::insert (a, r, c);
  return *this;
}

Cell&
Cell::insert (const Cell& a, const Array<octave_idx_type>& ra_idx)
{
  Array<octave_value>::insert (a, ra_idx);
  return *this;
}

Cell
Cell::map (ctype_mapper fcn) const
{
  Cell retval (dims ());
  octave_value *r = retval.fortran_vec ();

  const octave_value *p = data ();

  for (octave_idx_type i = 0; i < numel (); i++)
    r[i] = ((p++)->*fcn) ();

  return retval;
}

octave_value
Cell::resize_fill_value (void) const
{
  static octave_value rfv = octave_value (Matrix ());
  return rfv;
}

Cell
Cell::diag (octave_idx_type k) const
{
  return Array<octave_value>::diag (k);
}

Cell
Cell::diag (octave_idx_type m, octave_idx_type n) const
{
  return Array<octave_value>::diag (m, n);
}
