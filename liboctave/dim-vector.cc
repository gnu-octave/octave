/*

Copyright (C) 2010 VZLU Prague

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

#include "dim-vector.h"

void
dim_vector::chop_all_singletons (void)
{
  make_unique ();

  int j = 0;
  int l = ndims();

  for (int i = 0; i < l; i++)
    {
      if (rep[i] != 1)
        rep[j++] = rep[i];
    }

  if (j == 1)
    rep[1] = 1;

  ndims () = j > 2 ? j : 2;
}

std::string
dim_vector::str (char sep) const
{
  std::ostringstream buf;

  for (int i = 0; i < length (); i++)
    {
      buf << elem (i);

      if (i < length () - 1)
        buf << sep;
    }

  std::string retval = buf.str ();

  return retval;
}

int
dim_vector::num_ones (void) const
{
  int retval = 0;

  for (int i = 0; i < length (); i++)
    if (elem (i) == 1)
      retval++;

  return retval;
}

octave_idx_type
dim_vector::safe_numel (void) const
{
  octave_idx_type idx_max = std::numeric_limits<octave_idx_type>::max () - 1;
  octave_idx_type n = 1;
  int n_dims = length ();

  for (int i = 0; i < n_dims; i++)
    {
      n *= rep[i];
      if (rep[i] != 0)
        idx_max /= rep[i];
      if (idx_max <= 0)
        throw std::bad_alloc ();
    }

  return n;
}

dim_vector 
dim_vector::squeeze (void) const
{
  dim_vector new_dims = *this;

  bool dims_changed = 1;

  int k = 0;

  for (int i = 0; i < length (); i++)
    {
      if (elem (i) == 1)
        dims_changed = true;
      else
        new_dims(k++) = elem (i);
    }

  if (dims_changed)
    {
      if (k == 0)
        new_dims = dim_vector (1, 1);
      else if (k == 1)
        {
          // There is one non-singleton dimension, so we need
          // to decide the correct orientation.

          if (elem (0) == 1)
            {
              // The original dimension vector had a leading
              // singleton dimension.

              octave_idx_type tmp = new_dims(0);

              new_dims.resize (2);

              new_dims(0) = 1;
              new_dims(1) = tmp;
            }
          else
            {
              // The first element of the original dimension vector
              // was not a singleton dimension.

              new_dims.resize (2);

              new_dims(1) = 1;
            }
        }
      else
        new_dims.resize(k);
    }

  return new_dims;
}

bool
dim_vector::concat (const dim_vector& dvb, int dim)
{
  int orig_nd = ndims (), ndb = dvb.ndims ();
  int new_nd = dim < ndb ? ndb : dim + 1;
  if (new_nd > orig_nd)
    resize (new_nd, 1);
  else
    new_nd = orig_nd;

  make_unique ();

  bool match = true;

  for (int i = 0; i < ndb; i++)
    {
      if (i != dim && rep[i] != dvb(i))
        {
          match = false;
          break;
        }
    }

  for (int i = ndb; i < new_nd; i++)
    {
      if (i != dim && rep[i] != 1)
        {
          match = false;
          break;
        }
    }

  if (match)
    rep[dim] += (dim < ndb ? dvb(dim) : 1);
  else
    {
      // Dimensions don't match. The only allowed fix is
      // to omit 0x0.
      if (ndb == 2 && dvb(0) == 0 && dvb(1) == 0)
        match = true;
      else if (orig_nd == 2 && rep[0] == 0 && rep[1] == 0)
        {
          *this = dvb;
          match = true;
        }
    }

  chop_trailing_singletons ();

  return match;
}

dim_vector
dim_vector::redim (int n) const
{
  int n_dims = length ();

  if (n_dims == n)
    return *this;
  else if (n_dims < n)
    {
      dim_vector retval = alloc (n);

      int pad = 0;
      for (int i = 0; i < n_dims; i++)
        {
          retval.rep[i] = rep[i];
          if (rep[i] != 0)
            pad = 1;
        }

      for (int i = n_dims; i < n; i++)
        retval.rep[i] = pad;

      return retval;
    }
  else
    {
      if (n < 1) n = 1;

      dim_vector retval = alloc (n);

      retval.rep[1] = 1;

      for (int i = 0; i < n-1; i++)
        retval.rep[i] = rep[i];

      int k = rep[n-1];
      for (int i = n; i < n_dims; i++)
        k *= rep[i];

      retval.rep[n-1] = k;

      return retval;
    }
}
