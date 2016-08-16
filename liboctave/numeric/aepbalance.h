/*

Copyright (C) 1994-2016 John W. Eaton
Copyright (C) 2008-2015 Jaroslav Hajek

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

#if ! defined (octave_aepbalance_h)
#define octave_aepbalance_h 1

#include "octave-config.h"

namespace octave
{
namespace math
{

template <typename MT>
class aepbalance
{
public:

  typedef typename MT::real_column_vector_type VT;

  aepbalance (void) : balanced_mat (), scale (), ilo (), ihi (), job () { }

  aepbalance (const MT& a, bool noperm = false, bool noscal = false);

  aepbalance (const aepbalance& a)
    : balanced_mat (a.balanced_mat), scale (a.scale),
      ilo(a.ilo), ihi(a.ihi), job(a.job)
  {
  }

  aepbalance& operator = (const aepbalance& a)
  {
    if (this != &a)
      {
        balanced_mat = a.balanced_mat;
        scale = a.scale;
        ilo = a.ilo;
        ihi = a.ihi;
        job = a.job;
      }

    return *this;
  }

  virtual ~aepbalance (void) { }

  MT balancing_matrix (void) const;

  MT balanced_matrix (void) const
  {
    return balanced_mat;
  }

  VT permuting_vector (void) const
  {
    octave_idx_type n = balanced_mat.rows ();

    VT pv (n);

    for (octave_idx_type i = 0; i < n; i++)
      pv(i) = i+1;

    for (octave_idx_type i = n-1; i >= ihi; i--)
      {
        octave_idx_type j = scale(i) - 1;
        std::swap (pv(i), pv(j));
      }

    for (octave_idx_type i = 0; i < ilo-1; i++)
      {
        octave_idx_type j = scale(i) - 1;
        std::swap (pv(i), pv(j));
      }

    return pv;
  }

  VT scaling_vector (void) const
  {
    octave_idx_type n = balanced_mat.rows ();

    VT scv (n);

    for (octave_idx_type i = 0; i < ilo-1; i++)
      scv(i) = 1;

    for (octave_idx_type i = ilo-1; i < ihi; i++)
      scv(i) = scale(i);

    for (octave_idx_type i = ihi; i < n; i++)
      scv(i) = 1;

    return scv;
  }

protected:

  MT balanced_mat;
  VT scale;
  octave_idx_type ilo;
  octave_idx_type ihi;
  char job;
};

}
}

#endif
