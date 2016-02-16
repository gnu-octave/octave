/*

Copyright (C) 1994-2015 John W. Eaton

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

#if ! defined (octave_svd_h)
#define octave_svd_h 1

#include "octave-config.h"

#include <iosfwd>

template <typename T>
class
svd
{
public:

  typedef typename T::real_diag_matrix_type DM_T;

  enum type
  {
    std,
    economy,
    sigma_only
  };

  enum driver
  {
    GESVD,
    GESDD
  };

  svd (void)
    : type_computed (), left_sm (), sigma (), right_sm ()
  { }

  svd (const T& a, type svd_type = svd::std, driver svd_driver = svd::GESVD)
    : type_computed (), left_sm (), sigma (), right_sm ()
  {
    init (a, svd_type, svd_driver);
  }

  svd (const T& a, octave_idx_type& info, type svd_type = svd::std,
       driver svd_driver = svd::GESVD)
    : type_computed (), left_sm (), sigma (), right_sm ()
  {
    info = init (a, svd_type, svd_driver);
  }

  svd (const svd& a)
    : type_computed (a.type_computed), left_sm (a.left_sm),
      sigma (a.sigma), right_sm (a.right_sm)
  { }

  svd& operator = (const svd& a)
  {
    if (this != &a)
      {
        type_computed = a.type_computed;
        left_sm = a.left_sm;
        sigma = a.sigma;
        right_sm = a.right_sm;
      }

    return *this;
  }

  ~svd (void) { }

  T left_singular_matrix (void) const;

  DM_T singular_values (void) const { return sigma; }

  T right_singular_matrix (void) const;

private:

  svd::type type_computed;

  T left_sm;
  DM_T sigma;
  T right_sm;

  octave_idx_type
  init (const T& a, type svd_type, driver svd_driver);

  octave_idx_type
  empty_init (octave_idx_type nr, octave_idx_type nc, type svd_type);
};

#endif
