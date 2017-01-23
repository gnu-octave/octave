/*

Copyright (C) 1994-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_hess_h)
#define octave_hess_h 1

#include "octave-config.h"

#include <iosfwd>

namespace octave
{
  namespace math
  {
    template <typename T>
    class
    hess
    {
    public:

      hess (void)
        : hess_mat (), unitary_hess_mat ()
      { }

      hess (const T& a)
        : hess_mat (), unitary_hess_mat ()
      {
        init (a);
      }

      hess (const T& a, octave_idx_type& info)
        : hess_mat (), unitary_hess_mat ()
      {
        info = init (a);
      }

      hess (const hess& a)
        : hess_mat (a.hess_mat), unitary_hess_mat (a.unitary_hess_mat)
      { }

      hess& operator = (const hess& a)
      {
        if (this != &a)
          {
            hess_mat = a.hess_mat;
            unitary_hess_mat = a.unitary_hess_mat;
          }

        return *this;
      }

      ~hess (void) = default;

      T hess_matrix (void) const { return hess_mat; }

      T unitary_hess_matrix (void) const { return unitary_hess_mat; }

    private:

      T hess_mat;
      T unitary_hess_mat;

      octave_idx_type init (const T& a);
    };

    template <typename T>
    extern std::ostream&
    operator << (std::ostream& os, const hess<T>& a);
  }
}

#endif
