/*

Copyright (C) 1994-2017 John W. Eaton

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

#if ! defined (octave_schur_h)
#define octave_schur_h 1

#include "octave-config.h"

#include <string>

#include "dMatrix.h"
#include "CMatrix.h"
#include "fMatrix.h"
#include "fCMatrix.h"

namespace octave
{
  namespace math
  {
    template <typename T> class schur;

    template <typename T>
    class
    schur
    {
    public:

      schur (void) : schur_mat (), unitary_mat () { }

      schur (const T& a, const std::string& ord, bool calc_unitary = true)
        : schur_mat (), unitary_mat ()
      {
        init (a, ord, calc_unitary);
      }

      schur (const T& a, const std::string& ord, octave_f77_int_type& info,
             bool calc_unitary = true)
        : schur_mat (), unitary_mat ()
      {
        info = init (a, ord, calc_unitary);
      }

      // This one should really be protected or private but we need it in
      // rsf2csf and I don't see how to make that function a friend of
      // this class.
      schur (const T& s, const T& u) : schur_mat (s), unitary_mat (u) { }

      schur (const schur& a)

        : schur_mat (a.schur_mat), unitary_mat (a.unitary_mat)
      { }

      schur& operator = (const schur& a)
      {
        if (this != &a)
          {
            schur_mat = a.schur_mat;
            unitary_mat = a.unitary_mat;
          }

        return *this;
      }

      ~schur (void) = default;

      T schur_matrix (void) const { return schur_mat; }

      T unitary_matrix (void) const { return unitary_mat; }

    protected:

    private:

      T schur_mat;
      T unitary_mat;

      octave_f77_int_type
      init (const T& a, const std::string& ord, bool calc_unitary);
    };

    template <typename RT, typename AT>
    extern schur<RT>
    rsf2csf (const AT& s, const AT& u);
  }
}

#endif
