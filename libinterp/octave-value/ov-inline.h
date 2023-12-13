////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023 The Octave Project Developers
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

#if ! defined (octave_ov_inline_h)
#define octave_ov_inline_h 1

#include "octave-config.h"

#include "ov.h"

#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-bool.h"
#include "ov-base.h"


// class to construct octave_value:s inline

class octave_value_factory
{
public:

  static octave_value make (double d)
  {
    return octave_value (new octave_scalar (d));
  }

  static octave_value make (float d)
  {
    return octave_value (new octave_float_scalar (d));
  }

  static octave_value make (short int i)
  {
    return octave_value (new octave_scalar (i));
  }

  static octave_value make (unsigned short int i)
  {
    return octave_value (new octave_scalar (i));
  }

  static octave_value make (int i)
  {
    return octave_value (new octave_scalar (i));
  }

  static octave_value make (unsigned int i)
  {
    return octave_value (new octave_scalar (i));
  }

  static octave_value make (long int i)
  {
    return octave_value (new octave_scalar (i));
  }

  static octave_value make (unsigned long int i)
  {
    return octave_value (new octave_scalar (i));
  }

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
  static octave_value make (long long int i)
  {
    return octave_value (new octave_scalar (i));
  }
#endif

#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
  static octave_value make (unsigned long long int i)
  {
    return octave_value (new octave_scalar (i));
  }
#endif

  static octave_value make (octave::sys::time t)
  {
    return octave_value (new octave_scalar (t.double_value ()));
  }

  static octave_value make (const Complex& C)
  {
    octave_value ov (new octave_complex (C));
    ov.maybe_mutate (); // Fold e.g. 1+0i to 1
    return ov;
  }

  static octave_value make (const FloatComplex& C)
  {
    octave_value ov (new octave_float_complex (C));
    ov.maybe_mutate ();
    return ov;
  }

  static octave_value make (bool b)
  {
    return octave_value (new octave_bool (b));
  }

  // FIXME: The octave_value (octave_base_value *rep, bool) constructor
  // is already defined in the ov.h header file so is the
  // octave_value_factory::make_copy function really necessary?

  static octave_value make_copy (octave_base_value *rep)
  {
    return octave_value (rep, true);
  }

private:

  ~octave_value_factory () = delete;
  octave_value_factory () = delete;
};

#endif
