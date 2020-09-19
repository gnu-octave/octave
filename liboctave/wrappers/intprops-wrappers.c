////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020 The Octave Project Developers
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

#include "intprops.h"

#include "intprops-wrappers.h"

// The *_OVERFLOW macros are provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

int
octave_i_multiply_overflow_wrapper (int a, int b)
{
  return INT_MULTIPLY_OVERFLOW (a, b);
}

int
octave_li_multiply_overflow_wrapper (long int a, long int b)
{
  return INT_MULTIPLY_OVERFLOW (a, b);
}

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
int
octave_lli_multiply_overflow_wrapper (long long int a, long long int b)
{
  return INT_MULTIPLY_OVERFLOW (a, b);
}
#endif

int
octave_ui_multiply_overflow_wrapper (unsigned int a, unsigned int b)
{
  return INT_MULTIPLY_OVERFLOW (a, b);
}

int
octave_uli_multiply_overflow_wrapper (unsigned long int a,
                                      unsigned long int b)
{
  return INT_MULTIPLY_OVERFLOW (a, b);
}

#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
int
octave_ulli_multiply_overflow_wrapper (unsigned long long int a,
                                       unsigned long long int b)
{
  return INT_MULTIPLY_OVERFLOW (a, b);
}
#endif
