////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

// num_processors is provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "nproc.h"

#include "nproc-wrapper.h"

unsigned long int
octave_num_processors_wrapper (enum octave_nproc_query octave_query)
{
  enum nproc_query query = NPROC_CURRENT;

  switch (octave_query)
    {
    case OCTAVE_NPROC_ALL:
      query = NPROC_ALL;
      break;

    case OCTAVE_NPROC_CURRENT:
      query = NPROC_CURRENT;
      break;

    case OCTAVE_NPROC_CURRENT_OVERRIDABLE:
      query = NPROC_CURRENT_OVERRIDABLE;
      break;
    }

  return num_processors (query);
}
