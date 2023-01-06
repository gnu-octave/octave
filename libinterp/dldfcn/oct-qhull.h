////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#if ! defined (octave_oct_qhull_h)
#define octave_oct_qhull_h 1

#include "octave-config.h"

#include <cstdio>

extern "C" {

#if defined (HAVE_LIBQHULL_R_LIBQHULL_R_H)
#  include <libqhull_r/libqhull_r.h>
#  include <libqhull_r/qset_r.h>
#  include <libqhull_r/geom_r.h>
#  include <libqhull_r/poly_r.h>
#  include <libqhull_r/io_r.h>
#elif defined (HAVE_LIBQHULL_R_H)
#  include <libqhull_r.h>
#  include <qset_r.h>
#  include <geom_r.h>
#  include <poly_r.h>
#  include <io_r.h>
#endif

}

#endif
