////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#if ! defined (octave_xnorm_h)
#define octave_xnorm_h 1

#include "octave-config.h"

#include "oct-norm.h"

class octave_value;

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API octave_value
xnorm (const octave_value& x, const octave_value& p);

extern OCTINTERP_API octave_value
xcolnorms (const octave_value& x, const octave_value& p);

extern OCTINTERP_API octave_value
xrownorms (const octave_value& x, const octave_value& p);

extern OCTINTERP_API octave_value
xfrobnorm (const octave_value& x);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline octave_value
xnorm (const octave_value& x, const octave_value& p)
{
  return octave::xnorm (x, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline octave_value
xcolnorms (const octave_value& x, const octave_value& p)
{
  return octave::xcolnorms (x, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline octave_value
xrownorms (const octave_value& x, const octave_value& p)
{
  return octave::xrownorms (x, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline octave_value
xfrobnorm (const octave_value& x)
{
  return octave::xfrobnorm (x);
}

#endif

#endif
