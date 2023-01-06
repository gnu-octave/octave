////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
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

#if ! defined (octave_latex_text_renderer_h)
#define octave_latex_text_renderer_h 1

#include "octave-config.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_text_renderer;

extern base_text_renderer * make_latex_text_renderer (void);

OCTAVE_END_NAMESPACE(octave)

#endif
