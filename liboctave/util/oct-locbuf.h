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

#if ! defined (octave_oct_locbuf_h)
#define octave_oct_locbuf_h 1

#include "octave-config.h"

#include <cstddef>

#include <algorithm>
#include <memory>

#if __cplusplus >= 201402L

#define OCTAVE_LOCAL_BUFFER(T, buf, size)                               \
  auto octave_local_buffer_ ## buf = std::make_unique<T []> (size);     \
  T *buf = octave_local_buffer_ ## buf.get ()

#else

#define OCTAVE_LOCAL_BUFFER(T, buf, size)                               \
  std::unique_ptr<T []> octave_local_buffer_ ## buf { new T [size] };   \
  T *buf = octave_local_buffer_ ## buf.get ()

#endif

#define OCTAVE_LOCAL_BUFFER_INIT(T, buf, size, value)                   \
  OCTAVE_LOCAL_BUFFER (T, buf, size);                                   \
  std::fill_n (buf, size, value)

#endif
