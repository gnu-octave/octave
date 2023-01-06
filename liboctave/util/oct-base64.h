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

#if ! defined (octave_oct_base64_h)
#define octave_oct_base64_h 1

#include "octave-config.h"

#include <string>

#include "Array-fwd.h"
#include "intNDArray.h"

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTAVE_API bool
base64_encode (const char *inc, const std::size_t inlen, char **out);

extern OCTAVE_API Array<double>
base64_decode (const std::string& str);

extern OCTAVE_API intNDArray<octave_uint8>
base64_decode_bytes (const std::string& str);

OCTAVE_END_NAMESPACE(octave)

#endif
