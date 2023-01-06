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

#if ! defined (octave_base64_wrappers_h)
#define octave_base64_wrappers_h 1

#if defined (__cplusplus)
#  include <cstddef>
#else
#  include <stdbool.h>
#  include <stddef.h>
#endif

#if defined __cplusplus
extern "C" {
#endif

extern OCTAVE_API ptrdiff_t
octave_base64_encode_alloc_wrapper (const char *in, ptrdiff_t inlen, char **out);

extern OCTAVE_API bool
octave_base64_decode_alloc_wrapper (const char *in, ptrdiff_t inlen,
                                    char **out, ptrdiff_t *outlen);

#if defined __cplusplus
}
#endif

#endif
