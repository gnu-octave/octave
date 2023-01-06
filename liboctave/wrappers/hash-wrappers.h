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

#if ! defined (octave_hash_wrappers_h)
#define octave_hash_wrappers_h 1

#if defined (__cplusplus)
#  include <cstddef>
#else
#  include <stddef.h>
#endif

#if defined __cplusplus
extern "C" {
#endif

extern OCTAVE_API int octave_md2_digest_size (void);
extern OCTAVE_API int octave_md4_digest_size (void);
extern OCTAVE_API int octave_md5_digest_size (void);
extern OCTAVE_API int octave_sha1_digest_size (void);
extern OCTAVE_API int octave_sha224_digest_size (void);
extern OCTAVE_API int octave_sha256_digest_size (void);
extern OCTAVE_API int octave_sha384_digest_size (void);
extern OCTAVE_API int octave_sha512_digest_size (void);

extern OCTAVE_API void *
octave_md2_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_md4_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_md5_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_sha1_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_sha224_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_sha256_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_sha384_buffer_wrapper (const char *buf, size_t len, void *res);

extern OCTAVE_API void *
octave_sha512_buffer_wrapper (const char *buf, size_t len, void *res);

#if defined __cplusplus
}
#endif

#endif
