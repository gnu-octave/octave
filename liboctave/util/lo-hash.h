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

#if ! defined (octave_lo_hash_h)
#define octave_lo_hash_h 1

#include "octave-config.h"

#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(crypto)

typedef void *(hash_fptr) (const char *buffer, std::size_t len, void *res);

OCTAVE_API std::string
hash (hash_fptr hash_fcn, const std::string& str, int result_buf_len);

OCTAVE_API int md2_digest_size (void);
OCTAVE_API int md4_digest_size (void);
OCTAVE_API int md5_digest_size (void);
OCTAVE_API int sha1_digest_size (void);
OCTAVE_API int sha224_digest_size (void);
OCTAVE_API int sha256_digest_size (void);
OCTAVE_API int sha384_digest_size (void);
OCTAVE_API int sha512_digest_size (void);

OCTAVE_API std::string md2_hash (const std::string& str);
OCTAVE_API std::string md4_hash (const std::string& str);
OCTAVE_API std::string md5_hash (const std::string& str);
OCTAVE_API std::string sha1_hash (const std::string& str);
OCTAVE_API std::string sha224_hash (const std::string& str);
OCTAVE_API std::string sha256_hash (const std::string& str);
OCTAVE_API std::string sha384_hash (const std::string& str);
OCTAVE_API std::string sha512_hash (const std::string& str);

OCTAVE_API std::string
hash (const std::string& hash_type, const std::string& str);

OCTAVE_END_NAMESPACE(crypto)
OCTAVE_END_NAMESPACE(octave)

#endif
