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

// The hash functions are provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "md2.h"
#include "md4.h"
#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "sha512.h"

#include "hash-wrappers.h"

int octave_md2_digest_size (void) { return MD2_DIGEST_SIZE; }
int octave_md4_digest_size (void) { return MD4_DIGEST_SIZE; }
int octave_md5_digest_size (void) { return MD5_DIGEST_SIZE; }
int octave_sha1_digest_size (void) { return SHA1_DIGEST_SIZE; }
int octave_sha224_digest_size (void) { return SHA224_DIGEST_SIZE; }
int octave_sha256_digest_size (void) { return SHA256_DIGEST_SIZE; }
int octave_sha384_digest_size (void) { return SHA384_DIGEST_SIZE; }
int octave_sha512_digest_size (void) { return SHA512_DIGEST_SIZE; }

void *
octave_md2_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return md2_buffer (buf, len, res);
}

void *
octave_md4_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return md4_buffer (buf, len, res);
}

void *
octave_md5_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return md5_buffer (buf, len, res);
}

void *
octave_sha1_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return sha1_buffer (buf, len, res);
}

void *
octave_sha224_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return sha224_buffer (buf, len, res);
}

void *
octave_sha256_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return sha256_buffer (buf, len, res);
}

void *
octave_sha384_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return sha384_buffer (buf, len, res);
}

void *
octave_sha512_buffer_wrapper (const char *buf, size_t len, void *res)
{
  return sha512_buffer (buf, len, res);
}
