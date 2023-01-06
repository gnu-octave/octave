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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>
#include <iomanip>
#include <sstream>
#include <string>

#include "hash-wrappers.h"
#include "lo-error.h"
#include "lo-hash.h"
#include "oct-locbuf.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(crypto)

std::string
hash (hash_fptr hash_fcn, const std::string& str, int result_buf_len)
{
  OCTAVE_LOCAL_BUFFER (unsigned char, result_buf, result_buf_len);

  hash_fcn (str.data (), str.length (), result_buf);

  std::ostringstream buf;

  for (int i = 0; i < result_buf_len; i++)
    buf << std::hex << std::setw (2) << std::setfill ('0')
        << (result_buf[i] & 0xFF);

  return buf.str ();
}

int md2_digest_size (void) { return octave_md2_digest_size (); }
int md4_digest_size (void) { return octave_md4_digest_size (); }
int md5_digest_size (void) { return octave_md5_digest_size (); }
int sha1_digest_size (void) { return octave_sha1_digest_size (); }
int sha224_digest_size (void) { return octave_sha224_digest_size (); }
int sha256_digest_size (void) { return octave_sha256_digest_size (); }
int sha384_digest_size (void) { return octave_sha384_digest_size (); }
int sha512_digest_size (void) { return octave_sha512_digest_size (); }

std::string
md2_hash (const std::string& str)
{
  return hash (octave_md2_buffer_wrapper, str, md2_digest_size ());
}

std::string
md4_hash (const std::string& str)
{
  return hash (octave_md4_buffer_wrapper, str, md4_digest_size ());
}

std::string
md5_hash (const std::string& str)
{
  return hash (octave_md5_buffer_wrapper, str, md5_digest_size ());
}

std::string
sha1_hash (const std::string& str)
{
  return hash (octave_sha1_buffer_wrapper, str, sha1_digest_size ());
}

std::string
sha224_hash (const std::string& str)
{
  return hash (octave_sha224_buffer_wrapper, str, sha224_digest_size ());
}

std::string
sha256_hash (const std::string& str)
{
  return hash (octave_sha256_buffer_wrapper, str, sha256_digest_size ());
}

std::string
sha384_hash (const std::string& str)
{
  return hash (octave_sha384_buffer_wrapper, str, sha384_digest_size ());
}

std::string
sha512_hash (const std::string& str)
{
  return hash (octave_sha512_buffer_wrapper, str, sha512_digest_size ());
}

std::string
hash (const std::string& hash_type, const std::string& str)
{
  std::string ht = hash_type;

  std::transform (ht.begin (), ht.end (), ht.begin (), ::toupper);

  if (ht == "MD2")
    return md2_hash (str);
  else if (ht == "MD4")
    return md4_hash (str);
  else if (ht == "MD5")
    return md5_hash (str);
  else if (ht == "SHA1")
    return sha1_hash (str);
  else if (ht == "SHA224")
    return sha224_hash (str);
  else if (ht == "SHA256")
    return sha256_hash (str);
  else if (ht == "SHA384")
    return sha384_hash (str);
  else if (ht == "SHA512")
    return sha512_hash (str);
  else
    (*current_liboctave_error_handler)
      ("hash function '%s' not supported", hash_type.c_str ());
}

OCTAVE_END_NAMESPACE(crypto)
OCTAVE_END_NAMESPACE(octave)
