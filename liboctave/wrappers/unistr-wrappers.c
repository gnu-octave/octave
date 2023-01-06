////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#include "unistr.h"

#include "unistr-wrappers.h"

const uint8_t *
octave_u8_check_wrapper (const uint8_t *src, size_t n)
{
  return u8_check (src, n);
}

int
octave_u8_strmblen_wrapper (const uint8_t *src)
{
  return u8_strmblen (src);
}

size_t
octave_u8_mbsnlen_wrapper (const uint8_t *src, size_t n)
{
  return u8_mbsnlen (src, n);
}

int
octave_u8_strmbtouc_wrapper (uint32_t *puc, const uint8_t *src)
{
  return u8_strmbtouc (puc, src);
}

uint8_t *
octave_u16_to_u8_wrapper (const uint16_t *src, size_t src_len,
                          uint8_t *result_buf, size_t *lengthp)
{
  return u16_to_u8 (src, src_len, result_buf, lengthp);
}

uint8_t *
octave_u32_to_u8_wrapper (const uint32_t *src, size_t src_len,
                          uint8_t *result_buf, size_t *lengthp)
{
  return u32_to_u8 (src, src_len, result_buf, lengthp);
}

uint16_t *
octave_u8_to_u16_wrapper (const uint8_t *src, size_t src_len,
                          uint16_t *result_buf, size_t *lengthp)
{
  return u8_to_u16 (src, src_len, result_buf, lengthp);
}

uint32_t *
octave_u8_to_u32_wrapper (const uint8_t *src, size_t src_len,
                          uint32_t *result_buf, size_t *lengthp)
{
  return u8_to_u32 (src, src_len, result_buf, lengthp);
}
