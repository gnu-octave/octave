/*

Copyright (C) 2018 Markus Mützel

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "unistr.h"

#include "unistr-wrappers.h"

int
octave_u8_strmblen_wrapper (const uint8_t *src)
{
  return u8_strmblen (src);
}

uint32_t *
octave_u8_to_u32_wrapper (const uint8_t *src, size_t src_len,
                          uint32_t *result_buf, size_t *lengthp)
{
  return u8_to_u32 (src, src_len, result_buf, lengthp);
}
