////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2020 The Octave Project Developers
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

// The conversion functions are provided by gnulib.  We don't include
// gnulib headers directly in Octave's C++ source files to avoid
// problems that may be caused by the way that gnulib overrides standard
// library functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "uniconv.h"

#include "uniconv-wrappers.h"

uint8_t *
octave_u8_conv_from_encoding (const char *fromcode, const char *src,
                              size_t srclen, size_t *lengthp)
{
  return u8_conv_from_encoding (fromcode, iconveh_question_mark,
                                src, srclen, NULL, NULL, lengthp);
}

char *
octave_u8_conv_to_encoding (const char *tocode, const uint8_t *src,
                            size_t srclen, size_t *lengthp)
{
  return u8_conv_to_encoding (tocode, iconveh_question_mark,
                              src, srclen, NULL, NULL, lengthp);
}

char *
octave_u8_conv_to_encoding_strict (const char *tocode, const uint8_t *src,
                                   size_t srclen, size_t *lengthp)
{
  return u8_conv_to_encoding (tocode, iconveh_error,
                              src, srclen, NULL, NULL, lengthp);
}

char *
octave_u32_conv_to_encoding_strict (const char *tocode, const uint32_t *src,
                                    size_t srclen, size_t *lengthp)
{
  return u32_conv_to_encoding (tocode, iconveh_error,
                               src, srclen, NULL, NULL, lengthp);
}

char *
u8_from_wchar (const wchar_t *wc)
{
  // Convert wide char array to multibyte UTF-8 char array
  // The memory at the returned pointer must be freed after use.

  size_t srclen = wcslen (wc) * sizeof (wchar_t);
  const char *src = (const char *) wc;

  size_t length = 0;
  uint8_t *mbchar = u8_conv_from_encoding ("wchar_t", iconveh_question_mark,
                                           src, srclen, NULL, NULL, &length);

  // result might not be 0 terminated
  char *retval = malloc (length + 1);
  if (retval)
    {
      memcpy (retval, mbchar, length);
      free ((void *) mbchar);
      retval[length] = 0; // 0 terminate string
    }
  else
    free ((void *) mbchar);

  return retval;
}

wchar_t *
u8_to_wchar (const char *u8)
{
  // Convert multibyte UTF-8 char array to wide char array
  // The memory at the returned pointer must be freed after use.

  size_t srclen = strlen (u8);
  const uint8_t *src = (const uint8_t *) u8;

  size_t length = 0;

  char *wchar = u8_conv_to_encoding ("wchar_t", iconveh_question_mark,
                                     src, srclen, NULL, NULL, &length);
  // result might not be 0 terminated
  wchar_t *retval = malloc (length + 1 * sizeof (wchar_t));
  if (retval)
    {
      memcpy (retval, wchar, length);
      free ((void *) wchar);
      retval[length / sizeof (wchar_t)] = 0; // 0 terminate string
    }

  else
    free ((void *) wchar);

  return retval;
}
