////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

static char *
octave_u8_conv_to_encoding_intern (const char *tocode,
                                   enum iconv_ilseq_handler handler,
                                   const uint8_t *src, size_t srclen,
                                   size_t *offsets, size_t *lengthp)
{
  // FIXME: It looks like the input to u8_conv_to_encoding must be at least
  //        four bytes and zero-terminated to work correctly.  Zero-pad input.
  //        Should this be fixed in gnulib or iconv instead?
  size_t minlen = 4;
  size_t padlen = (srclen > minlen ? srclen : minlen) + 1;
  uint8_t *u8_str = (uint8_t *) malloc (padlen);
  memcpy (u8_str, src, srclen);
  for (size_t i_pad = 0; i_pad < padlen-srclen; i_pad++)
    u8_str[srclen+i_pad] = 0;

  // Convert from UTF-8 to output encoding
  char *ret = u8_conv_to_encoding (tocode, handler, u8_str, padlen,
                                   offsets, NULL, lengthp);
  free ((void *) u8_str);

  // FIXME: This assumes that "\0" is converted to a single byte.  This might
  //        not be true for some exotic output encodings (like UTF-7?).
  *lengthp = (*lengthp <= (padlen-srclen) ? 0 : *lengthp - (padlen-srclen));
  
  return ret;
}

char *
octave_u8_conv_to_encoding (const char *tocode, const uint8_t *src,
                            size_t srclen, size_t *lengthp)
{
  return octave_u8_conv_to_encoding_intern (tocode, iconveh_question_mark,
                                            src, srclen, NULL, lengthp);
}

char *
octave_u8_conv_to_encoding_strict (const char *tocode, const uint8_t *src,
                                   size_t srclen, size_t *lengthp)
{
  return octave_u8_conv_to_encoding_intern (tocode, iconveh_error,
                                            src, srclen, NULL, lengthp);
}

char *
octave_u32_conv_to_encoding_strict (const char *tocode, const uint32_t *src,
                                    size_t srclen, size_t *lengthp)
{
  return u32_conv_to_encoding (tocode, iconveh_error,
                               src, srclen, NULL, NULL, lengthp);
}

uint8_t *
octave_u8_conv_from_encoding_offsets
  (const char *fromcode, const char *src, size_t srclen,
   size_t *offsets, size_t *lengthp)
{
  return u8_conv_from_encoding (fromcode, iconveh_question_mark,
                                src, srclen, offsets, NULL, lengthp);
}

char *
octave_u8_conv_to_encoding_offsets
  (const char *tocode, const uint8_t *src, size_t srclen,
   size_t *offsets, size_t *lengthp)
{
  return octave_u8_conv_to_encoding_intern (tocode, iconveh_question_mark,
                                            src, srclen, offsets, lengthp);
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
