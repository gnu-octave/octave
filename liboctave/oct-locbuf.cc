/*

Copyright (C) 2008-2011 Jaroslav Hajek

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include "oct-locbuf.h"

// Query for configured chunk size, and if not defined, set it to 32 MB.
// FIXME: 32MB is hard-coded. Maybe we could use something better, like
// querying for available physical memory.
#ifndef OCTAVE_LOCBUF_CHUNKSIZE_MB
#define OCTAVE_LOCBUF_CHUNKSIZE_MB 32
#endif

// Each chunk will be at least this big.
const size_t octave_chunk_buffer::chunk_size =
  static_cast<size_t> (OCTAVE_LOCBUF_CHUNKSIZE_MB) << 20;

char *octave_chunk_buffer::top = 0, *octave_chunk_buffer::chunk = 0;
size_t octave_chunk_buffer::left = 0;

octave_chunk_buffer::octave_chunk_buffer (size_t size) : cnk (0), dat (0)
{
  // Alignment mask. The size of double or long int, whichever is greater.
  // All data will be aligned to this size. If it's not enough for a type,
  // that type should not be declared as POD.
  static const size_t align_mask = (sizeof (long) < sizeof (double)
                                    ? sizeof (double)
                                    : sizeof (long)) - 1;

  if (! size) return;
  // Align size. Note that size_t is unsigned, so size-1 must correctly
  // wrap around.
  size = ((size - 1) | align_mask) + 1;

  if (size > left)
    {
      // Big buffers (> 1/8 chunk) will be allocated as stand-alone and
      // won't disrupt the chain.
      if (size > chunk_size >> 3)
        {
          // Use new [] to get std::bad_alloc if out of memory. Could as
          // well be std::malloc and handle that ourselves.
          dat = new char [size];
          return;
        }

      dat = new char [chunk_size];
      chunk = top = dat;
      left = chunk_size;
    }

  // Now allocate memory from the chunk and update state.
  cnk = chunk;
  dat = top;
  left -= size;
  top += size;
}

octave_chunk_buffer::~octave_chunk_buffer (void)
{
  if (cnk == chunk)
    {
      // Our chunk is still the active one. Just restore the state.
      left += top - dat;
      top = dat;
    }
  else if (! cnk)
    {
      // We were a stand-alone buffer.
      delete [] dat;
    }
  else
    {
      // Responsible for deletion.
      delete [] chunk;
      chunk = cnk;
      top = dat;
      // FIXME: This will only work if chunk_size is constant.
      left = chunk_size - (dat - cnk);
    }
}
