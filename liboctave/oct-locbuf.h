/*

Copyright (C) 2008 Jaroslav Hajek <highegg@gmail.com>

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

#if !defined (octave_local_buffer_h)
#define octave_local_buffer_h 1

#include <cstddef>

// The default local buffer simply encapsulates an *array* pointer that gets
// delete[]d automatically. For common POD types, we provide specializations.

template <class T>
class octave_local_buffer
{
public:
  octave_local_buffer (size_t size)
    : data (0) 
    { 
      if (size) 
        data = new T[size]; 
    }
  ~octave_local_buffer (void) { delete [] data; }
  operator T *() const { return data; }
private:
  T *data;
};


// If the compiler supports dynamic stack arrays, we can use the attached hack to 
// place small buffer arrays on the stack. 

#ifdef HAVE_DYNAMIC_AUTO_ARRAYS

// Maximum buffer size (in bytes) to be placed on the stack.

#define OCTAVE_LOCAL_BUFFER_MAX_STACK_SIZE 8192

// If we have automatic arrays, we use an automatic array if the size is small
// enough.  To avoid possibly evaluating `size' multiple times, we first cache
// it.  Note that we always construct both the stack array and the
// octave_local_buffer object, but only one of them will be nonempty.

#define OCTAVE_LOCAL_BUFFER(T, buf, size) \
  const size_t _bufsize_ ## buf = size; \
  const bool _lbufaut_ ## buf = _bufsize_ ## buf * sizeof (T) \
     <= OCTAVE_LOCAL_BUFFER_MAX_STACK_SIZE; \
  T _bufaut_ ## buf [_lbufaut_ ## buf ? _bufsize_ ## buf : 0]; \
  octave_local_buffer<T> _bufheap_ ## buf (!_lbufaut_ ## buf ? _bufsize_ ## buf : 0); \
  T *buf = _lbufaut_ ## buf ? _bufaut_ ## buf : static_cast<T *> (_bufheap_ ## buf)

#else

// If we don't have automatic arrays, we simply always use octave_local_buffer.

#define OCTAVE_LOCAL_BUFFER(T, buf, size) \
  octave_local_buffer<T> _buffer_ ## buf (size); \
  T *buf = _buffer_ ## buf

#endif 

// Yeah overloading macros would be nice.
// Note: we use weird variables in the for loop to avoid warnings about
// shadowed parameters.
#define OCTAVE_LOCAL_BUFFER_INIT(T, buf, size, value) \
  OCTAVE_LOCAL_BUFFER(T, buf, size); \
  for (size_t _buf_iter = 0, _buf_size = size; \
       _buf_iter < _buf_size; _buf_iter++) buf[_buf_iter] = value

#endif

