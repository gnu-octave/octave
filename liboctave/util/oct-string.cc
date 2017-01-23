/*
Copyright (C) 2016 Carnë Draug

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-string.h"

#include <cctype>
#include <cstring>

#include <string>

#include <Array.h>

template <typename T>
static bool
str_data_cmp (const typename T::value_type* a, const typename T::value_type* b,
              const typename T::size_type n)
{
  for (typename T::size_type i = 0; i < n; ++i)
    if (a[i] != b[i])
      return false;
  return true;
}

template <typename T>
static bool
str_data_cmpi (const typename T::value_type* a, const typename T::value_type* b,
               const typename T::size_type n)
{
  for (typename T::size_type i = 0; i < n; ++i)
    if (std::tolower (a[i]) != std::tolower (b[i]))
      return false;
  return true;
}


// Templates to handle std::basic_string, std::vector, Array, and char*.
template <typename T>
typename T::size_type
numel (const T& str)
{
  return str.size ();
}

template <>
octave_idx_type
numel (const Array<char>& str)
{
  return str.numel ();
}

template <typename T>
typename T::size_type
strlen (const typename T::value_type* str)
{
  return std::strlen (str);
}

template <typename T>
bool
sizes_cmp (const T& str_a, const T& str_b)
{
  return str_a.size () == str_b.size ();
}

template <>
bool
sizes_cmp (const Array<char>& str_a, const Array<char>& str_b)
{
  return str_a.dims () == str_b.dims ();
}

template <typename T>
bool
sizes_cmp (const T& str_a, const typename T::value_type* str_b)
{
  return str_a.size () == strlen<T> (str_b);
}

template <>
bool
sizes_cmp (const Array<char>& str_a, const char* str_b)
{
  return (str_a.is_vector () && str_a.rows () == 1
          && str_a.numel () == strlen<Array<char>> (str_b));
}


template<typename T>
bool
octave::string::strcmp (const T& str_a, const T& str_b)
{
  return (sizes_cmp (str_a, str_b)
          && str_data_cmp<T> (str_a.data (), str_b.data (), numel (str_a)));
}

template<typename T>
bool
octave::string::strcmp (const T& str_a, const typename T::value_type* str_b)
{
  return (sizes_cmp (str_a, str_b)
          && str_data_cmp<T> (str_a.data (), str_b, numel (str_a)));
}


template<typename T>
bool
octave::string::strcmpi (const T& str_a, const T& str_b)
{
  return (sizes_cmp (str_a, str_b)
          && str_data_cmpi<T> (str_a.data (), str_b.data (), numel (str_a)));
}

template<typename T>
bool
octave::string::strcmpi (const T& str_a, const typename T::value_type* str_b)
{
  return (sizes_cmp (str_a, str_b)
          && str_data_cmpi<T> (str_a.data (), str_b, numel (str_a)));
}


template<typename T>
bool
octave::string::strncmp (const T& str_a, const T& str_b,
                         const typename T::size_type n)
{
  return (numel (str_a) >= n && numel (str_b) >= n
          && str_data_cmpi<T> (str_a.data (), str_b.data (), n));
}

template<typename T>
bool
octave::string::strncmp (const T& str_a, const typename T::value_type* str_b,
                         const typename T::size_type n)
{
  return (numel (str_a) >= n && strlen<T> (str_b) >= n
          && str_data_cmpi<T> (str_a.data (), str_b, n));
}


template<typename T>
bool
octave::string::strncmpi (const T& str_a, const T& str_b,
                          const typename T::size_type n)
{
  return (numel (str_a) >= n && numel (str_b) >= n
          && str_data_cmpi<T> (str_a.data (), str_b.data (), n));
}

template<typename T>
bool
octave::string::strncmpi (const T& str_a, const typename T::value_type* str_b,
                          const typename T::size_type n)
{
  return (numel (str_a) >= n && strlen<T> (str_b) >= n
          && str_data_cmpi<T> (str_a.data (), str_b, n));
}


// Instantiations we need
#define INSTANTIATE_OCTAVE_STRING(T)                                          \
  template bool octave::string::strcmp<T> (const T&, const T&);               \
  template bool octave::string::strcmp<T> (const T&,                          \
                                           const typename T::value_type*);    \
  template bool octave::string::strcmpi<T> (const T&, const T&);              \
  template bool octave::string::strcmpi<T> (const T&,                         \
                                            const typename T::value_type*);   \
  template bool octave::string::strncmp<T> (const T&, const T&,               \
                                            const typename T::size_type);     \
  template bool octave::string::strncmp<T> (const T&,                         \
                                            const typename T::value_type*,    \
                                            const typename T::size_type);     \
  template bool octave::string::strncmpi<T> (const T&, const T&,              \
                                             const typename T::size_type n);  \
  template bool octave::string::strncmpi<T> (const T&,                        \
                                             const typename T::value_type*,   \
                                             const typename T::size_type);

// We could also instantiate std::vector<char> but would it be
// useful for anyone?
INSTANTIATE_OCTAVE_STRING(std::string)
INSTANTIATE_OCTAVE_STRING(Array<char>)

#undef INSTANTIATE_OCTAVE_STRING
