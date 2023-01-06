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

#if ! defined (octave_oct_string_h)
#define octave_oct_string_h 1

#include "octave-config.h"

#include <locale>

#include "oct-cmplx.h"

OCTAVE_BEGIN_NAMESPACE(octave)

//! Octave string utility functions.
//!
//! This functions provide a C++ interface to most string functions
//! available in the Octave interpreter.
//!
//! Specializations for Array may consider its dimensions in addition
//! to the actual string contents.
//!
//! @attention
//! Octave's string comparison functions return true when strings are
//! are equal, just the opposite of the corresponding C library functions.
//! In addition, Octave's function only return bool and do not check
//! lexicographical order.

OCTAVE_BEGIN_NAMESPACE(string)

//! True if strings are the same.
//!
//! ## Specialization for Array<char>
//!
//! When comparing whole Array of chars, the actual Array dimensions
//! are significant.  A column vector and row vector with the same
//! char array, will still return false.

template <typename T>
OCTAVE_API bool
strcmp (const T& str_a, const T& str_b);

//! True if string is the same as character sequence.
//!
//! Compares a string to the null-terminated character sequence
//! beginning at the character pointed to by str_b.
//!
//! ## Specialization for Array<char>
//!
//! For purposes of comparison of dimensions, the character sequence
//! is considered to be a row vector.

template <typename T>
OCTAVE_API bool
strcmp (const T& str_a, const typename T::value_type *str_b);

//! True if strings are the same, ignoring case.
//!
//! ## Specialization for Array<char>
//!
//! When comparing whole Array of chars, the actual Array dimensions
//! are significant.  A column vector and row vector with the same
//! char array, will still return false.

template <typename T>
OCTAVE_API bool
strcmpi (const T& str_a, const T& str_b);

//! True if string is the same as character sequence, ignoring case.
//!
//! ## Specialization for Array<char>
//!
//! For purposes of comparison of dimensions, the character sequence
//! is considered to be a row vector.

template <typename T>
OCTAVE_API bool
strcmpi (const T& str_a, const typename T::value_type *str_b);

//! True if the first N characters are the same.
//!
//! ## Specialization for Array<char>
//!
//! The comparison is done in the first N characters, the actual
//! dimensions of the Array are irrelevant.  A row vector and
//! a column vector of the same still return true.

template <typename T>
OCTAVE_API bool
strncmp (const T& str_a, const T& str_b,
         const typename T::size_type n);

//! True if the first N characters are the same.
template <typename T>
OCTAVE_API bool
strncmp (const T& str_a, const typename T::value_type *str_b,
         const typename T::size_type n);

//! True if the first N characters are the same, ignoring case.
//!
//! ## Specialization for Array<char>
//!
//! The comparison is done in the first N characters, the actual
//! dimensions of the Array are irrelevant.  A row vector and
//! a column vector of the same still return true.

template <typename T>
OCTAVE_API bool
strncmpi (const T& str_a, const T& str_b,
          const typename T::size_type n);

//! True if the first N characters are the same, ignoring case.
template <typename T>
OCTAVE_API bool
strncmpi (const T& str_a, const typename T::value_type *str_b,
          const typename T::size_type n);

extern OCTAVE_API Complex
str2double (const std::string& str_arg);

extern OCTAVE_API std::string
u8_to_encoding (const std::string& who, const std::string& u8_string,
                const std::string& encoding);

extern OCTAVE_API std::string
u8_from_encoding (const std::string& who, const std::string& native_string,
                  const std::string& encoding);

enum u8_fallback_type
{
  U8_REPLACEMENT_CHAR,
  U8_ISO_8859_1
};

extern OCTAVE_API unsigned int
u8_validate (const std::string& who, std::string& in_string,
             const u8_fallback_type type = U8_REPLACEMENT_CHAR);


template<class Facet>
struct
  deletable_facet : Facet
{
  template<class ...Args>
  deletable_facet (Args&& ...args)
    : Facet (std::forward<Args> (args)...)
  { }

  // destructor needs to be public
  ~deletable_facet () { }
};

class
OCTAVE_API
codecvt_u8 : public std::codecvt<char, char, std::mbstate_t>
{
public:

  // No copying!

  codecvt_u8 (codecvt_u8&) = delete;

  codecvt_u8& operator = (codecvt_u8&) = delete;

  codecvt_u8 (const std::string& enc)
    : m_enc (enc)
  { }

  virtual ~codecvt_u8 () { }

  typedef char InternT;
  typedef char ExternT;
  typedef std::mbstate_t StateT;

private:

  OCTAVE_API
  typename std::codecvt<InternT, ExternT, StateT>::result
  do_out (StateT& state,
          const InternT *from, const InternT *from_end, const InternT *&from_next,
          ExternT *to, ExternT *to_end, ExternT *&to_next) const;

  OCTAVE_API
  typename std::codecvt<InternT, ExternT, StateT>::result
  do_in (StateT& state,
         const ExternT *from, const ExternT *from_end, const ExternT *&from_next,
         InternT *to, InternT *to_end, InternT *&to_next) const;

  typename std::codecvt<InternT, ExternT, StateT>::result
  do_unshift (StateT& /* state */, ExternT *to, ExternT * /* to_end */,
              ExternT *&to_next) const
  {
    // FIXME: What is the correct thing to unshift?
    // Just reset?
    to_next = to;

    return std::codecvt<InternT, ExternT, StateT>::ok;
  }

  int do_encoding () const throw ()
  {
    // return 0 because UTF-8 encoding is variable length
    return 0;
  }

  bool do_always_noconv () const throw ()
  {
    // return false to indicate non-identity conversion
    return false;
  }

  OCTAVE_API int
  do_length (StateT& state, const ExternT *src, const ExternT *end,
             std::size_t max) const;

  int do_max_length() const throw ()
  {
    // For UTF-8, a maximum of 4 bytes are needed for one character.
    return 4;
  }

  std::string m_enc;

};

OCTAVE_END_NAMESPACE(string)
OCTAVE_END_NAMESPACE(octave)

template <typename T>
extern OCTAVE_API std::string
rational_approx (T val, int len);

#endif
