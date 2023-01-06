////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020-2023 The Octave Project Developers
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

#if ! defined (octave_mx_type_traits_h)
#define octave_mx_type_traits_h 1

#include "octave-config.h"

#include "mxtypes.h"
#include "oct-inttypes-fwd.h"

template <typename T>
class
mx_type_traits
{
public:
  static const mxClassID mx_class;
  typedef T mx_type;
};

template <>
class
mx_type_traits<bool>
{
public:
  static const mxClassID mx_class = mxLOGICAL_CLASS;
  typedef mxDouble mx_type;
};

template <>
class
mx_type_traits<char>
{
public:
  static const mxClassID mx_class = mxCHAR_CLASS;
  typedef mxChar mx_type;
};

template <>
class
mx_type_traits<double>
{
public:
  static const mxClassID mx_class = mxDOUBLE_CLASS;
  typedef mxDouble mx_type;
};

template <>
class
mx_type_traits<float>
{
public:
  static const mxClassID mx_class = mxSINGLE_CLASS;
  typedef mxSingle mx_type;
};

template <>
class
mx_type_traits<octave_int8>
{
public:
  static const mxClassID mx_class = mxINT8_CLASS;
  typedef mxInt8 mx_type;
};

template <>
class
mx_type_traits<octave_uint8>
{
public:
  static const mxClassID mx_class = mxUINT8_CLASS;
  typedef mxUint8 mx_type;
};

template <>
class
mx_type_traits<octave_int16>
{
public:
  static const mxClassID mx_class = mxINT16_CLASS;
  typedef mxInt16 mx_type;
};

template <>
class
mx_type_traits<octave_uint16>
{
public:
  static const mxClassID mx_class = mxUINT16_CLASS;
  typedef mxUint16 mx_type;
};

template <>
class
mx_type_traits<octave_int32>
{
public:
  static const mxClassID mx_class = mxINT32_CLASS;
  typedef mxInt32 mx_type;
};

template <>
class
mx_type_traits<octave_uint32>
{
public:
  static const mxClassID mx_class = mxUINT32_CLASS;
  typedef mxUint32 mx_type;
};

template <>
class
mx_type_traits<octave_int64>
{
public:
  static const mxClassID mx_class = mxINT64_CLASS;
  typedef mxInt64 mx_type;
};

template <>
class
mx_type_traits<octave_uint64>
{
public:
  static const mxClassID mx_class = mxUINT64_CLASS;
  typedef mxUint64 mx_type;
};

#endif
