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

#if ! defined (octave_ov_range_traits_h)
#define octave_ov_range_traits_h 1

#include "octave-config.h"

#include "ov-bool-mat.h"
#include "ov-bool.h"
#include "ov-float.h"
#include "ov-flt-re-mat.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-int8.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-uint8.h"

template <typename T>
class
octave_value_range_traits
{
public:
  typedef T scalar_type;
  typedef T matrix_type;
};

template <>
class
octave_value_range_traits<bool>
{
public:
  typedef octave_bool scalar_type;
  typedef octave_bool_matrix matrix_type;
};

template <>
class
octave_value_range_traits<float>
{
public:
  typedef octave_float_scalar scalar_type;
  typedef octave_float_matrix matrix_type;
};

template <>
class
octave_value_range_traits<double>
{
public:
  typedef octave_scalar scalar_type;
  typedef octave_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_int8>
{
public:
  typedef octave_int8_scalar scalar_type;
  typedef octave_int8_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_int16>
{
public:
  typedef octave_int16_scalar scalar_type;
  typedef octave_int16_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_int32>
{
public:
  typedef octave_int32_scalar scalar_type;
  typedef octave_int32_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_int64>
{
public:
  typedef octave_int64_scalar scalar_type;
  typedef octave_int64_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_uint8>
{
public:
  typedef octave_uint8_scalar scalar_type;
  typedef octave_uint8_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_uint16>
{
public:
  typedef octave_uint16_scalar scalar_type;
  typedef octave_uint16_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_uint32>
{
public:
  typedef octave_uint32_scalar scalar_type;
  typedef octave_uint32_matrix matrix_type;
};

template <>
class
octave_value_range_traits<octave_uint64>
{
public:
  typedef octave_uint64_scalar scalar_type;
  typedef octave_uint64_matrix matrix_type;
};

#endif
