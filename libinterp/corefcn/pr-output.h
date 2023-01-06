////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_pr_output_h)
#define octave_pr_output_h 1

#include "octave-config.h"

#include <iosfwd>

#include "Array-fwd.h"
#include "intNDArray-fwd.h"
#include "oct-cmplx.h"
#include "oct-inttypes-fwd.h"
#include "range-fwd.h"

#include "pr-flt-fmt.h"

class ComplexMatrix;
class FloatComplexMatrix;
class ComplexDiagMatrix;
class FloatComplexDiagMatrix;
class ComplexNDArray;
class FloatComplexNDArray;
class Matrix;
class FloatMatrix;
class DiagMatrix;
class FloatDiagMatrix;
class NDArray;
class FloatNDArray;
class boolMatrix;
class boolNDArray;
class charMatrix;
class charNDArray;
class PermMatrix;
class Cell;
class octave_value;

template <typename T>
float_display_format
make_format (const std::complex<T>&)
{
  return float_display_format ();
}

template <>
float_display_format
make_format (const std::complex<double>& c);

template <>
float_display_format
make_format (const std::complex<float>& fc);

template <typename T>
float_display_format
make_format (const T&)
{
  return float_display_format ();
}

template <>
float_display_format
make_format (const double& d);

template <>
float_display_format
make_format (const float& f);

template <>
float_display_format
make_format (const octave::range<double>& r);

template <>
float_display_format
make_format (const Matrix& m);

template <>
float_display_format
make_format (const FloatMatrix& m);

template <>
float_display_format
make_format (const ComplexMatrix& m);

template <>
float_display_format
make_format (const FloatComplexMatrix& m);

template <>
float_display_format
make_format (const boolNDArray& nda);

template <>
float_display_format
make_format (const intNDArray<octave_int8>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_int16>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_int32>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_int64>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_uint8>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_uint16>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_uint32>& nda);

template <>
float_display_format
make_format (const intNDArray<octave_uint64>& nda);

template <>
float_display_format
make_format (const octave_int8& nda);

template <>
float_display_format
make_format (const octave_int16& nda);

template <>
float_display_format
make_format (const octave_int32& nda);

template <>
float_display_format
make_format (const octave_int64& nda);

template <>
float_display_format
make_format (const octave_uint8& nda);

template <>
float_display_format
make_format (const octave_uint16& nda);

template <>
float_display_format
make_format (const octave_uint32& nda);

template <>
float_display_format
make_format (const octave_uint64& nda);

// FIXME: templates plus specializations might help here.

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       bool d, bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, bool d,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       char c, bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, char c,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, c, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       double d, bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, double d,
                       bool pr_as_read_syntax = false)
{
  octave_print_internal (os, make_format (d), d, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       float d, bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, float d,
                       bool pr_as_read_syntax = false)
{
  octave_print_internal (os, make_format (d), d, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Matrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const DiagMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatDiagMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const NDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       const Complex& c, bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const Complex& c,
                       bool pr_as_read_syntax = false)
{
  octave_print_internal (os, make_format (c), c, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       const FloatComplex& c, bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const FloatComplex& c,
                       bool pr_as_read_syntax = false)
{
  octave_print_internal (os, make_format (c), c, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const ComplexMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const ComplexDiagMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplexMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplexDiagMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const ComplexNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplexNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const PermMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave::range<double>& r,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const boolMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const boolNDArray& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const charMatrix& chm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0,
                       bool pr_as_string = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const charNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0,
                       bool pr_as_string = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const std::string& s,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Array<std::string>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int8>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint8>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int16>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint16>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int32>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint32>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int64>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint64>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<int8_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<int8_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<uint8_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<uint8_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<int16_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<int16_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<uint16_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<uint16_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<int32_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<int32_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<uint32_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<uint32_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<int64_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<int64_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const octave_int<uint64_t>& sa,
                       bool pr_as_read_syntax = false);

inline void
octave_print_internal (std::ostream& os, const octave_int<uint64_t>& sa,
                       bool pr_as_read_syntax = false)
{
  float_display_format fmt (float_format (0, 0));
  octave_print_internal (os, fmt, sa, pr_as_read_syntax);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Cell& cell,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0,
                       bool pr_as_string = false);

inline void
octave_print_internal (std::ostream& os, const float_display_format&,
                       const Cell& cell, bool pr_as_read_syntax = false,
                       int extra_indent = 0, bool pr_as_string = false)
{
  octave_print_internal (os, cell, pr_as_read_syntax, extra_indent,
                         pr_as_string);
}

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_value& ov,
                       bool pr_as_read_syntax = false);

template <typename T>
class
pr_engineering_float
{
public:

  const float_format m_ff;

  T m_val;

  int exponent (void) const;

  T mantissa (void) const;

  pr_engineering_float (const float_format& ff, T val)
    : m_ff (ff), m_val (val) { }

  pr_engineering_float (const float_display_format& fdf, T val)
    : m_ff (fdf.real_format ()), m_val (val) { }
};

template <typename T>
class
pr_formatted_float
{
public:

  const float_format m_ff;

  T m_val;

  pr_formatted_float (const float_format& ff, T val)
    : m_ff (ff), m_val (val) { }

  pr_formatted_float (const float_display_format& fdf, T val)
    : m_ff (fdf.real_format ()), m_val (val) { }
};

template <typename T>
class
pr_rational_float
{
public:

  const float_format m_ff;

  T m_val;

  pr_rational_float (const float_format& ff, T val)
    : m_ff (ff), m_val (val) { }

  pr_rational_float (const float_display_format& fdf, T val)
    : m_ff (fdf.real_format ()), m_val (val) { }
};

template <typename T>
extern std::ostream&
operator << (std::ostream& os, const pr_engineering_float<T>& pef);

template <typename T>
extern std::ostream&
operator << (std::ostream& os, const pr_formatted_float<T>& pff);

template <typename T>
extern std::ostream&
operator << (std::ostream& os, const pr_rational_float<T>& prf);

// TRUE means that the dimensions of empty objects should be printed
// like this: x = [](2x0).
extern bool Vprint_empty_dimensions;

// TRUE means don't put empty lines in output
extern bool Vcompact_format;

#endif
