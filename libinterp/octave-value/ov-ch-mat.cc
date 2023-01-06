////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>
#include <ostream>

#include "dNDArray.h"
#include "fNDArray.h"
#include "int8NDArray.h"
#include "int16NDArray.h"
#include "int32NDArray.h"
#include "int64NDArray.h"
#include "uint8NDArray.h"
#include "uint16NDArray.h"
#include "uint32NDArray.h"
#include "uint64NDArray.h"

#include "lo-ieee.h"
#include "mx-base.h"
#include "unicase-wrappers.h"
#include "unictype-wrappers.h"
#include "unistr-wrappers.h"

#include "mxarray.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-ch-mat.h"
#include "errwarn.h"
#include "pr-output.h"

template class octave_base_matrix<charNDArray>;

octave::idx_vector
octave_char_matrix::index_vector (bool /* require_integers */) const
{
  const char *p = m_matrix.data ();
  if (numel () == 1 && *p == ':')
    return octave::idx_vector (':');
  else
    return octave::idx_vector (array_value (true));
}

double
octave_char_matrix::double_value (bool) const
{
  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("character matrix", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "character matrix", "real scalar");

  return static_cast<unsigned char> (m_matrix(0, 0));
}

float
octave_char_matrix::float_value (bool) const
{
  if (rows () == 0 && columns () == 0)
    err_invalid_conversion ("character matrix", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "character matrix", "real scalar");

  return static_cast<unsigned char> (m_matrix(0, 0));
}

octave_int64
octave_char_matrix::int64_scalar_value () const
{
  octave_int64 retval = 0;

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("character matrix", "int64 scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "character matrix", "int64 scalar");

  retval = octave_int64 (m_matrix(0, 0));

  return retval;
}

octave_uint64
octave_char_matrix::uint64_scalar_value () const
{
  octave_uint64 retval = 0;

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("character matrix", "uint64 scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "character matrix", "uint64 scalar");

  retval = octave_uint64 (m_matrix(0, 0));

  return retval;
}

Complex
octave_char_matrix::complex_value (bool) const
{
  if (rows () == 0 && columns () == 0)
    err_invalid_conversion ("character matrix", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "character matrix", "complex scalar");

  return Complex (static_cast<unsigned char> (m_matrix(0, 0)), 0);
}

FloatComplex
octave_char_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("character matrix", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "character matrix", "complex scalar");

  retval = static_cast<unsigned char> (m_matrix(0, 0));

  return retval;
}

octave_value
octave_char_matrix::as_double (void) const
{
  return NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_single (void) const
{
  return FloatNDArray (m_matrix);
}

octave_value
octave_char_matrix::as_int8 (void) const
{
  return int8NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_int16 (void) const
{
  return int16NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_int32 (void) const
{
  return int32NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_int64 (void) const
{
  return int64NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_uint8 (void) const
{
  return uint8NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_uint16 (void) const
{
  return uint16NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_uint32 (void) const
{
  return uint32NDArray (m_matrix);
}

octave_value
octave_char_matrix::as_uint64 (void) const
{
  return uint64NDArray (m_matrix);
}

void
octave_char_matrix::print_raw (std::ostream& os,
                               bool pr_as_read_syntax) const
{
  octave_print_internal (os, m_matrix, pr_as_read_syntax,
                         current_print_indent_level ());
}

mxArray *
octave_char_matrix::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxCHAR_CLASS, dims (), mxREAL);

  mxChar *pd = static_cast<mxChar *> (retval->get_data ());

  mwSize nel = numel ();

  const char *pdata = m_matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    pd[i] = pdata[i];

  return retval;
}

// The C++ standard guarantees cctype defines functions, not macros (and
// hence macros *CAN'T* be defined if only cctype is included) so
// there's no need to fuck around.  The exceptions are isascii and
// toascii, which are not C++.  Oddly enough, all those character
// functions are int (*) (int), even in C++.  Wicked!
static inline int xisascii (int c)
{
#if defined (HAVE_ISASCII)
  return isascii (c);
#else
  return (c >= 0x00 && c <= 0x7f);
#endif
}

octave_value
octave_char_matrix::map (unary_mapper_t umap) const
{
  octave_value retval;

  switch (umap)
    {
#define STRING_MAPPER(UMAP,FCN,TYPE)                                  \
    case umap_ ## UMAP:                                               \
      return octave_value (m_matrix.map<TYPE, int (&) (int)> (FCN))

      STRING_MAPPER (xisascii, xisascii, bool);

#define STRING_U8_MAPPER(UMAP,FCN)                                             \
    case umap_ ## UMAP:                                                        \
      {                                                                        \
        charNDArray in_m = m_matrix;                                           \
        Array<octave_idx_type> p (dim_vector (m_matrix.ndims (), 1));          \
        if (m_matrix.ndims () > 1)                                             \
          {                                                                    \
            for (octave_idx_type i=0; i < m_matrix.ndims (); i++)              \
              p(i) = i;                                                        \
            p(0) = 1;                                                          \
            p(1) = 0;                                                          \
            in_m = m_matrix.permute (p);                                       \
          }                                                                    \
        boolNDArray b_array = boolNDArray (in_m.dims ());                      \
        const uint8_t *in = reinterpret_cast<const uint8_t *> (in_m.data ());  \
        uint32_t uc;                                                           \
        for (octave_idx_type i = 0; i < in_m.numel (); )                       \
        {                                                                      \
          int mblen = octave_u8_strmbtouc_wrapper (&uc, in + i);               \
          if (mblen < 1)                                                       \
            mblen = 1;                                                         \
          bool is_upper = FCN (uc);                                            \
          for (int j = 0; j < mblen; j++)                                      \
            b_array(i+j) = is_upper;                                           \
          i += mblen;                                                          \
        }                                                                      \
        return octave_value ((m_matrix.ndims () > 1) ? b_array.permute (p, true) \
                                                     : b_array);               \
      }

      STRING_U8_MAPPER (xisalnum, octave_uc_is_alnum_wrapper);
      STRING_U8_MAPPER (xisalpha, octave_uc_is_alpha_wrapper);
      STRING_U8_MAPPER (xiscntrl, octave_uc_is_cntrl_wrapper);
      STRING_U8_MAPPER (xisdigit, octave_uc_is_digit_wrapper);
      STRING_U8_MAPPER (xisgraph, octave_uc_is_graph_wrapper);
      STRING_U8_MAPPER (xislower, octave_uc_is_lower_wrapper);
      STRING_U8_MAPPER (xisprint, octave_uc_is_print_wrapper);
      STRING_U8_MAPPER (xispunct, octave_uc_is_punct_wrapper);
      STRING_U8_MAPPER (xisspace, octave_uc_is_space_wrapper);
      STRING_U8_MAPPER (xisupper, octave_uc_is_upper_wrapper);
      STRING_U8_MAPPER (xisxdigit, octave_uc_is_xdigit_wrapper);

#define STRING_U8_FCN(UMAP,U8_FCN,STD_FCN)                                     \
    case umap_ ## UMAP:                                                        \
      {                                                                        \
        charNDArray in_m = m_matrix;                                           \
        Array<octave_idx_type> p (dim_vector (m_matrix.ndims (), 1));          \
        if (m_matrix.ndims () > 1)                                             \
          {                                                                    \
            for (octave_idx_type i=0; i < m_matrix.ndims (); i++)              \
              p(i) = i;                                                        \
            p(0) = 1;                                                          \
            p(1) = 0;                                                          \
            in_m = m_matrix.permute (p);                                       \
          }                                                                    \
        std::size_t output_length = in_m.numel ();                             \
        charNDArray ch_array = charNDArray (in_m.dims ());                     \
        const uint8_t *in = reinterpret_cast<const uint8_t *> (in_m.data ());  \
        uint8_t *buf = reinterpret_cast<uint8_t *> (ch_array.fortran_vec ());  \
        U8_FCN (in, m_matrix.numel (), nullptr, buf, &output_length);          \
        if (output_length != static_cast<std::size_t> (m_matrix.numel ()))     \
          {                                                                    \
            warning_with_id ("Octave:multi_byte_char_length",                  \
                             "UMAP: Possible multi-byte error.");              \
            return octave_value (m_matrix.map<char, int (&) (int)> (STD_FCN)); \
          }                                                                    \
        return octave_value ((m_matrix.ndims () > 1) ? ch_array.permute (p, true)\
                                                     : ch_array);              \
      }

      STRING_U8_FCN (xtolower, octave_u8_tolower_wrapper, std::tolower);
      STRING_U8_FCN (xtoupper, octave_u8_toupper_wrapper, std::toupper);

    // For Matlab compatibility, these should work on ASCII values
    // without error or warning.
    case umap_abs:
    case umap_ceil:
    case umap_fix:
    case umap_floor:
    case umap_imag:
    case umap_isfinite:
    case umap_isinf:
    case umap_isnan:
    case umap_real:
    case umap_round:
      {
        octave_matrix m (array_value (true));
        return m.map (umap);
      }

    default:
      error ("%s: argument must be numeric", get_umap_name (umap));
      break;
    }

  return retval;
}
