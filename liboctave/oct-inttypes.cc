/*

Copyright (C) 2004, 2005, 2006, 2007 John W. Eaton

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

#include "lo-error.h"

#include "oct-inttypes.h"

// define type names. 
#define DECLARE_OCTAVE_INT_TYPENAME(TYPE, TYPENAME) \
  template <> \
  const char * \
  octave_int<TYPE>::type_name () { return TYPENAME; }

DECLARE_OCTAVE_INT_TYPENAME (int8_t, "int8")
DECLARE_OCTAVE_INT_TYPENAME (int16_t, "int16")
DECLARE_OCTAVE_INT_TYPENAME (int32_t, "int32")
DECLARE_OCTAVE_INT_TYPENAME (int64_t, "int64")
DECLARE_OCTAVE_INT_TYPENAME (uint8_t, "uint8")
DECLARE_OCTAVE_INT_TYPENAME (uint16_t, "uint16")
DECLARE_OCTAVE_INT_TYPENAME (uint32_t, "uint32")
DECLARE_OCTAVE_INT_TYPENAME (uint64_t, "uint64")

static void
gripe_64bit_mul()
{
  (*current_liboctave_error_handler) 
    ("64-bit integer multiplication is not implemented");
}

template <>
uint64_t 
octave_int_arith_base<uint64_t, false>::mul (uint64_t, uint64_t)
{ 
  gripe_64bit_mul (); 
  return static_cast<uint64_t> (0);
}
template <>
int64_t 
octave_int_arith_base<int64_t, true>::mul (int64_t, int64_t)
{ 
  gripe_64bit_mul (); 
  return static_cast<int64_t> (0);
}

static void
gripe_64bit_mixed()
{
  (*current_liboctave_error_handler) 
    ("mixed double - 64-bit integer operations are not implemented");
}

#define DEFINE_DOUBLE_INT64_OP0(OP,ARGT,RETT) \
  template <> \
  OCTAVE_API RETT \
  operator OP (const double&, const ARGT&) \
  { gripe_64bit_mixed (); return 0.0; } \
  template <> \
  OCTAVE_API RETT \
  operator OP (const ARGT&, const double&) \
  { gripe_64bit_mixed (); return 0.0; } \

#define DEFINE_DOUBLE_INT64_BIN_OP(OP) \
  DEFINE_DOUBLE_INT64_OP0(OP,octave_int64,octave_int64) \
  DEFINE_DOUBLE_INT64_OP0(OP,octave_uint64,octave_uint64) 

DEFINE_DOUBLE_INT64_BIN_OP(+)
DEFINE_DOUBLE_INT64_BIN_OP(-)
DEFINE_DOUBLE_INT64_BIN_OP(*)
DEFINE_DOUBLE_INT64_BIN_OP(/)

#define DEFINE_DOUBLE_INT64_CMP_OP(OP) \
  DEFINE_DOUBLE_INT64_OP0(OP,octave_int64,bool) \
  DEFINE_DOUBLE_INT64_OP0(OP,octave_uint64,bool) 

DEFINE_DOUBLE_INT64_CMP_OP(<)
DEFINE_DOUBLE_INT64_CMP_OP(<=)
DEFINE_DOUBLE_INT64_CMP_OP(>)
DEFINE_DOUBLE_INT64_CMP_OP(>=)
DEFINE_DOUBLE_INT64_CMP_OP(==)
DEFINE_DOUBLE_INT64_CMP_OP(!=)

//template <class T>
//bool
//xisnan (const octave_int<T>&)
//{
//  return false;
//}

template <class T>
octave_int<T>
pow (const octave_int<T>& a, const octave_int<T>& b)
{
  octave_int<T> retval;

  octave_int<T> zero = octave_int<T> (0);
  octave_int<T> one = octave_int<T> (1);

  if (b == zero || a == one)
    retval = one;
  else if (b < zero)
    {
      if (std::numeric_limits<T>::is_signed && a.value () == -1)
        retval = (b.value () % 2) ? a : one;
      else
        retval = zero;
    }
  else
    {
      octave_int<T> a_val = a;
      T b_val = b; // no need to do saturation on b

      retval = a;

      b_val -= 1;

      while (b_val != 0)
	{
	  if (b_val & 1)
	    retval = retval * a_val;

	  b_val = b_val >> 1;

	  if (b_val)
	    a_val = a_val * a_val;
	}
    }

  return retval;
}

template <class T>
octave_int<T>
pow (const double& a, const octave_int<T>& b)
{ return octave_int<T> (pow (a, b.double_value ())); }

template <class T>
octave_int<T>
pow (const octave_int<T>& a, const double& b)
{ return octave_int<T> (pow (a.double_value (), b)); }

template <class T>
octave_int<T>
powf (const float& a, const octave_int<T>& b)
{ return octave_int<T> (pow (a, b.float_value ())); }

template <class T>
octave_int<T>
powf (const octave_int<T>& a, const float& b)
{ return octave_int<T> (pow (a.float_value (), b)); }

#define INSTANTIATE_INTTYPE(T) \
  template class OCTAVE_API octave_int<T>; \
  template OCTAVE_API octave_int<T> pow (const octave_int<T>&, const octave_int<T>&); \
  template OCTAVE_API octave_int<T> pow (const double&, const octave_int<T>&); \
  template OCTAVE_API octave_int<T> pow (const octave_int<T>&, const double&); \
  template OCTAVE_API octave_int<T> powf (const float&, const octave_int<T>&); \
  template OCTAVE_API octave_int<T> powf (const octave_int<T>&, const float&); \
  template OCTAVE_API std::ostream& operator << (std::ostream&, const octave_int<T>&); \
  template OCTAVE_API std::istream& operator >> (std::istream&, octave_int<T>&); \
  template OCTAVE_API octave_int<T> \
  bitshift (const octave_int<T>&, int, const octave_int<T>&); \

INSTANTIATE_INTTYPE (int8_t);
INSTANTIATE_INTTYPE (int16_t);
INSTANTIATE_INTTYPE (int32_t);
INSTANTIATE_INTTYPE (int64_t);

INSTANTIATE_INTTYPE (uint8_t);
INSTANTIATE_INTTYPE (uint16_t);
INSTANTIATE_INTTYPE (uint32_t);
INSTANTIATE_INTTYPE (uint64_t);


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
