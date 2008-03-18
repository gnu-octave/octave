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

#if !defined (octave_inttypes_h)
#define octave_inttypes_h 1

#include <climits>

#include <limits>
#include <iostream>

#include "oct-types.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

template <class T1, class T2>
class
octave_int_binop_traits
{
public:
  // The return type for a T1 by T2 binary operation.
  typedef T1 TR;
};

#define OCTAVE_INT_BINOP_TRAIT(T1, T2, T3) \
  template<> \
  class octave_int_binop_traits <T1, T2> \
  { \
  public: \
    typedef T3 TR; \
  }

OCTAVE_INT_BINOP_TRAIT (int8_t, int8_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, int16_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, int32_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, int64_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, uint8_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, uint16_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, uint32_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (int8_t, uint64_t, int8_t);

OCTAVE_INT_BINOP_TRAIT (int16_t, int8_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, int16_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, int32_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, int64_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, uint8_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, uint16_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, uint32_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (int16_t, uint64_t, int16_t);

OCTAVE_INT_BINOP_TRAIT (int32_t, int8_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, int16_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, int32_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, int64_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, uint8_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, uint16_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, uint32_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (int32_t, uint64_t, int32_t);

OCTAVE_INT_BINOP_TRAIT (int64_t, int8_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, int16_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, int32_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, int64_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, uint8_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, uint16_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, uint32_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (int64_t, uint64_t, int64_t);

OCTAVE_INT_BINOP_TRAIT (uint8_t, int8_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, int16_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, int32_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, int64_t, int8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, uint8_t, uint8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, uint16_t, uint8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, uint32_t, uint8_t);
OCTAVE_INT_BINOP_TRAIT (uint8_t, uint64_t, uint8_t);

OCTAVE_INT_BINOP_TRAIT (uint16_t, int8_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, int16_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, int32_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, int64_t, int16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, uint8_t, uint16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, uint16_t, uint16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, uint32_t, uint16_t);
OCTAVE_INT_BINOP_TRAIT (uint16_t, uint64_t, uint16_t);

OCTAVE_INT_BINOP_TRAIT (uint32_t, int8_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, int16_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, int32_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, int64_t, int32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, uint8_t, uint32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, uint16_t, uint32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, uint32_t, uint32_t);
OCTAVE_INT_BINOP_TRAIT (uint32_t, uint64_t, uint32_t);

OCTAVE_INT_BINOP_TRAIT (uint64_t, int8_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, int16_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, int32_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, int64_t, int64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, uint8_t, uint64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, uint16_t, uint64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, uint32_t, uint64_t);
OCTAVE_INT_BINOP_TRAIT (uint64_t, uint64_t, uint64_t);

template <class T1, class T2>
inline T2
octave_int_fit_to_range (const T1& x, const T2& mn, const T2& mx)
{
  return (x > mx ? mx : (x < mn ? mn : T2 (x)));
}

#define OCTAVE_DBL_FTR(T) \
  template <> \
  inline T \
  octave_int_fit_to_range<double, T> (const double& x, const T& mn, \
				      const T& mx) \
  { \
    return (lo_ieee_isnan (x) ? 0 : (x > mx ? mx : (x < mn ? mn : static_cast<T> (x)))); \
  }

OCTAVE_DBL_FTR (char)
OCTAVE_DBL_FTR (short)
OCTAVE_DBL_FTR (int)
OCTAVE_DBL_FTR (long)
OCTAVE_DBL_FTR (long long)

OCTAVE_DBL_FTR (unsigned char)
OCTAVE_DBL_FTR (unsigned short)
OCTAVE_DBL_FTR (unsigned int)
OCTAVE_DBL_FTR (unsigned long)
OCTAVE_DBL_FTR (unsigned long long)

// If X is unsigned and the new type is signed, then we only have to
// check the upper limit, but we should cast the maximum value of the
// new type to an unsigned type before performing the comparison.
// This should always be OK because the maximum value should always be
// positive.

#define OCTAVE_US_S_FTR(T1, T2, TC) \
  template <> \
  inline T2 \
  octave_int_fit_to_range<T1, T2> (const T1& x, const T2&, const T2& mx) \
  { \
    return x > static_cast<TC> (mx) ? mx : x; \
  }

#define OCTAVE_US_S_FTR_FCNS(T) \
  OCTAVE_US_S_FTR (T, char, unsigned char) \
  OCTAVE_US_S_FTR (T, signed char, unsigned char) \
  OCTAVE_US_S_FTR (T, short, unsigned short) \
  OCTAVE_US_S_FTR (T, int, unsigned int) \
  OCTAVE_US_S_FTR (T, long, unsigned long) \
  OCTAVE_US_S_FTR (T, long long, unsigned long long)

OCTAVE_US_S_FTR_FCNS (unsigned char)
OCTAVE_US_S_FTR_FCNS (unsigned short)
OCTAVE_US_S_FTR_FCNS (unsigned int)
OCTAVE_US_S_FTR_FCNS (unsigned long)
OCTAVE_US_S_FTR_FCNS (unsigned long long)

// If X is signed and the new type is unsigned, then we only have to
// check the lower limit (which will always be 0 for an unsigned
// type).  The upper limit will be enforced correctly by converting to
// the new type, even if the type of X is wider than the new type.

#define OCTAVE_S_US_FTR(T1, T2) \
  template <> \
  inline T2 \
  octave_int_fit_to_range<T1, T2> (const T1& x, const T2&, const T2&) \
  { \
    return x <= 0 ? 0 : x; \
  }

#define OCTAVE_S_US_FTR_FCNS(T) \
  OCTAVE_S_US_FTR (T, unsigned char) \
  OCTAVE_S_US_FTR (T, unsigned short) \
  OCTAVE_S_US_FTR (T, unsigned int) \
  OCTAVE_S_US_FTR (T, unsigned long) \
  OCTAVE_S_US_FTR (T, unsigned long long)

OCTAVE_S_US_FTR_FCNS (char)
OCTAVE_S_US_FTR_FCNS (signed char)
OCTAVE_S_US_FTR_FCNS (short)
OCTAVE_S_US_FTR_FCNS (int)
OCTAVE_S_US_FTR_FCNS (long)
OCTAVE_S_US_FTR_FCNS (long long)

#define OCTAVE_INT_FIT_TO_RANGE(r, T) \
  octave_int_fit_to_range (r, \
                           std::numeric_limits<T>::min (), \
                           std::numeric_limits<T>::max ())

#define OCTAVE_INT_MIN_VAL2(T1, T2) \
  std::numeric_limits<typename octave_int_binop_traits<T1, T2>::TR>::min ()

#define OCTAVE_INT_MAX_VAL2(T1, T2) \
  std::numeric_limits<typename octave_int_binop_traits<T1, T2>::TR>::max ()

#define OCTAVE_INT_FIT_TO_RANGE2(r, T1, T2) \
  octave_int_fit_to_range (r, \
                           OCTAVE_INT_MIN_VAL2 (T1, T2), \
                           OCTAVE_INT_MAX_VAL2 (T1, T2))

// We have all the machinery below (octave_int_helper) to avoid a few
// warnings from GCC about comparisons always false due to limited
// range of data types.  Ugh.  The cure may be worse than the disease.

// FIXME -- it would be nice to nest the helper class inside the
// octave_int class, but I don't see the magic for that at the moment.

template <class T> class octave_int;

template <class T, bool is_signed>
class octave_int_helper
{
public:
  static octave_int<T> abs (const T& x);

  static octave_int<T> signum (const T& x);

  template <class T2> static void rshift_eq (T& ival, const T2& x);
};

template <class T>
class octave_int_helper<T, false>
{
public:
  static octave_int<T>
  abs (const T& x) { return x; }

  static octave_int<T>
  signum (const T& x) { return x > 0 ? 1 : 0; }

  template <class T2>
  static void
  rshift_eq (T& ival, const T2& x) { ival = ival >> x; }
};

template <class T>
class octave_int_helper<T, true>
{
public:
  static octave_int<T>
  abs (const T& x) { return x < 0 ? -x : x; }

  static octave_int<T>
  signum (const T& x) { return x < 0 ? -1 : (x > 0 ? 1 : 0); }

  template <class T2>
  static void
  rshift_eq (T& ival, const T2& x)
  {
    if (ival < 0)
      ival = - (((-ival) >> x) & std::numeric_limits<T>::max());
    else
      ival = ival >> x;
  }
};

template <class T>
class
octave_int
{
public:

  typedef T val_type;

  octave_int (void) : ival () { }

  template <class U>
  octave_int (U i) : ival (OCTAVE_INT_FIT_TO_RANGE (i, T)) { }

  octave_int (double d) : ival (OCTAVE_INT_FIT_TO_RANGE (xround (d), T)) { }

  octave_int (bool b) : ival (b) { }

  template <class U>
  octave_int (const octave_int<U>& i)
    : ival (OCTAVE_INT_FIT_TO_RANGE (i.value (), T)) { }

  octave_int (const octave_int<T>& i) : ival (i.ival) { }

  octave_int& operator = (const octave_int<T>& i)
  {
    ival = i.ival;
    return *this;
  }

  ~octave_int (void) { }
  
  T value (void) const { return ival; }

  const unsigned char * iptr (void) const
  { return reinterpret_cast<const unsigned char *> (& ival); }

  bool operator ! (void) const { return ! ival; }

  octave_int<T> operator + (void) const { return *this; }

  octave_int<T> operator - (void) const
  {
    // Can't just return -ival because signed types are not
    // symmetric, which causes things like -intmin("int32") to be the
    // same as intmin("int32") instead of intmax("int32") (which is
    // what we should get with saturation semantics).

    return std::numeric_limits<T>::is_signed ?
      OCTAVE_INT_FIT_TO_RANGE (- static_cast<double> (ival), T) : 0;
  }

  bool bool_value (void) const { return static_cast<bool> (value ()); }

  char char_value (void) const { return static_cast<char> (value ()); }

  double double_value (void) const { return static_cast<double> (value ()); }

  float float_value (void) const { return static_cast<float> (value ()); }

  operator T (void) const { return value (); }

  // char and bool operators intentionally omitted.

  operator double (void) const { return double_value (); }

  operator float (void) const { return float_value (); }

  octave_int<T>& operator += (const octave_int<T>& x)
  {
    double t = static_cast<double> (value ());
    double tx = static_cast<double> (x.value ());
    ival = OCTAVE_INT_FIT_TO_RANGE (t + tx, T);
    return *this;
  }

  octave_int<T>& operator -= (const octave_int<T>& x)
  {
    double t = static_cast<double> (value ());
    double tx = static_cast<double> (x.value ());
    ival = OCTAVE_INT_FIT_TO_RANGE (t - tx, T);
    return *this;
  }

  octave_int<T>& operator *= (const octave_int<T>& x)
  {
    double t = static_cast<double> (value ());
    double tx = static_cast<double> (x.value ());
    ival = OCTAVE_INT_FIT_TO_RANGE (t * tx, T);
    return *this;
  }

  octave_int<T>& operator /= (const octave_int<T>& x)
  {
    double t = static_cast<double> (value ());
    double tx = static_cast<double> (x.value ());
    double r = (t == 0 && tx == 0) ? 0 : xround (t / tx);
    ival = OCTAVE_INT_FIT_TO_RANGE (r, T);
    return *this;
  }

  template <class T2>
  octave_int<T>& operator <<= (const T2& x)
  {
    ival = ival << x;
    return *this;
  }

  // Use helper functions in the operator >>=, abs, and signum
  // functions to avoid "comparison of unsigned expression < 0 is
  // always false" warnings from GCC when instantiating these funtions
  // for unsigned types.

  template <class T2>
  octave_int<T>& operator >>= (const T2& x)
  {
    octave_int_helper<T, std::numeric_limits<T>::is_signed>::rshift_eq (ival, x);
    return *this;
  }

  octave_int<T> abs (void) const
  {
    return octave_int_helper<T, std::numeric_limits<T>::is_signed>::abs (value ());
  }

  octave_int<T> signum (void) const 
  { 
    return octave_int_helper<T, std::numeric_limits<T>::is_signed>::signum (value ());
  }

  octave_int<T> min (void) const { return std::numeric_limits<T>::min (); }
  octave_int<T> max (void) const { return std::numeric_limits<T>::max (); }

  static int nbits (void) { return sizeof (T) * CHAR_BIT; }

  static int byte_size (void) { return sizeof(T); }

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return const_cast<T *> (&ival); }

private:

  T ival;
};

template <class T>
octave_int<T>
pow (const octave_int<T>& a, const octave_int<T>& b)
{
  octave_int<T> retval;

  octave_int<T> zero = octave_int<T> (0);
  octave_int<T> one = octave_int<T> (1);

  if (b == zero)
    retval = one;
  else if (b < zero)
    retval = zero;
  else
    {
      octave_int<T> a_val = a;
      octave_int<T> b_val = b;

      retval = a;

      b_val -= 1;

      while (b_val != zero)
	{
	  if ((b_val & one) != zero)
	    retval = retval * a_val;

	  b_val = b_val >> 1;

	  if (b_val > zero)
	    a_val = a_val * a_val;
	}
    }

  return retval;
}

template <class T>
octave_int<T>
pow (double a, const octave_int<T>& b)
{
  double tb = static_cast<double> (b.value ());
  double r = pow (a, tb);
  r = lo_ieee_isnan (r) ? 0 : xround (r);
  return OCTAVE_INT_FIT_TO_RANGE (r, T);
}

template <class T>
octave_int<T>
pow (const octave_int<T>& a, double b)
{
  double ta = static_cast<double> (a.value ());
  double r = pow (ta, b);
  r = lo_ieee_isnan (r) ? 0 : xround (r);
  return OCTAVE_INT_FIT_TO_RANGE (r, T);
}

template <class T>
std::ostream&
operator << (std::ostream& os, const octave_int<T>& ival)
{
  os << ival.value ();
  return os;
}

template <class T>
std::istream&
operator >> (std::istream& is, octave_int<T>& ival)
{
  T tmp = 0;
  is >> tmp;
  ival = tmp;
  return is;
}

typedef octave_int<int8_t> octave_int8;
typedef octave_int<int16_t> octave_int16;
typedef octave_int<int32_t> octave_int32;
typedef octave_int<int64_t> octave_int64;

typedef octave_int<uint8_t> octave_uint8;
typedef octave_int<uint16_t> octave_uint16;
typedef octave_int<uint32_t> octave_uint32;
typedef octave_int<uint64_t> octave_uint64;

#define OCTAVE_INT_BIN_OP(OP) \
  template <class T1, class T2> \
  octave_int<typename octave_int_binop_traits<T1, T2>::TR> \
  operator OP (const octave_int<T1>& x, const octave_int<T2>& y) \
  { \
    double tx = static_cast<double> (x.value ()); \
    double ty = static_cast<double> (y.value ()); \
    double r = tx OP ty; \
    return OCTAVE_INT_FIT_TO_RANGE2 (r, T1, T2); \
  }

OCTAVE_INT_BIN_OP(+)
OCTAVE_INT_BIN_OP(-)
OCTAVE_INT_BIN_OP(*)

template <class T1, class T2>
octave_int<typename octave_int_binop_traits<T1, T2>::TR>
operator / (const octave_int<T1>& x, const octave_int<T2>& y)
{
  double tx = static_cast<double> (x.value ());
  double ty = static_cast<double> (y.value ());
  double r = (tx == 0 && ty == 0) ? 0 : xround (tx / ty);
  return OCTAVE_INT_FIT_TO_RANGE2 (r, T1, T2);
}

#define OCTAVE_INT_DOUBLE_BIN_OP(OP) \
  template <class T> \
  octave_int<T> \
  operator OP (const octave_int<T>& x, double y) \
  { \
    double tx = static_cast<double> (x.value ()); \
    double r = xround (tx OP y); \
    r = lo_ieee_isnan (r) ? 0 : xround (r); \
    return OCTAVE_INT_FIT_TO_RANGE (r, T); \
  }

OCTAVE_INT_DOUBLE_BIN_OP(+)
OCTAVE_INT_DOUBLE_BIN_OP(-)
OCTAVE_INT_DOUBLE_BIN_OP(*)
OCTAVE_INT_DOUBLE_BIN_OP(/)

#define OCTAVE_DOUBLE_INT_BIN_OP(OP) \
  template <class T> \
  octave_int<T> \
  operator OP (double x, const octave_int<T>& y) \
  { \
    double ty = static_cast<double> (y.value ()); \
    double r = x OP ty; \
    r = lo_ieee_isnan (r) ? 0 : xround (r); \
    return OCTAVE_INT_FIT_TO_RANGE (r, T); \
  }

OCTAVE_DOUBLE_INT_BIN_OP(+)
OCTAVE_DOUBLE_INT_BIN_OP(-)
OCTAVE_DOUBLE_INT_BIN_OP(*)
OCTAVE_DOUBLE_INT_BIN_OP(/)

#define OCTAVE_INT_DOUBLE_CMP_OP(OP) \
  template <class T> \
  bool \
  operator OP (const octave_int<T>& x, const double& y) \
  { \
    double tx = static_cast<double> (x.value ()); \
    return tx OP y; \
  }

OCTAVE_INT_DOUBLE_CMP_OP (<)
OCTAVE_INT_DOUBLE_CMP_OP (<=)
OCTAVE_INT_DOUBLE_CMP_OP (>=)
OCTAVE_INT_DOUBLE_CMP_OP (>)
OCTAVE_INT_DOUBLE_CMP_OP (==)
OCTAVE_INT_DOUBLE_CMP_OP (!=)

#define OCTAVE_DOUBLE_INT_CMP_OP(OP) \
  template <class T> \
  bool \
  operator OP (const double& x, const octave_int<T>& y) \
  { \
    double ty = static_cast<double> (y.value ()); \
    return x OP ty; \
  }

OCTAVE_DOUBLE_INT_CMP_OP (<)
OCTAVE_DOUBLE_INT_CMP_OP (<=)
OCTAVE_DOUBLE_INT_CMP_OP (>=)
OCTAVE_DOUBLE_INT_CMP_OP (>)
OCTAVE_DOUBLE_INT_CMP_OP (==)
OCTAVE_DOUBLE_INT_CMP_OP (!=)

#define OCTAVE_INT_BITCMP_OP(OP) \
  template <class T> \
  octave_int<T> \
  operator OP (const octave_int<T>& x, const octave_int<T>& y) \
  { \
    return x.value () OP y.value (); \
  }

OCTAVE_INT_BITCMP_OP (&)
OCTAVE_INT_BITCMP_OP (|)
OCTAVE_INT_BITCMP_OP (^)

template <class T1, class T2>
octave_int<T1>
operator << (const octave_int<T1>& x, const T2& y)
{
  octave_int<T1> retval = x;
  return retval <<= y;
}

template <class T1, class T2>
octave_int<T1>
operator >> (const octave_int<T1>& x, const T2& y)
{
  octave_int<T1> retval = x;
  return retval >>= y;
}

template <class T>
octave_int<T>
bitshift (const octave_int<T>& a, int n,
	  const octave_int<T>& mask = std::numeric_limits<T>::max ())
{
  if (n > 0)
    return (a << n) & mask;
  else if (n < 0)
    return (a >> -n) & mask;
  else
    return a;
}

#define OCTAVE_INT_CMP_OP(OP) \
  template <class T1, class T2> \
  bool \
  operator OP (const octave_int<T1>& x, const octave_int<T2>& y) \
  { \
    return x.value () OP y.value (); \
  }

OCTAVE_INT_CMP_OP (<)
OCTAVE_INT_CMP_OP (<=)
OCTAVE_INT_CMP_OP (>=)
OCTAVE_INT_CMP_OP (>)
OCTAVE_INT_CMP_OP (==)
OCTAVE_INT_CMP_OP (!=)

// The following apply if the unsigned type is at least as wide as the
// signed type (then we can cast postive signed values to the unsigned
// type and compare).

#define OCTAVE_US_TYPE1_CMP_OP_DECL(OP, LTZ_VAL, UT, ST) \
  template <> \
  bool \
  OCTAVE_API operator OP (const octave_int<UT>& lhs, const octave_int<ST>& rhs);

#define OCTAVE_US_TYPE1_CMP_OP_DECLS(UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECL (<, false, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECL (<=, false, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECL (>=, true, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECL (>, true, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECL (==, false, UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECL (!=, true, UT, ST)

#define OCTAVE_SU_TYPE1_CMP_OP_DECL(OP, LTZ_VAL, ST, UT) \
  template <> \
  bool \
  OCTAVE_API operator OP (const octave_int<ST>& lhs, const octave_int<UT>& rhs);

#define OCTAVE_SU_TYPE1_CMP_OP_DECLS(ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP_DECL (<, true, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP_DECL (<=, true, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP_DECL (>=, false, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP_DECL (>, false, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP_DECL (==, false, ST, UT) \
  OCTAVE_SU_TYPE1_CMP_OP_DECL (!=, true, ST, UT)

#define OCTAVE_TYPE1_CMP_OP_DECLS(UT, ST) \
  OCTAVE_US_TYPE1_CMP_OP_DECLS (UT, ST) \
  OCTAVE_SU_TYPE1_CMP_OP_DECLS (ST, UT)

OCTAVE_TYPE1_CMP_OP_DECLS (uint32_t, int8_t)
OCTAVE_TYPE1_CMP_OP_DECLS (uint32_t, int16_t)
OCTAVE_TYPE1_CMP_OP_DECLS (uint32_t, int32_t)

OCTAVE_TYPE1_CMP_OP_DECLS (uint64_t, int8_t)
OCTAVE_TYPE1_CMP_OP_DECLS (uint64_t, int16_t)
OCTAVE_TYPE1_CMP_OP_DECLS (uint64_t, int32_t)
OCTAVE_TYPE1_CMP_OP_DECLS (uint64_t, int64_t)

// The following apply if the signed type is wider than the unsigned
// type (then we can cast unsigned values to the signed type and
// compare if the signed value is positive).

#define OCTAVE_US_TYPE2_CMP_OP_DECL(OP, LTZ_VAL, UT, ST) \
  template <> \
  bool \
  OCTAVE_API operator OP (const octave_int<UT>& lhs, const octave_int<ST>& rhs);

#define OCTAVE_US_TYPE2_CMP_OP_DECLS(ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP_DECL (<, false, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP_DECL (<=, false, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP_DECL (>=, true, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP_DECL (>, true, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP_DECL (==, false, ST, UT) \
  OCTAVE_US_TYPE2_CMP_OP_DECL (!=, true, ST, UT)

#define OCTAVE_SU_TYPE2_CMP_OP_DECL(OP, LTZ_VAL, ST, UT) \
  template <> \
  bool \
  OCTAVE_API operator OP (const octave_int<ST>& lhs, const octave_int<UT>& rhs);

#define OCTAVE_SU_TYPE2_CMP_OP_DECLS(ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP_DECL (<, true, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP_DECL (<=, true, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP_DECL (>=, false, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP_DECL (>, false, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP_DECL (==, false, ST, UT) \
  OCTAVE_SU_TYPE2_CMP_OP_DECL (!=, true, ST, UT)

#define OCTAVE_TYPE2_CMP_OP_DECLS(UT, ST) \
  OCTAVE_US_TYPE2_CMP_OP_DECLS (UT, ST) \
  OCTAVE_SU_TYPE2_CMP_OP_DECLS (ST, UT)

OCTAVE_TYPE2_CMP_OP_DECLS (uint32_t, int64_t)

#undef OCTAVE_INT_BINOP_TRAIT
#undef OCTAVE_US_S_FTR
#undef OCTAVE_US_S_FTR_FCNS
#undef OCTAVE_S_US_FTR
#undef OCTAVE_S_US_FTR_FCNS
#undef OCTAVE_INT_FIT_TO_RANGE
#undef OCTAVE_INT_MIN_VAL2
#undef OCTAVE_INT_MAX_VAL2
#undef OCTAVE_INT_FIT_TO_RANGE2
#undef OCTAVE_INT_BIN_OP
#undef OCTAVE_INT_DOUBLE_BIN_OP
#undef OCTAVE_DOUBLE_INT_BIN_OP
#undef OCTAVE_INT_DOUBLE_CMP_OP
#undef OCTAVE_DOUBLE_INT_CMP_OP
#undef OCTAVE_INT_BITCMP_OP
#undef OCTAVE_INT_CMP_OP
#undef OCTAVE_US_TYPE1_CMP_OP_DECL
#undef OCTAVE_US_TYPE1_CMP_OP_DECLS
#undef OCTAVE_SU_TYPE1_CMP_OP_DECL
#undef OCTAVE_SU_TYPE1_CMP_OP_DECLS
#undef OCTAVE_TYPE1_CMP_OP_DECLS
#undef OCTAVE_US_TYPE2_CMP_OP_DECL
#undef OCTAVE_US_TYPE2_CMP_OP_DECLS
#undef OCTAVE_SU_TYPE2_CMP_OP_DECL
#undef OCTAVE_SU_TYPE2_CMP_OP_DECLS
#undef OCTAVE_TYPE2_CMP_OP_DECLS

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
