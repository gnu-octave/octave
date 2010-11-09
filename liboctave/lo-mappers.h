/*

Copyright (C) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004, 2005,
              2006, 2007, 2008 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if !defined (octave_liboctave_mappers_h)
#define octave_liboctave_mappers_h 1

#include "oct-cmplx.h"
#include "lo-math.h"

// Double Precision 
extern OCTAVE_API double arg (double x);
extern OCTAVE_API double conj (double x);
extern OCTAVE_API double fix (double x);
extern OCTAVE_API double imag (double x);
extern OCTAVE_API double real (double x);
extern OCTAVE_API double xround (double x);
extern OCTAVE_API double xroundb (double x);
extern OCTAVE_API double signum (double x);
extern OCTAVE_API double xtrunc (double x);
extern OCTAVE_API double xmod (double x, double y);
extern OCTAVE_API double xrem (double x, double y);
extern OCTAVE_API double xlog2 (double x); 
extern OCTAVE_API Complex xlog2 (const Complex& x); 
extern OCTAVE_API double xlog2 (double x, int& exp);
extern OCTAVE_API Complex xlog2 (const Complex& x, int& exp);
extern OCTAVE_API double xexp2 (double x);

// These are used by the BOOL_OP macros in mx-op-defs.h.
inline bool xisnan (bool) { return false; }
inline bool xisnan (char) { return false; }

#if defined (HAVE_CMATH_ISNAN)
inline bool xisnan (double x)
{ return std::isnan (x); }
#else
extern OCTAVE_API bool xisnan (double x);
#endif
#if defined (HAVE_CMATH_ISFINITE)
inline bool xfinite (double x)
{ return std::isfinite (x); }
#else
extern OCTAVE_API bool xfinite (double x);
#endif
#if defined (HAVE_CMATH_ISINF)
inline bool xisinf (double x)
{ return std::isinf (x); }
#else
extern OCTAVE_API bool xisinf (double x);
#endif

extern OCTAVE_API bool octave_is_NA (double x);
extern OCTAVE_API bool octave_is_NaN_or_NA (double x) GCC_ATTR_DEPRECATED;

// Generic xmin, xmax definitions
template <class T>
inline T xmin (T x, T y)
{ return x <= y ? x : y; }
template <class T>
inline T xmax (T x, T y)
{ return x >= y ? x : y; }

// This form is favorable. GCC will translate (x <= y ? x : y) without a jump,
// hence the only conditional jump involved will be the first (xisnan), infrequent
// and hence friendly to branch prediction.
inline double xmin (double x, double y)
{ return xisnan (y) ? x : (x <= y ? x : y);; }
inline double xmax (double x, double y)
{ return xisnan (y) ? x : (x >= y ? x : y);; }

extern OCTAVE_API Complex acos (const Complex& x);
extern OCTAVE_API Complex acosh (const Complex& x);
extern OCTAVE_API Complex asin (const Complex& x);
extern OCTAVE_API Complex asinh (const Complex& x);
extern OCTAVE_API Complex atan (const Complex& x);
extern OCTAVE_API Complex atanh (const Complex& x);

extern OCTAVE_API Complex ceil (const Complex& x);
extern OCTAVE_API Complex fix (const Complex& x);
extern OCTAVE_API Complex floor (const Complex& x);
extern OCTAVE_API Complex xround (const Complex& x);
extern OCTAVE_API Complex xroundb (const Complex& x);
extern OCTAVE_API Complex signum (const Complex& x);

inline bool
xisnan (const Complex& x)
{ return (xisnan (real (x)) || xisnan (imag (x))); }
inline bool
xfinite (const Complex& x)
{ return (xfinite (real (x)) && xfinite (imag (x))); }
inline bool
xisinf (const Complex& x)
{ return (xisinf (real (x)) || xisinf (imag (x))); }

extern OCTAVE_API bool octave_is_NA (const Complex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const Complex& x);

extern OCTAVE_API Complex xmin (const Complex& x, const Complex& y);
extern OCTAVE_API Complex xmax (const Complex& x, const Complex& y);

// Single Precision 
extern OCTAVE_API float arg (float x);
extern OCTAVE_API float conj (float x);
extern OCTAVE_API float fix (float x);
extern OCTAVE_API float imag (float x);
extern OCTAVE_API float real (float x);
extern OCTAVE_API float xround (float x);
extern OCTAVE_API float xroundb (float x);
extern OCTAVE_API float signum (float x);
extern OCTAVE_API float xtrunc (float x);
extern OCTAVE_API float xmod (float x, float y);
extern OCTAVE_API float xrem (float x, float y);
extern OCTAVE_API float xlog2 (float x); 
extern OCTAVE_API FloatComplex xlog2 (const FloatComplex& x); 
extern OCTAVE_API float xlog2 (float x, int& exp);
extern OCTAVE_API FloatComplex xlog2 (const FloatComplex& x, int& exp);
extern OCTAVE_API float xexp2 (float x);

#if defined (HAVE_CMATH_ISNANF)
inline bool xisnan (float x)
{ return std::isnan (x); }
#else
extern OCTAVE_API bool xisnan (float x);
#endif
#if defined (HAVE_CMATH_ISFINITEF)
inline bool xfinite (float x)
{ return std::isfinite (x); }
#else
extern OCTAVE_API bool xfinite (float x);
#endif
#if defined (HAVE_CMATH_ISINFF)
inline bool xisinf (float x)
{ return std::isinf (x); }
#else
extern OCTAVE_API bool xisinf (float x);
#endif


extern OCTAVE_API bool octave_is_NA (float x);
extern OCTAVE_API bool octave_is_NaN_or_NA (float x) GCC_ATTR_DEPRECATED;

inline float xmin (float x, float y)
{ return xisnan (y) ? x : (x <= y ? x : y);; }
inline float xmax (float x, float y)
{ return xisnan (y) ? x : (x >= y ? x : y);; }

extern OCTAVE_API FloatComplex acos (const FloatComplex& x);
extern OCTAVE_API FloatComplex acosh (const FloatComplex& x);
extern OCTAVE_API FloatComplex asin (const FloatComplex& x);
extern OCTAVE_API FloatComplex asinh (const FloatComplex& x);
extern OCTAVE_API FloatComplex atan (const FloatComplex& x);
extern OCTAVE_API FloatComplex atanh (const FloatComplex& x);

extern OCTAVE_API FloatComplex ceil (const FloatComplex& x);
extern OCTAVE_API FloatComplex fix (const FloatComplex& x);
extern OCTAVE_API FloatComplex floor (const FloatComplex& x);
extern OCTAVE_API FloatComplex xround (const FloatComplex& x);
extern OCTAVE_API FloatComplex xroundb (const FloatComplex& x);
extern OCTAVE_API FloatComplex signum (const FloatComplex& x);

inline bool
xisnan (const FloatComplex& x)
{ return (xisnan (real (x)) || xisnan (imag (x))); }
inline bool
xfinite (const FloatComplex& x)
{ return (xfinite (real (x)) && xfinite (imag (x))); }
inline bool
xisinf (const FloatComplex& x)
{ return (xisinf (real (x)) || xisinf (imag (x))); }

extern OCTAVE_API bool octave_is_NA (const FloatComplex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const FloatComplex& x);

extern OCTAVE_API FloatComplex xmin (const FloatComplex& x, const FloatComplex& y);
extern OCTAVE_API FloatComplex xmax (const FloatComplex& x, const FloatComplex& y);

// These map reals to Complex.

extern OCTAVE_API Complex rc_acos (double);
extern OCTAVE_API FloatComplex rc_acos (float);
extern OCTAVE_API Complex rc_acosh (double);
extern OCTAVE_API FloatComplex rc_acosh (float);
extern OCTAVE_API Complex rc_asin (double);
extern OCTAVE_API FloatComplex rc_asin (float);
extern OCTAVE_API Complex rc_atanh (double);
extern OCTAVE_API FloatComplex rc_atanh (float);
extern OCTAVE_API Complex rc_log (double);
extern OCTAVE_API FloatComplex rc_log (float);
extern OCTAVE_API Complex rc_log2 (double);
extern OCTAVE_API FloatComplex rc_log2 (float);
extern OCTAVE_API Complex rc_log10 (double);
extern OCTAVE_API FloatComplex rc_log10 (float);
extern OCTAVE_API Complex rc_sqrt (double);
extern OCTAVE_API FloatComplex rc_sqrt (float);

// Some useful tests, that are commonly repeated.
// Test for a finite integer.
inline bool xisinteger (double x)
{ return xfinite (x) && x == xround (x); }
inline bool xisinteger (float x)
{ return xfinite (x) && x == xround (x); }

// Test for negative sign. 
extern OCTAVE_API bool xnegative_sign (double x);
extern OCTAVE_API bool xnegative_sign (float x);


#endif
