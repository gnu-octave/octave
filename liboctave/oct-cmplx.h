/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_oct_cmplx_h)
#define octave_oct_cmplx_h 1

// By using this file instead of <complex>, we can easily avoid buggy
// implementations of the standard complex data type (if needed).

#include <complex>

typedef std::complex<double> Complex;

#if defined (CXX_ISO_COMPLIANT_LIBRARY)

// If namespaces don't work, we will end up with some infinite looping.

inline double
real (const Complex& z)
{
  return std::real (z);
}

inline double
imag (const Complex& z)
{
  return std::imag (z);
}

inline double
abs (const Complex& z)
{
  return std::abs (z);
}

inline double
arg (const Complex& z)
{
  return std::arg (z);
}

inline double
norm (const Complex& z)
{
  return std::norm (z);
}

inline Complex
conj (const Complex& z)
{
  return std::conj (z);
}

inline Complex
polar (const double& x, const double& y);

inline Complex
cos (const Complex& z)
{
  return std::cos (z);
}

inline Complex
cosh (const Complex& z)
{
  return std::cosh (z);
}

inline Complex
exp (const Complex& z)
{
  return std::exp (z);
}

inline Complex
log (const Complex& z)
{
  return std::log (z);
}

inline Complex
log10 (const Complex& z)
{
  return std::log10 (z);
}

inline Complex
pow (const Complex& z, int n)
{
  return std::pow (z, n);
}

inline Complex
pow (const Complex& z, const double& x)
{
  // XXX FIXME XXX -- this should not be needed, but it avoids a bug
  // in some versions of libstdc++ (3.3.x and possibly others).

  return std::pow (z, Complex (x));
}

inline Complex
pow (const Complex& z1, const Complex& z2)
{
  return std::pow (z1, z2);
}

inline Complex
pow (const double& x, const Complex& z)
{
  return std::pow (x, z);
}

inline Complex
sin (const Complex& z)
{
  return std::sin (z);
}

inline Complex
sinh (const Complex& z)
{
  return std::sinh (z);
}

inline Complex
sqrt (const Complex& z)
{
  return std::sqrt (z);
}

inline Complex
tan (const Complex& z)
{
  return std::tan (z);
}

inline Complex
tanh (const Complex& z)
{
  return std::tanh (z);
}

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
