/*

Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2003, 2004,
              2005, 2006, 2007 John W. Eaton

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

// Instantiate Arrays of double values.

#include "Array.h"
#include "Array.cc"
#include "oct-sort.cc"

#if defined (HAVE_IEEE754_DATA_FORMAT)

static inline uint64_t
FloatFlip (uint64_t f)
{
  uint64_t mask
    = -static_cast<int64_t>(f >> 63) | 0x8000000000000000ULL;

  return f ^ mask;
}

static inline uint64_t
IFloatFlip (uint64_t f)
{
  uint64_t mask = ((f >> 63) - 1) | 0x8000000000000000ULL;

  return f ^ mask;
}

template <>
bool
ascending_compare (double a, double b)
{
  return (xisnan (b) || (a < b));
}

template <>
bool
ascending_compare (vec_index<double> *a, vec_index<double> *b)
{
  return (xisnan (b->vec) || (a->vec < b->vec));
}

template <>
bool
descending_compare (double a, double b)
{
  return (xisnan (a) || (a > b));
}

template <>
bool
descending_compare (vec_index<double> *a, vec_index<double> *b)
{
  return (xisnan (b->vec) || (a->vec > b->vec));
}

INSTANTIATE_ARRAY_SORT (uint64_t);

template <>
Array<double>
Array<double>::sort (octave_idx_type dim, sortmode mode) const
{
  Array<double> m = *this;

  dim_vector dv = m.dims ();

  if (m.length () < 1)
    return m;

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;
  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  double *v = m.fortran_vec ();

  uint64_t *p = reinterpret_cast<uint64_t *> (v);

  octave_sort<uint64_t> lsort;

  if (mode == ASCENDING)
    lsort.set_compare (ascending_compare);
  else if (mode == DESCENDING)
    lsort.set_compare (descending_compare);

  if (stride == 1)
    {
      for (octave_idx_type j = 0; j < iter; j++)
	{
	  // Flip the data in the vector so that int compares on
	  // IEEE754 give the correct ordering.

	  for (octave_idx_type i = 0; i < ns; i++)
	    p[i] = FloatFlip (p[i]);
	      
	  lsort.sort (p, ns);

	  // Flip the data out of the vector so that int compares
	  // on IEEE754 give the correct ordering.

	  for (octave_idx_type i = 0; i < ns; i++)
	    p[i] = IFloatFlip (p[i]);

	  // There are two representations of NaN.  One will be
	  // sorted to the beginning of the vector and the other
	  // to the end.  If it will be sorted incorrectly, fix
	  // things up.

	  if (lo_ieee_signbit (octave_NaN))
	    {
	      if (mode == UNDEFINED || mode == ASCENDING)
		{
		  octave_idx_type i = 0;
		  double *vtmp = reinterpret_cast<double *> (p);
		  while (xisnan (vtmp[i++]) && i < ns);
		  for (octave_idx_type l = 0; l < ns - i + 1; l++)
		    vtmp[l] = vtmp[l+i-1];
		  for (octave_idx_type l = ns - i + 1; l < ns; l++)
		    vtmp[l] = octave_NaN;
		}
	      else
		{
		  octave_idx_type i = ns;
		  double *vtmp = reinterpret_cast<double *> (p);
		  while (xisnan (vtmp[--i]) && i > 0);
		  for (octave_idx_type l = i; l >= 0; l--)
		    vtmp[l-i+ns-1] = vtmp[l];
		  for (octave_idx_type l = 0; l < ns - i - 1; l++)
		    vtmp[l] = octave_NaN;
		}
	    }

	  p += ns;
	}
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (uint64_t, vi, ns);

      for (octave_idx_type j = 0; j < iter; j++)
	{
	  octave_idx_type offset = j;
	  octave_idx_type offset2 = 0;
	  while (offset >= stride)
	    {
	      offset -= stride;
	      offset2++;
	    }
	  offset += offset2 * stride * ns;

	  // Flip the data in the vector so that int compares on
	  // IEEE754 give the correct ordering.

	  for (octave_idx_type i = 0; i < ns; i++)
	    vi[i] = FloatFlip (p[i*stride + offset]);

	  lsort.sort (vi, ns);

	  // Flip the data out of the vector so that int compares
	  // on IEEE754 give the correct ordering.

	  for (octave_idx_type i = 0; i < ns; i++)
	    p[i*stride + offset] = IFloatFlip (vi[i]);
	      
	  // There are two representations of NaN. One will be
	  // sorted to the beginning of the vector and the other
	  // to the end. If it will be sorted to the beginning,
	  // fix things up.

	  if (lo_ieee_signbit (octave_NaN))
	    {
	      if (mode == UNDEFINED || mode == ASCENDING)
		{
		   octave_idx_type i = 0;
		  while (xisnan (v[i++*stride + offset]) && i < ns);
		  for (octave_idx_type l = 0; l < ns - i + 1; l++)
		    v[l*stride + offset] = v[(l+i-1)*stride + offset];
		  for (octave_idx_type l = ns - i + 1; l < ns; l++)
		    v[l*stride + offset] = octave_NaN;
		}
	      else
		{
		   octave_idx_type i = ns;
		  while (xisnan (v[--i*stride + offset]) && i > 0);
		  for (octave_idx_type l = i; l >= 0; l--)
		    v[(l-i+ns-1)*stride + offset] = v[l*stride + offset];
		  for (octave_idx_type l = 0; l < ns - i - 1; l++)
		    v[l*stride + offset] = octave_NaN;
		}
	    }
	}
    }

  return m;
}

template <>
Array<double>
Array<double>::sort (Array<octave_idx_type> &sidx, octave_idx_type dim, 
		     sortmode mode) const
{
  Array<double> m = *this;

  dim_vector dv = m.dims ();

  if (m.length () < 1)
    {
      sidx = Array<octave_idx_type> (dv);
      return m;
    }

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;
  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  double *v = m.fortran_vec ();

  uint64_t *p = reinterpret_cast<uint64_t *> (v);

  octave_sort<vec_index<uint64_t> *> indexed_sort;

  if (mode == ASCENDING)
    indexed_sort.set_compare (ascending_compare);
  else if (mode == DESCENDING)
    indexed_sort.set_compare (descending_compare);

  OCTAVE_LOCAL_BUFFER (vec_index<uint64_t> *, vi, ns);
  OCTAVE_LOCAL_BUFFER (vec_index<uint64_t>, vix, ns);
  
  for (octave_idx_type i = 0; i < ns; i++)
    vi[i] = &vix[i];

  sidx = Array<octave_idx_type> (dv);
      
  for (octave_idx_type j = 0; j < iter; j++)
    {
      octave_idx_type offset = j;
      octave_idx_type offset2 = 0;
      while (offset >= stride)
	{
	  offset -= stride;
	  offset2++;
	}
      offset += offset2 * stride * ns;

      // Flip the data in the vector so that int compares on
      // IEEE754 give the correct ordering.

      for (octave_idx_type i = 0; i < ns; i++)
	{
	  vi[i]->vec = FloatFlip (p[i*stride + offset]);
	  vi[i]->indx = i;
	}

      indexed_sort.sort (vi, ns);

      // Flip the data out of the vector so that int compares on
      // IEEE754 give the correct ordering

      for (octave_idx_type i = 0; i < ns; i++)
	{
	  p[i*stride + offset] = IFloatFlip (vi[i]->vec);
	  sidx(i*stride + offset) = vi[i]->indx;
	}

      // There are two representations of NaN.  One will be sorted
      // to the beginning of the vector and the other to the end.
      // If it will be sorted to the beginning, fix things up.

      if (lo_ieee_signbit (octave_NaN))
	{
	  if (mode == UNDEFINED || mode == ASCENDING)
	    {
	      octave_idx_type i = 0;
	      while (xisnan (v[i++*stride+offset]) && i < ns);
	      OCTAVE_LOCAL_BUFFER (double, itmp, i - 1);
	      for (octave_idx_type l = 0; l < i -1; l++)
		itmp[l] = sidx(l*stride + offset);
	      for (octave_idx_type l = 0; l < ns - i + 1; l++)
		{
		  v[l*stride + offset] = v[(l+i-1)*stride + offset];
		  sidx(l*stride + offset) = sidx((l+i-1)*stride + offset);
		}
	      for (octave_idx_type k = 0, l = ns - i + 1; l < ns; l++, k++)
		{
		  v[l*stride + offset] = octave_NaN;
		  sidx(l*stride + offset) = 
		    static_cast<octave_idx_type>(itmp[k]);
		}
	    }
	  else 
	    {
	      octave_idx_type i = ns;
	      while (xisnan (v[--i*stride+offset]) && i > 0);
	      OCTAVE_LOCAL_BUFFER (double, itmp, ns - i - 1);
	      for (octave_idx_type l = 0; l < ns - i -1; l++)
		itmp[l] = sidx((l+i+1)*stride + offset);
	      for (octave_idx_type l = i; l >= 0; l--)
		{
		  v[(l-i+ns-1)*stride + offset] = v[l*stride + offset];
		  sidx((l-i+ns-1)*stride + offset) = sidx(l*stride + offset);
		}
	      for (octave_idx_type k = 0, l = 0; l < ns - i - 1; l++, k++)
		{
		  v[l*stride + offset] = octave_NaN;
		  sidx(l*stride + offset) = 
		    static_cast<octave_idx_type>(itmp[k]);
		}
	    }
	}
    }

  return m;
}

#else

template <>
bool
ascending_compare (double a, double b)
{
  return (xisnan (b) || (a < b));
}

template <>
bool
ascending_compare (vec_index<double> *a, 
		   vec_index<double> *b)
{
  return (xisnan (b->vec) || (a->vec < b->vec));
}

template <>
bool
descending_compare (double a, double b)
{
  return (xisnan (a) || (a > b));
}

template <>
bool
descending_compare (vec_index<double> *a, 
		    vec_index<double> *b)
{
  return (xisnan (b->vec) || (a->vec > b->vec));
}

INSTANTIATE_ARRAY_SORT (double);

#endif

INSTANTIATE_ARRAY_AND_ASSIGN (double, OCTAVE_API);

INSTANTIATE_ARRAY_ASSIGN (double, int, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (double, short, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (double, char, OCTAVE_API);

#include "Array2.h"

template class OCTAVE_API Array2<double>;

#include "ArrayN.h"
#include "ArrayN.cc"

template class OCTAVE_API ArrayN<double>;

template OCTAVE_API std::ostream& operator << (std::ostream&, const ArrayN<double>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<double>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
