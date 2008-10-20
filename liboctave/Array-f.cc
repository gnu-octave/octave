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

// Instantiate Arrays of float values.

#include "Array.h"
#include "Array.cc"
#include "oct-sort.cc"

#if defined (HAVE_IEEE754_DATA_FORMAT)

static inline uint32_t
FloatFlip (uint32_t f)
{
  uint32_t mask
    = -static_cast<int32_t>(f >> 31) | 0x80000000UL;

  return f ^ mask;
}

static inline uint32_t
IFloatFlip (uint32_t f)
{
  uint32_t mask = ((f >> 31) - 1) | 0x80000000UL;

  return f ^ mask;
}

template <>
bool
ascending_compare (float a, float b)
{
  return (xisnan (b) || (a < b));
}

template <>
bool
ascending_compare (vec_index<float> *a, vec_index<float> *b)
{
  return (xisnan (b->vec) || (a->vec < b->vec));
}

template <>
bool
descending_compare (float a, float b)
{
  return (xisnan (a) || (a > b));
}

template <>
bool
descending_compare (vec_index<float> *a, vec_index<float> *b)
{
  return (xisnan (b->vec) || (a->vec > b->vec));
}

INSTANTIATE_ARRAY_SORT (uint32_t);

template <>
Array<float>
Array<float>::sort (octave_idx_type dim, sortmode mode) const
{
  Array<float> m = *this;

  dim_vector dv = m.dims ();

  if (m.length () < 1)
    return m;

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;
  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  float *v = m.fortran_vec ();

  uint32_t *p = reinterpret_cast<uint32_t *> (v);

  octave_sort<uint32_t> lsort;

  if (mode == ASCENDING)
    lsort.set_compare (ascending_compare);
  else if (mode == DESCENDING)
    lsort.set_compare (descending_compare);
  else
    abort ();

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

	  if (lo_ieee_signbit (octave_Float_NaN))
	    {
	      if (mode == ASCENDING)
		{
		  octave_idx_type i = 0;
		  float *vtmp = reinterpret_cast<float *> (p);
		  while (xisnan (vtmp[i++]) && i < ns)
		    /* do nothing */;
		  for (octave_idx_type l = 0; l < ns - i + 1; l++)
		    vtmp[l] = vtmp[l+i-1];
		  for (octave_idx_type l = ns - i + 1; l < ns; l++)
		    vtmp[l] = octave_Float_NaN;
		}
	      else
		{
		  octave_idx_type i = ns;
		  float *vtmp = reinterpret_cast<float *> (p);
		  while (xisnan (vtmp[--i]) && i > 0)
		    /* do nothing */;
		  for (octave_idx_type l = i; l >= 0; l--)
		    vtmp[l-i+ns-1] = vtmp[l];
		  for (octave_idx_type l = 0; l < ns - i - 1; l++)
		    vtmp[l] = octave_Float_NaN;
		}
	    }

	  p += ns;
	}
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (uint32_t, vi, ns);

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

	  if (lo_ieee_signbit (octave_Float_NaN))
	    {
	      if (mode == ASCENDING)
		{
		   octave_idx_type i = 0;
		  while (xisnan (v[i++*stride + offset]) && i < ns)
		    /* do nothing */;
		  for (octave_idx_type l = 0; l < ns - i + 1; l++)
		    v[l*stride + offset] = v[(l+i-1)*stride + offset];
		  for (octave_idx_type l = ns - i + 1; l < ns; l++)
		    v[l*stride + offset] = octave_Float_NaN;
		}
	      else
		{
		   octave_idx_type i = ns;
		  while (xisnan (v[--i*stride + offset]) && i > 0)
		    /* do nothing */;
		  for (octave_idx_type l = i; l >= 0; l--)
		    v[(l-i+ns-1)*stride + offset] = v[l*stride + offset];
		  for (octave_idx_type l = 0; l < ns - i - 1; l++)
		    v[l*stride + offset] = octave_Float_NaN;
		}
	    }
	}
    }

  return m;
}

template <>
Array<float>
Array<float>::sort (Array<octave_idx_type> &sidx, octave_idx_type dim, 
		     sortmode mode) const
{
  Array<float> m = *this;

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

  float *v = m.fortran_vec ();

  uint32_t *p = reinterpret_cast<uint32_t *> (v);

  octave_sort<vec_index<uint32_t> *> indexed_sort;

  if (mode == ASCENDING)
    indexed_sort.set_compare (ascending_compare);
  else if (mode == DESCENDING)
    indexed_sort.set_compare (descending_compare);
  else
    abort ();

  OCTAVE_LOCAL_BUFFER (vec_index<uint32_t> *, vi, ns);
  OCTAVE_LOCAL_BUFFER (vec_index<uint32_t>, vix, ns);
  
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

      if (lo_ieee_signbit (octave_Float_NaN))
	{
	  if (mode == ASCENDING)
	    {
	      octave_idx_type i = 0;
	      while (xisnan (v[i++*stride+offset]) && i < ns)
		/* do nothing */;
	      OCTAVE_LOCAL_BUFFER (float, itmp, i - 1);
	      for (octave_idx_type l = 0; l < i -1; l++)
		itmp[l] = sidx(l*stride + offset);
	      for (octave_idx_type l = 0; l < ns - i + 1; l++)
		{
		  v[l*stride + offset] = v[(l+i-1)*stride + offset];
		  sidx(l*stride + offset) = sidx((l+i-1)*stride + offset);
		}
	      for (octave_idx_type k = 0, l = ns - i + 1; l < ns; l++, k++)
		{
		  v[l*stride + offset] = octave_Float_NaN;
		  sidx(l*stride + offset) = 
		    static_cast<octave_idx_type>(itmp[k]);
		}
	    }
	  else 
	    {
	      octave_idx_type i = ns;
	      while (xisnan (v[--i*stride+offset]) && i > 0)
		/* do nothing */;
	      OCTAVE_LOCAL_BUFFER (float, itmp, ns - i - 1);
	      for (octave_idx_type l = 0; l < ns - i -1; l++)
		itmp[l] = sidx((l+i+1)*stride + offset);
	      for (octave_idx_type l = i; l >= 0; l--)
		{
		  v[(l-i+ns-1)*stride + offset] = v[l*stride + offset];
		  sidx((l-i+ns-1)*stride + offset) = sidx(l*stride + offset);
		}
	      for (octave_idx_type k = 0, l = 0; l < ns - i - 1; l++, k++)
		{
		  v[l*stride + offset] = octave_Float_NaN;
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
ascending_compare (float a, float b)
{
  return (xisnan (b) || (a < b));
}

template <>
bool
ascending_compare (vec_index<float> *a, 
		   vec_index<float> *b)
{
  return (xisnan (b->vec) || (a->vec < b->vec));
}

template <>
bool
descending_compare (float a, float b)
{
  return (xisnan (a) || (a > b));
}

template <>
bool
descending_compare (vec_index<float> *a, 
		    vec_index<float> *b)
{
  return (xisnan (b->vec) || (a->vec > b->vec));
}

INSTANTIATE_ARRAY_SORT (float);

#endif

INSTANTIATE_ARRAY_AND_ASSIGN (float, OCTAVE_API);

INSTANTIATE_ARRAY_ASSIGN (float, int, OCTAVE_API)
INSTANTIATE_ARRAY_ASSIGN (float, short, OCTAVE_API)
INSTANTIATE_ARRAY_ASSIGN (float, char, OCTAVE_API)

#include "Array2.h"

template class OCTAVE_API Array2<float>;

#include "ArrayN.h"
#include "ArrayN.cc"

template class OCTAVE_API ArrayN<float>;

template OCTAVE_API std::ostream& operator << (std::ostream&, const ArrayN<float>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<float>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
