/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

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

#include <iostream>

#include "lo-ieee.h"
#include "mx-base.h"

#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-ch-mat.h"
#include "gripes.h"
#include "pr-output.h"

template class octave_base_matrix<charNDArray>;

idx_vector 
octave_char_matrix::index_vector (void) const
{ 
  const char *p = matrix.data ();
  if (numel () == 1 && *p == ':')
    return idx_vector (':');
  else
    return idx_vector (array_value (true)); 
}

double
octave_char_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "character matrix", "real scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "real scalar");

  return retval;
}

float
octave_char_matrix::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "character matrix", "real scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "real scalar");

  return retval;
}

Complex
octave_char_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "character matrix", "complex scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "complex scalar");

  return retval;
}

FloatComplex
octave_char_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "character matrix", "complex scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "complex scalar");

  return retval;
}

void
octave_char_matrix::print_raw (std::ostream& os,
			       bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

mxArray *
octave_char_matrix::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxCHAR_CLASS, dims (), mxREAL);

  mxChar *pr = static_cast<mxChar *> (retval->get_data ());

  mwSize nel = numel ();

  const char *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    pr[i] = p[i];

  return retval;
}

// The C++ standard guarantees cctype defines functions, not macros (and hence macros *CAN'T* 
// be defined if only cctype is included)
// so there's no need to f*ck around. The exceptions are isascii and toascii,
// which are not C++.
// Oddly enough, all those character functions are int (*) (int), even
// in C++. Wicked!
static inline int xisascii (int c)
{ return isascii (c); }

static inline int xtoascii (int c)
{ return toascii (c); }

octave_value
octave_char_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
#define STRING_MAPPER(UMAP,FCN,TYPE) \
    case UMAP: \
      return octave_value (matrix.map<TYPE, int (&) (int)> (FCN))

    STRING_MAPPER (umap_isalnum, std::isalnum, bool);
    STRING_MAPPER (umap_isalpha, std::isalpha, bool);
    STRING_MAPPER (umap_isascii, xisascii, bool);
    STRING_MAPPER (umap_iscntrl, std::iscntrl, bool);
    STRING_MAPPER (umap_isdigit, std::isdigit, bool);
    STRING_MAPPER (umap_isgraph, std::isgraph, bool);
    STRING_MAPPER (umap_islower, std::islower, bool);
    STRING_MAPPER (umap_isprint, std::isprint, bool);
    STRING_MAPPER (umap_ispunct, std::ispunct, bool);
    STRING_MAPPER (umap_isspace, std::isspace, bool);
    STRING_MAPPER (umap_isupper, std::isupper, bool);
    STRING_MAPPER (umap_isxdigit, std::isxdigit, bool);
    STRING_MAPPER (umap_toascii, xtoascii, double);
    STRING_MAPPER (umap_tolower, std::tolower, char);
    STRING_MAPPER (umap_toupper, std::toupper, char);

    default: 
      {
        octave_matrix m (array_value (true));
        return m.map (umap);
      }
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
