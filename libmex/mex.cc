////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2024 The Octave Project Developers
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

// #define DEBUG 1

#if defined (DEBUG)
#  include <iostream>
#endif

#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cctype>

#include <limits>
#include <map>
#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)
#  include <memory_resource>
#endif
#include <set>
#include <string>

#include "f77-fcn.h"
#include "oct-locbuf.h"
#include "quit.h"

#include "Cell.h"
#include "error.h"
#include "interpreter-private.h"
#include "interpreter.h"
// mxArray must be declared as a class before including mexproto.h.
#include "mxarray.h"
#include "mexproto.h"
#include "oct-map.h"
#include "ovl.h"
#include "ov.h"
#include "ov-classdef.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "graphics.h"

// These must be declared extern "C" but may be omitted from the set of
// symbols declared in mexproto.h, so we declare them here as well.

extern "C"
{
  extern OCTMEX_API const mxArray *
  mexGet_interleaved (double handle, const char *property);

  extern OCTMEX_API mxArray *
  mxCreateCellArray (mwSize ndims, const mwSize *dims);

  extern OCTMEX_API mxArray *
  mxCreateCellMatrix (mwSize m, mwSize n);

  extern OCTMEX_API mxArray *
  mxCreateCharArray (mwSize ndims, const mwSize *dims);

  extern OCTMEX_API mxArray *
  mxCreateCharMatrixFromStrings (mwSize m, const char **str);

  extern OCTMEX_API mxArray *
  mxCreateDoubleMatrix (mwSize nr, mwSize nc, mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateDoubleScalar (double val);

  extern OCTMEX_API mxArray *
  mxCreateLogicalArray (mwSize ndims, const mwSize *dims);

  extern OCTMEX_API mxArray *
  mxCreateLogicalMatrix (mwSize m, mwSize n);

  extern OCTMEX_API mxArray *
  mxCreateLogicalScalar (mxLogical val);

  extern OCTMEX_API mxArray *
  mxCreateNumericArray (mwSize ndims, const mwSize *dims, mxClassID class_id,
                        mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                         mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateUninitNumericArray (mwSize ndims, const mwSize *dims,
                              mxClassID class_id, mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateUninitNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                               mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateSparse (mwSize m, mwSize n, mwSize nzmax, mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateSparseLogicalMatrix (mwSize m, mwSize n, mwSize nzmax);

  extern OCTMEX_API mxArray *
  mxCreateString (const char *str);

  extern OCTMEX_API mxArray *
  mxCreateStructArray (mwSize ndims, const mwSize *dims, int num_keys,
                       const char **keys);

  extern OCTMEX_API mxArray *
  mxCreateStructMatrix (mwSize rows, mwSize cols, int num_keys,
                        const char **keys);

  extern OCTMEX_API mxArray *
  mxCreateCellArray_interleaved (mwSize ndims, const mwSize *dims);

  extern OCTMEX_API mxArray *
  mxCreateCellMatrix_interleaved (mwSize m, mwSize n);

  extern OCTMEX_API mxArray *
  mxCreateCharArray_interleaved (mwSize ndims, const mwSize *dims);

  extern OCTMEX_API mxArray *
  mxCreateCharMatrixFromStrings_interleaved (mwSize m, const char **str);

  extern OCTMEX_API mxArray *
  mxCreateDoubleMatrix_interleaved (mwSize nr, mwSize nc, mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateDoubleScalar_interleaved (double val);

  extern OCTMEX_API mxArray *
  mxCreateLogicalArray_interleaved (mwSize ndims, const mwSize *dims);

  extern OCTMEX_API mxArray *
  mxCreateLogicalMatrix_interleaved (mwSize m, mwSize n);

  extern OCTMEX_API mxArray *
  mxCreateLogicalScalar_interleaved (mxLogical val);

  extern OCTMEX_API mxArray *
  mxCreateNumericArray_interleaved (mwSize ndims, const mwSize *dims,
                                    mxClassID class_id, mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateNumericMatrix_interleaved (mwSize m, mwSize n, mxClassID class_id,
                                     mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateUninitNumericArray_interleaved (mwSize ndims, const mwSize *dims,
                                          mxClassID class_id,
                                          mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateUninitNumericMatrix_interleaved (mwSize m, mwSize n,
                                           mxClassID class_id,
                                           mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateSparse_interleaved (mwSize m, mwSize n, mwSize nzmax,
                              mxComplexity flag);

  extern OCTMEX_API mxArray *
  mxCreateSparseLogicalMatrix_interleaved (mwSize m, mwSize n, mwSize nzmax);

  extern OCTMEX_API mxArray *
  mxCreateString_interleaved (const char *str);

  extern OCTMEX_API mxArray *
  mxCreateStructArray_interleaved (mwSize ndims, const mwSize *dims,
                                   int num_keys, const char **keys);

  extern OCTMEX_API mxArray *
  mxCreateStructMatrix_interleaved (mwSize rows, mwSize cols, int num_keys,
                                    const char **keys);

  extern OCTMEX_API int mxMakeArrayReal (mxArray *ptr);
  extern OCTMEX_API int mxMakeArrayComplex (mxArray *ptr);

  extern OCTMEX_API mxDouble * mxGetDoubles (const mxArray *p);
  extern OCTMEX_API mxSingle * mxGetSingles (const mxArray *p);
  extern OCTMEX_API mxInt8 * mxGetInt8s (const mxArray *p);
  extern OCTMEX_API mxInt16 * mxGetInt16s (const mxArray *p);
  extern OCTMEX_API mxInt32 * mxGetInt32s (const mxArray *p);
  extern OCTMEX_API mxInt64 * mxGetInt64s (const mxArray *p);
  extern OCTMEX_API mxUint8 * mxGetUint8s (const mxArray *p);
  extern OCTMEX_API mxUint16 * mxGetUint16s (const mxArray *p);
  extern OCTMEX_API mxUint32 * mxGetUint32s (const mxArray *p);
  extern OCTMEX_API mxUint64 * mxGetUint64s (const mxArray *p);

  extern OCTMEX_API mxComplexDouble * mxGetComplexDoubles (const mxArray *p);
  extern OCTMEX_API mxComplexSingle * mxGetComplexSingles (const mxArray *p);

  extern OCTMEX_API double * mxGetPi (const mxArray *ptr);
  extern OCTMEX_API void * mxGetImagData (const mxArray *ptr);

  extern OCTMEX_API int mxSetDoubles (mxArray *p, mxDouble *d);
  extern OCTMEX_API int mxSetSingles (mxArray *p, mxSingle *d);
  extern OCTMEX_API int mxSetInt8s (mxArray *p, mxInt8 *d);
  extern OCTMEX_API int mxSetInt16s (mxArray *p, mxInt16 *d);
  extern OCTMEX_API int mxSetInt32s (mxArray *p, mxInt32 *d);
  extern OCTMEX_API int mxSetInt64s (mxArray *p, mxInt64 *d);
  extern OCTMEX_API int mxSetUint8s (mxArray *p, mxUint8 *d);
  extern OCTMEX_API int mxSetUint16s (mxArray *p, mxUint16 *d);
  extern OCTMEX_API int mxSetUint32s (mxArray *p, mxUint32 *d);
  extern OCTMEX_API int mxSetUint64s (mxArray *p, mxUint64 *d);

  extern OCTMEX_API int mxSetComplexDoubles (mxArray *p, mxComplexDouble *d);
  extern OCTMEX_API int mxSetComplexSingles (mxArray *p, mxComplexSingle *d);

  extern OCTMEX_API void mxSetPi (mxArray *ptr, double *pi);
  extern OCTMEX_API void mxSetImagData (mxArray *ptr, void *pi);
}

static void *
xmalloc (size_t n)
{
  void *ptr = std::malloc (n);

#if defined (DEBUG)
  std::cerr << "xmalloc (" << n << ") = " << ptr << std::endl;
#endif

  return ptr;
}

static void *
xrealloc (void *ptr, size_t n)
{
  void *newptr = std::realloc (ptr, n);

#if defined (DEBUG)
  std::cerr << "xrealloc (" << ptr << ", " << n << ") = " << newptr
            << std::endl;
#endif

  return newptr;
}

static void
xfree (void *ptr)
{
#if defined (DEBUG)
  std::cerr << "xfree (" << ptr << ")" << std::endl;
#endif

  std::free (ptr);
}

// FIXME: Is there a better/standard way to do this job?

template <typename T>
class fp_type_traits
{
public:
  static const bool is_complex = false;
};

template <>
class fp_type_traits<Complex>
{
public:
  static const bool is_complex = true;
};

template <>
class fp_type_traits <FloatComplex>
{
public:
  static const bool is_complex = true;
};

extern OCTINTERP_API mex *mex_context;

template <typename T>
static inline T *
maybe_unmark (T *ptr)
{
  if (mex_context)
    mex_context->unmark (ptr);

  return ptr;
}

static inline mxArray *
maybe_unmark_array (mxArray *ptr)
{
  if (mex_context)
    mex_context->unmark_array (ptr);

  return ptr;
}

// ------------------------------------------------------------------

// C interface to mxArray objects:

// Floating point predicates.

bool
mxIsFinite (const double v)
{
  return octave::math::isfinite (v) != 0;
}

bool
mxIsInf (const double v)
{
  return octave::math::isinf (v) != 0;
}

bool
mxIsNaN (const double v)
{
  return octave::math::isnan (v) != 0;
}

double
mxGetEps ()
{
  return std::numeric_limits<double>::epsilon ();
}

double
mxGetInf ()
{
  return lo_ieee_inf_value ();
}

double
mxGetNaN ()
{
  return lo_ieee_nan_value ();
}

// Memory management.
void *
mxCalloc (std::size_t n, std::size_t size)
{
  return mex_context ? mex_context->calloc (n, size) : ::calloc (n, size);
}

void *
mxMalloc (std::size_t n)
{
  return mex_context ? mex_context->malloc (n) : xmalloc (n);
}

void *
mxRealloc (void *ptr, std::size_t size)
{
  return (mex_context
          ? mex_context->realloc (ptr, size) : xrealloc (ptr, size));
}

void
mxFree (void *ptr)
{
  if (mex_context)
    mex_context->free (ptr);
  else
    xfree (ptr);
}

static inline mxArray *
maybe_mark_array (mxArray *ptr)
{
  return mex_context ? mex_context->mark_array (ptr) : ptr;
}

// Constructors.
mxArray *
mxCreateCellArray_interleaved (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (true, ndims, dims));
}

mxArray *
mxCreateCellArray (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (false, ndims, dims));
}

mxArray *
mxCreateCellMatrix_interleaved (mwSize m, mwSize n)
{
  return maybe_mark_array (new mxArray (true, m, n));
}

mxArray *
mxCreateCellMatrix (mwSize m, mwSize n)
{
  return maybe_mark_array (new mxArray (false, m, n));
}

mxArray *
mxCreateCharArray_interleaved (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (true, mxCHAR_CLASS, ndims, dims));
}

mxArray *
mxCreateCharArray (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (false, mxCHAR_CLASS, ndims, dims));
}

mxArray *
mxCreateCharMatrixFromStrings_interleaved (mwSize m, const char **str)
{
  return maybe_mark_array (new mxArray (true, m, str));
}

mxArray *
mxCreateCharMatrixFromStrings (mwSize m, const char **str)
{
  return maybe_mark_array (new mxArray (false, m, str));
}

mxArray *
mxCreateDoubleMatrix_interleaved (mwSize m, mwSize n, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (true, mxDOUBLE_CLASS, m, n, flag));
}

mxArray *
mxCreateDoubleMatrix (mwSize m, mwSize n, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (false, mxDOUBLE_CLASS, m, n, flag));
}

mxArray *
mxCreateDoubleScalar_interleaved (double val)
{
  return maybe_mark_array (new mxArray (true, mxDOUBLE_CLASS, val));
}

mxArray *
mxCreateDoubleScalar (double val)
{
  return maybe_mark_array (new mxArray (false, mxDOUBLE_CLASS, val));
}

mxArray *
mxCreateLogicalArray_interleaved (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (true, mxLOGICAL_CLASS, ndims, dims));
}

mxArray *
mxCreateLogicalArray (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (false, mxLOGICAL_CLASS, ndims, dims));
}

mxArray *
mxCreateLogicalMatrix_interleaved (mwSize m, mwSize n)
{
  return maybe_mark_array (new mxArray (true, mxLOGICAL_CLASS, m, n));
}

mxArray *
mxCreateLogicalMatrix (mwSize m, mwSize n)
{
  return maybe_mark_array (new mxArray (false, mxLOGICAL_CLASS, m, n));
}

mxArray *
mxCreateLogicalScalar_interleaved (mxLogical val)
{
  return maybe_mark_array (new mxArray (true, mxLOGICAL_CLASS, val));
}

mxArray *
mxCreateLogicalScalar (mxLogical val)
{
  return maybe_mark_array (new mxArray (false, mxLOGICAL_CLASS, val));
}

mxArray *
mxCreateNumericArray_interleaved (mwSize ndims, const mwSize *dims,
                                  mxClassID class_id, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (true, class_id, ndims, dims, flag));
}

mxArray *
mxCreateNumericArray (mwSize ndims, const mwSize *dims,
                      mxClassID class_id, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (false, class_id, ndims, dims, flag));
}

mxArray *
mxCreateNumericMatrix_interleaved (mwSize m, mwSize n, mxClassID class_id,
                                   mxComplexity flag)
{
  return maybe_mark_array (new mxArray (true, class_id, m, n, flag));
}

mxArray *
mxCreateNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                       mxComplexity flag)
{
  return maybe_mark_array (new mxArray (false, class_id, m, n, flag));
}

mxArray *
mxCreateUninitNumericArray_interleaved (mwSize ndims, const mwSize *dims,
                                        mxClassID class_id, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (true, class_id, ndims, dims, flag,
                                        false));
}

mxArray *
mxCreateUninitNumericArray (mwSize ndims, const mwSize *dims,
                            mxClassID class_id, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (false, class_id, ndims, dims, flag,
                                        false));
}

mxArray *
mxCreateUninitNumericMatrix_interleaved (mwSize m, mwSize n,
    mxClassID class_id, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (true, class_id, m, n, flag, false));
}

mxArray *
mxCreateUninitNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                             mxComplexity flag)
{
  return maybe_mark_array (new mxArray (false, class_id, m, n, flag, false));
}

mxArray *
mxCreateSparse_interleaved (mwSize m, mwSize n, mwSize nzmax, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (true, mxDOUBLE_CLASS, m, n, nzmax,
                                        flag));
}

mxArray *
mxCreateSparse (mwSize m, mwSize n, mwSize nzmax, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (false, mxDOUBLE_CLASS, m, n, nzmax,
                                        flag));
}

mxArray *
mxCreateSparseLogicalMatrix_interleaved (mwSize m, mwSize n, mwSize nzmax)
{
  return maybe_mark_array (new mxArray (true, mxLOGICAL_CLASS, m, n, nzmax));
}

mxArray *
mxCreateSparseLogicalMatrix (mwSize m, mwSize n, mwSize nzmax)
{
  return maybe_mark_array (new mxArray (false, mxLOGICAL_CLASS, m, n, nzmax));
}

mxArray *
mxCreateString_interleaved (const char *str)
{
  return maybe_mark_array (new mxArray (true, str));
}

mxArray *
mxCreateString (const char *str)
{
  return maybe_mark_array (new mxArray (false, str));
}

mxArray *
mxCreateStructArray_interleaved (mwSize ndims, const mwSize *dims,
                                 int num_keys, const char **keys)
{
  return maybe_mark_array (new mxArray (true, ndims, dims, num_keys, keys));
}

mxArray *
mxCreateStructArray (mwSize ndims, const mwSize *dims, int num_keys,
                     const char **keys)
{
  return maybe_mark_array (new mxArray (false, ndims, dims, num_keys, keys));
}

mxArray *
mxCreateStructMatrix_interleaved (mwSize m, mwSize n, int num_keys,
                                  const char **keys)
{
  return maybe_mark_array (new mxArray (true, m, n, num_keys, keys));
}

mxArray *
mxCreateStructMatrix (mwSize m, mwSize n, int num_keys,
                      const char **keys)
{
  return maybe_mark_array (new mxArray (false, m, n, num_keys, keys));
}

// Copy constructor.
mxArray *
mxDuplicateArray (const mxArray *ptr)
{
  return maybe_mark_array (ptr->dup ());
}

// Destructor.
void
mxDestroyArray (mxArray *ptr)
{
  if (! (mex_context && mex_context->free_value (ptr)))
    delete ptr;
}

// Type Predicates.
bool
mxIsCell (const mxArray *ptr)
{
  return ptr->iscell ();
}

bool
mxIsChar (const mxArray *ptr)
{
  return ptr->is_char ();
}

bool
mxIsClass (const mxArray *ptr, const char *name)
{
  return ptr->is_class (name);
}

bool
mxIsComplex (const mxArray *ptr)
{
  return ptr->is_complex ();
}

bool
mxIsDouble (const mxArray *ptr)
{
  return ptr->is_double ();
}

bool
mxIsFunctionHandle (const mxArray *ptr)
{
  return ptr->is_function_handle ();
}

bool
mxIsInt16 (const mxArray *ptr)
{
  return ptr->is_int16 ();
}

bool
mxIsInt32 (const mxArray *ptr)
{
  return ptr->is_int32 ();
}

bool
mxIsInt64 (const mxArray *ptr)
{
  return ptr->is_int64 ();
}

bool
mxIsInt8 (const mxArray *ptr)
{
  return ptr->is_int8 ();
}

bool
mxIsLogical (const mxArray *ptr)
{
  return ptr->is_logical ();
}

bool
mxIsNumeric (const mxArray *ptr)
{
  return ptr->is_numeric ();
}

bool
mxIsSingle (const mxArray *ptr)
{
  return ptr->is_single ();
}

bool
mxIsSparse (const mxArray *ptr)
{
  return ptr->is_sparse ();
}

bool
mxIsStruct (const mxArray *ptr)
{
  return ptr->is_struct ();
}

bool
mxIsUint16 (const mxArray *ptr)
{
  return ptr->is_uint16 ();
}

bool
mxIsUint32 (const mxArray *ptr)
{
  return ptr->is_uint32 ();
}

bool
mxIsUint64 (const mxArray *ptr)
{
  return ptr->is_uint64 ();
}

bool
mxIsUint8 (const mxArray *ptr)
{
  return ptr->is_uint8 ();
}

// Odd type+size predicate.
bool
mxIsLogicalScalar (const mxArray *ptr)
{
  return ptr->is_logical_scalar ();
}

// Odd type+size+value predicate.
bool
mxIsLogicalScalarTrue (const mxArray *ptr)
{
  return ptr->is_logical_scalar_true ();
}

// Size predicate.
bool
mxIsEmpty (const mxArray *ptr)
{
  return ptr->isempty ();
}

bool
mxIsScalar (const mxArray *ptr)
{
  return ptr->is_scalar ();
}

// FIXME: Just plain odd thing to ask of a value.
// Still, Octave is incompatible because it does not implement this.
bool
mxIsFromGlobalWS (const mxArray * /*ptr*/)
{
  mexErrMsgTxt ("mxIsFromGlobalWS() is unimplemented");

  return 0;
}

// Dimension extractors.
std::size_t
mxGetM (const mxArray *ptr)
{
  return ptr->get_m ();
}

std::size_t
mxGetN (const mxArray *ptr)
{
  return ptr->get_n ();
}

const mwSize *
mxGetDimensions (const mxArray *ptr)
{
  return ptr->get_dimensions ();
}

mwSize
mxGetNumberOfDimensions (const mxArray *ptr)
{
  return ptr->get_number_of_dimensions ();
}

std::size_t
mxGetNumberOfElements (const mxArray *ptr)
{
  return ptr->get_number_of_elements ();
}

// Dimension setters.
void
mxSetM (mxArray *ptr, mwSize m)
{
  ptr->set_m (m);
}

void
mxSetN (mxArray *ptr, mwSize n)
{
  ptr->set_n (n);
}

int
mxSetDimensions (mxArray *ptr, const mwSize *dims, mwSize ndims)
{
  return (ptr->set_dimensions (static_cast<mwSize *>
                               (maybe_unmark (const_cast<mwSize *> (dims))),
                               ndims));
}

// Data extractors.
double *
mxGetPr (const mxArray *ptr)
{
  return static_cast<double *> (ptr->get_data ());
}

double
mxGetScalar (const mxArray *ptr)
{
  return ptr->get_scalar ();
}

mxChar *
mxGetChars (const mxArray *ptr)
{
  if (mxIsChar (ptr))
    return static_cast<mxChar *> (ptr->get_data ());
  else
    return nullptr;
}

mxLogical *
mxGetLogicals (const mxArray *ptr)
{
  return static_cast<mxLogical *> (ptr->get_data ());
}

void *
mxGetData (const mxArray *ptr)
{
  return ptr->get_data ();
}

double *
mxGetPi (const mxArray *ptr)
{
  return static_cast<double *> (ptr->get_imag_data ());
}

void *
mxGetImagData (const mxArray *ptr)
{
  return ptr->get_imag_data ();
}

mxDouble *
mxGetDoubles (const mxArray *ptr)
{
  return ptr->get_doubles ();
}

mxSingle *
mxGetSingles (const mxArray *ptr)
{
  return ptr->get_singles ();
}

mxInt8 *
mxGetInt8s (const mxArray *ptr)
{
  return ptr->get_int8s ();
}

mxInt16 *
mxGetInt16s (const mxArray *ptr)
{
  return ptr->get_int16s ();
}

mxInt32 *
mxGetInt32s (const mxArray *ptr)
{
  return ptr->get_int32s ();
}

mxInt64 *
mxGetInt64s (const mxArray *ptr)
{
  return ptr->get_int64s ();
}

mxUint8 *
mxGetUint8s (const mxArray *ptr)
{
  return ptr->get_uint8s ();
}

mxUint16 *
mxGetUint16s (const mxArray *ptr)
{
  return ptr->get_uint16s ();
}

mxUint32 *
mxGetUint32s (const mxArray *ptr)
{
  return ptr->get_uint32s ();
}

mxUint64 *
mxGetUint64s (const mxArray *ptr)
{
  return ptr->get_uint64s ();
}

mxComplexDouble *
mxGetComplexDoubles (const mxArray *ptr)
{
  return ptr->get_complex_doubles ();
}

mxComplexSingle *
mxGetComplexSingles (const mxArray *ptr)
{
  return ptr->get_complex_singles ();
}

// Data setters.
void
mxSetPr (mxArray *ptr, double *pr)
{
  ptr->set_data (maybe_unmark (pr));
}

void
mxSetData (mxArray *ptr, void *pr)
{
  ptr->set_data (maybe_unmark (pr));
}

int
mxSetDoubles (mxArray *ptr, mxDouble *data)
{
  return ptr->set_doubles (maybe_unmark (data));
}

int
mxSetSingles (mxArray *ptr, mxSingle *data)
{
  return ptr->set_singles (maybe_unmark (data));
}

int
mxSetInt8s (mxArray *ptr, mxInt8 *data)
{
  return ptr->set_int8s (maybe_unmark (data));
}

int
mxSetInt16s (mxArray *ptr, mxInt16 *data)
{
  return ptr->set_int16s (maybe_unmark (data));
}

int
mxSetInt32s (mxArray *ptr, mxInt32 *data)
{
  return ptr->set_int32s (maybe_unmark (data));
}

int
mxSetInt64s (mxArray *ptr, mxInt64 *data)
{
  return ptr->set_int64s (maybe_unmark (data));
}

int
mxSetUint8s (mxArray *ptr, mxUint8 *data)
{
  return ptr->set_uint8s (maybe_unmark (data));
}

int
mxSetUint16s (mxArray *ptr, mxUint16 *data)
{
  return ptr->set_uint16s (maybe_unmark (data));
}

int
mxSetUint32s (mxArray *ptr, mxUint32 *data)
{
  return ptr->set_uint32s (maybe_unmark (data));
}

int
mxSetUint64s (mxArray *ptr, mxUint64 *data)
{
  return ptr->set_uint64s (maybe_unmark (data));
}

int
mxSetComplexDoubles (mxArray *ptr, mxComplexDouble *data)
{
  return ptr->set_complex_doubles (maybe_unmark (data));
}

int
mxSetComplexSingles (mxArray *ptr, mxComplexSingle *data)
{
  return ptr->set_complex_singles (maybe_unmark (data));
}

void
mxSetPi (mxArray *ptr, double *pi)
{
  ptr->set_imag_data (maybe_unmark (pi));
}

void
mxSetImagData (mxArray *ptr, void *pi)
{
  ptr->set_imag_data (maybe_unmark (pi));
}

// Classes.
mxClassID
mxGetClassID (const mxArray *ptr)
{
  return ptr->get_class_id ();
}

const char *
mxGetClassName (const mxArray *ptr)
{
  return ptr->get_class_name ();
}

void
mxSetClassName (mxArray *ptr, const char *name)
{
  ptr->set_class_name (name);
}

void
mxSetProperty (mxArray *ptr, mwIndex idx, const char *property_name,
               const mxArray *property_value)
{
  ptr->set_property (idx, property_name, property_value);
}

mxArray *
mxGetProperty (const mxArray *ptr, mwIndex idx, const char *property_name)
{
  return ptr->get_property (idx, property_name);
}

// Cell support.
mxArray *
mxGetCell (const mxArray *ptr, mwIndex idx)
{
  return ptr->get_cell (idx);
}

void
mxSetCell (mxArray *ptr, mwIndex idx, mxArray *val)
{
  ptr->set_cell (idx, val);
}

// Sparse support.
mwIndex *
mxGetIr (const mxArray *ptr)
{
  return ptr->get_ir ();
}

mwIndex *
mxGetJc (const mxArray *ptr)
{
  return ptr->get_jc ();
}

mwSize
mxGetNzmax (const mxArray *ptr)
{
  return ptr->get_nzmax ();
}

void
mxSetIr (mxArray *ptr, mwIndex *ir)
{
  ptr->set_ir (static_cast<mwIndex *> (maybe_unmark (ir)));
}

void
mxSetJc (mxArray *ptr, mwIndex *jc)
{
  ptr->set_jc (static_cast<mwIndex *> (maybe_unmark (jc)));
}

void
mxSetNzmax (mxArray *ptr, mwSize nzmax)
{
  ptr->set_nzmax (nzmax);
}

// Structure support.
int
mxAddField (mxArray *ptr, const char *key)
{
  return ptr->add_field (key);
}

void
mxRemoveField (mxArray *ptr, int key_num)
{
  ptr->remove_field (key_num);
}

mxArray *
mxGetField (const mxArray *ptr, mwIndex index, const char *key)
{
  int key_num = mxGetFieldNumber (ptr, key);
  return mxGetFieldByNumber (ptr, index, key_num);
}

mxArray *
mxGetFieldByNumber (const mxArray *ptr, mwIndex index, int key_num)
{
  return ptr->get_field_by_number (index, key_num);
}

void
mxSetField (mxArray *ptr, mwIndex index, const char *key, mxArray *val)
{
  int key_num = mxGetFieldNumber (ptr, key);
  mxSetFieldByNumber (ptr, index, key_num, val);
}

void
mxSetFieldByNumber (mxArray *ptr, mwIndex index, int key_num, mxArray *val)
{
  ptr->set_field_by_number (index, key_num, val);
}

int
mxGetNumberOfFields (const mxArray *ptr)
{
  return ptr->get_number_of_fields ();
}

const char *
mxGetFieldNameByNumber (const mxArray *ptr, int key_num)
{
  return ptr->get_field_name_by_number (key_num);
}

int
mxGetFieldNumber (const mxArray *ptr, const char *key)
{
  return ptr->get_field_number (key);
}

int
mxGetString (const mxArray *ptr, char *buf, mwSize buflen)
{
  return ptr->get_string (buf, buflen);
}

char *
mxArrayToString (const mxArray *ptr)
{
  return ptr->array_to_string ();
}

mwIndex
mxCalcSingleSubscript (const mxArray *ptr, mwSize nsubs, mwIndex *subs)
{
  return ptr->calc_single_subscript (nsubs, subs);
}

std::size_t
mxGetElementSize (const mxArray *ptr)
{
  return ptr->get_element_size ();
}

// ------------------------------------------------------------------

// C interface to mex functions:

const char *
mexFunctionName ()
{
  return mex_context ? mex_context->function_name () : "unknown";
}

int
mexCallMATLAB (int nargout, mxArray *argout[], int nargin,
               mxArray *argin[], const char *fname)
{
  octave_value_list args = mx_to_ov_args (nargin, argin);

  octave::interpreter& interp = octave::__get_interpreter__ ();

  bool execution_error = false;

  octave_value_list retval;

  try
    {
      octave::tree_evaluator& tw = interp.get_evaluator ();

      octave::unwind_action act
      ([&tw] (const std::list<octave::octave_lvalue> *lvl)
      {
        tw.set_lvalue_list (lvl);
      }, tw.lvalue_list ());

      tw.set_lvalue_list (nullptr);

      retval = interp.feval (fname, args, nargout);
    }
  catch (const octave::execution_exception&)
    {
      if (mex_context->trap_feval_error)
        {
          // FIXME: is there a way to indicate what error occurred?
          // Should the error message be displayed here?  Do we need to
          // save the exception info for lasterror?

          interp.recover_from_exception ();

          execution_error = true;
        }
      else
        {
          args.resize (0);
          retval.resize (0);

          throw;
        }
    }

  int num_to_copy = retval.length ();

  if (nargout < retval.length ())
    num_to_copy = nargout;

  for (int i = 0; i < num_to_copy; i++)
    {
      // FIXME: it would be nice to avoid copying the value here,
      // but there is no way to steal memory from a matrix, never mind
      // that matrix memory is allocated by new[] and mxArray memory
      // is allocated by malloc().
      argout[i] = mex_context->make_value (retval(i));
    }

  while (num_to_copy < nargout)
    argout[num_to_copy++] = nullptr;

  return execution_error ? 1 : 0;
}

mxArray *
mexCallMATLABWithTrap (int nargout, mxArray *argout[], int nargin,
                       mxArray *argin[], const char *fname)
{
  mxArray *mx = nullptr;

  int old_flag = (mex_context ? mex_context->trap_feval_error : 0);
  mexSetTrapFlag (1);
  if (mexCallMATLAB (nargout, argout, nargin, argin, fname))
    {
      const char *field_names[] = {"identifier", "message", "case", "stack"};
      mx = mxCreateStructMatrix (1, 1, 4, field_names);
      mxSetFieldByNumber (mx, 0, 0, mxCreateString ("Octave:MEX"));
      std::string msg = "mexCallMATLABWithTrap: function call <"
                        + std::string (fname) + "> failed";
      mxSetFieldByNumber (mx, 0, 1, mxCreateString (msg.c_str ()));
      mxSetFieldByNumber (mx, 0, 2, mxCreateCellMatrix (0, 0));
      mxSetFieldByNumber (mx, 0, 3, mxCreateStructMatrix (0, 1, 0, nullptr));
    }
  mexSetTrapFlag (old_flag);

  return mx;
}

void
mexSetTrapFlag (int flag)
{
  if (mex_context)
    mex_context->trap_feval_error = flag;
}

int
mexEvalString (const char *s)
{
  int retval = 0;

  octave::interpreter& interp = octave::__get_interpreter__ ();

  int parse_status;
  bool execution_error = false;

  octave_value_list ret;

  try
    {
      ret = interp.eval_string (std::string (s), false, parse_status, 0);
    }
  catch (const octave::execution_exception&)
    {
      interp.recover_from_exception ();

      execution_error = true;
    }

  if (parse_status || execution_error)
    retval = 1;

  return retval;
}

mxArray *
mexEvalStringWithTrap (const char *s)
{
  mxArray *mx = nullptr;

  octave::interpreter& interp = octave::__get_interpreter__ ();

  int parse_status;
  bool execution_error = false;

  octave_value_list ret;

  try
    {
      ret = interp.eval_string (std::string (s), false, parse_status, 0);
    }
  catch (const octave::execution_exception&)
    {
      interp.recover_from_exception ();

      execution_error = true;
    }

  if (parse_status || execution_error)
    {
      const char *field_names[] = {"identifier", "message", "case", "stack"};
      mx = mxCreateStructMatrix (1, 1, 4, field_names);
      mxSetFieldByNumber (mx, 0, 0, mxCreateString ("Octave:MEX"));
      std::string msg = "mexEvalStringWithTrap: eval of <"
                        + std::string (s) + "> failed";
      mxSetFieldByNumber (mx, 0, 1, mxCreateString (msg.c_str ()));
      mxSetFieldByNumber (mx, 0, 2, mxCreateCellMatrix (0, 0));
      mxSetFieldByNumber (mx, 0, 3, mxCreateStructMatrix (0, 1, 0, nullptr));
    }

  return mx;
}

void
mexErrMsgTxt (const char *s)
{
  mexErrMsgTxt_impl (mexFunctionName (), s);
}

void
mexErrMsgIdAndTxt (const char *id, const char *fmt, ...)
{
  if (fmt && strlen (fmt) > 0)
    {
      const char *fname = mexFunctionName ();
      std::size_t len = strlen (fname) + 2 + strlen (fmt) + 1;
      OCTAVE_LOCAL_BUFFER (char, tmpfmt, len);
      sprintf (tmpfmt, "%s: %s", fname, fmt);
      va_list args;
      va_start (args, fmt);
      verror_with_id (id, tmpfmt, args);
      va_end (args);
    }
  else
    {
      // For compatibility with Matlab, print an empty message.
      // Octave's error routine requires a non-null input so use a SPACE.
      error (" ");
    }
}

void
mexWarnMsgTxt (const char *s)
{
  std::size_t len;

  if (s && (len = strlen (s)) > 0)
    {
      if (s[len - 1] == '\n')
        {
          std::string s_tmp (s, len - 1);
          warning ("%s\n", s_tmp.c_str ());
        }
      else
        warning ("%s", s);
    }
  else
    {
      // For compatibility with Matlab, print an empty message.
      // Octave's warning routine requires a non-null input so use a SPACE.
      warning (" ");
    }
}

void
mexWarnMsgIdAndTxt (const char *id, const char *fmt, ...)
{
  // FIXME: is this right?  What does Matlab do if fmt is NULL or
  //        an empty string?

  if (fmt && strlen (fmt) > 0)
    {
      const char *fname = mexFunctionName ();
      std::size_t len = strlen (fname) + 2 + strlen (fmt) + 1;
      OCTAVE_LOCAL_BUFFER (char, tmpfmt, len);
      sprintf (tmpfmt, "%s: %s", fname, fmt);
      va_list args;
      va_start (args, fmt);
      vwarning_with_id (id, tmpfmt, args);
      va_end (args);
    }
}

int
mexPrintf (const char *fmt, ...)
{
  int retval;
  va_list args;
  va_start (args, fmt);
  retval = octave::vformat (octave_stdout, fmt, args);
  va_end (args);
  return retval;
}

mxArray *
mexGetVariable (const char *space, const char *name)
{
  mxArray *retval = nullptr;

  octave_value val;

  octave::interpreter& interp = octave::__get_interpreter__ ();

  if (! strcmp (space, "global"))
    val = interp.global_varval (name);
  else
    {
      // FIXME: should this be in variables.cc?

      octave::unwind_protect frame;

      bool caller = ! strcmp (space, "caller");
      bool base = ! strcmp (space, "base");

      if (caller || base)
        {
          // MEX files don't create a separate frame in the call stack,
          // so we are already in the "caller" frame.

          if (base)
            {
              octave::tree_evaluator& tw = interp.get_evaluator ();

              frame.add (&octave::tree_evaluator::restore_frame, &tw,
                         tw.current_call_stack_frame_number ());

              tw.goto_base_frame ();
            }

          val = interp.varval (name);
        }
      else
        mexErrMsgTxt ("mexGetVariable: symbol table does not exist");
    }

  if (val.is_defined ())
    {
      retval = mex_context->make_value (val);

      retval->set_name (name);
    }

  return retval;
}

const mxArray *
mexGetVariablePtr (const char *space, const char *name)
{
  return mexGetVariable (space, name);
}

int
mexPutVariable (const char *space, const char *name, const mxArray *ptr)
{
  return mexPutVariable_impl (space, name, ptr);
}

void
mexMakeArrayPersistent (mxArray *ptr)
{
  maybe_unmark_array (ptr);
}

void
mexMakeMemoryPersistent (void *ptr)
{
  maybe_unmark (ptr);
}

int
mexAtExit (void (*f) ())
{
  if (mex_context)
    {
      octave_mex_function& curr_mex_fcn = mex_context->current_mex_function ();

      curr_mex_fcn.atexit (f);
    }

  return 0;
}

const mxArray *
mexGet_interleaved (double handle, const char *property)
{
  mxArray *m = nullptr;

  octave_value ret
    = octave::get_property_from_handle (handle, property, "mexGet");

  if (ret.is_defined ())
    m = ret.as_mxArray (true);

  return m;
}

const mxArray *
mexGet (double handle, const char *property)
{
  mxArray *m = nullptr;

  octave_value ret
    = octave::get_property_from_handle (handle, property, "mexGet");

  if (ret.is_defined ())
    m = ret.as_mxArray (false);

  return m;
}

int
mexIsGlobal (const mxArray *ptr)
{
  return mxIsFromGlobalWS (ptr);
}

int
mexIsLocked ()
{
  int retval = 0;

  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      octave::interpreter& interp = octave::__get_interpreter__ ();

      retval = interp.mislocked (fname);
    }

  return retval;
}

std::map<std::string, int> mex_lock_count;

void
mexLock ()
{
  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      if (mex_lock_count.find (fname) == mex_lock_count.end ())
        mex_lock_count[fname] = 1;
      else
        mex_lock_count[fname]++;

      octave::interpreter& interp = octave::__get_interpreter__ ();

      interp.mlock ();
    }
}

int
mexSet (double handle, const char *property, mxArray *val)
{
  return mexSet_impl (handle, property, val);
}

void
mexUnlock ()
{
  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      auto p = mex_lock_count.find (fname);

      if (p != mex_lock_count.end ())
        {
          int count = --mex_lock_count[fname];

          if (count == 0)
            {
              octave::interpreter& interp = octave::__get_interpreter__ ();

              interp.munlock (fname);

              mex_lock_count.erase (p);
            }
        }
    }
}
