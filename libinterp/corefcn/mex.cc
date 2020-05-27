////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2020 The Octave Project Developers
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

#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cctype>

#include <limits>
#include <map>
#include <set>
#include <string>

#include "f77-fcn.h"
#include "lo-ieee.h"
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
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "graphics.h"

// These must be declared extern "C" but may be omitted from the set of
// symbols declared in mexproto.h, so we declare them here as well.

extern "C"
{
  extern OCTINTERP_API const mxArray *
  mexGet_interleaved (double handle, const char *property);

  extern OCTINTERP_API mxArray *
  mxCreateCellArray (mwSize ndims, const mwSize *dims);

  extern OCTINTERP_API mxArray *
  mxCreateCellMatrix (mwSize m, mwSize n);

  extern OCTINTERP_API mxArray *
  mxCreateCharArray (mwSize ndims, const mwSize *dims);

  extern OCTINTERP_API mxArray *
  mxCreateCharMatrixFromStrings (mwSize m, const char **str);

  extern OCTINTERP_API mxArray *
  mxCreateDoubleMatrix (mwSize nr, mwSize nc, mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateDoubleScalar (double val);

  extern OCTINTERP_API mxArray *
  mxCreateLogicalArray (mwSize ndims, const mwSize *dims);

  extern OCTINTERP_API mxArray *
  mxCreateLogicalMatrix (mwSize m, mwSize n);

  extern OCTINTERP_API mxArray *
  mxCreateLogicalScalar (mxLogical val);

  extern OCTINTERP_API mxArray *
  mxCreateNumericArray (mwSize ndims, const mwSize *dims, mxClassID class_id,
                        mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                         mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateUninitNumericArray (mwSize ndims, const mwSize *dims,
                              mxClassID class_id, mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateUninitNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                               mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateSparse (mwSize m, mwSize n, mwSize nzmax, mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateSparseLogicalMatrix (mwSize m, mwSize n, mwSize nzmax);

  extern OCTINTERP_API mxArray *
  mxCreateString (const char *str);

  extern OCTINTERP_API mxArray *
  mxCreateStructArray (mwSize ndims, const mwSize *dims, int num_keys,
                       const char **keys);

  extern OCTINTERP_API mxArray *
  mxCreateStructMatrix (mwSize rows, mwSize cols, int num_keys,
                        const char **keys);

  extern OCTINTERP_API mxArray *
  mxCreateCellArray_interleaved (mwSize ndims, const mwSize *dims);

  extern OCTINTERP_API mxArray *
  mxCreateCellMatrix_interleaved (mwSize m, mwSize n);

  extern OCTINTERP_API mxArray *
  mxCreateCharArray_interleaved (mwSize ndims, const mwSize *dims);

  extern OCTINTERP_API mxArray *
  mxCreateCharMatrixFromStrings_interleaved (mwSize m, const char **str);

  extern OCTINTERP_API mxArray *
  mxCreateDoubleMatrix_interleaved (mwSize nr, mwSize nc, mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateDoubleScalar_interleaved (double val);

  extern OCTINTERP_API mxArray *
  mxCreateLogicalArray_interleaved (mwSize ndims, const mwSize *dims);

  extern OCTINTERP_API mxArray *
  mxCreateLogicalMatrix_interleaved (mwSize m, mwSize n);

  extern OCTINTERP_API mxArray *
  mxCreateLogicalScalar_interleaved (mxLogical val);

  extern OCTINTERP_API mxArray *
  mxCreateNumericArray_interleaved (mwSize ndims, const mwSize *dims,
                                    mxClassID class_id, mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateNumericMatrix_interleaved (mwSize m, mwSize n, mxClassID class_id,
                                     mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateUninitNumericArray_interleaved (mwSize ndims, const mwSize *dims,
                                          mxClassID class_id,
                                          mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateUninitNumericMatrix_interleaved (mwSize m, mwSize n,
                                           mxClassID class_id,
                                           mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateSparse_interleaved (mwSize m, mwSize n, mwSize nzmax,
                              mxComplexity flag);

  extern OCTINTERP_API mxArray *
  mxCreateSparseLogicalMatrix_interleaved (mwSize m, mwSize n, mwSize nzmax);

  extern OCTINTERP_API mxArray *
  mxCreateString_interleaved (const char *str);

  extern OCTINTERP_API mxArray *
  mxCreateStructArray_interleaved (mwSize ndims, const mwSize *dims,
                                   int num_keys, const char **keys);

  extern OCTINTERP_API mxArray *
  mxCreateStructMatrix_interleaved (mwSize rows, mwSize cols, int num_keys,
                                    const char **keys);

  extern OCTINTERP_API int mxMakeArrayReal (mxArray *ptr);
  extern OCTINTERP_API int mxMakeArrayComplex (mxArray *ptr);

  extern OCTINTERP_API mxDouble * mxGetDoubles (const mxArray *p);
  extern OCTINTERP_API mxSingle * mxGetSingles (const mxArray *p);
  extern OCTINTERP_API mxInt8 * mxGetInt8s (const mxArray *p);
  extern OCTINTERP_API mxInt16 * mxGetInt16s (const mxArray *p);
  extern OCTINTERP_API mxInt32 * mxGetInt32s (const mxArray *p);
  extern OCTINTERP_API mxInt64 * mxGetInt64s (const mxArray *p);
  extern OCTINTERP_API mxUint8 * mxGetUint8s (const mxArray *p);
  extern OCTINTERP_API mxUint16 * mxGetUint16s (const mxArray *p);
  extern OCTINTERP_API mxUint32 * mxGetUint32s (const mxArray *p);
  extern OCTINTERP_API mxUint64 * mxGetUint64s (const mxArray *p);

  extern OCTINTERP_API mxComplexDouble * mxGetComplexDoubles (const mxArray *p);
  extern OCTINTERP_API mxComplexSingle * mxGetComplexSingles (const mxArray *p);
#if 0
  /* We don't have these yet. */
  extern OCTINTERP_API mxComplexInt8 * mxGetComplexInt8s (const mxArray *p);
  extern OCTINTERP_API mxComplexInt16 * mxGetComplexInt16s (const mxArray *p);
  extern OCTINTERP_API mxComplexInt32 * mxGetComplexInt32s (const mxArray *p);
  extern OCTINTERP_API mxComplexInt64 * mxGetComplexInt64s (const mxArray *p);
  extern OCTINTERP_API mxComplexUint8 * mxGetComplexUint8s (const mxArray *p);
  extern OCTINTERP_API mxComplexUint16 * mxGetComplexUint16s (const mxArray *p);
  extern OCTINTERP_API mxComplexUint32 * mxGetComplexUint32s (const mxArray *p);
  extern OCTINTERP_API mxComplexUint64 * mxGetComplexUint64s (const mxArray *p);
#endif

  extern OCTINTERP_API double * mxGetPi (const mxArray *ptr);
  extern OCTINTERP_API void * mxGetImagData (const mxArray *ptr);

  extern OCTINTERP_API int mxSetDoubles (mxArray *p, mxDouble *d);
  extern OCTINTERP_API int mxSetSingles (mxArray *p, mxSingle *d);
  extern OCTINTERP_API int mxSetInt8s (mxArray *p, mxInt8 *d);
  extern OCTINTERP_API int mxSetInt16s (mxArray *p, mxInt16 *d);
  extern OCTINTERP_API int mxSetInt32s (mxArray *p, mxInt32 *d);
  extern OCTINTERP_API int mxSetInt64s (mxArray *p, mxInt64 *d);
  extern OCTINTERP_API int mxSetUint8s (mxArray *p, mxUint8 *d);
  extern OCTINTERP_API int mxSetUint16s (mxArray *p, mxUint16 *d);
  extern OCTINTERP_API int mxSetUint32s (mxArray *p, mxUint32 *d);
  extern OCTINTERP_API int mxSetUint64s (mxArray *p, mxUint64 *d);

  extern OCTINTERP_API int mxSetComplexDoubles (mxArray *p, mxComplexDouble *d);
  extern OCTINTERP_API int mxSetComplexSingles (mxArray *p, mxComplexSingle *d);
#if 0
  /* We don't have these yet. */
  extern OCTINTERP_API int mxSetComplexInt8s (mxArray *p, mxComplexInt8 *d);
  extern OCTINTERP_API int mxSetComplexInt16s (mxArray *p, mxComplexInt16 *d);
  extern OCTINTERP_API int mxSetComplexInt32s (mxArray *p, mxComplexInt32 *d);
  extern OCTINTERP_API int mxSetComplexInt64s (mxArray *p, mxComplexInt64 *d);
  extern OCTINTERP_API int mxSetComplexUint8s (mxArray *p, mxComplexUint8 *d);
  extern OCTINTERP_API int mxSetComplexUint16s (mxArray *p, mxComplexUint16 *d);
  extern OCTINTERP_API int mxSetComplexUint32s (mxArray *p, mxComplexUint32 *d);
  extern OCTINTERP_API int mxSetComplexUint64s (mxArray *p, mxComplexUint64 *d);
#endif

  extern OCTINTERP_API void mxSetPi (mxArray *ptr, double *pi);
  extern OCTINTERP_API void mxSetImagData (mxArray *ptr, void *pi);
}

// #define DEBUG 1

static void
xfree (void *ptr)
{
  ::free (ptr);
}

static mwSize
max_str_len (mwSize m, const char **str)
{
  int max_len = 0;

  for (mwSize i = 0; i < m; i++)
    {
      mwSize tmp = strlen (str[i]);

      if (tmp > max_len)
        max_len = tmp;
    }

  return max_len;
}

static int
valid_key (const char *key)
{
  int retval = 0;

  int nel = strlen (key);

  if (nel > 0)
    {
      if (isalpha (key[0]))
        {
          for (int i = 1; i < nel; i++)
            {
              if (! (isalnum (key[i]) || key[i] == '_'))
                return retval;
            }

          retval = 1;
        }
    }

  return retval;
}

// ------------------------------------------------------------------

mxArray_base::mxArray_base (bool interleaved)
  : m_interleaved (interleaved)
{ }

static mwIndex
calc_single_subscript_internal (mwSize ndims, const mwSize *dims,
                                mwSize nsubs, const mwIndex *subs)
{
  mwIndex retval = 0;

  switch (nsubs)
    {
    case 0:
      break;

    case 1:
      retval = subs[0];
      break;

    default:
      {
        // Both nsubs and ndims should be at least 2 here.

        mwSize n = (nsubs <= ndims ? nsubs : ndims);

        retval = subs[--n];

        while (--n >= 0)
          retval = dims[n] * retval + subs[n];
      }
      break;
    }

  return retval;
}

// The object that handles values pass to MEX files from Octave.  Some
// methods in this class may set mutate_flag to TRUE to tell the
// mxArray class to convert to the Matlab-style representation and
// then invoke the method on that object instead (for example, getting
// a pointer to real or imaginary data from a complex object requires
// a mutation but getting a pointer to real data from a real object
// does not).  Changing the representation causes a copy so we try to
// avoid it unless it is really necessary.  Once the conversion
// happens, we delete this representation, so the conversion can only
// happen once per call to a MEX file.

static inline void * maybe_mark_foreign (void *ptr);

#define VOID_MUTATION_METHOD(FCN_NAME, ARG_LIST)        \
  void FCN_NAME ARG_LIST { request_mutation (); }

#define CONST_VOID_MUTATION_METHOD(FCN_NAME, ARG_LIST)  \
  void FCN_NAME ARG_LIST const { request_mutation (); }

#define MUTATION_METHOD(RET_TYPE, FCN_NAME, ARG_LIST, RET_VAL)          \
  RET_TYPE FCN_NAME ARG_LIST { request_mutation (); return RET_VAL; }

#define CONST_MUTATION_METHOD(RET_TYPE, FCN_NAME, ARG_LIST, RET_VAL)    \
  RET_TYPE FCN_NAME ARG_LIST const { request_mutation (); return RET_VAL; }

class mxArray_octave_value : public mxArray_base
{
public:

  mxArray_octave_value (bool interleaved, const octave_value& ov)
    : mxArray_base (interleaved), val (ov), mutate_flag (false),
      id (mxUNKNOWN_CLASS), class_name (nullptr), ndims (-1), dims (nullptr)
  { }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_octave_value& operator = (const mxArray_octave_value&) = delete;

  mxArray_base * dup (void) const { return new mxArray_octave_value (*this); }

  mxArray * as_mxArray (void) const
  {
    mxArray *retval = val.as_mxArray (m_interleaved);

    // RETVAL is assumed to be an mxArray_matlab object.  Should we
    // assert that condition here?

    if (retval)
      {
        // Preserve cached values of class name and dimensions in case
        // they will be used after we mutate.

        // set_class_name will handle deleting class name that comes
        // from as_mxArray conversion function.

        if (class_name)
          {
            retval->set_class_name (class_name);

            class_name = nullptr;
          }

        if (dims)
          {
            mwSize *xdims = retval->get_dimensions ();

            mxFree (xdims);

            retval->set_dimensions (dims, ndims);

            dims = nullptr;
          }
      }

    return retval;
  }

  ~mxArray_octave_value (void)
  {
    mxFree (class_name);
    mxFree (dims);
  }

  bool is_octave_value (void) const { return true; }

  int iscell (void) const { return val.iscell (); }

  int is_char (void) const { return val.is_string (); }

  int is_complex (void) const { return val.iscomplex (); }

  int is_double (void) const { return val.is_double_type (); }

  int is_function_handle (void) const { return val.is_function_handle (); }

  int is_int16 (void) const { return val.is_int16_type (); }

  int is_int32 (void) const { return val.is_int32_type (); }

  int is_int64 (void) const { return val.is_int64_type (); }

  int is_int8 (void) const { return val.is_int8_type (); }

  int is_logical (void) const { return val.islogical (); }

  int is_numeric (void) const { return val.isnumeric (); }

  int is_single (void) const { return val.is_single_type (); }

  int is_sparse (void) const { return val.issparse (); }

  int is_struct (void) const { return val.isstruct (); }

  int is_uint16 (void) const { return val.is_uint16_type (); }

  int is_uint32 (void) const { return val.is_uint32_type (); }

  int is_uint64 (void) const { return val.is_uint64_type (); }

  int is_uint8 (void) const { return val.is_uint8_type (); }

  int is_range (void) const { return val.is_range (); }

  int isreal (void) const { return val.isreal (); }

  int is_logical_scalar_true (void) const
  {
    return (is_logical_scalar () && val.is_true ());
  }

  mwSize get_m (void) const { return val.rows (); }

  mwSize get_n (void) const
  {
    mwSize n = 1;

    // Force dims and ndims to be cached.
    get_dimensions ();

    for (mwIndex i = ndims - 1; i > 0; i--)
      n *= dims[i];

    return n;
  }

  mwSize * get_dimensions (void) const
  {
    if (! dims)
      {
        ndims = val.ndims ();

        dims = static_cast<mwSize *> (mxArray::malloc (ndims
                                                       * sizeof (mwSize)));

        dim_vector dv = val.dims ();

        for (mwIndex i = 0; i < ndims; i++)
          dims[i] = dv(i);
      }

    return dims;
  }

  mwSize get_number_of_dimensions (void) const
  {
    // Force dims and ndims to be cached.
    get_dimensions ();

    return ndims;
  }

  VOID_MUTATION_METHOD (set_m, (mwSize))
  VOID_MUTATION_METHOD (set_n, (mwSize))

  MUTATION_METHOD (int, set_dimensions, (mwSize *, mwSize), 0)

  mwSize get_number_of_elements (void) const { return val.numel (); }

  int isempty (void) const { return val.isempty (); }

  bool is_scalar (void) const
  {
    // Force dims and ndims to be cached.
    get_dimensions ();

    return ndims == 2 && dims[0] == 1 && dims[1] == 1;
  }

  mxClassID get_class_id (void) const
  {
    id = mxUNKNOWN_CLASS;

    std::string cn = val.class_name ();

    if (cn == "double")
      id = mxDOUBLE_CLASS;
    else if (cn == "single")
      id = mxSINGLE_CLASS;
    else if (cn == "char")
      id = mxCHAR_CLASS;
    else if (cn == "logical")
      id = mxLOGICAL_CLASS;
    else if (cn == "cell")
      id = mxCELL_CLASS;
    else if (cn == "struct")
      id = mxSTRUCT_CLASS;
    else if (cn == "function_handle")
      id = mxFUNCTION_CLASS;
    else if (cn == "int8")
      id = mxINT8_CLASS;
    else if (cn == "uint8")
      id = mxUINT8_CLASS;
    else if (cn == "int16")
      id = mxINT16_CLASS;
    else if (cn == "uint16")
      id = mxUINT16_CLASS;
    else if (cn == "int32")
      id = mxINT32_CLASS;
    else if (cn == "uint32")
      id = mxUINT32_CLASS;
    else if (cn == "int64")
      id = mxINT64_CLASS;
    else if (cn == "uint64")
      id = mxUINT64_CLASS;

    return id;
  }

  const char * get_class_name (void) const
  {
    if (! class_name)
      {
        std::string s = val.class_name ();
        class_name = mxArray::strsave (s.c_str ());
      }

    return class_name;
  }

  // Not allowed.
  VOID_MUTATION_METHOD (set_class_name, (const char *))

  mxArray * get_property (mwIndex idx, const char *pname) const
  {
    mxArray *retval = nullptr;

    if (val.is_classdef_object ())
      {
        octave_classdef *ov_cdef = val.classdef_object_value ();

        if (ov_cdef)
          {
            octave_value pval = ov_cdef->get_property (idx, pname);

            if (pval.is_defined())
              retval = new mxArray (m_interleaved, pval);
          }
      }

    return retval;
  }

  void set_property (mwIndex idx, const char *pname, const mxArray *pval)
  {
    if (val.is_classdef_object ())
      {
        octave_classdef *ov_cdef = val.classdef_object_value ();

        if (ov_cdef)
          ov_cdef->set_property (idx, pname, pval->as_octave_value ());
      }
    else
      err_invalid_type ("set_property");
  }

  CONST_MUTATION_METHOD (mxArray *, get_cell, (mwIndex), nullptr)

  // Not allowed.
  VOID_MUTATION_METHOD (set_cell, (mwIndex, mxArray *))

  double get_scalar (void) const
  {
    if (val.issparse ())
      {
        // For sparse arrays, return the first non-zero value.
        void *data = val.mex_get_data ();
        if (data == nullptr)
          return 0.0;

        if (val.islogical ())
          return *static_cast<bool *> (data);
        else if (val.isreal ())
          return *static_cast<double *> (data);
        else  // Complex type, only return real part
          return *static_cast<double *> (data);
      }
    else
      return val.scalar_value (true);
  }

  void * get_data (void) const
  {
    void *retval = val.mex_get_data ();

    if (retval)
      maybe_mark_foreign (retval);
    else
      request_mutation ();

    return retval;
  }

  template <typename T>
  T * get_data (mxClassID class_id, mxComplexity complexity) const
  {
    T *retval = static_cast<T *> (val.mex_get_data (class_id, complexity));

    if (retval)
      maybe_mark_foreign (retval);
    else
      request_mutation ();

    return retval;
  }

  CONST_MUTATION_METHOD (mxDouble *, get_doubles, (void), nullptr);

  CONST_MUTATION_METHOD (mxSingle *, get_singles, (void), nullptr);

  CONST_MUTATION_METHOD (mxInt8 *, get_int8s, (void), nullptr);

  CONST_MUTATION_METHOD (mxInt16 *, get_int16s, (void), nullptr);

  CONST_MUTATION_METHOD (mxInt32 *, get_int32s, (void), nullptr);

  CONST_MUTATION_METHOD (mxInt64 *, get_int64s, (void), nullptr);

  CONST_MUTATION_METHOD (mxUint8 *, get_uint8s, (void), nullptr);

  CONST_MUTATION_METHOD (mxUint16 *, get_uint16s, (void), nullptr);

  CONST_MUTATION_METHOD (mxUint32 *, get_uint32s, (void), nullptr);

  CONST_MUTATION_METHOD (mxUint64 *, get_uint64s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexDouble *, get_complex_doubles, (void), nullptr);
  CONST_MUTATION_METHOD (mxComplexSingle *, get_complex_singles, (void), nullptr);

#if 0
  /* We don't have these yet. */
  CONST_MUTATION_METHOD (mxComplexInt8 *, get_complex_int8s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexInt16 *, get_complex_int16s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexInt32 *, get_complex_int32s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexInt64 *, get_complex_int64s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexUint8 *, get_complex_uint8s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexUint16 *, get_complex_uint16s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexUint32 *, get_complex_uint32s, (void), nullptr);

  CONST_MUTATION_METHOD (mxComplexUint64 *, get_complex_uint64s, (void), nullptr);
#endif

  void * get_imag_data (void) const
  {
    void *retval = nullptr;

    if (is_numeric () && isreal ())
      retval = nullptr;
    else
      request_mutation ();

    return retval;
  }

  // Not allowed.
  VOID_MUTATION_METHOD (set_data, (void *))

  MUTATION_METHOD (int, set_doubles, (mxDouble *), 0)
  MUTATION_METHOD (int, set_singles, (mxSingle *), 0)
  MUTATION_METHOD (int, set_int8s, (mxInt8 *), 0)
  MUTATION_METHOD (int, set_int16s, (mxInt16 *), 0)
  MUTATION_METHOD (int, set_int32s, (mxInt32 *), 0)
  MUTATION_METHOD (int, set_int64s, (mxInt64 *), 0)
  MUTATION_METHOD (int, set_uint8s, (mxUint8 *), 0)
  MUTATION_METHOD (int, set_uint16s, (mxUint16 *), 0)
  MUTATION_METHOD (int, set_uint32s, (mxUint32 *), 0)
  MUTATION_METHOD (int, set_uint64s, (mxUint64 *), 0)

  MUTATION_METHOD (int, set_complex_doubles, (mxComplexDouble *), 0)
  MUTATION_METHOD (int, set_complex_singles, (mxComplexSingle *), 0)
#if 0
  /* We don't have these yet. */
  MUTATION_METHOD (int, set_complex_int8s, (mxComplexInt8 *), 0)
  MUTATION_METHOD (int, set_complex_int16s, (mxComplexInt16 *), 0)
  MUTATION_METHOD (int, set_complex_int32s, (mxComplexInt32 *), 0)
  MUTATION_METHOD (int, set_complex_int64s, (mxComplexInt64 *), 0)
  MUTATION_METHOD (int, set_complex_uint8s, (mxComplexUint8 *), 0)
  MUTATION_METHOD (int, set_complex_uint16s, (mxComplexUint16 *), 0)
  MUTATION_METHOD (int, set_complex_uint32s, (mxComplexUint32 *), 0)
  MUTATION_METHOD (int, set_complex_uint64s, (mxComplexUint64 *), 0)
#endif

  // Not allowed.
  VOID_MUTATION_METHOD (set_imag_data, (void *))

  mwIndex * get_ir (void) const
  {
    return static_cast<mwIndex *> (maybe_mark_foreign (val.mex_get_ir ()));
  }

  mwIndex * get_jc (void) const
  {
    return static_cast<mwIndex *> (maybe_mark_foreign (val.mex_get_jc ()));
  }

  mwSize get_nzmax (void) const { return val.nzmax (); }

  // Not allowed.
  VOID_MUTATION_METHOD (set_ir, (mwIndex *))

  // Not allowed.
  VOID_MUTATION_METHOD (set_jc, (mwIndex *))

  // Not allowed.
  VOID_MUTATION_METHOD (set_nzmax, (mwSize))

  // Not allowed.
  MUTATION_METHOD (int, add_field, (const char *), 0)

  // Not allowed.
  VOID_MUTATION_METHOD (remove_field, (int))

  CONST_MUTATION_METHOD (mxArray *, get_field_by_number, (mwIndex, int), nullptr)

  // Not allowed.
  VOID_MUTATION_METHOD (set_field_by_number, (mwIndex, int, mxArray *))

  int get_number_of_fields (void) const { return val.nfields (); }

  CONST_MUTATION_METHOD (const char *, get_field_name_by_number, (int), nullptr)

  CONST_MUTATION_METHOD (int, get_field_number, (const char *), 0)

  int get_string (char *buf, mwSize buflen) const
  {
    int retval = 1;

    mwSize nel = get_number_of_elements ();

    if (val.is_string () && nel < buflen)
      {
        charNDArray tmp = val.char_array_value ();

        const char *p = tmp.data ();

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = p[i];

        buf[nel] = 0;

        retval = 0;
      }

    return retval;
  }

  char * array_to_string (void) const
  {
    // FIXME: this is supposed to handle multi-byte character strings.

    char *buf = nullptr;

    if (val.is_string ())
      {
        mwSize nel = get_number_of_elements ();

        buf = static_cast<char *> (mxArray::malloc (nel + 1));

        if (buf)
          {
            charNDArray tmp = val.char_array_value ();

            const char *p = tmp.data ();

            for (mwIndex i = 0; i < nel; i++)
              buf[i] = p[i];

            buf[nel] = '\0';
          }
      }

    return buf;
  }

  mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const
  {
    // Force ndims, dims to be cached.
    get_dimensions ();

    return calc_single_subscript_internal (ndims, dims, nsubs, subs);
  }

  size_t get_element_size (void) const
  {
    // Force id to be cached.
    get_class_id ();

    switch (id)
      {
      case mxCELL_CLASS: return sizeof (mxArray *);
      case mxSTRUCT_CLASS: return sizeof (mxArray *);
      case mxLOGICAL_CLASS: return sizeof (mxLogical);
      case mxCHAR_CLASS: return sizeof (mxChar);
      case mxDOUBLE_CLASS: return get_numeric_element_size (sizeof (mxDouble));
      case mxSINGLE_CLASS: return get_numeric_element_size (sizeof (mxSingle));
      case mxINT8_CLASS: return get_numeric_element_size (sizeof (mxInt8));
      case mxUINT8_CLASS: return get_numeric_element_size (sizeof (mxUint8));
      case mxINT16_CLASS: return get_numeric_element_size (sizeof (mxInt16));
      case mxUINT16_CLASS: return get_numeric_element_size (sizeof (mxUint16));
      case mxINT32_CLASS: return get_numeric_element_size (sizeof (mxInt32));
      case mxUINT32_CLASS: return get_numeric_element_size (sizeof (mxUint32));
      case mxINT64_CLASS: return get_numeric_element_size (sizeof (mxInt64));
      case mxUINT64_CLASS: return get_numeric_element_size (sizeof (mxUint64));
      case mxFUNCTION_CLASS: return 0;
      // FIXME: user-defined objects need their own class ID.
      //        What should they return, size of pointer?
      default: return 0;
      }
  }

  bool mutation_needed (void) const { return mutate_flag; }

  void request_mutation (void) const
  {
    if (mutate_flag)
      panic_impossible ();

    mutate_flag = true;
  }

  mxArray * mutate (void) const { return as_mxArray (); }

  octave_value as_octave_value (void) const { return val; }

protected:

  mxArray_octave_value (const mxArray_octave_value& arg)
    : mxArray_base (arg), val (arg.val), mutate_flag (arg.mutate_flag),
      id (arg.id), class_name (mxArray::strsave (arg.class_name)),
      ndims (arg.ndims),
      dims (ndims > 0
            ? static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize)))
            : nullptr)
  {
    if (dims)
      {
        for (mwIndex i = 0; i < ndims; i++)
          dims[i] = arg.dims[i];
      }
  }

private:

  octave_value val;

  mutable bool mutate_flag;

  // Caching these does not cost much or lead to much duplicated
  // code.  For other things, we just request mutation to a
  // Matlab-style mxArray object.

  mutable mxClassID id;
  mutable char *class_name;
  mutable mwSize ndims;
  mutable mwSize *dims;
};

// The base class for the Matlab-style representation, used to handle
// things that are common to all Matlab-style objects.

class mxArray_matlab : public mxArray_base
{
protected:

  mxArray_matlab (bool interleaved, mxClassID id_arg = mxUNKNOWN_CLASS)
    : mxArray_base (interleaved), class_name (nullptr), id (id_arg), ndims (0),
      dims (nullptr)
  { }

  mxArray_matlab (bool interleaved, mxClassID id_arg, mwSize ndims_arg,
                  const mwSize *dims_arg)
    : mxArray_base (interleaved), class_name (nullptr), id (id_arg),
      ndims (ndims_arg < 2 ? 2 : ndims_arg),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    if (ndims_arg == 0)
      {
        dims[0] = 0;
        dims[1] = 0;
      }
    else if (ndims_arg < 2)
      {
        dims[0] = 1;
        dims[1] = 1;
      }

    for (mwIndex i = 0; i < ndims_arg; i++)
      dims[i] = dims_arg[i];

    for (mwIndex i = ndims - 1; i > 1; i--)
      {
        if (dims[i] == 1)
          ndims--;
        else
          break;
      }
  }

  mxArray_matlab (bool interleaved, mxClassID id_arg, const dim_vector& dv)
    : mxArray_base (interleaved), class_name (nullptr), id (id_arg),
      ndims (dv.ndims ()),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    for (mwIndex i = 0; i < ndims; i++)
      dims[i] = dv(i);

    for (mwIndex i = ndims - 1; i > 1; i--)
      {
        if (dims[i] == 1)
          ndims--;
        else
          break;
      }
  }

  mxArray_matlab (bool interleaved, mxClassID id_arg, mwSize m, mwSize n)
    : mxArray_base (interleaved), class_name (nullptr), id (id_arg), ndims (2),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    dims[0] = m;
    dims[1] = n;
  }

  mxArray_matlab (const mxArray_matlab& val)
    : mxArray_base (val), class_name (mxArray::strsave (val.class_name)),
      id (val.id), ndims (val.ndims),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    for (mwIndex i = 0; i < ndims; i++)
      dims[i] = val.dims[i];
  }

public:

  // No assignment!
  // FIXME: should this be implemented?
  //        Note that we *do* have a copy constructor.

  mxArray_matlab& operator = (const mxArray_matlab&);

  ~mxArray_matlab (void)
  {
    mxFree (class_name);
    mxFree (dims);
  }

  int iscell (void) const { return id == mxCELL_CLASS; }

  int is_char (void) const { return id == mxCHAR_CLASS; }

  int is_complex (void) const { return 0; }

  int is_double (void) const { return id == mxDOUBLE_CLASS; }

  int is_function_handle (void) const { return id == mxFUNCTION_CLASS; }

  int is_int16 (void) const { return id == mxINT16_CLASS; }

  int is_int32 (void) const { return id == mxINT32_CLASS; }

  int is_int64 (void) const { return id == mxINT64_CLASS; }

  int is_int8 (void) const { return id == mxINT8_CLASS; }

  int is_logical (void) const { return id == mxLOGICAL_CLASS; }

  int is_numeric (void) const
  {
    return (id == mxDOUBLE_CLASS || id == mxSINGLE_CLASS
            || id == mxINT8_CLASS || id == mxUINT8_CLASS
            || id == mxINT16_CLASS || id == mxUINT16_CLASS
            || id == mxINT32_CLASS || id == mxUINT32_CLASS
            || id == mxINT64_CLASS || id == mxUINT64_CLASS);
  }

  int is_single (void) const { return id == mxSINGLE_CLASS; }

  int is_sparse (void) const { return 0; }

  int is_struct (void) const { return id == mxSTRUCT_CLASS; }

  int is_uint16 (void) const { return id == mxUINT16_CLASS; }

  int is_uint32 (void) const { return id == mxUINT32_CLASS; }

  int is_uint64 (void) const { return id == mxUINT64_CLASS; }

  int is_uint8 (void) const { return id == mxUINT8_CLASS; }

  int is_logical_scalar_true (void) const
  {
    return (is_logical_scalar ()
            && static_cast<mxLogical *> (get_data ())[0] != 0);
  }

  mwSize get_m (void) const { return dims[0]; }

  mwSize get_n (void) const
  {
    mwSize n = 1;

    for (mwSize i = ndims - 1 ; i > 0 ; i--)
      n *= dims[i];

    return n;
  }

  mwSize * get_dimensions (void) const { return dims; }

  mwSize get_number_of_dimensions (void) const { return ndims; }

  void set_m (mwSize m) { dims[0] = m; }

  void set_n (mwSize n) { dims[1] = n; }

  int set_dimensions (mwSize *dims_arg, mwSize ndims_arg)
  {
    ndims = ndims_arg;

    mxFree (dims);

    if (ndims > 0)
      {
        dims
          = static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize)));

        if (dims == nullptr)
          return 1;

        for (int i = 0; i < ndims; i++)
          dims[i] = dims_arg[i];

        return 0;
      }
    else
      {
        dims = nullptr;
        return 0;
      }
  }

  mwSize get_number_of_elements (void) const
  {
    mwSize retval = dims[0];

    for (mwIndex i = 1; i < ndims; i++)
      retval *= dims[i];

    return retval;
  }

  int isempty (void) const { return get_number_of_elements () == 0; }

  bool is_scalar (void) const
  {
    return ndims == 2 && dims[0] == 1 && dims[1] == 1;
  }

  mxClassID get_class_id (void) const { return id; }

  const char * get_class_name (void) const
  {
    switch (id)
      {
      case mxDOUBLE_CLASS: return "double";
      case mxSINGLE_CLASS: return "single";
      case mxCHAR_CLASS: return "char";
      case mxLOGICAL_CLASS: return "logical";
      case mxCELL_CLASS: return "cell";
      case mxSTRUCT_CLASS: return "struct";
      case mxFUNCTION_CLASS: return "function_handle";
      case mxINT8_CLASS: return "int8";
      case mxUINT8_CLASS: return "uint8";
      case mxINT16_CLASS: return "int16";
      case mxUINT16_CLASS: return "uint16";
      case mxINT32_CLASS: return "int32";
      case mxUINT32_CLASS: return "uint32";
      case mxINT64_CLASS: return "int64";
      case mxUINT64_CLASS: return "uint64";
      case mxUNKNOWN_CLASS: return "unknown";
      // FIXME: should return the classname of user-defined objects
      default: return "unknown";
      }
  }

  void set_class_name (const char *name_arg)
  {
    mxFree (class_name);
    class_name = static_cast<char *> (mxArray::malloc (strlen (name_arg) + 1));
    strcpy (class_name, name_arg);
  }

  mxArray * get_cell (mwIndex /*idx*/) const
  {
    err_invalid_type ("get_cell");
  }

  void set_cell (mwIndex /*idx*/, mxArray * /*val*/)
  {
    err_invalid_type ("set_cell");
  }

  double get_scalar (void) const
  {
    err_invalid_type ("get_scalar");
  }

  void * get_data (void) const
  {
    err_invalid_type ("get_data");
  }

  mxDouble * get_doubles (void) const
  {
    err_invalid_type ("get_doubles");
  }

  mxSingle * get_singles (void) const
  {
    err_invalid_type ("get_singles");
  }

  mxInt8 * get_int8s (void) const
  {
    err_invalid_type ("get_int8s");
  }

  mxInt16 * get_int16s (void) const
  {
    err_invalid_type ("get_int16s");
  }

  mxInt32 * get_int32s (void) const
  {
    err_invalid_type ("get_int32s");
  }

  mxInt64 * get_int64s (void) const
  {
    err_invalid_type ("get_int64s");
  }

  mxUint8 * get_uint8s (void) const
  {
    err_invalid_type ("get_uint8s");
  }

  mxUint16 * get_uint16s (void) const
  {
    err_invalid_type ("get_uint16s");
  }

  mxUint32 * get_uint32s (void) const
  {
    err_invalid_type ("get_uint32s");
  }

  mxUint64 * get_uint64s (void) const
  {
    err_invalid_type ("get_uint64s");
  }

  mxComplexDouble * get_complex_doubles (void) const
  {
    err_invalid_type ("get_complex_doubles");
  }

  mxComplexSingle * get_complex_singles (void) const
  {
    err_invalid_type ("get_complex_singles");
  }

#if 0
  /* We don't have these yet. */
  mxComplexInt8 * get_complex_int8s (void) const
  {
    err_invalid_type ("get_complex_int8s");
  }

  mxComplexInt16 * get_complex_int16s (void) const
  {
    err_invalid_type ("get_complex_int16s");
  }

  mxComplexInt32 * get_complex_int32s (void) const
  {
    err_invalid_type ("get_complex_int32s");
  }

  mxComplexInt64 * get_complex_int64s (void) const
  {
    err_invalid_type ("get_complex_int64s");
  }

  mxComplexUint8 * get_complex_uint8s (void) const
  {
    err_invalid_type ("get_complex_uint8s");
  }

  mxComplexUint16 * get_complex_uint16s (void) const
  {
    err_invalid_type ("get_complex_uint16s");
  }

  mxComplexUint32 * get_complex_uint32s (void) const
  {
    err_invalid_type ("get_complex_uint32s");
  }

  mxComplexUint64 * get_complex_uint64s (void) const
  {
    err_invalid_type ("get_complex_uint64s");
  }
#endif

  void * get_imag_data (void) const
  {
    err_invalid_type ("get_imag_data");
  }

  void set_data (void * /*pr*/)
  {
    err_invalid_type ("set_data");
  }

  int set_doubles (mxDouble *)
  {
    err_invalid_type ("set_doubles");
  }

  int set_singles (mxSingle *)
  {
    err_invalid_type ("set_singles");
  }

  int set_int8s (mxInt8 *)
  {
    err_invalid_type ("set_int8s");
  }

  int set_int16s (mxInt16 *)
  {
    err_invalid_type ("set_int16s");
  }

  int set_int32s (mxInt32 *)
  {
    err_invalid_type ("set_int32s");
  }

  int set_int64s (mxInt64 *)
  {
    err_invalid_type ("set_int64s");
  }

  int set_uint8s (mxUint8 *)
  {
    err_invalid_type ("set_uint8s");
  }

  int set_uint16s (mxUint16 *)
  {
    err_invalid_type ("set_uint16s");
  }

  int set_uint32s (mxUint32 *)
  {
    err_invalid_type ("set_uint32s");
  }

  int set_uint64s (mxUint64 *)
  {
    err_invalid_type ("set_uint64s");
  }

  int set_complex_doubles (mxComplexDouble *)
  {
    err_invalid_type ("set_complex_doubles");
  }

  int set_complex_singles (mxComplexSingle *)
  {
    err_invalid_type ("set_complex_singles");
  }

#if 0
  /* We don't have these yet. */
  int set_complex_int8s (mxComplexInt8 *)
  {
    err_invalid_type ("set_complex_int8s");
  }

  int set_complex_int16s (mxComplexInt16 *)
  {
    err_invalid_type ("set_complex_int16s");
  }

  int set_complex_int32s (mxComplexInt32 *)
  {
    err_invalid_type ("set_complex_int32s");
  }

  int set_complex_int64s (mxComplexInt64 *)
  {
    err_invalid_type ("set_complex_int64s");
  }

  int set_complex_uint8s (mxComplexUint8 *)
  {
    err_invalid_type ("set_complex_uint8s");
  }

  int set_complex_uint16s (mxComplexUint16 *)
  {
    err_invalid_type ("set_complex_uint16s");
  }

  int set_complex_uint32s (mxComplexUint32 *)
  {
    err_invalid_type ("set_complex_uint32s");
  }

  int set_complex_uint64s (mxComplexUint64 *)
  {
    err_invalid_type ("set_complex_uint64s");
  }
#endif

  void set_imag_data (void * /*pi*/)
  {
    err_invalid_type ("set_imag_data");
  }

  mwIndex * get_ir (void) const
  {
    err_invalid_type ("get_ir");
  }

  mwIndex * get_jc (void) const
  {
    err_invalid_type ("get_jc");
  }

  mwSize get_nzmax (void) const
  {
    err_invalid_type ("get_nzmax");
  }

  void set_ir (mwIndex * /*ir*/)
  {
    err_invalid_type ("set_ir");
  }

  void set_jc (mwIndex * /*jc*/)
  {
    err_invalid_type ("set_jc");
  }

  void set_nzmax (mwSize /*nzmax*/)
  {
    err_invalid_type ("set_nzmax");
  }

  int add_field (const char * /*key*/)
  {
    err_invalid_type ("add_field");
  }

  void remove_field (int /*key_num*/)
  {
    err_invalid_type ("remove_field");
  }

  mxArray * get_field_by_number (mwIndex /*index*/, int /*key_num*/) const
  {
    err_invalid_type ("get_field_by_number");
  }

  void set_field_by_number (mwIndex /*index*/, int /*key_num*/,
                            mxArray * /*val*/)
  {
    err_invalid_type ("set_field_by_number");
  }

  int get_number_of_fields (void) const
  {
    err_invalid_type ("get_number_of_fields");
  }

  const char * get_field_name_by_number (int /*key_num*/) const
  {
    err_invalid_type ("get_field_name_by_number");
  }

  int get_field_number (const char * /*key*/) const
  {
    return -1;
  }

  int get_string (char * /*buf*/, mwSize /*buflen*/) const
  {
    err_invalid_type ("get_string");
  }

  char * array_to_string (void) const
  {
    err_invalid_type ("array_to_string");
  }

  mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const
  {
    return calc_single_subscript_internal (ndims, dims, nsubs, subs);
  }

  size_t get_element_size (void) const
  {
    switch (id)
      {
      case mxCELL_CLASS: return sizeof (mxArray *);
      case mxSTRUCT_CLASS: return sizeof (mxArray *);
      case mxLOGICAL_CLASS: return sizeof (mxLogical);
      case mxCHAR_CLASS: return sizeof (mxChar);
      case mxDOUBLE_CLASS: return get_numeric_element_size (sizeof (mxDouble));
      case mxSINGLE_CLASS: return get_numeric_element_size (sizeof (mxSingle));
      case mxINT8_CLASS: return get_numeric_element_size (sizeof (mxInt8));
      case mxUINT8_CLASS: return get_numeric_element_size (sizeof (mxUint8));
      case mxINT16_CLASS: return get_numeric_element_size (sizeof (mxInt16));
      case mxUINT16_CLASS: return get_numeric_element_size (sizeof (mxUint16));
      case mxINT32_CLASS: return get_numeric_element_size (sizeof (mxInt32));
      case mxUINT32_CLASS: return get_numeric_element_size (sizeof (mxUint32));
      case mxINT64_CLASS: return get_numeric_element_size (sizeof (mxInt64));
      case mxUINT64_CLASS: return get_numeric_element_size (sizeof (mxUint64));
      case mxFUNCTION_CLASS: return 0;
      // FIXME: user-defined objects need their own class ID.
      //        What should they return, size of pointer?
      default: return 0;
      }
  }

protected:

  dim_vector
  dims_to_dim_vector (void) const
  {
    mwSize nd = get_number_of_dimensions ();

    mwSize *d = get_dimensions ();

    dim_vector dv;
    dv.resize (nd);

    for (mwIndex i = 0; i < nd; i++)
      dv(i) = d[i];

    return dv;
  }

private:

  char *class_name;

  mxClassID id;

  mwSize ndims;
  mwSize *dims;
};


// Matlab-style numeric, character, and logical data.

#define TYPED_GET_METHOD(TYPE, FCN_NAME)        \
  TYPE FCN_NAME (void) const                    \
  {                                             \
    if (! m_interleaved)                        \
      panic_impossible ();                      \
                                                \
    return static_cast<TYPE> (pr);              \
  }

#define TYPED_SET_METHOD(TYPE, FCN_NAME)        \
  int FCN_NAME (TYPE d)                         \
  {                                             \
    if (! m_interleaved)                        \
      panic_impossible ();                      \
                                                \
    pr = d;                                     \
    return 0;                                   \
  }

class mxArray_number : public mxArray_matlab
{
public:

  mxArray_number (bool interleaved, mxClassID id_arg, mwSize ndims_arg,
                  const mwSize *dims_arg, mxComplexity flag = mxREAL,
                  bool init = true)
    : mxArray_matlab (interleaved, id_arg, ndims_arg, dims_arg),
      m_complex (flag == mxCOMPLEX),
      pr (init
          ? mxArray::calloc (get_number_of_elements (), get_element_size ())
          : mxArray::malloc (get_number_of_elements () * get_element_size ())),
      pi (m_interleaved
          ? nullptr
          : (m_complex
             ? (init
                ? mxArray::calloc (get_number_of_elements (), get_element_size ())
                : mxArray::malloc (get_number_of_elements () * get_element_size ()))
             : nullptr))
  { }

  mxArray_number (bool interleaved, mxClassID id_arg, const dim_vector& dv,
                  mxComplexity flag = mxREAL)
    : mxArray_matlab (interleaved, id_arg, dv), m_complex (flag == mxCOMPLEX),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (m_interleaved
          ? nullptr
          : (m_complex
             ? mxArray::calloc (get_number_of_elements (), get_element_size ())
             : nullptr))
  { }

  mxArray_number (bool interleaved, mxClassID id_arg, mwSize m, mwSize n,
                  mxComplexity flag = mxREAL, bool init = true)
    : mxArray_matlab (interleaved, id_arg, m, n), m_complex (flag == mxCOMPLEX),
      pr (init
          ? mxArray::calloc (get_number_of_elements (), get_element_size ())
          : mxArray::malloc (get_number_of_elements () * get_element_size ())),
      pi (m_interleaved
          ? nullptr
          : (m_complex
             ? (init
                ? mxArray::calloc (get_number_of_elements (), get_element_size ())
                : mxArray::malloc (get_number_of_elements () * get_element_size ()))
             : nullptr))
  { }

  mxArray_number (bool interleaved, mxClassID id_arg, double val)
    : mxArray_matlab (interleaved, id_arg, 1, 1), m_complex (false),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (nullptr)
  {
    double *dpr = static_cast<double *> (pr);
    dpr[0] = val;
  }

  mxArray_number (bool interleaved, mxClassID id_arg, mxLogical val)
    : mxArray_matlab (interleaved, id_arg, 1, 1), m_complex (false),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (nullptr)
  {
    mxLogical *lpr = static_cast<mxLogical *> (pr);
    lpr[0] = val;
  }

  mxArray_number (bool interleaved, const char *str)
    : mxArray_matlab (interleaved, mxCHAR_CLASS,
                      str ? (strlen (str) ? 1 : 0) : 0,
                      str ? strlen (str) : 0),
      m_complex (false),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (nullptr)
  {
    mxChar *cpr = static_cast<mxChar *> (pr);
    mwSize nel = get_number_of_elements ();
    for (mwIndex i = 0; i < nel; i++)
      cpr[i] = str[i];
  }

  // FIXME: ???
  mxArray_number (bool interleaved, mwSize m, const char **str)
    : mxArray_matlab (interleaved, mxCHAR_CLASS, m, max_str_len (m, str)),
      m_complex (false),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (nullptr)
  {
    mxChar *cpr = static_cast<mxChar *> (pr);

    mwSize *dv = get_dimensions ();

    mwSize nc = dv[1];

    for (mwIndex j = 0; j < m; j++)
      {
        const char *ptr = str[j];

        size_t tmp_len = strlen (ptr);

        for (size_t i = 0; i < tmp_len; i++)
          cpr[m*i+j] = static_cast<mxChar> (ptr[i]);

        for (size_t i = tmp_len; i < static_cast<size_t> (nc); i++)
          cpr[m*i+j] = static_cast<mxChar> (' ');
      }
  }

protected:

  mxArray_number (const mxArray_number& val)
    : mxArray_matlab (val), m_complex (val.m_complex),
      pr (mxArray::malloc (get_number_of_elements () * get_element_size ())),
      pi (m_interleaved
          ? nullptr
          : (val.pi
             ? mxArray::malloc (get_number_of_elements () * get_element_size ())
             : nullptr))
  {
    size_t nbytes = get_number_of_elements () * get_element_size ();

    if (pr)
      memcpy (pr, val.pr, nbytes);

    if (pi)
      memcpy (pi, val.pi, nbytes);
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_number& operator = (const mxArray_number&);

  mxArray_base * dup (void) const
  {
    return new mxArray_number (*this);
  }

  ~mxArray_number (void)
  {
    mxFree (pr);
    mxFree (pi);
  }

  int is_complex (void) const
  {
    return m_interleaved ? m_complex : (pi != nullptr);
  }

  double get_scalar (void) const
  {
    // FIXME: how does this work for interleaved complex arrays?

    double retval = 0;

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        retval = *(static_cast<double *> (pr));
        break;

      case mxSINGLE_CLASS:
        retval = *(static_cast<float *> (pr));
        break;

      case mxCHAR_CLASS:
        retval = *(static_cast<mxChar *> (pr));
        break;

      case mxLOGICAL_CLASS:
        retval = *(static_cast<bool *> (pr));
        break;

      case mxINT8_CLASS:
        retval = *(static_cast<int8_t *> (pr));
        break;

      case mxUINT8_CLASS:
        retval = *(static_cast<uint8_t *> (pr));
        break;

      case mxINT16_CLASS:
        retval = *(static_cast<int16_t *> (pr));
        break;

      case mxUINT16_CLASS:
        retval = *(static_cast<uint16_t *> (pr));
        break;

      case mxINT32_CLASS:
        retval = *(static_cast<int32_t *> (pr));
        break;

      case mxUINT32_CLASS:
        retval = *(static_cast<uint32_t *> (pr));
        break;

      case mxINT64_CLASS:
        retval = *(static_cast<int64_t *> (pr));
        break;

      case mxUINT64_CLASS:
        retval = *(static_cast<uint64_t *> (pr));
        break;

      default:
        panic_impossible ();
      }

    return retval;
  }

  void * get_data (void) const { return pr; }

  void * get_imag_data (void) const
  {
    if (m_interleaved)
      panic_impossible ();

    return pi;
  }

  void set_data (void *pr_arg) { pr = pr_arg; }

  void set_imag_data (void *pi_arg)
  {
    if (m_interleaved)
      panic_impossible ();

    pi = pi_arg;
  }

  TYPED_GET_METHOD (mxDouble *, get_doubles)
  TYPED_GET_METHOD (mxSingle *, get_singles)
  TYPED_GET_METHOD (mxInt8 *, get_int8s)
  TYPED_GET_METHOD (mxInt16 *, get_int16s)
  TYPED_GET_METHOD (mxInt32 *, get_int32s)
  TYPED_GET_METHOD (mxInt64 *, get_int64s)
  TYPED_GET_METHOD (mxUint8 *, get_uint8s)
  TYPED_GET_METHOD (mxUint16 *, get_uint16s)
  TYPED_GET_METHOD (mxUint32 *, get_uint32s)
  TYPED_GET_METHOD (mxUint64 *, get_uint64s)

  TYPED_GET_METHOD (mxComplexDouble *, get_complex_doubles)
  TYPED_GET_METHOD (mxComplexSingle *, get_complex_singles)
#if 0
  /* We don't have these yet. */
  TYPED_GET_METHOD (mxComplexInt8 *, get_complex_int8s)
  TYPED_GET_METHOD (mxComplexInt16 *, get_complex_int16s)
  TYPED_GET_METHOD (mxComplexInt32 *, get_complex_int32s)
  TYPED_GET_METHOD (mxComplexInt64 *, get_complex_int64s)
  TYPED_GET_METHOD (mxComplexUint8 *, get_complex_uint8s)
  TYPED_GET_METHOD (mxComplexUint16 *, get_complex_uint16s)
  TYPED_GET_METHOD (mxComplexUint32 *, get_complex_uint32s)
  TYPED_GET_METHOD (mxComplexUint64 *, get_complex_uint64s)
#endif

  TYPED_SET_METHOD (mxDouble *, set_doubles)
  TYPED_SET_METHOD (mxSingle *, set_singles)
  TYPED_SET_METHOD (mxInt8 *, set_int8s)
  TYPED_SET_METHOD (mxInt16 *, set_int16s)
  TYPED_SET_METHOD (mxInt32 *, set_int32s)
  TYPED_SET_METHOD (mxInt64 *, set_int64s)
  TYPED_SET_METHOD (mxUint8 *, set_uint8s)
  TYPED_SET_METHOD (mxUint16 *, set_uint16s)
  TYPED_SET_METHOD (mxUint32 *, set_uint32s)
  TYPED_SET_METHOD (mxUint64 *, set_uint64s)

  TYPED_SET_METHOD (mxComplexDouble *, set_complex_doubles)
  TYPED_SET_METHOD (mxComplexSingle *, set_complex_singles)
#if 0
  /* We don't have these yet. */
  TYPED_SET_METHOD (mxComplexInt8 *, set_complex_int8s)
  TYPED_SET_METHOD (mxComplexInt16 *, set_complex_int16s)
  TYPED_SET_METHOD (mxComplexInt32 *, set_complex_int32s)
  TYPED_SET_METHOD (mxComplexInt64 *, set_complex_int64s)
  TYPED_SET_METHOD (mxComplexUint8 *, set_complex_uint8s)
  TYPED_SET_METHOD (mxComplexUint16 *, set_complex_uint16s)
  TYPED_SET_METHOD (mxComplexUint32 *, set_complex_uint32s)
  TYPED_SET_METHOD (mxComplexUint64 *, set_complex_uint64s)
#endif

  int get_string (char *buf, mwSize buflen) const
  {
    int retval = 0;

    mwSize nel = get_number_of_elements ();

    if (! (nel < buflen))
      {
        retval = 1;
        if (buflen > 0)
          nel = buflen-1;
      }

    if (nel < buflen)
      {
        mxChar *ptr = static_cast<mxChar *> (pr);

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = static_cast<char> (ptr[i]);

        buf[nel] = 0;
      }

    return retval;
  }

  char * array_to_string (void) const
  {
    // FIXME: this is supposed to handle multi-byte character strings.

    mwSize nel = get_number_of_elements ();

    char *buf = static_cast<char *> (mxArray::malloc (nel + 1));

    if (buf)
      {
        mxChar *ptr = static_cast<mxChar *> (pr);

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = static_cast<char> (ptr[i]);

        buf[nel] = '\0';
      }

    return buf;
  }

  octave_value as_octave_value (void) const
  {
    octave_value retval;

    dim_vector dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        {
          mwSize nel = get_number_of_elements ();

          if (is_complex ())
            {
              if (m_interleaved)
                {
                  Complex *ppr = static_cast<Complex *> (pr);

                  ComplexNDArray val (dv);
                  Complex *ptr = val.fortran_vec ();

                  for (mwIndex i = 0; i < nel; i++)
                    ptr[i] = ppr[i];

                  retval = val;
                }
              else
                {
                  double *ppr = static_cast<double *> (pr);

                  ComplexNDArray val (dv);

                  Complex *ptr = val.fortran_vec ();

                  double *ppi = static_cast<double *> (pi);

                  for (mwIndex i = 0; i < nel; i++)
                    ptr[i] = Complex (ppr[i], ppi[i]);

                  retval = val;
                }
            }
          else
            {
              double *ppr = static_cast<double *> (pr);

              NDArray val (dv);

              double *ptr = val.fortran_vec ();

              for (mwIndex i = 0; i < nel; i++)
                ptr[i] = ppr[i];

              retval = val;
            }
        }
        break;

      case mxSINGLE_CLASS:
        {
          mwSize nel = get_number_of_elements ();

          if (is_complex ())
            {
              if (m_interleaved)
                {
                  FloatComplex *ppr = static_cast<FloatComplex *> (pr);

                  FloatComplexNDArray val (dv);
                  FloatComplex *ptr = val.fortran_vec ();

                  for (mwIndex i = 0; i < nel; i++)
                    ptr[i] = ppr[i];

                  retval = val;
                }
              else
                {
                  float *ppr = static_cast<float *> (pr);

                  FloatComplexNDArray val (dv);

                  FloatComplex *ptr = val.fortran_vec ();

                  float *ppi = static_cast<float *> (pi);

                  for (mwIndex i = 0; i < nel; i++)
                    ptr[i] = FloatComplex (ppr[i], ppi[i]);

                  retval = val;
                }
            }
          else
            {
              float *ppr = static_cast<float *> (pr);

              FloatNDArray val (dv);

              float *ptr = val.fortran_vec ();

              for (mwIndex i = 0; i < nel; i++)
                ptr[i] = ppr[i];

              retval = val;
            }
        }
        break;

      case mxCHAR_CLASS:
        {
          mwSize nel = get_number_of_elements ();

          mxChar *ppr = static_cast<mxChar *> (pr);

          charNDArray val (dv);

          char *ptr = val.fortran_vec ();

          for (mwIndex i = 0; i < nel; i++)
            ptr[i] = static_cast<char> (ppr[i]);

          retval = val;
        }
        break;

      case mxLOGICAL_CLASS:
        retval = int_to_ov<mxLogical, boolNDArray, bool> (dv);
        break;

      case mxINT8_CLASS:
        retval = int_to_ov<int8_t, int8NDArray, octave_int8> (dv);
        break;

      case mxUINT8_CLASS:
        retval = int_to_ov<uint8_t, uint8NDArray, octave_uint8> (dv);
        break;

      case mxINT16_CLASS:
        retval = int_to_ov<int16_t, int16NDArray, octave_int16> (dv);
        break;

      case mxUINT16_CLASS:
        retval = int_to_ov<uint16_t, uint16NDArray, octave_uint16> (dv);
        break;

      case mxINT32_CLASS:
        retval = int_to_ov<int32_t, int32NDArray, octave_int32> (dv);
        break;

      case mxUINT32_CLASS:
        retval = int_to_ov<uint32_t, uint32NDArray, octave_uint32> (dv);
        break;

      case mxINT64_CLASS:
        retval = int_to_ov<int64_t, int64NDArray, octave_int64> (dv);
        break;

      case mxUINT64_CLASS:
        retval = int_to_ov<uint64_t, uint64NDArray, octave_uint64> (dv);
        break;

      default:
        panic_impossible ();
      }

    return retval;
  }

protected:

  template <typename ELT_T, typename ARRAY_T, typename ARRAY_ELT_T>
  octave_value
  int_to_ov (const dim_vector& dv) const
  {
    if (is_complex ())
      error ("complex integer types are not supported");

    mwSize nel = get_number_of_elements ();

    ELT_T *ppr = static_cast<ELT_T *> (pr);

    ARRAY_T val (dv);

    ARRAY_ELT_T *ptr = val.fortran_vec ();

    for (mwIndex i = 0; i < nel; i++)
      ptr[i] = ppr[i];

    return octave_value (val);
  }

private:

  // Flag to identify complex object if using interleaved data and PI is
  // always nullptr.
  bool m_complex;

  // If using interleaved complex storage, this is the pointer to data
  // (real, complex, or logical).  Otherwise, it is the pointer to the
  // real part of the data.
  void *pr;

  // If using non-interleaved complex storage, this is the pointer to
  // the imaginary part of the data.  Othrwise is is always nullptr.
  void *pi;
};

// Matlab-style sparse arrays.

class mxArray_sparse : public mxArray_matlab
{
public:

  mxArray_sparse (bool interleaved, mxClassID id_arg, mwSize m, mwSize n,
                  mwSize nzmax_arg, mxComplexity flag = mxREAL)
    : mxArray_matlab (interleaved, id_arg, m, n), m_complex (flag == mxCOMPLEX),

      nzmax (nzmax_arg > 0 ? nzmax_arg : 1),
      pr (mxArray::calloc (nzmax, get_element_size ())),
      pi (m_interleaved
          ? nullptr
          : (m_complex
             ? mxArray::calloc (nzmax, get_element_size ())
             : nullptr)),
      ir (static_cast<mwIndex *> (mxArray::calloc (nzmax, sizeof (mwIndex)))),
      jc (static_cast<mwIndex *> (mxArray::calloc (n + 1, sizeof (mwIndex))))
  { }

private:

  mxArray_sparse (const mxArray_sparse& val)
    : mxArray_matlab (val), nzmax (val.nzmax),
      pr (mxArray::malloc (nzmax * get_element_size ())),
      pi (m_interleaved
          ? nullptr
          : (val.pi
             ? mxArray::malloc (nzmax * get_element_size ())
             : nullptr)),
      ir (static_cast<mwIndex *> (mxArray::malloc (nzmax * sizeof (mwIndex)))),
      jc (static_cast<mwIndex *> (mxArray::malloc (nzmax * sizeof (mwIndex))))
  {
    size_t nbytes = nzmax * get_element_size ();

    if (pr)
      memcpy (pr, val.pr, nbytes);

    if (pi)
      memcpy (pi, val.pi, nbytes);

    if (ir)
      memcpy (ir, val.ir, nzmax * sizeof (mwIndex));

    if (jc)
      memcpy (jc, val.jc, (val.get_n () + 1) * sizeof (mwIndex));
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_sparse& operator = (const mxArray_sparse&);

  mxArray_base * dup (void) const
  {
    return new mxArray_sparse (*this);
  }

  ~mxArray_sparse (void)
  {
    mxFree (pr);
    mxFree (pi);
    mxFree (ir);
    mxFree (jc);
  }

  int is_complex (void) const
  {
    return m_interleaved ? m_complex : (pi != nullptr);
  }

  int is_sparse (void) const { return 1; }

  void * get_data (void) const { return pr; }

  void * get_imag_data (void) const
  {
    if (m_interleaved)
      panic_impossible ();

    return pi;
  }

  void set_data (void *pr_arg) { pr = pr_arg; }

  void set_imag_data (void *pi_arg)
  {
    if (m_interleaved)
      panic_impossible ();

    pi = pi_arg;
  }

  TYPED_GET_METHOD (mxDouble *, get_doubles)
  TYPED_GET_METHOD (mxComplexDouble *, get_complex_doubles)

  TYPED_SET_METHOD (mxDouble *, set_doubles)
  TYPED_SET_METHOD (mxComplexDouble *, set_complex_doubles)

  mwIndex * get_ir (void) const { return ir; }

  mwIndex * get_jc (void) const { return jc; }

  mwSize get_nzmax (void) const { return nzmax; }

  void set_ir (mwIndex *ir_arg) { ir = ir_arg; }

  void set_jc (mwIndex *jc_arg) { jc = jc_arg; }

  void set_nzmax (mwSize nzmax_arg)
  {
    /* Require storage for at least 1 element */
    nzmax = (nzmax_arg > 0 ? nzmax_arg : 1);
  }

  octave_value as_octave_value (void) const
  {
    octave_value retval;

    dim_vector dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        {
          if (is_complex ())
            {
              if (m_interleaved)
                {
                  Complex *ppr = static_cast<Complex *> (pr);

                  SparseComplexMatrix val (get_m (), get_n (),
                                           static_cast<octave_idx_type> (nzmax));

                  for (mwIndex i = 0; i < nzmax; i++)
                    {
                      val.xdata (i) = ppr[i];
                      val.xridx (i) = ir[i];
                    }

                  for (mwIndex i = 0; i < get_n () + 1; i++)
                    val.xcidx (i) = jc[i];

                  retval = val;
                }
              else
                {
                  double *ppr = static_cast<double *> (pr);
                  double *ppi = static_cast<double *> (pi);

                  SparseComplexMatrix val (get_m (), get_n (),
                                           static_cast<octave_idx_type> (nzmax));

                  for (mwIndex i = 0; i < nzmax; i++)
                    {
                      val.xdata (i) = Complex (ppr[i], ppi[i]);
                      val.xridx (i) = ir[i];
                    }

                  for (mwIndex i = 0; i < get_n () + 1; i++)
                    val.xcidx (i) = jc[i];

                  retval = val;
                }
            }
          else
            {
              double *ppr = static_cast<double *> (pr);

              SparseMatrix val (get_m (), get_n (),
                                static_cast<octave_idx_type> (nzmax));

              for (mwIndex i = 0; i < nzmax; i++)
                {
                  val.xdata (i) = ppr[i];
                  val.xridx (i) = ir[i];
                }

              for (mwIndex i = 0; i < get_n () + 1; i++)
                val.xcidx (i) = jc[i];

              retval = val;
            }
        }
        break;

      case mxLOGICAL_CLASS:
        {
          bool *ppr = static_cast<bool *> (pr);

          SparseBoolMatrix val (get_m (), get_n (),
                                static_cast<octave_idx_type> (nzmax));

          for (mwIndex i = 0; i < nzmax; i++)
            {
              val.xdata (i) = ppr[i];
              val.xridx (i) = ir[i];
            }

          for (mwIndex i = 0; i < get_n () + 1; i++)
            val.xcidx (i) = jc[i];

          retval = val;
        }
        break;

      case mxSINGLE_CLASS:
        error ("single precision sparse data type not supported");
        break;

      default:
        panic_impossible ();
      }

    return retval;
  }

private:

  // Flag to identify complex object if using interleaved data and PI is
  // always nullptr.
  bool m_complex;

  // Maximun number of nonzero elements.
  mwSize nzmax;

  // If using interleaved complex storage, this is the pointer to data
  // (real, complex, or logical).  Otherwise, it is the pointer to the
  // real part of the data.
  void *pr;

  // If using non-interleaved complex storage, this is the pointer to
  // the imaginary part of the data.  Othrwise is is always nullptr.
  void *pi;

  // Sparse storage indexing arrays.
  mwIndex *ir;
  mwIndex *jc;
};

// Matlab-style struct arrays.

class mxArray_struct : public mxArray_matlab
{
public:

  mxArray_struct (bool interleaved, mwSize ndims_arg, const mwSize *dims_arg,
                  int num_keys_arg, const char **keys)
    : mxArray_matlab (interleaved, mxSTRUCT_CLASS, ndims_arg, dims_arg),
      nfields (num_keys_arg),
      fields (static_cast<char **> (mxArray::calloc (nfields,
                                                     sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::calloc (nfields *
                                                      get_number_of_elements (),
                                                      sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (bool interleaved, const dim_vector& dv, int num_keys_arg,
                  const char **keys)
    : mxArray_matlab (interleaved, mxSTRUCT_CLASS, dv), nfields (num_keys_arg),
      fields (static_cast<char **> (mxArray::calloc (nfields,
                                                     sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::calloc (nfields *
                                                      get_number_of_elements (),
                                                      sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (bool interleaved, mwSize m, mwSize n, int num_keys_arg,
                  const char **keys)
    : mxArray_matlab (interleaved, mxSTRUCT_CLASS, m, n),
      nfields (num_keys_arg),
      fields (static_cast<char **> (mxArray::calloc (nfields,
                                                     sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::calloc (nfields *
                                                      get_number_of_elements (),
                                                      sizeof (mxArray *))))
  {
    init (keys);
  }

private:

  mxArray_struct (const mxArray_struct& val)
    : mxArray_matlab (val), nfields (val.nfields),
      fields (static_cast<char **> (mxArray::malloc (nfields
                                                     * sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::malloc (nfields *
                                                      get_number_of_elements ()
                                                      * sizeof (mxArray *))))
  {
    for (int i = 0; i < nfields; i++)
      fields[i] = mxArray::strsave (val.fields[i]);

    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel * nfields; i++)
      {
        mxArray *ptr = val.data[i];
        data[i] = (ptr ? ptr->dup () : nullptr);
      }
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_struct& operator = (const mxArray_struct& val);

  void init (const char **keys)
  {
    for (int i = 0; i < nfields; i++)
      fields[i] = mxArray::strsave (keys[i]);
  }

  mxArray_base * dup (void) const { return new mxArray_struct (*this); }

  ~mxArray_struct (void)
  {
    for (int i = 0; i < nfields; i++)
      mxFree (fields[i]);

    mxFree (fields);

    mwSize ntot = nfields * get_number_of_elements ();

    for  (mwIndex i = 0; i < ntot; i++)
      delete data[i];

    mxFree (data);
  }

  int add_field (const char *key)
  {
    int retval = -1;

    if (valid_key (key))
      {
        nfields++;

        fields = static_cast<char **>
                  (mxRealloc (fields, nfields * sizeof (char *)));

        if (fields)
          {
            fields[nfields-1] = mxArray::strsave (key);

            mwSize nel = get_number_of_elements ();

            mwSize ntot = nfields * nel;

            mxArray **new_data;
            new_data = static_cast<mxArray **>
                        (mxArray::malloc (ntot * sizeof (mxArray *)));

            if (new_data)
              {
                mwIndex j = 0;
                mwIndex k = 0;
                mwIndex n = 0;

                for (mwIndex i = 0; i < ntot; i++)
                  {
                    if (++n == nfields)
                      {
                        new_data[j++] = nullptr;
                        n = 0;
                      }
                    else
                      new_data[j++] = data[k++];
                  }

                mxFree (data);

                data = new_data;

                retval = nfields - 1;
              }
          }
      }

    return retval;
  }

  void remove_field (int key_num)
  {
    if (key_num >= 0 && key_num < nfields)
      {
        mwSize nel = get_number_of_elements ();

        mwSize ntot = nfields * nel;

        int new_nfields = nfields - 1;

        char **new_fields = static_cast<char **>
                             (mxArray::malloc (new_nfields * sizeof (char *)));

        mxArray **new_data = static_cast<mxArray **>
                              (mxArray::malloc (new_nfields * nel
                                                * sizeof (mxArray *)));

        for (int i = 0; i < key_num; i++)
          new_fields[i] = fields[i];

        for (int i = key_num + 1; i < nfields; i++)
          new_fields[i-1] = fields[i];

        if (new_nfields > 0)
          {
            mwIndex j = 0;
            mwIndex k = 0;
            mwIndex n = 0;

            for (mwIndex i = 0; i < ntot; i++)
              {
                if (n == key_num)
                  k++;
                else
                  new_data[j++] = data[k++];

                if (++n == nfields)
                  n = 0;
              }
          }

        nfields = new_nfields;

        mxFree (fields);
        mxFree (data);

        fields = new_fields;
        data = new_data;
      }
  }

  mxArray * get_field_by_number (mwIndex index, int key_num) const
  {
    return key_num >= 0 && key_num < nfields
           ? data[nfields * index + key_num] : nullptr;
  }

  void set_field_by_number (mwIndex index, int key_num, mxArray *val);

  int get_number_of_fields (void) const { return nfields; }

  const char * get_field_name_by_number (int key_num) const
  {
    return key_num >= 0 && key_num < nfields ? fields[key_num] : nullptr;
  }

  int get_field_number (const char *key) const
  {
    int retval = -1;

    for (int i = 0; i < nfields; i++)
      {
        if (! strcmp (key, fields[i]))
          {
            retval = i;
            break;
          }
      }

    return retval;
  }

  void * get_data (void) const { return data; }

  void set_data (void *data_arg) { data = static_cast<mxArray **> (data_arg); }

  octave_value as_octave_value (void) const
  {
    dim_vector dv = dims_to_dim_vector ();

    string_vector keys (fields, nfields);

    octave_map m (dv);

    mwSize ntot = nfields * get_number_of_elements ();

    for (int i = 0; i < nfields; i++)
      {
        Cell c (dv);

        octave_value *p = c.fortran_vec ();

        mwIndex k = 0;
        for (mwIndex j = i; j < ntot; j += nfields)
          p[k++] = mxArray::as_octave_value (data[j]);

        m.assign (keys[i], c);
      }

    return m;
  }

private:

  int nfields;

  char **fields;

  mxArray **data;
};

// Matlab-style cell arrays.

class mxArray_cell : public mxArray_matlab
{
public:

  mxArray_cell (bool interleaved, mwSize ndims_arg, const mwSize *dims_arg)
    : mxArray_matlab (interleaved, mxCELL_CLASS, ndims_arg, dims_arg),
      data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (),
                                     sizeof (mxArray *)))) { }

  mxArray_cell (bool interleaved, const dim_vector& dv)
    : mxArray_matlab (interleaved, mxCELL_CLASS, dv),
      data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (),
                                     sizeof (mxArray *)))) { }

  mxArray_cell (bool interleaved, mwSize m, mwSize n)
    : mxArray_matlab (interleaved, mxCELL_CLASS, m, n),
      data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (),
                                     sizeof (mxArray *)))) { }

private:

  mxArray_cell (const mxArray_cell& val)
    : mxArray_matlab (val),
      data (static_cast<mxArray **> (mxArray::malloc (get_number_of_elements ()
                                                      * sizeof (mxArray *))))
  {
    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      {
        mxArray *ptr = val.data[i];
        data[i] = (ptr ? ptr->dup () : nullptr);
      }
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_cell& operator = (const mxArray_cell&);

  mxArray_base * dup (void) const { return new mxArray_cell (*this); }

  ~mxArray_cell (void)
  {
    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      delete data[i];

    mxFree (data);
  }

  mxArray * get_cell (mwIndex idx) const
  {
    return idx >= 0 && idx < get_number_of_elements () ? data[idx] : nullptr;
  }

  void set_cell (mwIndex idx, mxArray *val);

  void * get_data (void) const { return data; }

  void set_data (void *data_arg) { data = static_cast<mxArray **> (data_arg); }

  octave_value as_octave_value (void) const
  {
    dim_vector dv = dims_to_dim_vector ();

    Cell c (dv);

    mwSize nel = get_number_of_elements ();

    octave_value *p = c.fortran_vec ();

    for (mwIndex i = 0; i < nel; i++)
      p[i] = mxArray::as_octave_value (data[i]);

    return c;
  }

private:

  mxArray **data;
};

// ------------------------------------------------------------------

mxArray::mxArray (bool interleaved, const octave_value& ov)
  : rep (create_rep (interleaved, ov)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mwSize ndims,
                  const mwSize *dims, mxComplexity flag, bool init)
  : rep (create_rep (interleaved, id, ndims, dims, flag, init)),
    name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, const dim_vector& dv,
                  mxComplexity flag)
  : rep (create_rep (interleaved, id, dv, flag)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mwSize m, mwSize n,
                  mxComplexity flag, bool init)
  : rep (create_rep (interleaved, id, m, n, flag, init)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, double val)
  : rep (create_rep (interleaved, id, val)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mxLogical val)
  : rep (create_rep (interleaved, id, val)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, const char *str)
  : rep (create_rep (interleaved, str)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize m, const char **str)
  : rep (create_rep (interleaved, m, str)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mwSize m, mwSize n,
                  mwSize nzmax, mxComplexity flag)
  : rep (create_rep (interleaved, id, m, n, nzmax, flag)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize ndims, const mwSize *dims,
                  int num_keys,
                  const char **keys)
  : rep (new mxArray_struct (interleaved, ndims, dims, num_keys, keys)),
    name (nullptr)
{ }

mxArray::mxArray (bool interleaved, const dim_vector& dv, int num_keys,
                  const char **keys)
  : rep (new mxArray_struct (interleaved, dv, num_keys, keys)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize m, mwSize n, int num_keys,
                  const char **keys)
  : rep (new mxArray_struct (interleaved, m, n, num_keys, keys)),
    name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize ndims, const mwSize *dims)
  : rep (new mxArray_cell (interleaved, ndims, dims)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, const dim_vector& dv)
  : rep (new mxArray_cell (interleaved, dv)), name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize m, mwSize n)
  : rep (new mxArray_cell (interleaved, m, n)), name (nullptr)
{ }

mxArray::~mxArray (void)
{
  mxFree (name);

  delete rep;
}

void
mxArray::set_name (const char *name_arg)
{
  mxFree (name);
  name = mxArray::strsave (name_arg);
}

octave_value
mxArray::as_octave_value (const mxArray *ptr, bool null_is_empty)
{
  static const octave_value empty_matrix = Matrix ();

  return (ptr
          ? ptr->as_octave_value ()
          : (null_is_empty ? empty_matrix : octave_value ()));
}

octave_value
mxArray::as_octave_value (void) const
{
  return rep->as_octave_value ();
}

mxArray_base *
mxArray::create_rep (bool interleaved, const octave_value& ov)
{
  return new mxArray_octave_value (interleaved, ov);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mwSize ndims,
                     const mwSize *dims, mxComplexity flag, bool init)
{
  return new mxArray_number (interleaved, id, ndims, dims, flag, init);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, const dim_vector& dv,
                     mxComplexity flag)
{
  return new mxArray_number (interleaved, id, dv, flag);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mwSize m, mwSize n,
                     mxComplexity flag, bool init)
{
  return new mxArray_number (interleaved, id, m, n, flag, init);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, double val)
{
  return new mxArray_number (interleaved, id, val);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mxLogical val)
{
  return new mxArray_number (interleaved, id, val);
}

mxArray_base *
mxArray::create_rep (bool interleaved, const char *str)
{
  return new mxArray_number (interleaved, str);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mwSize m, const char **str)
{
  return new mxArray_number (interleaved, m, str);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mwSize m, mwSize n,
                     mwSize nzmax, mxComplexity flag)
{
  return new mxArray_sparse (interleaved, id, m, n, nzmax, flag);
}

void
mxArray::maybe_mutate (void) const
{
  if (rep->is_octave_value ())
    {
      // The mutate function returns a pointer to a complete new
      // mxArray object (or 0, if no mutation happened).  We just want
      // to replace the existing rep with the rep from the new object.

      mxArray *new_val = rep->mutate ();

      if (new_val)
        {
          delete rep;
          rep = new_val->rep;
          new_val->rep = nullptr;
          delete new_val;
        }
    }
}

// ------------------------------------------------------------------

// A class to manage calls to MEX functions.  Mostly deals with memory
// management.

class mex
{
public:

  mex (octave_mex_function& f)
    : curr_mex_fcn (f), memlist (), arraylist (), fname (nullptr) { }

  // No copying!

  mex (const mex&) = delete;

  mex& operator = (const mex&) = delete;

  ~mex (void)
  {
    // We can't use mex::free here because it modifies memlist.
    while (! memlist.empty ())
      {
        auto p = memlist.begin ();
        xfree (*p);
        memlist.erase (p);
      }

    // We can't use mex::free_value here because it modifies arraylist.
    while (! arraylist.empty ())
      {
        auto p = arraylist.begin ();
        delete *p;
        arraylist.erase (p);
      }

    if (! (memlist.empty () && arraylist.empty ()))
      error ("mex: %s: cleanup failed", function_name ());

    mxFree (fname);
  }

  const char * function_name (void) const
  {
    if (! fname)
      {
        octave::tree_evaluator& tw
          = octave::__get_evaluator__ ("mex::function_name");

        octave_function *fcn = tw.current_function ();

        if (fcn)
          {
            std::string nm = fcn->name ();
            fname = mxArray::strsave (nm.c_str ());
          }
        else
          fname = mxArray::strsave ("unknown");
      }

    return fname;
  }

  // Allocate memory.
  void * malloc_unmarked (size_t n)
  {
    void *ptr = std::malloc (n);

    if (! ptr)
      {
        // FIXME: could use "octave_new_handler();" instead
        error ("%s: failed to allocate %zd bytes of memory",
               function_name (), n);
      }

    global_mark (ptr);

    return ptr;
  }

  // Allocate memory to be freed on exit.
  void * malloc (size_t n)
  {
    void *ptr = malloc_unmarked (n);

    mark (ptr);

    return ptr;
  }

  // Allocate memory and initialize to 0.
  void * calloc_unmarked (size_t n, size_t t)
  {
    void *ptr = malloc_unmarked (n*t);

    memset (ptr, 0, n*t);

    return ptr;
  }

  // Allocate memory to be freed on exit and initialize to 0.
  void * calloc (size_t n, size_t t)
  {
    void *ptr = calloc_unmarked (n, t);

    mark (ptr);

    return ptr;
  }

  // Reallocate a pointer obtained from malloc or calloc.
  // If the pointer is NULL, allocate using malloc.
  // We don't need an "unmarked" version of this.
  void * realloc (void *ptr, size_t n)
  {
    void *v;

    if (ptr)
      {
        auto p_local = memlist.find (ptr);
        auto p_global = global_memlist.find (ptr);

        v = std::realloc (ptr, n);

        if (v)
          {
            if (p_local != memlist.end ())
              {
                memlist.erase (p_local);
                memlist.insert (v);
              }

            if (p_global != global_memlist.end ())
              {
                global_memlist.erase (p_global);
                global_memlist.insert (v);
              }
          }
      }
    else
      v = malloc (n);

    return v;
  }

  // Free a pointer obtained from malloc or calloc.
  void free (void *ptr)
  {
    if (ptr)
      {
        unmark (ptr);

        auto p = global_memlist.find (ptr);

        if (p != global_memlist.end ())
          {
            global_memlist.erase (p);

            xfree (ptr);
          }
        else
          {
            p = foreign_memlist.find (ptr);

            if (p != foreign_memlist.end ())
              foreign_memlist.erase (p);
#if defined (DEBUG)
            else
              warning ("mxFree: skipping memory not allocated by mxMalloc, mxCalloc, or mxRealloc");
#endif
          }
      }
  }

  // Mark a pointer to be freed on exit.
  void mark (void *ptr)
  {
#if defined (DEBUG)
    if (memlist.find (ptr) != memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    memlist.insert (ptr);
  }

  // Unmark a pointer to be freed on exit, either because it was
  // made persistent, or because it was already freed.
  void unmark (void *ptr)
  {
    auto p = memlist.find (ptr);

    if (p != memlist.end ())
      memlist.erase (p);
#if defined (DEBUG)
    else
      warning ("%s: value not marked", function_name ());
#endif
  }

  mxArray * mark_array (mxArray *ptr)
  {
    arraylist.insert (ptr);
    return ptr;
  }

  void unmark_array (mxArray *ptr)
  {
    auto p = arraylist.find (ptr);

    if (p != arraylist.end ())
      arraylist.erase (p);
  }

  // Mark a pointer as one we allocated.
  void mark_foreign (void *ptr)
  {
#if defined (DEBUG)
    if (foreign_memlist.find (ptr) != foreign_memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    foreign_memlist.insert (ptr);
  }

  // Unmark a pointer as one we allocated.
  void unmark_foreign (void *ptr)
  {
    auto p = foreign_memlist.find (ptr);

    if (p != foreign_memlist.end ())
      foreign_memlist.erase (p);
#if defined (DEBUG)
    else
      warning ("%s: value not marked", function_name ());
#endif

  }

  // Make a new array value and initialize from an octave value; it will be
  // freed on exit unless marked as persistent.
  mxArray * make_value (const octave_value& ov)
  {
    bool interleaved = curr_mex_fcn.use_interleaved_complex ();

    return mark_array (new mxArray (interleaved, ov));
  }

  // Free an array and its contents.
  bool free_value (mxArray *ptr)
  {
    bool inlist = false;

    auto p = arraylist.find (ptr);

    if (p != arraylist.end ())
      {
        inlist = true;
        arraylist.erase (p);
        delete ptr;
      }
#if defined (DEBUG)
    else
      warning ("mex::free_value: skipping memory not allocated by mex::make_value");
#endif

    return inlist;
  }

  octave_mex_function& current_mex_function (void) const
  {
    return curr_mex_fcn;
  }

  // 1 if error should be returned to MEX file, 0 if abort.
  int trap_feval_error = 0;

private:

  // Pointer to the mex function that corresponds to this mex context.
  octave_mex_function& curr_mex_fcn;

  // List of memory resources that need to be freed upon exit.
  std::set<void *> memlist;

  // List of mxArray objects that need to be freed upon exit.
  std::set<mxArray *> arraylist;

  // List of memory resources we know about, but that were allocated
  // elsewhere.
  std::set<void *> foreign_memlist;

  // The name of the currently executing function.
  mutable char *fname;

  // List of memory resources we allocated.
  static std::set<void *> global_memlist;

  // Mark a pointer as one we allocated.
  void global_mark (void *ptr)
  {
#if defined (DEBUG)
    if (global_memlist.find (ptr) != global_memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    global_memlist.insert (ptr);
  }

  // Unmark a pointer as one we allocated.
  void global_unmark (void *ptr)
  {
    auto p = global_memlist.find (ptr);

    if (p != global_memlist.end ())
      global_memlist.erase (p);
#if defined (DEBUG)
    else
      warning ("%s: value not marked", function_name ());
#endif
  }
};

// List of memory resources we allocated.
std::set<void *> mex::global_memlist;

// Current context.
mex *mex_context = nullptr;

void *
mxArray::malloc (size_t n)
{
  return mex_context ? mex_context->malloc_unmarked (n) : std::malloc (n);
}

void *
mxArray::calloc (size_t n, size_t t)
{
  return mex_context ? mex_context->calloc_unmarked (n, t) : ::calloc (n, t);
}

static inline void *
maybe_mark_foreign (void *ptr)
{
  if (mex_context)
    mex_context->mark_foreign (ptr);

  return ptr;
}

static inline mxArray *
maybe_unmark_array (mxArray *ptr)
{
  if (mex_context)
    mex_context->unmark_array (ptr);

  return ptr;
}

template <typename T>
static inline T *
maybe_unmark (T *ptr)
{
  if (mex_context)
    mex_context->unmark (ptr);

  return ptr;
}

void
mxArray_struct::set_field_by_number (mwIndex index, int key_num, mxArray *val)
{
  if (key_num >= 0 && key_num < nfields)
    data[nfields * index + key_num] = maybe_unmark_array (val);
}

void
mxArray_cell::set_cell (mwIndex idx, mxArray *val)
{
  if (idx >= 0 && idx < get_number_of_elements ())
    data[idx] = maybe_unmark_array (val);
}

// ------------------------------------------------------------------

// C interface to mxArray objects:

// Floating point predicates.

bool
mxIsFinite (const double v)
{
  return lo_ieee_finite (v) != 0;
}

bool
mxIsInf (const double v)
{
  return lo_ieee_isinf (v) != 0;
}

bool
mxIsNaN (const double v)
{
  return lo_ieee_isnan (v) != 0;
}

double
mxGetEps (void)
{
  return std::numeric_limits<double>::epsilon ();
}

double
mxGetInf (void)
{
  return lo_ieee_inf_value ();
}

double
mxGetNaN (void)
{
  return lo_ieee_nan_value ();
}

// Memory management.
void *
mxCalloc (size_t n, size_t size)
{
  return mex_context ? mex_context->calloc (n, size) : ::calloc (n, size);
}

void *
mxMalloc (size_t n)
{
  return mex_context ? mex_context->malloc (n) : std::malloc (n);
}

void *
mxRealloc (void *ptr, size_t size)
{
  return mex_context ? mex_context->realloc (ptr, size)
                     : std::realloc (ptr, size);
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
size_t
mxGetM (const mxArray *ptr)
{
  return ptr->get_m ();
}

size_t
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

size_t
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

mxDouble * mxGetDoubles (const mxArray *ptr)
{
  return ptr->get_doubles ();
}

mxSingle * mxGetSingles (const mxArray *ptr)
{
  return ptr->get_singles ();
}

mxInt8 * mxGetInt8s (const mxArray *ptr)
{
  return ptr->get_int8s ();
}

mxInt16 * mxGetInt16s (const mxArray *ptr)
{
  return ptr->get_int16s ();
}

mxInt32 * mxGetInt32s (const mxArray *ptr)
{
  return ptr->get_int32s ();
}

mxInt64 * mxGetInt64s (const mxArray *ptr)
{
  return ptr->get_int64s ();
}

mxUint8 * mxGetUint8s (const mxArray *ptr)
{
  return ptr->get_uint8s ();
}

mxUint16 * mxGetUint16s (const mxArray *ptr)
{
  return ptr->get_uint16s ();
}

mxUint32 * mxGetUint32s (const mxArray *ptr)
{
  return ptr->get_uint32s ();
}

mxUint64 * mxGetUint64s (const mxArray *ptr)
{
  return ptr->get_uint64s ();
}

mxComplexDouble * mxGetComplexDoubles (const mxArray *ptr)
{
  return ptr->get_complex_doubles ();
}

mxComplexSingle * mxGetComplexSingles (const mxArray *ptr)
{
  return ptr->get_complex_singles ();
}

#if 0
/* We don't have these yet. */
mxComplexInt8 * mxGetComplexInt8s (const mxArray *ptr)
{
  return ptr->get_complex_int8s ();
}

mxComplexInt16 * mxGetComplexInt16s (const mxArray *ptr)
{
  return ptr->get_complex_int16s ();
}

mxComplexInt32 * mxGetComplexInt32s (const mxArray *ptr)
{
  return ptr->get_complex_int32s ();
}

mxComplexInt64 * mxGetComplexInt64s (const mxArray *ptr)
{
  return ptr->get_complex_int64s ();
}

mxComplexUint8 * mxGetComplexUint8s (const mxArray *ptr)
{
  return ptr->get_complex_uint8s ();
}

mxComplexUint16 * mxGetComplexUint16s (const mxArray *ptr)
{
  return ptr->get_complex_uint16s ();
}

mxComplexUint32 * mxGetComplexUint32s (const mxArray *ptr)
{
  return ptr->get_complex_uint32s ();
}

mxComplexUint64 * mxGetComplexUint64s (const mxArray *ptr)
{
  return ptr->get_complex_uint64s ();
}
#endif

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

int mxSetDoubles (mxArray *ptr, mxDouble *data)
{
  return ptr->set_doubles (maybe_unmark (data));
}

int mxSetSingles (mxArray *ptr, mxSingle *data)
{
  return ptr->set_singles (maybe_unmark (data));
}

int mxSetInt8s (mxArray *ptr, mxInt8 *data)
{
  return ptr->set_int8s (maybe_unmark (data));
}

int mxSetInt16s (mxArray *ptr, mxInt16 *data)
{
  return ptr->set_int16s (maybe_unmark (data));
}

int mxSetInt32s (mxArray *ptr, mxInt32 *data)
{
  return ptr->set_int32s (maybe_unmark (data));
}

int mxSetInt64s (mxArray *ptr, mxInt64 *data)
{
  return ptr->set_int64s (maybe_unmark (data));
}

int mxSetUint8s (mxArray *ptr, mxUint8 *data)
{
  return ptr->set_uint8s (maybe_unmark (data));
}

int mxSetUint16s (mxArray *ptr, mxUint16 *data)
{
  return ptr->set_uint16s (maybe_unmark (data));
}

int mxSetUint32s (mxArray *ptr, mxUint32 *data)
{
  return ptr->set_uint32s (maybe_unmark (data));
}

int mxSetUint64s (mxArray *ptr, mxUint64 *data)
{
  return ptr->set_uint64s (maybe_unmark (data));
}

int mxSetComplexDoubles (mxArray *ptr, mxComplexDouble *data)
{
  return ptr->set_complex_doubles (maybe_unmark (data));
}

int mxSetComplexSingles (mxArray *ptr, mxComplexSingle *data)
{
  return ptr->set_complex_singles (maybe_unmark (data));
}

#if 0
/* We don't have these yet. */
int mxSetComplexInt8s (mxArray *ptr, mxComplexInt8 *data)
{
  return ptr->set_complex_int8s (maybe_unmark (data));
}

int mxSetComplexInt16s (mxArray *ptr, mxComplexInt16 *data)
{
  return ptr->set_complex_int16s (maybe_unmark (data));
}

int mxSetComplexInt32s (mxArray *ptr, mxComplexInt32 *data)
{
  return ptr->set_complex_int32s (maybe_unmark (data));
}

int mxSetComplexInt64s (mxArray *ptr, mxComplexInt64 *data)
{
  return ptr->set_complex_int64s (maybe_unmark (data));
}

int mxSetComplexUint8s (mxArray *ptr, mxComplexUint8 *data)
{
  return ptr->set_complex_uint8s (maybe_unmark (data));
}

int mxSetComplexUint16s (mxArray *ptr, mxComplexUint16 *data)
{
  return ptr->set_complex_uint16s (maybe_unmark (data));
}

int mxSetComplexUint32s (mxArray *ptr, mxComplexUint32 *data)
{
  return ptr->set_complex_uint32s (maybe_unmark (data));
}

int mxSetComplexUint64s (mxArray *ptr, mxComplexUint64 *data)
{
  return ptr->set_complex_uint64s (maybe_unmark (data));
}
#endif

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

size_t
mxGetElementSize (const mxArray *ptr)
{
  return ptr->get_element_size ();
}

// ------------------------------------------------------------------

typedef void (*cmex_fptr) (int nlhs, mxArray **plhs, int nrhs, mxArray **prhs);
typedef F77_RET_T (*fmex_fptr) (F77_INT& nlhs, mxArray **plhs,
                                F77_INT& nrhs, mxArray **prhs);

octave_value_list
call_mex (octave_mex_function& mex_fcn, const octave_value_list& args,
          int nargout_arg)
{
  octave_quit ();

  // Use at least 1 for nargout since even for zero specified args,
  // still want to be able to return an ans.

  volatile int nargout = nargout_arg;

  int nargin = args.length ();
  OCTAVE_LOCAL_BUFFER (mxArray *, argin, nargin);
  for (int i = 0; i < nargin; i++)
    argin[i] = nullptr;

  int nout = (nargout == 0 ? 1 : nargout);
  OCTAVE_LOCAL_BUFFER (mxArray *, argout, nout);
  for (int i = 0; i < nout; i++)
    argout[i] = nullptr;

  octave::unwind_protect_safe frame;

  // Save old mex pointer.
  frame.protect_var (mex_context);

  mex context (mex_fcn);

  for (int i = 0; i < nargin; i++)
    argin[i] = context.make_value (args(i));

  mex_context = &context;

  void *mex_fcn_ptr = mex_fcn.mex_fcn_ptr ();

  if (mex_fcn.is_fmex ())
    {
      fmex_fptr fcn = reinterpret_cast<fmex_fptr> (mex_fcn_ptr);

      F77_INT tmp_nargout = nargout;
      F77_INT tmp_nargin = nargin;

      fcn (tmp_nargout, argout, tmp_nargin, argin);
    }
  else
    {
      cmex_fptr fcn = reinterpret_cast<cmex_fptr> (mex_fcn_ptr);

      fcn (nargout, argout, nargin, argin);
    }

  // Convert returned array entries back into octave values.

  octave_value_list retval;

  if (nargout == 0 && argout[0])
    {
      // We have something for ans.
      nargout = 1;
    }

  retval.resize (nargout);

  for (int i = 0; i < nargout; i++)
    retval(i) = mxArray::as_octave_value (argout[i], false);

  return retval;
}

// C interface to mex functions:

const char *
mexFunctionName (void)
{
  return mex_context ? mex_context->function_name () : "unknown";
}

int
mexCallMATLAB (int nargout, mxArray *argout[], int nargin,
               mxArray *argin[], const char *fname)
{
  octave_value_list args;

  // FIXME: do we need unwind protect to clean up args?  Off hand, I
  // would say that this problem is endemic to Octave and we will
  // continue to have memory leaks after Ctrl-C until proper exception
  // handling is implemented.

  // FIXME: Proper exception handling has been implemented (Jan. 2016).
  //        Can this code be re-factored?
  args.resize (nargin);

  for (int i = 0; i < nargin; i++)
    args(i) = mxArray::as_octave_value (argin[i]);

  octave::interpreter& interp = octave::__get_interpreter__ ("mexCallMATLAB");

  bool execution_error = false;

  octave_value_list retval;

  try
    {
      retval = octave::feval (fname, args, nargout);
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

  octave::interpreter& interp = octave::__get_interpreter__ ("mexEvalString");

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

  octave::interpreter& interp = octave::__get_interpreter__ ("mexEvalString");

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
  size_t len;

  if (s && (len = strlen (s)) > 0)
    {
      if (s[len - 1] == '\n')
        {
          std::string s_tmp (s, len - 1);
          error ("%s: %s\n", mexFunctionName (), s_tmp.c_str ());
        }
      else
        error ("%s: %s", mexFunctionName (), s);
    }
  else
    {
      // For compatibility with Matlab, print an empty message.
      // Octave's error routine requires a non-null input so use a SPACE.
      error (" ");
    }
}

void
mexErrMsgIdAndTxt (const char *id, const char *fmt, ...)
{
  if (fmt && strlen (fmt) > 0)
    {
      const char *fname = mexFunctionName ();
      size_t len = strlen (fname) + 2 + strlen (fmt) + 1;
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
  size_t len;

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
      size_t len = strlen (fname) + 2 + strlen (fmt) + 1;
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

  octave::interpreter& interp = octave::__get_interpreter__ ("mexGetVariable");

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

              frame.add_method (tw, &octave::tree_evaluator::restore_frame,
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
  if (! ptr)
    return 1;

  if (! name)
    return 1;

  if (name[0] == '\0')
    name = ptr->get_name ();

  if (! name || name[0] == '\0')
    return 1;

  octave::interpreter& interp = octave::__get_interpreter__ ("mexPutVariable");

  if (! strcmp (space, "global"))
    interp.global_assign (name, mxArray::as_octave_value (ptr));
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

              frame.add_method (tw, &octave::tree_evaluator::restore_frame,
                                tw.current_call_stack_frame_number ());

              tw.goto_base_frame ();
            }

          interp.assign (name, mxArray::as_octave_value (ptr));
        }
      else
        mexErrMsgTxt ("mexPutVariable: symbol table does not exist");
    }

  return 0;
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
mexAtExit (void (*f) (void))
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

  octave_value ret = get_property_from_handle (handle, property, "mexGet");

  if (ret.is_defined ())
    m = ret.as_mxArray (true);

  return m;
}

const mxArray *
mexGet (double handle, const char *property)
{
  mxArray *m = nullptr;

  octave_value ret = get_property_from_handle (handle, property, "mexGet");

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
mexIsLocked (void)
{
  int retval = 0;

  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      octave::interpreter& interp = octave::__get_interpreter__ ("mexIsLocked");

      retval = interp.mislocked (fname);
    }

  return retval;
}

std::map<std::string,int> mex_lock_count;

void
mexLock (void)
{
  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      if (mex_lock_count.find (fname) == mex_lock_count.end ())
        mex_lock_count[fname] = 1;
      else
        mex_lock_count[fname]++;

      octave::interpreter& interp = octave::__get_interpreter__ ("mexLock");

      interp.mlock ();
    }
}

int
mexSet (double handle, const char *property, mxArray *val)
{
  bool ret
    = set_property_in_handle (handle, property, mxArray::as_octave_value (val),
                              "mexSet");
  return (ret ? 0 : 1);
}

void
mexUnlock (void)
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
              octave::interpreter& interp
                = octave::__get_interpreter__ ("mexUnLock");

              interp.munlock (fname);

              mex_lock_count.erase (p);
            }
        }
    }
}
