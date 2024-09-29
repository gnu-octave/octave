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
#include "mxarray.h"
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

OCTAVE_NORETURN static void
error_impossible_call (const char *fcn_name)
{
  error ("unexpected call to %s - please report this bug", fcn_name);
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

static void * mxRealloc (void *ptr, std::size_t size);

static void mxFree (void *ptr);

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

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

class mx_deleting_memory_resource : public std::pmr::memory_resource
{
private:

  void * do_allocate (std::size_t bytes, size_t /*alignment*/)
  {
    void *ptr = xmalloc (bytes);

    if (! ptr)
      throw std::bad_alloc ();

    return ptr;
  }

  void do_deallocate (void *ptr, std::size_t /*bytes*/,
                      std::size_t /*alignment*/)
  {
    xfree (ptr);
  }

  bool do_is_equal (const std::pmr::memory_resource& other) const noexcept
  {
    return this == dynamic_cast<const mx_deleting_memory_resource *> (&other);
  }
};

class mx_preserving_memory_resource : public std::pmr::memory_resource
{
private:

  void * do_allocate (std::size_t bytes, size_t /*alignment*/)
  {
    void *ptr = xmalloc (bytes);

    if (! ptr)
      throw std::bad_alloc ();

    return ptr;
  }

  void do_deallocate (void * /*ptr*/, std::size_t /*bytes*/,
                      std::size_t /*alignment*/)
  { }

  bool do_is_equal (const std::pmr::memory_resource& other) const noexcept
  {
    return this == dynamic_cast<const mx_preserving_memory_resource *> (&other);
  }
};

// FIXME: Is it OK for the memory resource object to be defined this
// way?
static mx_deleting_memory_resource the_mx_deleting_memory_resource;
OCTINTERP_API mx_preserving_memory_resource the_mx_preserving_memory_resource;

OCTINTERP_API std::pmr::memory_resource *current_mx_memory_resource = &the_mx_deleting_memory_resource;

#endif

octave_value_list
mx_to_ov_args (int nargin, mxArray *argin[])
{
  // Use a separate function for this job so that the
  // current_mx_memory_resource will be restored immediately after the
  // octave_value objects borrow the mxArray data.  We could also use a
  // dummy scope in mexCallMATLAB, but this function seems less likely
  // to be accidentally deleted.

  octave_value_list args (nargin);

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

  // Use allocator that doesn't free memory because Octave may mutate
  // the value (single element mxArray -> scalar octave_value object,
  // for example) and we need these objects to continue to exist after
  // mexCallMATLAB returns.

  octave::unwind_protect_var<std::pmr::memory_resource *>
  upv (current_mx_memory_resource, &the_mx_preserving_memory_resource);

#endif

  for (int i = 0; i < nargin; i++)
    args(i) = mxArray::as_octave_value (argin[i]);

  return args;
}

void
mexErrMsgTxt_impl (const char *who, const char *s)
{
  std::size_t len;

  if (s && (len = strlen (s)) > 0)
    {
      if (s[len - 1] == '\n')
        {
          std::string s_tmp (s, len - 1);
          error ("%s: %s\n", who, s_tmp.c_str ());
        }
      else
        error ("%s: %s", who, s);
    }
  else
    {
      // For compatibility with Matlab, print an empty message.
      // Octave's error routine requires a non-null input so use a SPACE.
      error (" ");
    }
}

int
mexPutVariable_impl (const char *space, const char *name, const mxArray *ptr)
{
  if (! ptr)
    return 1;

  if (! name)
    return 1;

  if (name[0] == '\0')
    name = ptr->get_name ();

  if (! name || name[0] == '\0')
    return 1;

  octave::interpreter& interp = octave::__get_interpreter__ ();

  if (! strcmp (space, "global"))
    {
#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

      // Use allocator that doesn't free memory because Octave may mutate
      // the value (single element mxArray -> scalar octave_value object,
      // for example) and we need these objects to continue to exist after
      // mexCallMATLAB returns.

      octave::unwind_protect_var<std::pmr::memory_resource *>
      upv (current_mx_memory_resource, &the_mx_preserving_memory_resource);
#endif

      interp.global_assign (name, mxArray::as_octave_value (ptr));
    }
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

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

          // Use allocator that doesn't free memory because Octave may
          // mutate the value (single element mxArray -> scalar
          // octave_value object, for example) and we need these objects
          // to continue to exist after mexCallMATLAB returns.

          octave::unwind_protect_var<std::pmr::memory_resource *>
          upv (current_mx_memory_resource,
               &the_mx_preserving_memory_resource);
#endif

          interp.assign (name, mxArray::as_octave_value (ptr));
        }
      else
        mexErrMsgTxt_impl ("mexPutVariable",
                           "symbol table does not exist");
    }

  return 0;
}

int
mexSet_impl (double handle, const char *property, mxArray *val)
{
#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

  // Use allocator that doesn't free memory because Octave may mutate
  // the value (single element mxArray -> scalar octave_value object,
  // for example) and we need these objects to continue to exist after
  // mexCallMATLAB returns.

  octave::unwind_protect_var<std::pmr::memory_resource *>
  upv (current_mx_memory_resource, &the_mx_preserving_memory_resource);

#endif

  bool ret = octave::set_property_in_handle (handle, property,
             mxArray::as_octave_value (val),
             "mexSet");
  return (ret ? 0 : 1);
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

static inline void *maybe_mark_foreign (void *ptr);

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)
static inline void maybe_disown_ptr (void *ptr);
#endif

#define VOID_MUTATION_METHOD(FCN_NAME, ARG_LIST)        \
  void FCN_NAME ARG_LIST { request_mutation (); }

#define CONST_VOID_MUTATION_METHOD(FCN_NAME, ARG_LIST)  \
  void FCN_NAME ARG_LIST const { request_mutation (); }

#define MUTATION_METHOD(RET_TYPE, FCN_NAME, ARG_LIST, RET_VAL)          \
  RET_TYPE FCN_NAME ARG_LIST { request_mutation (); return RET_VAL; }

#define CONST_MUTATION_METHOD(RET_TYPE, FCN_NAME, ARG_LIST, RET_VAL)    \
  RET_TYPE FCN_NAME ARG_LIST const { request_mutation (); return RET_VAL; }

#define GET_DATA_METHOD(RT, FCN_NAME, ID, COMPLEXITY)                   \
  RT * FCN_NAME () const { return get_data<RT> (ID, COMPLEXITY); }

class mxArray_octave_value : public mxArray_base
{
public:

  mxArray_octave_value () = delete;

  mxArray_octave_value (bool interleaved, const octave_value& ov)
    : mxArray_base (interleaved), m_val (ov), m_mutate_flag (false),
      m_id (mxUNKNOWN_CLASS), m_class_name (nullptr), m_ndims (-1),
      m_dims (nullptr)
  { }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_octave_value& operator = (const mxArray_octave_value&) = delete;

  mxArray_base * dup () const { return new mxArray_octave_value (*this); }

  mxArray * as_mxArray () const
  {
    mxArray *retval = m_val.as_mxArray (m_interleaved);

    // RETVAL is assumed to be an mxArray_matlab object.  Should we
    // panic_unless that condition here or just check and throw an
    // error?

    if (retval)
      {
        // Preserve cached values of class name and dimensions in case
        // they will be used after we mutate.

        // set_class_name will handle deleting class name that comes
        // from as_mxArray conversion function.

        if (m_class_name)
          {
            retval->set_class_name (m_class_name);

            m_class_name = nullptr;
          }

        if (m_dims)
          {
            mwSize *xdims = retval->get_dimensions ();

            mxFree (xdims);

            retval->set_dimensions (m_dims, m_ndims);

            m_dims = nullptr;
          }
      }

    return retval;
  }

  ~mxArray_octave_value ()
  {
    mxFree (m_class_name);
    mxFree (m_dims);
  }

  bool is_octave_value () const { return true; }

  int iscell () const { return m_val.iscell (); }

  int is_char () const { return m_val.is_string (); }

  int is_complex () const { return m_val.iscomplex (); }

  int is_double () const { return m_val.is_double_type (); }

  int is_function_handle () const { return m_val.is_function_handle (); }

  int is_int16 () const { return m_val.is_int16_type (); }

  int is_int32 () const { return m_val.is_int32_type (); }

  int is_int64 () const { return m_val.is_int64_type (); }

  int is_int8 () const { return m_val.is_int8_type (); }

  int is_logical () const { return m_val.islogical (); }

  int is_numeric () const { return m_val.isnumeric (); }

  int is_single () const { return m_val.is_single_type (); }

  int is_sparse () const { return m_val.issparse (); }

  int is_struct () const { return m_val.isstruct (); }

  int is_uint16 () const { return m_val.is_uint16_type (); }

  int is_uint32 () const { return m_val.is_uint32_type (); }

  int is_uint64 () const { return m_val.is_uint64_type (); }

  int is_uint8 () const { return m_val.is_uint8_type (); }

  int is_range () const { return m_val.is_range (); }

  int isreal () const { return m_val.isreal (); }

  int is_logical_scalar_true () const
  {
    return (is_logical_scalar () && m_val.is_true ());
  }

  mwSize get_m () const { return m_val.rows (); }

  mwSize get_n () const
  {
    mwSize n = 1;

    // Force m_dims and m_ndims to be cached.
    get_dimensions ();

    for (mwIndex i = m_ndims - 1; i > 0; i--)
      n *= m_dims[i];

    return n;
  }

  mwSize * get_dimensions () const
  {
    if (! m_dims)
      {
        m_ndims = m_val.ndims ();

        m_dims = static_cast<mwSize *> (mxArray::malloc (m_ndims
                                        * sizeof (mwSize)));

        const dim_vector& dv = m_val.dims ();

        for (mwIndex i = 0; i < m_ndims; i++)
          m_dims[i] = dv(i);
      }

    return m_dims;
  }

  mwSize get_number_of_dimensions () const
  {
    // Force m_dims and m_ndims to be cached.
    get_dimensions ();

    return m_ndims;
  }

  VOID_MUTATION_METHOD (set_m, (mwSize))
  VOID_MUTATION_METHOD (set_n, (mwSize))

  MUTATION_METHOD (int, set_dimensions, (mwSize *, mwSize), 0)

  mwSize get_number_of_elements () const { return m_val.numel (); }

  int isempty () const { return m_val.isempty (); }

  bool is_scalar () const
  {
    // Force m_dims and m_ndims to be cached.
    get_dimensions ();

    return m_ndims == 2 && m_dims[0] == 1 && m_dims[1] == 1;
  }

  mxClassID get_class_id () const
  {
    m_id = mxUNKNOWN_CLASS;

    std::string cn = m_val.class_name ();

    if (cn == "double")
      m_id = mxDOUBLE_CLASS;
    else if (cn == "single")
      m_id = mxSINGLE_CLASS;
    else if (cn == "char")
      m_id = mxCHAR_CLASS;
    else if (cn == "logical")
      m_id = mxLOGICAL_CLASS;
    else if (cn == "cell")
      m_id = mxCELL_CLASS;
    else if (cn == "struct")
      m_id = mxSTRUCT_CLASS;
    else if (cn == "function_handle")
      m_id = mxFUNCTION_CLASS;
    else if (cn == "int8")
      m_id = mxINT8_CLASS;
    else if (cn == "uint8")
      m_id = mxUINT8_CLASS;
    else if (cn == "int16")
      m_id = mxINT16_CLASS;
    else if (cn == "uint16")
      m_id = mxUINT16_CLASS;
    else if (cn == "int32")
      m_id = mxINT32_CLASS;
    else if (cn == "uint32")
      m_id = mxUINT32_CLASS;
    else if (cn == "int64")
      m_id = mxINT64_CLASS;
    else if (cn == "uint64")
      m_id = mxUINT64_CLASS;

    return m_id;
  }

  const char * get_class_name () const
  {
    if (! m_class_name)
      {
        std::string s = m_val.class_name ();
        m_class_name = mxArray::strsave (s.c_str ());
      }

    return m_class_name;
  }

  // Not allowed.
  VOID_MUTATION_METHOD (set_class_name, (const char *))

  mxArray * get_property (mwIndex idx, const char *pname) const
  {
    mxArray *retval = nullptr;

    if (m_val.is_classdef_object ())
      {
        octave_classdef *ov_cdef = m_val.classdef_object_value ();

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
    if (m_val.is_classdef_object ())
      {
        octave_classdef *ov_cdef = m_val.classdef_object_value ();

        if (ov_cdef)
          ov_cdef->set_property (idx, pname, pval->as_octave_value ());
      }
    else
      err_invalid_type ("set_property");
  }

  CONST_MUTATION_METHOD (mxArray *, get_cell, (mwIndex), nullptr)

  // Not allowed.
  VOID_MUTATION_METHOD (set_cell, (mwIndex, mxArray *))

  double get_scalar () const
  {
    if (m_val.issparse ())
      {
        // For sparse arrays, return the first nonzero value.
        const void *m_data = m_val.mex_get_data ();
        if (m_data == nullptr)
          return 0.0;

        if (m_val.islogical ())
          return *static_cast<const bool *> (m_data);
        else if (m_val.isreal ())
          return *static_cast<const double *> (m_data);
        else  // Complex type, only return real part
          return *static_cast<const double *> (m_data);
      }
    else
      return m_val.scalar_value (true);
  }

  void * get_data () const
  {
    // Casting away const required for MEX interface.

    void *retval = const_cast<void *> (m_val.mex_get_data ());

    if (retval && (m_val.isreal () || m_interleaved))
      {
        maybe_mark_foreign (retval);
        return retval;
      }

    request_mutation ();
    return nullptr;
  }

  template <typename T>
  T * get_data (mxClassID class_id, mxComplexity complexity) const
  {
    // Casting away const required for MEX interface.

    void *ptr = const_cast<void *> (m_val.mex_get_data (class_id, complexity));

    T *retval = static_cast<T *> (ptr);

    if (retval && (complexity == mxREAL || m_interleaved))
      {
        maybe_mark_foreign (retval);
        return retval;
      }

    request_mutation ();
    return nullptr;
  }

  GET_DATA_METHOD (mxDouble, get_doubles, mxDOUBLE_CLASS, mxREAL);

  GET_DATA_METHOD (mxSingle, get_singles, mxSINGLE_CLASS, mxREAL);

  GET_DATA_METHOD (mxInt8, get_int8s, mxINT8_CLASS, mxREAL);

  GET_DATA_METHOD (mxInt16, get_int16s, mxINT16_CLASS, mxREAL);

  GET_DATA_METHOD (mxInt32, get_int32s, mxINT32_CLASS, mxREAL);

  GET_DATA_METHOD (mxInt64, get_int64s, mxINT64_CLASS, mxREAL);

  GET_DATA_METHOD (mxUint8, get_uint8s, mxUINT8_CLASS, mxREAL);

  GET_DATA_METHOD (mxUint16, get_uint16s, mxUINT16_CLASS, mxREAL);

  GET_DATA_METHOD (mxUint32, get_uint32s, mxUINT32_CLASS, mxREAL);

  GET_DATA_METHOD (mxUint64, get_uint64s, mxUINT64_CLASS, mxREAL);

  GET_DATA_METHOD (mxComplexDouble, get_complex_doubles,
                   mxDOUBLE_CLASS, mxCOMPLEX);

  GET_DATA_METHOD (mxComplexSingle, get_complex_singles,
                   mxDOUBLE_CLASS, mxCOMPLEX);

  void * get_imag_data () const
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

  // Not allowed.
  VOID_MUTATION_METHOD (set_imag_data, (void *))

  mwIndex * get_ir () const
  {
    // Casting away const required for MEX interface.

    octave_idx_type *ptr = const_cast<octave_idx_type *> (m_val.mex_get_ir ());
    return static_cast<mwIndex *> (maybe_mark_foreign (ptr));
  }

  mwIndex * get_jc () const
  {
    // Casting away const required for MEX interface.

    octave_idx_type *ptr = const_cast<octave_idx_type *> (m_val.mex_get_jc ());
    return static_cast<mwIndex *> (maybe_mark_foreign (ptr));
  }

  mwSize get_nzmax () const { return m_val.nzmax (); }

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

  int get_number_of_fields () const { return m_val.nfields (); }

  CONST_MUTATION_METHOD (const char *, get_field_name_by_number, (int), nullptr)

  CONST_MUTATION_METHOD (int, get_field_number, (const char *), 0)

  int get_string (char *buf, mwSize buflen) const
  {
    int retval = 1;

    mwSize nel = get_number_of_elements ();

    if (m_val.is_string () && nel < buflen)
      {
        charNDArray tmp = m_val.char_array_value ();

        const char *p = tmp.data ();

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = p[i];

        buf[nel] = 0;

        retval = 0;
      }

    return retval;
  }

  char * array_to_string () const
  {
    // FIXME: this is supposed to handle multi-byte character strings.

    char *buf = nullptr;

    if (m_val.is_string ())
      {
        mwSize nel = get_number_of_elements ();

        buf = static_cast<char *> (mxArray::malloc (nel + 1));

        if (buf)
          {
            charNDArray tmp = m_val.char_array_value ();

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
    // Force m_ndims, n_dims to be cached.
    get_dimensions ();

    return calc_single_subscript_internal (m_ndims, m_dims, nsubs, subs);
  }

  std::size_t get_element_size () const
  {
    // Force m_id to be cached.
    get_class_id ();

    switch (m_id)
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

  bool mutation_needed () const { return m_mutate_flag; }

  void request_mutation () const
  {
    if (m_mutate_flag)
      error ("unexpected: m_mutate_flag is true in mxArray_octave_value::request_mutation - please report this bug");

    m_mutate_flag = true;
  }

  mxArray * mutate () const { return as_mxArray (); }

  octave_value as_octave_value () const { return m_val; }

protected:

  mxArray_octave_value (const mxArray_octave_value& arg)
    : mxArray_base (arg), m_val (arg.m_val), m_mutate_flag (arg.m_mutate_flag),
      m_id (arg.m_id), m_class_name (mxArray::strsave (arg.m_class_name)),
      m_ndims (arg.m_ndims),
      m_dims (m_ndims > 0
              ? static_cast<mwSize *> (mxArray::malloc (m_ndims * sizeof (mwSize)))
              : nullptr)
  {
    if (m_dims)
      {
        for (mwIndex i = 0; i < m_ndims; i++)
          m_dims[i] = arg.m_dims[i];
      }
  }

private:

  octave_value m_val;

  mutable bool m_mutate_flag;

  // Caching these does not cost much or lead to much duplicated
  // code.  For other things, we just request mutation to a
  // Matlab-style mxArray object.

  mutable mxClassID m_id;
  mutable char *m_class_name;
  mutable mwSize m_ndims;
  mutable mwSize *m_dims;
};

// The base class for the Matlab-style representation, used to handle
// things that are common to all Matlab-style objects.

class mxArray_matlab : public mxArray_base
{
public:

  mxArray_matlab () = delete;

  // No assignment!
  // FIXME: should this be implemented?
  //        Note that we *do* have a copy constructor.

  mxArray_matlab& operator = (const mxArray_matlab&) = delete;

  ~mxArray_matlab ()
  {
    mxFree (m_class_name);
    mxFree (m_dims);
  }

  int iscell () const { return m_id == mxCELL_CLASS; }

  int is_char () const { return m_id == mxCHAR_CLASS; }

  int is_complex () const { return m_is_complex; }

  int is_double () const { return m_id == mxDOUBLE_CLASS; }

  int is_function_handle () const { return m_id == mxFUNCTION_CLASS; }

  int is_int16 () const { return m_id == mxINT16_CLASS; }

  int is_int32 () const { return m_id == mxINT32_CLASS; }

  int is_int64 () const { return m_id == mxINT64_CLASS; }

  int is_int8 () const { return m_id == mxINT8_CLASS; }

  int is_logical () const { return m_id == mxLOGICAL_CLASS; }

  int is_numeric () const
  {
    return (m_id == mxDOUBLE_CLASS || m_id == mxSINGLE_CLASS
            || m_id == mxINT8_CLASS || m_id == mxUINT8_CLASS
            || m_id == mxINT16_CLASS || m_id == mxUINT16_CLASS
            || m_id == mxINT32_CLASS || m_id == mxUINT32_CLASS
            || m_id == mxINT64_CLASS || m_id == mxUINT64_CLASS);
  }

  int is_single () const { return m_id == mxSINGLE_CLASS; }

  int is_sparse () const { return 0; }

  int is_struct () const { return m_id == mxSTRUCT_CLASS; }

  int is_uint16 () const { return m_id == mxUINT16_CLASS; }

  int is_uint32 () const { return m_id == mxUINT32_CLASS; }

  int is_uint64 () const { return m_id == mxUINT64_CLASS; }

  int is_uint8 () const { return m_id == mxUINT8_CLASS; }

  int is_logical_scalar_true () const
  {
    return (is_logical_scalar ()
            && static_cast<mxLogical *> (get_data ())[0] != 0);
  }

  mwSize get_m () const { return m_dims[0]; }

  mwSize get_n () const
  {
    mwSize n = 1;

    for (mwSize i = m_ndims - 1 ; i > 0 ; i--)
      n *= m_dims[i];

    return n;
  }

  mwSize * get_dimensions () const { return m_dims; }

  mwSize get_number_of_dimensions () const { return m_ndims; }

  void set_m (mwSize m) { m_dims[0] = m; }

  void set_n (mwSize n) { m_dims[1] = n; }

  int set_dimensions (mwSize *dims, mwSize ndims)
  {
    m_ndims = ndims;

    mxFree (m_dims);

    if (m_ndims > 0)
      {
        m_dims
          = static_cast<mwSize *> (mxArray::malloc (m_ndims * sizeof (mwSize)));

        if (m_dims == nullptr)
          return 1;

        for (int i = 0; i < m_ndims; i++)
          m_dims[i] = dims[i];

        return 0;
      }
    else
      {
        m_dims = nullptr;
        return 0;
      }
  }

  mwSize get_number_of_elements () const
  {
    mwSize retval = m_dims[0];

    for (mwIndex i = 1; i < m_ndims; i++)
      retval *= m_dims[i];

    return retval;
  }

  int isempty () const { return get_number_of_elements () == 0; }

  bool is_scalar () const
  {
    return m_ndims == 2 && m_dims[0] == 1 && m_dims[1] == 1;
  }

  mxClassID get_class_id () const { return m_id; }

  static std::string get_class_name (mxClassID id)
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

  const char * get_class_name () const
  {
    return m_class_name;
  }

  void set_class_name (const char *name)
  {
    mxFree (m_class_name);
    m_class_name = static_cast<char *> (mxArray::malloc (strlen (name) + 1));
    strcpy (m_class_name, name);
  }

  mxArray * get_cell (mwIndex /*idx*/) const
  {
    err_invalid_type ("get_cell");
  }

  void set_cell (mwIndex /*idx*/, mxArray * /*val*/)
  {
    err_invalid_type ("set_cell");
  }

  double get_scalar () const
  {
    err_invalid_type ("get_scalar");
  }

  void * get_data () const
  {
    err_invalid_type ("get_data");
  }

  mxDouble * get_doubles () const
  {
    err_invalid_type ("get_doubles");
  }

  mxSingle * get_singles () const
  {
    err_invalid_type ("get_singles");
  }

  mxInt8 * get_int8s () const
  {
    err_invalid_type ("get_int8s");
  }

  mxInt16 * get_int16s () const
  {
    err_invalid_type ("get_int16s");
  }

  mxInt32 * get_int32s () const
  {
    err_invalid_type ("get_int32s");
  }

  mxInt64 * get_int64s () const
  {
    err_invalid_type ("get_int64s");
  }

  mxUint8 * get_uint8s () const
  {
    err_invalid_type ("get_uint8s");
  }

  mxUint16 * get_uint16s () const
  {
    err_invalid_type ("get_uint16s");
  }

  mxUint32 * get_uint32s () const
  {
    err_invalid_type ("get_uint32s");
  }

  mxUint64 * get_uint64s () const
  {
    err_invalid_type ("get_uint64s");
  }

  mxComplexDouble * get_complex_doubles () const
  {
    err_invalid_type ("get_complex_doubles");
  }

  mxComplexSingle * get_complex_singles () const
  {
    err_invalid_type ("get_complex_singles");
  }

  void * get_imag_data () const
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

  void set_imag_data (void * /*pi*/)
  {
    err_invalid_type ("set_imag_data");
  }

  mwIndex * get_ir () const
  {
    err_invalid_type ("get_ir");
  }

  mwIndex * get_jc () const
  {
    err_invalid_type ("get_jc");
  }

  mwSize get_nzmax () const
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

  int get_number_of_fields () const
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

  char * array_to_string () const
  {
    err_invalid_type ("array_to_string");
  }

  mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const
  {
    return calc_single_subscript_internal (m_ndims, m_dims, nsubs, subs);
  }

  std::size_t get_element_size () const
  {
    switch (m_id)
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

  mxArray_matlab (bool interleaved, bool is_complex, mxClassID id,
                  mwSize ndims, const mwSize *dims)
    : mxArray_base (interleaved), m_class_name (nullptr), m_id (id),
      m_is_complex (is_complex), m_ndims (ndims < 2 ? 2 : ndims),
      m_dims (static_cast<mwSize *> (mxArray::malloc (m_ndims * sizeof (mwSize))))
  {
    if (ndims == 0)
      {
        m_dims[0] = 0;
        m_dims[1] = 0;
      }
    else if (ndims < 2)
      {
        m_dims[0] = 1;
        m_dims[1] = 1;
      }

    for (mwIndex i = 0; i < ndims; i++)
      m_dims[i] = dims[i];

    for (mwIndex i = m_ndims - 1; i > 1; i--)
      {
        if (m_dims[i] == 1)
          m_ndims--;
        else
          break;
      }
  }

  mxArray_matlab (bool interleaved, bool is_complex, mxClassID id,
                  const dim_vector& dv)
    : mxArray_base (interleaved), m_class_name (nullptr), m_id (id),
      m_is_complex (is_complex), m_ndims (dv.ndims ()),
      m_dims (static_cast<mwSize *> (mxArray::malloc (m_ndims * sizeof (mwSize))))
  {
    for (mwIndex i = 0; i < m_ndims; i++)
      m_dims[i] = dv(i);

    for (mwIndex i = m_ndims - 1; i > 1; i--)
      {
        if (m_dims[i] == 1)
          m_ndims--;
        else
          break;
      }
  }

  mxArray_matlab (bool interleaved, bool is_complex, mxClassID id,
                  mwSize m, mwSize n)
    : mxArray_base (interleaved), m_class_name (nullptr), m_id (id),
      m_is_complex (is_complex), m_ndims (2),
      m_dims (static_cast<mwSize *> (mxArray::malloc (m_ndims * sizeof (mwSize))))
  {
    m_dims[0] = m;
    m_dims[1] = n;
  }

  mxArray_matlab (const mxArray_matlab& val)
    : mxArray_base (val), m_class_name (mxArray::strsave (val.m_class_name)),
      m_id (val.m_id), m_is_complex (val.m_is_complex), m_ndims (val.m_ndims),
      m_dims (static_cast<mwSize *> (mxArray::malloc (m_ndims * sizeof (mwSize))))
  {
    for (mwIndex i = 0; i < m_ndims; i++)
      m_dims[i] = val.m_dims[i];
  }

  void set_complexity (bool is_complex) { m_is_complex = is_complex; }

  dim_vector
  dims_to_dim_vector () const
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

  char *m_class_name;

  mxClassID m_id;

  bool m_is_complex;

  mwSize m_ndims;
  mwSize *m_dims;
};


// Matlab-style numeric, character, and logical data.

class mxArray_base_full : public mxArray_matlab
{
public:

  mxArray_base_full () = delete;

  mxArray_base_full (bool interleaved, bool is_complex, mxClassID id,
                     mwSize ndims, const mwSize *dims, bool init = true)
    : mxArray_matlab (interleaved, is_complex, id, ndims, dims),
      m_pr (mxArray::alloc (init, get_number_of_elements (), get_element_size ()))
  { }

  mxArray_base_full (bool interleaved, bool is_complex, mxClassID id,
                     const dim_vector& dv)
    : mxArray_matlab (interleaved, is_complex, id, dv),
      m_pr (mxArray::calloc (get_number_of_elements (), get_element_size ()))
  { }

  mxArray_base_full (bool interleaved, bool is_complex, mxClassID id,
                     mwSize m, mwSize n, bool init = true)
    : mxArray_matlab (interleaved, is_complex, id, m, n),
      m_pr (mxArray::alloc (init, get_number_of_elements (), get_element_size ()))
  { }

  mxArray_base_full (bool interleaved, mxClassID id, double val)
    : mxArray_matlab (interleaved, false, id, 1, 1),
      m_pr (mxArray::calloc (get_number_of_elements (), get_element_size ()))
  {
    double *dpr = static_cast<double *> (m_pr);
    dpr[0] = val;
  }

  mxArray_base_full (bool interleaved, mxClassID id, mxLogical val)
    : mxArray_matlab (interleaved, false, id, 1, 1),
      m_pr (mxArray::calloc (get_number_of_elements (), get_element_size ()))
  {
    mxLogical *lpr = static_cast<mxLogical *> (m_pr);
    lpr[0] = val;
  }

  mxArray_base_full (bool interleaved, const char *str)
    : mxArray_matlab (interleaved, false, mxCHAR_CLASS,
                      str ? (strlen (str) ? 1 : 0) : 0,
                      str ? strlen (str) : 0),
      m_pr (mxArray::calloc (get_number_of_elements (), get_element_size ()))
  {
    mxChar *cpr = static_cast<mxChar *> (m_pr);
    mwSize nel = get_number_of_elements ();
    for (mwIndex i = 0; i < nel; i++)
      cpr[i] = str[i];
  }

  // FIXME: ???
  mxArray_base_full (bool interleaved, mwSize m, const char **str)
    : mxArray_matlab (interleaved, false, mxCHAR_CLASS, m, max_str_len (m, str)),
      m_pr (mxArray::calloc (get_number_of_elements (), get_element_size ()))
  {
    mxChar *cpr = static_cast<mxChar *> (m_pr);

    mwSize *dv = get_dimensions ();

    mwSize nc = dv[1];

    for (mwIndex j = 0; j < m; j++)
      {
        const char *ptr = str[j];

        std::size_t tmp_len = strlen (ptr);

        for (std::size_t i = 0; i < tmp_len; i++)
          cpr[m*i+j] = static_cast<mxChar> (ptr[i]);

        for (std::size_t i = tmp_len; i < static_cast<std::size_t> (nc); i++)
          cpr[m*i+j] = static_cast<mxChar> (' ');
      }
  }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_base_full& operator = (const mxArray_base_full&) = delete;

  mxArray_base * dup () const
  {
    return new mxArray_base_full (*this);
  }

  ~mxArray_base_full ()
  {
    mxFree (m_pr);
  }

  double get_scalar () const
  {
    // FIXME: how does this work for interleaved complex arrays?

    double retval = 0;

    mxClassID id = get_class_id ();

    switch (id)
      {
      case mxDOUBLE_CLASS:
        retval = *(static_cast<double *> (m_pr));
        break;

      case mxSINGLE_CLASS:
        retval = *(static_cast<float *> (m_pr));
        break;

      case mxCHAR_CLASS:
        retval = *(static_cast<mxChar *> (m_pr));
        break;

      case mxLOGICAL_CLASS:
        retval = *(static_cast<bool *> (m_pr));
        break;

      case mxINT8_CLASS:
        retval = *(static_cast<int8_t *> (m_pr));
        break;

      case mxUINT8_CLASS:
        retval = *(static_cast<uint8_t *> (m_pr));
        break;

      case mxINT16_CLASS:
        retval = *(static_cast<int16_t *> (m_pr));
        break;

      case mxUINT16_CLASS:
        retval = *(static_cast<uint16_t *> (m_pr));
        break;

      case mxINT32_CLASS:
        retval = *(static_cast<int32_t *> (m_pr));
        break;

      case mxUINT32_CLASS:
        retval = *(static_cast<uint32_t *> (m_pr));
        break;

      case mxINT64_CLASS:
        retval = *(static_cast<int64_t *> (m_pr));
        break;

      case mxUINT64_CLASS:
        retval = *(static_cast<uint64_t *> (m_pr));
        break;

      case mxCELL_CLASS:
      case mxFUNCTION_CLASS:
      case mxSTRUCT_CLASS:
      case mxUNKNOWN_CLASS:
      case mxVOID_CLASS:
        {
          std::string dest_cname = get_class_name (id);
          error ("invalid conversion from %s mxArray to %s scalar value", get_class_name (), dest_cname.c_str ());
        }
        break;

        // We should have handled all possible enum values above.  Rely
        // on compiler diagnostics to warn if we haven't.  For example,
        // GCC's -Wswitch option, enabled by -Wall, will provide a
        // warning.
      }

    return retval;
  }

  void * get_data () const { return m_pr; }

  void set_data (void *pr) { m_pr = pr; }

  // The typed get and set functions only work for interleaved data but
  // they are defined here because this class owns PR.  There are
  // definitions in the mxArray_separate_full class that override these
  // functions.

  mxDouble * get_doubles () const
  {
    return static_cast<mxDouble *> (m_pr);
  }

  mxSingle * get_singles () const
  {
    return static_cast<mxSingle *> (m_pr);
  }

  mxInt8 * get_int8s () const
  {
    return static_cast<mxInt8 *> (m_pr);
  }

  mxInt16 * get_int16s () const
  {
    return static_cast<mxInt16 *> (m_pr);
  }

  mxInt32 * get_int32s () const
  {
    return static_cast<mxInt32 *> (m_pr);
  }

  mxInt64 * get_int64s () const
  {
    return static_cast<mxInt64 *> (m_pr);
  }

  mxUint8 * get_uint8s () const
  {
    return static_cast<mxUint8 *> (m_pr);
  }

  mxUint16 * get_uint16s () const
  {
    return static_cast<mxUint16 *> (m_pr);
  }

  mxUint32 * get_uint32s () const
  {
    return static_cast<mxUint32 *> (m_pr);
  }

  mxUint64 * get_uint64s () const
  {
    return static_cast<mxUint64 *> (m_pr);
  }

  mxComplexDouble * get_complex_doubles () const
  {
    return static_cast<mxComplexDouble *> (m_pr);
  }

  mxComplexSingle * get_complex_singles () const
  {
    return static_cast<mxComplexSingle *> (m_pr);
  }

  int set_doubles (mxDouble *d)
  {
    m_pr = d;
    return 0;
  }

  int set_singles (mxSingle *d)
  {
    m_pr = d;
    return 0;
  }

  int set_int8s (mxInt8 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_int16s (mxInt16 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_int32s (mxInt32 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_int64s (mxInt64 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_uint8s (mxUint8 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_uint16s (mxUint16 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_uint32s (mxUint32 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_uint64s (mxUint64 *d)
  {
    m_pr = d;
    return 0;
  }

  int set_complex_doubles (mxComplexDouble *d)
  {
    m_pr = d;
    return 0;
  }

  int set_complex_singles (mxComplexSingle *d)
  {
    m_pr = d;
    return 0;
  }

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
        mxChar *ptr = static_cast<mxChar *> (m_pr);

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = static_cast<char> (ptr[i]);

        buf[nel] = 0;
      }

    return retval;
  }

  char * array_to_string () const
  {
    // FIXME: this is supposed to handle multi-byte character strings.

    mwSize nel = get_number_of_elements ();

    char *buf = static_cast<char *> (mxArray::malloc (nel + 1));

    if (buf)
      {
        mxChar *ptr = static_cast<mxChar *> (m_pr);

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = static_cast<char> (ptr[i]);

        buf[nel] = '\0';
      }

    return buf;
  }

  octave_value as_octave_value () const
  {
    octave_value retval;

    const dim_vector& dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        return (is_complex ()
                ? fp_to_ov<Complex> (dv) : fp_to_ov<double> (dv));

      case mxSINGLE_CLASS:
        return (is_complex ()
                ? fp_to_ov<FloatComplex> (dv) : fp_to_ov<float> (dv));

      case mxCHAR_CLASS:
        return int_to_ov<mxChar, charNDArray, char> (dv);

      case mxLOGICAL_CLASS:
        return int_to_ov<mxLogical, boolNDArray, bool> (dv);

      case mxINT8_CLASS:
        return int_to_ov<int8_t, int8NDArray, octave_int8> (dv);

      case mxUINT8_CLASS:
        return int_to_ov<uint8_t, uint8NDArray, octave_uint8> (dv);

      case mxINT16_CLASS:
        return int_to_ov<int16_t, int16NDArray, octave_int16> (dv);

      case mxUINT16_CLASS:
        return int_to_ov<uint16_t, uint16NDArray, octave_uint16> (dv);

      case mxINT32_CLASS:
        return int_to_ov<int32_t, int32NDArray, octave_int32> (dv);

      case mxUINT32_CLASS:
        return int_to_ov<uint32_t, uint32NDArray, octave_uint32> (dv);

      case mxINT64_CLASS:
        return int_to_ov<int64_t, int64NDArray, octave_int64> (dv);

      case mxUINT64_CLASS:
        return int_to_ov<uint64_t, uint64NDArray, octave_uint64> (dv);

      case mxCELL_CLASS:
      case mxFUNCTION_CLASS:
      case mxSTRUCT_CLASS:
      case mxUNKNOWN_CLASS:
      case mxVOID_CLASS:
        error ("invalid conversion from %s%s mxArray to octave_value", (is_complex () ? "complex " : ""), get_class_name ());
        break;

        // We should have handled all possible enum values above.  Rely
        // on compiler diagnostics to warn if we haven't.  For example,
        // GCC's -Wswitch option, enabled by -Wall, will provide a
        // warning.
      }

    return retval;
  }

protected:

  mxArray_base_full (const mxArray_base_full& val)
    : mxArray_matlab (val),
      m_pr (mxArray::malloc (get_number_of_elements () * get_element_size ()))
  {
    if (m_pr)
      memcpy (m_pr, val.m_pr, get_number_of_elements () * get_element_size ());
  }

  template <typename ELT_T>
  octave_value
  fp_to_ov (const dim_vector& dv) const
  {
    octave_value retval;

    ELT_T *ppr = static_cast<ELT_T *> (m_pr);

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

    if (current_mx_memory_resource == &the_mx_deleting_memory_resource)
      {
        octave::unwind_action act ([this] () { maybe_disown_ptr (m_pr); });

        return octave_value (Array<ELT_T> (ppr, dv, current_mx_memory_resource));
      }
    else
      return octave_value (Array<ELT_T> (ppr, dv, current_mx_memory_resource));

#else

    // Copy data instead of allowing the octave_value object to borrow
    // the mxArray object data.

    Array<ELT_T> val (dv);

    ELT_T *ptr = val.rwdata ();

    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      ptr[i] = ppr[i];

    return octave_value (val);

#endif
  }

  template <typename ELT_T, typename ARRAY_T, typename ARRAY_ELT_T>
  octave_value
  int_to_ov (const dim_vector& dv) const
  {
    if (is_complex ())
      error ("complex integer types are not supported");

    ELT_T *ppr = static_cast<ELT_T *> (m_pr);

    // Copy data instead of allowing the octave_value object to borrow
    // the mxArray object data.

    ARRAY_T val (dv);

    ARRAY_ELT_T *ptr = val.rwdata ();

    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      ptr[i] = ppr[i];

    return octave_value (val);

  }

protected:

  // If using interleaved complex storage, this is the pointer to data
  // (real, complex, or logical).  Otherwise, it is the pointer to the
  // real part of the data.
  void *m_pr;
};

class mxArray_interleaved_full : public mxArray_base_full
{
public:

  mxArray_interleaved_full () = delete;

  mxArray_interleaved_full (mxClassID id, mwSize ndims, const mwSize *dims,
                            mxComplexity flag = mxREAL, bool init = true)
    : mxArray_base_full (true, flag == mxCOMPLEX, id, ndims, dims, init)
  { }

  mxArray_interleaved_full (mxClassID id, const dim_vector& dv,
                            mxComplexity flag = mxREAL)
    : mxArray_base_full (true, flag == mxCOMPLEX, id, dv)
  { }

  mxArray_interleaved_full (mxClassID id, mwSize m, mwSize n,
                            mxComplexity flag = mxREAL, bool init = true)
    : mxArray_base_full (true, flag == mxCOMPLEX, id, m, n, init)
  { }

  mxArray_interleaved_full (mxClassID id, double val)
    : mxArray_base_full (true, id, val)
  { }

  mxArray_interleaved_full (mxClassID id, mxLogical val)
    : mxArray_base_full (true, id, val)
  { }

  mxArray_interleaved_full (const char *str)
    : mxArray_base_full (true, str)
  { }

  // FIXME: ???
  mxArray_interleaved_full (mwSize m, const char **str)
    : mxArray_base_full (true, m, str)
  { }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_interleaved_full& operator = (const mxArray_interleaved_full&) = delete;

  mxArray_base * dup () const
  {
    return new mxArray_interleaved_full (*this);
  }

  ~mxArray_interleaved_full () = default;

  void * get_imag_data () const { error_impossible_call ("mxArray_interleaved_full::get_imag_data"); }

  void set_imag_data (void */*pi*/) { error_impossible_call ("mxArray_interleaved_full::set_imag_data"); }

protected:

  mxArray_interleaved_full (const mxArray_interleaved_full& val)
    : mxArray_base_full (val)
  { }
};

class mxArray_separate_full : public mxArray_base_full
{
public:

  mxArray_separate_full () = delete;

  mxArray_separate_full (mxClassID id, mwSize ndims, const mwSize *dims,
                         mxComplexity flag = mxREAL, bool init = true)
    : mxArray_base_full (false, flag == mxCOMPLEX, id, ndims, dims, init),
      m_pi (flag == mxCOMPLEX
            ? mxArray::alloc (init, get_number_of_elements (), get_element_size ())
            : nullptr)
  { }

  mxArray_separate_full (mxClassID id, const dim_vector& dv,
                         mxComplexity flag = mxREAL)
    : mxArray_base_full (false, flag == mxCOMPLEX, id, dv),
      m_pi (is_complex ()
            ? mxArray::calloc (get_number_of_elements (), get_element_size ())
            : nullptr)
  { }

  mxArray_separate_full (mxClassID id, mwSize m, mwSize n,
                         mxComplexity flag = mxREAL, bool init = true)
    : mxArray_base_full (false, flag == mxCOMPLEX, id, m, n, init),
      m_pi (is_complex ()
            ? (mxArray::alloc (init, get_number_of_elements (), get_element_size ()))
            : nullptr)
  { }

  mxArray_separate_full (mxClassID id, double val)
    : mxArray_base_full (false, id, val), m_pi (nullptr)
  { }

  mxArray_separate_full (mxClassID id, mxLogical val)
    : mxArray_base_full (false, id, val), m_pi (nullptr)
  { }

  mxArray_separate_full (const char *str)
    : mxArray_base_full (false, str), m_pi (nullptr)
  { }

  // FIXME: ???
  mxArray_separate_full (mwSize m, const char **str)
    : mxArray_base_full (false, m, str), m_pi (nullptr)
  { }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_separate_full& operator = (const mxArray_separate_full&) = delete;

  mxArray_base * dup () const
  {
    return new mxArray_separate_full (*this);
  }

  ~mxArray_separate_full ()
  {
    mxFree (m_pi);
  }

  void * get_imag_data () const { return m_pi; }

  void set_imag_data (void *pi)
  {
    m_pi = pi;

    set_complexity (m_pi != nullptr);
  }

  mxDouble * get_doubles () const { error_impossible_call ("mxArray_separate_full::get_doubles"); }
  mxSingle * get_singles () const { error_impossible_call ("mxArray_separate_full::get_singles"); }
  mxInt8 * get_int8s () const { error_impossible_call ("mxArray_separate_full::get_int8s"); }
  mxInt16 * get_int16s () const { error_impossible_call ("mxArray_separate_full::get_int16s"); }
  mxInt32 * get_int32s () const { error_impossible_call ("mxArray_separate_full::get_int32s"); }
  mxInt64 * get_int64s () const { error_impossible_call ("mxArray_separate_full::get_int64s"); }
  mxUint8 * get_uint8s () const { error_impossible_call ("mxArray_separate_full::get_uint8s"); }
  mxUint16 * get_uint16s () const { error_impossible_call ("mxArray_separate_full::get_uint16s"); }
  mxUint32 * get_uint32s () const { error_impossible_call ("mxArray_separate_full::get_uint32s"); }
  mxUint64 * get_uint64s () const { error_impossible_call ("mxArray_separate_full::get_uint64s"); }

  mxComplexDouble * get_complex_doubles () const { error_impossible_call ("mxArray_separate_full::get_complex_doubles"); }
  mxComplexSingle * get_complex_singles () const { error_impossible_call ("mxArray_separate_full::get_complex_singles"); }

  // We don't have complex integer types, but for separate storage they
  // still would not work.
  mxComplexInt8 * get_complex_int8s () const { error_impossible_call ("mxArray_separate_full::get_complex_int8s"); }
  mxComplexInt16 * get_complex_int16s () const { error_impossible_call ("mxArray_separate_full::get_complex_int16s"); }
  mxComplexInt32 * get_complex_int32s () const { error_impossible_call ("mxArray_separate_full::get_complex_int32s"); }
  mxComplexInt64 * get_complex_int64s () const { error_impossible_call ("mxArray_separate_full::get_complex_int64s"); }
  mxComplexUint8 * get_complex_uint8s () const { error_impossible_call ("mxArray_separate_full::get_complex_uint8s"); }
  mxComplexUint16 * get_complex_uint16s () const { error_impossible_call ("mxArray_separate_full::get_complex_uint16s"); }
  mxComplexUint32 * get_complex_uint32s () const { error_impossible_call ("mxArray_separate_full::get_complex_uint32s"); }
  mxComplexUint64 * get_complex_uint64s () const { error_impossible_call ("mxArray_separate_full::get_complex_uint64s"); }

  int set_doubles (mxDouble *) { error_impossible_call ("mxArray_separate_full::set_doubles"); }
  int set_singles (mxSingle *) { error_impossible_call ("mxArray_separate_full::set_singles"); }
  int set_int8s (mxInt8 *) { error_impossible_call ("mxArray_separate_full::set_int8s"); }
  int set_int16s (mxInt16 *) { error_impossible_call ("mxArray_separate_full::set_int16s"); }
  int set_int32s (mxInt32 *) { error_impossible_call ("mxArray_separate_full::set_int32s"); }
  int set_int64s (mxInt64 *) { error_impossible_call ("mxArray_separate_full::set_int64s"); }
  int set_uint8s (mxUint8 *) { error_impossible_call ("mxArray_separate_full::set_uint8s"); }
  int set_uint16s (mxUint16 *) { error_impossible_call ("mxArray_separate_full::set_uint16s"); }
  int set_uint32s (mxUint32 *) { error_impossible_call ("mxArray_separate_full::set_uint32s"); }
  int set_uint64s (mxUint64 *) { error_impossible_call ("mxArray_separate_full::set_uint64s"); }

  int set_complex_doubles (mxComplexDouble *) { error_impossible_call ("mxArray_separate_full::set_complex_doubles"); }
  int set_complex_singles (mxComplexSingle *) { error_impossible_call ("mxArray_separate_full::set_complex_singles"); }

  // We don't have complex integer types, but for separate storage they
  // still would not work.
  int set_complex_int8s (mxComplexInt8 *) { error_impossible_call ("mxArray_separate_full::set_complex_int8s"); }
  int set_complex_int16s (mxComplexInt16 *) { error_impossible_call ("mxArray_separate_full::set_complex_int16s"); }
  int set_complex_int32s (mxComplexInt32 *) { error_impossible_call ("mxArray_separate_full::set_complex_int32s"); }
  int set_complex_int64s (mxComplexInt64 *) { error_impossible_call ("mxArray_separate_full::set_complex_int64s"); }
  int set_complex_uint8s (mxComplexUint8 *) { error_impossible_call ("mxArray_separate_full::set_complex_uint8s"); }
  int set_complex_uint16s (mxComplexUint16 *) { error_impossible_call ("mxArray_separate_full::set_complex_uint16s"); }
  int set_complex_uint32s (mxComplexUint32 *) { error_impossible_call ("mxArray_separate_full::set_complex_uint32s"); }
  int set_complex_uint64s (mxComplexUint64 *) { error_impossible_call ("mxArray_separate_full::set_complex_uint64s"); }

  octave_value as_octave_value () const
  {
    if (! is_complex ())
      return mxArray_base_full::as_octave_value ();

    octave_value retval;

    const dim_vector& dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        return to_ov<double> (dv);

      case mxSINGLE_CLASS:
        return to_ov<float> (dv);

      case mxLOGICAL_CLASS:
      case mxINT8_CLASS:
      case mxUINT8_CLASS:
      case mxINT16_CLASS:
      case mxUINT16_CLASS:
      case mxINT32_CLASS:
      case mxUINT32_CLASS:
      case mxINT64_CLASS:
      case mxUINT64_CLASS:
        error ("complex integer types are not supported");

      case mxCELL_CLASS:
      case mxCHAR_CLASS:
      case mxFUNCTION_CLASS:
      case mxSTRUCT_CLASS:
      case mxUNKNOWN_CLASS:
      case mxVOID_CLASS:
        error ("invalid conversion from complex %s mxArray to octave_value", get_class_name ());
        break;

        // We should have handled all possible enum values above.  Rely
        // on compiler diagnostics to warn if we haven't.  For example,
        // GCC's -Wswitch option, enabled by -Wall, will provide a
        // warning.
      }

    return retval;
  }

protected:

  mxArray_separate_full (const mxArray_separate_full& val)
    : mxArray_base_full (val),
      m_pi (val.m_pi
            ? mxArray::malloc (get_number_of_elements () * get_element_size ())
            : nullptr)
  {
    if (m_pi)
      memcpy (m_pi, val.m_pi, get_number_of_elements () * get_element_size ());
  }

private:

  template <typename T>
  octave_value
  to_ov (const dim_vector& dv) const
  {
    mwSize nel = get_number_of_elements ();

    T *ppr = static_cast<T *> (m_pr);

    // We allocate in the Array<T> constructor and copy here, so we
    // don't need the custom allocator for this object.

    Array<std::complex<T>> val (dv);

    std::complex<T> *ptr = val.rwdata ();

    T *ppi = static_cast<T *> (m_pi);

    for (mwIndex i = 0; i < nel; i++)
      ptr[i] = std::complex<T> (ppr[i], ppi[i]);

    return octave_value (val);
  }

  // Pointer to the imaginary part of the data.
  void *m_pi;
};

// Matlab-style sparse arrays.

class mxArray_base_sparse : public mxArray_matlab
{
public:

  mxArray_base_sparse () = delete;

  mxArray_base_sparse (bool interleaved, bool is_complex,
                       mxClassID id, mwSize m, mwSize n, mwSize nzmax)
    : mxArray_matlab (interleaved, is_complex, id, m, n),

      m_nzmax (nzmax > 0 ? nzmax : 1),
      m_ir (static_cast<mwIndex *> (mxArray::calloc (m_nzmax, sizeof (mwIndex)))),
      m_jc (static_cast<mwIndex *> (mxArray::calloc (n + 1, sizeof (mwIndex)))),
      m_pr (mxArray::calloc (m_nzmax, get_element_size ()))
  { }

protected:

  mxArray_base_sparse (const mxArray_base_sparse& val)
    : mxArray_matlab (val), m_nzmax (val.m_nzmax),
      m_ir (static_cast<mwIndex *> (mxArray::malloc (m_nzmax * sizeof (mwIndex)))),
      m_jc (static_cast<mwIndex *> (mxArray::malloc (m_nzmax * sizeof (mwIndex)))),
      m_pr (mxArray::malloc (m_nzmax * get_element_size ()))
  {
    if (m_ir)
      memcpy (m_ir, val.m_ir, m_nzmax * sizeof (mwIndex));

    if (m_jc)
      memcpy (m_jc, val.m_jc, (val.get_n () + 1) * sizeof (mwIndex));

    if (m_pr)
      memcpy (m_pr, val.m_pr, m_nzmax * get_element_size ());
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_base_sparse& operator = (const mxArray_base_sparse&) = delete;

  mxArray_base * dup () const
  {
    return new mxArray_base_sparse (*this);
  }

  ~mxArray_base_sparse ()
  {
    mxFree (m_ir);
    mxFree (m_jc);
    mxFree (m_pr);
  }

  int is_sparse () const { return 1; }

  void * get_data () const { return m_pr; }

  void set_data (void *pr) { m_pr = pr; }

  mxDouble * get_doubles () const
  {
    return static_cast<mxDouble *> (m_pr);
  }

  mxComplexDouble * get_complex_doubles () const
  {
    return static_cast<mxComplexDouble *> (m_pr);
  }

  int set_doubles (mxDouble *d)
  {
    m_pr = d;
    return 0;
  }

  int set_complex_doubles (mxComplexDouble *d)
  {
    m_pr = d;
    return 0;
  }

  mwIndex * get_ir () const { return m_ir; }

  mwIndex * get_jc () const { return m_jc; }

  mwSize get_nzmax () const { return m_nzmax; }

  void set_ir (mwIndex *ir) { m_ir = ir; }

  void set_jc (mwIndex *jc) { m_jc = jc; }

  void set_nzmax (mwSize nzmax)
  {
    /* Require storage for at least 1 element */
    m_nzmax = (nzmax > 0 ? nzmax : 1);
  }

  octave_value as_octave_value () const
  {
    octave_value retval;

    const dim_vector& dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        return is_complex () ? to_ov<Complex> (dv): to_ov<double> (dv);

      case mxSINGLE_CLASS:
        error ("single precision sparse data type not supported");

      case mxLOGICAL_CLASS:
        return to_ov<bool> (dv);

      case mxINT8_CLASS:
      case mxUINT8_CLASS:
      case mxINT16_CLASS:
      case mxUINT16_CLASS:
      case mxINT32_CLASS:
      case mxUINT32_CLASS:
      case mxINT64_CLASS:
      case mxUINT64_CLASS:
      case mxCELL_CLASS:
      case mxCHAR_CLASS:
      case mxFUNCTION_CLASS:
      case mxSTRUCT_CLASS:
      case mxUNKNOWN_CLASS:
      case mxVOID_CLASS:
        error ("invalid conversion from %s%s sparse mxArray to octave_value", (is_complex () ? "complex " : ""), get_class_name ());
        break;

        // We should have handled all possible enum values above.  Rely
        // on compiler diagnostics to warn if we haven't.  For example,
        // GCC's -Wswitch option, enabled by -Wall, will provide a
        // warning.
      }

    return retval;
  }

protected:

  template <typename ELT_T>
  octave_value
  to_ov (const dim_vector& dv) const
  {
    ELT_T *ppr = static_cast<ELT_T *> (m_pr);

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

    if (current_mx_memory_resource == &the_mx_deleting_memory_resource)
      {
        octave::unwind_action act ([this] ()
        {
          maybe_disown_ptr (m_pr);
          maybe_disown_ptr (m_ir);
          maybe_disown_ptr (m_jc);
        });

        return octave_value
               (Sparse<ELT_T> (dv,  static_cast<octave_idx_type> (m_nzmax),
                               ppr, m_ir, m_jc, current_mx_memory_resource));
      }
    else
      return octave_value
             (Sparse<ELT_T> (dv,  static_cast<octave_idx_type> (m_nzmax),
                             ppr, m_ir, m_jc, current_mx_memory_resource));
#else

    // Copy data instead of allowing the octave_value object to borrow
    // the mxArray object data.

    octave_idx_type m = dv(0);
    octave_idx_type n = dv(1);

    Sparse<ELT_T> val (m, n, static_cast<octave_idx_type> (m_nzmax));

    for (mwIndex i = 0; i < m_nzmax; i++)
      {
        val.xdata (i) = ppr[i];
        val.xridx (i) = m_ir[i];
      }

    for (mwIndex i = 0; i < n + 1; i++)
      val.xcidx (i) = m_jc[i];

    return octave_value (val);

#endif
  }

  // Maximun number of nonzero elements.
  mwSize m_nzmax;

  // Sparse storage indexing arrays.
  mwIndex *m_ir;
  mwIndex *m_jc;

  // If using interleaved complex storage, this is the pointer to data
  // (real, complex, or logical).  Otherwise, it is the pointer to the
  // real part of the data.
  void *m_pr;
};

class mxArray_interleaved_sparse : public mxArray_base_sparse
{
public:

  mxArray_interleaved_sparse () = delete;

  mxArray_interleaved_sparse (mxClassID id, mwSize m, mwSize n, mwSize nzmax,
                              mxComplexity flag = mxREAL)
    : mxArray_base_sparse (true, flag == mxCOMPLEX, id, m, n, nzmax)
  { }

private:

  mxArray_interleaved_sparse (const mxArray_interleaved_sparse& val)
    : mxArray_base_sparse (val)
  { }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_interleaved_sparse& operator = (const mxArray_interleaved_sparse&) = delete;

  mxArray_base * dup () const
  {
    return new mxArray_interleaved_sparse (*this);
  }

  ~mxArray_interleaved_sparse () = default;

  void * get_imag_data () const { error_impossible_call ("mxArray_interleaved_sparse::get_imag_data"); }

  void set_imag_data (void */*pi*/) { error_impossible_call ("mxArray_interleaved_sparse::set_imag_data"); }
};

class mxArray_separate_sparse : public mxArray_base_sparse
{
public:

  mxArray_separate_sparse () = delete;

  mxArray_separate_sparse (mxClassID id, mwSize m, mwSize n, mwSize nzmax,
                           mxComplexity flag = mxREAL)
    : mxArray_base_sparse (false, flag == mxCOMPLEX, id, m, n, nzmax),
      m_pi (is_complex ()
            ? mxArray::calloc (m_nzmax, get_element_size ())
            : nullptr)
  { }

private:

  mxArray_separate_sparse (const mxArray_separate_sparse& val)
    : mxArray_base_sparse (val),
      m_pi (val.m_pi
            ? mxArray::malloc (m_nzmax * get_element_size ())
            : nullptr)
  {
    if (m_pi)
      memcpy (m_pi, val.m_pi, m_nzmax * get_element_size ());
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_separate_sparse& operator = (const mxArray_separate_sparse&) = delete;

  mxArray_base * dup () const
  {
    return new mxArray_separate_sparse (*this);
  }

  ~mxArray_separate_sparse ()
  {
    mxFree (m_pi);
  }

  void * get_imag_data () const { return m_pi; }

  void set_imag_data (void *pi)
  {
    m_pi = pi;
    set_complexity (m_pi != nullptr);
  }

  mxDouble * get_doubles () const { error_impossible_call ("mxArray_separate_sparse::get_doubles"); }
  mxComplexDouble * get_complex_doubles () const { error_impossible_call ("mxArray_separate_sparse::get_complex_doubles"); }

  int set_doubles (mxDouble *) { error_impossible_call ("mxArray_separate_sparse::set_doubles"); }
  int set_complex_doubles (mxComplexDouble *) { error_impossible_call ("mxArray_separate_sparse::set_complex_doubles"); }

  octave_value as_octave_value () const
  {
    if (! is_complex ())
      return mxArray_base_sparse::as_octave_value ();

    octave_value retval;

    const dim_vector& dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxDOUBLE_CLASS:
        {
          double *ppr = static_cast<double *> (m_pr);
          double *ppi = static_cast<double *> (m_pi);

          SparseComplexMatrix val (get_m (), get_n (),
                                   static_cast<octave_idx_type> (m_nzmax));

          for (mwIndex i = 0; i < m_nzmax; i++)
            {
              val.xdata (i) = Complex (ppr[i], ppi[i]);
              val.xridx (i) = m_ir[i];
            }

          for (mwIndex i = 0; i < get_n () + 1; i++)
            val.xcidx (i) = m_jc[i];

          retval = val;
        }
        break;

      case mxSINGLE_CLASS:
        error ("single precision sparse data type not supported");

      case mxLOGICAL_CLASS:
      case mxINT8_CLASS:
      case mxUINT8_CLASS:
      case mxINT16_CLASS:
      case mxUINT16_CLASS:
      case mxINT32_CLASS:
      case mxUINT32_CLASS:
      case mxINT64_CLASS:
      case mxUINT64_CLASS:
      case mxCELL_CLASS:
      case mxCHAR_CLASS:
      case mxFUNCTION_CLASS:
      case mxSTRUCT_CLASS:
      case mxUNKNOWN_CLASS:
      case mxVOID_CLASS:
        error ("invalid conversion from complex %s sparse mxArray to octave_value", get_class_name ());
        break;

        // We should have handled all possible enum values above.  Rely
        // on compiler diagnostics to warn if we haven't.  For example,
        // GCC's -Wswitch option, enabled by -Wall, will provide a
        // warning.
      }

    return retval;
  }

private:

  // Pointer to the imaginary part of the data.
  void *m_pi;
};

// Matlab-style struct arrays.

class mxArray_struct : public mxArray_matlab
{
public:

  mxArray_struct () = delete;

  mxArray_struct (bool interleaved, mwSize ndims, const mwSize *dims,
                  int num_keys, const char **keys)
    : mxArray_matlab (interleaved, false, mxSTRUCT_CLASS, ndims, dims),
      m_nfields (num_keys),
      m_fields (static_cast<char **> (mxArray::calloc (m_nfields,
                                      sizeof (char *)))),
      m_data (static_cast<mxArray * *> (mxArray::calloc (m_nfields *
                                        get_number_of_elements (),
                                        sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (bool interleaved, const dim_vector& dv, int num_keys,
                  const char **keys)
    : mxArray_matlab (interleaved, false, mxSTRUCT_CLASS, dv),
      m_nfields (num_keys),
      m_fields (static_cast<char **> (mxArray::calloc (m_nfields,
                                      sizeof (char *)))),
      m_data (static_cast<mxArray * *> (mxArray::calloc (m_nfields *
                                        get_number_of_elements (),
                                        sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (bool interleaved, mwSize m, mwSize n, int num_keys,
                  const char **keys)
    : mxArray_matlab (interleaved, false, mxSTRUCT_CLASS, m, n),
      m_nfields (num_keys),
      m_fields (static_cast<char **> (mxArray::calloc (m_nfields,
                                      sizeof (char *)))),
      m_data (static_cast<mxArray * *> (mxArray::calloc (m_nfields *
                                        get_number_of_elements (),
                                        sizeof (mxArray *))))
  {
    init (keys);
  }

private:

  mxArray_struct (const mxArray_struct& val)
    : mxArray_matlab (val), m_nfields (val.m_nfields),
      m_fields (static_cast<char **> (mxArray::malloc (m_nfields
                                      * sizeof (char *)))),
      m_data (static_cast<mxArray * *> (mxArray::malloc (m_nfields *
                                        get_number_of_elements ()
                                        * sizeof (mxArray *))))
  {
    for (int i = 0; i < m_nfields; i++)
      m_fields[i] = mxArray::strsave (val.m_fields[i]);

    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel * m_nfields; i++)
      {
        mxArray *ptr = val.m_data[i];
        m_data[i] = (ptr ? ptr->dup () : nullptr);
      }
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_struct& operator = (const mxArray_struct& val) = delete;

  void init (const char **keys)
  {
    for (int i = 0; i < m_nfields; i++)
      m_fields[i] = mxArray::strsave (keys[i]);
  }

  mxArray_base * dup () const { return new mxArray_struct (*this); }

  ~mxArray_struct ()
  {
    for (int i = 0; i < m_nfields; i++)
      mxFree (m_fields[i]);

    mxFree (m_fields);

    mwSize ntot = m_nfields * get_number_of_elements ();

    for  (mwIndex i = 0; i < ntot; i++)
      delete m_data[i];

    mxFree (m_data);
  }

  int add_field (const char *key)
  {
    int retval = -1;

    m_nfields++;

    m_fields = static_cast<char **>
               (mxRealloc (m_fields, m_nfields * sizeof (char *)));

    if (m_fields)
      {
        m_fields[m_nfields-1] = mxArray::strsave (key);

        mwSize nel = get_number_of_elements ();

        mwSize ntot = m_nfields * nel;

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
                if (++n == m_nfields)
                  {
                    new_data[j++] = nullptr;
                    n = 0;
                  }
                else
                  new_data[j++] = m_data[k++];
              }

            mxFree (m_data);

            m_data = new_data;

            retval = m_nfields - 1;
          }
      }

    return retval;
  }

  void remove_field (int key_num)
  {
    if (key_num >= 0 && key_num < m_nfields)
      {
        mwSize nel = get_number_of_elements ();

        mwSize ntot = m_nfields * nel;

        int new_nfields = m_nfields - 1;

        char **new_fields = static_cast<char **>
                            (mxArray::malloc (new_nfields * sizeof (char *)));

        mxArray **new_data = static_cast<mxArray **>
                             (mxArray::malloc (new_nfields * nel
                                               * sizeof (mxArray *)));

        for (int i = 0; i < key_num; i++)
          new_fields[i] = m_fields[i];

        for (int i = key_num + 1; i < m_nfields; i++)
          new_fields[i-1] = m_fields[i];

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
                  new_data[j++] = m_data[k++];

                if (++n == m_nfields)
                  n = 0;
              }
          }

        m_nfields = new_nfields;

        mxFree (m_fields);
        mxFree (m_data);

        m_fields = new_fields;
        m_data = new_data;
      }
  }

  mxArray * get_field_by_number (mwIndex index, int key_num) const
  {
    return key_num >= 0 && key_num < m_nfields
           ? m_data[m_nfields * index + key_num] : nullptr;
  }

  void set_field_by_number (mwIndex index, int key_num, mxArray *val);

  int get_number_of_fields () const { return m_nfields; }

  const char * get_field_name_by_number (int key_num) const
  {
    return key_num >= 0 && key_num < m_nfields ? m_fields[key_num] : nullptr;
  }

  int get_field_number (const char *key) const
  {
    int retval = -1;

    for (int i = 0; i < m_nfields; i++)
      {
        if (! strcmp (key, m_fields[i]))
          {
            retval = i;
            break;
          }
      }

    return retval;
  }

  void * get_data () const { return m_data; }

  void set_data (void *data) { m_data = static_cast<mxArray **> (data); }

  octave_value as_octave_value () const
  {
    const dim_vector& dv = dims_to_dim_vector ();

    string_vector keys (m_fields, m_nfields);

    octave_map m (dv);

    mwSize ntot = m_nfields * get_number_of_elements ();

    for (int i = 0; i < m_nfields; i++)
      {
        Cell c (dv);

        octave_value *p = c.rwdata ();

        mwIndex k = 0;
        for (mwIndex j = i; j < ntot; j += m_nfields)
          p[k++] = mxArray::as_octave_value (m_data[j]);

        m.assign (keys[i], c);
      }

    return m;
  }

private:

  int m_nfields;

  char **m_fields;

  mxArray **m_data;
};

// Matlab-style cell arrays.

class mxArray_cell : public mxArray_matlab
{
public:

  mxArray_cell () = delete;

  mxArray_cell (bool interleaved, mwSize ndims, const mwSize *dims)
    : mxArray_matlab (interleaved, false, mxCELL_CLASS, ndims, dims),
      m_data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (), sizeof (mxArray *))))
  { }

  mxArray_cell (bool interleaved, const dim_vector& dv)
    : mxArray_matlab (interleaved, false, mxCELL_CLASS, dv),
      m_data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (), sizeof (mxArray *))))
  { }

  mxArray_cell (bool interleaved, mwSize m, mwSize n)
    : mxArray_matlab (interleaved, false, mxCELL_CLASS, m, n),
      m_data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (), sizeof (mxArray *))))
  { }

private:

  mxArray_cell (const mxArray_cell& val)
    : mxArray_matlab (val),
      m_data (static_cast<mxArray **> (mxArray::malloc (get_number_of_elements () * sizeof (mxArray *))))
  {
    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      {
        mxArray *ptr = val.m_data[i];
        m_data[i] = (ptr ? ptr->dup () : nullptr);
      }
  }

public:

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_cell& operator = (const mxArray_cell&) = delete;

  mxArray_base * dup () const { return new mxArray_cell (*this); }

  ~mxArray_cell ()
  {
    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      delete m_data[i];

    mxFree (m_data);
  }

  mxArray * get_cell (mwIndex idx) const
  {
    return idx >= 0 && idx < get_number_of_elements () ? m_data[idx] : nullptr;
  }

  void set_cell (mwIndex idx, mxArray *val);

  void * get_data () const { return m_data; }

  void set_data (void *data) { m_data = static_cast<mxArray **> (data); }

  octave_value as_octave_value () const
  {
    const dim_vector& dv = dims_to_dim_vector ();

    Cell c (dv);

    mwSize nel = get_number_of_elements ();

    octave_value *p = c.rwdata ();

    for (mwIndex i = 0; i < nel; i++)
      p[i] = mxArray::as_octave_value (m_data[i]);

    return c;
  }

private:

  mxArray **m_data;
};

// ------------------------------------------------------------------

mxArray::mxArray (bool interleaved, const octave_value& ov)
  : m_rep (create_rep (interleaved, ov)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mwSize ndims,
                  const mwSize *dims, mxComplexity flag, bool init)
  : m_rep (create_rep (interleaved, id, ndims, dims, flag, init)),
    m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, const dim_vector& dv,
                  mxComplexity flag)
  : m_rep (create_rep (interleaved, id, dv, flag)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mwSize m, mwSize n,
                  mxComplexity flag, bool init)
  : m_rep (create_rep (interleaved, id, m, n, flag, init)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, double val)
  : m_rep (create_rep (interleaved, id, val)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mxLogical val)
  : m_rep (create_rep (interleaved, id, val)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, const char *str)
  : m_rep (create_rep (interleaved, str)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize m, const char **str)
  : m_rep (create_rep (interleaved, m, str)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mxClassID id, mwSize m, mwSize n,
                  mwSize nzmax, mxComplexity flag)
  : m_rep (create_rep (interleaved, id, m, n, nzmax, flag)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize ndims, const mwSize *dims,
                  int num_keys,
                  const char **keys)
  : m_rep (new mxArray_struct (interleaved, ndims, dims, num_keys, keys)),
    m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, const dim_vector& dv, int num_keys,
                  const char **keys)
  : m_rep (new mxArray_struct (interleaved, dv, num_keys, keys)),
    m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize m, mwSize n, int num_keys,
                  const char **keys)
  : m_rep (new mxArray_struct (interleaved, m, n, num_keys, keys)),
    m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize ndims, const mwSize *dims)
  : m_rep (new mxArray_cell (interleaved, ndims, dims)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, const dim_vector& dv)
  : m_rep (new mxArray_cell (interleaved, dv)), m_name (nullptr)
{ }

mxArray::mxArray (bool interleaved, mwSize m, mwSize n)
  : m_rep (new mxArray_cell (interleaved, m, n)), m_name (nullptr)
{ }

mxArray::~mxArray ()
{
  mxFree (m_name);

  delete m_rep;
}

void
mxArray::set_name (const char *name)
{
  mxFree (m_name);
  m_name = mxArray::strsave (name);
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
mxArray::as_octave_value () const
{
  return m_rep->as_octave_value ();
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
  if (interleaved)
    return new mxArray_interleaved_full (id, ndims, dims, flag, init);
  else
    return new mxArray_separate_full (id, ndims, dims, flag, init);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, const dim_vector& dv,
                     mxComplexity flag)
{
  if (interleaved)
    return new mxArray_interleaved_full (id, dv, flag);
  else
    return new mxArray_separate_full (id, dv, flag);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mwSize m, mwSize n,
                     mxComplexity flag, bool init)
{
  if (interleaved)
    return new mxArray_interleaved_full (id, m, n, flag, init);
  else
    return new mxArray_separate_full (id, m, n, flag, init);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, double val)
{
  if (interleaved)
    return new mxArray_interleaved_full (id, val);
  else
    return new mxArray_separate_full (id, val);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mxLogical val)
{
  if (interleaved)
    return new mxArray_interleaved_full (id, val);
  else
    return new mxArray_separate_full (id, val);
}

mxArray_base *
mxArray::create_rep (bool interleaved, const char *str)
{
  if (interleaved)
    return new mxArray_interleaved_full (str);
  else
    return new mxArray_separate_full (str);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mwSize m, const char **str)
{
  if (interleaved)
    return new mxArray_interleaved_full (m, str);
  else
    return new mxArray_separate_full (m, str);
}

mxArray_base *
mxArray::create_rep (bool interleaved, mxClassID id, mwSize m, mwSize n,
                     mwSize nzmax, mxComplexity flag)
{
  if (interleaved)
    return new mxArray_interleaved_sparse (id, m, n, nzmax, flag);
  else
    return new mxArray_separate_sparse (id, m, n, nzmax, flag);
}

void
mxArray::maybe_mutate () const
{
  if (m_rep->is_octave_value ())
    {
      // The mutate function returns a pointer to a complete new
      // mxArray object (or 0, if no mutation happened).  We just want
      // to replace the existing rep with the rep from the new object.

      mxArray *new_val = m_rep->mutate ();

      if (new_val)
        {
          delete m_rep;
          m_rep = new_val->m_rep;
          new_val->m_rep = nullptr;
          delete new_val;
        }
    }
}

// ------------------------------------------------------------------

// A class to manage calls to MEX functions.  Mostly deals with memory
// management.

mex::~mex ()
{
  // We can't use mex::free here because it modifies memlist.
  while (! m_memlist.empty ())
    {
      auto p = m_memlist.begin ();
      xfree (*p);
      m_memlist.erase (p);
    }

  // We can't use mex::free_value here because it modifies arraylist.
  while (! m_arraylist.empty ())
    {
      auto p = m_arraylist.begin ();
      delete *p;
      m_arraylist.erase (p);
    }

  if (! (m_memlist.empty () && m_arraylist.empty ()))
    error ("mex: %s: cleanup failed", function_name ());

  mxFree (m_fname);
}

const char *
mex::function_name () const
{
  if (! m_fname)
    {
      octave::tree_evaluator& tw = octave::__get_evaluator__ ();

      octave_function *fcn = tw.current_function ();

      if (fcn)
        {
          std::string nm = fcn->name ();
          m_fname = mxArray::strsave (nm.c_str ());
        }
      else
        m_fname = mxArray::strsave ("unknown");
    }

  return m_fname;
}

// Allocate memory.
void *
mex::malloc_unmarked (std::size_t n)
{
  void *ptr = xmalloc (n);

  if (! ptr)
    {
      // FIXME: could use "octave_new_handler();" instead
      error ("%s: failed to allocate %zd bytes of memory",
             function_name (), n);
    }

  global_mark (ptr);

  return ptr;
}

// Reallocate a pointer obtained from malloc or calloc.
// If the pointer is NULL, allocate using malloc.
// We don't need an "unmarked" version of this.
void *
mex::realloc (void *ptr, std::size_t n)
{
  void *v;

  if (ptr)
    {
      auto p_local = m_memlist.find (ptr);
      auto p_global = s_global_memlist.find (ptr);

      v = xrealloc (ptr, n);

      if (v)
        {
          if (p_local != m_memlist.end ())
            {
              m_memlist.erase (p_local);
              m_memlist.insert (v);
            }

          if (p_global != s_global_memlist.end ())
            {
              s_global_memlist.erase (p_global);
              s_global_memlist.insert (v);
            }
        }
    }
  else
    v = malloc (n);

  return v;
}

// Free a pointer obtained from malloc or calloc.
void mex::free (void *ptr)
{
  if (ptr)
    {
      unmark (ptr);

      auto p = s_global_memlist.find (ptr);

      if (p != s_global_memlist.end ())
        {
          s_global_memlist.erase (p);

          xfree (ptr);
        }
      else
        {
          p = m_foreign_memlist.find (ptr);

          if (p != m_foreign_memlist.end ())
            m_foreign_memlist.erase (p);
#if defined (DEBUG)
          else
            warning ("mxFree: skipping memory not allocated by mxMalloc, mxCalloc, or mxRealloc");
#endif
        }
    }
}

// List of memory resources we allocated.
std::set<void *> mex::s_global_memlist;

// Current context.
OCTINTERP_API mex *mex_context = nullptr;

void * mxRealloc (void *ptr, std::size_t size)
{
  return (mex_context
          ? mex_context->realloc (ptr, size) : xrealloc (ptr, size));
}

void mxFree (void *ptr)
{
  if (mex_context)
    mex_context->free (ptr);
  else
    xfree (ptr);
}

void *
mxArray::malloc (std::size_t n)
{
  return mex_context ? mex_context->malloc_unmarked (n) : xmalloc (n);
}

void *
mxArray::calloc (std::size_t n, std::size_t t)
{
  return mex_context ? mex_context->calloc_unmarked (n, t) : ::calloc (n, t);
}

void *
mxArray::alloc (bool init, std::size_t n, std::size_t t)
{
  return init ? mxArray::calloc (n, t) : mxArray::malloc (n * t);
}

static inline void *
maybe_mark_foreign (void *ptr)
{
  if (mex_context)
    mex_context->mark_foreign (ptr);

  return ptr;
}

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)

static inline void
maybe_disown_ptr (void *ptr)
{
  if (mex_context)
    {
      mex_context->unmark (ptr);
      mex_context->global_unmark (ptr);
      mex_context->mark_foreign (ptr);
    }
}

#endif

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
  if (key_num >= 0 && key_num < m_nfields)
    m_data[m_nfields * index + key_num] = maybe_unmark_array (val);
}

void
mxArray_cell::set_cell (mwIndex idx, mxArray *val)
{
  if (idx >= 0 && idx < get_number_of_elements ())
    m_data[idx] = maybe_unmark_array (val);
}

// ------------------------------------------------------------------

typedef void (*cmex_fptr) (int nlhs, mxArray **plhs, int nrhs, mxArray **prhs);
typedef F77_RET_T (*fmex_fptr) (F77_INT& nlhs, mxArray **plhs,
                                F77_INT& nrhs, mxArray **prhs);

OCTAVE_BEGIN_NAMESPACE(octave)

octave_value_list
call_mex (octave_mex_function& mex_fcn, const octave_value_list& args,
          int nargout_arg)
{
  octave_quit ();

  // Use at least 1 for nargout since even for zero specified args,
  // still want to be able to return an ans.

  int nargout = nargout_arg;

  int nargin = args.length ();
  OCTAVE_LOCAL_BUFFER (mxArray *, argin, nargin);
  for (int i = 0; i < nargin; i++)
    argin[i] = nullptr;

  int nout = (nargout == 0 ? 1 : nargout);
  OCTAVE_LOCAL_BUFFER (mxArray *, argout, nout);
  for (int i = 0; i < nout; i++)
    argout[i] = nullptr;

  // Save old mex pointer.
  octave::unwind_protect_var<mex *> restore_var (mex_context);

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

  // If using std::pmr::memory_resource object to manage memory, pass
  // default allocator here because we are done with these mxArray
  // values and want Octave to delete them.

  for (int i = 0; i < nargout; i++)
    retval(i) = mxArray::as_octave_value (argout[i], false);

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
