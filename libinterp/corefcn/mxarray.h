////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
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

/*

Part of this code was originally distributed as part of Octave Forge under
the following terms:

Author: Paul Kienzle
I grant this code to the public domain.
2001-03-22

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

*/

#if ! defined (octave_mxarray_h)
#define octave_mxarray_h 1

#include "octave-config.h"

#include "mxtypes.h"

#if ! defined (MXARRAY_TYPEDEFS_ONLY)

#include <cstring>
#include "error.h"

class octave_value;
class dim_vector;

#define DO_MUTABLE_METHOD(RET_T, METHOD_CALL)   \
  RET_T retval = m_rep->METHOD_CALL;            \
                                                \
  if (m_rep->mutation_needed ())                \
    {                                           \
      maybe_mutate ();                          \
      retval = m_rep->METHOD_CALL;              \
    }                                           \
                                                \
  return retval

#define DO_VOID_MUTABLE_METHOD(METHOD_CALL)     \
  m_rep->METHOD_CALL;                           \
                                                \
  if (m_rep->mutation_needed ())                \
    {                                           \
      maybe_mutate ();                          \
      m_rep->METHOD_CALL;                       \
    }

class OCTINTERP_API mxArray;

// A class to provide the default implementation of some of the
// virtual functions declared in the mxArray class.

class OCTINTERP_API mxArray_base
{
protected:

  OCTINTERP_API mxArray_base (bool interleaved);

public:

  virtual mxArray_base * dup (void) const = 0;

  virtual mxArray * as_mxArray (void) const { return nullptr; }

  virtual ~mxArray_base (void) = default;

  virtual bool is_octave_value (void) const { return false; }

  virtual int iscell (void) const = 0;

  virtual int is_char (void) const = 0;

  virtual int is_class (const char *name_arg) const
  {
    int retval = 0;

    const char *cname = get_class_name ();

    if (cname && name_arg)
      retval = ! strcmp (cname, name_arg);

    return retval;
  }

  virtual int is_complex (void) const = 0;

  virtual int is_double (void) const = 0;

  virtual int is_function_handle (void) const = 0;

  virtual int is_int16 (void) const = 0;

  virtual int is_int32 (void) const = 0;

  virtual int is_int64 (void) const = 0;

  virtual int is_int8 (void) const = 0;

  virtual int is_logical (void) const = 0;

  virtual int is_numeric (void) const = 0;

  virtual int is_single (void) const = 0;

  virtual int is_sparse (void) const = 0;

  virtual int is_struct (void) const = 0;

  virtual int is_uint16 (void) const = 0;

  virtual int is_uint32 (void) const = 0;

  virtual int is_uint64 (void) const = 0;

  virtual int is_uint8 (void) const = 0;

  virtual int is_logical_scalar (void) const
  {
    return is_logical () && get_number_of_elements () == 1;
  }

  virtual int is_logical_scalar_true (void) const = 0;

  virtual mwSize get_m (void) const = 0;

  virtual mwSize get_n (void) const = 0;

  virtual mwSize * get_dimensions (void) const = 0;

  virtual mwSize get_number_of_dimensions (void) const = 0;

  virtual void set_m (mwSize m) = 0;

  virtual void set_n (mwSize n) = 0;

  virtual int set_dimensions (mwSize *dims_arg, mwSize ndims_arg) = 0;

  virtual mwSize get_number_of_elements (void) const = 0;

  virtual int isempty (void) const = 0;

  virtual bool is_scalar (void) const = 0;

  virtual mxClassID get_class_id (void) const = 0;

  virtual const char * get_class_name (void) const = 0;

  virtual void set_class_name (const char *name_arg) = 0;

  // The following functions aren't pure virtual because they are only
  // valid for one type.  Making them pure virtual would mean that they
  // have to be implemented for all derived types, and all of those
  // would need to throw errors instead of just doing it once here.

  virtual mxArray *
  get_property (mwIndex /*idx*/, const char * /*pname*/) const
  {
    return nullptr;
  }

  virtual void set_property (mwIndex /*idx*/, const char * /*pname*/,
                             const mxArray * /*pval*/)
  {
    err_invalid_type ("set_property");
  }

  virtual mxArray * get_cell (mwIndex /*idx*/) const
  {
    err_invalid_type ("get_cell");
  }

  virtual void set_cell (mwIndex idx, mxArray *val) = 0;

  virtual double get_scalar (void) const = 0;

  virtual void * get_data (void) const = 0;

  virtual mxDouble * get_doubles (void) const = 0;
  virtual mxSingle * get_singles (void) const = 0;
  virtual mxInt8 * get_int8s (void) const = 0;
  virtual mxInt16 * get_int16s (void) const = 0;
  virtual mxInt32 * get_int32s (void) const = 0;
  virtual mxInt64 * get_int64s (void) const = 0;
  virtual mxUint8 * get_uint8s (void) const = 0;
  virtual mxUint16 * get_uint16s (void) const = 0;
  virtual mxUint32 * get_uint32s (void) const = 0;
  virtual mxUint64 * get_uint64s (void) const = 0;

  virtual mxComplexDouble * get_complex_doubles (void) const = 0;
  virtual mxComplexSingle * get_complex_singles (void) const = 0;
#if 0
  /* We don't have these yet. */
  virtual mxComplexInt8 * get_complex_int8s (void) const = 0;
  virtual mxComplexInt16 * get_complex_int16s (void) const = 0;
  virtual mxComplexInt32 * get_complex_int32s (void) const = 0;
  virtual mxComplexInt64 * get_complex_int64s (void) const = 0;
  virtual mxComplexUint8 * get_complex_uint8s (void) const = 0;
  virtual mxComplexUint16 * get_complex_uint16s (void) const = 0;
  virtual mxComplexUint32 * get_complex_uint32s (void) const = 0;
  virtual mxComplexUint64 * get_complex_uint64s (void) const = 0;
#endif

  virtual void * get_imag_data (void) const = 0;

  virtual void set_data (void *pr) = 0;

  virtual int set_doubles (mxDouble *data) = 0;
  virtual int set_singles (mxSingle *data) = 0;
  virtual int set_int8s (mxInt8 *data) = 0;
  virtual int set_int16s (mxInt16 *data) = 0;
  virtual int set_int32s (mxInt32 *data) = 0;
  virtual int set_int64s (mxInt64 *data) = 0;
  virtual int set_uint8s (mxUint8 *data) = 0;
  virtual int set_uint16s (mxUint16 *data) = 0;
  virtual int set_uint32s (mxUint32 *data) = 0;
  virtual int set_uint64s (mxUint64 *data) = 0;

  virtual int set_complex_doubles (mxComplexDouble *data) = 0;
  virtual int set_complex_singles (mxComplexSingle *data) = 0;
#if 0
  /* We don't have these yet. */
  virtual int set_complex_int8s (mxComplexInt8 *data) = 0;
  virtual int set_complex_int16s (mxComplexInt16 *data) = 0;
  virtual int set_complex_int32s (mxComplexInt32 *data) = 0;
  virtual int set_complex_int64s (mxComplexInt64 *data) = 0;
  virtual int set_complex_uint8s (mxComplexUint8 *data) = 0;
  virtual int set_complex_uint16s (mxComplexUint16 *data) = 0;
  virtual int set_complex_uint32s (mxComplexUint32 *data) = 0;
  virtual int set_complex_uint64s (mxComplexUint64 *data) = 0;
#endif

  virtual void set_imag_data (void *pi) = 0;

  virtual mwIndex * get_ir (void) const = 0;

  virtual mwIndex * get_jc (void) const = 0;

  virtual mwSize get_nzmax (void) const = 0;

  virtual void set_ir (mwIndex *ir) = 0;

  virtual void set_jc (mwIndex *jc) = 0;

  virtual void set_nzmax (mwSize nzmax) = 0;

  virtual int add_field (const char *key) = 0;

  virtual void remove_field (int key_num) = 0;

  virtual mxArray * get_field_by_number (mwIndex index, int key_num) const = 0;

  virtual void
  set_field_by_number (mwIndex index, int key_num, mxArray *val) = 0;

  virtual int get_number_of_fields (void) const = 0;

  virtual const char * get_field_name_by_number (int key_num) const = 0;

  virtual int get_field_number (const char *key) const = 0;

  virtual int get_string (char *buf, mwSize buflen) const = 0;

  virtual char * array_to_string (void) const = 0;

  virtual mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const = 0;

  virtual std::size_t get_element_size (void) const = 0;

  virtual bool mutation_needed (void) const { return false; }

  virtual mxArray * mutate (void) const { return nullptr; }

  virtual octave_value as_octave_value (void) const = 0;

protected:

  mxArray_base (const mxArray_base&) = default;

  std::size_t get_numeric_element_size (std::size_t size) const
  {
    return (m_interleaved
            ? is_complex () ? 2 * size : size
            : size);
  }

  OCTAVE_NORETURN void err_invalid_type (const char *op) const
  {
    error ("%s: invalid type for mxArray::%s", get_class_name (), op);
  }

  //--------

  // If TRUE, we are using interleaved storage for complex numeric arrays.
  bool m_interleaved;

};

// The main interface class.  The representation can be based on an
// octave_value object or a separate object that tries to reproduce
// the semantics of mxArray objects in Matlab more directly.

class mxArray
{
public:

  OCTINTERP_API mxArray (bool interleaved, const octave_value& ov);

  OCTINTERP_API mxArray (bool interleaved, mxClassID id, mwSize ndims,
                         const mwSize *dims, mxComplexity flag = mxREAL,
                         bool init = true);

  OCTINTERP_API mxArray (bool interleaved, mxClassID id, const dim_vector& dv,
                         mxComplexity flag = mxREAL);

  OCTINTERP_API mxArray (bool interleaved, mxClassID id, mwSize m, mwSize n,
                         mxComplexity flag = mxREAL, bool init = true);

  OCTINTERP_API mxArray (bool interleaved, mxClassID id, double val);

  OCTINTERP_API mxArray (bool interleaved, mxClassID id, mxLogical val);

  OCTINTERP_API mxArray (bool interleaved, const char *str);

  OCTINTERP_API mxArray (bool interleaved, mwSize m, const char **str);

  OCTINTERP_API mxArray (bool interleaved, mxClassID id, mwSize m, mwSize n,
                         mwSize nzmax, mxComplexity flag = mxREAL);

  OCTINTERP_API mxArray (bool interleaved, mwSize ndims, const mwSize *dims,
                         int num_keys, const char **keys);

  OCTINTERP_API mxArray (bool interleaved, const dim_vector& dv, int num_keys,
                         const char **keys);

  OCTINTERP_API mxArray (bool interleaved, mwSize m, mwSize n, int num_keys,
                         const char **keys);

  OCTINTERP_API mxArray (bool interleaved, mwSize ndims, const mwSize *dims);

  OCTINTERP_API mxArray (bool interleaved, const dim_vector& dv);

  OCTINTERP_API mxArray (bool interleaved, mwSize m, mwSize n);

  mxArray * dup (void) const
  {
    mxArray *retval = m_rep->as_mxArray ();

    if (retval)
      retval->set_name (m_name);
    else
      {
        mxArray_base *new_rep = m_rep->dup ();

        retval = new mxArray (new_rep, m_name);
      }

    return retval;
  }

  // No copying!

  mxArray (const mxArray&) = delete;

  mxArray& operator = (const mxArray&) = delete;

  OCTINTERP_API ~mxArray (void);

  bool is_octave_value (void) const { return m_rep->is_octave_value (); }

  int iscell (void) const { return m_rep->iscell (); }

  int is_char (void) const { return m_rep->is_char (); }

  int is_class (const char *name_arg) const { return m_rep->is_class (name_arg); }

  int is_complex (void) const { return m_rep->is_complex (); }

  int is_double (void) const { return m_rep->is_double (); }

  int is_function_handle (void) const { return m_rep->is_function_handle (); }

  int is_int16 (void) const { return m_rep->is_int16 (); }

  int is_int32 (void) const { return m_rep->is_int32 (); }

  int is_int64 (void) const { return m_rep->is_int64 (); }

  int is_int8 (void) const { return m_rep->is_int8 (); }

  int is_logical (void) const { return m_rep->is_logical (); }

  int is_numeric (void) const { return m_rep->is_numeric (); }

  int is_single (void) const { return m_rep->is_single (); }

  int is_sparse (void) const { return m_rep->is_sparse (); }

  int is_struct (void) const { return m_rep->is_struct (); }

  int is_uint16 (void) const { return m_rep->is_uint16 (); }

  int is_uint32 (void) const { return m_rep->is_uint32 (); }

  int is_uint64 (void) const { return m_rep->is_uint64 (); }

  int is_uint8 (void) const { return m_rep->is_uint8 (); }

  int is_logical_scalar (void) const { return m_rep->is_logical_scalar (); }

  int is_logical_scalar_true (void) const
  { return m_rep->is_logical_scalar_true (); }

  mwSize get_m (void) const { return m_rep->get_m (); }

  mwSize get_n (void) const { return m_rep->get_n (); }

  mwSize * get_dimensions (void) const { return m_rep->get_dimensions (); }

  mwSize get_number_of_dimensions (void) const
  { return m_rep->get_number_of_dimensions (); }

  void set_m (mwSize m) { DO_VOID_MUTABLE_METHOD (set_m (m)); }

  void set_n (mwSize n) { DO_VOID_MUTABLE_METHOD (set_n (n)); }

  int set_dimensions (mwSize *dims_arg, mwSize ndims_arg)
  { DO_MUTABLE_METHOD (int, set_dimensions (dims_arg, ndims_arg)); }

  mwSize get_number_of_elements (void) const
  { return m_rep->get_number_of_elements (); }

  int isempty (void) const { return get_number_of_elements () == 0; }

  bool is_scalar (void) const { return m_rep->is_scalar (); }

  const char * get_name (void) const { return m_name; }

  OCTINTERP_API void set_name (const char *name);

  mxClassID get_class_id (void) const { return m_rep->get_class_id (); }

  const char * get_class_name (void) const { return m_rep->get_class_name (); }

  mxArray * get_property (mwIndex idx, const char *pname) const
  { return m_rep->get_property (idx, pname); }

  void set_property (mwIndex idx, const char *pname, const mxArray *pval)
  { m_rep->set_property (idx, pname, pval); }

  void set_class_name (const char *name_arg)
  { DO_VOID_MUTABLE_METHOD (set_class_name (name_arg)); }

  mxArray * get_cell (mwIndex idx) const
  { DO_MUTABLE_METHOD (mxArray *, get_cell (idx)); }

  void set_cell (mwIndex idx, mxArray *val)
  { DO_VOID_MUTABLE_METHOD (set_cell (idx, val)); }

  double get_scalar (void) const { return m_rep->get_scalar (); }

  void * get_data (void) const { DO_MUTABLE_METHOD (void *, get_data ()); }

  mxDouble * get_doubles (void) const
  { DO_MUTABLE_METHOD (mxDouble *, get_doubles ()); }

  mxSingle * get_singles (void) const
  { DO_MUTABLE_METHOD (mxSingle *, get_singles ()); }

  mxInt8 * get_int8s (void) const
  { DO_MUTABLE_METHOD (mxInt8 *, get_int8s ()); }

  mxInt16 * get_int16s (void) const
  { DO_MUTABLE_METHOD (mxInt16 *, get_int16s ()); }

  mxInt32 * get_int32s (void) const
  { DO_MUTABLE_METHOD (mxInt32 *, get_int32s ()); }

  mxInt64 * get_int64s (void) const
  { DO_MUTABLE_METHOD (mxInt64 *, get_int64s ()); }

  mxUint8 * get_uint8s (void) const
  { DO_MUTABLE_METHOD (mxUint8 *, get_uint8s ()); }

  mxUint16 * get_uint16s (void) const
  { DO_MUTABLE_METHOD (mxUint16 *, get_uint16s ()); }

  mxUint32 * get_uint32s (void) const
  { DO_MUTABLE_METHOD (mxUint32 *, get_uint32s ()); }

  mxUint64 * get_uint64s (void) const
  { DO_MUTABLE_METHOD (mxUint64 *, get_uint64s ()); }

  mxComplexDouble * get_complex_doubles (void) const
  { DO_MUTABLE_METHOD (mxComplexDouble *, get_complex_doubles ()); }

  mxComplexSingle * get_complex_singles (void) const
  { DO_MUTABLE_METHOD (mxComplexSingle *, get_complex_singles ()); }

#if 0
  /* We don't have these yet. */
  mxComplexInt8 * get_complex_int8s (void) const
  { DO_MUTABLE_METHOD (mxComplexInt8 *, get_complex_int8s ()); }

  mxComplexInt16 * get_complex_int16s (void) const
  { DO_MUTABLE_METHOD (mxComplexInt16 *, get_complex_int16s ()); }

  mxComplexInt32 * get_complex_int32s (void) const
  { DO_MUTABLE_METHOD (mxComplexInt32 *, get_complex_int32s ()); }

  mxComplexInt64 * get_complex_int64s (void) const
  { DO_MUTABLE_METHOD (mxComplexInt64 *, get_complex_int64s ()); }

  mxComplexUint8 * get_complex_uint8s (void) const
  { DO_MUTABLE_METHOD (mxComplexUint8 *, get_complex_uint8s ()); }

  mxComplexUint16 * get_complex_uint16s (void) const
  { DO_MUTABLE_METHOD (mxComplexUint16 *, get_complex_uint16s ()); }

  mxComplexUint32 * get_complex_uint32s (void) const
  { DO_MUTABLE_METHOD (mxComplexUint32 *, get_complex_uint32s ()); }

  mxComplexUint64 * get_complex_uint64s (void) const
  { DO_MUTABLE_METHOD (mxComplexUint64 *, get_complex_uint64s ()); }
#endif

  void * get_imag_data (void) const
  { DO_MUTABLE_METHOD (void *, get_imag_data ()); }

  void set_data (void *pr) { DO_VOID_MUTABLE_METHOD (set_data (pr)); }

  int set_doubles (mxDouble *data)
  { DO_MUTABLE_METHOD (int, set_doubles (data)); }

  int set_singles (mxSingle *data)
  { DO_MUTABLE_METHOD (int, set_singles (data)); }

  int set_int8s (mxInt8 *data)
  { DO_MUTABLE_METHOD (int, set_int8s (data)); }

  int set_int16s (mxInt16 *data)
  { DO_MUTABLE_METHOD (int, set_int16s (data)); }

  int set_int32s (mxInt32 *data)
  { DO_MUTABLE_METHOD (int, set_int32s (data)); }

  int set_int64s (mxInt64 *data)
  { DO_MUTABLE_METHOD (int, set_int64s (data)); }

  int set_uint8s (mxUint8 *data)
  { DO_MUTABLE_METHOD (int, set_uint8s (data)); }

  int set_uint16s (mxUint16 *data)
  { DO_MUTABLE_METHOD (int, set_uint16s (data)); }

  int set_uint32s (mxUint32 *data)
  { DO_MUTABLE_METHOD (int, set_uint32s (data)); }

  int set_uint64s (mxUint64 *data)
  { DO_MUTABLE_METHOD (int, set_uint64s (data)); }

  int set_complex_doubles (mxComplexDouble *data)
  { DO_MUTABLE_METHOD (int, set_complex_doubles (data)); }

  int set_complex_singles (mxComplexSingle *data)
  { DO_MUTABLE_METHOD (int, set_complex_singles (data)); }

#if 0
  /* We don't have these yet. */
  int set_complex_int8s (mxComplexInt8 *data)
  { DO_MUTABLE_METHOD (int, set_complex_int8s (data)); }

  int set_complex_int16s (mxComplexInt16 *data)
  { DO_MUTABLE_METHOD (int, set_complex_int16s (data)); }

  int set_complex_int32s (mxComplexInt32 *data)
  { DO_MUTABLE_METHOD (int, set_complex_int32s (data)); }

  int set_complex_int64s (mxComplexInt64 *data)
  { DO_MUTABLE_METHOD (int, set_complex_int64s (data)); }

  int set_complex_uint8s (mxComplexUint8 *data)
  { DO_MUTABLE_METHOD (int, set_complex_uint8s (data)); }

  int set_complex_uint16s (mxComplexUint16 *data)
  { DO_MUTABLE_METHOD (int, set_complex_uint16s (data)); }

  int set_complex_uint32s (mxComplexUint32 *data)
  { DO_MUTABLE_METHOD (int, set_complex_uint32s (data)); }

  int set_complex_uint64s (mxComplexUint64 *data)
  { DO_MUTABLE_METHOD (int, set_complex_uint64s (data)); }
#endif

  void set_imag_data (void *pi) { DO_VOID_MUTABLE_METHOD (set_imag_data (pi)); }

  mwIndex * get_ir (void) const { DO_MUTABLE_METHOD (mwIndex *, get_ir ()); }

  mwIndex * get_jc (void) const { DO_MUTABLE_METHOD (mwIndex *, get_jc ()); }

  mwSize get_nzmax (void) const { return m_rep->get_nzmax (); }

  void set_ir (mwIndex *ir) { DO_VOID_MUTABLE_METHOD (set_ir (ir)); }

  void set_jc (mwIndex *jc) { DO_VOID_MUTABLE_METHOD (set_jc (jc)); }

  void set_nzmax (mwSize nzmax) { DO_VOID_MUTABLE_METHOD (set_nzmax (nzmax)); }

  int add_field (const char *key) { DO_MUTABLE_METHOD (int, add_field (key)); }

  void remove_field (int key_num)
  { DO_VOID_MUTABLE_METHOD (remove_field (key_num)); }

  mxArray * get_field_by_number (mwIndex index, int key_num) const
  { DO_MUTABLE_METHOD (mxArray *, get_field_by_number (index, key_num)); }

  void set_field_by_number (mwIndex index, int key_num, mxArray *val)
  { DO_VOID_MUTABLE_METHOD (set_field_by_number (index, key_num, val)); }

  int get_number_of_fields (void) const
  { return m_rep->get_number_of_fields (); }

  const char * get_field_name_by_number (int key_num) const
  { DO_MUTABLE_METHOD (const char *, get_field_name_by_number (key_num)); }

  int get_field_number (const char *key) const
  { DO_MUTABLE_METHOD (int, get_field_number (key)); }

  int get_string (char *buf, mwSize buflen) const
  { return m_rep->get_string (buf, buflen); }

  char * array_to_string (void) const { return m_rep->array_to_string (); }

  mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const
  { return m_rep->calc_single_subscript (nsubs, subs); }

  std::size_t get_element_size (void) const
  { return m_rep->get_element_size (); }

  bool mutation_needed (void) const { return m_rep->mutation_needed (); }

  mxArray * mutate (void) const { return m_rep->mutate (); }

  static OCTINTERP_API void * malloc (std::size_t n);

  static OCTINTERP_API void * calloc (std::size_t n, std::size_t t);

  static OCTINTERP_API void * alloc (bool init, std::size_t n, std::size_t t);

  static char * strsave (const char *str)
  {
    char *retval = nullptr;

    if (str)
      {
        mwSize sz = sizeof (mxChar) * (strlen (str) + 1);

        retval = static_cast<char *> (mxArray::malloc (sz));

        if (retval)
          strcpy (retval, str);
      }

    return retval;
  }

  static OCTINTERP_API octave_value
  as_octave_value (const mxArray *ptr, bool null_is_empty = true);

  OCTINTERP_API octave_value as_octave_value (void) const;

private:

  mxArray (mxArray_base *r, const char *n)
    : m_rep (r), m_name (mxArray::strsave (n)) { }

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, const octave_value& ov);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mxClassID id, mwSize ndims,
              const mwSize *dims, mxComplexity flag, bool init);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mxClassID id, const dim_vector& dv,
              mxComplexity flag);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mxClassID id, mwSize m, mwSize n,
              mxComplexity flag, bool init);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mxClassID id, double val);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mxClassID id, mxLogical val);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, const char *str);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mwSize m, const char **str);

  static OCTINTERP_API mxArray_base *
  create_rep (bool interleaved, mxClassID id, mwSize m, mwSize n,
              mwSize nzmax, mxComplexity flag);

  OCTINTERP_API void maybe_mutate (void) const;

  //--------

  mutable mxArray_base *m_rep;

  char *m_name;

};

#undef DO_MUTABLE_METHOD
#undef DO_VOID_MUTABLE_METHOD

#endif
#endif
