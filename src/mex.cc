#include <config.h>

#include <cfloat>
#include <csetjmp>
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cctype>

#include <set>

#include "f77-fcn.h"
#include "lo-ieee.h"

// mxArray must be declared as a class before including mexproto.h.
class mxArray;
#include "Cell.h"
#include "mexproto.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// #define DEBUG 1

static void
xfree (void *ptr)
{
  ::free (ptr);
}

static int
max_str_len (int m, const char **str)
{
  int max_len = 0;

  for (int i = 0; i < m; i++)
    {
      int tmp = strlen (str[i]);

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
		goto done;
	    }

	  retval = 1;
	}
    }

 done:

  return retval;
}

// ------------------------------------------------------------------

// A class to provide the default implemenation of some of the virtual
// functions declared in the mxArray class.

class mxArray_base : public mxArray
{
protected:

  mxArray_base (void) : mxArray (xmxArray ()) { }

public:

  mxArray *clone (void) const = 0;

  ~mxArray_base (void) { }

  bool is_octave_value (void) const { return false; }

  int is_cell (void) const = 0;

  int is_char (void) const = 0;

  int is_class (const char *name_arg) const
  {
    int retval = 0;

    const char *cname = get_class_name ();

    if (cname && name_arg)
      retval = ! strcmp (cname, name_arg);

    return retval;
  }

  int is_complex (void) const = 0;

  int is_double (void) const = 0;

  int is_int16 (void) const = 0;

  int is_int32 (void) const = 0;

  int is_int64 (void) const = 0;

  int is_int8 (void) const = 0;

  int is_logical (void) const = 0;

  int is_numeric (void) const = 0;

  int is_single (void) const = 0;

  int is_sparse (void) const = 0;

  int is_struct (void) const = 0;

  int is_uint16 (void) const = 0;

  int is_uint32 (void) const = 0;

  int is_uint64 (void) const = 0;

  int is_uint8 (void) const = 0;

  int is_logical_scalar (void) const
  {
    return is_logical () && get_number_of_elements () == 1;
  }

  int is_logical_scalar_true (void) const = 0;

  int get_m (void) const = 0;

  int get_n (void) const = 0;

  int *get_dimensions (void) const = 0;

  int get_number_of_dimensions (void) const = 0;

  void set_m (int m) = 0;

  void set_n (int n) = 0;

  void set_dimensions (int *dims_arg, int ndims_arg) = 0;

  int get_number_of_elements (void) const = 0;

  int is_empty (void) const = 0;

  mxClassID get_class_id (void) const = 0;

  const char *get_class_name (void) const = 0;

  void set_class_name (const char *name_arg) = 0;

  mxArray *get_cell (int /*idx*/) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_cell (int idx, mxArray *val) = 0;

  void *get_data (void) const = 0;

  void *get_imag_data (void) const = 0;

  void set_data (void *pr) = 0;

  void set_imag_data (void *pi) = 0;

  int *get_ir (void) const = 0;

  int *get_jc (void) const = 0;

  int get_nzmax (void) const = 0;

  void set_ir (int *ir) = 0;

  void set_jc (int *jc) = 0;

  void set_nzmax (int nzmax) = 0;

  int add_field (const char *key) = 0;

  void remove_field (int key_num) = 0;

  mxArray *get_field_by_number (int index, int key_num) const = 0;

  void set_field_by_number (int index, int key_num, mxArray *val) = 0;

  int get_number_of_fields (void) const = 0;

  const char *get_field_name_by_number (int key_num) const = 0;

  int get_field_number (const char *key) const = 0;

  int get_string (char *buf, int buflen) const = 0;

  char *array_to_string (void) const = 0;

  int calc_single_subscript (int nsubs, int *subs) const = 0;

  int get_element_size (void) const = 0;

  bool mutation_needed (void) const { return false; }

  mxArray *mutate (void) const { return 0; }

protected:

  octave_value as_octave_value (void) const = 0;

  mxArray_base (const mxArray_base&) : mxArray (xmxArray ()) { }

  void invalid_type_error (void) const
  {
    error ("invalid type for operation");
  }

  void error (const char *msg) const
  {
    // FIXME
    ::error ("%s", msg);
  }
};

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

class mxArray_octave_value : public mxArray_base
{
public:

  mxArray_octave_value (const octave_value& ov)
    : mxArray_base (), val (ov), mutate_flag (false),
      id (mxUNKNOWN_CLASS), class_name (0), ndims (-1), dims (0) { }

  mxArray *clone (void) const { return new mxArray_octave_value (*this); }

  ~mxArray_octave_value (void)
  {
    mxFree (class_name);
    mxFree (dims);
  }

  bool is_octave_value (void) const { return true; }

  int is_cell (void) const { return val.is_cell (); }

  int is_char (void) const { return val.is_string (); }

  int is_complex (void) const { return val.is_complex_type (); }

  int is_double (void) const { return val.is_double_type (); }

  int is_int16 (void) const { return val.is_int16_type (); }

  int is_int32 (void) const { return val.is_int32_type (); }

  int is_int64 (void) const { return val.is_int64_type (); }

  int is_int8 (void) const { return val.is_int8_type (); }

  int is_logical (void) const { return val.is_bool_type (); }

  int is_numeric (void) const { return val.is_numeric_type (); }

  int is_single (void) const { return val.is_single_type (); }

  int is_sparse (void) const { return val.is_sparse_type (); }

  int is_struct (void) const { return val.is_map (); }

  int is_uint16 (void) const { return val.is_uint16_type (); }

  int is_uint32 (void) const { return val.is_uint32_type (); }

  int is_uint64 (void) const { return val.is_uint64_type (); }

  int is_uint8 (void) const { return val.is_uint8_type (); }

  int is_range (void) const { return val.is_range (); }

  int is_real_type (void) const { return val.is_real_type (); }

  int is_logical_scalar_true (void) const
  {
    return (is_logical_scalar () && val.is_true ());
  }

  int get_m (void) const { return val.rows (); }

  int get_n (void) const 
  {
    int n = 1;

    // Force dims and ndims to be cached.
    get_dimensions();

    for (int i = ndims - 1; i > 0; i--)
      n *= dims[i];

    return n;
  }

  int *get_dimensions (void) const
  {
    if (! dims)
      {
	// Force ndims to be cached.
	get_number_of_dimensions ();

	dims = static_cast<int *> (malloc (ndims * sizeof (int)));

	dim_vector dv = val.dims ();

	for (int i = 0; i < ndims; i++)
	  dims[i] = dv(i);
      }

    return dims;
  }

  int get_number_of_dimensions (void) const
  {
    if (ndims < 0)
      ndims = val.ndims ();

    return ndims;
  }

  void set_m (int /*m*/) { panic_impossible (); }

  void set_n (int /*n*/) { panic_impossible (); }

  void set_dimensions (int */*dims_arg*/, int /*ndims_arg*/)
  {
    panic_impossible ();
  }

  int get_number_of_elements (void) const { return val.numel (); }

  int is_empty (void) const { return val.is_empty (); }

  mxClassID get_class_id (void) const
  {
    id = mxUNKNOWN_CLASS;

    std::string cn = val.class_name ();

    if (cn == "cell")
      id = mxCELL_CLASS;
    else if (cn == "struct")
      id = mxSTRUCT_CLASS;
    else if (cn == "logical")
      id = mxLOGICAL_CLASS;
    else if (cn == "char")
      id = mxCHAR_CLASS;
    else if (cn == "double")
      id = mxDOUBLE_CLASS;
    else if (cn == "sparse")
      {
	if (val.is_bool_type ())
	  id = mxLOGICAL_CLASS;
	else
	  id = mxDOUBLE_CLASS;
      }
    else if (cn == "single")
      id = mxSINGLE_CLASS;
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
    else if (cn == "function handle")
      id = mxFUNCTION_CLASS;

    return id;
  }

  const char *get_class_name (void) const
  {
    if (! class_name)
      {
	std::string s = val.class_name ();
	class_name = strsave (s.c_str ());
      }

    return class_name;
  }

  // Not allowed.
  void set_class_name (const char */*name_arg*/) { panic_impossible (); }

  mxArray *get_cell (int /*idx*/) const
  {
    request_mutation ();
    return 0;
  }

  // Not allowed.
  void set_cell (int /*idx*/, mxArray */*val*/) { panic_impossible (); }

  void *get_data (void) const
  {
    void *retval = 0;

    if (is_char ()
	|| (is_numeric () && is_real_type () && ! is_range ()))
      retval = val.mex_get_data ();
    else
      request_mutation ();

    return retval;
  }

  void *get_imag_data (void) const
  {
    void *retval = 0;

    if (is_numeric () && is_real_type ())
      retval = 0;
    else
      request_mutation ();

    return retval;
  }

  // Not allowed.
  void set_data (void */*pr*/) { panic_impossible (); }

  // Not allowed.
  void set_imag_data (void */*pi*/) { panic_impossible (); }

  int *get_ir (void) const
  {
#if SIZEOF_OCTAVE_IDX_TYPE == SIZEOF_INT
    return val.mex_get_ir ();
#else
    request_mutation ();
    return 0;
#endif
  }

  int *get_jc (void) const
  {
#if SIZEOF_OCTAVE_IDX_TYPE == SIZEOF_INT
    return val.mex_get_jc ();
#else
    request_mutation ();
    return 0;
#endif
  }

  int get_nzmax (void) const { return val.nzmax (); }

  // Not allowed.
  void set_ir (int */*ir*/) { panic_impossible (); }

  // Not allowed.
  void set_jc (int */*jc*/) { panic_impossible (); }

  // Not allowed.
  void set_nzmax (int /*nzmax*/) { panic_impossible (); }

  // Not allowed.
  int add_field (const char */*key*/)
  {
    panic_impossible ();
    return -1;
  }

  // Not allowed.
  void remove_field (int /*key_num*/) { panic_impossible (); }

  mxArray *get_field_by_number (int /*index*/, int /*key_num*/) const
  {
    request_mutation ();
    return 0;
  }

  // Not allowed.
  void set_field_by_number (int /*index*/, int /*key_num*/, mxArray */*val*/)
  {
    panic_impossible ();
  }

  int get_number_of_fields (void) const { return val.nfields (); }

  const char *get_field_name_by_number (int /*key_num*/) const
  {
    request_mutation ();
    return 0;
  }

  int get_field_number (const char */*key*/) const
  {
    request_mutation ();
    return 0;
  }

  int get_string (char *buf, int buflen) const
  {
    int retval = 1;

    int nel = get_number_of_elements ();

    if (val.is_string () && nel < buflen)
      {
	charNDArray tmp = val.char_array_value ();

	const char *p = tmp.data ();

	for (int i = 0; i < buflen; i++)
	  buf[i] = p[i];

	buf[nel] = 0;

	retval = 0;
      }

    return retval;
  }

  char *array_to_string (void) const
  {
    // FIXME -- this is suposed to handle multi-byte character
    // strings.

    char *buf = 0;

    if (val.is_string ())
      {
	int nel = get_number_of_elements ();

	buf = static_cast<char *> (malloc (nel + 1));

	if (buf)
	  {
	    charNDArray tmp = val.char_array_value ();

	    const char *p = tmp.data ();

	    for (int i = 0; i < nel; i++)
	      buf[i] = p[i];

	    buf[nel] = '\0';
	  }
      }

    return buf;
  }

  int calc_single_subscript (int nsubs, int *subs) const
  {
    int retval = 0;

    // Force ndims, dims to be cached.
    get_dimensions ();

    int n = nsubs <= ndims ? nsubs : ndims;

    while (--n > 0)
      retval = retval * dims[n] + subs[n];

    return retval;
  }

  int get_element_size (void) const
  {
    // Force id to be cached.
    get_class_id ();

    switch (id)
      {
      case mxCELL_CLASS: return sizeof (mxArray *);
      case mxSTRUCT_CLASS: return sizeof (mxArray *);
      case mxLOGICAL_CLASS: return sizeof (mxLogical);
      case mxCHAR_CLASS: return sizeof (mxChar);
      case mxDOUBLE_CLASS: return sizeof (double);
      case mxSINGLE_CLASS: return sizeof (float);
      case mxINT8_CLASS: return 1;
      case mxUINT8_CLASS: return 1;
      case mxINT16_CLASS: return 2;
      case mxUINT16_CLASS: return 2;
      case mxINT32_CLASS: return 4;
      case mxUINT32_CLASS: return 4;
      case mxINT64_CLASS: return 8;
      case mxUINT64_CLASS: return 8;
      case mxFUNCTION_CLASS: return 0;
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

  mxArray *mutate (void) const { return val.as_mxArray (); }

protected:

  octave_value as_octave_value (void) const { return val; }

  mxArray_octave_value (const mxArray_octave_value& arg)
    : mxArray_base (arg), val (arg.val), mutate_flag (arg.mutate_flag),
      id (arg.id), class_name (strsave (arg.class_name)), ndims (arg.ndims),
      dims (ndims > 0 ? static_cast<int *> (malloc (ndims * sizeof (int))) : 0)
  {
    if (dims)
      {
	for (int i = 0; i < ndims; i++)
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
  mutable int ndims;
  mutable int *dims;
};

// The base class for the Matlab-style representation, used to handle
// things that are common to all Matlab-style objects.

class mxArray_matlab : public mxArray_base
{
protected:

  mxArray_matlab (mxClassID id_arg = mxUNKNOWN_CLASS)
    : mxArray_base (), class_name (0), id (id_arg), ndims (0), dims (0) { }

  mxArray_matlab (mxClassID id_arg, int ndims_arg, const int *dims_arg)
    : mxArray_base (), class_name (0), id (id_arg),
      ndims (ndims_arg < 2 ? 2 : ndims_arg),
      dims (static_cast<int *> (malloc (ndims * sizeof (int))))
  {
    if (ndims_arg < 2)
      {
	dims[0] = 1;
	dims[1] = 1;
      }

    for (int i = 0; i < ndims_arg; i++)
      dims[i] = dims_arg[i];

    for (int i = ndims - 1; i > 1; i--)
      {
	if (dims[i] == 1)
	  ndims--;
	else
	  break;
      }
  }

  mxArray_matlab (mxClassID id_arg, const dim_vector& dv)
    : mxArray_base (), class_name (0), id (id_arg),
      ndims (dv.length ()),
      dims (static_cast<int *> (malloc (ndims * sizeof (int))))
  {
    for (int i = 0; i < ndims; i++)
      dims[i] = dv(i);

    for (int i = ndims - 1; i > 1; i--)
      {
	if (dims[i] == 1)
	  ndims--;
	else
	  break;
      }
  }

  mxArray_matlab (mxClassID id_arg, int m, int n)
    : mxArray_base (), class_name (0), id (id_arg), ndims (2),
      dims (static_cast<int *> (malloc (ndims * sizeof (int))))
  {
    dims[0] = m;
    dims[1] = n;
  }

public:

  ~mxArray_matlab (void)
  {
    mxFree (class_name);
    mxFree (dims);
  }

  int is_cell (void) const { return id == mxCELL_CLASS; }

  int is_char (void) const { return id == mxCHAR_CLASS; }

  int is_complex (void) const { return 0; }

  int is_double (void) const { return id == mxDOUBLE_CLASS; }

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

  int get_m (void) const { return dims[0]; }

  int get_n (void) const
  {
    int n = 1;

    for (int i = ndims - 1 ; i > 0 ; i--)
      n *= dims[i];

    return n;
  }

  int *get_dimensions (void) const { return dims; }

  int get_number_of_dimensions (void) const { return ndims; }

  void set_m (int m) { dims[0] = m; }

  void set_n (int n) { dims[1] = n; }

  void set_dimensions (int *dims_arg, int ndims_arg)
  {
    dims = dims_arg;
    ndims = ndims_arg;
  }

  int get_number_of_elements (void) const
  {
    int retval = dims[0];

    for (int i = 1; i < ndims; i++)
      retval *= dims[i];

    return retval;
  }

  int is_empty (void) const { return get_number_of_elements () == 0; }

  mxClassID get_class_id (void) const { return id; }

  const char *get_class_name (void) const
  {
    switch (id)
      {
      case mxCELL_CLASS: return "cell";
      case mxSTRUCT_CLASS: return "struct";
      case mxLOGICAL_CLASS: return "logical";
      case mxCHAR_CLASS: return "char";
      case mxDOUBLE_CLASS: return "double";
      case mxSINGLE_CLASS: return "single";
      case mxINT8_CLASS: return "int8";
      case mxUINT8_CLASS: return "uint8";
      case mxINT16_CLASS: return "int16";
      case mxUINT16_CLASS: return "uint16";
      case mxINT32_CLASS: return "int32";
      case mxUINT32_CLASS: return "uint32";
      case mxINT64_CLASS: return "int64";
      case mxUINT64_CLASS: return "uint64";
      case mxFUNCTION_CLASS: return "function handle";
      default: return "unknown";
      }
  }

  void set_class_name (const char *name_arg)
  {
    mxFree (class_name);
    class_name = static_cast<char *> (malloc (strlen (name_arg) + 1));
    strcpy (class_name, name_arg);
  }

  mxArray *get_cell (int /*idx*/) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_cell (int /*idx*/, mxArray */*val*/)
  {
    invalid_type_error ();
  }

  void *get_data (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void *get_imag_data (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_data (void */*pr*/)
  {
    invalid_type_error ();
  }

  void set_imag_data (void */*pi*/)
  {
    invalid_type_error ();
  }

  int *get_ir (void) const
  {
    invalid_type_error ();
    return 0;
  }

  int *get_jc (void) const
  {
    invalid_type_error ();
    return 0;
  }

  int get_nzmax (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_ir (int */*ir*/)
  {
    invalid_type_error ();
  }

  void set_jc (int */*jc*/)
  {
    invalid_type_error ();
  }

  void set_nzmax (int /*nzmax*/)
  {
    invalid_type_error ();
  }

  int add_field (const char */*key*/)
  {
    invalid_type_error ();
    return -1;
  }

  void remove_field (int /*key_num*/)
  {
    invalid_type_error ();
  }

  mxArray *get_field_by_number (int /*index*/, int /*key_num*/) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_field_by_number (int /*index*/, int /*key_num*/, mxArray */*val*/)
  {
    invalid_type_error ();
  }

  int get_number_of_fields (void) const
  {
    invalid_type_error ();
    return 0;
  }

  const char *get_field_name_by_number (int /*key_num*/) const
  {
    invalid_type_error ();
    return 0;
  }

  int get_field_number (const char */*key*/) const
  {
    return -1;
  }

  int get_string (char */*buf*/, int /*buflen*/) const
  {
    invalid_type_error ();
    return 0;
  }

  char *array_to_string (void) const
  {
    invalid_type_error ();
    return 0;
  }

  int calc_single_subscript (int nsubs, int *subs) const
  {
    int retval = 0;

    int n = nsubs <= ndims ? nsubs : ndims;

    while (--n > 0)
      retval = retval * dims[n] + subs[n];

    return retval;
  }

  int get_element_size (void) const
  {
    switch (id)
      {
      case mxCELL_CLASS: return sizeof (mxArray *);
      case mxSTRUCT_CLASS: return sizeof (mxArray *);
      case mxLOGICAL_CLASS: return sizeof (mxLogical);
      case mxCHAR_CLASS: return sizeof (mxChar);
      case mxDOUBLE_CLASS: return sizeof (double);
      case mxSINGLE_CLASS: return sizeof (float);
      case mxINT8_CLASS: return 1;
      case mxUINT8_CLASS: return 1;
      case mxINT16_CLASS: return 2;
      case mxUINT16_CLASS: return 2;
      case mxINT32_CLASS: return 4;
      case mxUINT32_CLASS: return 4;
      case mxINT64_CLASS: return 8;
      case mxUINT64_CLASS: return 8;
      case mxFUNCTION_CLASS: return 0;
      default: return 0;
      }    
  }

protected:

  mxArray_matlab (const mxArray_matlab& val)
    : mxArray_base (val), class_name (strsave (val.class_name)),
      id (val.id), ndims (val.ndims),
      dims (static_cast<int *> (malloc (ndims * sizeof (int))))
  {
    for (int i = 0; i < ndims; i++)
      dims[i] = val.dims[i];
  }

  dim_vector
  dims_to_dim_vector (void) const
  {
    int nd = get_number_of_dimensions ();

    int *d = get_dimensions ();

    dim_vector dv;
    dv.resize (nd);

    for (int i = 0; i < nd; i++)
      dv(i) = d[i];

    return dv;
  }

private:

  char *class_name;

  mxClassID id;

  int ndims;
  int *dims;

  void invalid_type_error (void) const
  {
    error ("invalid type for operation");
  }
};

// Matlab-style numeric, character, and logical data.

class mxArray_number : public mxArray_matlab
{
public:

  mxArray_number (mxClassID id_arg, int ndims_arg, const int *dims_arg,
		  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, ndims_arg, dims_arg),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (flag == mxCOMPLEX ? calloc (get_number_of_elements (), get_element_size ()) : 0) { }

  mxArray_number (mxClassID id_arg, const dim_vector& dv,
		  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, dv),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (flag == mxCOMPLEX ? calloc (get_number_of_elements (), get_element_size ()) : 0) { }

  mxArray_number (mxClassID id_arg, int m, int n, mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, m, n),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (flag == mxCOMPLEX ? calloc (get_number_of_elements (), get_element_size ()) : 0) { }

  mxArray_number (mxClassID id_arg, double val)
    : mxArray_matlab (id_arg, 1, 1),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    double *dpr = static_cast<double *> (pr);
    dpr[0] = val;
  }

  mxArray_number (mxClassID id_arg, mxLogical val)
    : mxArray_matlab (id_arg, 1, 1),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    mxLogical *lpr = static_cast<mxLogical *> (pr);
    lpr[0] = val;
  }

  mxArray_number (const char *str)
    : mxArray_matlab (mxCHAR_CLASS, 1, strlen (str)),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    mxChar *cpr = static_cast<mxChar *> (pr);
    int nel = get_number_of_elements ();
    for (int i = 0; i < nel; i++)
      cpr[i] = str[i];
  }

  mxArray_number (int m, const char **str)
    : mxArray_matlab (mxCHAR_CLASS, m, max_str_len (m, str)),
      pr (calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    mxChar *cpr = static_cast<mxChar *> (pr);
    
    int *dv = get_dimensions ();

    int nc = dv[1];

    for (int j = 0; j < m; j++)
      {
	const char *ptr = str[j];

	int tmp_len = strlen (ptr);

	for (int i = 0; i < tmp_len; i++)
	  cpr[i] = static_cast<mxChar> (ptr[i]);

	for (int i = tmp_len; i < nc; i++)
	  cpr[i] = static_cast<mxChar> (' ');
      }	
  }

  mxArray_number *clone (void) const { return new mxArray_number (*this); }

  ~mxArray_number (void)
  {
    mxFree (pr);
    mxFree (pi);
  }

  int is_complex (void) const { return pi != 0; }

  void *get_data (void) const { return pr; }

  void *get_imag_data (void) const { return pi; }

  void set_data (void *pr_arg) { pr = pr_arg; }

  void set_imag_data (void *pi_arg) { pi = pi_arg; }

  int get_string (char *buf, int buflen) const
  {
    int retval = 1;

    int n = get_number_of_elements ();

    if (n < buflen)
      {
	mxChar *ptr = static_cast<mxChar *> (pr);

	for (int i = 0; i < n; i++)
	  buf[i] = static_cast<char> (ptr[i]);

	buf[n] = 0;
      }

    return retval;
  }

  char *array_to_string (void) const
  {
    // FIXME -- this is suposed to handle multi-byte character
    // strings.

    int nel = get_number_of_elements ();

    char *buf = static_cast<char *> (malloc (nel + 1));

    if (buf)
      {
	mxChar *ptr = static_cast<mxChar *> (pr);

	for (int i = 0; i < nel; i++)
	  buf[i] = static_cast<char> (ptr[i]);

	buf[nel] = '\0';
      }

    return buf;
  }

protected:

  template <typename ELT_T, typename ARRAY_T, typename ARRAY_ELT_T>
  octave_value
  int_to_ov (const dim_vector& dv) const
  {
    octave_value retval;

    int nel = get_number_of_elements ();

    ELT_T *ppr = static_cast<ELT_T *> (pr);

    if (pi)
      error ("complex integer types are not supported");
    else
      {
	ARRAY_T val (dv);

	ARRAY_ELT_T *ptr = val.fortran_vec ();

	for (int i = 0; i < nel; i++)
	  ptr[i] = ppr[i];

	retval = val;
      }

    return retval;
  }

  octave_value as_octave_value (void) const
  {
    octave_value retval;

    dim_vector dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxLOGICAL_CLASS:
	retval = int_to_ov<bool, boolNDArray, bool> (dv);
	break;

      case mxCHAR_CLASS:
	{
	  int nel = get_number_of_elements ();

	  mxChar *ppr = static_cast<mxChar *> (pr);

	  charNDArray val (dv);

	  char *ptr = val.fortran_vec ();

	  for (int i = 0; i < nel; i++)
	    ptr[i] = static_cast<char> (ppr[i]);

	  retval = octave_value (val, true, '\'');
	}
	break;

      case mxSINGLE_CLASS:
	error ("single precision data type not supported");
	break;

      case mxDOUBLE_CLASS:
	{
	  int nel = get_number_of_elements ();

	  double *ppr = static_cast<double *> (pr);

	  if (pi)
	    {
	      ComplexNDArray val (dv);

	      Complex *ptr = val.fortran_vec ();

	      double *ppi = static_cast<double *> (pi);

	      for (int i = 0; i < nel; i++)
		ptr[i] = Complex (ppr[i], ppi[i]);

	      retval = val;
	    }
	  else
	    {
	      NDArray val (dv);

	      double *ptr = val.fortran_vec ();

	      for (int i = 0; i < nel; i++)
		ptr[i] = ppr[i];

	      retval = val;
	    }
	}
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

  mxArray_number (const mxArray_number& val)
    : mxArray_matlab (val),
      pr (malloc (get_number_of_elements () * get_element_size ())),
      pi (val.pi ? malloc (get_number_of_elements () * get_element_size ()) : 0)
  {
    size_t nbytes = get_number_of_elements () * get_element_size ();

    if (pr)
      memcpy (pr, val.pr, nbytes);

    if (pi)
      memcpy (pi, val.pi, nbytes);
  }

private:

  void *pr;
  void *pi;
};

// Matlab-style sparse arrays.

class mxArray_sparse : public mxArray_matlab
{
public:

  mxArray_sparse (mxClassID id_arg, int m, int n, int nzmax_arg,
		  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, m, n), nzmax (nzmax_arg)
  {
    pr = (calloc (nzmax, get_element_size ()));
    pi = (flag == mxCOMPLEX ? calloc (nzmax, get_element_size ()) : 0);
    ir = static_cast<int *> (calloc (nzmax, sizeof (int)));
    jc = static_cast<int *> (calloc (n + 1, sizeof (int)));
  }

  mxArray_sparse *clone (void) const { return new mxArray_sparse (*this); }

  ~mxArray_sparse (void)
  {
    mxFree (pr);
    mxFree (pi);
    mxFree (ir);
    mxFree (jc);
  }

  int is_complex (void) const { return pi != 0; }

  int is_sparse (void) const { return 1; }

  void *get_data (void) const { return pr; }

  void *get_imag_data (void) const { return pi; }

  void set_data (void *pr_arg) { pr = pr_arg; }

  void set_imag_data (void *pi_arg) { pi = pi_arg; }

  int *get_ir (void) const { return ir; }

  int *get_jc (void) const { return jc; }

  int get_nzmax (void) const { return nzmax; }

  void set_ir (int *ir_arg) { ir = ir_arg; }

  void set_jc (int *jc_arg) { jc = jc_arg; }

  void set_nzmax (int nzmax_arg) { nzmax = nzmax_arg; }

protected:

  octave_value as_octave_value (void) const
  {
    octave_value retval;

    dim_vector dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxLOGICAL_CLASS:
	{
	  bool *ppr = static_cast<bool *> (pr);

	  SparseBoolMatrix val (get_m (), get_n (),
				static_cast<octave_idx_type> (nzmax));

	  for (int i = 0; i < nzmax; i++)
	    {
	      val.xdata(i) = ppr[i];
	      val.xridx(i) = ir[i];
	    }

	  for (int i = 0; i < get_n () + 1; i++)
	    val.xcidx(i) = jc[i];

	  retval = val;
	}
	break;

      case mxSINGLE_CLASS:
	error ("single precision data type not supported");
	break;

      case mxDOUBLE_CLASS:
	{
	  if (pi)
	    {
	      double *ppr = static_cast<double *> (pr);
	      double *ppi = static_cast<double *> (pi);

	      SparseComplexMatrix val (get_m (), get_n (),
				       static_cast<octave_idx_type> (nzmax));

	      for (int i = 0; i < nzmax; i++)
		{
		  val.xdata(i) = Complex (ppr[i], ppi[i]);
		  val.xridx(i) = ir[i];
		}

	      for (int i = 0; i < get_n () + 1; i++)
		val.xcidx(i) = jc[i];

	      retval = val;
	    }
	  else
	    {
	      double *ppr = static_cast<double *> (pr);

	      SparseMatrix val (get_m (), get_n (),
				static_cast<octave_idx_type> (nzmax));

	      for (int i = 0; i < nzmax; i++)
		{
		  val.xdata(i) = ppr[i];
		  val.xridx(i) = ir[i];
		}

	      for (int i = 0; i < get_n () + 1; i++)
		val.xcidx(i) = jc[i];

	      retval = val;
	    }
	}
	break;

      default:
	panic_impossible ();
      }

    return retval;
  }

private:

  int nzmax;

  void *pr;
  void *pi;
  int *ir;
  int *jc;

  mxArray_sparse (const mxArray_sparse& val)
    : mxArray_matlab (val), nzmax (val.nzmax),
      ir (static_cast<int *> (malloc (nzmax * sizeof (int)))),
      jc (static_cast<int *> (malloc (nzmax * sizeof (int))))
  {
    size_t nbytes = nzmax * get_element_size ();

    if (pr)
      memcpy (pr, val.pr, nbytes);

    if (pi)
      memcpy (pi, val.pi, nbytes);

    if (ir)
      memcpy (ir, val.ir, nzmax * sizeof (int));

    if (jc)
      memcpy (jc, val.jc, (val.get_n () + 1) * sizeof (int));
  }
};

// Matlab-style struct arrays.

class mxArray_struct : public mxArray_matlab
{
public:

  mxArray_struct (int ndims_arg, const int *dims_arg, int num_keys_arg,
		  const char **keys)
    : mxArray_matlab (mxSTRUCT_CLASS, ndims_arg, dims_arg), nfields (num_keys_arg),
      fields (static_cast<char **> (calloc (nfields, sizeof (char *)))),
      data (static_cast<mxArray **> (calloc (nfields * get_number_of_elements (), sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (const dim_vector& dv, int num_keys_arg, const char **keys)
    : mxArray_matlab (mxSTRUCT_CLASS, dv), nfields (num_keys_arg),
      fields (static_cast<char **> (calloc (nfields, sizeof (char *)))),
      data (static_cast<mxArray **> (calloc (nfields * get_number_of_elements (), sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (int m, int n, int num_keys_arg, const char **keys)
    : mxArray_matlab (mxSTRUCT_CLASS, m, n), nfields (num_keys_arg),
      fields (static_cast<char **> (calloc (nfields, sizeof (char *)))),
      data (static_cast<mxArray **> (calloc (nfields * get_number_of_elements (), sizeof (mxArray *))))
  {
    init (keys);
  }

  void init (const char **keys)
  {
    for (int i = 0; i < nfields; i++)
      fields[i] = strsave (keys[i]);
  }

  mxArray_struct *clone (void) const { return new mxArray_struct (*this); }

  ~mxArray_struct (void)
  {
    for (int i = 0; i < nfields; i++)
      mxFree (fields[i]);

    mxFree (fields);

    int ntot = nfields * get_number_of_elements ();

    for  (int i = 0; i < ntot; i++)
      delete data[i];

    mxFree (data);
  }

  int add_field (const char *key)
  {
    int retval = -1;

    if (valid_key (key))
      {
	nfields++;

	fields = static_cast<char **> (mxRealloc (fields, nfields * sizeof (char *)));

	if (fields)
	  {
	    fields[nfields-1] = strsave (key);

	    int nel = get_number_of_elements ();

	    int ntot = nfields * nel;

	    mxArray **new_data = static_cast<mxArray **> (malloc (ntot * sizeof (mxArray *)));

	    if (new_data)
	      {
		int j = 0;
		int k = 0;
		int n = 0;

		for (int i = 0; i < ntot; i++)
		  {
		    if (++n == nfields)
		      {
			new_data[j++] = 0;
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
	int nel = get_number_of_elements ();

	int ntot = nfields * nel;

	int new_nfields = nfields - 1;

	char **new_fields = static_cast<char **> (malloc (new_nfields * sizeof (char *)));

	mxArray **new_data = static_cast<mxArray **> (malloc (new_nfields * nel * sizeof (mxArray *)));

	for (int i = 0; i < key_num; i++)
	  new_fields[i] = fields[i];

	for (int i = key_num + 1; i < nfields; i++)
	  new_fields[i-1] = fields[i];

	if (new_nfields > 0)
	  {
	    int j = 0;
	    int k = 0;
	    int n = 0;

	    for (int i = 0; i < ntot; i++)
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

  mxArray *get_field_by_number (int index, int key_num) const
  {
    return key_num >= 0 && key_num < nfields
      ? data[nfields * index + key_num] : 0;
  }

  void set_field_by_number (int index, int key_num, mxArray *val);

  int get_number_of_fields (void) const { return nfields; }

  const char *get_field_name_by_number (int key_num) const
  {
    return key_num >= 0 && key_num < nfields ? fields[key_num] : 0;
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

  void *get_data (void) const { return data; }

  void set_data (void *data_arg) { data = static_cast<mxArray **> (data_arg); }

protected:

  octave_value as_octave_value (void) const
  {
    dim_vector dv = dims_to_dim_vector ();

    string_vector keys (fields, nfields);

    Octave_map m;

    int ntot = nfields * get_number_of_elements ();

    for (int i = 0; i < nfields; i++)
      {
	Cell c (dv);

	octave_value *p = c.fortran_vec ();

	int k = 0;
	for (int j = i; j < ntot; j += nfields)
	  p[k++] = mxArray::as_octave_value (data[j]);

	m.assign (keys[i], c);
      }

    return m;
  }

private:

  int nfields;

  char **fields;

  mxArray **data;

  mxArray_struct (const mxArray_struct& val)
    : mxArray_matlab (val), nfields (val.nfields),
      fields (static_cast<char **> (malloc (nfields * sizeof (char *)))),
      data (static_cast<mxArray **> (malloc (nfields * get_number_of_elements () * sizeof (mxArray *))))
  {
    for (int i = 0; i < nfields; i++)
      fields[i] = strsave (val.fields[i]);

    int nel = get_number_of_elements ();

    for (int i = 0; i < nel * nfields; i++)
      data[i] = val.data[i]->clone ();
  }
};

// Matlab-style cell arrays.

class mxArray_cell : public mxArray_matlab
{
public:

  mxArray_cell (int ndims_arg, const int *dims_arg)
    : mxArray_matlab (mxCELL_CLASS, ndims_arg, dims_arg),
      data (static_cast<mxArray **> (calloc (get_number_of_elements (), sizeof (mxArray *)))) { }

  mxArray_cell (const dim_vector& dv)
    : mxArray_matlab (mxCELL_CLASS, dv),
      data (static_cast<mxArray **> (calloc (get_number_of_elements (), sizeof (mxArray *)))) { }

  mxArray_cell (int m, int n)
    : mxArray_matlab (mxCELL_CLASS, m, n),
      data (static_cast<mxArray **> (calloc (get_number_of_elements (), sizeof (mxArray *)))) { }

  mxArray_cell *clone (void) const { return new mxArray_cell (*this); }

  ~mxArray_cell (void)
  {
    int nel = get_number_of_elements ();

    for  (int i = 0; i < nel; i++)
      delete data[i];

    mxFree (data);
  }

  mxArray *get_cell (int idx) const
  {
    return idx >= 0 && idx < get_number_of_elements () ? data[idx] : 0;
  }

  void set_cell (int idx, mxArray *val);

  void *get_data (void) const { return data; }

  void set_data (void *data_arg) { data = static_cast<mxArray **> (data_arg); }

protected:

  octave_value as_octave_value (void) const
  {
    dim_vector dv = dims_to_dim_vector ();

    Cell c (dv);

    int nel = get_number_of_elements ();

    octave_value *p = c.fortran_vec ();

    for (int i = 0; i < nel; i++)
      p[i] = mxArray::as_octave_value (data[i]);

    return c;
  }

private:

  mxArray **data;

  mxArray_cell (const mxArray_cell& val)
    : mxArray_matlab (val),
      data (static_cast<mxArray **> (malloc (get_number_of_elements () * sizeof (mxArray *))))
  {
    int nel = get_number_of_elements ();

    for (int i = 0; i < nel; i++)
      data[i] = val.data[i]->clone ();
  }
};

// ------------------------------------------------------------------

mxArray::mxArray (const octave_value& ov)
  : rep (new mxArray_octave_value (ov)), name (0) { }

mxArray::mxArray (mxClassID id, int ndims, const int *dims, mxComplexity flag)
  : rep (new mxArray_number (id, ndims, dims, flag)), name (0) { }

mxArray::mxArray (mxClassID id, const dim_vector& dv, mxComplexity flag)
  : rep (new mxArray_number (id, dv, flag)), name (0) { }

mxArray::mxArray (mxClassID id, int m, int n, mxComplexity flag)
  : rep (new mxArray_number (id, m, n, flag)), name (0) { }

mxArray::mxArray (mxClassID id, double val)
  : rep (new mxArray_number (id, val)), name (0) { }

mxArray::mxArray (mxClassID id, mxLogical val)
  : rep (new mxArray_number (id, val)), name (0) { }

mxArray::mxArray (const char *str)
  : rep (new mxArray_number (str)), name (0) { }

mxArray::mxArray (int m, const char **str)
  : rep (new mxArray_number (m, str)), name (0) { }

mxArray::mxArray (mxClassID id, int m, int n, int nzmax, mxComplexity flag)
  : rep (new mxArray_sparse (id, m, n, nzmax, flag)), name (0) { }

mxArray::mxArray (int ndims, const int *dims, int num_keys, const char **keys)
  : rep (new mxArray_struct (ndims, dims, num_keys, keys)), name (0) { }

mxArray::mxArray (const dim_vector& dv, int num_keys, const char **keys)
  : rep (new mxArray_struct (dv, num_keys, keys)), name (0) { }

mxArray::mxArray (int m, int n, int num_keys, const char **keys)
  : rep (new mxArray_struct (m, n, num_keys, keys)), name (0) { }

mxArray::mxArray (int ndims, const int *dims)
  : rep (new mxArray_cell (ndims, dims)), name (0) { }

mxArray::mxArray (const dim_vector& dv)
  : rep (new mxArray_cell (dv)), name (0) { }

mxArray::mxArray (int m, int n)
  : rep (new mxArray_cell (m, n)), name (0) { }

mxArray::~mxArray (void)
{
  mxFree (name);

  delete rep;
}

void
mxArray::set_name (const char *name_arg)
{
  mxFree (name);
  name = strsave (name_arg);
}

octave_value
mxArray::as_octave_value (mxArray *ptr)
{
  return ptr ? ptr->as_octave_value () : octave_value (Matrix ());
}

octave_value
mxArray::as_octave_value (void) const
{
  return rep->as_octave_value ();
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
	  new_val->rep = 0;
	  delete new_val;
	}
    }
}

// ------------------------------------------------------------------

// A clas to manage calls to MEX functions.  Mostly deals with memory
// management.

class mex
{
public:

  mex (octave_mex_function *f)
    : curr_mex_fcn (f), memlist (), arraylist (), fname (0) { }

  ~mex (void)
  {
    if (! memlist.empty ())
      error ("mex: %s: cleanup failed", function_name ());

    mxFree (fname);
  }

  const char *function_name (void) const
  {
    if (! fname)
      {
	octave_function *fcn = octave_call_stack::current ();

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

  // Free all unmarked pointers obtained from malloc and calloc.
  static void cleanup (void *ptr)
  {
    mex *context = static_cast<mex *> (ptr);

    // We can't use mex::free here because it modifies memlist.
    for (std::set<void *>::iterator p = context->memlist.begin ();
	 p != context->memlist.end (); p++)
      {
	if (*p)
	  {
	    context->unmark (*p);

	    xfree (*p);
	  }
      }

    context->memlist.clear ();

    // We can't use mex::free_value here because it modifies arraylist.
    for (std::set<mxArray *>::iterator p = context->arraylist.begin ();
	 p != context->arraylist.end (); p++)
      delete *p;

    context->arraylist.clear ();
  }

  // Allocate memory.
  void *malloc_unmarked (size_t n)
  {
    void *ptr = ::malloc (n);

    if (! ptr)
      {
	// FIXME -- could use "octave_new_handler();" instead

	error ("%s: failed to allocate %d bytes of memory",
	       function_name (), n);

	abort ();
      }

    global_mark (ptr);

    return ptr;
  }

  // Allocate memory to be freed on exit.
  void *malloc (size_t n)
  {
    void *ptr = malloc_unmarked (n);

    mark (ptr);

    return ptr;
  }

  // Allocate memory and initialize to 0.
  void *calloc_unmarked (size_t n, size_t t)
  {
    void *ptr = malloc_unmarked (n*t);

    memset (ptr, 0, n*t);

    return ptr;
  }

  // Allocate memory to be freed on exit and initialize to 0.
  void *calloc (size_t n, size_t t)
  {
    void *ptr = calloc_unmarked (n, t);

    mark (ptr);

    return ptr;
  }

  // Reallocate a pointer obtained from malloc or calloc.  We don't
  // need an "unmarked" version of this.
  void *realloc (void *ptr, size_t n)
  {
    void *v = ::realloc (ptr, n);

    std::set<void *>::iterator p = memlist.find (ptr);

    if (v && p != memlist.end ())
      {
	memlist.erase (p);
	memlist.insert (v);
      }

    p = global_memlist.find (ptr);

    if (v && p != global_memlist.end ())
      {
	global_memlist.erase (p);
	global_memlist.insert (v);
      }

    return v;
  }

  // Free a pointer obtained from malloc or calloc.
  void free (void *ptr)
  {
    if (ptr)
      {
	unmark (ptr);

	std::set<void *>::iterator p = global_memlist.find (ptr);

	if (p != global_memlist.end ())
	  {
	    global_memlist.erase (p);

	    xfree (ptr);
	  }
	else
	  warning ("mxFree: skipping memory not allocated by mxMalloc, mxCalloc, or mxRealloc");
      }
  }

  // Mark a pointer so that it will not be freed on exit.
  void persistent (void *ptr) { unmark (ptr); }

  mxArray *mark_array (mxArray *ptr)
  {
    arraylist.insert (ptr);
    return ptr;
  }

  void unmark_array (mxArray *ptr)
  {
    std::set<mxArray *>::iterator p = arraylist.find (ptr);

    if (p != arraylist.end ())
      arraylist.erase (p);
  }

  // Make a new array value and initialize from an octave value; it will be
  // freed on exit unless marked as persistent.
  mxArray *make_value (const octave_value& ov)
  {
    return mark_array (new mxArray (ov));
  }

  // Free an array and its contents.
  bool free_value (mxArray *ptr)
  {
    bool inlist = false;

    std::set<mxArray *>::iterator p = arraylist.find (ptr);

    if (p != arraylist.end ())
      {
	inlist = true;
	arraylist.erase (p);
	delete ptr;
      }
#ifdef DEBUG
    else
      warning ("mex::free_value: skipping memory not allocated by mex::make_value");
#endif

    return inlist;
  }

  // Remove PTR from the list of arrays to be free on exit.
  void persistent (mxArray *ptr) { unmark_array (ptr); }

  octave_mex_function *current_mex_function (void) const
  {
    return curr_mex_fcn;
  }

  // 1 if error should be returned to MEX file, 0 if abort.
  int trap_feval_error;

  // longjmp return point if mexErrMsgTxt or error.
  jmp_buf jump;

  // Trigger a long jump back to the mex calling function.
  void abort (void) { longjmp (jump, 1); }

private:

  // Pointer to the mex function that corresponds to this mex context.
  octave_mex_function *curr_mex_fcn;

  // List of memory resources that need to be freed upon exit.
  std::set<void *> memlist;

  std::set<mxArray *> arraylist;

  // The name of the currently executing function.
  mutable char *fname;

  // Mark a pointer to be freed on exit.
  void mark (void *ptr)
  {
#ifdef DEBUG
    if (memlist.find (ptr) != memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    memlist.insert (ptr);
  }

  // Unmark a pointer to be freed on exit, either because it was
  // made persistent, or because it was already freed.
  void unmark (void *ptr)
  {
    std::set<void *>::iterator p = memlist.find (ptr);

    if (p != memlist.end ())
      memlist.erase (p);
#ifdef DEBUG
    else
      warning ("%s: value not marked", function_name ());
#endif
  }

  // List of memory resources we allocated.
  static std::set<void *> global_memlist;

  // Mark a pointer as one we allocated.
  void global_mark (void *ptr)
  {
#ifdef DEBUG
    if (global_memlist.find (ptr) != global_memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    global_memlist.insert (ptr);
  }

  // Unmark a pointer as one we allocated.
  void global_unmark (void *ptr)
  {
    std::set<void *>::iterator p = global_memlist.find (ptr);

    if (p != global_memlist.end ())
      global_memlist.erase (p);
#ifdef DEBUG
    else
      warning ("%s: value not marked", function_name ());
#endif

  }
};

// List of memory resources we allocated.
std::set<void *> mex::global_memlist;

// Current context.
mex *mex_context = 0;

void *
mxArray::malloc (size_t n)
{
  return mex_context ? mex_context->malloc_unmarked (n) : ::malloc (n);
}

void *
mxArray::calloc (size_t n, size_t t)
{
  return mex_context ? mex_context->calloc_unmarked (n, t) : ::calloc (n, t);
}

static inline mxArray *
maybe_unmark_array (mxArray *ptr)
{
  if (mex_context)
    mex_context->unmark_array (ptr);

  return ptr;
}

void
mxArray_struct::set_field_by_number (int index, int key_num, mxArray *val)
{
  if (key_num >= 0 && key_num < nfields)
    data[nfields * index + key_num] = maybe_unmark_array (val);
}

void
mxArray_cell::set_cell (int idx, mxArray *val)
{
  if (idx >= 0 && idx < get_number_of_elements ())
    data[idx] = maybe_unmark_array (val);
}

// ------------------------------------------------------------------

// C interface to mxArray objects:

// Floating point predicates.

int
mxIsFinite (const double v)
{
  return lo_ieee_finite (v) != 0;
}

int
mxIsInf (const double v)
{
  return lo_ieee_isinf (v) != 0;
}

int
mxIsNaN (const double v)
{
  return lo_ieee_isnan (v) != 0;
}

double
mxGetEps (void)
{
  return DBL_EPSILON;
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
  return mex_context ? mex_context->calloc (n, size) : calloc (n, size);
}

void *
mxMalloc (size_t n)
{
  return mex_context ? mex_context->malloc (n) : malloc (n);
}

void *
mxRealloc (void *ptr, size_t size)
{
  return mex_context ? mex_context->realloc (ptr, size) : realloc (ptr, size);
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
mxCreateCellArray (int ndims, const int *dims)
{
  return maybe_mark_array (new mxArray (ndims, dims));
}

mxArray *
mxCreateCellMatrix (int m, int n)
{
  return maybe_mark_array (new mxArray (m, n));
}

mxArray *
mxCreateCharArray (int ndims, const int *dims)
{
  return maybe_mark_array (new mxArray (mxCHAR_CLASS, ndims, dims));
}

mxArray *
mxCreateCharMatrixFromStrings (int m, const char **str)
{
  return maybe_mark_array (new mxArray (m, str));
}

mxArray *
mxCreateDoubleMatrix (int m, int n, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (mxDOUBLE_CLASS, m, n, flag));
}

mxArray *
mxCreateDoubleScalar (double val)
{
  return maybe_mark_array (new mxArray (mxDOUBLE_CLASS, val));
}

mxArray *
mxCreateLogicalArray (int ndims, const int *dims)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, ndims, dims));
}

mxArray *
mxCreateLogicalMatrix (int m, int n)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, m, n));
}

mxArray *
mxCreateLogicalScalar (int val)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, val));
}

mxArray *
mxCreateNumericArray (int ndims, const int *dims, mxClassID class_id,
		      mxComplexity flag)
{
  return maybe_mark_array (new mxArray (class_id, ndims, dims, flag));
}

mxArray *
mxCreateNumericMatrix (int m, int n, mxClassID class_id, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (class_id, m, n, flag));
}

mxArray *
mxCreateSparse (int m, int n, int nzmax, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (mxDOUBLE_CLASS, m, n, nzmax, flag));
}

mxArray *
mxCreateSparseLogicalMatrix (int m, int n, int nzmax)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, m, n, nzmax));
}

mxArray *
mxCreateString (const char *str)
{
  return maybe_mark_array (new mxArray (str));
}

mxArray *
mxCreateStructArray (int ndims, int *dims, int num_keys, const char **keys)
{
  return maybe_mark_array (new mxArray (ndims, dims, num_keys, keys));
}

mxArray *
mxCreateStructMatrix (int m, int n, int num_keys, const char **keys)
{
  return maybe_mark_array (new mxArray (m, n, num_keys, keys));
}

// Copy constructor.
mxArray *
mxDuplicateArray (const mxArray *ptr)
{
  return maybe_mark_array (ptr->clone ());
}

// Destructor.
void
mxDestroyArray (mxArray *ptr)
{
  if (! (mex_context && mex_context->free_value (ptr)))
    delete ptr;
}

// Type Predicates.
int
mxIsCell (const mxArray *ptr)
{
  return ptr->is_cell ();
}

int
mxIsChar (const mxArray *ptr)
{
  return ptr->is_char ();
}

int
mxIsClass (const mxArray *ptr, const char *name)
{
  return ptr->is_class (name);
}

int
mxIsComplex (const mxArray *ptr)
{
  return ptr->is_complex ();
}

int
mxIsDouble (const mxArray *ptr)
{
  return ptr->is_double ();
}

int
mxIsInt16 (const mxArray *ptr)
{
  return ptr->is_int16 ();
}

int
mxIsInt32 (const mxArray *ptr)
{
  return ptr->is_int32 ();
}

int
mxIsInt64 (const mxArray *ptr)
{
  return ptr->is_int64 ();
}

int
mxIsInt8 (const mxArray *ptr)
{
  return ptr->is_int8 ();
}

int
mxIsLogical (const mxArray *ptr)
{
  return ptr->is_logical ();
}

int
mxIsNumeric (const mxArray *ptr)
{
  return ptr->is_numeric ();
}

int
mxIsSingle (const mxArray *ptr)
{
  return ptr->is_single ();
}

int
mxIsSparse (const mxArray *ptr)
{
  return ptr->is_sparse ();
}

int
mxIsStruct (const mxArray *ptr)
{
  return ptr->is_struct ();
}

int
mxIsUint16 (const mxArray *ptr)
{
  return ptr->is_uint16 ();
}

int
mxIsUint32 (const mxArray *ptr)
{
  return ptr->is_uint32 ();
}

int
mxIsUint64 (const mxArray *ptr)
{
  return ptr->is_uint64 ();
}

int
mxIsUint8 (const mxArray *ptr)
{
  return ptr->is_uint8 ();
}

// Odd type+size predicate.
int
mxIsLogicalScalar (const mxArray *ptr)
{
  return ptr->is_logical_scalar ();
}

// Odd type+size+value predicate.
int
mxIsLogicalScalarTrue (const mxArray *ptr)
{
  return ptr->is_logical_scalar_true ();
}

// Size predicate.
int
mxIsEmpty (const mxArray *ptr)
{
  return ptr->is_empty ();
}

// Just plain odd thing to ask of a value.
int
mxIsFromGlobalWS (const mxArray */*ptr*/)
{
  // FIXME
  abort ();
  return 0;
}

// Dimension extractors.
int
mxGetM (const mxArray *ptr)
{
  return ptr->get_m ();
}

int
mxGetN (const mxArray *ptr)
{
  return ptr->get_n ();
}

int *
mxGetDimensions (const mxArray *ptr)
{
  return ptr->get_dimensions ();
}

int
mxGetNumberOfDimensions (const mxArray *ptr)
{
  return ptr->get_number_of_dimensions ();
}

int
mxGetNumberOfElements (const mxArray *ptr)
{
  return ptr->get_number_of_elements ();
}

// Dimension setters.
void
mxSetM (mxArray *ptr, int m)
{
  ptr->set_m (m);
}

void
mxSetN (mxArray *ptr, int n)
{
  ptr->set_n (n);
}

void
mxSetDimensions (mxArray *ptr, int *dims, int ndims)
{
  ptr->set_dimensions (dims, ndims);
}
  
// Data extractors.
double *
mxGetPr (const mxArray *ptr)
{
  return static_cast<double *> (ptr->get_data ());
}

double *
mxGetPi (const mxArray *ptr)
{
  return static_cast<double *> (ptr->get_imag_data ());
}

double
mxGetScalar (const mxArray *ptr)
{
  double *d = mxGetPr (ptr);
  return d[0];
}

mxChar *
mxGetChars (const mxArray *ptr)
{
  return static_cast<mxChar *> (ptr->get_data ());
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

void *
mxGetImagData (const mxArray *ptr)
{
  return ptr->get_imag_data ();
}

// Data setters.
void
mxSetPr (mxArray *ptr, double *pr)
{
  ptr->set_data (pr);
}

void
mxSetPi (mxArray *ptr, double *pi)
{
  ptr->set_imag_data (pi);
}

void
mxSetData (mxArray *ptr, void *pr)
{
  ptr->set_data (pr);
}

void
mxSetImagData (mxArray *ptr, void *pi)
{
  ptr->set_imag_data (pi);
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

// Cell support.
mxArray *
mxGetCell (const mxArray *ptr, int idx)
{
  return ptr->get_cell (idx);
}

void
mxSetCell (mxArray *ptr, int idx, mxArray *val)
{
  ptr->set_cell (idx, val);
}

// Sparse support.
int *
mxGetIr (const mxArray *ptr)
{
  return ptr->get_ir ();
}

int *
mxGetJc (const mxArray *ptr)
{
  return ptr->get_jc ();
}

int
mxGetNzmax (const mxArray *ptr)
{
  return ptr->get_nzmax ();
}

void
mxSetIr (mxArray *ptr, int *ir)
{
  ptr->set_ir (ir);
}

void
mxSetJc (mxArray *ptr, int *jc)
{
  ptr->set_jc (jc);
}

void
mxSetNzmax (mxArray *ptr, int nzmax)
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
mxGetField (const mxArray *ptr, int index, const char *key)
{
  int key_num = mxGetFieldNumber (ptr, key);
  return mxGetFieldByNumber (ptr, index, key_num);
}

mxArray *
mxGetFieldByNumber (const mxArray *ptr, int index, int key_num)
{
  return ptr->get_field_by_number (index, key_num);
}

void
mxSetField (mxArray *ptr, int index, const char *key, mxArray *val)
{
  int key_num = mxGetFieldNumber (ptr, key);
  mxSetFieldByNumber (ptr, index, key_num, val);
}

void
mxSetFieldByNumber (mxArray *ptr, int index, int key_num, mxArray *val)
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
mxGetString (const mxArray *ptr, char *buf, int buflen)
{
  return ptr->get_string (buf, buflen);
}

char *
mxArrayToString (const mxArray *ptr)
{
  return ptr->array_to_string ();
}
  
int
mxCalcSingleSubscript (const mxArray *ptr, int nsubs, int *subs)
{
  return ptr->calc_single_subscript (nsubs, subs);
}

int
mxGetElementSize (const mxArray *ptr)
{
  return ptr->get_element_size ();
}

// ------------------------------------------------------------------

typedef void (*cmex_fptr) (int nlhs, mxArray **plhs, int nrhs, mxArray **prhs);
typedef F77_RET_T (*fmex_fptr) (int& nlhs, mxArray **plhs, int& nrhs, mxArray **prhs);

octave_value_list
call_mex (bool have_fmex, void *f, const octave_value_list& args,
	  int nargout, octave_mex_function *curr_mex_fcn)
{
  // Use at least 1 for nargout since even for zero specified args,
  // still want to be able to return an ans.

  int nargin = args.length ();
  OCTAVE_LOCAL_BUFFER (mxArray *, argin, nargin);
  for (int i = 0; i < nargin; i++)
    argin[i] = 0;

  int nout = nargout == 0 ? 1 : nargout;
  OCTAVE_LOCAL_BUFFER (mxArray *, argout, nout);
  for (int i = 0; i < nout; i++)
    argout[i] = 0;

  unwind_protect::begin_frame ("call_mex");

  // Save old mex pointer.
  unwind_protect_ptr (mex_context);

  mex context (curr_mex_fcn);

  unwind_protect::add (mex::cleanup, static_cast<void *> (&context));

  for (int i = 0; i < nargin; i++)
    argin[i] = context.make_value (args(i));

  if (setjmp (context.jump) == 0)
    {
      mex_context = &context;

      if (have_fmex)
	{
	  fmex_fptr fcn = FCN_PTR_CAST (fmex_fptr, f);

	  int tmp_nargout = nargout;
	  int tmp_nargin = nargin;

	  fcn (tmp_nargout, argout, tmp_nargin, argin);
	}
      else
	{
	  cmex_fptr fcn = FCN_PTR_CAST (cmex_fptr, f);

	  fcn (nargout, argout, nargin, argin);
	}
    }

  // Convert returned array entries back into octave values.

  octave_value_list retval;

  if (! error_state)
    {
      if (nargout == 0 && argout[0])
	{
	  // We have something for ans.
	  nargout = 1;
	}

      retval.resize (nargout);

      for (int i = 0; i < nargout; i++)
	retval(i) = mxArray::as_octave_value (argout[i]);
    }

  // Clean up mex resources.
  unwind_protect::run_frame ("call_mex");

  return retval;
}

// C interface to mex functions:

const char *
mexFunctionName (void)
{
  return mex_context ? mex_context->function_name () : "unknown";
}

int
mexCallMATLAB (int nargout, mxArray *argout[], int nargin, mxArray *argin[],
	       const char *fname)
{
  octave_value_list args;

  // FIXME -- do we need unwind protect to clean up args?  Off hand, I
  // would say that this problem is endemic to Octave and we will
  // continue to have memory leaks after Ctrl-C until proper exception
  // handling is implemented.  longjmp() only clears the stack, so any
  // class which allocates data on the heap is going to leak.

  args.resize (nargin);

  for (int i = 0; i < nargin; i++)
    args(i) = mxArray::as_octave_value (argin[i]);

  octave_value_list retval = feval (fname, args, nargout);

  if (error_state && mex_context->trap_feval_error == 0)
    {
      // FIXME -- is this the correct way to clean up?  abort() is
      // going to trigger a long jump, so the normal class destructors
      // will not be called.  Hopefully this will reduce things to a
      // tiny leak.  Maybe create a new octave memory tracer type
      // which prints a friendly message every time it is
      // created/copied/deleted to check this.

      args.resize (0);
      retval.resize (0);
      mex_context->abort ();
    }

  int num_to_copy = retval.length ();

  if (nargout < retval.length ())
    num_to_copy = nargout;

  for (int i = 0; i < num_to_copy; i++)
    {
      // FIXME -- it would be nice to avoid copying the value here,
      // but there is no way to steal memory from a matrix, never mind
      // that matrix memory is allocated by new[] and mxArray memory
      // is allocated by malloc().
      argout[i] = mex_context->make_value (retval (i));
    }

  while (num_to_copy < nargout)
    argout[num_to_copy++] = 0;

  if (error_state)
    {
      error_state = 0;
      return 1;
    }
  else
    return 0;
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

  int parse_status;

  octave_value_list ret;

  ret = eval_string (s, false, parse_status, 0);

  if (parse_status || error_state)
    {
      error_state = 0;

      retval = 1;
    }

  return retval;
}

void
mexErrMsgTxt (const char *s)
{
  if (s && strlen (s) > 0)
    error ("%s: %s", mexFunctionName (), s);
  else
    // Just set the error state; don't print msg.
    error ("");

  mex_context->abort ();
}

void
mexErrMsgIdAndTxt (const char *id, const char *s)
{
  if (s && strlen (s) > 0)
    error_with_id (id, "%s: %s", mexFunctionName (), s);
  else
    // Just set the error state; don't print msg.
    error ("");

  mex_context->abort ();
}

void
mexWarnMsgTxt (const char *s)
{
  warning ("%s", s);
}

void
mexWarnMsgIdAndTxt (const char *id, const char *s)
{
  warning_with_id (id, "%s", s);
}

void
mexPrintf (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  octave_vformat (octave_stdout, fmt, args);
  va_end (args);
}

mxArray *
mexGetVariable (const char *space, const char *name)
{
  mxArray *retval = 0;

  // FIXME -- this should be in variable.cc, but the correct
  // functionality is not exported.  Particularly, get_global_value()
  // generates an error if the symbol is undefined.

  symbol_record *sr = 0;

  if (! strcmp (space, "global"))
    sr = global_sym_tab->lookup (name);
  else if (! strcmp (space, "caller"))
    sr = curr_sym_tab->lookup (name);
  else if (! strcmp (space, "base"))
    sr = top_level_sym_tab->lookup (name);
  else
    mexErrMsgTxt ("mexGetVariable: symbol table does not exist");

  if (sr)
    {
      octave_value sr_def = sr->def ();

      if (sr_def.is_defined ())
	{
	  retval = mex_context->make_value (sr_def);

	  retval->set_name (name);
	}
    }

  return retval;
}

const mxArray *
mexGetVariablePtr (const char *space, const char *name)
{
  return mexGetVariable (space, name);
}

int
mexPutVariable (const char *space, const char *name, mxArray *ptr)
{
  if (! ptr)
    return 1;

  if (! name)
    return 1;

  if (name[0] == '\0')
    name = ptr->get_name ();

  if (! name || name[0] == '\0')
    return 1;

  if (! strcmp (space, "global"))
    set_global_value (name, mxArray::as_octave_value (ptr));
  else
    {
      // FIXME -- this belongs in variables.cc.

      symbol_record *sr = 0;

      if (! strcmp (space, "caller"))
	sr = curr_sym_tab->lookup (name, true);
      else if (! strcmp (space, "base"))
	sr = top_level_sym_tab->lookup (name, true);
      else
	mexErrMsgTxt ("mexPutVariable: symbol table does not exist");

      if (sr)
	sr->define (mxArray::as_octave_value (ptr));
      else
	panic_impossible ();
    }

  return 0;
}

void
mexMakeArrayPersistent (mxArray *ptr)
{
  if (mex_context)
    mex_context->persistent (ptr);
}

void
mexMakeMemoryPersistent (void *ptr)
{
  if (mex_context)
    mex_context->persistent (ptr);
}

int
mexAtExit (void (*f) (void))
{
  if (mex_context)
    {
      octave_mex_function *curr_mex_fcn = mex_context->current_mex_function ();

      assert (curr_mex_fcn);

      curr_mex_fcn->atexit (f);
    }

  return 0;
}

const mxArray *
mexGet (double /*handle*/, const char */*property*/)
{
  // FIXME
  error ("mexGet: not implemented");
  return 0;
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

      retval = mislocked (fname);
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

      mlock (fname);
    }
}

int
mexSet (double /*handle*/, const char */*property*/, mxArray */*val*/)
{
  // FIXME
  error ("mexSet: not implemented");
  return 0;
}

void
mexUnlock (void)
{
  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      std::map<std::string,int>::iterator p = mex_lock_count.find (fname);

      if (p != mex_lock_count.end ())
	{
	  int count = --mex_lock_count[fname];

	  if (count == 0)
	    {
	      munlock (fname);

	      mex_lock_count.erase (p);
	    }
	}
    }
}
