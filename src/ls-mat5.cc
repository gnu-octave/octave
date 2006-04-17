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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// Author: James R. Van Zandt <jrv@vanzandt.mv.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

#include "ls-utils.h"
#include "ls-mat5.h"

#ifdef HAVE_ZLIB
#include <zlib.h>
#endif

#define PAD(l) (((l)<=4)?4:(((l)+7)/8)*8)
#define TAGLENGTH(l) ((l)<=4?4:8)

enum arrayclasstype
  {
    mxCELL_CLASS=1,		// cell array
    mxSTRUCT_CLASS,		// structure
    mxOBJECT_CLASS,		// object
    mxCHAR_CLASS,		// character array
    mxSPARSE_CLASS,		// sparse array
    mxDOUBLE_CLASS,		// double precision array
    mxSINGLE_CLASS,		// single precision floating point
    mxINT8_CLASS,		// 8 bit signed integer
    mxUINT8_CLASS,		// 8 bit unsigned integer
    mxINT16_CLASS,		// 16 bit signed integer
    mxUINT16_CLASS,		// 16 bit unsigned integer
    mxINT32_CLASS,		// 32 bit signed integer
    mxUINT32_CLASS,		// 32 bit unsigned integer
    mxINT64_CLASS,		// 64 bit signed integer
    mxUINT64_CLASS,		// 64 bit unsigned integer
    mxFUNCTION_CLASS            // Function handle
  };

// Read COUNT elements of data from IS in the format specified by TYPE,
// placing the result in DATA.  If SWAP is TRUE, swap the bytes of
// each element before copying to DATA.  FLT_FMT specifies the format
// of the data if we are reading floating point numbers.

static void
read_mat5_binary_data (std::istream& is, double *data,
		       int count, bool swap, mat5_data_type type,
		       oct_mach_info::float_format flt_fmt)
{
  
  switch (type)
    {
    case miINT8:
      read_doubles (is, data, LS_CHAR, count, swap, flt_fmt);
      break;

    case miUTF8:
    case miUINT8:
      read_doubles (is, data, LS_U_CHAR, count, swap, flt_fmt);
      break;

    case miINT16:
      read_doubles (is, data, LS_SHORT, count, swap, flt_fmt);
      break;

    case miUINT16:
      read_doubles (is, data, LS_U_SHORT, count, swap, flt_fmt);
      break;

    case miINT32:
      read_doubles (is, data, LS_INT, count, swap, flt_fmt);
      break;

    case miUINT32:
      read_doubles (is, data, LS_U_INT, count, swap, flt_fmt);
      break;

    case miSINGLE:
      read_doubles (is, data, LS_FLOAT, count, swap, flt_fmt);
      break;

    case miRESERVE1:
      break;

    case miDOUBLE:
      read_doubles (is, data, LS_DOUBLE, count, swap, flt_fmt);
      break;

    case miRESERVE2:
    case miRESERVE3:
      break;

    case miINT64:
#ifdef EIGHT_BYTE_INT
      read_doubles (is, data, LS_LONG, count, swap, flt_fmt);
#endif
      break;

    case miUINT64:
#ifdef EIGHT_BYTE_INT
      read_doubles (is, data, LS_U_LONG, count, swap, flt_fmt);
#endif
      break;

    case miMATRIX:
    default:
      break;
    }
}

template <class T>
void
read_mat5_integer_data (std::istream& is, T *m, int count, bool swap,
			mat5_data_type type)
{

#define READ_INTEGER_DATA(TYPE, swap, data, size, len, stream)	\
  do \
    { \
      if (len > 0) \
	{ \
	  OCTAVE_LOCAL_BUFFER (TYPE, ptr, len); \
	  stream.read (reinterpret_cast<char *> (ptr), size * len); \
	  if (swap) \
	    swap_bytes< size > (ptr, len); \
	  for (int i = 0; i < len; i++) \
	    data[i] = ptr[i]; \
	} \
    } \
  while (0)

  switch (type)
    {
    case miINT8:
      READ_INTEGER_DATA (signed char, swap, m, 1, count, is);
      break;

    case miUINT8:
      READ_INTEGER_DATA (unsigned char, swap, m, 1, count, is);
      break;

    case miINT16:
      READ_INTEGER_DATA (signed TWO_BYTE_INT, swap, m, 2, count, is);
      break;

    case miUINT16:
      READ_INTEGER_DATA (unsigned TWO_BYTE_INT, swap, m, 2, count, is);
      break;

    case miINT32:
      READ_INTEGER_DATA (signed FOUR_BYTE_INT, swap, m, 4, count, is);
      break;

    case miUINT32:
      READ_INTEGER_DATA (unsigned FOUR_BYTE_INT, swap, m, 4, count, is);
      break;

    case miSINGLE:
    case miRESERVE1:
    case miDOUBLE:
    case miRESERVE2:
    case miRESERVE3:
      break;

    case miINT64:
#ifdef EIGHT_BYTE_INT
      READ_INTEGER_DATA (signed EIGHT_BYTE_INT, swap, m, 8, count, is);
#endif
      break;

    case miUINT64:
#ifdef EIGHT_BYTE_INT
      READ_INTEGER_DATA (unsigned EIGHT_BYTE_INT, swap, m, 8, count, is);
#endif
      break;

    case miMATRIX:
    default:
      break;
    }

#undef READ_INTEGER_DATA

}

template void read_mat5_integer_data (std::istream& is, octave_int8 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_int16 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_int32 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_int64 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_uint8 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_uint16 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_uint32 *m,
				      int count, bool swap,
				      mat5_data_type type);
template void read_mat5_integer_data (std::istream& is, octave_uint64 *m,
				      int count, bool swap,
				      mat5_data_type type);

template void read_mat5_integer_data (std::istream& is, int *m,
				      int count, bool swap,
				      mat5_data_type type);

#define OCTAVE_MAT5_INTEGER_READ(TYP) \
  { \
	TYP re (dims); \
  \
	std::streampos tmp_pos; \
  \
	if (read_mat5_tag (is, swap, type, len)) \
	  { \
	    error ("load: reading matrix data for `%s'", retval.c_str ()); \
	    goto data_read_error; \
	  } \
  \
	int n = re.length (); \
	tmp_pos = is.tellg (); \
	read_mat5_integer_data (is, re.fortran_vec (), n, swap,	\
				static_cast<enum mat5_data_type> (type)); \
  \
	if (! is || error_state) \
	  { \
	    error ("load: reading matrix data for `%s'", retval.c_str ()); \
	    goto data_read_error; \
	  } \
  \
	is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (len))); \
  \
	if (imag) \
	  { \
	    /* We don't handle imag integer types, convert to an array */ \
	    NDArray im (dims); \
  \
	    if (read_mat5_tag (is, swap, type, len)) \
	      { \
		error ("load: reading matrix data for `%s'", \
		       retval.c_str ()); \
		goto data_read_error; \
	      } \
  \
	    n = im.length (); \
	    read_mat5_binary_data (is, im.fortran_vec (), n, swap, \
				   static_cast<enum mat5_data_type> (type), flt_fmt); \
  \
	    if (! is || error_state) \
	      { \
		error ("load: reading imaginary matrix data for `%s'", \
		       retval.c_str ()); \
		goto data_read_error; \
	      } \
  \
	    ComplexNDArray ctmp (dims); \
  \
	    for (int i = 0; i < n; i++) \
	      ctmp(i) = Complex (double (re(i)), im(i)); \
  \
            tc = ctmp;  \
	  } \
	else \
	  tc = re; \
  }
  
// Read one element tag from stream IS, 
// place the type code in TYPE and the byte count in BYTES
// return nonzero on error
static int
read_mat5_tag (std::istream& is, bool swap, int& type, int& bytes)
{
  unsigned int upper;
  FOUR_BYTE_INT temp;

  if (! is.read (reinterpret_cast<char *> (&temp), 4 ))
    goto data_read_error;

  if (swap)
    swap_bytes<4> (&temp);

  upper = (temp >> 16) & 0xffff;
  type = temp & 0xffff;

  if (upper)
    {
      // "compressed" format
      bytes = upper;
    }
  else
    {
      if (! is.read (reinterpret_cast<char *> (&temp), 4 ))
	goto data_read_error;
      if (swap)
	swap_bytes<4> (&temp);
      bytes = temp;
    }

  return 0;

 data_read_error:
  return 1;
}

static void
read_int (std::istream& is, bool swap, FOUR_BYTE_INT& val)
{
  is.read (reinterpret_cast<char *> (&val), 4);

  if (swap)
    swap_bytes<4> (&val);
}

// Extract one data element (scalar, matrix, string, etc.) from stream
// IS and place it in TC, returning the name of the variable.
//
// The data is expected to be in Matlab's "Version 5" .mat format,
// though not all the features of that format are supported.
//
// FILENAME is used for error messages.

std::string
read_mat5_binary_element (std::istream& is, const std::string& filename,
			  bool swap, bool& global, octave_value& tc)
{
  std::string retval;

  // These are initialized here instead of closer to where they are
  // first used to avoid errors from gcc about goto crossing
  // initialization of variable.

  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;
  int type = 0;
  bool imag;
  bool logicalvar;
  enum arrayclasstype arrayclass;
  FOUR_BYTE_INT nzmax;
  FOUR_BYTE_INT flags;
  dim_vector dims;
  int len;
  int element_length;
  std::streampos pos;
  TWO_BYTE_INT number;
  number = *(TWO_BYTE_INT *)"\x00\x01";

  global = false;

  // MAT files always use IEEE floating point
  if ((number == 1) ^ swap)
    flt_fmt = oct_mach_info::flt_fmt_ieee_big_endian;
  else
    flt_fmt = oct_mach_info::flt_fmt_ieee_little_endian;

  // element type and length
  if (read_mat5_tag (is, swap, type, element_length))
    return retval;			// EOF

#ifdef HAVE_ZLIB
  if (type == miCOMPRESSED)
    {
      // If C++ allowed us direct access to the file descriptor of an ifstream 
      // in a uniform way, the code below could be vastly simplified, and 
      // additional copies of the data in memory wouldn't be needed!!

      OCTAVE_LOCAL_BUFFER (char, inbuf, element_length);
      is.read (inbuf, element_length);

      // We uncompress the first 8 bytes of the header to get the buffer length
      // This will fail with an error Z_MEM_ERROR
      uLongf destLen = 8;
      OCTAVE_LOCAL_BUFFER (unsigned int, tmp, 2);
      if (uncompress (reinterpret_cast<Bytef *> (tmp), &destLen, 
		      reinterpret_cast<Bytef *> (inbuf), element_length)
	  !=  Z_MEM_ERROR)
	{
	  // Why should I have to initialize outbuf as I'll just overwrite!!
	  if (swap)
	    swap_bytes<4> (tmp, 2);

	  destLen = tmp[1] + 8;
	  std::string outbuf (destLen, ' '); 

	  // XXX FIXME XXX -- find a way to avoid casting away const here!

	  int err = uncompress (reinterpret_cast<Bytef *> (const_cast<char *> (outbuf.c_str ())), &destLen, 
				reinterpret_cast<Bytef *> (inbuf), element_length);

	  if (err != Z_OK)
	    error ("load: error uncompressing data element");
	  else
	    {
	      std::istringstream gz_is (outbuf);
	      retval = read_mat5_binary_element (gz_is, filename, 
						 swap, global, tc);
	    }
	}
      else
	error ("load: error probing size of compressed data element");

      return retval;
    }
#endif

  if (type != miMATRIX)
    {
      error ("load: invalid element type");
      goto early_read_error;
    }

  if (element_length == 0)
    {
      tc = Matrix ();
      return retval;
    }

  pos = is.tellg ();

  // array flags subelement
  if (read_mat5_tag (is, swap, type, len) || type != miUINT32 || len != 8)
    {
      error ("load: invalid array flags subelement");
      goto early_read_error;
    }

  read_int (is, swap, flags);
  imag = (flags & 0x0800) != 0;	// has an imaginary part?
  global = (flags & 0x0400) != 0; // global variable?
  logicalvar = (flags & 0x0200) != 0; // boolean ?
  arrayclass = static_cast<arrayclasstype> (flags & 0xff);
  read_int (is, swap, nzmax);	// max number of non-zero in sparse
  
  // dimensions array subelement
  {
    FOUR_BYTE_INT dim_len;

    if (read_mat5_tag (is, swap, type, dim_len) || type != miINT32)
      {
	error ("load: invalid dimensions array subelement");
	goto early_read_error;
      }

    int ndims = dim_len / 4;
    dims.resize (ndims);
    for (int i = 0; i < ndims; i++)
      {
	FOUR_BYTE_INT n;
	read_int (is, swap, n);
	dims(i) = n;
      }

    std::streampos tmp_pos = is.tellg ();
    is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (dim_len) - dim_len));
  }

  if (read_mat5_tag (is, swap, type, len) || type != miINT8)
    {
      error ("load: invalid array name subelement");
      goto early_read_error;
    }

  {
    OCTAVE_LOCAL_BUFFER (char, name, len+1);

    // Structure field subelements have zero-length array name subelements.

    std::streampos tmp_pos = is.tellg ();

    if (len)
      {
	if (! is.read (name, len ))
	  goto data_read_error;
	
	is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (len)));
      }

    name[len] = '\0';
    retval = name;
  }

  switch (arrayclass)
    {
    case mxCELL_CLASS:
      {
	Cell cell_array (dims);

	int n = cell_array.length ();

	for (int i = 0; i < n; i++)
	  {
	    octave_value tc2;

	    std::string nm
	      = read_mat5_binary_element (is, filename, swap, global, tc2);

	    if (! is || error_state)
	      {
		error ("load: reading cell data for `%s'", nm.c_str ());
		goto data_read_error;
	      }

	    cell_array(i) = tc2;
	  }

	tc = cell_array;
      }
      break;

    case mxOBJECT_CLASS:
      warning ("load: objects are not implemented");
      goto skip_ahead;

    case mxSPARSE_CLASS:
#if SIZEOF_INT != SIZEOF_OCTAVE_IDX_TYPE
      warning ("load: sparse objects are not implemented");
      goto skip_ahead;
#else
      {
	int nr = dims(0);
	int nc = dims(1);
	SparseMatrix sm;
	SparseComplexMatrix scm;
	int *ridx;
	int *cidx;
	double *data;

	// Setup return value
	if (imag)
	  {
	    scm = SparseComplexMatrix (static_cast<octave_idx_type> (nr),
				       static_cast<octave_idx_type> (nc),
				       static_cast<octave_idx_type> (nzmax));
	    ridx = scm.ridx ();
	    cidx = scm.cidx ();
	    data = 0;
	  }
	else
	  {
	    sm = SparseMatrix (static_cast<octave_idx_type> (nr),
			       static_cast<octave_idx_type> (nc),
			       static_cast<octave_idx_type> (nzmax));
	    ridx = sm.ridx ();
	    cidx = sm.cidx ();
	    data = sm.data ();
	  }

	// row indices
	std::streampos tmp_pos;
	  
	if (read_mat5_tag (is, swap, type, len))
	  {
	    error ("load: reading sparse row data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	tmp_pos = is.tellg ();

	read_mat5_integer_data (is, ridx, nzmax, swap,
				static_cast<enum mat5_data_type> (type));

	if (! is || error_state)
	  {
	    error ("load: reading sparse row data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (len)));

	// col indices
	if (read_mat5_tag (is, swap, type, len))
	  {
	    error ("load: reading sparse column data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	tmp_pos = is.tellg ();

	read_mat5_integer_data (is, cidx, nc + 1, swap,
				static_cast<enum mat5_data_type> (type));

	if (! is || error_state)
	  {
	    error ("load: reading sparse column data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (len)));

	// real data subelement
	if (read_mat5_tag (is, swap, type, len))
	  {
	    error ("load: reading sparse matrix data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	FOUR_BYTE_INT nnz = cidx[nc];
	NDArray re;
	if (imag)
	  {
	    re = NDArray (dim_vector (static_cast<int> (nnz)));
	    data = re.fortran_vec ();
	  }

	tmp_pos = is.tellg ();
	read_mat5_binary_data (is, data, nnz, swap,
			       static_cast<enum mat5_data_type> (type), flt_fmt);

	if (! is || error_state)
	  {
	    error ("load: reading sparse matrix data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (len)));

	// imaginary data subelement
	if (imag)
	  {
	    NDArray im (dim_vector (static_cast<int> (nnz)));
	  
	    if (read_mat5_tag (is, swap, type, len))
	      {
		error ("load: reading sparse matrix data for `%s'", retval.c_str ());
		goto data_read_error;
	      }

	    read_mat5_binary_data (is, im.fortran_vec (), nnz, swap,
				   static_cast<enum mat5_data_type> (type), flt_fmt);

	    if (! is || error_state)
	      {
		error ("load: reading imaginary sparse matrix data for `%s'",
		       retval.c_str ());
		goto data_read_error;
	      }

	    for (int i = 0; i < nnz; i++)
	      scm.xdata (i) = Complex (re (i), im (i));

	    tc = scm;
	  }
	else
	  tc = sm;
      }
      break;
#endif

    case mxFUNCTION_CLASS:
      warning ("load: function handles are not implemented");
      goto skip_ahead;

    case mxSTRUCT_CLASS:
      {
	Octave_map m;
	FOUR_BYTE_INT fn_type;
	FOUR_BYTE_INT fn_len;
	FOUR_BYTE_INT field_name_length;

	// field name length subelement -- actually the maximum length
	// of a field name.  The Matlab docs promise this will always
	// be 32.  We read and use the actual value, on the theory
	// that eventually someone will recognize that's a waste of
	// space.
	if (read_mat5_tag (is, swap, fn_type, fn_len) || fn_type != miINT32)
	  {
	    error ("load: invalid field name subelement");
	    goto data_read_error;
	  }

	if (! is.read (reinterpret_cast<char *> (&field_name_length), fn_len ))
	  goto data_read_error;

	if (swap)
	  swap_bytes<4> (&field_name_length);

	// field name subelement.  The length of this subelement tells
	// us how many fields there are.
	if (read_mat5_tag (is, swap, fn_type, fn_len) || fn_type != miINT8)
	  {
	    error ("load: invalid field name subelement");
	    goto data_read_error;
	  }

	octave_idx_type n_fields = fn_len/field_name_length;

	fn_len = PAD (fn_len);

	OCTAVE_LOCAL_BUFFER (char, elname, fn_len);

	if (! is.read (elname, fn_len))
	  goto data_read_error;

	std::vector<Cell> elt (n_fields);

	for (octave_idx_type i = 0; i < n_fields; i++)
	  elt[i] = Cell (dims);

	octave_idx_type n = dims.numel ();

	// fields subelements
	for (octave_idx_type j = 0; j < n; j++)
	  {
	    for (octave_idx_type i = 0; i < n_fields; i++)
	      {
		octave_value fieldtc;
		read_mat5_binary_element (is, filename, swap, global, fieldtc);
		elt[i](j) = fieldtc;
	      }

	  }

	for (octave_idx_type i = 0; i < n_fields; i++)
	  {
	    const char *key = elname + i*field_name_length;

	    m.assign (key, elt[i]);
	  }

	tc = m;
      }
      break;

    case mxINT8_CLASS:
      OCTAVE_MAT5_INTEGER_READ (int8NDArray);
      break;

    case mxUINT8_CLASS:
      {
	OCTAVE_MAT5_INTEGER_READ (uint8NDArray);

	// logical variables can either be mxUINT8_CLASS or mxDOUBLE_CLASS,
	// so chek if we have a logical variable and convert it

	if (logicalvar)
	  {
	    uint8NDArray in = tc.uint8_array_value ();
	    int nel = in.nelem ();
	    boolNDArray out (dims);
	    
	    for (int i = 0; i < nel; i++)
	      out (i) = static_cast<bool> (double (in (i)));

	    tc = out;
	  }
      }
      break;

    case mxINT16_CLASS:
      OCTAVE_MAT5_INTEGER_READ (int16NDArray);
      break;

    case mxUINT16_CLASS:
      OCTAVE_MAT5_INTEGER_READ (uint16NDArray);
      break;

    case mxINT32_CLASS:
      OCTAVE_MAT5_INTEGER_READ (int32NDArray);
      break;

    case mxUINT32_CLASS:
      OCTAVE_MAT5_INTEGER_READ (uint32NDArray);
      break;

    case mxINT64_CLASS:
      OCTAVE_MAT5_INTEGER_READ (int64NDArray);
      break;

    case mxUINT64_CLASS:
      OCTAVE_MAT5_INTEGER_READ (uint64NDArray);
      break;

    case mxCHAR_CLASS:
      // handle as a numerical array to start with

    case mxDOUBLE_CLASS:
    case mxSINGLE_CLASS:
    default:
      {
	NDArray re (dims);
      
	// real data subelement

	std::streampos tmp_pos;
	  
	if (read_mat5_tag (is, swap, type, len))
	  {
	    error ("load: reading matrix data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	int n = re.length ();
	tmp_pos = is.tellg ();
	read_mat5_binary_data (is, re.fortran_vec (), n, swap,
			       static_cast<enum mat5_data_type> (type), flt_fmt);

	if (! is || error_state)
	  {
	    error ("load: reading matrix data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	is.seekg (tmp_pos + static_cast<std::streamoff> (PAD (len)));

	if (logicalvar)
	  {
	    // logical variables can either be mxUINT8_CLASS or mxDOUBLE_CLASS,
	    // so chek if we have a logical variable and convert it

	    boolNDArray out (dims);
	    
	    for (int i = 0; i < n; i++)
	      out (i) = static_cast<bool> (re (i));

	    tc = out;
	  }
	else if (imag)
	  {
	    // imaginary data subelement

	    NDArray im (dims);
	  
	    if (read_mat5_tag (is, swap, type, len))
	      {
		error ("load: reading matrix data for `%s'", retval.c_str ());
		goto data_read_error;
	      }

	    n = im.length ();
	    read_mat5_binary_data (is, im.fortran_vec (), n, swap,
				   static_cast<enum mat5_data_type> (type), flt_fmt);

	    if (! is || error_state)
	      {
		error ("load: reading imaginary matrix data for `%s'",
		       retval.c_str ());
		goto data_read_error;
	      }

	    ComplexNDArray ctmp (dims);

	    for (int i = 0; i < n; i++)
	      ctmp(i) = Complex (re(i), im(i));

	    tc = ctmp;
	  }
	else
	  {
	    if (arrayclass == mxCHAR_CLASS)
	      {
		if (type == miUTF16 || type == miUTF32)
		  {
		    error ("load: can not read Unicode UTF16 and UTF32 encoded characters");
		    goto data_read_error;
		  }
		else if (type == miUTF8)
		  {
		    // Search for multi-byte encoded UTF8 characters and
		    // replace with 0x3F for '?'... Give the user a warning

		    bool utf8_multi_byte = false;
		    for (int i = 0; i < n; i++)
		      {
			unsigned char a = static_cast<unsigned char> (re(i));
			if (a > 0x7f)
			  utf8_multi_byte = true;
		      }
		    
		    if (utf8_multi_byte)
		      {
			warning ("load: can not read multi-byte encoded UTF8 characters.");
			warning ("      Replacing unreadable characters with '?'.");
			for (int i = 0; i < n; i++)
			  {
			    unsigned char a = static_cast<unsigned char> (re(i));
			    if (a > 0x7f)
			      re(i) = '?';
			  }
		      }
		  }
		tc = re;
		tc = tc.convert_to_str (false, true, '\'');
	      }
	    else
	      tc = re;
	  }
      }
    }

  is.seekg (pos + static_cast<std::streamoff> (element_length));

  if (is.eof ())
    is.clear ();

  return retval;

 data_read_error:
 early_read_error:
  error ("load: trouble reading binary file `%s'", filename.c_str ());
  return std::string ();

 skip_ahead:
  warning ("skipping over `%s'", retval.c_str ());
  is.seekg (pos + static_cast<std::streamoff> (element_length));
  return read_mat5_binary_element (is, filename, swap, global, tc);
}

int
read_mat5_binary_file_header (std::istream& is, bool& swap, bool quiet)
{
  TWO_BYTE_INT version=0, magic=0;

  is.seekg (124, std::ios::beg);
  is.read (reinterpret_cast<char *> (&version), 2);
  is.read (reinterpret_cast<char *> (&magic), 2);

  if (magic == 0x4d49)
    swap = 0;
  else if (magic == 0x494d)
    swap = 1;
  else
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }

  if (! swap)			// version number is inverse swapped!
    version = ((version >> 8) & 0xff) + ((version & 0xff) << 8);

  if (version != 1 && !quiet)
    warning ("load: found version %d binary MAT file, "
	     "but only prepared for version 1", version);

  return 0;
}

static int 
write_mat5_tag (std::ostream& is, int type, int bytes)
{
  FOUR_BYTE_INT temp;

  if (bytes <= 4)
    temp = (bytes << 16) + type;
  else
    {
      temp = type;
      if (! is.write (reinterpret_cast<char *> (&temp), 4))
	goto data_write_error;
      temp = bytes;
    }

  if (! is.write (reinterpret_cast<char *> (&temp), 4))
    goto data_write_error;

  return 0;

 data_write_error:
  return 1;
}

// write out the numeric values in M to OS,
// preceded by the appropriate tag.
static void 
write_mat5_array (std::ostream& os, const NDArray& m, bool save_as_floats)
{
  int nel = m.nelem ();
  double max_val, min_val;
  save_type st = LS_DOUBLE;
  mat5_data_type mst;
  int size;
  unsigned len;
  const double *data = m.data ();

// Have to use copy here to avoid writing over data accessed via
// Matrix::data().

#define MAT5_DO_WRITE(TYPE, data, count, stream) \
  do \
    { \
      OCTAVE_LOCAL_BUFFER (TYPE, ptr, count); \
      for (int i = 0; i < count; i++) \
        ptr[i] = static_cast<TYPE> (data[i]); \
      stream.write (reinterpret_cast<char *> (ptr), count * sizeof (TYPE)); \
    } \
  while (0)

  if (save_as_floats)
    {
      if (m.too_large_for_float ())
	{
	  warning ("save: some values too large to save as floats --");
	  warning ("save: saving as doubles instead");
	}
      else
	st = LS_FLOAT;
    }

  if (m.all_integers (max_val, min_val))
    st = get_save_type (max_val, min_val);

  switch (st)
    {
    default:
    case LS_DOUBLE:  mst = miDOUBLE; size = 8; break;
    case LS_FLOAT:   mst = miSINGLE; size = 4; break;
    case LS_U_CHAR:  mst = miUINT8;  size = 1; break;
    case LS_U_SHORT: mst = miUINT16; size = 2; break;
    case LS_U_INT:   mst = miUINT32; size = 4; break;
    case LS_CHAR:    mst = miINT8;   size = 1; break;
    case LS_SHORT:   mst = miINT16;  size = 2; break;
    case LS_INT:     mst = miINT32;  size = 4; break;
    }

  len = nel*size;
  write_mat5_tag (os, mst, len);

  {
    switch (st)
      {
      case LS_U_CHAR:
	MAT5_DO_WRITE (unsigned char, data, nel, os);
	break;
	
      case LS_U_SHORT:
	MAT5_DO_WRITE (unsigned TWO_BYTE_INT, data, nel, os);
	break;
	
      case LS_U_INT:
	MAT5_DO_WRITE (unsigned FOUR_BYTE_INT, data, nel, os);
	break;
	
	// provide for 64 bit ints, even though get_save_type does
	// not yet implement them
#ifdef EIGHT_BYTE_INT
      case LS_U_LONG:
	MAT5_DO_WRITE (unsigned EIGHT_BYTE_INT, data, nel, os);
	break;
#endif

      case LS_CHAR:
	MAT5_DO_WRITE (signed char, data, nel, os);
	break;
	
      case LS_SHORT:
	MAT5_DO_WRITE (TWO_BYTE_INT, data, nel, os);
	break;

      case LS_INT:
	MAT5_DO_WRITE (FOUR_BYTE_INT, data, nel, os);
	break;

#ifdef EIGHT_BYTE_INT
      case LS_LONG:
	MAT5_DO_WRITE (EIGHT_BYTE_INT, data, nel, os);
	break;
#endif

      case LS_FLOAT:
	MAT5_DO_WRITE (float, data, nel, os);
	break;

      case LS_DOUBLE: // No conversion necessary.
	os.write (reinterpret_cast<const char *> (data), len);
	break;

      default:
	(*current_liboctave_error_handler)
	  ("unrecognized data format requested");
	break;
      }
  }
  if (PAD (len) > len)
    {
      static char buf[9]="\x00\x00\x00\x00\x00\x00\x00\x00";
      os.write (buf, PAD (len) - len);
    }
}

template <class T>
void 
write_mat5_integer_data (std::ostream& os, const T *m, int size, int nel)
{
  mat5_data_type mst;
  unsigned len;

  switch (size)
    {
    case 1:
      mst = miUINT8;
      break;
    case 2:
      mst = miUINT16;
      break;
    case 4:
      mst = miUINT32;
      break;
    case 8:
      mst = miUINT64;
      break;
    case -1:
      mst = miINT8;
      size = - size;
      break;
    case -2:
      mst = miINT16;
      size = - size;
      break;
    case -4:
      mst = miINT32;
      size = - size;
      break;
    case -8:
    default:
      mst = miINT64;
      size = - size;
      break;
    }

  len = nel*size;
  write_mat5_tag (os, mst, len);

  os.write (reinterpret_cast<const char *> (m), len);

  if (PAD (len) > len)
    {
      static char buf[9]="\x00\x00\x00\x00\x00\x00\x00\x00";
      os.write (buf, PAD (len) - len);
    }
}

template void write_mat5_integer_data (std::ostream& os, const octave_int8 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_int16 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_int32 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_int64 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_uint8 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_uint16 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_uint32 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const octave_uint64 *m,
				       int size, int nel);
template void write_mat5_integer_data (std::ostream& os, const int *m, 
				       int size, int nel);

// Write out cell element values in the cell array to OS, preceded by
// the appropriate tag.

static bool 
write_mat5_cell_array (std::ostream& os, const Cell& cell,
		       bool mark_as_global, bool save_as_floats)
{
  int nel = cell.nelem ();

  for (int i = 0; i < nel; i++)
    {
      octave_value ov = cell(i);

      if (! save_mat5_binary_element (os, ov, "", mark_as_global,
				      false, save_as_floats))
	return false;
    }

  return true;
}

int
save_mat5_array_length (const double* val, int nel, bool save_as_floats)
{
  if (nel > 0)
    {
      int size = 8;

      if (save_as_floats)
	{
	  bool too_large_for_float = false;
	  for (int i = 0; i < nel; i++)
	    {
	      double tmp = val [i];

	      if (! (xisnan (tmp) || xisinf (tmp))
		  && fabs (tmp) > FLT_MAX)
		{
		  too_large_for_float = true;
		  break;
		}
	    }

	  if (!too_large_for_float)
	    size = 4;
	}

      // The code below is disabled since get_save_type currently doesn't
      // deal with integer types. This will need to be activated if get_save_type
      // is changed.

      // double max_val = val[0];
      // double min_val = val[0];
      // bool all_integers =  true;
      //
      // for (int i = 0; i < nel; i++)
      //   {
      //     double val = val[i];
      //
      //     if (val > max_val)
      //       max_val = val;
      //
      //     if (val < min_val)
      //       min_val = val;
      //
      //     if (D_NINT (val) != val)
      //       {
      //         all_integers = false;
      //         break;
      //       }
      //   }
      //
      // if (all_integers)
      //   {
      //     if (max_val < 256 && min_val > -1)
      //       size = 1;
      //     else if (max_val < 65536 && min_val > -1)
      //       size = 2;
      //     else if (max_val < 4294967295UL && min_val > -1)
      //       size = 4;
      //     else if (max_val < 128 && min_val >= -128)
      //       size = 1;
      //     else if (max_val < 32768 && min_val >= -32768)
      //       size = 2;
      //     else if (max_val <= 2147483647L && min_val >= -2147483647L)
      //       size = 4;
      //   }

      return 8 + nel * size;
    }
  else
    return 8;
}

int
save_mat5_array_length (const Complex* val, int nel, bool save_as_floats)
{
  int ret;

  OCTAVE_LOCAL_BUFFER (double, tmp, nel);

  for (int i = 1; i < nel; i++)
    tmp[i] = std::real (val[i]);

  ret = save_mat5_array_length (tmp, nel, save_as_floats);

  for (int i = 1; i < nel; i++)
    tmp[i] = std::imag (val[i]);

  ret += save_mat5_array_length (tmp, nel, save_as_floats);

  return ret;
}

int
save_mat5_element_length (const octave_value& tc, const std::string& name, 
			  bool save_as_floats, bool mat7_format)
{
  int max_namelen = (mat7_format ? 63 : 31);
  int len = name.length ();
  std::string cname = tc.class_name ();
  int ret = 32;

  if (len > 4)
    ret += PAD (len > max_namelen ? max_namelen : len);

  ret += PAD (4 * tc.ndims ());
  
  if (tc.is_string ())
    {
      charMatrix chm = tc.char_matrix_value ();
      ret += 8;
      if (chm.nelem () > 1)
	ret += PAD (2 * chm.rows () * chm.cols ());
    }
  else if (cname == "sparse")
    {
      if (tc.is_complex_type ())
	{
	  SparseComplexMatrix m = tc.sparse_complex_matrix_value ();
	  int nc = m.cols ();
	  int nnz = m.nzmax ();

	  ret += 16 + PAD (nnz * sizeof (int)) + PAD ((nc + 1) * sizeof (int)) +
	    save_mat5_array_length (m.data (), m.nelem (), save_as_floats);
	}
      else
	{
	  SparseMatrix m = tc.sparse_matrix_value ();
	  int nc = m.cols ();
	  int nnz = m.nzmax ();

	  ret += 16 + PAD (nnz * sizeof (int)) + PAD ((nc + 1) * sizeof (int)) +
	    save_mat5_array_length (m.data (), m.nelem (), save_as_floats);
	}
    }

#define INT_LEN(nel, size) \
  { \
    ret += 8; \
    int sz = nel * size; \
    if (sz > 4) \
      ret += PAD (sz);	\
  }

  else if (cname == "int8")
    INT_LEN (tc.int8_array_value ().nelem (), 1)
  else if (cname == "int16")
    INT_LEN (tc.int16_array_value ().nelem (), 2)
  else if (cname == "int32")
    INT_LEN (tc.int32_array_value ().nelem (), 4)
  else if (cname == "int64")
    INT_LEN (tc.int64_array_value ().nelem (), 8)
  else if (cname == "uint8")
    INT_LEN (tc.uint8_array_value ().nelem (), 1)
  else if (cname == "uint16")
    INT_LEN (tc.uint16_array_value ().nelem (), 2)
  else if (cname == "uint32")
    INT_LEN (tc.uint32_array_value ().nelem (), 4)
  else if (cname == "uint64")
    INT_LEN (tc.uint64_array_value ().nelem (), 8)
  else if (tc.is_bool_type ())
    INT_LEN (tc.bool_array_value ().nelem (), 1)
  else if (tc.is_real_scalar () || tc.is_real_matrix () || tc.is_range ())
    {
      NDArray m = tc.array_value ();
      ret += save_mat5_array_length (m.fortran_vec (), m.nelem (),
				     save_as_floats);
    }
  else if (tc.is_cell ())
    {
      Cell cell = tc.cell_value ();
      int nel = cell.nelem ();

      for (int i = 0; i < nel; i++)
	ret += 8 + 
	  save_mat5_element_length (cell (i), "", save_as_floats, mat7_format);
    }
  else if (tc.is_complex_scalar () || tc.is_complex_matrix ()) 
    {
      ComplexNDArray m = tc.complex_array_value ();
      ret += save_mat5_array_length (m.fortran_vec (), m.nelem (),
				     save_as_floats);
    }
  else if (tc.is_map ()) 
    {
      int fieldcnt = 0;
      const Octave_map m = tc.map_value ();
      int nel = m.numel ();

      for (Octave_map::const_iterator i = m.begin (); i != m.end (); i++)
	fieldcnt++;

      ret += 16 + fieldcnt * (max_namelen + 1);


      for (int j = 0; j < nel; j++)
	{

	  for (Octave_map::const_iterator i = m.begin (); i != m.end (); i++)
	    {
	      Cell elts = m.contents (i);

	      ret += 8 + save_mat5_element_length (elts (j), "", 
					       save_as_floats, mat7_format);
	    }
	}
    }
  else
    ret = -1;

  return ret;
}

// save the data from TC along with the corresponding NAME on stream
// OS in the MatLab version 5 binary format.  Return true on success.

bool
save_mat5_binary_element (std::ostream& os,
			  const octave_value& tc, const std::string& name,
			  bool mark_as_global, bool mat7_format,
			  bool save_as_floats, bool compressing) 
{
  FOUR_BYTE_INT flags=0;
  FOUR_BYTE_INT nnz=0;
  std::streampos fixup, contin;
  std::string cname = tc.class_name ();
  int max_namelen = (mat7_format ? 63 : 31);

#ifdef HAVE_ZLIB
  if (mat7_format && !compressing)
    {
      bool ret = false;

      std::ostringstream buf;

      // The code seeks backwards in the stream to fix the header. Can't
      // do this with zlib, so use a stringstream.
      ret = save_mat5_binary_element (buf, tc, name, mark_as_global, true,
				      save_as_floats, true);

      if (ret)
	{
	  // destLen must be at least 0.1% larger than source buffer 
	  // + 12 bytes. Reality is it must be larger again than that.
	  std::string buf_str = buf.str ();
	  uLongf srcLen = buf_str.length ();
	  uLongf destLen = srcLen * 101 / 100 + 12; 
	  OCTAVE_LOCAL_BUFFER (char, out_buf, destLen);

	  if (compress (reinterpret_cast<Bytef *> (out_buf), &destLen, 
			reinterpret_cast<const Bytef *> (buf_str.c_str ()), srcLen) == Z_OK)
	    {
	      write_mat5_tag (os, miCOMPRESSED, static_cast<int> (destLen)); 
	      os.write (out_buf, destLen);
	    }
	  else
	    {
	      error ("save: error compressing data element");
	      ret = false;
	    }
	}

      return ret;
    }
#endif

  // element type and length
  fixup = os.tellp ();
  write_mat5_tag (os, miMATRIX, save_mat5_element_length 
		  (tc, name, save_as_floats, mat7_format));
  
  // array flags subelement
  write_mat5_tag (os, miUINT32, 8);

  if (tc.is_bool_type ())
    flags |= 0x0200;

  if (mark_as_global)
    flags |= 0x0400;

  if (tc.is_complex_scalar () || tc.is_complex_matrix ())
    flags |= 0x0800;

  if (tc.is_string ())
    flags |= mxCHAR_CLASS;
  else if (cname == "int8")
    flags |= mxINT8_CLASS;
  else if (cname == "int16")
    flags |= mxINT16_CLASS;
  else if (cname == "int32")
    flags |= mxINT32_CLASS;
  else if (cname == "int64")
    flags |= mxINT64_CLASS;
  else if (cname == "uint8" || tc.is_bool_type ())
    flags |= mxUINT8_CLASS;
  else if (cname == "uint16")
    flags |= mxUINT16_CLASS;
  else if (cname == "uint32")
    flags |= mxUINT32_CLASS;
  else if (cname == "uint64")
    flags |= mxUINT64_CLASS;
  else if (cname == "sparse")
    {
      flags |= mxSPARSE_CLASS;
      if (tc.is_complex_type ())
	{
	  SparseComplexMatrix scm = tc.sparse_complex_matrix_value ();
	  nnz = scm.nzmax ();
	}
      else
	{
	  SparseMatrix sm = tc.sparse_matrix_value ();
	  nnz = sm.nzmax ();
	}
    }
  else if (tc.is_real_scalar ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_real_matrix () || tc.is_range ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_complex_scalar ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_complex_matrix ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_map ()) 
    flags |= mxSTRUCT_CLASS;
  else if (tc.is_cell ())
    flags |= mxCELL_CLASS;
  else
    {
      gripe_wrong_type_arg ("save", tc, false);
      goto error_cleanup;
    }

  os.write (reinterpret_cast<char *> (&flags), 4);
  os.write (reinterpret_cast<char *> (&nnz), 4);

  {
    dim_vector dv = tc.dims ();
    int nd = tc.ndims ();
    int dim_len = 4*nd;

    write_mat5_tag (os, miINT32, dim_len);

    for (int i = 0; i < nd; i++)
      {
	FOUR_BYTE_INT n = dv(i);
	os.write (reinterpret_cast<char *> (&n), 4);
      }

    if (PAD (dim_len) > dim_len)
      {
	static char buf[9]="\x00\x00\x00\x00\x00\x00\x00\x00";
	os.write (buf, PAD (dim_len) - dim_len);
      }
  }

  // array name subelement
  {
    int namelen = name.length ();

    if (namelen > max_namelen)
      namelen = max_namelen; // only 31 or 63 char names permitted in mat file

    int paddedlength = PAD (namelen);

    write_mat5_tag (os, miINT8, namelen);
    OCTAVE_LOCAL_BUFFER (char, paddedname, paddedlength);
    memset (paddedname, 0, paddedlength);
    strncpy (paddedname, name.c_str (), namelen);
    os.write (paddedname, paddedlength);
  }

  // data element
  if (tc.is_string ())
    {
      charMatrix chm = tc.char_matrix_value ();
      int nr = chm.rows ();
      int nc = chm.cols ();
      int len = nr*nc*2;
      int paddedlength = PAD (nr*nc*2);

      OCTAVE_LOCAL_BUFFER (TWO_BYTE_INT, buf, nc*nr+3);
      write_mat5_tag (os, miUINT16, len);

      for (int i = 0; i < nr; i++)
	{
	  std::string tstr = chm.row_as_string (i);
	  const char *s = tstr.data ();

	  for (int j = 0; j < nc; j++)
	    buf[j*nr+i] = *s++ & 0x00FF;
	}
      os.write (reinterpret_cast<char *> (buf), nr*nc*2);
      
      if (paddedlength > len)
	os.write (reinterpret_cast<char *> (buf), paddedlength - len);
    }
  else if (cname == "sparse")
    {
      if (tc.is_complex_type ())
	{
	  SparseComplexMatrix m = tc.sparse_complex_matrix_value ();
	  int nc = m.cols ();

	  write_mat5_integer_data (os, m.ridx (), - sizeof(int), nnz);
	  write_mat5_integer_data (os, m.cidx (), - sizeof(int), nc + 1);

	  NDArray buf (dim_vector (nnz, 1));

	  for (int i = 0; i < nnz; i++)
	    buf (i) = std::real (m.data (i));

	  write_mat5_array (os, buf, save_as_floats);

	  for (int i = 0; i < nnz; i++)
	    buf (i) = std::imag (m.data (i));

	  write_mat5_array (os, buf, save_as_floats);
	}
      else
	{
	  SparseMatrix m = tc.sparse_matrix_value ();
	  int nc = m.cols ();

	  write_mat5_integer_data (os, m.ridx (), - sizeof(int), nnz);
	  write_mat5_integer_data (os, m.cidx (), - sizeof(int), nc + 1);

	  // XXX FIXME XXX
	  // Is there a way to easily do without this buffer
	  NDArray buf (dim_vector (nnz, 1));

	  for (int i = 0; i < nnz; i++)
	    buf (i) = m.data (i);

	  write_mat5_array (os, buf, save_as_floats);
	}
    }
  else if (cname == "int8")
    {
      int8NDArray m = tc.int8_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), -1, m.nelem ());
    }
  else if (cname == "int16")
    {
      int16NDArray m = tc.int16_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), -2, m.nelem ());
    }
  else if (cname == "int32")
    {
      int32NDArray m = tc.int32_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), -4, m.nelem ());
    }
  else if (cname == "int64")
    {
      int64NDArray m = tc.int64_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), -8, m.nelem ());
    }
  else if (cname == "uint8")
    {
      uint8NDArray m = tc.uint8_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), 1, m.nelem ());
    }
  else if (cname == "uint16")
    {
      uint16NDArray m = tc.uint16_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), 2, m.nelem ());
    }
  else if (cname == "uint32")
    {
      uint32NDArray m = tc.uint32_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), 4, m.nelem ());
    }
  else if (cname == "uint64")
    {
      uint64NDArray m = tc.uint64_array_value ();

      write_mat5_integer_data (os, m.fortran_vec (), 8, m.nelem ());
    }
  else if (tc.is_bool_type ())
    {
      uint8NDArray m (tc.bool_array_value ());

      write_mat5_integer_data (os, m.fortran_vec (), 1, m.nelem ());
    }
  else if (tc.is_real_scalar () || tc.is_real_matrix () || tc.is_range ())
    {
      NDArray m = tc.array_value ();

      write_mat5_array (os, m, save_as_floats);
    }
  else if (tc.is_cell ())
    {
      Cell cell = tc.cell_value ();

      if (! write_mat5_cell_array (os, cell, mark_as_global, save_as_floats))
	goto error_cleanup;
    }
  else if (tc.is_complex_scalar () || tc.is_complex_matrix ()) 
    {
      ComplexNDArray m_cmplx = tc.complex_array_value ();

      write_mat5_array (os, ::real (m_cmplx), save_as_floats);
      write_mat5_array (os, ::imag (m_cmplx), save_as_floats);
    }
  else if (tc.is_map ()) 
    {
      // an Octave structure */
      // recursively write each element of the structure
      const Octave_map m = tc.map_value ();

      {
	char buf[64];
	FOUR_BYTE_INT maxfieldnamelength = max_namelen + 1;
	int fieldcnt = 0;

	for (Octave_map::const_iterator i = m.begin (); i != m.end (); i++)
	  fieldcnt++;

	write_mat5_tag (os, miINT32, 4);
	os.write (reinterpret_cast<char *> (&maxfieldnamelength), 4);
	write_mat5_tag (os, miINT8, fieldcnt*maxfieldnamelength);

	for (Octave_map::const_iterator i = m.begin (); i != m.end (); i++)
	  {
	    // write the name of each element
	    std::string tstr = m.key (i);
	    memset (buf, 0, max_namelen + 1);
	    strncpy (buf, tstr.c_str (), max_namelen); // only 31 or 63 char names permitted
	    os.write (buf, max_namelen + 1);
	  }

	int len = m.numel ();

	for (int j = 0; j < len; j++)
	  {
	    // write the data of each element

	    for (Octave_map::const_iterator i = m.begin (); i != m.end (); i++)
	      {
		Cell elts = m.contents (i);

		bool retval2 = save_mat5_binary_element (os, elts(j), "",
							 mark_as_global,
							 false, 
							 save_as_floats);
		if (! retval2)
		  goto error_cleanup;
	      }
	  }
      }
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  contin = os.tellp ();

  return true;

 error_cleanup:
  error ("save: error while writing `%s' to MAT file", name.c_str ());

  return false;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

