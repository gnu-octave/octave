////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020 The Octave Project Developers
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

#include "builtin-defun-decls.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "oct-string.h"
#include "ovl.h"

#if defined (HAVE_RAPIDJSON)
#  include <rapidjson/prettywriter.h>
#  include <rapidjson/stringbuffer.h>
#  include <rapidjson/writer.h>
#endif

#if defined (HAVE_RAPIDJSON)

//! Encodes a scalar Octave value into a numerical JSON value.
//!
//! @param writer RapidJSON's writer that is responsible for generating json.
//! @param obj scalar Octave value.
//! @param ConvertInfAndNaN @c bool that converts @c Inf and @c NaN to @c null.
//!
//! @b Example:
//!
//! @code{.cc}
//! octave_value obj (7);
//! encode_numeric (writer, obj,true);
//! @endcode

template <typename T> void
encode_numeric (T& writer, const octave_value& obj, const bool& ConvertInfAndNaN)
{
  double value =  obj.scalar_value ();
  if (obj.is_bool_scalar ())
    writer.Bool (obj.bool_value ());
  // Any numeric input from the interpreter will be in double type so in order
  // to detect ints, we will check if the floor of the input and the input are
  // equal using fabs(A - B) < epsilon method as it is more accurate.
  // If value > 999999, MATLAB will encode it in scientific notation (double)
  else if (fabs (floor (value) - value) < std::numeric_limits<double>::epsilon ()
           && value <= 999999 && value >= -999999)
    writer.Int64 (value);
  // NA values doesn't exist in MATLAB, so I will decode it as null instead
  else if (((octave::math::isnan (value) || std::isinf (value)
            || std::isinf (-value)) && ConvertInfAndNaN)
           || obj.isna ().bool_value ())
    writer.Null ();
  else if (obj.is_double_type ())
    writer.Double (value);
  else
    error ("jsonencode: Unsupported type.");
}

//! Encodes character vectors and arrays into JSON strings.
//!
//! @param writer RapidJSON's writer that is responsible for generating json.
//! @param obj character vectors or character arrays.
//! @param original_dims The original dimensions of the array being encoded.
//! @param level The level of recursion for the function.
//!
//! @b Example:
//!
//! @code{.cc}
//! octave_value obj ("foo");
//! encode_string (writer, obj,true);
//! @endcode

template <typename T> void
encode_string (T& writer, const octave_value& obj,
               const dim_vector& original_dims, int level = 0)
{
  charNDArray array = obj.char_array_value ();
  if (array.isempty ())
    writer.String ("");
  else if (array.isvector ())
    {
      // Handle the special case when the input is a vector with more than
      // 2 dimensions (e.g. cat (8, ['a'], ['c'])). In this case, we don't
      // split the inner vectors of the input. we merge them into one.
      if (level == 0)
        {
          std::string char_vector = "";
          for (octave_idx_type i = 0; i < array.numel (); ++i)
            char_vector += array(i);
          writer.String (char_vector.c_str ());
        }
      else
        for (octave_idx_type i = 0; i < array.numel () / original_dims(1); ++i)
          {
            std::string char_vector = "";
            for (octave_idx_type k = 0; k < original_dims(1); ++k)
              char_vector += array(i * original_dims(1) + k);
            writer.String (char_vector.c_str ());
          }
    }
  else
    {
      octave_idx_type idx;
      octave_idx_type ndims = array.ndims ();
      dim_vector dims = array.dims ();

      // In this case, we already have a vector. So,  we transform it to 2-D
      // vector in order to be detected by "isvector" in the recursive call
      if (dims.num_ones () == ndims - 1)
      {
        // Handle the special case when the input is a vector with more than
        // 2 dimensions (e.g. cat (8, ['a'], ['c'])). In this case, we don't
        // add dimension brackets and treat it as if it is a vector
        if (level != 0)
          // Place an opening and a closing bracket (represents a dimension)
          // for every dimension that equals 1 till we reach the 2-D vector
          for (int i = level; i < ndims - 1; ++i)
            writer.StartArray ();

        encode_string (writer, array.as_row (), original_dims, level);

        if (level != 0)
          for (int i = level; i < ndims - 1; ++i)
            writer.EndArray ();
      }
      else
        {
          // We place an opening and a closing bracket for each dimension
          // that equals 1 to preserve the number of dimensions when decoding
          // the array after encoding it.
          if (original_dims (level) == 1 && level != 1)
          {
            writer.StartArray ();
            encode_string (writer, array, original_dims, level + 1);
            writer.EndArray ();
          }
          else
            {
              // The second dimension contains the number of the chars in
              // the char vector. We want to treat them as a one object,
              // so we replace it with 1
              dims(1) = 1;

              for (idx = 0; idx < ndims; ++idx)
                if (dims(idx) != 1)
                  break;
              // Create the dimensions that will be used to call "num2cell"
              // We called "num2cell" to divide the array to smaller sub arrays
              // in order to encode it recursively.
              // The recursive encoding is necessary to support encoding of
              // higher-dimensional arrays.
              RowVector conversion_dims;
              conversion_dims.resize (ndims - 1);
              for (octave_idx_type i = 0; i < idx; ++i)
                conversion_dims(i) = i + 1;
              for (octave_idx_type i = idx ; i < ndims - 1; ++i)
                conversion_dims(i) = i + 2;

              octave_value_list args (obj);
              args.append (conversion_dims);

              Cell sub_arrays = Fnum2cell (args)(0).cell_value ();

              writer.StartArray ();

              for (octave_idx_type i = 0; i < sub_arrays.numel (); ++i)
                encode_string (writer, sub_arrays(i), original_dims,
                               level + 1);

              writer.EndArray ();
            }
        }
    }
}

//! Encodes a struct Octave value into a JSON object or a JSON array depending
//! on the type of the struct (scalar struct or struct array.)
//!
//! @param writer RapidJSON's writer that is responsible for generating json.
//! @param obj struct Octave value.
//! @param ConvertInfAndNaN @c bool that converts @c Inf and @c NaN to @c null.
//!
//! @b Example:
//!
//! @code{.cc}
//! octave_value obj (octave_map ());
//! encode_struct (writer, obj,true);
//! @endcode

template <typename T> void
encode_struct (T& writer, const octave_value& obj, const bool& ConvertInfAndNaN)
{
  octave_map struct_array = obj.map_value ();
  octave_idx_type numel = struct_array.numel ();
  string_vector keys = struct_array.keys ();

  if (numel > 1)
    writer.StartArray ();

  for (octave_idx_type i = 0; i < numel; ++i)
    {
      writer.StartObject ();
      for (octave_idx_type k = 0; k < keys.numel (); ++k)
        {
          writer.Key (keys(k).c_str ());
          encode (writer, struct_array(i).getfield (keys(k)), ConvertInfAndNaN);
        }
      writer.EndObject ();
    }

  if (numel > 1)
    writer.EndArray ();
}

//! Encodes a Cell Octave value into a JSON array
//!
//! @param writer RapidJSON's writer that is responsible for generating json.
//! @param obj Cell Octave value.
//! @param ConvertInfAndNaN @c bool that converts @c Inf and @c NaN to @c null.
//!
//! @b Example:
//!
//! @code{.cc}
//! octave_value obj (cell ());
//! encode_cell (writer, obj,true);
//! @endcode

template <typename T> void
encode_cell (T& writer, const octave_value& obj, const bool& ConvertInfAndNaN)
{
  Cell cell = obj.cell_value ();

  writer.StartArray ();

  for (octave_idx_type i = 0; i < cell.numel (); ++i)
    encode (writer, cell(i), ConvertInfAndNaN);

  writer.EndArray ();
}

//! Encodes a numeric or logical Octave array into a JSON array
//!
//! @param writer RapidJSON's writer that is responsible for generating json.
//! @param obj numeric or logical Octave array.
//! @param ConvertInfAndNaN @c bool that converts @c Inf and @c NaN to @c null.
//! @param original_dims The original dimensions of the array being encoded.
//! @param level The level of recursion for the function.
//!
//! @b Example:
//!
//! @code{.cc}
//! octave_value obj (NDArray ());
//! encode_array (writer, obj,true);
//! @endcode

template <typename T> void
encode_array (T& writer, const octave_value& obj, const bool& ConvertInfAndNaN,
              const dim_vector& original_dims, int level = 0)
{
  NDArray array = obj.array_value ();
  if (array.isempty ())
    {
      writer.StartArray ();
      writer.EndArray ();
    }
  else if (array.isvector ())
    {
      writer.StartArray ();
      for (octave_idx_type i = 0; i < array.numel (); ++i)
        {
          if (obj.islogical ())
            encode_numeric (writer, bool (array(i)), ConvertInfAndNaN);
          else
            encode_numeric (writer, array(i), ConvertInfAndNaN);
        }
      writer.EndArray ();
    }
  else
    {
      octave_idx_type idx;
      octave_idx_type ndims = array.ndims ();
      dim_vector dims = array.dims ();

      // In this case, we already have a vector. So,  we transform it to 2-D
      // vector in order to be detected by "isvector" in the recursive call
      if (dims.num_ones () == ndims - 1)
        {
          // Handle the special case when the input is a vector with more than
          // 2 dimensions (e.g. ones ([1 1 1 1 1 6])). In this case, we don't
          // add dimension brackets and treat it as if it is a vector
          if (level != 0)
            // Place an opening and a closing bracket (represents a dimension)
            // for every dimension that equals 1 till we reach the 2-D vector
            for (int i = level; i < ndims - 1; ++i)
              writer.StartArray ();

          encode_array (writer, array.as_row (), ConvertInfAndNaN,
                        original_dims);

          if (level != 0)
            for (int i = level; i < ndims - 1; ++i)
              writer.EndArray ();
        }
      else
        {
          // We place an opening and a closing bracket for each dimension
          // that equals 1 to preserve the number of dimensions when decoding
          // the array after encoding it.
          if (original_dims (level) == 1)
          {
            writer.StartArray ();
            encode_array (writer, array, ConvertInfAndNaN,
                          original_dims, level + 1);
            writer.EndArray ();
          }
          else
            {
              for (idx = 0; idx < ndims; ++idx)
                if (dims(idx) != 1)
                  break;

              // Create the dimensions that will be used to call "num2cell"
              // We called "num2cell" to divide the array to smaller sub arrays
              // in order to encode it recursively.
              // The recursive encoding is necessary to support encoding of
              // higher-dimensional arrays.
              RowVector conversion_dims;
              conversion_dims.resize (ndims - 1);
              for (octave_idx_type i = 0; i < idx; ++i)
                conversion_dims(i) = i + 1;
              for (octave_idx_type i = idx ; i < ndims - 1; ++i)
                conversion_dims(i) = i + 2;

              octave_value_list args (obj);
              args.append (conversion_dims);

              Cell sub_arrays = Fnum2cell (args)(0).cell_value ();

              writer.StartArray ();

              for (octave_idx_type i = 0; i < sub_arrays.numel (); ++i)
                encode_array (writer, sub_arrays(i), ConvertInfAndNaN,
                              original_dims, level + 1);

              writer.EndArray ();
            }
        }
    }
}

//! Encodes any Octave object. This function only serves as an interface
//! by choosing which function to call from the previous functions.
//!
//! @param writer RapidJSON's writer that is responsible for generating json.
//! @param obj any @ref octave_value that is supported.
//! @param ConvertInfAndNaN @c bool that converts @c Inf and @c NaN to @c null.
//!
//! @b Example:
//!
//! @code{.cc}
//! octave_value obj (true);
//! encode (writer, obj,true);
//! @endcode

template <typename T> void
encode (T& writer, const octave_value& obj, const bool& ConvertInfAndNaN)
{
  if (obj.is_real_scalar ())
    encode_numeric (writer, obj, ConvertInfAndNaN);
  // As I checked for scalars, this will detect numeric & logical arrays
  else if (obj.isnumeric () || obj.islogical ())
    encode_array (writer, obj, ConvertInfAndNaN, obj.dims ());
  else if (obj.is_string ())
    encode_string (writer, obj, obj.dims ());
  else if (obj.isstruct ())
    encode_struct (writer, obj, ConvertInfAndNaN);
  else if (obj.iscell ())
    encode_cell (writer, obj, ConvertInfAndNaN);
  else if (obj.class_name () == "containers.Map")
    // To extract the data in containers.Map, Convert it to a struct.
    // The struct will have a "map" field that its value is a struct that
    // contains the desired data.
    // In order to convert it we will need to disable the
    // "Octave:classdef-to-struct" warning and re-enable it.
    {
      set_warning_state ("Octave:classdef-to-struct", "off");
      encode_struct (writer, obj.scalar_map_value ().getfield ("map"),
                     ConvertInfAndNaN);
      set_warning_state ("Octave:classdef-to-struct", "on");
    }
  else if (obj.isobject ())
    {
      set_warning_state ("Octave:classdef-to-struct", "off");
      encode_struct (writer, obj.scalar_map_value (), ConvertInfAndNaN);
      set_warning_state ("Octave:classdef-to-struct", "on");
    }
  else
    error ("jsonencode: Unsupported type.");
}

#endif

DEFUN (jsonencode, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{json} =} jsonencode (@var{object})
@deftypefnx {} {@var{json} =} jsonencode (@var{object}, "ConvertInfAndNaN", @var{conv})
@deftypefnx {} {@var{json} =} jsonencode (@var{object}, "PrettyWriter", @var{pretty})
@deftypefnx {} {@var{json} =} jsonencode (@var{object}, @dots{})

Encode Octave's data types into JSON text.

The input @var{object} is the Octave's object we want to encode.
The output @var{json} is the JSON text that contains the result
of encoding @var{object}.

If the value of the option @qcode{"ConvertInfAndNaN"} is true, @qcode{"NaN"},
@qcode{"Inf"} and @qcode{"-Inf"} values will be converted to @qcode{"null"}
value in the output.  If it is false, they will remain with their
original values. The default value for this option is true.

If the value of the option @qcode{"PrettyWriter"} is true, the output text will
have indentations and line feeds.  If it is false, the output will be condensed
and without any white-spaces.  The default value for this option is false.

-NOTES:
@itemize @bullet
@item
Complex numbers are not supported.

@item
@qcode{"classdef"} objects and @qcode{"containers.Map"} objects are converted
into structs then encoded as structs.

@item
To preserve the escape characters (e.g. "\n"), use double-quote strings.

@item
Every character after the null character ("\0") in double-quoted strings will
be dropped during encoding.

@item
It is not guaranteed to get the same dimensions for arrays if you encode
and then decode it.  For example, if you encoded a row vector then decoded it,
you will get a column vector.

@item
It is not guaranteed to get the same data type if you encode and then decode
an Octave value as Octave supports more data types than JSON.  For example, if
you encoded an @qcode{"int32"} then decoded it, you will get a @qcode{"double"}.
@end itemize


This table shows the conversions from Octave data types to JSON data types:

@table @asis
@item Scalar @qcode{"logical"}
@qcode{"Boolean"}

@item @qcode{"NaN"}, @qcode{"Inf"}, @qcode{"-Inf"}
@qcode{"null"}

@item Scalar numeric
@qcode{"Number"}

@item @qcode{"Numeric vector"}
@qcode{"Numeric array"}

@item @qcode{"Numeric array"}
Nested @qcode{"numeric array"}

@item @qcode{"Logical vector"}
@qcode{"Boolean array"}

@item @qcode{"Logical array"}
Nested @qcode{"boolean array"}

@item @qcode{"Char vector"}
@qcode{"String"}

@item @qcode{"Char array"}
Nested @qcode{"string array"}

@item Scalar @qcode{"cell"}
@qcode{"array"} with a single element

@item @qcode{"Cell vector"}
@qcode{"Array"}

@item @qcode{"Cell array"}
single dimensional @qcode{"array"}

@item Scalar @qcode{"struct"}
@qcode{"JSON Object"}

@item @qcode{"Struct vector"}
@qcode{"JSON objects array"}

@item @qcode{"Struct array"}
Nested @qcode{"JSON objects array"}

@item @qcode{"classdef"} objects
@qcode{"JSON object"}

@item @qcode{"containers.Map"}
@qcode{"JSON object"}
@end table

Examples:

@example
@group
jsonencode ([1 NaN; 3 4])
@result{} [[1,null],[3,4]]
@end group

@group
jsonencode ([1 NaN; 3 4], "ConvertInfAndNaN", false)
@result{} [[1,NaN],[3,4]]
@end group

@group
## Escape characters inside a single-quoted string
jsonencode ('\0\a\b\t\n\v\f\r')
@result{} "\\0\\a\\b\\t\\n\\v\\f\\r"
@end group

@group
## Escape characters inside a double-quoted string
jsonencode ("\a\b\t\n\v\f\r")
@result{} "\u0007\b\t\n\u000B\f\r"
@end group

@group
jsonencode ([true; false], "ConvertInfAndNaN", false, "PrettyWriter", true)
@result{} ans = [
       true,
       false
   ]
@end group

@group
jsonencode (['foo', 'bar'; 'foo', 'bar'])
@result{} ["foobar","foobar"]
@end group

@group
jsonencode (struct ('a', Inf, 'b', [], 'c', struct ()))
@result{} @{"a":null,"b":[],"c":@{@}@}
@end group

@group
jsonencode (struct ('structarray', struct ('a', @{1; 3@}, 'b', @{2; 4@})))
@result{} @{"structarray":[@{"a":1,"b":2@},@{"a":3,"b":4@}]@}
@end group

@group
jsonencode (@{'foo'; 'bar'; @{'foo'; 'bar'@}@})
@result{} ["foo","bar",["foo","bar"]]
@end group

@group
jsonencode (containers.Map(@{'foo'; 'bar'; 'baz'@}, [1, 2, 3]))
@result{} @{"bar":2,"baz":3,"foo":1@}
@end group
@end example

@seealso{jsondecode}
@end deftypefn */)
{
#if defined (HAVE_RAPIDJSON)

  int nargin = args.length ();
  // jsonencode has two options 'ConvertInfAndNaN' and 'PrettyWriter'
  if (! (nargin == 1 || nargin == 3 || nargin == 5))
    print_usage ();

  // Initialize options with their default values
  bool ConvertInfAndNaN = true;
  bool PrettyWriter = false;

  for (octave_idx_type i = 1; i < nargin; ++i)
    {
      if (! args(i).is_string ())
        error ("jsonencode: Option must be character vector");
      if (! args(i+1).is_bool_scalar ())
        error ("jsonencode: Value for options must be logical scalar");

      std::string option_name = args(i++).string_value ();
      if (octave::string::strcmpi(option_name, "ConvertInfAndNaN"))
        ConvertInfAndNaN = args(i).bool_value ();
      else if (octave::string::strcmpi(option_name, "PrettyWriter"))
        PrettyWriter = args(i).bool_value ();
      else
        error ("jsonencode: Valid options are \'ConvertInfAndNaN\'"
               " and \'PrettyWriter\'");
    }

  // FIXME: RapidJSON 1.1.0 (2016-08-25) is the latest release (2020-08-18)
  //        and does not support the "PrettyWriter" option.  Once a newer
  //        RapidJSON version is released and established with major
  //        distributions, make that version a requirement.
  #if ! defined (HAVE_RAPIDJSON_DEV)
     if (PrettyWriter)
       {
         warn_disabled_feature ("jsonencode",
                                "the \'PrettyWriter\' option of RapidJSON");
         PrettyWriter = false;
       }
  #endif

  rapidjson::StringBuffer json;
  if (PrettyWriter)
    {
      #if defined (HAVE_RAPIDJSON_DEV)
         rapidjson::PrettyWriter<rapidjson::StringBuffer, rapidjson::UTF8<>,
                                 rapidjson::UTF8<>, rapidjson::CrtAllocator,
                                 rapidjson::kWriteNanAndInfFlag> writer (json);
         encode (writer, args(0), ConvertInfAndNaN);
      #endif
    }
  else
    {
      rapidjson::Writer<rapidjson::StringBuffer, rapidjson::UTF8<>,
                        rapidjson::UTF8<>, rapidjson::CrtAllocator,
                        rapidjson::kWriteNanAndInfFlag> writer (json);
      encode (writer, args(0), ConvertInfAndNaN);
    }

  return octave_value (json.GetString ());

#else

  octave_unused_parameter (args);

  err_disabled_feature ("jsonencode", "JSON encoding through RapidJSON");

#endif
}
