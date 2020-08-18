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

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "parse.h"

#if defined (HAVE_RAPIDJSON)
#  include <rapidjson/document.h>
#  include <rapidjson/error/en.h>
#endif

#if defined (HAVE_RAPIDJSON)

octave_value
decode (const rapidjson::Value& val, const octave_value_list& options);

//! Decodes a numerical JSON value into a scalar number.
//!
//! @param val JSON value that is guaranteed to be a numerical value.
//!
//! @return @ref octave_value that contains the numerical value of @p val.
//!
//! @b Example:
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("123");
//! octave_value num = decode_number (d);
//! @endcode

octave_value
decode_number (const rapidjson::Value& val)
{
  if (val.IsUint ())
    return octave_value (val.GetUint ());
  else if (val.IsInt ())
    return octave_value (val.GetInt ());
  else if (val.IsUint64 ())
    return octave_value (val.GetUint64 ());
  else if (val.IsInt64 ())
    return octave_value (val.GetInt64 ());
  else if (val.IsDouble ())
    return octave_value (val.GetDouble ());
  else
    error ("jsondecode.cc: Unidentified type.");
}

//! Decodes a JSON object into a scalar struct.
//!
//! @param val JSON value that is guaranteed to be a JSON object.
//! @param options @c ReplacementStyle and @c Prefix options with their values.
//!
//! @return @ref octave_value that contains the equivalent scalar struct of @p val.
//!
//! @b Example:
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("{\"a\": 1, \"b\": 2}");
//! octave_value struct = decode_object (d, octave_value_list ());
//! @endcode

octave_value
decode_object (const rapidjson::Value& val, const octave_value_list& options)
{
  octave_scalar_map retval;
  for (const auto& pair : val.GetObject ())
    {
      std::string fcn_name = "matlab.lang.makeValidName";
      octave_value_list args = octave_value_list (pair.name.GetString ());
      args.append (options);
      std::string validName = octave::feval (fcn_name,args)(0).string_value ();
      retval.assign (validName, decode (pair.value, options));
    }
  return octave_value (retval);
}

//! Decodes a JSON array that contains only numerical or null values
//! into an NDArray.
//!
//! @param val JSON value that is guaranteed to be a numeric array.
//!
//! @return @ref octave_value that contains the equivalent NDArray of @p val.
//!
//! @b Example:
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[1, 2, 3, 4]");
//! octave_value numeric_array = decode_numeric_array (d);
//! @endcode

octave_value
decode_numeric_array (const rapidjson::Value& val)
{
  NDArray retval (dim_vector (val.Size (), 1));
  octave_idx_type index = 0;
  for (const auto& elem : val.GetArray ())
    retval(index++) = elem.IsNull () ? octave_NaN
                                     : decode_number (elem).double_value ();
  return retval;
}

//! Decodes a JSON array that contains only boolean values into a boolNDArray.
//!
//! @param val JSON value that is guaranteed to be a boolean array.
//!
//! @return @ref octave_value that contains the equivalent boolNDArray of @p val.
//!
//! @b Example:
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[true, false, true]");
//! octave_value boolean_array = decode_boolean_array (d);
//! @endcode

octave_value
decode_boolean_array (const rapidjson::Value& val)
{
  boolNDArray retval (dim_vector (val.Size (), 1));
  octave_idx_type index = 0;
  for (const auto& elem : val.GetArray ())
    retval(index++) = elem.GetBool ();
  return retval;
}

//! Decodes a JSON array that contains different types
//! or string values only into a Cell.
//!
//! @param val JSON value that is guaranteed to be a mixed or string array.
//! @param options @c ReplacementStyle and @c Prefix options with their values.
//!
//! @return @ref octave_value that contains the equivalent Cell of @p val.
//!
//! @b Example (decoding a string array):
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[\"foo\", \"bar\", \"baz\"]");
//! octave_value cell = decode_string_and_mixed_array (d, octave_value_list ());
//! @endcode
//!
//! @b Example (decoding a mixed array):
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[\"foo\", 123, true]");
//! octave_value cell = decode_string_and_mixed_array (d, octave_value_list ());
//! @endcode

octave_value
decode_string_and_mixed_array (const rapidjson::Value& val,
                               const octave_value_list& options)
{
  Cell retval (dim_vector (val.Size (), 1));
  octave_idx_type index = 0;
  for (const auto& elem : val.GetArray ())
    retval(index++) = decode (elem, options);
  return retval;
}

//! Decodes a JSON array that contains only objects into a Cell or a struct array
//! depending on the similarity of the objects' keys.
//!
//! @param val JSON value that is guaranteed to be an object array.
//! @param options @c ReplacementStyle and @c Prefix options with their values.
//!
//! @return @ref octave_value that contains the equivalent Cell
//! or struct array of @p val.
//!
//! @b Example (returns a struct array):
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[{\"a\":1,\"b\":2},{\"a\":3,\"b\":4}]");
//! octave_value object_array = decode_object_array (d, octave_value_list ());
//! @endcode
//!
//! @b Example (returns a Cell):
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[{\"a\":1,\"b\":2},{\"b\":3,\"a\":4}]");
//! octave_value object_array = decode_object_array (d, octave_value_list ());
//! @endcode

octave_value
decode_object_array (const rapidjson::Value& val,
                     const octave_value_list& options)
{
  Cell struct_cell = decode_string_and_mixed_array (val, options).cell_value ();
  string_vector field_names = struct_cell(0).scalar_map_value ().fieldnames ();
  bool same_field_names = true;
  for (octave_idx_type i = 1; i < struct_cell.numel (); ++i)
    if (field_names.std_list ()
        != struct_cell(i).scalar_map_value ().fieldnames ().std_list ())
      {
        same_field_names = 0;
        break;
      }
  if (same_field_names)
    {
      octave_map struct_array;
      Cell value (dim_vector (struct_cell.numel (), 1));
      for (octave_idx_type i = 0; i < field_names.numel (); ++i)
        {
          for (octave_idx_type k = 0; k < struct_cell.numel (); ++k)
            value(k) = struct_cell(k).scalar_map_value ().getfield (field_names(i));
          struct_array.assign (field_names(i), value);
        }
      return octave_value (struct_array);
    }
  else
    return struct_cell;
}

//! Decodes a JSON array that contains only arrays into a Cell or an NDArray
//! depending on the dimensions and the elements' type of the sub arrays.
//!
//! @param val JSON value that is guaranteed to be an array of arrays.
//! @param options @c ReplacementStyle and @c Prefix options with their values.
//!
//! @return @ref octave_value that contains the equivalent Cell
//! or NDArray of @p val.
//!
//! @b Example (returns an NDArray):
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[[1, 2], [3, 4]]");
//! octave_value array = decode_array_of_arrays (d, octave_value_list ());
//! @endcode
//!
//! @b Example (returns a Cell):
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[[1, 2], [3, 4, 5]]");
//! octave_value cell = decode_array_of_arrays (d, octave_value_list ());
//! @endcode

octave_value
decode_array_of_arrays (const rapidjson::Value& val,
                        const octave_value_list& options)
{
  // Some arrays should be decoded as NDArrays and others as cell arrays
  Cell cell = decode_string_and_mixed_array(val, options).cell_value ();
  // Only arrays with sub arrays of booleans and numericals will return NDArray
  bool is_bool = cell(0).is_bool_matrix ();
  dim_vector sub_array_dims = cell(0).dims ();
  octave_idx_type sub_array_ndims = cell(0).ndims ();
  octave_idx_type cell_numel = cell.numel ();
  for (octave_idx_type i = 0; i < cell_numel; ++i)
    {
      // If one element is cell return the cell array as at least one of
      // the sub arrays area either an array of: strings, objects or mixed array
      if (cell(i).iscell ())
        return cell;
      // If not the same dim of elements or dim = 0 return cell array
      if (cell(i).dims () != sub_array_dims || sub_array_dims == dim_vector ())
        return cell;
      // If not numeric sub arrays only or bool
      // sub arrays only return cell array
      if(cell(i).is_bool_matrix () != is_bool)
        return cell;
    }
  // Calculate the dims of the output array
  dim_vector array_dims;
	array_dims.resize (sub_array_ndims + 1);
	array_dims(0) = cell_numel;
	for (auto i = 1; i < sub_array_ndims + 1; i++)
  	array_dims(i) = sub_array_dims(i-1);
  NDArray array (array_dims);

  // Populate the array with specific order to generate MATLAB-identical output
  octave_idx_type array_index = 0;
  for (octave_idx_type i = 0; i < array.numel () / cell_numel; ++i)
    for (octave_idx_type k = 0; k < cell_numel; ++k)
      array(array_index++) = cell(k).array_value ()(i);
  return array;
}

//! Decodes any type of JSON arrays. This function only serves as an interface
//! by choosing which function to call from the previous functions.
//!
//! @param val JSON value that is guaranteed to be an array.
//! @param options @c ReplacementStyle and @c Prefix options with their values.
//!
//! @return @ref octave_value that contains the output of decoding @p val.
//!
//! @b Example:
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[[1, 2], [3, 4, 5]]");
//! octave_value array = decode_array (d, octave_value_list ());
//! @endcode

octave_value
decode_array (const rapidjson::Value& val, const octave_value_list& options)
{
  // Handle empty arrays
  if (val.Empty ())
    return NDArray (dim_vector (0,0));

  // Compare with other elements to know if the array has multiple types
  rapidjson::Type array_type = val[0].GetType ();
  // Check if the array is numeric and if it has multible types
  bool same_type = 1, is_numeric = 1;
  for (const auto& elem : val.GetArray ())
    {
      rapidjson::Type current_elem_type = elem.GetType ();
      if (is_numeric && ! (current_elem_type == rapidjson::kNullType
          || current_elem_type == rapidjson::kNumberType))
        is_numeric = 0;
      if (same_type && (current_elem_type != array_type))
        // RapidJSON doesn't have kBoolean Type it has kTrueType and kFalseType
        if (! ((current_elem_type == rapidjson::kTrueType
                && array_type == rapidjson::kFalseType)
            || (current_elem_type == rapidjson::kFalseType
                && array_type == rapidjson::kTrueType)))
          same_type = 0;
    }
  if (is_numeric)
    return decode_numeric_array (val);
  if (same_type && (array_type != rapidjson::kStringType))
    {
      if (array_type == rapidjson::kTrueType
          || array_type == rapidjson::kFalseType)
        return decode_boolean_array (val);
      else if (array_type == rapidjson::kObjectType)
        return decode_object_array (val, options);
      else if (array_type == rapidjson::kArrayType)
        return decode_array_of_arrays (val, options);
      else
        error ("jsondecode.cc: Unidentified type.");
    }
  else
    return decode_string_and_mixed_array (val, options);
}

//! Decodes any JSON value. This function only serves as an interface
//! by choosing which function to call from the previous functions.
//!
//! @param val JSON value.
//! @param options @c ReplacementStyle and @c Prefix options with their values.
//!
//! @return @ref octave_value that contains the output of decoding @p val.
//!
//! @b Example:
//!
//! @code{.cc}
//! rapidjson::Document d;
//! d.Parse ("[{\"a\":1,\"b\":2},{\"b\":3,\"a\":4}]");
//! octave_value value = decode (d, octave_value_list ());
//! @endcode

octave_value
decode (const rapidjson::Value& val, const octave_value_list& options)
{
  if (val.IsBool ())
    return val.GetBool ();
  else if (val.IsNumber ())
    return decode_number (val);
  else if (val.IsString ())
    return val.GetString ();
  else if (val.IsObject ())
    return decode_object (val, options);
  else if (val.IsNull ())
    return NDArray (dim_vector (0,0));
  else if (val.IsArray ())
    return decode_array (val, options);
  else
    error ("jsondecode.cc: Unidentified type.");
}

#endif

DEFUN (jsondecode, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{object} =} jsondecode (@var{json})
@deftypefnx {} {@var{object} =} jsondecode (@var{json}, "ReplacementStyle", @var{rs})
@deftypefnx {} {@var{object} =} jsondecode (@var{json}, "Prefix", @var{pfx})
@deftypefnx {} {@var{object} =} jsondecode (@var{json}, @dots{})

Decode text that is formatted in JSON.

The input @var{json} is a string that contains JSON text.
The output @var{object} is an Octave object that contains the result
of decoding @var{json}.

For more information about the options @qcode{"ReplacementStyle"} and
@qcode{"Prefix"}, see @ref{matlab.lang.makeValidName}.

-NOTE: It is not guaranteed to get the same JSON text if you decode
and then encode it as some names may change by @ref{matlab.lang.makeValidName}.

This table shows the conversions from JSON data types to Octave data types:

@table @asis
@item @qcode{"Boolean"}
Scalar @qcode{"logical"}

@item @qcode{"Number"}
Scalar @qcode{"double"}

@item @qcode{"String"}
@qcode{"Vector"} of chars

@item JSON @qcode{"Object"}
Scalar @qcode{"struct"} (field names of the struct may be different from
the keys of the JSON object due to @ref{matlab.lang.makeValidName})

@item @qcode{"Array"} of different data types
@qcode{"Cell"} array

@item @qcode{"Array"} of booleans
@qcode{"Array"} of logicals

@item @qcode{"Array"} of numbers
@qcode{"Array"} of doubles

@item @qcode{"Array"} of strings
@qcode{"Cell"} array of vectors of chars

@item @qcode{"Array"} of JSON objects (All objects have the same field names)
@qcode{"Struct array"}

@item @qcode{"Array"} of JSON objects (Objects have different field names)
@qcode{"Cell"} array of scalar structs

@item @qcode{"null"} inside a numeric array
@qcode{"NaN"}

@item @qcode{"null"} inside a non-numeric array
Empty @qcode{"Array"} of doubles (@qcode{"[]"})
@end table

Examples:

@example
@group
jsondecode ('[1, 2, null, 3]')
    @result{} ans =

      1
      2
    NaN
      3
@end group

@group
jsondecode ('["foo", "bar", ["foo", "bar"]]')
    @result{} ans =
       @{
         [1,1] = foo
         [2,1] = bar
         [3,1] =
         @{
           [1,1] = foo
           [2,1] = bar
         @}

       @}
@end group

@group
jsondecode ('@{"nu#m#ber": 7, "s#tr#ing": "hi"@}', 'ReplacementStyle', 'delete')
    @result{} scalar structure containing the fields:

         number = 7
         string = hi
@end group

@group
jsondecode ('@{"1": "one", "2": "two"@}', 'Prefix', 'm_')
    @result{} scalar structure containing the fields:

         m_1 = one
         m_2 = two
@end group
@end example

@seealso{jsonencode, matlab.lang.makeValidName}
@end deftypefn */)
{
#if defined (HAVE_RAPIDJSON)

  int nargin = args.length ();
  // makeValidName options are pairs, the number of arguments must be odd.
  if (! (nargin % 2))
    print_usage ();

  if(! args(0).is_string ())
    error ("jsondecode: The input must be a character string");

  std::string json = args (0).string_value ();
  rapidjson::Document d;
  // DOM is chosen instead of SAX as SAX publishes events to a handler that
  // decides what to do depending on the event only.  This will cause a
  // problem in decoding JSON arrays as the output may be an array or a cell
  // and that doesn't only depend on the event (startArray) but also on the
  // types of the elements inside the array.
  d.Parse <rapidjson::kParseNanAndInfFlag>(json.c_str ());

  if (d.HasParseError ())
    error("jsondecode: Parse error at offset %u: %s\n",
          static_cast<unsigned int> (d.GetErrorOffset ()),
          rapidjson::GetParseError_En (d.GetParseError ()));

  return decode (d, args.slice (1, nargin - 1));

#else

  octave_unused_parameter (args);

  err_disabled_feature ("jsondecode", "JSON decoding through RapidJSON");

#endif
}
