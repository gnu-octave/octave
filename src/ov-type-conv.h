/*

Copyright (C) 2004, 2005, 2006, 2007 John W. Eaton

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

#define OCTAVE_TYPE_CONV_BODY3(NAME, MATRIX_RESULT_T, SCALAR_RESULT_T) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  if (nargin == 1) \
    { \
      const octave_value arg = args(0); \
 \
      int t_arg = arg.type_id (); \
 \
      int t_result = MATRIX_RESULT_T::static_type_id (); \
 \
      if (t_arg == t_result || arg.class_name () == #NAME) \
	{ \
	  retval = arg; \
	} \
      else \
        { \
          octave_base_value::type_conv_fcn cf \
	    = octave_value_typeinfo::lookup_type_conv_op (t_arg, t_result); \
 \
          if (cf) \
	    { \
	      octave_base_value *tmp (cf (*(arg.internal_rep ()))); \
 \
	      if (tmp) \
		{ \
		  retval = octave_value (tmp); \
 \
		  retval.maybe_mutate (); \
		} \
	    } \
	  else \
	    { \
	      std::string arg_tname = arg.type_name (); \
 \
	      std::string result_tname = arg.numel () == 1 \
		? SCALAR_RESULT_T::static_type_name () \
		: MATRIX_RESULT_T::static_type_name (); \
 \
	      gripe_invalid_conversion (arg_tname, result_tname); \
	    } \
	} \
    } \
  else \
    print_usage (); \
 \
  return retval

#define OCTAVE_TYPE_CONV_BODY(NAME) \
  OCTAVE_TYPE_CONV_BODY3 (NAME, octave_ ## NAME ## _matrix, \
			  octave_ ## NAME ## _scalar)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
