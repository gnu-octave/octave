/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_defun_h)
#define octave_defun_h 1

#if defined (octave_defun_dld_h)
#error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

// Define a builtin function.
//
//   name is the name of the function, unqouted.
//
//   args_name is the name of the octave_value_list variable used to pass
//     the argument list to this function.
//
//   nargout_name is the name of the int variable used to pass the
//     number of output arguments this function is expected to produce.
//
//   doc is the simple help text for the function.

#define DEFUN(name, args_name, nargout_name, doc) \
  DEFUN_INTERNAL (name, args_name, nargout_name, false, doc)

// This one can be used when `name' cannot be used directly (if it is
// already defined as a macro).  In that case, name is already a
// quoted string, and the internal name of the function must be passed
// too (the convention is to use a prefix of "F", so "foo" becomes "Ffoo").

#define DEFUNX(name, fname, args_name, nargout_name, doc) \
  DEFUNX_INTERNAL (name, fname, args_name, nargout_name, false, doc)

// Define a builtin command-style function.
//
// This is like DEFUN, except that it defines a function that can be
// called from the Octave language without using parenthesis to
// surround the arguments). 

#define DEFCMD(name, args_name, nargout_name, doc) \
  DEFUN_INTERNAL (name, args_name, nargout_name, true, doc)

// For backward compatibility.

#define DEFUN_TEXT DEFCMD

// This is a function with a name that can't be hidden by a variable.
#define DEFCONSTFUN(name, args_name, nargout_name, doc) \
  DEFCONSTFUN_INTERNAL (name, args_name, nargout_name, true, doc)

// Define a mapper function.
//
//   name is the name of the function, unquoqted.
//
//   ch_map is a pointer to a function that should be called for
//     integer arguments that are expected to create integer results.
//     (It's a kluge to handle character mappers like isalpha.)
//
//   d_b_map is a pointer to a function that should be called for real
//     arguments that are expected to create bool results.
//
//   c_b_map is a pointer to a function that should be called for
//     complex arguments that are expected to create bool results.
//
//   d_d_map is a pointer to a function that should be called for real
//     arguments that are expected to create real results.
//
//   c_d_map is a pointer to a function that should be called for
//     complex arguments that are expected to create real results.
//
//   c_c_map is a pointer to a function that should be called for
//     complex arguments that are expected to create complex results.
//
//   lo is the lower bound of the range for which real arguments
//     return real results (e.g., lo == 0 for sqrt).
//
//   hi is the upper bound of the range for which real arguments
//     return real results (e.g., hi == Inf for sqrt).
//
//   ch_map_flag has the following meanings for the ch_map function:
//
//     0  =>  this function returns a matrix of ones and zeros
//     1  =>  this function returns a numeric matrix (any values)
//     2  =>  this function returns a std::string array
//
//   can_ret_cmplx_for_real is a flag that says whether this function
//     can create a complex number given a real-valued  argument
//     (e.g., sqrt (-1)).
//
//   doc is the simple help text for the function.

#define DEFUN_MAPPER(name, ch_map, d_b_map, c_b_map, d_d_map, \
		     c_d_map, c_c_map, lo, hi, ch_map_flag, \
		     can_ret_cmplx_for_real, doc) \
  DEFUN_MAPPER_INTERNAL (name, ch_map, d_b_map, c_b_map, d_d_map, \
			 c_d_map, c_c_map, lo, hi, ch_map_flag, \
			 can_ret_cmplx_for_real, doc)

// Make alias another name for the existing function name.  This macro
// must be used in the same file where name is defined, after the
// definition for name.

#define DEFALIAS(alias, name) \
  DEFALIAS_INTERNAL (alias, name)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
